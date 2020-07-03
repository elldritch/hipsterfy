{-# LANGUAGE Arrows #-}

module Hipsterfy.Artist
  ( Artist (..),
    ArtistID (..),
    toDatabaseArtistID,
    fromDatabaseArtistID,
    createArtist,
    ArtistListenersAgg,
    ArtistListenersAggF,
    toArtist,
    getArtistsBy,
    getArtistByID,
    getArtistBySpotifyID,
    refreshArtistInsights,
    setUpdateSubmitted,
    setUpdateCompleted,
  )
where

import Control.Arrow (returnA)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.HashMap.Strict as HashMap
import Data.Profunctor.Product (p2)
import Data.Time (Day, UTCTime, cdDays, diffGregorianDurationClip, getCurrentTime, utctDay)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Hipsterfy.Application (MonadApp)
import Hipsterfy.Database (QueryParameters, runInsert, runInsertOne, runSelectOne, runTransaction, runUpdate)
import Hipsterfy.Database.Artist
  ( ArtistIDReadF,
    ArtistIDT (..),
    ArtistListenersReadF,
    ArtistListenersT (..),
    ArtistReadF,
    ArtistT (..),
    artistListenersTable,
    artistTable,
    pArtist,
    pArtistID,
    pArtistListeners,
  )
import qualified Hipsterfy.Database.Artist as D (Artist, ArtistID)
import Hipsterfy.Database.Jobs (UpdateJobInfoF, UpdateJobInfoT (..), pUpdateJobInfo)
import Hipsterfy.Jobs (UpdateJobInfo (..))
import Hipsterfy.Spotify (SpotifyArtist (..), SpotifyArtistID (..), SpotifyArtistInsights (..), getSpotifyArtistInsights)
import Hipsterfy.Spotify.Auth (AnonymousBearerToken)
import Opaleye.Aggregate (aggregate, arrayAgg, groupBy)
import Opaleye.Field (Field, null, toNullable)
import Opaleye.Manipulation (Insert (..), Update (..), rCount, rReturning, updateEasy)
import Opaleye.Operators ((.===), restrict)
import Opaleye.Select (Select, SelectArr)
import Opaleye.SqlTypes (SqlArray, SqlInt4, SqlTimestamptz, sqlInt4, sqlStrictText, sqlUTCTime)
import Opaleye.Table (selectTable)
import Relude hiding (null)

-- Application data structures.

newtype ArtistID = ArtistID Int
  deriving (Show, Eq, Ord, ToField, FromField, FromJSON, ToJSON, Generic)

instance Hashable ArtistID

data Artist = Artist
  { artistID :: ArtistID,
    spotifyArtist :: SpotifyArtist,
    monthlyListeners :: HashMap Day Int,
    updateJobInfo :: UpdateJobInfo
  }
  deriving (Show, Eq, Generic)

instance Hashable Artist

toDatabaseArtistID :: ArtistID -> ArtistIDReadF
toDatabaseArtistID (ArtistID i) = ArtistIDT $ sqlInt4 i

fromDatabaseArtistID :: D.ArtistID -> ArtistID
fromDatabaseArtistID (ArtistIDT i) = ArtistID i

-- Named parameter sets for tracing.

instance QueryParameters Artist

newtype ArtistIDParam = ArtistIDParam {artistID :: ArtistID} deriving (Generic)

instance QueryParameters ArtistIDParam

newtype SpotifyArtistIDParam = SpotifyArtistIDParam {spotifyArtistID :: SpotifyArtistID} deriving (Generic)

instance QueryParameters SpotifyArtistIDParam

data JobSubmittedParam = JobSubmittedParam
  { artistID :: ArtistID,
    jobSubmitted :: UTCTime
  }
  deriving (Generic)

instance QueryParameters JobSubmittedParam

data JobCompletedParam = JobCompletedParam
  { artistID :: ArtistID,
    jobCompleted :: UTCTime
  }
  deriving (Generic)

instance QueryParameters JobCompletedParam

data ArtistListenersParam = ArtistListenersParam
  { artistID :: ArtistID,
    listeners :: Int
  }
  deriving (Generic)

instance QueryParameters ArtistListenersParam

-- Creation.

createArtist :: (MonadApp m) => SpotifyArtist -> m Artist
createArtist SpotifyArtist {spotifyArtistID, spotifyURL, name} = do
  runTransaction $ do
    -- Construct an artist if one doesn't already exist.
    maybeArtist <- getArtistBySpotifyID spotifyArtistID
    case maybeArtist of
      Just artist -> return artist
      Nothing -> do
        let artist =
              Artist
                { artistID = error "createArtist: impossible: artistID not used during insertion",
                  spotifyArtist =
                    SpotifyArtist
                      { spotifyArtistID,
                        spotifyURL,
                        name
                      },
                  updateJobInfo =
                    UpdateJobInfo
                      { lastUpdateJobSubmitted = Nothing,
                        lastUpdateJobCompleted = Nothing
                      },
                  monthlyListeners = mempty
                }
        aid <- runInsertOne "insertArtist" insertArtist artist
        return (artist {artistID = fromDatabaseArtistID aid} :: Artist)
  where
    insertArtist :: Artist -> Insert [D.ArtistID]
    insertArtist Artist {..} =
      let SpotifyArtist {spotifyURL = su, name = n, spotifyArtistID = SpotifyArtistID saID} = spotifyArtist
       in Insert
            { iTable = artistTable,
              iRows =
                [ ArtistT
                    { artistID = ArtistIDT Nothing,
                      name = sqlStrictText n,
                      spotifyArtistID = sqlStrictText saID,
                      spotifyURL = sqlStrictText su,
                      updateJobInfo =
                        UpdateJobInfoT
                          { lastUpdateJobSubmitted = null,
                            lastUpdateJobCompleted = null
                          }
                    }
                ],
              iReturning = rReturning (\ArtistT {artistID = aid} -> aid),
              iOnConflict = Nothing
            }

-- Retrieval.

type ArtistListenersAggF =
  ArtistListenersT
    (ArtistIDT (Field (SqlArray SqlInt4)))
    (Field (SqlArray SqlTimestamptz))
    (Field (SqlArray SqlInt4))

type ArtistListenersAgg = ArtistListenersT (ArtistIDT [Int]) [UTCTime] [Int]

toArtist :: (D.Artist, ArtistListenersAgg) -> Artist
toArtist (ArtistT {..}, ArtistListenersT {..}) =
  let UpdateJobInfoT {..} = updateJobInfo
   in Artist
        { artistID = fromDatabaseArtistID artistID,
          spotifyArtist = SpotifyArtist {spotifyArtistID, spotifyURL, name},
          updateJobInfo = UpdateJobInfo {lastUpdateJobCompleted, lastUpdateJobSubmitted},
          monthlyListeners = HashMap.fromList $ zip (utctDay <$> createdAt) monthlyListeners
        }

getArtistsBy :: SelectArr ArtistReadF () -> Select (ArtistReadF, ArtistListenersAggF)
getArtistsBy artistArr = aggregateArtistListeners getArtistsAndListeners
  where
    getArtistsAndListeners :: Select (ArtistReadF, ArtistListenersReadF)
    getArtistsAndListeners = proc () -> do
      artist@ArtistT {artistID = rowAID} <- selectTable artistTable -< ()
      listeners@ArtistListenersT {listenersArtistID = listenerAID} <- selectTable artistListenersTable -< ()

      restrict -< rowAID .=== listenerAID
      artistArr -< artist

      returnA -< (artist, listeners)
    aggregateArtistListeners :: Select (ArtistReadF, ArtistListenersReadF) -> Select (ArtistReadF, ArtistListenersAggF)
    aggregateArtistListeners =
      aggregate
        ( p2
            ( pArtist
                ArtistT
                  { artistID = pArtistID $ ArtistIDT groupBy,
                    name = groupBy,
                    spotifyArtistID = groupBy,
                    spotifyURL = groupBy,
                    updateJobInfo =
                      pUpdateJobInfo $
                        UpdateJobInfoT
                          { lastUpdateJobSubmitted = groupBy,
                            lastUpdateJobCompleted = groupBy
                          }
                  },
              pArtistListeners
                ArtistListenersT
                  { listenersArtistID = pArtistID $ ArtistIDT arrayAgg,
                    createdAt = arrayAgg,
                    monthlyListeners = arrayAgg
                  }
            )
        )

getArtistByID :: (MonadApp m) => ArtistID -> m (Maybe Artist)
getArtistByID artistID = do
  maybeArtist <- runSelectOne "getArtistByID" makeSelect $ ArtistIDParam artistID
  return $ toArtist <$> maybeArtist
  where
    makeSelect :: ArtistIDParam -> Select (ArtistReadF, ArtistListenersAggF)
    makeSelect (ArtistIDParam paramAID) =
      getArtistsBy $ proc ArtistT {artistID = rowAID} -> do
        restrict -< rowAID .=== toDatabaseArtistID paramAID

getArtistBySpotifyID :: (MonadApp m) => SpotifyArtistID -> m (Maybe Artist)
getArtistBySpotifyID spotifyArtistID = do
  maybeArtist <- runSelectOne "getArtistBySpotifyID" makeSelect $ SpotifyArtistIDParam spotifyArtistID
  return $ toArtist <$> maybeArtist
  where
    makeSelect :: SpotifyArtistIDParam -> Select (ArtistReadF, ArtistListenersAggF)
    makeSelect (SpotifyArtistIDParam (SpotifyArtistID paramSAID)) =
      getArtistsBy $ proc ArtistT {spotifyArtistID = rowSAID} -> do
        restrict -< rowSAID .=== sqlStrictText paramSAID

-- Operations.

refreshArtistInsightsTimeoutDays :: (Integral n) => n
refreshArtistInsightsTimeoutDays = 15

refreshArtistInsights :: (MonadApp m) => AnonymousBearerToken -> Artist -> m Artist
refreshArtistInsights bearerToken artist@Artist {artistID, spotifyArtist = SpotifyArtist {spotifyArtistID}, monthlyListeners} = do
  now <- liftIO getCurrentTime
  let today = utctDay now
  maybeNewSample <- case viaNonEmpty head $ reverse $ sort $ HashMap.toList monthlyListeners of
    Just (sampleDay, _) ->
      if cdDays (diffGregorianDurationClip today sampleDay) > refreshArtistInsightsTimeoutDays
        then Just <$> updateInsights
        else return Nothing
    _ -> Just <$> updateInsights
  return $ case maybeNewSample of
    Just newSample ->
      artist {monthlyListeners = HashMap.insert today newSample monthlyListeners} :: Artist
    Nothing -> artist
  where
    updateInsights :: (MonadApp m) => m Int
    updateInsights = do
      SpotifyArtistInsights {monthlyListeners = listeners} <-
        liftIO $ getSpotifyArtistInsights bearerToken spotifyArtistID
      void $ runInsert "refreshArtistInsights" makeInsert $ ArtistListenersParam {artistID, listeners}
      return listeners
    makeInsert :: ArtistListenersParam -> Insert Int64
    makeInsert ArtistListenersParam {artistID = artistID', listeners} =
      Insert
        { iTable = artistListenersTable,
          iRows =
            [ ArtistListenersT
                { listenersArtistID = toDatabaseArtistID artistID',
                  createdAt = Nothing,
                  monthlyListeners = sqlInt4 listeners
                }
            ],
          iReturning = rCount,
          iOnConflict = Nothing
        }

-- Update status.

setUpdateSubmitted :: (MonadApp m) => ArtistID -> m ()
setUpdateSubmitted artistID = do
  now <- liftIO getCurrentTime
  void $ runUpdate "artistUpdateJobSubmitted" makeUpdate $ JobSubmittedParam {artistID, jobSubmitted = now}
  where
    makeUpdate :: JobSubmittedParam -> Update Int64
    makeUpdate JobSubmittedParam {artistID = paramUID, jobSubmitted} =
      Update
        { uTable = artistTable,
          uUpdateWith = updateEasy $
            \u@ArtistT {updateJobInfo} ->
              (u {updateJobInfo = updateJobInfo {lastUpdateJobSubmitted = toNullable $ sqlUTCTime jobSubmitted} :: UpdateJobInfoF} :: ArtistReadF),
          uWhere = \ArtistT {artistID = rowUID} -> rowUID .=== toDatabaseArtistID paramUID,
          uReturning = rCount
        }

setUpdateCompleted :: (MonadApp m) => ArtistID -> m ()
setUpdateCompleted artistID = do
  now <- liftIO getCurrentTime
  void $ runUpdate "artistUpdateJobCompleted" makeUpdate $ JobCompletedParam {artistID, jobCompleted = now}
  where
    makeUpdate :: JobCompletedParam -> Update Int64
    makeUpdate JobCompletedParam {artistID = paramUID, jobCompleted} =
      Update
        { uTable = artistTable,
          uUpdateWith = updateEasy $
            \u@ArtistT {updateJobInfo} ->
              (u {updateJobInfo = updateJobInfo {lastUpdateJobCompleted = toNullable $ sqlUTCTime jobCompleted} :: UpdateJobInfoF} :: ArtistReadF),
          uWhere = \ArtistT {artistID = rowUID} -> rowUID .=== toDatabaseArtistID paramUID,
          uReturning = rCount
        }
