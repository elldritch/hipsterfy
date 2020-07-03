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
import Opaleye (Nullable)
import Opaleye.Aggregate (aggregate, arrayAgg, groupBy)
import Opaleye.Field (Field, FieldNullable, null, toNullable)
import Opaleye.Join (leftJoin)
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

data CreateArtistParams = CreateArtistParams
  { name :: Text,
    spotifyArtistID :: SpotifyArtistID,
    spotifyURL :: Text
  }
  deriving (Generic)

instance QueryParameters CreateArtistParams

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
createArtist SpotifyArtist {..} = do
  runTransaction $ do
    -- Construct an artist if one doesn't already exist.
    maybeArtist <- getArtistBySpotifyID spotifyArtistID
    case maybeArtist of
      Just artist -> return artist
      Nothing -> do
        aid <- runInsertOne "insertArtist" insertArtist CreateArtistParams {..}
        return
          Artist
            { artistID = fromDatabaseArtistID aid,
              spotifyArtist = SpotifyArtist {..},
              updateJobInfo =
                UpdateJobInfo
                  { lastUpdateJobSubmitted = Nothing,
                    lastUpdateJobCompleted = Nothing
                  },
              monthlyListeners = mempty
            }
  where
    insertArtist :: CreateArtistParams -> Insert [D.ArtistID]
    insertArtist CreateArtistParams {spotifyURL = su, name = n, spotifyArtistID = SpotifyArtistID saID} =
      Insert
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
    (ArtistIDT (Field (SqlArray (Nullable SqlInt4))))
    (Field (SqlArray (Nullable SqlTimestamptz)))
    (Field (SqlArray (Nullable SqlInt4)))

type ArtistListenersAgg = ArtistListenersT (ArtistIDT [Maybe Int]) [Maybe UTCTime] [Maybe Int]

type NullableArtistIDF = ArtistIDT (FieldNullable SqlInt4)

type NullableArtistListenersF = ArtistListenersT NullableArtistIDF (FieldNullable SqlTimestamptz) (FieldNullable SqlInt4)

getArtistsBy :: SelectArr ArtistReadF () -> Select (ArtistReadF, ArtistListenersAggF)
getArtistsBy artistArr = aggregateArtistListeners getArtistsAndListeners
  where
    getArtistsAndListeners :: Select (ArtistReadF, NullableArtistListenersF)
    getArtistsAndListeners = proc () -> do
      (artist, listeners) <- artistsWithAnyListeners -< ()
      artistArr -< artist
      returnA -< (artist, listeners)
      where
        artistsWithAnyListeners :: Select (ArtistReadF, NullableArtistListenersF)
        artistsWithAnyListeners = leftJoin (selectTable artistTable) (selectTable artistListenersTable) $
          \(ArtistT {..}, ArtistListenersT {..}) -> artistID .=== listenersArtistID
    aggregateArtistListeners :: Select (ArtistReadF, NullableArtistListenersF) -> Select (ArtistReadF, ArtistListenersAggF)
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

toArtist :: (D.Artist, ArtistListenersAgg) -> Artist
toArtist (ArtistT {..}, ArtistListenersT {..}) =
  let UpdateJobInfoT {..} = updateJobInfo
   in Artist
        { artistID = fromDatabaseArtistID artistID,
          spotifyArtist = SpotifyArtist {..},
          updateJobInfo = UpdateJobInfo {..},
          monthlyListeners = HashMap.fromList $ zipMaybes (utctDay <<$>> createdAt) monthlyListeners
        }
  where
    zipMaybes :: [Maybe a] -> [Maybe b] -> [(a, b)]
    zipMaybes (Just x : xs) (Just y : ys) = (x, y) : zipMaybes xs ys
    zipMaybes (Nothing : xs) (_ : ys) = zipMaybes xs ys
    zipMaybes (_ : xs) (Nothing : ys) = zipMaybes xs ys
    zipMaybes _ [] = []
    zipMaybes [] _ = []

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
refreshArtistInsights bearerToken artist@Artist {spotifyArtist = SpotifyArtist {spotifyArtistID}, ..} = do
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
      void $ runInsert "refreshArtistInsights" makeInsert $ ArtistListenersParam {..}
      return listeners
    makeInsert :: ArtistListenersParam -> Insert Int64
    makeInsert ArtistListenersParam {artistID = artistID', ..} =
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
  void $ runUpdate "artistUpdateJobSubmitted" makeUpdate $ JobSubmittedParam {jobSubmitted = now, ..}
  where
    makeUpdate :: JobSubmittedParam -> Update Int64
    makeUpdate JobSubmittedParam {artistID = paramUID, ..} =
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
  void $ runUpdate "artistUpdateJobCompleted" makeUpdate $ JobCompletedParam {jobCompleted = now, ..}
  where
    makeUpdate :: JobCompletedParam -> Update Int64
    makeUpdate JobCompletedParam {artistID = paramUID, ..} =
      Update
        { uTable = artistTable,
          uUpdateWith = updateEasy $
            \u@ArtistT {updateJobInfo} ->
              (u {updateJobInfo = updateJobInfo {lastUpdateJobCompleted = toNullable $ sqlUTCTime jobCompleted} :: UpdateJobInfoF} :: ArtistReadF),
          uWhere = \ArtistT {artistID = rowUID} -> rowUID .=== toDatabaseArtistID paramUID,
          uReturning = rCount
        }
