module Hipsterfy.Artist
  ( Artist (..),
    ArtistID (..),
    toDatabaseArtistID,
    fromDatabaseArtistID,
    createArtistIfNotExists,
    getArtist,
    refreshArtistInsights,
    UpdateStatus (..),
    getUpdateStatus,
    setUpdateSubmitted,
    setUpdateCompleted,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.HashMap.Strict as HashMap
import Data.Time (Day, cdDays, diffGregorianDurationClip, getCurrentTime, utctDay)
import Database.PostgreSQL.Simple (Only (..), execute, query)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Hipsterfy.Application (Config (..), MonadApp)
import Hipsterfy.Database.Artist (ArtistIDReadF, ArtistIDT (..))
import qualified Hipsterfy.Database.Artist as D (ArtistID)
import Hipsterfy.Jobs (UpdateJobInfo(..), UpdateStatus (..), getUpdateStatusRaw, setUpdateCompletedRaw, setUpdateSubmittedRaw)
import Hipsterfy.Spotify (SpotifyArtist (..), SpotifyArtistInsights (..), getSpotifyArtistInsights)
import Hipsterfy.Spotify.Auth (AnonymousBearerToken)
import Opaleye.SqlTypes (sqlInt4)
import Relude

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

-- Creation.

createArtistIfNotExists :: (MonadApp m) => SpotifyArtist -> m Artist
createArtistIfNotExists SpotifyArtist {spotifyArtistID, spotifyURL, name} = do
  Config {postgres} <- ask
  artistRows <-
    liftIO $
      query
        postgres
        "INSERT INTO spotify_artist\
        \ (name, spotify_artist_id, spotify_url, created_at)\
        \ VALUES\
        \ (?, ?, ?, NOW())\
        \ ON CONFLICT (spotify_artist_id) DO UPDATE SET name = ?\
        \ RETURNING id"
        (name, spotifyArtistID, spotifyURL, name)
  case artistRows of
    [Only artistID] ->
      return
        Artist
          { artistID,
            spotifyArtist = SpotifyArtist {spotifyArtistID, spotifyURL, name},
            monthlyListeners = mempty,
            updateJobInfo = UpdateJobInfo{lastUpdateJobSubmitted = Nothing, lastUpdateJobCompleted = Nothing}
          }
    [] -> error "createArtistIfNotExists: impossible: insert of single Artist returned 0 rows"
    _ -> error "createArtistIfNotExists: impossible: insert of single Artist returned more than 1 row"

-- Retrieval.

getArtist :: (MonadApp m) => ArtistID -> m (Maybe Artist)
getArtist artistID = do
  Config {postgres} <- ask
  listeners <-
    liftIO $
      query
        postgres
        "SELECT spotify_artist_listeners.created_at, monthly_listeners\
        \ FROM spotify_artist_listeners\
        \ JOIN spotify_artist ON spotify_artist.id = spotify_artist_listeners.spotify_artist_id\
        \ WHERE spotify_artist.id = ?"
        (Only artistID)
  artist <-
    liftIO $
      query
        postgres
        "SELECT spotify_artist_id, spotify_url, name, last_update_job_submitted, last_update_job_completed FROM spotify_artist WHERE id = ?"
        (Only artistID)
  return $ case artist of
    [(spotifyArtistID, spotifyURL, name, lastUpdateJobSubmitted, lastUpdateJobCompleted)] ->
      Just
        Artist
          { artistID,
            spotifyArtist = SpotifyArtist {spotifyArtistID, spotifyURL, name},
            monthlyListeners = fromList $ map (first utctDay) listeners,
            updateJobInfo = UpdateJobInfo{lastUpdateJobSubmitted, lastUpdateJobCompleted}
          }
    [] -> Nothing
    _ -> error $ "getArtist: impossible: selected multiple Artists with id " <> show artistID

-- Operations.

refreshArtistInsightsTimeoutDays :: (Integral n) => n
refreshArtistInsightsTimeoutDays = 15

refreshArtistInsights :: (MonadApp m) => AnonymousBearerToken -> Artist -> m Artist
refreshArtistInsights bearerToken artist@Artist {artistID, spotifyArtist = SpotifyArtist {spotifyArtistID}, monthlyListeners} = do
  Config {postgres} <- ask
  now <- liftIO getCurrentTime
  let today = utctDay now
  maybeNewSample <- case viaNonEmpty head $ reverse $ sort $ HashMap.toList monthlyListeners of
    Just (sampleDay, _) ->
      if cdDays (diffGregorianDurationClip today sampleDay) > refreshArtistInsightsTimeoutDays
        then Just <$> updateInsights postgres
        else return Nothing
    _ -> Just <$> updateInsights postgres
  return $ case maybeNewSample of
    Just newSample ->
      artist {monthlyListeners = HashMap.insert today newSample monthlyListeners} :: Artist
    Nothing -> artist
  where
    updateInsights conn = do
      SpotifyArtistInsights {monthlyListeners = newListenerSample} <-
        liftIO $ getSpotifyArtistInsights bearerToken spotifyArtistID
      void $ liftIO $
        execute
          conn
          "INSERT INTO spotify_artist_listeners\
          \ (spotify_artist_id, created_at, monthly_listeners)\
          \ VALUES (?, NOW(), ?)"
          (artistID, newListenerSample)
      return newListenerSample

-- Update status.

getUpdateStatus :: (MonadApp m) => ArtistID -> m UpdateStatus
getUpdateStatus = getUpdateStatusRaw "spotify_artist"

setUpdateSubmitted :: (MonadApp m) => ArtistID -> m ()
setUpdateSubmitted = setUpdateSubmittedRaw "spotify_artist"

setUpdateCompleted :: (MonadApp m) => ArtistID -> m ()
setUpdateCompleted = setUpdateCompletedRaw "spotify_artist"
