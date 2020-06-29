module Hipsterfy.Artist
  ( Artist (..),
    ArtistID (..),
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
import Data.Map (insert, toDescList)
import Data.Time (Day, cdDays, diffGregorianDurationClip, getCurrentTime, utctDay)
import Database.PostgreSQL.Simple (Only (..), execute, query)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Hipsterfy.Application (Config (..), MonadApp)
import Hipsterfy.Jobs (UpdateStatus (..), getUpdateStatusRaw, setUpdateCompletedRaw, setUpdateSubmittedRaw)
import Hipsterfy.Spotify (SpotifyArtist (..), SpotifyArtistInsights (..), getSpotifyArtistInsights)
import Hipsterfy.Spotify.Auth (AnonymousBearerToken)
import Relude

newtype ArtistID = ArtistID Int
  deriving (Show, Eq, Ord, ToField, FromField, FromJSON, ToJSON)

data Artist = Artist
  { artistID :: ArtistID,
    spotifyArtist :: SpotifyArtist,
    monthlyListeners :: Map Day Int
  }
  deriving (Show, Eq, Ord)

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
            monthlyListeners = mempty
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
        "SELECT spotify_artist_id, spotify_url, name FROM spotify_artist WHERE id = ?"
        (Only artistID)
  return $ case artist of
    [(spotifyArtistID, spotifyURL, name)] ->
      Just
        Artist
          { artistID,
            spotifyArtist = SpotifyArtist {spotifyArtistID, spotifyURL, name},
            monthlyListeners = fromList $ map (first utctDay) listeners
          }
    [] -> Nothing
    _ -> error $ "getArtist: impossible: selected multiple Artists with id " <> show artistID

-- Operations.

refreshArtistInsights :: (MonadApp m) => AnonymousBearerToken -> Artist -> m Artist
refreshArtistInsights bearerToken artist@Artist {artistID, spotifyArtist = SpotifyArtist {spotifyArtistID}, monthlyListeners} = do
  Config {postgres} <- ask
  -- TODO: add a check for whether listeners need to be updated (whether it's a new month)?
  now <- liftIO getCurrentTime
  let today = utctDay now
  maybeNewSample <- case viaNonEmpty head $ toDescList monthlyListeners of
    Just (sampleDay, _) ->
      if cdDays (diffGregorianDurationClip today sampleDay) > 15
        then Just <$> updateInsights postgres
        else return Nothing
    _ -> Just <$> updateInsights postgres
  return $ case maybeNewSample of
    Just newSample ->
      artist {monthlyListeners = insert today newSample monthlyListeners} :: Artist
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
