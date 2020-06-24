module Hipsterfy.Artist
  ( Artist (..),
    ArtistID (..),
    createArtistIfNotExists,
    getArtist,
    refreshArtistInsights,
    UpdateStatus (..),
    getUpdateStatus,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (insert, toDescList)
import Data.Time (Day, cdDays, diffGregorianDurationClip, getCurrentTime, utctDay)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Hipsterfy.Jobs (UpdateStatus (..), getUpdateStatusRaw)
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

createArtistIfNotExists :: (MonadIO m) => Connection -> SpotifyArtist -> m Artist
createArtistIfNotExists conn SpotifyArtist {spotifyArtistID, spotifyURL, name} = do
  artistRows <-
    liftIO $
      query
        conn
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

getArtist :: (MonadIO m) => Connection -> ArtistID -> m (Maybe Artist)
getArtist conn artistID = do
  listeners <-
    liftIO $
      query
        conn
        "SELECT spotify_artist_listeners.created_at, monthly_listeners\
        \ FROM spotify_artist_listeners\
        \ JOIN spotify_artist ON spotify_artist.id = spotify_artist_listeners.spotify_artist_id\
        \ WHERE spotify_artist.id = ?"
        (Only artistID)
  artist <-
    liftIO $
      query
        conn
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

-- Updating monthly listeners.

getUpdateStatus :: (MonadIO m) => Connection -> ArtistID -> m UpdateStatus
getUpdateStatus conn = getUpdateStatusRaw conn "spotify_artist"

refreshArtistInsights :: (MonadIO m) => Connection -> AnonymousBearerToken -> Artist -> m Artist
refreshArtistInsights conn bearerToken artist@Artist {artistID, spotifyArtist = SpotifyArtist {spotifyArtistID}, monthlyListeners} = do
  -- TODO: add a check for whether listeners need to be updated (whether it's a new month)?
  now <- liftIO getCurrentTime
  let today = utctDay now
  maybeNewSample <- case viaNonEmpty head $ toDescList monthlyListeners of
    Just (sampleDay, _) ->
      if cdDays (diffGregorianDurationClip today sampleDay) > 15
        then Just <$> updateInsights
        else return Nothing
    _ -> Just <$> updateInsights
  return $ case maybeNewSample of
    Just newSample ->
      artist {monthlyListeners = insert today newSample monthlyListeners} :: Artist
    Nothing -> artist
  where
    updateInsights :: (MonadIO m) => m Int
    updateInsights = do
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
