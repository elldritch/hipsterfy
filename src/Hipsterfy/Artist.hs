module Hipsterfy.Artist
  ( Artist (..),
    ArtistID (..),
    createArtistIfNotExists,
    getArtistBySpotifyArtistID,
    needsUpdate,
    refreshArtistInsightsIfNeeded,
  )
where

import Data.Time (Day, NominalDiffTime, diffUTCTime, getCurrentTime, utctDay)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Hipsterfy.Spotify (SpotifyArtist (..), SpotifyArtistID, SpotifyArtistInsights (..), getSpotifyArtistInsights)
import Hipsterfy.Spotify.Auth (AnonymousBearerToken)
import Relude

newtype ArtistID = ArtistID Int
  deriving (Show, Eq, Ord, ToField, FromField)

data Artist = Artist
  { artistID :: ArtistID,
    spotifyArtist :: SpotifyArtist,
    monthlyListeners :: Map Day Int
  } deriving (Show, Eq, Ord)

-- Creation.

createArtistIfNotExists :: (MonadIO m) => Connection -> SpotifyArtist -> m Artist
createArtistIfNotExists conn SpotifyArtist {spotifyArtistID, spotifyURL, name} = do
  now <- liftIO getCurrentTime
  artistRows <-
    liftIO $
      query
        conn
        "INSERT INTO spotify_artist\
        \ (name, spotify_artist_id, spotify_url, created_at)\
        \ VALUES\
        \ (?, ?, ?, ?)\
        \ ON CONFLICT (spotify_artist_id) DO UPDATE SET name = ?\
        \ RETURNING id"
        (name, spotifyArtistID, spotifyURL, now, name)
  case artistRows of
    [Only artistID] ->
      return
        Artist
          { artistID,
            spotifyArtist = SpotifyArtist {spotifyArtistID, spotifyURL, name},
            monthlyListeners = mempty
          }
    [] -> error "createArtistIfNotExists: impossible: insert of single SpotifyArtist returned 0 rows"
    _ -> error "createArtistIfNotExists: impossible: insert of single SpotifyArtist returned more than 1 row"

-- Retrieval.

getArtistBySpotifyArtistID :: (MonadIO m) => Connection -> SpotifyArtistID -> m Artist
getArtistBySpotifyArtistID conn spotifyArtistID = do
  listeners <-
    liftIO $
      query
        conn
        "SELECT spotify_artist_listeners.created_at, monthly_listeners\
        \ FROM spotify_artist_listeners\
        \ JOIN spotify_artist ON spotify_artist.id = spotify_artist_listeners.spotify_artist_id\
        \ WHERE spotify_artist.spotify_artist_id = ?"
        (Only spotifyArtistID)
  artist <-
    liftIO $
      query
        conn
        "SELECT id, spotify_artist_id, spotify_url, name FROM spotify_artist WHERE spotify_artist_id = ?"
        (Only spotifyArtistID)
  case artist of
    [(artistID, spotifyArtistID', spotifyURL, name)] ->
      return
        Artist
          { artistID,
            spotifyArtist = SpotifyArtist {spotifyArtistID = spotifyArtistID', spotifyURL, name},
            monthlyListeners = fromList $ map (first utctDay) listeners
          }
    [] -> error $ "getArtistInsights': could not find SpotifyArtist with spotify_artist_id " <> show spotifyArtistID
    _ -> error $ "getArtistInsights': impossible: selected multiple spotify_artist rows with spotify_artist_id " <> show spotifyArtistID

-- Updating monthly listeners.

needsUpdateInterval :: NominalDiffTime
needsUpdateInterval = 60 * 60 * 24 * 15

needsUpdate :: (MonadIO m) => Connection -> SpotifyArtistID -> m Bool
needsUpdate conn spotifyArtistID = do
  now <- liftIO getCurrentTime
  latest <-
    liftIO $
      query
        conn
        "SELECT spotify_artist_listeners.created_at\
        \ FROM spotify_artist_listeners\
        \ JOIN spotify_artist ON spotify_artist.id = spotify_artist_listeners.spotify_artist_id\
        \ WHERE spotify_artist.spotify_artist_id = ?\
        \ ORDER BY spotify_artist_listeners.created_at DESC\
        \ LIMIT 1"
        (Only spotifyArtistID)
  return $ case latest of
    [Only t] -> diffUTCTime now t > needsUpdateInterval
    [] -> True
    _ -> error $ "needsUpdate: impossible: found multiple artists with spotify ID " <> show spotifyArtistID

refreshArtistInsightsIfNeeded :: (MonadIO m) => Connection -> AnonymousBearerToken -> SpotifyArtistID -> m SpotifyArtistInsights
refreshArtistInsightsIfNeeded conn bearerToken spotifyArtistID = do
  now <- liftIO getCurrentTime
  insights@SpotifyArtistInsights {monthlyListeners} <- liftIO $ getSpotifyArtistInsights bearerToken spotifyArtistID
  void $ liftIO $
    execute
      conn
      "INSERT INTO spotify_artist_listeners\
      \ (spotify_artist_id, created_at, monthly_listeners)\
      \ SELECT spotify_artist.id, ?, ? FROM spotify_artist WHERE spotify_artist_id = ?"
      (now, monthlyListeners, spotifyArtistID)
  return insights
