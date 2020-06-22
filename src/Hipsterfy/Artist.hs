module Hipsterfy.Artist
  ( Artist (..),
    getArtistInsights,
    getArtistInsights',
    getCachedArtistInsights,
    refreshArtistInsights,
    getArtistBySpotifyArtistID,
    insertArtistIfNotExists,
  )
where

import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query)
import Hipsterfy.Spotify (SpotifyArtist (..), SpotifyArtistInsights (..), getSpotifyArtistInsights)
import Hipsterfy.Spotify.Auth (AnonymousBearerToken)
import Relude

data Artist = Artist
  { artistID :: Int,
    spotifyArtist :: SpotifyArtist
  }

getArtistInsights :: (MonadIO m) => Connection -> AnonymousBearerToken -> SpotifyArtist -> m SpotifyArtistInsights
getArtistInsights conn bearerToken artist = do
  cachedInsights <- getCachedArtistInsights conn artist
  case cachedInsights of
    Just insights -> return insights
    Nothing -> refreshArtistInsights conn bearerToken artist

getArtistInsights' :: (MonadIO m) => Connection -> AnonymousBearerToken -> Text -> m SpotifyArtistInsights
getArtistInsights' conn bearerToken spotifyArtistID = do
  Artist {spotifyArtist} <- getArtistBySpotifyArtistID conn spotifyArtistID
  getArtistInsights conn bearerToken spotifyArtist

getCachedArtistInsights :: (MonadIO m) => Connection -> SpotifyArtist -> m (Maybe SpotifyArtistInsights)
getCachedArtistInsights conn SpotifyArtist {spotifyArtistID} = do
  now <- liftIO getCurrentTime
  rows <-
    liftIO $
      query
        conn
        "SELECT monthly_listeners\
        \ FROM spotify_artist_listeners\
        \ JOIN spotify_artist ON spotify_artist.id = spotify_artist_listeners.spotify_artist_id\
        \ WHERE spotify_artist.spotify_artist_id = ?\
        \ AND date_trunc('month', spotify_artist_listeners.created_at) = date_trunc('month', ? :: TIMESTAMP)\
        \ ORDER BY spotify_artist_listeners.created_at DESC\
        \ LIMIT 1"
        (spotifyArtistID, now)
  case rows of
    [Only monthlyListeners] -> return $ Just SpotifyArtistInsights {monthlyListeners}
    [] -> return Nothing
    _ -> error "impossible: select of single SpotifyArtistInsights returned more than 1 row"

refreshArtistInsights :: (MonadIO m) => Connection -> AnonymousBearerToken -> SpotifyArtist -> m SpotifyArtistInsights
refreshArtistInsights conn bearerToken spotifyArtist = do
  -- Save artist profile.
  now <- liftIO getCurrentTime
  Artist {artistID} <- insertArtistIfNotExists conn spotifyArtist

  -- Save artist listeners.
  -- TODO: add a check to avoid sampling listeners multiple times?
  insights@SpotifyArtistInsights {monthlyListeners} <- liftIO $ getSpotifyArtistInsights bearerToken spotifyArtist
  void $ liftIO $
    execute
      conn
      "INSERT INTO spotify_artist_listeners\
      \ (spotify_artist_id, created_at, monthly_listeners)\
      \ VALUES\
      \ (?, ?, ?)"
      (artistID :: Int, now, monthlyListeners)

  return insights

getArtistBySpotifyArtistID :: (MonadIO m) => Connection -> Text -> m Artist
getArtistBySpotifyArtistID conn spotifyArtistID = do
  rows <-
    liftIO $
      query
        conn
        "SELECT id, spotify_artist_id, spotify_url, name FROM spotify_artist WHERE spotify_artist_id = ?"
        (Only spotifyArtistID)
  case rows of
    [(artistID, spotifyArtistID, spotifyURL, name)] ->
      return Artist {artistID, spotifyArtist = SpotifyArtist {spotifyArtistID, spotifyURL, name}}
    [] -> error $ "getArtistInsights': could not find SpotifyArtist with spotify_artist_id " <> show spotifyArtistID
    _ -> error $ "getArtistInsights': impossible: selected multiple spotify_artist rows with spotify_artist_id " <> show spotifyArtistID

insertArtistIfNotExists :: (MonadIO m) => Connection -> SpotifyArtist -> m Artist
insertArtistIfNotExists conn SpotifyArtist {spotifyArtistID, spotifyURL, name} = do
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
      return Artist {artistID, spotifyArtist = SpotifyArtist {spotifyArtistID, spotifyURL, name}}
    _ -> error "impossible: insert of single SpotifyArtist returned 0 or more than 1 row"
