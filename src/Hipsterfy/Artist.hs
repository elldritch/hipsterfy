module Hipsterfy.Artist (getArtistInsights, refreshArtistInsights) where

import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query)
import Hipsterfy.Spotify (AnonymousBearerToken, SpotifyArtist (..), SpotifyArtistInsights (..), getSpotifyArtistInsights)
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Relude

getArtistInsights :: (MonadIO m) => Connection -> AnonymousBearerToken -> SpotifyArtist -> m SpotifyArtistInsights
getArtistInsights conn bearerToken artist@SpotifyArtist {spotifyArtistID} = do
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
        \ LIMIT 1"
        (spotifyArtistID, now)
  case rows of
    [Only monthlyListeners] -> return SpotifyArtistInsights {monthlyListeners}
    [] -> refreshArtistInsights conn bearerToken artist
    _ -> error "impossible: select of single SpotifyArtistInsights returned more than 1 row"

refreshArtistInsights :: (MonadIO m) => Connection -> AnonymousBearerToken -> SpotifyArtist -> m SpotifyArtistInsights
refreshArtistInsights conn bearerToken artist@SpotifyArtist {name, spotifyArtistID, spotifyURL, followers, genres, popularity} = do
  -- Save artist profile.
  now <- liftIO getCurrentTime
  artistRows <-
    liftIO $
      query
        conn
        "INSERT INTO spotify_artist\
        \ (name, spotify_artist_id, spotify_url, followers, genres, popularity, last_updated, created_at)\
        \ VALUES\
        \ (?, ?, ?, ?, ?, ?, ?, ?)\
        \ ON CONFLICT (spotify_artist_id) DO UPDATE SET followers = ?, genres = ?, popularity = ?, last_updated = ?\
        \ RETURNING id"
        (name, spotifyArtistID, spotifyURL, followers, PGArray genres, popularity, now, now, followers, PGArray genres, popularity, now)
  artistID <- case artistRows of
    [Only artistID] -> return artistID
    _ -> error "impossible: insert of single SpotifyArtist returned 0 or more than 1 row"

  -- Save artist listeners.
  -- TODO: add a check to avoid sampling listeners multiple times?
  insights@SpotifyArtistInsights {monthlyListeners} <- liftIO $ getSpotifyArtistInsights bearerToken artist
  void $ liftIO $
    execute
      conn
      "INSERT INTO spotify_artist_listeners\
      \ (spotify_artist_id, created_at, monthly_listeners)\
      \ VALUES\
      \ (?, ?, ?)"
      (artistID :: Int, now, monthlyListeners)

  return insights
