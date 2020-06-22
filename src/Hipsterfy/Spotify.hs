module Hipsterfy.Spotify
  ( SpotifyUser (..),
    getSpotifyUser,
    SpotifyArtist (..),
    getFollowedSpotifyArtists,
    getSpotifyArtistsOfSavedTracks,
    getSpotifyArtistsOfSavedAlbums,
    SpotifyArtistInsights (..),
    getSpotifyArtistInsights,
  )
where

import Control.Lens ((.~))
import Data.Aeson ((.:), (.:?), FromJSON (..), withObject)
import Hipsterfy.Spotify.API (SpotifyPagedResponse, requestAsJSON, requestSpotifyAPI, requestSpotifyAPIPages, requestSpotifyAPIPages', spotifyAPIURL)
import Hipsterfy.Spotify.Auth (AnonymousBearerToken (..), SpotifyCredentials (..))
import Network.Wreq (defaults, getWith, header)
import Relude

data SpotifyUser = SpotifyUser
  { spotifyUserID :: Text,
    spotifyUserName :: Text
  }

instance FromJSON SpotifyUser where
  parseJSON = withObject "user" $ \o ->
    SpotifyUser <$> o .: "id" <*> o .: "display_name"

getSpotifyUser :: (MonadIO m) => SpotifyCredentials -> m SpotifyUser
getSpotifyUser creds = requestSpotifyAPI creds $ spotifyAPIURL <> "/me"

data SpotifyArtist = SpotifyArtist
  { spotifyArtistID :: Text,
    spotifyURL :: Text,
    name :: Text
  }
  deriving (Show)

instance Eq SpotifyArtist where
  (==) = (==) `on` spotifyArtistID

instance Ord SpotifyArtist where
  compare = comparing spotifyArtistID

instance FromJSON SpotifyArtist where
  parseJSON = withObject "artist" $ \o -> do
    spotifyArtistID <- o .: "id"
    urls <- o .: "external_urls"
    spotifyURL <- withObject "external_urls" (.: "spotify") urls
    name <- o .: "name"
    return SpotifyArtist {spotifyArtistID, spotifyURL, name}

data SpotifyArtistImage = SpotifyArtistImage
  { height :: Int,
    width :: Int,
    url :: Text
  }
  deriving (Show, Generic, Eq)

instance FromJSON SpotifyArtistImage

{- HLINT ignore SpotifyFollowedArtistsResponse "Use newtype instead of data" -}
data SpotifyFollowedArtistsResponse = SpotifyFollowedArtistsResponse
  { artists :: SpotifyPagedResponse SpotifyArtist
  }
  deriving (Show, Generic)

instance FromJSON SpotifyFollowedArtistsResponse

getFollowedSpotifyArtists :: (MonadIO m) => SpotifyCredentials -> m (Int, [SpotifyArtist])
getFollowedSpotifyArtists creds =
  requestSpotifyAPIPages' creds artists $ spotifyAPIURL <> "/me/following?type=artist&limit=50"

{- HLINT ignore SpotifyTrack "Use newtype instead of data" -}
data SpotifyTrack = SpotifyTrack
  { spotifyTrackArtists :: [SpotifyArtist]
  }

instance FromJSON SpotifyTrack where
  parseJSON = withObject "track item" $ \item -> do
    track <- item .: "track"
    spotifyTrackArtists <- withObject "track" (\t -> (t .: "artists") >>= parseJSON) track
    return SpotifyTrack {spotifyTrackArtists}

getSpotifyArtistsOfSavedTracks :: (MonadIO m) => SpotifyCredentials -> m (Int, [SpotifyArtist])
getSpotifyArtistsOfSavedTracks creds = do
  (total, tracks) <- requestSpotifyAPIPages creds $ spotifyAPIURL <> "/me/tracks?limit=50"
  return (total, ordNub $ concatMap spotifyTrackArtists tracks)

{- HLINT ignore SpotifyAlbum "Use newtype instead of data" -}
data SpotifyAlbum = SpotifyAlbum
  { spotifyAlbumArtists :: [SpotifyArtist]
  }

instance FromJSON SpotifyAlbum where
  parseJSON = withObject "album item" $ \item -> do
    album <- item .: "album"
    spotifyAlbumArtists <- withObject "album" (\t -> (t .: "artists") >>= parseJSON) album
    return SpotifyAlbum {spotifyAlbumArtists}

getSpotifyArtistsOfSavedAlbums :: (MonadIO m) => SpotifyCredentials -> m (Int, [SpotifyArtist])
getSpotifyArtistsOfSavedAlbums creds = do
  (total, albums) <- requestSpotifyAPIPages creds $ spotifyAPIURL <> "/me/albums?limit=50"
  return (total, ordNub $ concatMap spotifyAlbumArtists albums)

{- HLINT ignore SpotifyArtistInsights "Use newtype instead of data" -}
data SpotifyArtistInsights = SpotifyArtistInsights
  { monthlyListeners :: Int
  }
  deriving (Show, Eq, Ord)

instance FromJSON SpotifyArtistInsights where
  parseJSON = withObject "artist insights response" $ \res -> do
    insights <- res .: "artistInsights"
    withObject
      "artistInsights"
      ( \o -> do
          listeners <- o .:? "monthly_listeners"
          followers <- o .: "follower_count"
          return $ SpotifyArtistInsights {monthlyListeners = fromMaybe followers listeners}
      )
      insights

getSpotifyArtistInsights :: (MonadIO m) => AnonymousBearerToken -> SpotifyArtist -> m SpotifyArtistInsights
getSpotifyArtistInsights (AnonymousBearerToken bearerToken) SpotifyArtist {spotifyArtistID} =
  requestAsJSON $
    getWith
      (defaults & header "Authorization" .~ ["Bearer " <> encodeUtf8 bearerToken])
      (toString $ "https://spclient.wg.spotify.com/open-backend-2/v1/artists/" <> spotifyArtistID)
