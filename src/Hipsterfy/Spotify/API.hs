module Hipsterfy.Spotify.API
  ( spotifyAPIURL,
    requestAsJSON,
    requestSpotifyAPI,
    requestSpotifyAPIPages,
    requestSpotifyAPIPages',
    SpotifyPagedResponse (..),
  )
where

import Control.Lens ((.~))
import qualified Control.Monad.Parallel as Parallel (mapM)
import Data.Aeson ((.:), FromJSON (..), withObject)
import Hipsterfy.Spotify.Auth (SpotifyCredentials (..))
import Hipsterfy.Spotify.Internal (requestAsJSON)
import Network.Wreq (defaults, getWith, header)
import Relude

spotifyAPIURL :: String
spotifyAPIURL = "https://api.spotify.com/v1"

requestSpotifyAPI :: (MonadIO m, FromJSON t) => SpotifyCredentials -> String -> m t
requestSpotifyAPI SpotifyCredentials {accessToken} url =
  requestAsJSON $
    getWith (defaults & header "Authorization" .~ ["Bearer " <> encodeUtf8 accessToken]) url

data SpotifyPagedResponse t = SpotifyPagedResponse
  { items :: [t],
    total :: Int,
    next :: Maybe Text
  }
  deriving (Show)

instance (FromJSON t) => FromJSON (SpotifyPagedResponse t) where
  parseJSON = withObject "Spotify API response" $ \o -> do
    items <- parseJSON =<< o .: "items"
    next <- o .: "next"
    total <- o .: "total"
    return SpotifyPagedResponse {items, next, total}

requestSpotifyAPIPages :: (MonadIO m, FromJSON v) => SpotifyCredentials -> String -> m (Int, [v])
requestSpotifyAPIPages creds = requestSpotifyAPIPages' creds id

requestSpotifyAPIPages' :: (MonadIO m, FromJSON t) => SpotifyCredentials -> (t -> SpotifyPagedResponse v) -> String -> m (Int, [v])
requestSpotifyAPIPages' creds resToPage url = do
  firstPage <- loadPage url
  let t = total firstPage
  -- WARNING: modifying the URL as a string this way is dangerous - we basically
  -- assume that the URL ends with a querystring.
  -- TODO: build a DSL for representing Spotify API queries.
  let pageURLs = fmap (\offset -> url <> "&offset=" <> show offset) [50, 100 .. t]
  pages <- liftIO $ Parallel.mapM loadPage pageURLs
  return (t, concat $ items <$> pages)
  where
    loadPage u = resToPage <$> requestSpotifyAPI creds u
