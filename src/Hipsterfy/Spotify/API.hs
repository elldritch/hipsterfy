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
import Control.Monad.Loops (unfoldrM)
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

-- TODO: can we fetch each offset in parallel, to make this faster at the cost of correctness?
unfoldPages :: (MonadIO m) => (String -> m (SpotifyPagedResponse t)) -> String -> m [t]
unfoldPages loadPage url = do
  pages <- unfoldrM unfoldPages' $ Just url
  return $ concatMap items pages
  where
    unfoldPages' u =
      case u of
        Nothing -> return Nothing
        Just currURL -> do
          page <- loadPage currURL
          return $ case next page of
            Just nextURL -> Just (page, Just $ toString nextURL)
            Nothing -> Just (page, Nothing)

requestSpotifyAPIPages :: (MonadIO m, FromJSON v) => SpotifyCredentials -> String -> m [v]
requestSpotifyAPIPages creds = requestSpotifyAPIPages' creds id

requestSpotifyAPIPages' :: (MonadIO m, FromJSON t) => SpotifyCredentials -> (t -> SpotifyPagedResponse v) -> String -> m [v]
requestSpotifyAPIPages' creds resToPage = unfoldPages loadPage
  where
    loadPage url = resToPage <$> requestSpotifyAPI creds url
