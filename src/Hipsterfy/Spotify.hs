module Hipsterfy.Spotify
  ( SpotifyApp (..),
    SpotifyCredentials (..),
    redirectURI,
    requestAccessTokenFromAuthorizationCode,
    requestAccessTokenFromRefreshToken,
    Scope,
    scopeUserFollowRead,
    scopeUserLibraryRead,
    scopeUserTopRead,
    getSpotifyUserID,
    SpotifyArtist (..),
    getFollowedSpotifyArtists,
  )
where

import Control.Lens ((.~), (^.))
import Data.Aeson ((.:), FromJSON (..), withObject)
import Data.Text (unpack)
import Data.Text.Encoding.Base64 (encodeBase64)
import qualified Data.Text.Lazy as LT
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Network.HTTP.Types (renderSimpleQuery)
import Network.Wreq (FormParam ((:=)), asJSON, defaults, getWith, header, postWith, responseBody)
import Relude

data SpotifyApp = SpotifyApp
  { clientID :: Text,
    clientSecret :: Text,
    redirectAddress :: Text
  }

data SpotifyCredentials = SpotifyCredentials
  { accessToken :: Text,
    refreshToken :: Text,
    expiration :: UTCTime
  }
  deriving (Show)

spotifyAuthURL :: Text
spotifyAuthURL = "https://accounts.spotify.com/authorize"

newtype Scope = Scope Text deriving (Show)

scopeUserLibraryRead :: Scope
scopeUserLibraryRead = Scope "user-library-read"

scopeUserFollowRead :: Scope
scopeUserFollowRead = Scope "user-follow-read"

scopeUserTopRead :: Scope
scopeUserTopRead = Scope "user-top-read"

redirectURI :: SpotifyApp -> [Scope] -> Text -> LT.Text
redirectURI (SpotifyApp {clientID, redirectAddress}) scopes oauthState =
  fromStrict $ spotifyAuthURL <> qs
  where
    renderScopes :: [Scope] -> Text
    renderScopes ss = mconcat $ intersperse (" " :: Text) bss
      where
        bss :: [Text]
        bss = fmap (\(Scope s) -> s) ss
    qs :: Text
    qs =
      decodeUtf8 $
        renderSimpleQuery
          True
          [ ("client_id", encodeUtf8 clientID),
            ("response_type", "code"),
            ("redirect_uri", encodeUtf8 redirectAddress),
            ("state", encodeUtf8 oauthState),
            ("scope", encodeUtf8 $ renderScopes scopes)
          ]

spotifyTokenURL :: Text
spotifyTokenURL = "https://accounts.spotify.com/api/token"

data SpotifyTokenResponse = SpotifyTokenResponse
  { access_token :: Text,
    token_type :: Text,
    expires_in :: Int,
    refresh_token :: Maybe Text,
    scope :: Text
  }
  deriving (Generic, Show)

instance FromJSON SpotifyTokenResponse

requestAccessTokenFromAuthorizationCode :: (MonadIO m) => SpotifyApp -> Text -> m SpotifyCredentials
requestAccessTokenFromAuthorizationCode app code =
  requestAccessToken
    app
    [ "grant_type" := ("authorization_code" :: Text),
      "code" := code,
      "redirect_uri" := redirectAddress app
    ]

requestAccessTokenFromRefreshToken :: (MonadIO m) => SpotifyApp -> SpotifyCredentials -> m SpotifyCredentials
requestAccessTokenFromRefreshToken app (SpotifyCredentials {refreshToken}) = do
  refreshedCreds <-
    requestAccessToken
      app
      [ "grant_type" := ("refresh_token" :: Text),
        "refresh_token" := refreshToken
      ]
  return $ refreshedCreds {refreshToken = refreshToken}

requestAccessToken :: (MonadIO m) => SpotifyApp -> [FormParam] -> m SpotifyCredentials
requestAccessToken (SpotifyApp {clientID, clientSecret}) params = do
  res <-
    liftIO $
      asJSON
        =<< postWith
          (defaults & header "Authorization" .~ ["Basic " <> encodeUtf8 secret])
          (unpack spotifyTokenURL)
          params
  now <- liftIO getCurrentTime
  let body = res ^. responseBody
  return $
    SpotifyCredentials
      { accessToken = access_token body,
        -- WARNING: this is a hack because otherwise life gets annoying. This is
        -- because the refresh_token isn't returned as a field when the access
        -- token is retrieved from a refresh.
        refreshToken = case refresh_token body of
          Just t -> t
          Nothing -> "",
        expiration = addUTCTime (fromInteger $ toInteger $ expires_in body :: NominalDiffTime) now
      }
  where
    secret :: Text
    secret = encodeBase64 $ clientID <> ":" <> clientSecret

data SpotifyUserObjectResponse = SpotifyUserObjectResponse
  { spotifyUserID :: Text
  }
  deriving (Generic)

instance FromJSON SpotifyUserObjectResponse where
  parseJSON = withObject "user" $ \v -> SpotifyUserObjectResponse <$> v .: "id"

spotifyAPIURL :: Text
spotifyAPIURL = "https://api.spotify.com/v1"

getSpotifyUserID :: (MonadIO m) => SpotifyCredentials -> m Text
getSpotifyUserID (SpotifyCredentials {accessToken}) = do
  res <-
    liftIO $
      asJSON
        =<< getWith
          (defaults & header "Authorization" .~ ["Bearer " <> (encodeUtf8 accessToken)])
          (unpack $ spotifyAPIURL <> "/me")
  let body = res ^. responseBody
  return $ spotifyUserID body

data SpotifyArtist = SpotifyArtist
  { spotifyArtistID :: Text,
    spotifyURL :: Text,
    name :: Text,
    followers :: Int,
    popularity :: Int,
    monthlyListeners :: Int
  }
  deriving (Show)

data SpotifyFollowedArtistsResponse = SpotifyFollowedArtistsResponse
  { artists :: [SpotifyArtistObjectResponse],
    total :: Int,
    next :: Maybe Text
  }
  deriving (Show)

instance FromJSON SpotifyFollowedArtistsResponse where
  parseJSON = withObject "followed artists response" $ \o -> do
    artistsObject <- o .: "artists"
    withObject
      "artists"
      ( \v -> do
          artists <- (traceWithMessage "v" v) .: "items"
          next <- v .: "next"
          total <- v .: "total"
          return SpotifyFollowedArtistsResponse {artists, next, total}
      )
      artistsObject

data SpotifyArtistObjectResponse = SpotifyArtistObjectResponse
  { spotifyArtistID :: Text,
    spotifyURL :: Text,
    name :: Text,
    followers :: Int,
    popularity :: Int
  }
  deriving (Show)

instance FromJSON SpotifyArtistObjectResponse where
  parseJSON = withObject "artist" $ \v -> do
    spotifyArtistID <- v .: "id"
    urls <- v .: "external_urls"
    spotifyURL <- withObject "external_urls" (\u -> u .: "spotify") urls
    name <- v .: "name"
    followersObject <- v .: "followers"
    followers <- withObject "followers" (\f -> f .: "total") followersObject
    popularity <- v .: "popularity"
    return
      SpotifyArtistObjectResponse
        { spotifyArtistID,
          spotifyURL,
          name,
          followers,
          popularity
        }

traceWithMessage :: Show a => String -> a -> a
traceWithMessage msg x = trace (msg ++ ": " ++ show x) x

getFollowedSpotifyArtists :: (MonadIO m) => SpotifyCredentials -> m [SpotifyArtist]
getFollowedSpotifyArtists (SpotifyCredentials {accessToken}) = do
  -- res <-
  --   liftIO $
  --     asJSON
  --       =<< getWith
  --         (defaults & header "Authorization" .~ ["Bearer " <> (encodeUtf8 accessToken)])
  --         (unpack $ spotifyAPIURL <> "/me")
  res <-
    liftIO $
      getWith
        (defaults & header "Authorization" .~ ["Bearer " <> (encodeUtf8 accessToken)])
        (unpack $ spotifyAPIURL <> "/me/following?type=artist")
  print $ res ^. responseBody

  res2 <- liftIO $ asJSON res
  let body = traceWithMessage "followed artist response" $ res2 ^. responseBody :: SpotifyFollowedArtistsResponse
  print body
  undefined

-- getSpotifyUserID :: (MonadIO m) => SpotifyCredentials -> m Text
-- getSpotifyUserID (SpotifyCredentials {accessToken}) = do
--   res <-
--     liftIO $
--       asJSON
--         =<< getWith
--           (defaults & header "Authorization" .~ ["Bearer " <> (encodeUtf8 accessToken)])
--           (unpack $ spotifyAPIURL <> "/me")
--   let body = res ^. responseBody
--   return $ spotifyUserID body

getSpotifyArtistMonthlyListeners :: (MonadIO m) => SpotifyArtist -> m (Maybe Int)
getSpotifyArtistMonthlyListeners artistID = undefined
