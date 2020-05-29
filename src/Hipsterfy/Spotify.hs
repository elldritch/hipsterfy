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
import Data.Aeson ((.:), FromJSON (..), Value (Object))
import Data.Aeson.Types (typeMismatch)
import Data.Fixed (Fixed (MkFixed))
import Data.Text (unpack)
import Data.Text.Encoding.Base64 (encodeBase64)
import qualified Data.Text.Lazy as LT
import Data.Time (UTCTime, addUTCTime, getCurrentTime, secondsToNominalDiffTime)
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
    refresh_token :: Text,
    scope :: Text
  }
  deriving (Generic)

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
requestAccessTokenFromRefreshToken app (SpotifyCredentials {refreshToken}) =
  requestAccessToken
    app
    [ "grant_type" := ("refresh_token" :: Text),
      "refresh_token" := refreshToken
    ]

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
        refreshToken = refresh_token body,
        expiration = addUTCTime (secondsToNominalDiffTime $ MkFixed $ toInteger $ expires_in body) now
      }
  where
    secret :: Text
    secret = encodeBase64 $ clientID <> ":" <> clientSecret

data SpotifyUserObjectResponse = SpotifyUserObjectResponse
  { spotifyID :: Text
  }
  deriving (Generic)

instance FromJSON SpotifyUserObjectResponse where
  parseJSON (Object v) = SpotifyUserObjectResponse <$> v .: "id"
  parseJSON invalid = typeMismatch "user" invalid

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
  return $ spotifyID body

data SpotifyArtist = SpotifyArtist
  { spotifyArtistID :: Text,
    spotifyURL :: Text,
    name :: Text,
    followers :: Int,
    popularity :: Int,
    monthlyListeners :: Int
  }

getFollowedSpotifyArtists :: (MonadIO m) => SpotifyCredentials -> m [SpotifyArtist]
getFollowedSpotifyArtists creds = undefined

getSpotifyArtistMonthlyListeners :: (MonadIO m) => SpotifyArtist -> m (Maybe Int)
getSpotifyArtistMonthlyListeners artistID = undefined
