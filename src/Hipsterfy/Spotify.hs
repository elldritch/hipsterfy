module Hipsterfy.Spotify
  ( redirectURI,
    exchangeToken,
    SpotifyApp (..),
    SpotifyCredentials (..),
    Scope,
    scopeUserFollowRead,
    scopeUserLibraryRead,
    scopeUserTopRead,
  )
where

import Control.Lens ((.~), (^.))
import Data.Aeson (FromJSON)
import Data.Fixed (Fixed (MkFixed))
import Data.Text (unpack)
import Data.Text.Encoding.Base64 (encodeBase64)
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types (renderSimpleQuery)
import Data.Time (UTCTime, addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Network.Wreq (FormParam ((:=)), asJSON, defaults, header, postWith, responseBody)
import Relude

data SpotifyApp = SpotifyApp
  { clientID :: Text,
    clientSecret :: Text,
    redirectAddress :: Text
  }

data SpotifyCredentials = SpotifyCredentials
  { accessToken :: Text,
    refreshToken :: Text,
    expiration :: UTCTime,
    scopes :: [Scope]
  }
  deriving (Show)

data SpotifyTokenResponse = SpotifyTokenResponse
  { access_token :: Text,
    token_type :: Text,
    expires_in :: Int,
    refresh_token :: Text,
    scope :: Text
  }
  deriving (Generic)

instance FromJSON SpotifyTokenResponse

spotifyAuthURL :: Text
spotifyAuthURL = "https://accounts.spotify.com/authorize"

spotifyTokenURL :: Text
spotifyTokenURL = "https://accounts.spotify.com/api/token"

newtype Scope = Scope ByteString deriving (Show)

scopeUserLibraryRead :: Scope
scopeUserLibraryRead = Scope "user-library-read"

scopeUserFollowRead :: Scope
scopeUserFollowRead = Scope "user-follow-read"

scopeUserTopRead :: Scope
scopeUserTopRead = Scope "user-top-read"

redirectURI :: SpotifyApp -> [Scope] -> ByteString -> LT.Text
redirectURI (SpotifyApp {clientID, redirectAddress}) scopes oauthState =
  fromStrict $ spotifyAuthURL <> qs
  where
    renderScopes :: [Scope] -> ByteString
    renderScopes ss = mconcat $ intersperse (" " :: ByteString) bss
      where
        bss :: [ByteString]
        bss = fmap (\(Scope s) -> s) ss
    qs :: Text
    qs =
      decodeUtf8 $
        renderSimpleQuery
          True
          [ ("client_id", encodeUtf8 clientID),
            ("response_type", "code"),
            ("redirect_uri", encodeUtf8 redirectAddress),
            ("state", oauthState),
            ("scope", renderScopes scopes)
          ]

exchangeToken :: (MonadIO m) => SpotifyApp -> ByteString -> m SpotifyCredentials
exchangeToken (SpotifyApp {clientID, clientSecret, redirectAddress}) code = do
  res <-
    liftIO $
      asJSON
        =<< postWith
          (defaults & header "Authorization" .~ ["Basic " <> secret])
          (unpack spotifyTokenURL)
          [ "grant_type" := ("authorization_code" :: Text),
            "code" := code,
            "redirect_uri" := redirectAddress
          ]
  now <- liftIO getCurrentTime
  let body = res ^. responseBody
  return $
    SpotifyCredentials
      { accessToken = access_token body,
        refreshToken = refresh_token body,
        expiration = addUTCTime (secondsToNominalDiffTime $ MkFixed $ toInteger $ expires_in body) now,
        scopes = parseScopes $ scope body
      }
  where
    secret :: ByteString
    secret = encodeUtf8 (encodeBase64 $ clientID <> ":" <> clientSecret)
    parseScopes :: Text -> [Scope]
    parseScopes s = fmap (\w -> Scope (encodeUtf8 w)) $ words s
