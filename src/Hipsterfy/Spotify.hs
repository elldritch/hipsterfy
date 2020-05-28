module Hipsterfy.Spotify
  ( SpotifyApp (..),
    SpotifyCredentials (..),
    OAuthState,
    redirectURI,
    exchangeToken,
    Scope,
    scopeUserFollowRead,
    scopeUserLibraryRead,
    scopeUserTopRead,
    SpotifyUserID,
    getSpotifyUserID,
    SpotifyArtist (..),
    getSpotifyArtists,
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
import Database.PostgreSQL.Simple (Connection, execute, query)
import Network.HTTP.Types (renderSimpleQuery)
import Database.PostgreSQL.Simple.Types (Only (Only))
import Network.Wreq (FormParam ((:=)), asJSON, defaults, getWith, header, postWith, responseBody)
import Relude
import Test.RandomStrings (randomASCII, randomWord)

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

spotifyAuthURL :: Text
spotifyAuthURL = "https://accounts.spotify.com/authorize"

spotifyTokenURL :: Text
spotifyTokenURL = "https://accounts.spotify.com/api/token"

spotifyAPIURL :: Text
spotifyAPIURL = "https://api.spotify.com/v1"

newtype Scope = Scope ByteString deriving (Show)

scopeUserLibraryRead :: Scope
scopeUserLibraryRead = Scope "user-library-read"

scopeUserFollowRead :: Scope
scopeUserFollowRead = Scope "user-follow-read"

scopeUserTopRead :: Scope
scopeUserTopRead = Scope "user-top-read"

type OAuthState = ByteString

redirectURI :: (MonadIO m) => SpotifyApp -> Connection -> [Scope] -> m LT.Text
redirectURI app conn scopes = do
  oauthState <- liftIO $ randomWord randomASCII 20
  let url = redirectURI' app scopes $ encodeUtf8 oauthState
  void $ liftIO $ execute conn "INSERT INTO spotify_oauth_request (oauth2_secret) VALUES (?)" $ Only oauthState
  return url

redirectURI' :: SpotifyApp -> [Scope] -> OAuthState -> LT.Text
redirectURI' (SpotifyApp {clientID, redirectAddress}) scopes oauthState =
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

data SpotifyTokenResponse = SpotifyTokenResponse
  { access_token :: Text,
    token_type :: Text,
    expires_in :: Int,
    refresh_token :: Text,
    scope :: Text
  }
  deriving (Generic)

instance FromJSON SpotifyTokenResponse

exchangeToken :: (MonadIO m, MonadFail m) => SpotifyApp -> Connection -> ByteString -> OAuthState -> m (Either Text SpotifyCredentials)
exchangeToken sa conn code oauthState = do
  [Only count] <- liftIO (query conn "SELECT COUNT(*) FROM spotify_oauth_request WHERE oauth2_secret = ?" (Only oauthState) :: IO [Only Int])
  if count == 0
    then return $ Left "invalid OAuth request state"
    else do
      void $ liftIO $ execute conn "DELETE FROM spotify_oauth_request WHERE oauth2_secret = ?" (Only oauthState)
      exchangeToken' sa code >>= return . Right

exchangeToken' :: (MonadIO m) => SpotifyApp -> ByteString -> m SpotifyCredentials
exchangeToken' (SpotifyApp {clientID, clientSecret, redirectAddress}) code = do
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
    secret = encodeUtf8 $ encodeBase64 $ clientID <> ":" <> clientSecret
    parseScopes :: Text -> [Scope]
    parseScopes s = fmap (Scope . encodeUtf8) $ words s

type SpotifyUserID = Text

data SpotifyUserObjectResponse = SpotifyUserObjectResponse
  { spotifyID :: SpotifyUserID
  }
  deriving (Generic)

instance FromJSON SpotifyUserObjectResponse where
  parseJSON (Object v) = SpotifyUserObjectResponse <$> v .: "id"
  parseJSON invalid = typeMismatch "user" invalid

-- TODO: support refresh token.
getSpotifyUserID :: (MonadIO m) => SpotifyCredentials -> m SpotifyUserID
getSpotifyUserID (SpotifyCredentials {accessToken}) = do
  res <-
    liftIO $
      asJSON
        =<< getWith
          (defaults & header "Authorization" .~ ["Bearer " <> (encodeUtf8 accessToken)])
          (unpack $ spotifyAPIURL <> "/me")
  let body = res ^. responseBody
  return $ spotifyID body

type SpotifyArtistID = Text

data SpotifyArtist = SpotifyArtist
  { spotifyURL :: Text,
    name :: Text,
    followers :: Int,
    popularity :: Int,
    monthlyListeners :: Int
  }

getSpotifyArtists :: (MonadIO m) => SpotifyCredentials -> m [SpotifyArtist]
getSpotifyArtists creds = undefined

getSpotifyArtistMonthlyListeners :: (MonadIO m) => SpotifyCredentials -> SpotifyArtistID -> m (Maybe Int)
getSpotifyArtistMonthlyListeners creds artistID = undefined
