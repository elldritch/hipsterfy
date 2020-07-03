module Hipsterfy.Spotify.Auth
  ( SpotifyApp (..),
    SpotifyCredentials (..),
    authorizationURL,
    requestAccessTokenFromAuthorizationCode,
    requestAccessTokenFromRefreshToken,
    Scope,
    scopeUserFollowRead,
    scopeUserLibraryRead,
    scopeUserTopRead,
    AnonymousBearerToken (..),
    getAnonymousBearerToken,
  )
where

import Control.Lens ((.~))
import Data.Aeson (FromJSON)
import Data.Text.Encoding.Base64 (encodeBase64)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Hipsterfy.Spotify.Internal (requestAsJSON)
import Network.HTTP.Types (renderSimpleQuery)
import Network.Wreq (FormParam (..), defaults, get, header, postWith)
import Relude hiding (get)
import Relude.Unsafe (fromJust)

-- Constant API URLs.

spotifyAuthURL :: Text
spotifyAuthURL = "https://accounts.spotify.com/authorize"

spotifyTokenURL :: String
spotifyTokenURL = "https://accounts.spotify.com/api/token"

-- Data structures for authentication and authorization.

-- | A @SpotifyApp@ represents the configuration of the Spotify application.
data SpotifyApp = SpotifyApp
  { -- | The application client ID.
    clientID :: Text,
    -- | The application client secret.
    clientSecret :: Text,
    -- | The URI of the application, for Spotify to redirect back to after
    -- authorization.
    redirectURI :: Text
  }
  deriving (Show)

-- | @SpotifyCredentials@ represent the credentials for a single user.
data SpotifyCredentials = SpotifyCredentials
  { -- | The user's access token.
    accessToken :: Text,
    -- | The user's refresh token.
    refreshToken :: Text,
    -- | The deadline after which the access token is expired.
    expiration :: UTCTime
  }
  deriving (Show, Generic)

-- Scopes.

-- | A @Scope@ represents an authorization scope for the Spotify API. For details,
-- see <https://developer.spotify.com/documentation/general/guides/scopes/ Authorization Scopes>.
newtype Scope = Scope Text deriving (Show, Eq)

-- | @scopeUserLibraryRead@ is the authorization scope
-- @<https://developer.spotify.com/documentation/general/guides/scopes/#user-library-read user-library-read>@.
scopeUserLibraryRead :: Scope
scopeUserLibraryRead = Scope "user-library-read"

-- | @scopeUserFollowRead@ is the authorization scope
-- @<https://developer.spotify.com/documentation/general/guides/scopes/#user-follow-read user-follow-read>@.
scopeUserFollowRead :: Scope
scopeUserFollowRead = Scope "user-follow-read"

-- | @scopeUserTopRead@ is the authorization scope
-- @<https://developer.spotify.com/documentation/general/guides/scopes/#user-top-read user-top-read>@.
scopeUserTopRead :: Scope
scopeUserTopRead = Scope "user-top-read"

-- Generating a redirect URI for authorization.

-- | @authorizationURL@ generates a URI to redirect users to authorize the
-- application to access their Spotify account.
--
-- For more details, see step 1 in
-- <https://developer.spotify.com/documentation/general/guides/authorization-guide/#authorization-code-flow Authorization Code Flow>.
authorizationURL ::
  -- | The Spotify application's configuration.
  SpotifyApp ->
  -- | The authorization scopes to request.
  [Scope] ->
  -- | The OAuth state of the request, which protects against CSRF. See step 1 in
  -- <https://developer.spotify.com/documentation/general/guides/authorization-guide/#authorization-code-flow Authorization Code Flow>.
  Text ->
  -- | The URI to redirect users to.
  LText
authorizationURL SpotifyApp {clientID, redirectURI} scopes oauthState =
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
            ("redirect_uri", encodeUtf8 redirectURI),
            ("state", encodeUtf8 oauthState),
            ("scope", encodeUtf8 $ renderScopes scopes)
          ]

-- Obtaining access tokens.

data SpotifyTokenResponse = SpotifyTokenResponse
  { access_token :: Text,
    token_type :: Text,
    expires_in :: Int,
    refresh_token :: Maybe Text,
    scope :: Text
  }
  deriving (Generic, Show)

instance FromJSON SpotifyTokenResponse

-- | @requestAccessTokenFromAuthorizationCode@ exchanges a user's authorization
-- code (provided by Spotify redirecting back to the application after
-- authorization) for their access and refresh token.
--
-- For more details, see step 2 in
-- <https://developer.spotify.com/documentation/general/guides/authorization-guide/#authorization-code-flow Authorization Code Flow>.
requestAccessTokenFromAuthorizationCode ::
  (MonadIO m) =>
  -- | The Spotify application's configuration.
  SpotifyApp ->
  -- | The authorization code provided by Spotify.
  Text ->
  -- | The user's access and refresh token.
  m SpotifyCredentials
requestAccessTokenFromAuthorizationCode app code = do
  creds <-
    requestAccessToken
      app
      [ "grant_type" := ("authorization_code" :: Text),
        "code" := code,
        "redirect_uri" := redirectURI app
      ]
  expiration <- expirationToDeadline $ expires_in creds
  return $
    SpotifyCredentials
      { accessToken = access_token creds,
        refreshToken = fromJust $ refresh_token creds,
        ..
      }

-- | @requestAccessTokenFromAuthorizationCode@ refreshes a user's access token
-- using their refresh token.
--
-- Refreshing an access token invalidates previous access tokens generated with
-- the same refresh token.
--
-- For more details, see step 4 in
-- <https://developer.spotify.com/documentation/general/guides/authorization-guide/#authorization-code-flow Authorization Code Flow>.
requestAccessTokenFromRefreshToken ::
  (MonadIO m) =>
  -- | The Spotify application's configuration.
  SpotifyApp ->
  -- | The user's previous access and refresh tokens.
  SpotifyCredentials ->
  -- | The user's refreshed access and refresh tokens.
  m SpotifyCredentials
requestAccessTokenFromRefreshToken app SpotifyCredentials {refreshToken} = do
  refreshedCreds <-
    requestAccessToken
      app
      [ "grant_type" := ("refresh_token" :: Text),
        "refresh_token" := refreshToken
      ]
  expiration <- expirationToDeadline $ expires_in refreshedCreds
  return $ SpotifyCredentials {accessToken = access_token refreshedCreds, ..}

-- | expirationToDeadline converts an integer expiration (in seconds) to a
-- deadline by adding it to the current time.
expirationToDeadline :: (MonadIO m) => Int -> m UTCTime
expirationToDeadline expiration = do
  now <- liftIO getCurrentTime
  return $ addUTCTime (fromInteger $ toInteger expiration :: NominalDiffTime) now

-- | requestAccessToken makes requests for API tokens.
requestAccessToken :: (MonadIO m) => SpotifyApp -> [FormParam] -> m SpotifyTokenResponse
requestAccessToken SpotifyApp {..} params =
  requestAsJSON $
    postWith
      (defaults & header "Authorization" .~ ["Basic " <> encodeUtf8 secret])
      spotifyTokenURL
      params
  where
    secret :: Text
    secret = encodeBase64 $ clientID <> ":" <> clientSecret

-- Anonymous bearer tokens.

-- | @AnonymousBearerToken@s are used to make anonymous web player API requests.
newtype AnonymousBearerToken = AnonymousBearerToken Text deriving (Show, Eq)

data SpotifyAnonymousBearerTokenResponse = SpotifyAnonymousBearerTokenResponse
  { clientId :: Text,
    accessToken :: Text,
    accessTokenExpirationTimestampMs :: Int,
    isAnonymous :: Bool
  }
  deriving (Generic)

instance FromJSON SpotifyAnonymousBearerTokenResponse

-- | @getAnonymousBearerToken@ requests a new 'AnonymousBearerToken' from the
-- web player API.
getAnonymousBearerToken :: (MonadIO m) => m AnonymousBearerToken
getAnonymousBearerToken = do
  res <- requestAsJSON $ get "https://open.spotify.com/get_access_token?reason=transport&productType=web_player"
  return $ AnonymousBearerToken $ accessToken (res :: SpotifyAnonymousBearerTokenResponse)
