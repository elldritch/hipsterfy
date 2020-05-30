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
    SpotifyUser (..),
    getSpotifyUser,
    SpotifyArtist (..),
    getFollowedSpotifyArtists,
    SpotifyArtistInsights (..),
    getSpotifyArtistInsights,
    AnonymousBearerToken,
    getAnonymousBearerToken,
  )
where

import Control.Lens ((.~), (^.))
import Control.Monad.Loops (unfoldrM)
import Data.Aeson ((.:), FromJSON (..), withObject)
import Data.Text (unpack)
import Data.Text.Encoding.Base64 (encodeBase64)
import qualified Data.Text.Lazy as Lazy
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Network.HTTP.Types (renderSimpleQuery)
import Network.Wreq (FormParam ((:=)), asJSON, defaults, get, getWith, header, postWith, responseBody)
import Relude hiding (get)

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

redirectURI :: SpotifyApp -> [Scope] -> Text -> Lazy.Text
redirectURI SpotifyApp {clientID, redirectAddress} scopes oauthState =
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
requestAccessTokenFromRefreshToken app SpotifyCredentials {refreshToken} = do
  refreshedCreds <-
    requestAccessToken
      app
      [ "grant_type" := ("refresh_token" :: Text),
        "refresh_token" := refreshToken
      ]
  return $ refreshedCreds {refreshToken = refreshToken}

requestAccessToken :: (MonadIO m) => SpotifyApp -> [FormParam] -> m SpotifyCredentials
requestAccessToken SpotifyApp {clientID, clientSecret} params = do
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
        refreshToken = fromMaybe "" $ refresh_token body,
        expiration = addUTCTime (fromInteger $ toInteger $ expires_in body :: NominalDiffTime) now
      }
  where
    secret :: Text
    secret = encodeBase64 $ clientID <> ":" <> clientSecret

data SpotifyUser = SpotifyUser
  { spotifyUserID :: Text,
    spotifyUserName :: Text
  }

instance FromJSON SpotifyUser where
  parseJSON = withObject "user" $ \v ->
    SpotifyUser <$> v .: "id" <*> v .: "display_name"

spotifyAPIURL :: Text
spotifyAPIURL = "https://api.spotify.com/v1"

getSpotifyUser :: (MonadIO m) => SpotifyCredentials -> m SpotifyUser
getSpotifyUser SpotifyCredentials {accessToken} = do
  res <-
    liftIO $
      asJSON
        =<< getWith
          (defaults & header "Authorization" .~ ["Bearer " <> encodeUtf8 accessToken])
          (unpack $ spotifyAPIURL <> "/me")
  return $ res ^. responseBody

data SpotifyArtist = SpotifyArtist
  { spotifyArtistID :: Text,
    spotifyURL :: Text,
    name :: Text,
    followers :: Int,
    genres :: [Text],
    images :: [SpotifyArtistImage],
    popularity :: Int
  }
  deriving (Show, Eq)

instance FromJSON SpotifyArtist where
  parseJSON = withObject "artist" $ \v -> do
    spotifyArtistID <- v .: "id"
    urls <- v .: "external_urls"
    spotifyURL <- withObject "external_urls" (.: "spotify") urls
    name <- v .: "name"
    followersObject <- v .: "followers"
    followers <- withObject "followers" (.: "total") followersObject
    genres <- v .: "genres"
    images <- v .: "images"
    popularity <- v .: "popularity"
    return SpotifyArtist {spotifyArtistID, spotifyURL, name, followers, genres, images, popularity}

data SpotifyArtistImage = SpotifyArtistImage
  { height :: Int,
    width :: Int,
    url :: Text
  }
  deriving (Show, Generic, Eq)

instance FromJSON SpotifyArtistImage

data SpotifyFollowedArtistsResponse = SpotifyFollowedArtistsResponse
  { artists :: [SpotifyArtist],
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
          artists <- v .: "items"
          next <- v .: "next"
          total <- v .: "total"
          return SpotifyFollowedArtistsResponse {artists, next, total}
      )
      artistsObject

getFollowedSpotifyArtists :: (MonadIO m) => SpotifyCredentials -> m [SpotifyArtist]
getFollowedSpotifyArtists SpotifyCredentials {accessToken} = do
  -- Load all followed artists.
  pages <- liftIO $ unfoldPages $ unpack $ spotifyAPIURL <> "/me/following?type=artist&limit=50"
  let followedArtists = concatMap artists pages
  return followedArtists
  where
    unfoldPages :: String -> IO [SpotifyFollowedArtistsResponse]
    unfoldPages url = unfoldrM unfoldPages' $ Just url
    unfoldPages' :: Maybe String -> IO (Maybe (SpotifyFollowedArtistsResponse, Maybe String))
    unfoldPages' u = case u of
      Nothing -> return Nothing
      Just url -> do
        page <- loadPage url
        return $ case next page of
          Just url' -> Just (page, Just $ unpack url')
          Nothing -> Just (page, Nothing)
    loadPage :: String -> IO SpotifyFollowedArtistsResponse
    loadPage url = do
      res <-
        asJSON
          =<< getWith
            (defaults & header "Authorization" .~ ["Bearer " <> encodeUtf8 accessToken])
            url
      return $ res ^. responseBody

data SpotifyAnonymousBearerTokenResponse = SpotifyAnonymousBearerTokenResponse
  { clientId :: Text,
    accessToken :: Text,
    accessTokenExpirationTimestampMs :: Int,
    isAnonymous :: Bool
  }
  deriving (Generic)

instance FromJSON SpotifyAnonymousBearerTokenResponse

newtype AnonymousBearerToken = AnonymousBearerToken Text

getAnonymousBearerToken :: (MonadIO m) => m AnonymousBearerToken
getAnonymousBearerToken = do
  res <- liftIO $ asJSON =<< get "https://open.spotify.com/get_access_token?reason=transport&productType=web_player"
  return $ AnonymousBearerToken $ accessToken (res ^. responseBody :: SpotifyAnonymousBearerTokenResponse)

{- HLINT ignore SpotifyArtistInsights "Use newtype instead of data" -}
data SpotifyArtistInsights = SpotifyArtistInsights
  { monthlyListeners :: Int
  }
  deriving (Show, Eq)

instance FromJSON SpotifyArtistInsights where
  parseJSON = withObject "artist insights response" $ \res -> do
    insights <- res .: "artistInsights"
    withObject
      "artistInsights"
      ( \v -> do
          monthlyListeners <- v .: "monthly_listeners"
          return $ SpotifyArtistInsights {monthlyListeners}
      )
      insights

getSpotifyArtistInsights :: (MonadIO m) => AnonymousBearerToken -> SpotifyArtist -> m SpotifyArtistInsights
getSpotifyArtistInsights (AnonymousBearerToken bearerToken) SpotifyArtist {spotifyArtistID} = do
  res <-
    liftIO $
      asJSON
        =<< getWith
          (defaults & header "Authorization" .~ ["Bearer " <> encodeUtf8 bearerToken])
          (unpack $ "https://spclient.wg.spotify.com/open-backend-2/v1/artists/" <> spotifyArtistID)
  return (res ^. responseBody :: SpotifyArtistInsights)
