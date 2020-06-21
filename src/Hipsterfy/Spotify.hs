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
    getSpotifyArtistsOfSavedTracks,
    getSpotifyArtistsOfSavedAlbums,
    SpotifyArtistInsights (..),
    getSpotifyArtistInsights,
    AnonymousBearerToken,
    getAnonymousBearerToken,
  )
where

import Control.Lens ((.~), (^.))
import Control.Monad.Loops (unfoldrM)
import Data.Aeson ((.:), FromJSON (..), eitherDecode, withObject)
import Data.Text.Encoding.Base64 (encodeBase64)
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

redirectURI :: SpotifyApp -> [Scope] -> Text -> LText
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

spotifyTokenURL :: String
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
          spotifyTokenURL
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
  parseJSON = withObject "user" $ \o ->
    SpotifyUser <$> o .: "id" <*> o .: "display_name"

spotifyAPIURL :: String
spotifyAPIURL = "https://api.spotify.com/v1"

requestSpotifyAPI :: (MonadIO m, FromJSON t) => SpotifyCredentials -> String -> m t
requestSpotifyAPI SpotifyCredentials {accessToken} url = do
  res <-
    liftIO $
      getWith
        (defaults & header "Authorization" .~ ["Bearer " <> encodeUtf8 accessToken])
        url
  let x = eitherDecode $ res ^. responseBody
  case x of
    Right r -> return r
    Left e -> do
      putStrLn "JSON RESPONSE"
      putStrLn $ decodeUtf8 $ res ^. responseBody
      putStrLn "JSON ERROR"
      error $ toText e

getSpotifyUser :: (MonadIO m) => SpotifyCredentials -> m SpotifyUser
getSpotifyUser creds = requestSpotifyAPI creds $ spotifyAPIURL <> "/me"

data SpotifyArtist = SpotifyArtist
  { spotifyArtistID :: Text,
    spotifyURL :: Text,
    name :: Text,
    followers :: Int,
    genres :: [Text],
    images :: [SpotifyArtistImage],
    popularity :: Int
  }
  deriving (Show)

instance Eq SpotifyArtist where
  (==) = (==) `on` (spotifyArtistID :: SpotifyArtist -> Text)

instance Ord SpotifyArtist where
  compare = comparing (spotifyArtistID :: SpotifyArtist -> Text)

instance FromJSON SpotifyArtist where
  parseJSON v = do
    simplifiedArtist <- parseJSON v
    let SpotifySimplifiedArtist {spotifyArtistID, spotifyURL, name} = simplifiedArtist
    withObject
      "artist"
      ( \o -> do
          followersObject <- o .: "followers"
          followers <- withObject "followers" (.: "total") followersObject
          genres <- o .: "genres"
          images <- o .: "images"
          popularity <- o .: "popularity"
          return SpotifyArtist {spotifyArtistID, spotifyURL, name, followers, genres, images, popularity}
      )
      v

data SpotifyArtistImage = SpotifyArtistImage
  { height :: Int,
    width :: Int,
    url :: Text
  }
  deriving (Show, Generic, Eq)

instance FromJSON SpotifyArtistImage

getSpotifyArtist :: (MonadIO m) => SpotifyCredentials -> String -> m SpotifyArtist
getSpotifyArtist creds artistID = requestSpotifyAPI creds $ spotifyAPIURL <> "/artists/" <> artistID

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
requestSpotifyAPIPages' creds resToPage = unfoldPages responseToPages
  where
    responseToPages u = resToPage <$> requestSpotifyAPI creds u

{- HLINT ignore SpotifyFollowedArtistsResponse "Use newtype instead of data" -}
data SpotifyFollowedArtistsResponse = SpotifyFollowedArtistsResponse
  { artists :: SpotifyPagedResponse SpotifyArtist
  }
  deriving (Show, Generic)

instance FromJSON SpotifyFollowedArtistsResponse

getFollowedSpotifyArtists :: (MonadIO m) => SpotifyCredentials -> m [SpotifyArtist]
getFollowedSpotifyArtists creds =
  requestSpotifyAPIPages' creds artists $ spotifyAPIURL <> "/me/following?type=artist&limit=50"

data SpotifySimplifiedArtist = SpotifySimplifiedArtist
  { spotifyArtistID :: Text,
    spotifyURL :: Text,
    name :: Text
  }
  deriving (Show)

instance FromJSON SpotifySimplifiedArtist where
  parseJSON = withObject "simplified artist object" $ \o -> do
    spotifyArtistID <- o .: "id"
    urls <- o .: "external_urls"
    spotifyURL <- withObject "external_urls" (.: "spotify") urls
    name <- o .: "name"
    return SpotifySimplifiedArtist {spotifyArtistID, spotifyURL, name}

instance Eq SpotifySimplifiedArtist where
  (==) = (==) `on` (spotifyArtistID :: SpotifySimplifiedArtist -> Text)

instance Ord SpotifySimplifiedArtist where
  compare = comparing (spotifyArtistID :: SpotifySimplifiedArtist -> Text)

{- HLINT ignore SpotifyTrack "Use newtype instead of data" -}
data SpotifyTrack = SpotifyTrack
  { spotifyTrackArtists :: [SpotifySimplifiedArtist]
  }

instance FromJSON SpotifyTrack where
  parseJSON = withObject "track item" $ \item -> do
    track <- item .: "track"
    spotifyTrackArtists <- withObject "track" (\t -> (t .: "artists") >>= parseJSON) track
    return SpotifyTrack {spotifyTrackArtists}

getSpotifyArtistsOfSavedTracks :: (MonadIO m) => SpotifyCredentials -> m [SpotifySimplifiedArtist]
getSpotifyArtistsOfSavedTracks creds = do
  tracks <- requestSpotifyAPIPages creds $ spotifyAPIURL <> "/me/tracks"
  -- TODO: maybe we should hit the database to check the cache here?
  return $ ordNub $ concatMap spotifyTrackArtists tracks

{- HLINT ignore SpotifyAlbum "Use newtype instead of data" -}
data SpotifyAlbum = SpotifyAlbum
  { spotifyAlbumArtists :: [SpotifySimplifiedArtist]
  }

instance FromJSON SpotifyAlbum where
  parseJSON = withObject "album item" $ \item -> do
    album <- item .: "album"
    spotifyAlbumArtists <- withObject "album" (\t -> (t .: "artists") >>= parseJSON) album
    return SpotifyAlbum {spotifyAlbumArtists}

getSpotifyArtistsOfSavedAlbums :: (MonadIO m) => SpotifyCredentials -> m [SpotifySimplifiedArtist]
getSpotifyArtistsOfSavedAlbums creds = do
  albums <- requestSpotifyAPIPages creds $ spotifyAPIURL <> "/me/albums"
  return $ ordNub $ concatMap spotifyAlbumArtists albums

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
      ( \o -> do
          monthlyListeners <- o .: "monthly_listeners"
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
          (toString $ "https://spclient.wg.spotify.com/open-backend-2/v1/artists/" <> spotifyArtistID)
  return (res ^. responseBody :: SpotifyArtistInsights)
