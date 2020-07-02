module Hipsterfy.User
  ( createOAuthRedirect,
    User (..),
    UserID (..),
    createUser,
    getUserByID,
    getUserBySpotifyID,
    getUserByFriendCode,
    refreshCredentialsIfNeeded,
    getFollowedArtists,
    setFollowedArtists,
    UpdateStatus (..),
    getUpdateStatus,
    setUpdateSubmitted,
    setUpdateCompleted,
  )
where

import Control.Monad.Except (liftEither, throwError)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map (fromList)
import Data.Time (UTCTime, getCurrentTime, utctDay)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (toList)
import Database.PostgreSQL.Simple
  ( Only (..),
    Query,
    ToRow,
    execute,
    query,
    withTransaction,
  )
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Hipsterfy.Application (Config (..), MonadApp)
import Hipsterfy.Artist (Artist (..), ArtistID)
import Hipsterfy.Jobs
  ( UpdateStatus (..),
    getUpdateStatusRaw,
    setUpdateCompletedRaw,
    setUpdateSubmittedRaw,
  )
import Hipsterfy.Spotify
  ( SpotifyArtist (..),
    SpotifyArtistID,
    SpotifyUser (..),
    SpotifyUserID,
    getSpotifyUser,
  )
import Hipsterfy.Spotify.Auth
  ( Scope,
    SpotifyCredentials (..),
    authorizationURL,
    requestAccessTokenFromAuthorizationCode,
    requestAccessTokenFromRefreshToken,
  )
import Monitor.Tracing (childSpan)
import Monitor.Tracing.Zipkin (tag)
import Opaleye (SqlInt4)
import Opaleye.Internal.RunQuery (DefaultFromField)
import Relude
import Test.RandomStrings (randomASCII, randomWord)

newtype UserID = UserID Int
  deriving (Show, Eq, Ord, FromJSON, ToJSON, ToField, FromField, DefaultFromField SqlInt4)

data User = User
  { userID :: UserID,
    friendCode :: Text,
    spotifyUserID :: SpotifyUserID,
    spotifyUserName :: Text,
    spotifyCredentials :: SpotifyCredentials,
    lastUpdated :: Maybe UTCTime
  }

-- Signup and creation.

createOAuthRedirect :: (MonadApp m) => [Scope] -> m LText
createOAuthRedirect scopes = do
  Config {postgres, spotifyApp} <- ask
  oauthState <- liftIO $ toText <$> randomWord randomASCII 20
  void $ liftIO $ execute postgres "INSERT INTO spotify_oauth_request (oauth2_state, created_at) VALUES (?, NOW())" (Only oauthState)
  return $ authorizationURL spotifyApp scopes oauthState

createUser :: (MonadApp m) => Text -> Text -> m (Either Text User)
createUser authCode oauthState =
  runExceptT $ do
    Config {spotifyApp, postgres} <- ask
    -- Validate the OAuth state, then delete that state.
    oauthStateRows <- liftIO (query postgres "SELECT oauth2_state FROM spotify_oauth_request WHERE oauth2_state = ?" (Only oauthState) :: IO [Only Text])
    spotifyCredentials <- liftEither =<< case oauthStateRows of
      [_] -> do
        void $ liftIO $ execute postgres "DELETE FROM spotify_oauth_request WHERE oauth2_state = ?" (Only oauthState)
        Right <$> requestAccessTokenFromAuthorizationCode spotifyApp authCode
      _ -> throwError "invalid OAuth request state"

    -- Exchange OAuth authorization code for credentials.
    spotifyUser@SpotifyUser {spotifyUserID, spotifyUserName} <- liftIO $ getSpotifyUser spotifyCredentials

    -- Construct a user if one doesn't already exist.
    user <- lift $ getUserBySpotifyID spotifyUserID
    case user of
      Just u -> return u
      Nothing -> do
        friendCode <- liftIO $ toText <$> randomWord randomASCII 20
        userRows <- liftIO $ insertUser postgres friendCode spotifyUser spotifyCredentials
        case userRows of
          [Only userID] -> return $ User {userID, friendCode, spotifyUserID, spotifyUserName, spotifyCredentials, lastUpdated = Nothing}
          [] -> error "createUser: impossible: insert of single User returned 0 rows"
          _ -> error "createUser: impossible: insert of single User returned more than 1 row"
  where
    insertUser conn friendCode SpotifyUser {spotifyUserID, spotifyUserName} SpotifyCredentials {accessToken, expiration, refreshToken} =
      query
        conn
        "INSERT INTO hipsterfy_user\
        \ (friend_code,\
        \  spotify_user_id,\
        \  spotify_user_name,\
        \  spotify_access_token,\
        \  spotify_access_token_expiration,\
        \  spotify_refresh_token,\
        \  created_at)\
        \ VALUES (?, ?, ?, ?, ?, ?, NOW())\
        \ RETURNING id"
        ( friendCode,
          spotifyUserID,
          spotifyUserName,
          accessToken,
          expiration,
          refreshToken
        )

-- Retrieval.

getUserByID :: (MonadApp m) => UserID -> m (Maybe User)
getUserByID userID =
  getUser
    "SELECT\
    \ id, friend_code,\
    \ spotify_user_id, spotify_user_name, spotify_access_token, spotify_access_token_expiration, spotify_refresh_token,\
    \ last_update_job_completed\
    \ FROM hipsterfy_user\
    \ WHERE id = ?"
    (Only userID)

getUserBySpotifyID :: (MonadApp m) => SpotifyUserID -> m (Maybe User)
getUserBySpotifyID spotifyUserID =
  getUser
    "SELECT\
    \ id, friend_code,\
    \ spotify_user_id, spotify_user_name, spotify_access_token, spotify_access_token_expiration, spotify_refresh_token,\
    \ last_update_job_completed\
    \ FROM hipsterfy_user\
    \ WHERE spotify_user_id = ?"
    (Only spotifyUserID)

getUserByFriendCode :: (MonadApp m) => Text -> m (Maybe User)
getUserByFriendCode friendCode =
  getUser
    "SELECT\
    \ id, friend_code,\
    \ spotify_user_id, spotify_user_name, spotify_access_token, spotify_access_token_expiration, spotify_refresh_token,\
    \ last_update_job_completed\
    \ FROM hipsterfy_user\
    \ WHERE friend_code = ?"
    (Only friendCode)

getUser :: (MonadApp m, ToRow q) => Query -> q -> m (Maybe User)
getUser sql params = do
  Config {postgres} <- ask
  rows <- liftIO $ query postgres sql params
  return $ case rows of
    [ ( userID,
        friendCode,
        spotifyUserID,
        spotifyUserName,
        accessToken,
        expiration,
        refreshToken,
        lastUpdated
        )
      ] ->
        Just $
          User
            { userID,
              friendCode,
              spotifyUserID,
              spotifyUserName,
              spotifyCredentials = SpotifyCredentials {accessToken, refreshToken, expiration},
              lastUpdated
            }
    [] -> Nothing
    _ -> error "getUser: impossible: selected multiple unique Users"

-- Operations.

refreshCredentialsIfNeeded :: (MonadApp m) => UserID -> SpotifyCredentials -> m SpotifyCredentials
refreshCredentialsIfNeeded userID creds = do
  Config {spotifyApp, postgres} <- ask
  now <- liftIO getCurrentTime
  if now > expiration creds
    then do
      refreshedCreds <- requestAccessTokenFromRefreshToken spotifyApp creds
      liftIO $ updateCreds postgres refreshedCreds
      return refreshedCreds
    else return creds
  where
    updateCreds conn SpotifyCredentials {accessToken, refreshToken, expiration} =
      void $
        execute
          conn
          "UPDATE hipsterfy_user\
          \ SET spotify_access_token = ?, spotify_access_token_expiration = ?, spotify_refresh_token = ?\
          \ WHERE id = ?"
          ( accessToken,
            expiration,
            refreshToken,
            userID
          )

getFollowedArtists :: (MonadApp m) => UserID -> m [Artist]
getFollowedArtists userID = do
  Config {postgres} <- ask
  artists <-
    childSpan "QUERY getFollowedArtists" $ do
      tag "kind" "QUERY"
      tag "user" $ show userID
      tag "http.route" ""
      tag "http.verb" ""
      tag "http.params.foo" ""
      tag "http.statusCode" ""
      tag "error" ""
      tag "name" "postgresql"
      tag "service.name" "postgresql"
      tag "db.query" "getFollowedArtists"
      tag "db.params.userID" $ show userID
      liftIO $
        query
          postgres
          "SELECT\
          \   spotify_artist.id, spotify_artist.spotify_artist_id, spotify_artist.spotify_url, spotify_artist.name,\
          \   ARRAY_AGG(spotify_artist_listeners.created_at), ARRAY_AGG(spotify_artist_listeners.monthly_listeners)\
          \ FROM hipsterfy_user\
          \ JOIN hipsterfy_user_spotify_artist_follow ON hipsterfy_user_spotify_artist_follow.user_id = hipsterfy_user.id\
          \ JOIN spotify_artist ON spotify_artist.id = hipsterfy_user_spotify_artist_follow.spotify_artist_id\
          \ JOIN spotify_artist_listeners ON spotify_artist_listeners.spotify_artist_id = spotify_artist.id\
          \ WHERE hipsterfy_user.id = ?\
          \ GROUP BY spotify_artist.id\
          \ ORDER BY spotify_artist.id ASC;"
          (Only userID)
  return $ map toArtist artists
  where
    toArtist :: (ArtistID, SpotifyArtistID, Text, Text, Vector UTCTime, Vector Int) -> Artist
    toArtist (artistID, spotifyArtistID, spotifyURL, name, sampleDates, sampleListeners) =
      Artist
        { artistID,
          spotifyArtist =
            SpotifyArtist
              { spotifyArtistID,
                spotifyURL,
                name
              },
          monthlyListeners = Map.fromList $ zip (utctDay <$> Vector.toList sampleDates) (Vector.toList sampleListeners)
        }

setFollowedArtists :: (MonadApp m) => UserID -> [ArtistID] -> m ()
setFollowedArtists userID artists = do
  Config {postgres} <- ask
  liftIO $ withTransaction postgres $ do
    -- Clear previous follows.
    void $ liftIO $ execute postgres "DELETE FROM hipsterfy_user_spotify_artist_follow WHERE user_id = ?;" (Only userID)
    -- Insert new follows.
    mapM_ (setFollowedArtist postgres) artists
  where
    setFollowedArtist conn artistID = do
      void $ liftIO $
        execute
          conn
          "INSERT INTO hipsterfy_user_spotify_artist_follow\
          \ (user_id, spotify_artist_id)\
          \ VALUES (?, ?);"
          (userID, artistID)

-- Update status.

getUpdateStatus :: (MonadApp m) => UserID -> m UpdateStatus
getUpdateStatus = getUpdateStatusRaw "hipsterfy_user"

setUpdateSubmitted :: (MonadApp m) => UserID -> m ()
setUpdateSubmitted = setUpdateSubmittedRaw "hipsterfy_user"

setUpdateCompleted :: (MonadApp m) => UserID -> m ()
setUpdateCompleted = setUpdateCompletedRaw "hipsterfy_user"
