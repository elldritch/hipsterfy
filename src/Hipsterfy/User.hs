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
import Data.Time (UTCTime, getCurrentTime)
import Database.PostgreSQL.Simple (Connection, Only (Only), Query, ToRow, execute, fromOnly, query, withTransaction)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Hipsterfy.Artist (Artist (..), ArtistID, getArtist)
import Hipsterfy.Jobs (UpdateStatus (..), getUpdateStatusRaw, setUpdateCompletedRaw, setUpdateSubmittedRaw)
import Hipsterfy.Spotify
  ( SpotifyUser (..),
    SpotifyUserID,
    getSpotifyUser,
  )
import Hipsterfy.Spotify.Auth
  ( Scope,
    SpotifyApp,
    SpotifyCredentials (..),
    authorizationURL,
    requestAccessTokenFromAuthorizationCode,
    requestAccessTokenFromRefreshToken,
  )
import Relude
import Test.RandomStrings (randomASCII, randomWord)

newtype UserID = UserID Int
  deriving (Show, Eq, Ord, FromJSON, ToJSON, ToField, FromField)

data User = User
  { userID :: UserID,
    friendCode :: Text,
    spotifyUserID :: SpotifyUserID,
    spotifyUserName :: Text,
    spotifyCredentials :: SpotifyCredentials,
    lastUpdated :: Maybe UTCTime
  }

-- Signup and creation.

createOAuthRedirect :: (MonadIO m) => SpotifyApp -> Connection -> [Scope] -> m LText
createOAuthRedirect app conn scopes = do
  oauthState <- liftIO $ toText <$> randomWord randomASCII 20
  void $ liftIO $ execute conn "INSERT INTO spotify_oauth_request (oauth2_state, created_at) VALUES (?, NOW())" (Only oauthState)
  return $ authorizationURL app scopes oauthState

createUser :: (MonadIO m) => SpotifyApp -> Connection -> Text -> Text -> m (Either Text User)
createUser app conn authCode oauthState =
  runExceptT $ do
    -- Validate the OAuth state, then delete that state.
    oauthStateRows <- liftIO (query conn "SELECT oauth2_state FROM spotify_oauth_request WHERE oauth2_state = ?" (Only oauthState) :: IO [Only Text])
    spotifyCredentials <- liftEither =<< case oauthStateRows of
      [_] -> do
        void $ liftIO $ execute conn "DELETE FROM spotify_oauth_request WHERE oauth2_state = ?" (Only oauthState)
        Right <$> requestAccessTokenFromAuthorizationCode app authCode
      _ -> throwError "invalid OAuth request state"

    -- Exchange OAuth authorization code for credentials.
    spotifyUser@SpotifyUser {spotifyUserID, spotifyUserName} <- liftIO $ getSpotifyUser spotifyCredentials

    -- Construct a user if one doesn't already exist.
    user <- lift $ getUserBySpotifyID conn spotifyUserID
    case user of
      Just u -> return u
      Nothing -> do
        friendCode <- liftIO $ toText <$> randomWord randomASCII 20
        userRows <- liftIO $ insertUser friendCode spotifyUser spotifyCredentials
        case userRows of
          [Only userID] -> return $ User {userID, friendCode, spotifyUserID, spotifyUserName, spotifyCredentials, lastUpdated = Nothing}
          [] -> error "createUser: impossible: insert of single User returned 0 rows"
          _ -> error "createUser: impossible: insert of single User returned more than 1 row"
  where
    insertUser :: Text -> SpotifyUser -> SpotifyCredentials -> IO [Only UserID]
    insertUser friendCode SpotifyUser {spotifyUserID, spotifyUserName} SpotifyCredentials {accessToken, expiration, refreshToken} =
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

getUserByID :: (MonadIO m) => Connection -> UserID -> m (Maybe User)
getUserByID conn userID =
  getUser
    conn
    "SELECT\
    \ id, friend_code,\
    \ spotify_user_id, spotify_user_name, spotify_access_token, spotify_access_token_expiration, spotify_refresh_token,\
    \ last_update_job_completed\
    \ FROM hipsterfy_user\
    \ WHERE id = ?"
    (Only userID)

getUserBySpotifyID :: (MonadIO m) => Connection -> SpotifyUserID -> m (Maybe User)
getUserBySpotifyID conn spotifyUserID =
  getUser
    conn
    "SELECT\
    \ id, friend_code,\
    \ spotify_user_id, spotify_user_name, spotify_access_token, spotify_access_token_expiration, spotify_refresh_token,\
    \ last_update_job_completed\
    \ FROM hipsterfy_user\
    \ WHERE spotify_user_id = ?"
    (Only spotifyUserID)

getUserByFriendCode :: (MonadIO m) => Connection -> Text -> m (Maybe User)
getUserByFriendCode conn friendCode =
  getUser
    conn
    "SELECT\
    \ id, friend_code,\
    \ spotify_user_id, spotify_user_name, spotify_access_token, spotify_access_token_expiration, spotify_refresh_token,\
    \ last_update_job_completed\
    \ FROM hipsterfy_user\
    \ WHERE friend_code = ?"
    (Only friendCode)

getUser :: (MonadIO m, ToRow q) => Connection -> Query -> q -> m (Maybe User)
getUser conn sql params = do
  rows <- liftIO $ query conn sql params
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

refreshCredentialsIfNeeded :: (MonadIO m) => SpotifyApp -> Connection -> UserID -> SpotifyCredentials -> m SpotifyCredentials
refreshCredentialsIfNeeded app conn userID creds = do
  now <- liftIO getCurrentTime
  if now > expiration creds
    then do
      refreshedCreds <- requestAccessTokenFromRefreshToken app creds
      liftIO $ updateCreds refreshedCreds
      return refreshedCreds
    else return creds
  where
    updateCreds :: SpotifyCredentials -> IO ()
    updateCreds SpotifyCredentials {accessToken, refreshToken, expiration} =
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

getFollowedArtists :: (MonadIO m) => Connection -> UserID -> m [Artist]
getFollowedArtists conn userID = do
  artists <-
    liftIO $
      mapM (getArtist conn . fromOnly)
        =<< query
          conn
          "SELECT\
          \ spotify_artist.id\
          \ FROM hipsterfy_user\
          \ JOIN hipsterfy_user_spotify_artist_follow ON hipsterfy_user_spotify_artist_follow.user_id = hipsterfy_user.id \
          \ JOIN spotify_artist ON spotify_artist.id = hipsterfy_user_spotify_artist_follow.spotify_artist_id\
          \ WHERE hipsterfy_user.id = ?"
          (Only userID)
  return $ catMaybes artists

setFollowedArtists :: (MonadIO m) => Connection -> UserID -> [ArtistID] -> m ()
setFollowedArtists conn userID artists =
  liftIO $ withTransaction conn $ do
    -- Clear previous follows.
    void $ liftIO $ execute conn "DELETE FROM hipsterfy_user_spotify_artist_follow WHERE user_id = ?;" (Only userID)
    -- Insert new follows.
    mapM_ setFollowedArtist artists
  where
    setFollowedArtist artistID = do
      void $ liftIO $
        execute
          conn
          "INSERT INTO hipsterfy_user_spotify_artist_follow\
          \ (user_id, spotify_artist_id)\
          \ VALUES (?, ?);"
          (userID, artistID)

-- Update status.

getUpdateStatus :: (MonadIO m) => Connection -> UserID -> m UpdateStatus
getUpdateStatus conn = getUpdateStatusRaw conn "hipsterfy_user"

setUpdateSubmitted :: (MonadIO m) => Connection -> UserID -> m ()
setUpdateSubmitted conn = setUpdateSubmittedRaw conn "hipsterfy_user"

setUpdateCompleted :: (MonadIO m) => Connection -> UserID -> m ()
setUpdateCompleted conn = setUpdateCompletedRaw conn "hipsterfy_user"
