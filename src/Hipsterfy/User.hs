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
    FollowUpdateStatus (..),
    userFollowUpdateInProgress,
    userFollowUpdateInProgress',
    needsUpdate,
    startUserFollowUpdate,
    completeUserFollowUpdate,
    setFollowedArtists,
  )
where

import Control.Monad.Except (liftEither, throwError)
import Data.Aeson (FromJSON, ToJSON)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Database.PostgreSQL.Simple (fromOnly, Connection, Only (Only), Query, ToRow, execute, query, withTransaction)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Hipsterfy.Artist (getArtistBySpotifyArtistID, Artist (..), createArtistIfNotExists)
import Hipsterfy.Spotify
  ( SpotifyArtist (..),
    SpotifyUser (..),
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
import Relude.Unsafe (fromJust)
import Test.RandomStrings (randomASCII, randomWord)

newtype UserID = UserID Int
  deriving (Show, Eq, Ord, FromJSON, ToJSON, ToField, FromField)

data User = User
  { userID :: UserID,
    friendCode :: Text,
    spotifyUserID :: SpotifyUserID,
    spotifyUserName :: Text,
    spotifyCredentials :: SpotifyCredentials
  }

-- Signup and creation.

createOAuthRedirect :: (MonadIO m) => SpotifyApp -> Connection -> [Scope] -> m LText
createOAuthRedirect app conn scopes = do
  oauthState <- liftIO $ toText <$> randomWord randomASCII 20
  now <- liftIO getCurrentTime
  void $ liftIO $ execute conn "INSERT INTO spotify_oauth_request (oauth2_state, created_at) VALUES (?, ?)" (oauthState, now)
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
          [Only userID] -> return $ User {userID, friendCode, spotifyUserID, spotifyUserName, spotifyCredentials}
          [] -> error "createUser: impossible: insert of single User returned 0 rows"
          _ -> error "createUser: impossible: insert of single User returned more than 1 row"
  where
    insertUser :: Text -> SpotifyUser -> SpotifyCredentials -> IO [Only UserID]
    insertUser friendCode SpotifyUser {spotifyUserID, spotifyUserName} SpotifyCredentials {accessToken, expiration, refreshToken} = do
      now <- getCurrentTime
      query
        conn
        "INSERT INTO hipsterfy_user\
        \ (friend_code, spotify_user_id, spotify_user_name, spotify_access_token, spotify_access_token_expiration, spotify_refresh_token, created_at)\
        \ VALUES (?, ?, ?, ?, ?, ?, ?)\
        \ RETURNING id"
        ( friendCode,
          spotifyUserID,
          spotifyUserName,
          accessToken,
          expiration,
          refreshToken,
          now
        )

-- Retrieval.

getUserByID :: (MonadIO m) => Connection -> UserID -> m (Maybe User)
getUserByID conn userID =
  getUser
    conn
    "SELECT\
    \ id, friend_code,\
    \ spotify_user_id, spotify_user_name, spotify_access_token, spotify_access_token_expiration, spotify_refresh_token\
    \ FROM hipsterfy_user\
    \ WHERE id = ?"
    (Only userID)

getUserBySpotifyID :: (MonadIO m) => Connection -> SpotifyUserID -> m (Maybe User)
getUserBySpotifyID conn spotifyUserID =
  getUser
    conn
    "SELECT\
    \ id, friend_code,\
    \ spotify_user_id, spotify_user_name, spotify_access_token, spotify_access_token_expiration, spotify_refresh_token\
    \ FROM hipsterfy_user\
    \ WHERE spotify_user_id = ?"
    (Only spotifyUserID)

getUserByFriendCode :: (MonadIO m) => Connection -> Text -> m (Maybe User)
getUserByFriendCode conn friendCode =
  getUser
    conn
    "SELECT\
    \ id, friend_code,\
    \ spotify_user_id, spotify_user_name, spotify_access_token, spotify_access_token_expiration, spotify_refresh_token\
    \ FROM hipsterfy_user\
    \ WHERE friend_code = ?"
    (Only friendCode)

getUser :: (MonadIO m, ToRow q) => Connection -> Query -> q -> m (Maybe User)
getUser conn sql params = do
  -- TODO: is there a way we can compose the SQL query, so we can specify the
  -- columns and just take the `WHERE` clause as an argument?
  rows <- liftIO $ query conn sql params
  return $ case rows of
    [ ( userID,
        friendCode,
        spotifyUserID,
        spotifyUserName,
        accessToken,
        expiration,
        refreshToken
        )
      ] ->
        Just $
          User
            { userID,
              friendCode,
              spotifyUserID,
              spotifyUserName,
              spotifyCredentials = SpotifyCredentials {accessToken, refreshToken, expiration}
            }
    _ -> Nothing

-- Operations.

refreshCredentialsIfNeeded :: (MonadIO m) => SpotifyApp -> Connection -> User -> m SpotifyCredentials
refreshCredentialsIfNeeded app conn User {userID, spotifyCredentials} = do
  now <- liftIO getCurrentTime
  if now > expiration spotifyCredentials
    then do
      refreshedCreds <- requestAccessTokenFromRefreshToken app spotifyCredentials
      liftIO $ updateCreds refreshedCreds
      return refreshedCreds
    else return spotifyCredentials
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

getFollowedArtists :: (MonadIO m) => Connection -> User -> m (FollowUpdateStatus, [Artist])
getFollowedArtists conn User {userID} = do
  follows <-
    liftIO $
      query
        conn
        "SELECT\
        \ spotify_artist.spotify_artist_id\
        \ FROM hipsterfy_user\
        \ JOIN hipsterfy_user_spotify_artist_follow ON hipsterfy_user_spotify_artist_follow.user_id = hipsterfy_user.id \
        \ JOIN spotify_artist ON spotify_artist.id = hipsterfy_user_spotify_artist_follow.spotify_artist_id\
        \ WHERE hipsterfy_user.id = ?"
        (Only userID)
  artists <- mapM (getArtistBySpotifyArtistID conn . fromOnly) follows
  user <-
    liftIO $
      query
        conn
        "SELECT\
        \ follows_currently_updating,\
        \ follows_last_update_start,\
        \ follows_current_update_total\
        \ FROM hipsterfy_user WHERE hipsterfy_user.id = ?"
        (Only userID)
  status <- case user of
    [(currentlyUpdating, lastUpdateStart, currentUpdateTotal)] -> do
      now <- liftIO getCurrentTime
      return $
        if updateNotTimedOut currentlyUpdating lastUpdateStart now
          then InProgress (length artists) $ fromJust currentUpdateTotal
          else UpdatedAt lastUpdateStart
    [] -> error $ "getFollowedArtists: could not find user with ID " <> show userID
    _ -> error $ "getFollowedArtists: impossible: selected multiple users with ID " <> show userID

  return (status, artists)

-- Determining follow update status.

data FollowUpdateStatus = UpdatedAt UTCTime | InProgress Int Int

followUpdateTimeout :: NominalDiffTime
followUpdateTimeout = 60 * 10

userFollowUpdateInProgress :: (MonadIO m) => Connection -> User -> m Bool
userFollowUpdateInProgress conn User {userID} = userFollowUpdateInProgress' conn userID

userFollowUpdateInProgress' :: (MonadIO m) => Connection -> UserID -> m Bool
userFollowUpdateInProgress' conn userID = do
  status <- getUpdateStatus conn userID
  return $ case status of
    InProgress' -> True
    _ -> False

needsUpdateInterval :: NominalDiffTime
needsUpdateInterval = 60 * 60 * 24

needsUpdate :: (MonadIO m) => Connection -> User -> m Bool
needsUpdate conn User {userID} = do
  now <- liftIO getCurrentTime
  status <- getUpdateStatus conn userID
  return $ case status of
    UpdatedAt' t -> diffUTCTime now t > needsUpdateInterval
    InProgress' -> False

-- Determining follow update status: helpers.

updateNotTimedOut :: Bool -> UTCTime -> UTCTime -> Bool
updateNotTimedOut currentlyUpdating lastUpdateStart now =
  currentlyUpdating && diffUTCTime now lastUpdateStart < followUpdateTimeout

data FollowUpdateStatus' = UpdatedAt' UTCTime | InProgress'

getUpdateStatus :: (MonadIO m) => Connection -> UserID -> m FollowUpdateStatus'
getUpdateStatus conn userID = do
  row <-
    liftIO $
      query
        conn
        "SELECT\
        \ follows_currently_updating,\
        \ follows_last_update_start\
        \ FROM hipsterfy_user WHERE hipsterfy_user.id = ?"
        (Only userID)
  case row of
    [(currentlyUpdating, lastUpdateStart)] -> do
      now <- liftIO getCurrentTime
      return $
        if updateNotTimedOut currentlyUpdating lastUpdateStart now
          then InProgress'
          else UpdatedAt' lastUpdateStart
    [] -> error $ "getUpdateStatus: could not find user with ID " <> show userID
    _ -> error $ "getUpdateStatus: impossible: selected multiple users with ID " <> show userID

-- Updating followed artists.

startUserFollowUpdate :: (MonadIO m) => Connection -> User -> UTCTime -> Int -> m ()
startUserFollowUpdate conn User {userID} started total = do
  void $ liftIO $
    execute
      conn
      "UPDATE hipsterfy_user SET\
      \ follows_currently_updating = true,\
      \ follows_last_update_start = ?,\
      \ follows_current_update_total = ?\
      \ WHERE id = ?"
      (started, total, userID)

completeUserFollowUpdate :: (MonadIO m) => Connection -> User -> m ()
completeUserFollowUpdate conn User {userID} = do
  void $ liftIO $
    execute
      conn
      "UPDATE hipsterfy_user SET follows_currently_updating = false WHERE id = ?"
      (Only userID)

setFollowedArtists :: (MonadIO m) => Connection -> User -> [SpotifyArtist] -> m ()
setFollowedArtists conn User {userID} artists = do
  liftIO $ withTransaction conn $ do
    -- Clear previous follows.
    void $ liftIO $ execute conn "DELETE FROM hipsterfy_user_spotify_artist_follow WHERE user_id = ?;" (Only userID)
    -- Insert new follows.
    mapM_ setFollowedArtist artists
  where
    setFollowedArtist spotifyArtist = do
      Artist {artistID} <- createArtistIfNotExists conn spotifyArtist
      void $ liftIO $
        execute
          conn
          "INSERT INTO hipsterfy_user_spotify_artist_follow\
          \ (user_id, spotify_artist_id)\
          \ VALUES (?, ?);"
          (userID, artistID)
