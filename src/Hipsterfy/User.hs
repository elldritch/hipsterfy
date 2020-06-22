module Hipsterfy.User
  ( createOAuthRedirect,
    User (..),
    createUser,
    getUserByID,
    getUserBySpotifyID,
    getUserByFriendCode,
    getCredentials,
    startUserFollowUpdate,
    completeUserFollowUpdate,
    getFollowedArtists,
    setFollowedArtists,
  )
where

import Control.Monad.Except (liftEither, throwError)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Database.PostgreSQL.Simple (Connection, Only (Only), Query, ToRow, execute, query)
import Hipsterfy.Spotify
  ( SpotifyArtist (..),
    SpotifyUser (..),
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
import Relude.Unsafe (fromJust)

data User = User
  { userID :: Int,
    friendCode :: Text,
    spotifyUserID :: Text,
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
          _ -> error "impossible: insert of single User returned zero or more than 1 row"
  where
    insertUser :: Text -> SpotifyUser -> SpotifyCredentials -> IO [Only Int]
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

getUserByID :: (MonadIO m) => Connection -> Int -> m (Maybe User)
getUserByID conn userID =
  getUser
    conn
    "SELECT\
    \ id, friend_code,\
    \ spotify_user_id, spotify_user_name, spotify_access_token, spotify_access_token_expiration, spotify_refresh_token\
    \ FROM hipsterfy_user\
    \ WHERE id = ?"
    (Only userID)

getUserBySpotifyID :: (MonadIO m) => Connection -> Text -> m (Maybe User)
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

getCredentials :: (MonadIO m) => SpotifyApp -> Connection -> User -> m SpotifyCredentials
getCredentials app conn User {userID, spotifyCredentials} = do
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

-- TODO: Ideally, this would take place in some MonadDatabase, and its caller
-- would be in some MonadClock.
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

data UpdateStatus = UpdatedAt UTCTime | InProgress Int Int

followUpdateTimeout :: NominalDiffTime
followUpdateTimeout = 60 * 10

getFollowedArtists :: (MonadIO m) => Connection -> User -> m (UpdateStatus, [SpotifyArtist])
getFollowedArtists conn User {userID} = do
  rows <-
    liftIO $
      query
        conn
        "SELECT\
        \ spotify_artist.spotify_artist_id,\
        \ spotify_artist.spotify_url,\
        \ spotify_artist.name\
        \ FROM hipsterfy_user\
        \ JOIN hipsterfy_user_spotify_artist_follow ON hipsterfy_user_spotify_artist_follow.user_id = hipsterfy_user.id \
        \ JOIN spotify_artist ON spotify_artist.id = hipsterfy_user_spotify_artist_follow.spotify_artist_id\
        \ WHERE hipsterfy_user.id = ?"
        (Only userID)
  let artists = map (\(spotifyArtistID, spotifyURL, name) -> SpotifyArtist {spotifyArtistID, spotifyURL, name}) rows

  row <-
    liftIO $
      query
        conn
        "SELECT\
        \ follows_currently_updating,\
        \ follows_last_update_start,\
        \ follows_current_update_total\
        \ FROM hipsterfy_user WHERE hipsterfy_user.id = ?"
        (Only userID)
  status <- case row of
    [(currentlyUpdating, lastUpdateStart, currentUpdateTotal)] -> do
      now <- liftIO getCurrentTime
      return $
        if currentlyUpdating && diffUTCTime now lastUpdateStart < followUpdateTimeout
          then InProgress (length artists) $ fromJust currentUpdateTotal
          else UpdatedAt lastUpdateStart
    [] -> error $ "could not find user with ID " <> show userID
    _ -> error "getFollowedArtists: impossible: selected multiple users with same ID"

  return (status, artists)

setFollowedArtists :: (MonadIO m) => Connection -> User -> [SpotifyArtist] -> m ()
setFollowedArtists conn user artists = do
  undefined
