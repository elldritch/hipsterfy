module Hipsterfy.User
  ( createOAuthRedirect,
    User (..),
    createUser,
    getUserBySpotifyID,
    getUserByFriendCode,
  )
where

import Control.Monad.Except (liftEither)
import Data.Text (pack)
import qualified Data.Text.Lazy as LT
import Database.PostgreSQL.Simple (Connection, Query, ToRow, execute, query)
import Database.PostgreSQL.Simple.Types (Only (Only))
import Hipsterfy.Spotify (Scope, SpotifyApp, SpotifyCredentials (..), getSpotifyUserID, redirectURI, requestAccessTokenFromAuthorizationCode)
import Relude
import Test.RandomStrings (randomASCII, randomWord)

data User = User
  { userID :: Int,
    friendCode :: Text,
    spotifyUserID :: Text,
    spotifyCredentials :: SpotifyCredentials
  }

-- Signup and creation.

createOAuthRedirect :: (MonadIO m) => SpotifyApp -> Connection -> [Scope] -> m LT.Text
createOAuthRedirect app conn scopes = do
  oauthState <- liftIO $ pack <$> randomWord randomASCII 20
  void $ liftIO $ execute conn "INSERT INTO spotify_oauth_request (oauth2_state) VALUES (?)" $ Only oauthState
  return $ redirectURI app scopes oauthState

createUser :: (MonadIO m) => SpotifyApp -> Connection -> Text -> Text -> m (Either Text User)
createUser app conn authCode oauthState =
  runExceptT $ do
    -- Validate the OAuth state, then delete that state.
    oauthStateRows <- liftIO (query conn "SELECT oauth2_state FROM spotify_oauth_request WHERE oauth2_state = ?" (Only oauthState) :: IO [Only Text])
    creds <- liftEither =<< case oauthStateRows of
      [_] -> do
        void $ liftIO $ execute conn "DELETE FROM spotify_oauth_request WHERE oauth2_state = ?" (Only oauthState)
        requestAccessTokenFromAuthorizationCode app authCode >>= return . Right
      _ -> return $ Left "invalid OAuth request state"

    -- Exchange OAuth authorization code for credentials.
    spotifyUserID <- liftIO $ getSpotifyUserID creds

    -- Construct a user if one doesn't already exist.
    spotifyUser <- lift $ getUserBySpotifyID conn spotifyUserID
    case spotifyUser of
      Just u -> return u
      Nothing -> do
        friendCode <- liftIO $ pack <$> randomWord randomASCII 20
        userRows <- liftIO $ insertUser friendCode spotifyUserID creds
        liftEither $ case userRows of
          [(Only userID)] ->
            Right $
              User
                { userID = userID,
                  friendCode = friendCode,
                  spotifyUserID = spotifyUserID,
                  spotifyCredentials = creds
                }
          _ -> Left "impossible: insert of single User returned zero or multiple IDs"
  where
    insertUser :: Text -> Text -> SpotifyCredentials -> IO [Only Int]
    insertUser friendCode spotifyUserID creds =
      query
        conn
        "INSERT INTO hipsterfy_user\
        \ (friend_code, spotify_user_id, spotify_access_token, spotify_access_token_expiration, spotify_refresh_token)\
        \ VALUES (?, ?, ?, ?, ?)\
        \ RETURNING id"
        ( friendCode,
          spotifyUserID,
          accessToken creds,
          expiration creds,
          refreshToken creds
        )

-- Retrieval.

getUserBySpotifyID :: (MonadIO m) => Connection -> Text -> m (Maybe User)
getUserBySpotifyID conn spotifyUserID =
  getUser
    conn
    "SELECT\
    \ id, friend_code,\
    \ spotify_user_id, spotify_access_token, spotify_access_token_expiration, spotify_refresh_token\
    \ FROM hipsterfy_user\
    \ WHERE spotify_user_id = ?"
    (Only spotifyUserID)

getUserByFriendCode :: (MonadIO m) => Connection -> Text -> m (Maybe User)
getUserByFriendCode conn friendCode =
  getUser
    conn
    "SELECT\
    \ id, friend_code,\
    \ spotify_user_id, spotify_access_token, spotify_access_token_expiration, spotify_refresh_token\
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
        spotifyUserID',
        spotifyAccessToken,
        spotifyAccessTokenExpiration,
        spotifyRefreshToken
        )
      ] ->
        Just $
          User
            { userID = userID,
              friendCode = friendCode,
              spotifyUserID = spotifyUserID',
              spotifyCredentials =
                SpotifyCredentials
                  { accessToken = spotifyAccessToken,
                    refreshToken = spotifyRefreshToken,
                    expiration = spotifyAccessTokenExpiration
                  }
            }
    [] -> Nothing
    _ -> Nothing