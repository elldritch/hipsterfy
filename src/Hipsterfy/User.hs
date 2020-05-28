module Hipsterfy.User
  ( UserID,
    FriendCode,
    User (..),
    createUser,
    getUserBySpotifyID,
    getUserByFriendCode,
  )
where

import Data.Text (pack)
import Database.PostgreSQL.Simple (Connection, query)
import Database.PostgreSQL.Simple.Types (Only (Only))
import Hipsterfy.Spotify (SpotifyCredentials (..), SpotifyUserID)
import Relude
import Test.RandomStrings (randomASCII, randomWord)

type UserID = Int

type FriendCode = Text

data User = User
  { userID :: UserID,
    friendCode :: FriendCode,
    spotifyUserID :: SpotifyUserID,
    spotifyCredentials :: SpotifyCredentials
  }

createUser :: (MonadIO m, MonadFail m) => Connection -> SpotifyUserID -> SpotifyCredentials -> m User
createUser conn spotifyUserID creds = do
  friendCode <- liftIO $ randomWord randomASCII 20
  [Only userID] <-
    liftIO $
      ( query
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
          ) ::
          IO [Only Int]
      )
  return $
    User
      { userID = userID,
        friendCode = pack friendCode,
        spotifyUserID = spotifyUserID,
        spotifyCredentials = creds
      }

getUserBySpotifyID :: (MonadIO m) => Connection -> SpotifyUserID -> m (Maybe User)
getUserBySpotifyID conn spotifyUserID = do
  rows <-
    liftIO $
      query
        conn
        "SELECT\
        \ id, friend_code,\
        \ spotify_user_id, spotify_access_token, spotify_access_token_expiration, spotify_refresh_token\
        \ FROM hipsterfy_user\
        \ WHERE spotify_user_id = ?"
        (Only spotifyUserID)

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
                    expiration = spotifyAccessTokenExpiration,
                    scopes = [] -- TODO: should this be saved?
                  }
            }
    [] -> Nothing
    _ -> Nothing

getUserByFriendCode :: (MonadIO m) => Connection -> FriendCode -> m (Maybe user)
getUserByFriendCode conn friendCode = undefined
