module Hipsterfy.Session (getSession, startSession, endSession) where

import Data.Time (secondsToDiffTime)
import Database.PostgreSQL.Simple (Connection, execute, query)
import Database.PostgreSQL.Simple.Types (Only (Only))
import Hipsterfy.Spotify (SpotifyCredentials (..))
import Hipsterfy.User (User (..))
import Relude
import Test.RandomStrings (randomASCII, randomWord)
import Web.Cookie (SetCookie (..), defaultSetCookie)
import Web.Scotty.Cookie (deleteCookie, getCookie, setCookie)
import Web.Scotty.Trans (ActionT, ScottyError)

hipsterfyCookieName :: Text
hipsterfyCookieName = "hipsterfy_user"

getSession :: (MonadIO m, ScottyError e) => Connection -> ActionT e m (Maybe User)
getSession conn = do
  cookie <- getCookie hipsterfyCookieName
  case cookie of
    Nothing -> return Nothing
    Just c -> do
      -- TODO: handle exception case where user is not found.
      [ ( userID,
          friendCode,
          spotifyUserID,
          spotifyAccessToken,
          spotifyAccessTokenExpiration,
          spotifyRefreshToken
          )
        ] <-
        liftIO $
          query
            conn
            "SELECT\
            \ hipsterfy_user.id, friend_code,\
            \ spotify_user_id, spotify_access_token, spotify_access_token_expiration, spotify_refresh_token\
            \ FROM hipsterfy_user_session JOIN hipsterfy_user ON hipsterfy_user_session.user_id = hipsterfy_user.id\
            \ WHERE hipsterfy_user_session.cookie_secret = ?"
            (Only c)
      return $ Just $
        User
          { userID = userID,
            friendCode = friendCode,
            spotifyUserID = spotifyUserID,
            spotifyCredentials =
              SpotifyCredentials
                { accessToken = spotifyAccessToken,
                  refreshToken = spotifyRefreshToken,
                  expiration = spotifyAccessTokenExpiration
                }
          }

startSession :: (MonadIO m, ScottyError e) => Connection -> User -> ActionT e m ()
startSession conn (User {userID}) = do
  -- Create a new session in the database.
  cookieSecret <- liftIO $ randomWord randomASCII 20
  void $ liftIO $
    execute
      conn
      "INSERT INTO hipsterfy_user_session\
      \ (user_id, cookie_secret)\
      \ VALUES (?, ?)"
      ( userID,
        cookieSecret
      )

  -- Set session cookies.
  setCookie
    ( defaultSetCookie
        { setCookieName = encodeUtf8 hipsterfyCookieName,
          setCookieValue = encodeUtf8 cookieSecret,
          setCookiePath = Just "/",
          setCookieMaxAge = Just $ secondsToDiffTime $ 60 * 60 * 24 * 365 * 2
        }
    )

endSession :: (MonadIO m, ScottyError e) => Connection -> ActionT e m ()
endSession conn = do
  cookie <- getCookie hipsterfyCookieName
  case cookie of
    Nothing -> return ()
    Just cookieSecret -> do
      -- Find and delete session in the database.
      void $ liftIO $ execute conn "DELETE FROM hipsterfy_user_session WHERE cookie_secret = ?" (Only cookieSecret)

      -- Delete session cookies.
      deleteCookie hipsterfyCookieName
