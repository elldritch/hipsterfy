module Hipsterfy.Session
  ( getSessionByCookieSecret,
    createSession,
    deleteSession,
  )
where

import Database.PostgreSQL.Simple (Connection, execute, query)
import Database.PostgreSQL.Simple.Types (Only (Only))
import Hipsterfy.Spotify.Auth (SpotifyCredentials (..))
import Hipsterfy.User (User (..))
import Monitor.Tracing (MonadTrace, childSpan)
import Monitor.Tracing.Zipkin (tag)
import Relude

getSessionByCookieSecret :: (MonadIO m, MonadTrace m) => Connection -> Text -> m (Maybe User)
getSessionByCookieSecret conn cookieSecret = do
  rows <-
    childSpan "QUERY getSessionByCookieSecret" $ do
      tag "params.cookieSecret" cookieSecret
      liftIO $
        query
          conn
          "SELECT\
          \ hipsterfy_user.id, friend_code, last_update_job_completed,\
          \ spotify_user_id, spotify_user_name, spotify_access_token, spotify_access_token_expiration, spotify_refresh_token\
          \ FROM hipsterfy_user_session JOIN hipsterfy_user ON hipsterfy_user_session.user_id = hipsterfy_user.id\
          \ WHERE hipsterfy_user_session.cookie_secret = ?"
          (Only cookieSecret)
  return $ case rows of
    [(userID, friendCode, lastUpdated, spotifyUserID, spotifyUserName, accessToken, expiration, refreshToken)] ->
      Just $
        User
          { userID,
            friendCode,
            lastUpdated,
            spotifyUserID,
            spotifyUserName,
            spotifyCredentials = SpotifyCredentials {accessToken, refreshToken, expiration}
          }
    [] -> Nothing
    _ -> error "impossible: multiple sessions have the same cookie secret"

createSession :: (MonadIO m, MonadTrace m) => Connection -> User -> Text -> m ()
createSession conn User {userID} cookieSecret =
  void
    $ childSpan "QUERY createSession"
    $ do
      tag "params.userID" $ show userID
      tag "params.cookieSecret" cookieSecret
      liftIO $
        execute
          conn
          "INSERT INTO hipsterfy_user_session\
          \ (user_id, cookie_secret, created_at)\
          \ VALUES (?, ?, NOW())"
          (userID, cookieSecret)

deleteSession :: (MonadIO m, MonadTrace m) => Connection -> Text -> m ()
deleteSession conn cookieSecret =
  void
    $ childSpan "QUERY deleteSession"
    $ do
      tag "params.cookieSecret" cookieSecret
      liftIO $
        execute
          conn
          "DELETE FROM hipsterfy_user_session WHERE cookie_secret = ?"
          (Only cookieSecret)
