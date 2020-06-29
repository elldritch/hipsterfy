module Hipsterfy.Session
  ( getSessionByCookieSecret,
    createSession,
    deleteSession,
  )
where

import Database.PostgreSQL.Simple (execute, query)
import Database.PostgreSQL.Simple.Types (Only (Only))
import Hipsterfy.Application (Config (..), MonadApp)
import Hipsterfy.Spotify.Auth (SpotifyCredentials (..))
import Hipsterfy.User (User (..))
import Monitor.Tracing (childSpan)
import Monitor.Tracing.Zipkin (tag)
import Relude

getSessionByCookieSecret :: (MonadApp m) => Text -> m (Maybe User)
getSessionByCookieSecret cookieSecret = do
  Config {postgres} <- ask
  rows <-
    childSpan "QUERY getSessionByCookieSecret" $ do
      tag "params.cookieSecret" cookieSecret
      liftIO $
        query
          postgres
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

createSession :: (MonadApp m) => User -> Text -> m ()
createSession User {userID} cookieSecret = do
  Config {postgres} <- ask
  void
    $ childSpan "QUERY createSession"
    $ do
      tag "params.userID" $ show userID
      tag "params.cookieSecret" cookieSecret
      liftIO $
        execute
          postgres
          "INSERT INTO hipsterfy_user_session\
          \ (user_id, cookie_secret, created_at)\
          \ VALUES (?, ?, NOW())"
          (userID, cookieSecret)

deleteSession :: (MonadApp m) => Text -> m ()
deleteSession cookieSecret =do
  Config {postgres} <- ask
  void
    $ childSpan "QUERY deleteSession"
    $ do
      tag "params.cookieSecret" cookieSecret
      liftIO $
        execute
          postgres
          "DELETE FROM hipsterfy_user_session WHERE cookie_secret = ?"
          (Only cookieSecret)
