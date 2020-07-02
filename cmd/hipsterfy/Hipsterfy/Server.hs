module Hipsterfy.Server
  ( handleHomePage,
    handleLogin,
    handleLoginFinish,
    handleLogout,
    handleCompare,
    handleForceRefreshUpdates,
    handleHealthCheck,
  )
where

import Hipsterfy.Application (MonadApp)
import Hipsterfy.Jobs.UpdateUser (enqueueUpdateUser, forceEnqueueUpdateUser)
import Hipsterfy.Server.Handlers (get, post)
import Hipsterfy.Server.Pages (accountPage, comparePage, loginPage)
import Hipsterfy.Server.Session (endSession, getSession, startSession)
import Hipsterfy.Spotify.Auth (scopeUserFollowRead, scopeUserLibraryRead, scopeUserTopRead)
import Hipsterfy.User (User (..), createOAuthRedirect, createUser, getFollowedArtists, getUserByFriendCode)
import Network.HTTP.Types (status200)
import Relude hiding (get)
import Web.Scotty.Trans (ScottyError, ScottyT, html, next, param, redirect, status)
import Hipsterfy.Jobs (infoToStatus)

-- Home page. Check cookies to see if logged in.
-- If not logged in, prompt to authorize.
-- If logged in, provide friend code input.
handleHomePage :: (ScottyError e, MonadApp m) => ScottyT e m ()
handleHomePage = get "/" $ do
  maybeUser <- getSession
  case maybeUser of
    Just user@User {userID, updateJobInfo} -> do
      void $ enqueueUpdateUser userID
      followed <- getFollowedArtists userID
      updateStatus <- infoToStatus updateJobInfo
      html $ accountPage user updateStatus followed
    Nothing -> html loginPage

-- Authorization redirect. Generate a new user's OAuth secret and friend code. Redirect to Spotify.
handleLogin :: (ScottyError e, MonadApp m) => ScottyT e m ()
handleLogin = get "/authorize" $ do
  createOAuthRedirect spotifyScopes >>= redirect
  where
    spotifyScopes = [scopeUserLibraryRead, scopeUserFollowRead, scopeUserTopRead]

-- Authorization callback. Populate a user's Spotify information based on the callback. Set cookies to logged in. Redirect to home page.
handleLoginFinish :: (ScottyError e, MonadApp m) => ScottyT e m ()
handleLoginFinish = get "/authorize/callback" $ do
  -- If a session is already set, then ignore this request.
  session <- getSession
  case session of
    Just _ -> redirect "/"
    Nothing -> pass

  -- Obtain access tokens.
  code <- param "code"
  oauthState <- param "state"
  user <- createUser code oauthState

  -- Create a new session.
  startSession user

  -- Redirect to dashboard.
  redirect "/"

-- Compare your artists against a friend code. Must be logged in.
handleCompare :: (ScottyError e, MonadApp m) => ScottyT e m ()
handleCompare = post "/compare" $ do
  -- Require authentication.
  maybeUser <- getSession
  user <- case maybeUser of
    Just u -> return u
    Nothing -> redirect "/"

  -- Check that the friend code is valid.
  friendCode <- param "friend-code"
  maybeFriend <- getUserByFriendCode friendCode
  friend <- case maybeFriend of
    Just f -> return f
    -- TODO: display an error message for invalid friend codes.
    Nothing -> redirect "/"

  -- Load followed artists.
  yourArtists <- getFollowedArtists $ userID user
  friendArtists <- getFollowedArtists $ userID friend

  -- Render page.
  html $ comparePage yourArtists friendArtists

handleForceRefreshUpdates :: (ScottyError e, MonadApp m) => ScottyT e m ()
handleForceRefreshUpdates = get "/refresh" $ do
  user <- getSession
  case user of
    Just User {userID} -> void $ forceEnqueueUpdateUser userID
    _ -> pass
  redirect "/"

-- Clear logged in cookies.
handleLogout :: (ScottyError e, MonadApp m) => ScottyT e m ()
handleLogout = get "/logout" $ endSession >> redirect "/"

handleHealthCheck :: (ScottyError e, MonadApp m) => Text -> ScottyT e m ()
handleHealthCheck healthSecret = get "/health" $ do
  secret <- param "secret"
  if secret == healthSecret
    then status status200
    else next

-- TODO: handle 404s and 500s
