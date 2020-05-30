module Hipsterfy (runServer, Options (..)) where

import Control.Monad.Except (throwError)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import Hipsterfy.Artist (getArtistInsights)
import Hipsterfy.Pages (accountPage, comparePage, loginPage)
import Hipsterfy.Session (endSession, getSession, startSession)
import Hipsterfy.Spotify (AnonymousBearerToken, Scope, SpotifyApp (SpotifyApp), SpotifyArtist, SpotifyArtistInsights, getAnonymousBearerToken, getFollowedSpotifyArtists, scopeUserFollowRead, scopeUserLibraryRead, scopeUserTopRead)
import Hipsterfy.User (User, createOAuthRedirect, createUser, getCredentials, getUserByFriendCode)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Relude
import Web.Scotty (ActionM, ScottyM, html, middleware, param, redirect, scottyOpts)
import qualified Web.Scotty as S
import Web.Scotty.Internal.Types (ActionError (Redirect))

data Options = Options
  { host :: Text,
    port :: Int,
    pgConn :: Text,
    clientID :: Text,
    clientSecret :: Text
  }
  deriving (Show)

runServer :: Options -> IO ()
runServer Options {host, port, pgConn, clientID, clientSecret} = do
  conn <- connectPostgreSQL $ encodeUtf8 pgConn

  putStrLn $ "Starting server at: " `mappend` show address
  scottyOpts
    S.Options {verbose = 0, settings = defaultSettings & setPort port}
    $ server spotifyApp conn
  where
    address :: Text
    address = host `mappend` case port of
      80 -> ""
      other -> ":" `mappend` show other
    callbackURL :: Text
    callbackURL = address <> "/authorize/callback"
    spotifyApp :: SpotifyApp
    spotifyApp = SpotifyApp clientID clientSecret callbackURL

server :: SpotifyApp -> Connection -> ScottyM ()
server spotifyApp conn = do
  middleware logStdoutDev

  -- Home page. Check cookies to see if logged in.
  -- If not logged in, prompt to authorize.
  -- If logged in, provide friend code input.
  S.get "/" $ do
    user <- getSession conn
    case user of
      Just u -> html $ accountPage u
      Nothing -> html loginPage

  -- Authorization redirect. Generate a new user's OAuth secret and friend code. Redirect to Spotify.
  S.get "/authorize" $ createOAuthRedirect spotifyApp conn spotifyScopes >>= redirect

  -- Authorization callback. Populate a user's Spotify information based on the callback. Set cookies to logged in. Redirect to home page.
  S.get "/authorize/callback" $ do
    -- If a session is already set, then ignore this request.
    session <- getSession conn
    case session of
      Just _ -> throwError $ Redirect "/"
      Nothing -> return ()

    -- Obtain access tokens.
    code <- param "code" :: ActionM Text
    oauthState <- param "state" :: ActionM Text
    eitherUser <- createUser spotifyApp conn code oauthState
    user <- case eitherUser of
      Right user -> return user
      Left err -> do
        print err
        throwError $ Redirect "/"

    -- Create a new session.
    startSession conn user

    -- Redirect to dashboard.
    redirect "/"

  -- Compare your artists against a friend code. Must be logged in.
  S.post "/compare" $ do
    -- Require authentication.
    maybeUser <- getSession conn
    user <- case maybeUser of
      Just u -> return u
      Nothing -> throwError $ Redirect "/"

    -- Check that the friend code is valid.
    friendCode <- param "friend-code" :: ActionM Text
    maybeFriend <- getUserByFriendCode conn friendCode
    friend <- case maybeFriend of
      Just f -> return f
      -- TODO: display an error message for invalid friend codes.
      Nothing -> throwError $ Redirect "/"

    -- Load followed artists.
    bearerToken <- getAnonymousBearerToken
    yourArtists <- getFollowedArtists conn bearerToken user
    friendArtists <- getFollowedArtists conn bearerToken friend

    -- Render page.
    html $ comparePage yourArtists friendArtists

  -- Clear logged in cookies.
  S.get "/logout" $ do
    endSession conn
    redirect "/"
  where
    spotifyScopes :: [Scope]
    spotifyScopes = [scopeUserLibraryRead, scopeUserFollowRead, scopeUserTopRead]
    getFollowedArtists :: (MonadIO m) => Connection -> AnonymousBearerToken -> User -> m [(SpotifyArtist, SpotifyArtistInsights)]
    getFollowedArtists conn' bearerToken user = do
      creds <- liftIO $ getCredentials spotifyApp conn' user
      artists <- getFollowedSpotifyArtists creds
      insights <- mapM (getArtistInsights conn' bearerToken) artists
      return $ zip artists insights
