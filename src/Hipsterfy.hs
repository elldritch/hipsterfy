module Hipsterfy (runServer, Options (..)) where

import Control.Monad.Except (liftEither, throwError)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import Hipsterfy.Session (endSession, User (..), createUser, getSession, getSpotifyUser, startSession)
import Hipsterfy.Spotify (Scope, SpotifyApp (SpotifyApp), exchangeToken, getSpotifyUserID, redirectURI, scopeUserFollowRead, scopeUserLibraryRead, scopeUserTopRead)
import Hipsterfy.Pages (accountPage, loginPage)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Relude
import Web.Scotty (ActionM, ScottyM, scottyOpts, middleware, html, redirect, param)
import qualified Web.Scotty as S
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

data Options = Options
  { host :: Text,
    port :: Int,
    pgConn :: ByteString,
    clientID :: Text,
    clientSecret :: Text
  }
  deriving (Show)

runServer :: Options -> IO ()
runServer (Options {host, port, pgConn, clientID, clientSecret}) = do
  conn <- connectPostgreSQL pgConn

  putStrLn $ "Starting server at: " `mappend` (show address)
  scottyOpts
    S.Options {verbose = 0, settings = defaultSettings & setPort port}
    $ app conn
  where
    address :: Text
    address = host `mappend` case port of
      80 -> ""
      other -> ":" `mappend` show other
    callbackURL :: Text
    callbackURL = address <> "/authorize/callback"
    spotifyApp :: SpotifyApp
    spotifyApp = SpotifyApp clientID clientSecret callbackURL
    spotifyScopes :: [Scope]
    spotifyScopes = [scopeUserLibraryRead, scopeUserFollowRead, scopeUserTopRead]
    app :: Connection -> ScottyM ()
    app conn = do
      middleware logStdoutDev

      -- Home page. Check cookies to see if logged in.
      -- If not logged in, prompt to authorize.
      -- If logged in, provide friend code input.
      S.get "/" $ do
        user <- getSession conn
        case user of
          Just u -> html $ accountPage
          Nothing -> html $ loginPage

      -- Authorization redirect. Generate a new user's OAuth secret and friend code. Redirect to Spotify.
      S.get "/authorize" $ do
        url <- redirectURI spotifyApp conn spotifyScopes
        redirect url

      -- Authorization callback. Populate a user's Spotify information based on the callback. Set cookies to logged in. Redirect to home page.
      S.get "/authorize/callback" $ do
        result <- runExceptT $ do
          -- If a session is already set, then ignore this request.
          session <- lift $ getSession conn
          when (isJust session) $ throwError "session already set"

          -- Obtain access tokens.
          code <- lift $ (param "code" :: ActionM ByteString)
          oauthSecret <- lift $ (param "state" :: ActionM ByteString)
          creds <- liftEither =<< exchangeToken spotifyApp conn code oauthSecret

          -- Check whether this user already exists. If it doesn't, then create a new user.
          spotifyUserID <- liftIO $ getSpotifyUserID creds
          spotifyUser <- lift $ getSpotifyUser conn spotifyUserID
          user <- case spotifyUser of
            Just u -> do
              return (userID u)
            Nothing -> do
              newUser <- lift $ createUser conn spotifyUserID creds
              return (userID newUser)

          -- Create a new session.
          lift $ startSession conn user

        print result
        -- Redirect to dashboard.
        redirect "/"

      -- Compare your artists against a friend code. Must be logged in.
      S.get "/compare" $ do
        undefined

      -- Clear logged in cookies.
      S.get "/logout" $ do
        endSession conn
        redirect "/"
