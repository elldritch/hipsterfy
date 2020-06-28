
module Main (main) where

import Control.Concurrent (myThreadId, throwTo)
import Control.Monad.Trace (TraceT)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import Faktory.Client (Client, newClient)
import Faktory.Settings (ConnectionInfo (..), Queue, Settings (..))
import qualified Faktory.Settings as Faktory (Settings, defaultSettings)
import Hipsterfy.Jobs.UpdateUser (enqueueUpdateUser, forceEnqueueUpdateUser, updateUserQueue)
import Hipsterfy.Server.Pages (accountPage, comparePage, loginPage)
import Hipsterfy.Server.Session (endSession, getSession, startSession)
import Hipsterfy.Spotify.Auth
  ( Scope,
    SpotifyApp (..),
    scopeUserFollowRead,
    scopeUserLibraryRead,
    scopeUserTopRead,
  )
import Hipsterfy.User
  ( User (..),
    createOAuthRedirect,
    createUser,
    getFollowedArtists,
    getUpdateStatus,
    getUserByFriendCode,
  )
import Monitor.Tracing (alwaysSampled, rootSpan)
import Control.Monad.Trace.Class (MonadTrace(..))
import Monitor.Tracing.Zipkin (tag, Zipkin, new, run, Settings (..))
import qualified Monitor.Tracing.Zipkin as Zipkin (defaultSettings)
import Network.Wai (Response, responseStatus)
import Network.Wai.Handler.Warp (runSettings, setPort)
import qualified Network.Wai.Handler.Warp as Warp (defaultSettings)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Options.Applicative
  ( ParserInfo,
    auto,
    briefDesc,
    execParser,
    helper,
    info,
    long,
    option,
    progDesc,
    strOption,
  )
import Relude hiding (trace)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (BufferMode (NoBuffering), hSetBuffering)
import System.Posix (Handler (Catch))
import System.Posix.Signals (installHandler, softwareTermination)
import Web.Scotty (scottyOpts)
import qualified Web.Scotty.Trans as S
import Web.Scotty.Trans (ActionT(..), ScottyError, ScottyT, html, middleware, param, redirect, scottyAppT)
import Web.Scotty.Cookie (getCookie)
import Hipsterfy.Session (getSessionByCookieSecret)
import Control.Monad.Trans.Control (restoreM, MonadBaseControl(liftBaseWith))
import UnliftIO (MonadUnliftIO)
import Web.Scotty.Internal.Types (ActionT(ActionT))
import Hipsterfy.Server.Trace (withTracing)
import Network.HTTP.Types (StdMethod(GET))

data Options = Options
  { host :: Text,
    port :: Int,
    clientID :: Text,
    clientSecret :: Text,
    pgConn :: Text,
    faktoryHost :: Text,
    faktoryPort :: Int,
    faktoryPassword :: Text
  }
  deriving (Show)

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (briefDesc <> progDesc "Hipsterfy server")
  where
    options =
      Options
        <$> strOption (long "host")
        <*> option auto (long "port")
        <*> strOption (long "client_id")
        <*> strOption (long "client_secret")
        <*> strOption (long "db")
        <*> strOption (long "faktory_host")
        <*> option auto (long "faktory_port")
        <*> strOption (long "faktory_password")

-- -- traceResponse :: Zipkin -> TraceT IO Response -> IO Response
-- traceResponse :: Zipkin -> TraceT IO Response -> IO Response
-- traceResponse zipkin res = do
--   run x zipkin
--   where
--     x = do
--       r <- res
--       let status = responseStatus r
--       liftIO $ putStrLn $ "status: " <> show status
--       tag "http.statusCode" $ show status
--       return r

main :: IO ()
main = do
  tid <- myThreadId
  _ <- installHandler softwareTermination (Catch $ throwTo tid ExitSuccess) Nothing
  hSetBuffering stdout NoBuffering
  options <- execParser opts
  runServer options

runServer :: Options -> IO ()
runServer Options {host, port, pgConn, clientID, clientSecret, faktoryHost, faktoryPassword, faktoryPort} = do
  conn <- connectPostgreSQL $ encodeUtf8 pgConn
  updateUserClient <- newClient (settingsForQ updateUserQueue) Nothing
  zipkin <- new Zipkin.defaultSettings {settingsPublishPeriod = Just 1}

  putStrLn $ "Starting server at: " `mappend` show address
  app <- scottyAppT
    (`run` zipkin)
    (server spotifyApp updateUserClient conn :: (MonadIO m, MonadTrace m) => ScottyT LText m ())
  runSettings (Warp.defaultSettings & setPort port) app
  where
    address :: Text
    address = host `mappend` case port of
      80 -> ""
      other -> ":" `mappend` show other
    spotifyApp :: SpotifyApp
    spotifyApp = SpotifyApp {clientID, clientSecret, redirectURI = address <> "/authorize/callback"}
    settingsForQ :: Queue -> Faktory.Settings
    settingsForQ queue =
      Faktory.defaultSettings
        { settingsQueue = queue,
          settingsConnection =
            ConnectionInfo
              { connectionInfoTls = False,
                connectionInfoHostName = toString faktoryHost,
                connectionInfoPassword = Just $ toString faktoryPassword,
                connectionInfoPort = fromInteger $ toInteger faktoryPort
              }
        }

-- server :: SpotifyApp -> Client -> Connection -> Zipkin -> ScottyT LText IO ()
-- server :: (MonadIO m, MonadTrace m) => SpotifyApp -> Client -> Connection -> ScottyT LText m ()
server :: (ScottyError e, MonadIO m, MonadTrace m) => SpotifyApp -> Client -> Connection -> ScottyT e m ()
server spotifyApp updateUserClient conn = do
  middleware logStdoutDev

  let hipsterfyCookieName = "hipsterfy_user"

  -- Home page. Check cookies to see if logged in.
  -- If not logged in, prompt to authorize.
  -- If logged in, provide friend code input.
  withTracing GET "/" $ do
    cookie <- getCookie hipsterfyCookieName
    res <- case cookie of
      Nothing -> return loginPage
      Just c -> do
        user <- getSessionByCookieSecret conn c
        case user of
          Just u@User {userID} -> do
            void $ enqueueUpdateUser updateUserClient conn userID
            followed <- getFollowedArtists conn userID
            status <- getUpdateStatus conn userID
            return $ accountPage u status followed
          Nothing -> return loginPage
    html res

  -- Authorization redirect. Generate a new user's OAuth secret and friend code. Redirect to Spotify.
  S.get "/authorize" $ createOAuthRedirect spotifyApp conn spotifyScopes >>= redirect

  -- Authorization callback. Populate a user's Spotify information based on the callback. Set cookies to logged in. Redirect to home page.
  S.get "/authorize/callback" $ do
    -- If a session is already set, then ignore this request.
    session <- getSession conn
    case session of
      Just _ -> redirect "/"
      Nothing -> pass

    -- Obtain access tokens.
    code <- param "code"
    oauthState <- param "state"
    eitherUser <- createUser spotifyApp conn code oauthState
    user <- case eitherUser of
      Right user -> return user
      Left err -> do
        print err
        redirect "/"

    -- Create a new session.
    startSession conn user

    -- Redirect to dashboard.
    redirect "/"

  -- Compare your artists against a friend code. Must be logged in.
  -- S.post "/compare" $ do
  --   -- Require authentication.
  --   maybeUser <- getSession conn
  --   user <- case maybeUser of
  --     Just u -> return u
  --     Nothing -> redirect "/"

  --   -- Check that the friend code is valid.
  --   friendCode <- param "friend-code"
  --   maybeFriend <- getUserByFriendCode conn friendCode
  --   friend <- case maybeFriend of
  --     Just f -> return f
  --     -- TODO: display an error message for invalid friend codes.
  --     Nothing -> redirect "/"

  --   -- Load followed artists.
  --   yourArtists <- getFollowedArtists conn $ userID user
  --   friendArtists <- getFollowedArtists conn $ userID friend

  --   -- Render page.
  --   html $ comparePage yourArtists friendArtists

  S.get "/refresh" $ do
    user <- getSession conn
    _ <- case user of
      Just User {userID} -> void $ forceEnqueueUpdateUser updateUserClient conn userID
      _ -> pass
    redirect "/"

  -- Clear logged in cookies.
  S.get "/logout" $ do
    endSession conn
    redirect "/"
  where
    spotifyScopes :: [Scope]
    spotifyScopes = [scopeUserLibraryRead, scopeUserFollowRead, scopeUserTopRead]
