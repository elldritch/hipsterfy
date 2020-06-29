module Main (main) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trace (TraceT)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Faktory.Client (newClient)
import Faktory.Settings (ConnectionInfo (..), Settings (..))
import qualified Faktory.Settings as Faktory (defaultSettings)
import Hipsterfy.Application (Config (..), MonadApp, runAsContainer)
import Hipsterfy.Server
  (handleHealthCheck,  handleCompare,
    handleForceRefreshUpdates,
    handleHomePage,
    handleLogin,
    handleLoginFinish,
    handleLogout,
  )
import Hipsterfy.Spotify.Auth (SpotifyApp (..))
import Monitor.Tracing.Zipkin (Settings (..), new, run)
import qualified Monitor.Tracing.Zipkin as Zipkin (defaultSettings)
import Network.Wai.Handler.Warp (runSettings, setPort)
import qualified Network.Wai.Handler.Warp as Warp (defaultSettings)
import Network.Wai.Middleware.RequestLogger (logStdout)
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
import Text.URI (Authority (..), URI (..), emptyURI, mkHost, mkPathPiece, mkScheme, render, renderStr)
import Web.Scotty.Trans (ScottyError, ScottyT, middleware, scottyAppT)

data Options = Options
  { host :: Text,
    port :: Int,
    clientID :: Text,
    clientSecret :: Text,
    pgConn :: Text,
    faktoryHost :: Text,
    faktoryPort :: Int,
    faktoryPassword :: Text,
    zipkinHost :: Text,
    zipkinPort :: Int,
    healthSecret :: Text,
    podName :: Text
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
        <*> strOption (long "zipkin_host")
        <*> option auto (long "zipkin_port")
        <*> strOption (long "health_secret")
        <*> strOption (long "pod_name")

createRedirectURI :: (MonadThrow m) => Text -> Int -> m URI
createRedirectURI host port = do
  s <- mkScheme "http"
  h <- mkHost host
  path1 <- mkPathPiece "authorize"
  path2 <- mkPathPiece "callback"
  return
    emptyURI
      { uriScheme = Just s,
        uriAuthority =
          Right
            Authority
              { authUserInfo = Nothing,
                authHost = h,
                authPort =
                  if port == 80
                    then Nothing
                    else Just $ fromInteger $ toInteger port
              },
        uriPath = Just (False, path1 :| [path2])
      }

type ServerM = ScottyT LText (TraceT (ReaderT Config IO))

runServerM :: Options -> (Text -> ServerM ()) -> IO ()
runServerM Options {..} app = do
  postgres <- connectPostgreSQL $ encodeUtf8 pgConn
  faktory <- newClient faktorySettings Nothing
  zipkin <- new zipkinSettings
  redirectURI <- createRedirectURI host port

  let spotifyApp = SpotifyApp {clientID, clientSecret, redirectURI = render redirectURI}
  let runConfig = (`runReaderT` Config {postgres, faktory, spotifyApp})
  let runZipkin = (`run` zipkin)
  let runInner = runConfig . runZipkin

  wai <- scottyAppT runInner (app healthSecret)
  putStrLn $ "Starting server at: " <> renderStr redirectURI {uriPath = Nothing}
  runSettings warpSettings wai
  where
    faktorySettings =
      Faktory.defaultSettings
        { settingsConnection =
            ConnectionInfo
              { connectionInfoTls = False,
                connectionInfoHostName = toString faktoryHost,
                connectionInfoPassword = Just $ toString faktoryPassword,
                connectionInfoPort = fromInteger $ toInteger faktoryPort
              }
        }
    warpSettings = Warp.defaultSettings & setPort port
    zipkinSettings =
      Zipkin.defaultSettings
        { settingsPublishPeriod = Just 1,
          settingsHostname = Just $ toString zipkinHost,
          settingsPort = Just $ fromInteger $ toInteger zipkinPort
        }

server :: (ScottyError e, MonadApp m) => Text -> ScottyT e m ()
server healthSecret = do
  middleware logStdout

  handleHomePage
  handleLogin
  handleLoginFinish
  handleLogout
  handleCompare
  handleForceRefreshUpdates
  handleHealthCheck healthSecret

main :: IO ()
main = do
  runAsContainer
  options <- execParser opts
  runServerM options server
