module Main (main) where

import Control.Monad.Trace (TraceT)
import Hipsterfy.Application
  ( Config (..),
    MonadApp,
    makeFaktory,
    makePostgres,
    makeZipkin,
    runApp,
    runAsContainer,
  )
import Hipsterfy.Server
  ( handleCompare,
    handleForceRefreshUpdates,
    handleHealthCheck,
    handleHomePage,
    handleLogin,
    handleLoginFinish,
    handleLogout,
  )
import Hipsterfy.Spotify.Auth (SpotifyApp (..))
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
import Web.Scotty.Trans (ScottyError, ScottyT, middleware, scottyAppT)

data Options = Options
  { port :: Int,
    pgConn :: Text,
    clientID :: Text,
    clientSecret :: Text,
    redirectURI :: Text,
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
        <$> option auto (long "port")
        <*> strOption (long "db")
        <*> strOption (long "spotify_client_id")
        <*> strOption (long "spotify_client_secret")
        <*> strOption (long "spotify_redirect_uri")
        <*> strOption (long "faktory_host")
        <*> option auto (long "faktory_port")
        <*> strOption (long "faktory_password")
        <*> strOption (long "zipkin_host")
        <*> option auto (long "zipkin_port")
        <*> strOption (long "health_secret")
        <*> strOption (long "pod_name")

type ServerM = ScottyT LText (TraceT (ReaderT Config IO))

runServerM :: Options -> (Text -> ServerM ()) -> IO ()
runServerM Options {..} app = do
  postgres <- makePostgres pgConn
  faktory <- makeFaktory faktoryHost faktoryPassword faktoryPort
  zipkin <- makeZipkin zipkinHost zipkinPort

  let spotifyApp = SpotifyApp {..}
  let config = Config {..}

  wai <- scottyAppT (runApp config) (app healthSecret)
  putStrLn $ "Starting server, listening to port " <> show port
  runSettings warpSettings wai
  where
    warpSettings = Warp.defaultSettings & setPort port

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
