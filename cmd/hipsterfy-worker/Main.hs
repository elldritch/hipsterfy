module Main (main) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (async, wait)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Faktory.Client (newClient)
import Faktory.Settings (ConnectionInfo (..), Settings (..))
import qualified Faktory.Settings as Faktory (defaultSettings)
import Faktory.Worker (runWorker)
import Hipsterfy.Application (Config (..), runAsContainer)
import Hipsterfy.Internal.OrphanInstances ()
import Hipsterfy.Jobs.UpdateArtist (handleUpdateArtist, updateArtistQueue)
import Hipsterfy.Jobs.UpdateUser (handleUpdateUser, updateUserQueue)
import Hipsterfy.Spotify.Auth (SpotifyApp (..))
import Monitor.Tracing.Zipkin (Settings (..), new, run)
import qualified Monitor.Tracing.Zipkin as Zipkin (defaultSettings)
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
import Relude

data Options = Options
  { clientID :: Text,
    clientSecret :: Text,
    pgConn :: Text,
    faktoryHost :: Text,
    faktoryPort :: Int,
    faktoryPassword :: Text,
    zipkinHost :: Text,
    zipkinPort :: Int
  }
  deriving (Show)

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (briefDesc <> progDesc "Hipsterfy worker")
  where
    options =
      Options
        <$> strOption (long "client_id")
        <*> strOption (long "client_secret")
        <*> strOption (long "db")
        <*> strOption (long "faktory_host")
        <*> option auto (long "faktory_port")
        <*> strOption (long "faktory_password")
        <*> strOption (long "zipkin_host")
        <*> option auto (long "zipkin_port")

runWorkers :: Options -> IO ()
runWorkers Options {..} = do
  postgres <- connectPostgreSQL $ encodeUtf8 pgConn
  faktory <- newClient faktorySettings Nothing
  zipkin <- new zipkinSettings

  let spotifyApp = SpotifyApp {clientID, clientSecret, redirectURI = error "runWorker: impossible: redirectURI never used"}
  let runConfig = (`runReaderT` Config {postgres, faktory, spotifyApp})
  let runZipkin = (`run` zipkin)
  let runInner = runConfig . runZipkin

  caps <- getNumCapabilities
  putStrLn $ "Starting workers (" <> show caps <> " threads)."

  updateUserWorker <-
    async
      $ runWorker (workerSettings updateUserQueue)
      $ runInner . handleUpdateUser
  updateArtistWorkers <-
    forM [1 .. caps - 1]
      $ const
      $ async
      $ runWorker (workerSettings updateArtistQueue)
      $ runInner . handleUpdateArtist
  mapM_ wait $ updateUserWorker : updateArtistWorkers
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
    workerSettings queue = faktorySettings {settingsQueue = queue}
    zipkinSettings =
      Zipkin.defaultSettings
        { settingsPublishPeriod = Just 1,
          settingsHostname = Just $ toString zipkinHost,
          settingsPort = Just $ fromInteger $ toInteger zipkinPort
        }

main :: IO ()
main = do
  runAsContainer
  options <- execParser opts
  runWorkers options
