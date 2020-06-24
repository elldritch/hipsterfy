module Main (main) where

import Control.Concurrent (getNumCapabilities, myThreadId, throwTo)
import Control.Concurrent.Async (async, wait)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Faktory.Client (newClient)
import Faktory.Settings (ConnectionInfo (..), Settings (..), defaultSettings)
import qualified Faktory.Worker as W (runWorker)
import Hipsterfy.Jobs.UpdateArtist (handleUpdateArtist, updateArtistQueue)
import Hipsterfy.Jobs.UpdateUser (handleUpdateUser, updateUserQueue)
import Hipsterfy.Spotify.Auth (SpotifyApp (..))
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
import System.Exit (ExitCode (ExitSuccess))
import System.IO (BufferMode (NoBuffering), hSetBuffering)
import System.Posix (Handler (Catch))
import System.Posix.Signals (installHandler, softwareTermination)

data Options = Options
  { clientID :: Text,
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

-- TODO: add health check

main :: IO ()
main = do
  tid <- myThreadId
  _ <- installHandler softwareTermination (Catch $ throwTo tid ExitSuccess) Nothing
  hSetBuffering stdout NoBuffering
  options <- execParser opts
  runWorker options

runWorker :: Options -> IO ()
runWorker Options {pgConn, faktoryHost, faktoryPort, faktoryPassword, clientID, clientSecret} = do
  conn <- connectPostgreSQL $ encodeUtf8 pgConn
  updateArtistClient <- newClient (settingsForQ updateArtistQueue) Nothing
  let app = SpotifyApp {clientID, clientSecret, redirectURI = error "runWorker: impossible: redirectURI never used"}

  caps <- getNumCapabilities
  putStrLn $ "Starting workers (" <> show caps <> " threads)."
  updateUserWorker <-
    async
      $ W.runWorker (settingsForQ updateUserQueue)
      $ handleUpdateUser app updateArtistClient conn
  updateArtistWorkers <-
    mapM
      ( const $ async
          $ W.runWorker (settingsForQ updateArtistQueue)
          $ handleUpdateArtist conn
      )
      [1 .. caps - 1]
  mapM_ wait $ updateUserWorker : updateArtistWorkers
  where
    settingsForQ queue =
      defaultSettings
        { settingsQueue = queue,
          settingsConnection =
            ConnectionInfo
              { connectionInfoTls = False,
                connectionInfoHostName = toString faktoryHost,
                connectionInfoPassword = Just $ toString faktoryPassword,
                connectionInfoPort = fromInteger $ toInteger faktoryPort
              }
        }
