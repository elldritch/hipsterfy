module Hipsterfy.Worker (Options (..), runWorker) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (async, wait)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Faktory.Client (newClient)
import Faktory.Settings (ConnectionInfo (..), Settings (..), defaultSettings)
import qualified Faktory.Worker as W (runWorker)
import Hipsterfy.Jobs.UpdateArtist (handleUpdateArtist, updateArtistQueue)
import Hipsterfy.Jobs.UpdateUser (handleUpdateUser, updateUserQueue)
import Hipsterfy.Spotify.Auth (SpotifyApp (..))
import Relude

data Options = Options
  { pgConn :: Text,
    faktoryHost :: Text,
    faktoryPort :: Int,
    faktoryPassword :: Maybe Text,
    clientID :: Text,
    clientSecret :: Text
  }
  deriving (Show)

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
                connectionInfoPassword = toString <$> faktoryPassword,
                connectionInfoPort = fromInteger $ toInteger faktoryPort
              }
        }
