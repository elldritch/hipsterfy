module Hipsterfy.Worker (Options (..), runWorker) where

import Control.Concurrent.Async (async, waitBoth)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Faktory.Settings (ConnectionInfo (..), Queue (..), Settings (..), defaultSettings)
import qualified Faktory.Worker as W (runWorker)
import Hipsterfy.Spotify.Auth (SpotifyApp (..))
import Hipsterfy.Jobs.UpdateUser (handleUpdateUser)
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
  let app = SpotifyApp {clientID, clientSecret, redirectURI = error "redirectURI field invalid for workers"}

  putStrLn "Starting workers."
  updateUserWorker <-
    async
      $ W.runWorker (settingsForQ "update-user")
      $ handleUpdateUser app conn
  updateArtistWorker <-
    async
      $ W.runWorker (settingsForQ "update-artist")
      $ \job -> do
        putStrLn job
  _ <- waitBoth updateUserWorker updateArtistWorker
  pass
  where
    settingsForQ queue =
      defaultSettings
        { settingsQueue = Queue queue,
          settingsConnection =
            ConnectionInfo
              { connectionInfoTls = False,
                connectionInfoHostName = toString faktoryHost,
                connectionInfoPassword = toString <$> faktoryPassword,
                connectionInfoPort = fromInteger $ toInteger faktoryPort
              }
        }
