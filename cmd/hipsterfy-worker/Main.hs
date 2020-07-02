module Main (main) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (async, wait)
import Hipsterfy.Application (Config (..), Faktory (..), makeFaktory, makePostgres, makeZipkin, runAsContainer)
import Hipsterfy.Internal.OrphanInstances ()
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
  postgres <- makePostgres pgConn
  faktory@Faktory {runWorker} <- makeFaktory faktoryHost faktoryPassword faktoryPort
  zipkin <- makeZipkin zipkinHost zipkinPort

  let spotifyApp = SpotifyApp {clientID, clientSecret, redirectURI = error "runWorkers: impossible: redirectURI never used"}
  let config = Config {postgres, faktory, zipkin, address = error "runWorkers: impossible: address never used", spotifyApp}

  caps <- getNumCapabilities
  putStrLn $ "Starting workers (" <> show caps <> " threads)."

  updateUserWorker <-
    async $ runWorker config updateUserQueue handleUpdateUser
  updateArtistWorkers <-
    forM [1 .. caps - 1] $ const $ async $ runWorker config updateArtistQueue handleUpdateArtist
  mapM_ wait $ updateUserWorker : updateArtistWorkers

main :: IO ()
main = do
  runAsContainer
  options <- execParser opts
  runWorkers options
