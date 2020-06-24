module Main (main) where

import Control.Concurrent (myThreadId, throwTo)
import Hipsterfy.Worker (Options(..), runWorker)
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
import System.IO (BufferMode (NoBuffering), hSetBuffering)
import System.Posix (Handler (Catch))
import System.Exit (ExitCode (ExitSuccess))
import System.Posix.Signals (installHandler, softwareTermination)

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
