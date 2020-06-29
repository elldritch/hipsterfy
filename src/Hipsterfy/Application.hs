module Hipsterfy.Application (Config (..), MonadApp, runAsContainer) where

import Control.Concurrent (myThreadId, throwTo)
import Database.PostgreSQL.Simple (Connection)
import Faktory.Client (Client)
import Hipsterfy.Spotify.Auth (SpotifyApp)
import Monitor.Tracing (MonadTrace)
import Relude
import System.Exit (ExitCode (ExitSuccess))
import System.IO (BufferMode (NoBuffering), hSetBuffering)
import System.Posix (Handler (Catch))
import System.Posix.Signals (installHandler, softwareTermination)

data Config = Config
  { postgres :: Connection,
    faktory :: Client,
    spotifyApp :: SpotifyApp
  }

type MonadApp m = (MonadIO m, MonadTrace m, MonadReader Config m)

runAsContainer :: (MonadIO m) => m ()
runAsContainer = do
  -- Handle SIGTERM (Docker Compose container termination signal), which is different from usual C-c SIGINT.
  tid <- liftIO myThreadId
  void $ liftIO $ installHandler softwareTermination (Catch $ throwTo tid ExitSuccess) Nothing
  -- Don't buffer logs (otherwise, logs of server start don't print).
  liftIO $ hSetBuffering stdout NoBuffering
