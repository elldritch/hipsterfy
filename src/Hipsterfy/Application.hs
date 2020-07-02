{-# LANGUAGE RankNTypes #-}

module Hipsterfy.Application
  ( Config (..),
    MonadApp,
    AppT,
    runApp,
    makePostgres,
    Faktory (..),
    makeFaktory,
    makeZipkin,
    makeHTTPURI,
    withPathPiece,
    runAsContainer,
  )
where

import Control.Concurrent (myThreadId, throwTo)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trace (TraceT)
import Data.Aeson (FromJSON)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import Faktory.Client (Client, newClient)
import qualified Faktory.Settings as Faktory (Settings, defaultSettings)
import Faktory.Settings (ConnectionInfo (..), Queue, Settings (..))
import qualified Faktory.Worker as Faktory (runWorker)
import Hipsterfy.Spotify.Auth (SpotifyApp)
import Monitor.Tracing (MonadTrace)
import Monitor.Tracing.Zipkin (Settings (..), Zipkin, new, run)
import qualified Monitor.Tracing.Zipkin as Zipkin (defaultSettings)
import Relude
import System.Exit (ExitCode (ExitSuccess))
import System.IO (BufferMode (NoBuffering), hSetBuffering)
import System.Posix (Handler (Catch))
import System.Posix.Signals (installHandler, softwareTermination)
import Text.URI (Authority (..), URI (..), emptyURI, mkHost, mkPathPiece, mkScheme)

data Faktory = Faktory
  { client :: Client,
    runWorker :: forall a. (FromJSON a) => Config -> Queue -> (a -> AppT IO ()) -> IO ()
  }

data Config = Config
  { postgres :: Connection,
    faktory :: Faktory,
    zipkin :: Zipkin,
    address :: URI,
    spotifyApp :: SpotifyApp
  }

type AppT m a = TraceT (ReaderT Config m) a

runApp :: Config -> AppT m a -> m a
runApp config = runConfig . runZipkin
  where
    runConfig = (`runReaderT` config)
    runZipkin = (`run` zipkin config)

makePostgres :: (MonadIO m) => Text -> m Connection
makePostgres = liftIO . connectPostgreSQL . encodeUtf8

makeFaktory :: (MonadIO m) => Text -> Text -> Int -> m Faktory
makeFaktory host password port = do
  client <- liftIO $ newClient faktorySettings Nothing
  let runWorker config queue action =
        Faktory.runWorker faktorySettings {settingsQueue = queue} $ runApp config . action
  return Faktory {client, runWorker}
  where
    faktorySettings = makeFaktorySettings host password port

makeFaktorySettings :: Text -> Text -> Int -> Faktory.Settings
makeFaktorySettings host password port =
  Faktory.defaultSettings
    { settingsConnection =
        ConnectionInfo
          { connectionInfoTls = False,
            connectionInfoHostName = toString host,
            connectionInfoPassword = Just $ toString password,
            connectionInfoPort = fromInteger $ toInteger port
          }
    }

makeZipkin :: (MonadIO m) => Text -> Int -> m Zipkin
makeZipkin host port =
  new
    Zipkin.defaultSettings
      { settingsPublishPeriod = Just 1,
        settingsHostname = Just $ toString host,
        settingsPort = Just $ fromInteger $ toInteger port
      }

makeHTTPURI :: (MonadThrow m) => Text -> Int -> m URI
makeHTTPURI host port = do
  s <- mkScheme "http"
  h <- mkHost host
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
        uriPath = Nothing
      }

withPathPiece :: (MonadThrow m) => URI -> Text -> m URI
withPathPiece uri piece = do
  path <- mkPathPiece piece
  return $ case uriPath uri of
    Just (t, ph :| ps) -> uri {uriPath = Just (t, ph :| (ps <> [path]))}
    Nothing -> uri {uriPath = Just (False, path :| [])}

type MonadApp m = (MonadIO m, MonadTrace m, MonadReader Config m)

runAsContainer :: (MonadIO m) => m ()
runAsContainer = do
  -- Handle SIGTERM (Docker Compose container termination signal), which is different from usual C-c SIGINT.
  tid <- liftIO myThreadId
  void $ liftIO $ installHandler softwareTermination (Catch $ throwTo tid ExitSuccess) Nothing
  -- Don't buffer logs (otherwise, logs of server start don't print).
  liftIO $ hSetBuffering stdout NoBuffering
