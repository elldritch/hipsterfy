module Hipsterfy.Jobs.UpdateArtist
  ( handleUpdateArtist,
    enqueueUpdateArtist,
    updateArtistQueue,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple (Connection)
import Faktory.Client (Client)
import Faktory.Job (perform, queue)
import Faktory.Settings (Queue (Queue))
import Hipsterfy.Artist (getArtist, UpdateStatus (..), getUpdateStatus, refreshArtistInsights, ArtistID)
import Hipsterfy.Spotify.Auth (getAnonymousBearerToken)
import Relude

updateArtistQueue :: Queue
updateArtistQueue = Queue "update-artist"

{- HLINT ignore UpdateArtistJob "Use newtype instead of data" -}
data UpdateArtistJob = UpdateArtistJob
  { artistID :: ArtistID
  }
  deriving (Generic)

instance FromJSON UpdateArtistJob

instance ToJSON UpdateArtistJob

enqueueUpdateArtist :: (MonadIO m) => Client -> Connection -> ArtistID -> m ()
enqueueUpdateArtist client conn artistID = do
  status <- getUpdateStatus conn artistID
  case status of
    NeedsUpdate -> void $ liftIO $ perform (queue updateArtistQueue) client UpdateArtistJob {artistID}
    _ -> pass

handleUpdateArtist :: (MonadIO m) => Connection -> UpdateArtistJob -> m ()
handleUpdateArtist conn UpdateArtistJob {artistID} = do
  maybeArtist <- getArtist conn artistID
  artist <- case maybeArtist of
    Just a -> return a
    Nothing -> error $ "handleUpdateArtist: could not find artist with ID " <> show artistID
  bearerToken <- getAnonymousBearerToken
  _ <- refreshArtistInsights conn bearerToken artist
  pass
