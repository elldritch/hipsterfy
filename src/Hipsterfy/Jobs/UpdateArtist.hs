module Hipsterfy.Jobs.UpdateArtist (handleUpdateArtist, enqueueUpdateArtist, updateArtistQueue) where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple (Connection)
import Faktory.Client (Client)
import Faktory.Job (JobId, perform, queue)
import Faktory.Settings (Queue (Queue))
import Hipsterfy.Spotify (SpotifyArtist (..))
import Hipsterfy.Spotify.Auth (getAnonymousBearerToken)
import Hipsterfy.Artist (getArtistInsights')
import Relude

updateArtistQueue :: Queue
updateArtistQueue = Queue "update-artist"

{- HLINT ignore UpdateArtistJob "Use newtype instead of data" -}
data UpdateArtistJob = UpdateArtistJob
  { spotifyArtistID :: Text
  }
  deriving (Generic)

instance FromJSON UpdateArtistJob

instance ToJSON UpdateArtistJob

-- TODO: make a check here to ensure the artist needs updating?
enqueueUpdateArtist :: (MonadIO m) => Client -> SpotifyArtist -> m JobId
enqueueUpdateArtist client SpotifyArtist {spotifyArtistID} =
  liftIO $ perform (queue updateArtistQueue) client UpdateArtistJob {spotifyArtistID}

handleUpdateArtist :: (MonadIO m) => Connection -> UpdateArtistJob -> m ()
handleUpdateArtist conn UpdateArtistJob {spotifyArtistID} = do
  bearerToken <- getAnonymousBearerToken
  _ <- getArtistInsights' conn bearerToken spotifyArtistID
  pass
