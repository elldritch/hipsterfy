module Hipsterfy.Jobs.UpdateArtist (handleUpdateArtist, enqueueUpdateArtist) where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple (Connection)
import Faktory.Job (JobId)
import Hipsterfy.Spotify (SpotifyArtist (..))
import Relude

{- HLINT ignore UpdateArtistJob "Use newtype instead of data" -}
data UpdateArtistJob = UpdateArtistJob
  { userID :: Int
  }
  deriving (Generic)

instance FromJSON UpdateArtistJob

instance ToJSON UpdateArtistJob

handleUpdateArtist :: (MonadIO m) => Connection -> UpdateArtistJob -> m ()
handleUpdateArtist conn UpdateArtistJob {userID} = undefined

enqueueUpdateArtist :: (MonadIO m) => Connection -> SpotifyArtist -> m JobId
enqueueUpdateArtist conn artist = undefined
