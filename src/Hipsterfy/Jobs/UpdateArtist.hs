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
import Hipsterfy.Artist (needsUpdate, refreshArtistInsightsIfNeeded)
import Hipsterfy.Spotify (SpotifyArtist (..), SpotifyArtistID)
import Hipsterfy.Spotify.Auth (getAnonymousBearerToken)
import Relude

updateArtistQueue :: Queue
updateArtistQueue = Queue "update-artist"

{- HLINT ignore UpdateArtistJob "Use newtype instead of data" -}
data UpdateArtistJob = UpdateArtistJob
  { spotifyArtistID :: SpotifyArtistID
  }
  deriving (Generic)

instance FromJSON UpdateArtistJob

instance ToJSON UpdateArtistJob

-- TODO: make a check here to ensure the artist needs updating?
enqueueUpdateArtist :: (MonadIO m) => Client -> Connection -> SpotifyArtist -> m ()
enqueueUpdateArtist client conn SpotifyArtist {spotifyArtistID} = do
  updateNeeded <- needsUpdate conn spotifyArtistID
  if updateNeeded
    then void $ liftIO $ perform (queue updateArtistQueue) client UpdateArtistJob {spotifyArtistID}
    else pass

handleUpdateArtist :: (MonadIO m) => Connection -> UpdateArtistJob -> m ()
handleUpdateArtist conn UpdateArtistJob {spotifyArtistID} = do
  bearerToken <- getAnonymousBearerToken
  _ <- refreshArtistInsightsIfNeeded conn bearerToken spotifyArtistID
  pass
