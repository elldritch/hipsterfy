module Hipsterfy.Jobs.UpdateUser (handleUpdateUser, enqueueUpdateUser, updateUserQueue) where

import qualified Control.Monad.Parallel as Parallel (mapM_)
import Data.Aeson (FromJSON, ToJSON)
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (Connection)
import Faktory.Client (Client)
import Faktory.Job (JobId, perform, queue)
import Faktory.Settings (Queue (Queue))
import Hipsterfy.Jobs.UpdateArtist (enqueueUpdateArtist)
import Hipsterfy.Spotify (getFollowedSpotifyArtists, getSpotifyArtistsOfSavedAlbums, getSpotifyArtistsOfSavedTracks)
import Hipsterfy.Spotify.Auth (SpotifyApp (..))
import Hipsterfy.User (User (..), UserID, completeUserFollowUpdate, getCredentials, getUserByID, isUpdatingFollowers', setFollowedArtists, startUserFollowUpdate)
import Relude

updateUserQueue :: Queue
updateUserQueue = Queue "update-user"

{- HLINT ignore UpdateUserJob "Use newtype instead of data" -}
data UpdateUserJob = UpdateUserJob
  { userID :: UserID
  }
  deriving (Generic)

instance FromJSON UpdateUserJob

instance ToJSON UpdateUserJob

-- TODO: make a check here to ensure the user needs updating and is not currently being updated?
enqueueUpdateUser :: (MonadIO m) => Client -> User -> m JobId
enqueueUpdateUser client User {userID} =
  liftIO $ perform (queue updateUserQueue) client UpdateUserJob {userID}

handleUpdateUser :: (MonadIO m) => SpotifyApp -> Client -> Connection -> UpdateUserJob -> m ()
handleUpdateUser app client conn UpdateUserJob {userID} = do
  -- Short circuit: user is already being updated.
  updating <- isUpdatingFollowers' conn userID
  if updating
    then pass
    else do
      -- Get user.
      maybeUser <- getUserByID conn userID
      user <- case maybeUser of
        Just u -> return u
        Nothing -> error $ "UpdateUserJob: could not find user with ID " <> show userID

      -- Start update (get totals).
      -- TODO: verify that laziness actually doesn't evaluate the second tuple
      -- element yet.
      creds <- getCredentials app conn user
      (totalFollowed, followedArtists) <- getFollowedSpotifyArtists creds
      (totalTrack, trackArtists) <- getSpotifyArtistsOfSavedTracks creds
      (totalAlbum, albumArtists) <- getSpotifyArtistsOfSavedAlbums creds

      -- Set user update status.
      now <- liftIO getCurrentTime
      startUserFollowUpdate conn user now (totalFollowed + totalTrack + totalAlbum)

      -- Run the update.
      let artists = ordNub $ followedArtists ++ trackArtists ++ albumArtists
      setFollowedArtists conn user artists

      -- Enqueue any artist updates needed.
      liftIO $ Parallel.mapM_ (enqueueUpdateArtist client) artists

      -- Finish the updating status.
      completeUserFollowUpdate conn user
