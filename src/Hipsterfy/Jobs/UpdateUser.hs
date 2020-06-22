module Hipsterfy.Jobs.UpdateUser (handleUpdateUser, enqueueUpdateUser) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (Connection)
import Faktory.Job (JobId)
import Hipsterfy.Spotify (getFollowedSpotifyArtists, getSpotifyArtistsOfSavedAlbums, getSpotifyArtistsOfSavedTracks)
import Hipsterfy.User (User (..), completeUserFollowUpdate, getUserByID, setFollowedArtists, startUserFollowUpdate)
import Relude

{- HLINT ignore UpdateUserJob "Use newtype instead of data" -}
data UpdateUserJob = UpdateUserJob
  { userID :: Int
  }
  deriving (Generic)

instance FromJSON UpdateUserJob

instance ToJSON UpdateUserJob

handleUpdateUser :: (MonadIO m) => Connection -> UpdateUserJob -> m ()
handleUpdateUser conn UpdateUserJob {userID} = do
  -- Get user.
  maybeUser <- getUserByID conn userID
  user <- case maybeUser of
    Just u -> return u
    Nothing -> error $ "UpdateUserJob: could not find user with ID " <> show userID

  -- Start update (get totals).
  -- TODO: verify that laziness actually doesn't evaluate the second tuple
  -- element yet.
  let creds = spotifyCredentials user
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
  -- Finish the updating status.
  completeUserFollowUpdate conn user

enqueueUpdateUser :: (MonadIO m) => Connection -> User -> m JobId
enqueueUpdateUser conn user = undefined
