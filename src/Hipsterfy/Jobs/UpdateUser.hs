module Hipsterfy.Jobs.UpdateUser
  ( handleUpdateUser,
    enqueueUpdateUser,
    forceEnqueueUpdateUser,
    updateUserQueue,
  )
where

import qualified Control.Monad.Parallel as Parallel (mapM_)
import Control.Monad.Parallel (MonadParallel (..))
import Data.Aeson (FromJSON, ToJSON)
import Faktory.Job (perform, queue)
import Faktory.Settings (Queue)
import Hipsterfy.Application (Config (..), Faktory (..), MonadApp)
import Hipsterfy.Artist (Artist (..), createArtist)
import Hipsterfy.Jobs (UpdateStatus (..), infoToStatus)
import Hipsterfy.Jobs.UpdateArtist (enqueueUpdateArtist)
import Hipsterfy.Spotify
  ( getFollowedSpotifyArtists,
    getSpotifyArtistsOfSavedAlbums,
    getSpotifyArtistsOfSavedTracks,
  )
import Hipsterfy.User
  ( User (..),
    UserID,
    getUserByID,
    refreshCredentialsIfNeeded,
    setFollowedArtists,
    setUpdateCompleted,
    setUpdateSubmitted,
  )
import Relude

updateUserQueue :: Queue
updateUserQueue = "update-user"

newtype UpdateUserJob = UpdateUserJob
  { userID :: UserID
  }
  deriving (Generic)

instance FromJSON UpdateUserJob

instance ToJSON UpdateUserJob

enqueueUpdateUser :: (MonadApp m) => UserID -> m ()
enqueueUpdateUser userID = do
  maybeUser <- getUserByID userID
  User {updateJobInfo} <- case maybeUser of
    Just user -> return user
    Nothing -> error $ "enqueueUpdateUser: no user found with ID " <> show userID
  status <- infoToStatus updateJobInfo
  case status of
    NeedsUpdate -> forceEnqueueUpdateUser userID
    _ -> pass

forceEnqueueUpdateUser :: (MonadApp m) => UserID -> m ()
forceEnqueueUpdateUser userID = do
  Config {faktory = Faktory {client}} <- ask
  setUpdateSubmitted userID
  void $ liftIO $ perform (queue updateUserQueue) client UpdateUserJob {userID}

handleUpdateUser :: (MonadApp m, MonadParallel m) => UpdateUserJob -> m ()
handleUpdateUser UpdateUserJob {userID} = do
  -- Get user.
  maybeUser <- getUserByID userID
  User {spotifyCredentials} <- case maybeUser of
    Just u -> return u
    Nothing -> error $ "handleUpdateUser: could not find user with ID " <> show userID

  -- Get artists.
  creds <- refreshCredentialsIfNeeded userID spotifyCredentials
  followedArtists <- getFollowedSpotifyArtists creds
  trackArtists <- getSpotifyArtistsOfSavedTracks creds
  albumArtists <- getSpotifyArtistsOfSavedAlbums creds

  -- Create artists.
  let spotifyArtists = hashNub $ followedArtists ++ trackArtists ++ albumArtists
  artists <- mapM createArtist spotifyArtists
  let artistIDs = map artistID artists

  -- Update followed artists.
  setFollowedArtists userID artistIDs

  -- Enqueue artist updates needed.
  Parallel.mapM_ enqueueUpdateArtist artistIDs

  -- Set the update status.
  setUpdateCompleted userID
