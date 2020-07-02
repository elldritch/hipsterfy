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
import Hipsterfy.Artist (Artist (..), createArtistIfNotExists)
import Hipsterfy.Jobs.UpdateArtist (enqueueUpdateArtist)
import Hipsterfy.Spotify
  ( getFollowedSpotifyArtists,
    getSpotifyArtistsOfSavedAlbums,
    getSpotifyArtistsOfSavedTracks,
  )
import Hipsterfy.User
  ( UpdateStatus (..),
    User (..),
    UserID,
    getUpdateStatus,
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
  status <- getUpdateStatus userID
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
  let spotifyArtists = ordNub $ followedArtists ++ trackArtists ++ albumArtists
  artists <- mapM createArtistIfNotExists spotifyArtists
  let artistIDs = map artistID artists

  -- Update followed artists.
  setFollowedArtists userID artistIDs

  -- Enqueue artist updates needed.
  Parallel.mapM_ enqueueUpdateArtist artistIDs

  -- Set the update status.
  setUpdateCompleted userID
