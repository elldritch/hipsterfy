module Hipsterfy.Jobs.UpdateUser
  ( handleUpdateUser,
    enqueueUpdateUser,
    forceEnqueueUpdateUser,
    updateUserQueue,
  )
where

import qualified Control.Monad.Parallel as Parallel (mapM_)
import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple (Connection)
import Faktory.Client (Client)
import Faktory.Job (perform, queue)
import Faktory.Settings (Queue (Queue))
import Hipsterfy.Artist (Artist (..), createArtistIfNotExists)
import Hipsterfy.Jobs.UpdateArtist (enqueueUpdateArtist)
import Hipsterfy.Spotify
  ( getFollowedSpotifyArtists,
    getSpotifyArtistsOfSavedAlbums,
    getSpotifyArtistsOfSavedTracks,
  )
import Hipsterfy.Spotify.Auth (SpotifyApp (..))
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
updateUserQueue = Queue "update-user"

{- HLINT ignore UpdateUserJob "Use newtype instead of data" -}
data UpdateUserJob = UpdateUserJob
  { userID :: UserID
  }
  deriving (Generic)

instance FromJSON UpdateUserJob

instance ToJSON UpdateUserJob

enqueueUpdateUser :: (MonadIO m) => Client -> Connection -> UserID -> m ()
enqueueUpdateUser client conn userID = do
  status <- getUpdateStatus conn userID
  case status of
    NeedsUpdate -> forceEnqueueUpdateUser client conn userID
    _ -> pass

forceEnqueueUpdateUser :: (MonadIO m) => Client -> Connection -> UserID -> m ()
forceEnqueueUpdateUser client conn userID = do
  setUpdateSubmitted conn userID
  void $ liftIO $ perform (queue updateUserQueue) client UpdateUserJob {userID}

handleUpdateUser :: (MonadIO m) => SpotifyApp -> Client -> Connection -> UpdateUserJob -> m ()
handleUpdateUser app client conn UpdateUserJob {userID} = do
  -- Get user.
  maybeUser <- getUserByID conn userID
  User {spotifyCredentials} <- case maybeUser of
    Just u -> return u
    Nothing -> error $ "handleUpdateUser: could not find user with ID " <> show userID

  -- Get artists.
  creds <- refreshCredentialsIfNeeded app conn userID spotifyCredentials
  followedArtists <- getFollowedSpotifyArtists creds
  trackArtists <- getSpotifyArtistsOfSavedTracks creds
  albumArtists <- getSpotifyArtistsOfSavedAlbums creds

  -- Create artists.
  let spotifyArtists = ordNub $ followedArtists ++ trackArtists ++ albumArtists
  artists <- mapM (createArtistIfNotExists conn) spotifyArtists
  let artistIDs = map artistID artists

  -- Update followed artists.
  setFollowedArtists conn userID artistIDs

  -- Enqueue artist updates needed.
  liftIO $ Parallel.mapM_ (enqueueUpdateArtist client conn) artistIDs

  -- Set the update status.
  setUpdateCompleted conn userID
