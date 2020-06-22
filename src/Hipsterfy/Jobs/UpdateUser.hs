module Hipsterfy.Jobs.UpdateUser (handleUpdateUser) where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple (Connection)
import Hipsterfy.Spotify.Auth (SpotifyApp (..))
import Hipsterfy.User (User(..), getUserByID, setUserFollowUpdating)
import Relude

{- HLINT ignore UpdateUserJob "Use newtype instead of data" -}
data UpdateUserJob = UpdateUserJob {
  userID :: Int
} deriving (Generic)

instance FromJSON UpdateUserJob

instance ToJSON UpdateUserJob

handleUpdateUser :: (MonadIO m) => SpotifyApp -> Connection -> UpdateUserJob -> m ()
handleUpdateUser app conn UpdateUserJob { userID } = do
  -- Get user.
  maybeUser <- getUserByID conn userID
  user <- case maybeUser of
    Just u -> return u
    Nothing -> error $ "UpdateUserJob: could not find user with ID " <> show userID

  -- Set user update status.
  -- Run the update.
  -- Enqueue any artist updates needed.
  -- Finish the updating status.
  undefined
