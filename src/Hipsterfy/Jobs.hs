module Hipsterfy.Jobs
  ( UpdateStatus (..),
    UpdateJobInfo (..),
    infoToStatus,
  )
where

import Data.Hashable.Time ()
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Relude

data UpdateStatus
  = NeedsUpdate
  | RecentlyUpdatedAt UTCTime
  | QueuedAt UTCTime
  deriving (Show, Eq)

jobInQueueTimeout :: NominalDiffTime
jobInQueueTimeout = 60 * 5

recentUpdateTimeout :: NominalDiffTime
recentUpdateTimeout = 60 * 60 * 24 * 10

data UpdateJobInfo = UpdateJobInfo
  { lastUpdateJobSubmitted :: Maybe UTCTime,
    lastUpdateJobCompleted :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic)

instance Hashable UpdateJobInfo

infoToStatus :: (MonadIO m) => UpdateJobInfo -> m UpdateStatus
infoToStatus UpdateJobInfo {lastUpdateJobSubmitted, lastUpdateJobCompleted} = do
  now <- liftIO getCurrentTime
  return $ case lastUpdateJobSubmitted of
    Just submitted ->
      case lastUpdateJobCompleted of
        Just completed ->
          if completed > submitted
            then checkCompleted now completed
            else checkSubmitted now submitted
        Nothing -> checkSubmitted now submitted
    Nothing ->
      case lastUpdateJobCompleted of
        Just _ -> error "infoToStatus: impossible: job completed without submitting"
        Nothing -> NeedsUpdate
  where
    checkCompleted :: UTCTime -> UTCTime -> UpdateStatus
    checkCompleted now completed =
      if diffUTCTime now completed > recentUpdateTimeout
        then NeedsUpdate
        else RecentlyUpdatedAt completed
    checkSubmitted :: UTCTime -> UTCTime -> UpdateStatus
    checkSubmitted now submitted =
      if diffUTCTime now submitted > jobInQueueTimeout
        then NeedsUpdate
        else QueuedAt submitted
