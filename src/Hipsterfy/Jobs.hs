module Hipsterfy.Jobs (UpdateStatus (..), getUpdateStatusRaw) where

import Data.Time (diffUTCTime, NominalDiffTime, UTCTime, getCurrentTime)
import Database.PostgreSQL.Simple (Connection, Only (..), query)
import Database.PostgreSQL.Simple.ToField (ToField)
import Relude

data UpdateStatus
  = NeedsUpdate
  | RecentlyUpdatedAt UTCTime
  | QueuedAt UTCTime

jobInQueueTimeout :: NominalDiffTime
jobInQueueTimeout = 60 * 5

recentUpdateTimeout :: NominalDiffTime
recentUpdateTimeout = 60 * 60 * 24

-- WARNING: this is super fragile and schema-dependent.

getUpdateStatusRaw :: (MonadIO m, ToField i, Show i) => Connection -> Text -> i -> m UpdateStatus
getUpdateStatusRaw conn table rowID = do
  now <- liftIO getCurrentTime
  row <-
    liftIO $
      query
        conn
        ("SELECT last_update_job_submitted, last_update_job_completed FROM " <> fromString (toString table) <> " WHERE id = ?")
        (Only rowID)
  return $ case row of
    [(lastJobSubmitted, lastJobCompleted)] ->
      case lastJobSubmitted of
        Just submitted ->
          case lastJobCompleted of
            Just completed ->
              if completed > submitted
                then checkCompleted now completed
                else checkSubmitted now submitted
            Nothing -> checkSubmitted now submitted
        Nothing ->
          case lastJobCompleted of
            Just _ ->
              error $ "getUpdateStatusRaw: impossible: job completed without submitting for row " <> show rowID
            Nothing -> NeedsUpdate
    [] -> error $ "getUpdateStatusRaw: could not find row with ID " <> show rowID
    _ -> error $ "getUpdateStatusRaw: impossible: selected multiple rows with ID " <> show rowID
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
