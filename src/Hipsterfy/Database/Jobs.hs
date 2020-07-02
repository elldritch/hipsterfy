module Hipsterfy.Database.Jobs
  ( UpdateJobInfo,
    UpdateJobInfoT (..),
    UpdateJobInfoF,
    pUpdateJobInfo,
    updateJobInfoColumns,
  )
where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time (UTCTime)
import Opaleye.Field (FieldNullable)
import Opaleye.SqlTypes (SqlTimestamptz)
import Opaleye.Table (TableFields, required)
import Relude

data UpdateJobInfoT ts = UpdateJobInfoT
  { lastUpdateJobSubmitted :: ts,
    lastUpdateJobCompleted :: ts
  }

type UpdateJobInfo = UpdateJobInfoT (Maybe UTCTime)

type UpdateJobInfoF = UpdateJobInfoT (FieldNullable SqlTimestamptz)

$(makeAdaptorAndInstance "pUpdateJobInfo" ''UpdateJobInfoT)

updateJobInfoColumns :: TableFields UpdateJobInfoF UpdateJobInfoF
updateJobInfoColumns =
  pUpdateJobInfo
    UpdateJobInfoT
      { lastUpdateJobSubmitted = required "last_update_job_submitted",
        lastUpdateJobCompleted = required "last_update_job_completed"
      }
