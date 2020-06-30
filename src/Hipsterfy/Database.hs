{-# LANGUAGE Arrows #-}

module Hipsterfy.Database (demo, getUser') where

import Control.Arrow (returnA)
import Data.Profunctor.Product (p2)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection)
import Hipsterfy.Spotify (SpotifyUserID)
import Hipsterfy.User (UserID)
import Opaleye
  ( Field,
    FieldNullable,
    Select,
    SqlInt4,
    SqlText,
    SqlTimestamptz,
    Table,
    TableFields,
    optional,
    required,
    runSelect,
    selectTable,
    showSql,
    table,
  )
import Relude hiding (optional)

data SpotifyCredentialsT t ts = SpotifyCredentialsT
  { accessToken :: t,
    refreshToken :: t,
    expiration :: ts
  }
  deriving (Show)

type SpotifyCredentials = SpotifyCredentialsT Text UTCTime

type SpotifyCredentialsF = SpotifyCredentialsT (Field SqlText) (Field SqlTimestamptz)

$(makeAdaptorAndInstance "pSpotifyCredentials" ''SpotifyCredentialsT)

selectSpotifyCredentials :: TableFields SpotifyCredentialsF SpotifyCredentialsF
selectSpotifyCredentials =
  pSpotifyCredentials
    SpotifyCredentialsT
      { accessToken = required "spotify_access_token",
        expiration = required "spotify_access_token_expiration",
        refreshToken = required "spotify_refresh_token"
      }

data UserT uid suid t creds = UserT
  { userID :: uid,
    friendCode :: t,
    spotifyUserID :: suid,
    spotifyUserName :: t,
    spotifyCredentials :: creds
  }
  deriving (Show)

type User = UserT UserID SpotifyUserID Text SpotifyCredentials

type UserReadF = UserT (Field SqlInt4) (Field SqlText) (Field SqlText) SpotifyCredentialsF

type UserWriteF = UserT (Maybe (Field SqlInt4)) (Field SqlText) (Field SqlText) SpotifyCredentialsF

$(makeAdaptorAndInstance "pUser" ''UserT)

selectUser :: TableFields UserWriteF UserReadF
selectUser =
  pUser
    UserT
      { userID = optional "id",
        friendCode = required "friend_code",
        spotifyUserID = required "spotify_user_id",
        spotifyUserName = required "spotify_user_name",
        spotifyCredentials = selectSpotifyCredentials
      }

data UpdateJobInfoT ts = UpdateJobInfoT
  { lastUpdateJobSubmitted :: ts,
    lastUpdateJobCompleted :: ts
  }

type UpdateJobInfo = UpdateJobInfoT (Maybe UTCTime)

type UpdateJobInfoReadF = UpdateJobInfoT (FieldNullable SqlTimestamptz)

type UpdateJobInfoWriteF = UpdateJobInfoT (Maybe (FieldNullable SqlTimestamptz))

$(makeAdaptorAndInstance "pUpdateJobInfo" ''UpdateJobInfoT)

selectUpdateJobInfo :: TableFields UpdateJobInfoWriteF UpdateJobInfoReadF
selectUpdateJobInfo =
  pUpdateJobInfo
    UpdateJobInfoT
      { lastUpdateJobSubmitted = optional "last_update_job_submitted",
        lastUpdateJobCompleted = optional "last_update_job_completed"
      }

type UserTableReadF = (UserReadF, UpdateJobInfoReadF)

type UserTableWriteF = (UserWriteF, UpdateJobInfoWriteF)

userTable :: Table UserTableWriteF UserTableReadF
userTable =
  table
    "hipsterfy_user"
    (p2 (selectUser, selectUpdateJobInfo))

getUser :: Select UserReadF
getUser = proc () -> do
  (user, _) <- selectTable userTable -< ()
  returnA -< user

getUser' :: Connection -> IO [User]
getUser' conn = runSelect conn getUser

demo :: Maybe String
demo = showSql getUser
