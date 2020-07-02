module Hipsterfy.Database.User
  ( User,
    UserT (User),
    UserReadF,
    UserWriteF,
    userTable,
  )
where

import Data.Profunctor.Product (p2)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time (UTCTime)
import Hipsterfy.Database.Jobs (UpdateJobInfoF, updateJobInfoColumns)
import Hipsterfy.Spotify (SpotifyUserID)
import Hipsterfy.User (UserID)
import Opaleye.Field (Field)
import Opaleye.SqlTypes (SqlInt4, SqlText, SqlTimestamptz)
import Opaleye.Table (Table, TableFields, optional, required, table)
import Relude hiding (optional)

data SpotifyCredentialsT t ts = SpotifyCredentials
  { accessToken :: t,
    refreshToken :: t,
    expiration :: ts
  }
  deriving (Show)

type SpotifyCredentials = SpotifyCredentialsT Text UTCTime

type SpotifyCredentialsF = SpotifyCredentialsT (Field SqlText) (Field SqlTimestamptz)

$(makeAdaptorAndInstance "pSpotifyCredentials" ''SpotifyCredentialsT)

spotifyCredentialsColumns :: TableFields SpotifyCredentialsF SpotifyCredentialsF
spotifyCredentialsColumns =
  pSpotifyCredentials
    SpotifyCredentials
      { accessToken = required "spotify_access_token",
        expiration = required "spotify_access_token_expiration",
        refreshToken = required "spotify_refresh_token"
      }

data UserT uid suid t creds = User
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

userColumns :: TableFields UserWriteF UserReadF
userColumns =
  pUser
    User
      { userID = optional "id",
        friendCode = required "friend_code",
        spotifyUserID = required "spotify_user_id",
        spotifyUserName = required "spotify_user_name",
        spotifyCredentials = spotifyCredentialsColumns
      }

type UserTableReadF = (UserReadF, UpdateJobInfoF)

type UserTableWriteF = (UserWriteF, UpdateJobInfoF)

userTable :: Table UserTableWriteF UserTableReadF
userTable = table "hipsterfy_user" $ p2 (userColumns, updateJobInfoColumns)
