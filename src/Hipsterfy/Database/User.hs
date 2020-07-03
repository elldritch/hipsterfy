module Hipsterfy.Database.User
  ( UserT (..),
    User,
    UserReadF,
    UserWriteF,
    UserIDT (..),
    UserID,
    UserIDReadF,
    SpotifyCredentialsT (..),
    SpotifyCredentials,
    pUserID,
    userTable,
    UserArtistFollowT (..),
    UserArtistFollowF,
    pUserArtistFollow,
    userArtistFollowTable,
    SpotifyOAuthRequestT (..),
    SpotifyOAuthRequest,
    SpotifyOAuthRequestF,
    spotifyOAuthRequestTable,
    UserSessionT (..),
    UserSession,
    UserSessionF,
    userSessionTable,
  )
where

import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time (UTCTime)
import Hipsterfy.Database.Artist (ArtistIDReadF, ArtistIDT (..), pArtistID)
import Hipsterfy.Database.Jobs (UpdateJobInfo, UpdateJobInfoF, updateJobInfoColumns)
import Hipsterfy.Spotify (SpotifyUserID)
import Opaleye.Field (Field)
import Opaleye.SqlTypes (SqlInt4, SqlText, SqlTimestamptz)
import Opaleye.Table (Table, TableFields, optional, required, table)
import Relude hiding (optional)

-- "hipsterfy_user" table.

data SpotifyCredentialsT t ts = SpotifyCredentialsT
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
    SpotifyCredentialsT
      { accessToken = required "spotify_access_token",
        refreshToken = required "spotify_refresh_token",
        expiration = required "spotify_access_token_expiration"
      }

newtype UserIDT a = UserIDT a
  deriving (Show, Eq, Ord, FromJSON, ToJSON, Functor)

type UserID = UserIDT Int

type UserIDReadF = UserIDT (Field SqlInt4)

type UserIDWriteF = UserIDT (Maybe (Field SqlInt4))

$(makeAdaptorAndInstance "pUserID" ''UserIDT)

data UserT uid suid t creds u = UserT
  { userID :: uid,
    friendCode :: t,
    spotifyUserID :: suid,
    spotifyUserName :: t,
    spotifyCredentials :: creds,
    updateJobInfo :: u
  }
  deriving (Show)

type User = UserT UserID SpotifyUserID Text SpotifyCredentials UpdateJobInfo

type UserReadF = UserT UserIDReadF (Field SqlText) (Field SqlText) SpotifyCredentialsF UpdateJobInfoF

type UserWriteF = UserT UserIDWriteF (Field SqlText) (Field SqlText) SpotifyCredentialsF UpdateJobInfoF

$(makeAdaptorAndInstance "pUser" ''UserT)

userColumns :: TableFields UserWriteF UserReadF
userColumns =
  pUser
    UserT
      { userID = pUserID $ UserIDT $ optional "id",
        friendCode = required "friend_code",
        spotifyUserID = required "spotify_user_id",
        spotifyUserName = required "spotify_user_name",
        spotifyCredentials = spotifyCredentialsColumns,
        updateJobInfo = updateJobInfoColumns
      }

userTable :: Table UserWriteF UserReadF
userTable = table "hipsterfy_user" userColumns

-- "hipsterfy_user_spotify_artist_follow" table.

data UserArtistFollowT uid aid = UserArtistFollowT
  { followUserID :: uid,
    followArtistID :: aid
  }

type UserArtistFollowF = UserArtistFollowT UserIDReadF ArtistIDReadF

$(makeAdaptorAndInstance "pUserArtistFollow" ''UserArtistFollowT)

userArtistFollowTable :: Table UserArtistFollowF UserArtistFollowF
userArtistFollowTable =
  table "hipsterfy_user_spotify_artist_follow" $
    pUserArtistFollow
      UserArtistFollowT
        { followUserID = pUserID $ UserIDT $ required "user_id",
          followArtistID = pArtistID $ ArtistIDT $ required "spotify_artist_id"
        }

-- "spotify_oauth_request" table.

newtype SpotifyOAuthRequestT t = SpotifyOAuthRequestT
  { oauth2State :: t
  }

type SpotifyOAuthRequest = SpotifyOAuthRequestT Text

type SpotifyOAuthRequestF = SpotifyOAuthRequestT (Field SqlText)

$(makeAdaptorAndInstance "pSpotifyOAuthRequest" ''SpotifyOAuthRequestT)

spotifyOAuthRequestTable :: Table SpotifyOAuthRequestF SpotifyOAuthRequestF
spotifyOAuthRequestTable =
  table "spotify_oauth_request" $
    pSpotifyOAuthRequest
      SpotifyOAuthRequestT
        { oauth2State = required "oauth2_state"
        }

-- "hipsterfy_user_session" table.

data UserSessionT uid s = UserSessionT
  { sessionUserID :: uid,
    cookieSecret :: s
  }

type UserSession = UserSessionT UserID Text

type UserSessionF = UserSessionT UserIDReadF (Field SqlText)

$(makeAdaptorAndInstance "pUserSession" ''UserSessionT)

userSessionTable :: Table UserSessionF UserSessionF
userSessionTable =
  table "hipsterfy_user_session" $
    pUserSession
      UserSessionT
        { sessionUserID = pUserID $ UserIDT $ required "user_id",
          cookieSecret = required "cookie_secret"
        }
