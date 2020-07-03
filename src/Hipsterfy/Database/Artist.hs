module Hipsterfy.Database.Artist
  ( Artist,
    ArtistT (..),
    ArtistReadF,
    ArtistWriteF,
    ArtistID,
    ArtistIDT (..),
    ArtistIDReadF,
    pArtistID,
    pArtist,
    artistTable,
    ArtistListeners,
    ArtistListenersT (..),
    ArtistListenersReadF,
    ArtistListenersWriteF,
    pArtistListeners,
    artistListenersTable,
  )
where

import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time (UTCTime)
import Hipsterfy.Database.Jobs (UpdateJobInfo, UpdateJobInfoF, updateJobInfoColumns)
import Hipsterfy.Spotify (SpotifyArtistID)
import Opaleye.Field (Field)
import Opaleye.SqlTypes (SqlInt4, SqlText, SqlTimestamptz)
import Opaleye.Table (Table, TableFields, optional, required, table)
import Relude hiding (optional)

-- "spotify_artist" table.

newtype ArtistIDT a = ArtistIDT a
  deriving (Show, Eq, Ord, FromJSON, ToJSON, Functor)

type ArtistID = ArtistIDT Int

type ArtistIDReadF = ArtistIDT (Field SqlInt4)

type ArtistIDWriteF = ArtistIDT (Maybe (Field SqlInt4))

$(makeAdaptorAndInstance "pArtistID" ''ArtistIDT)

data ArtistT aid said t u = ArtistT
  { artistID :: aid,
    name :: t,
    spotifyArtistID :: said,
    spotifyURL :: t,
    updateJobInfo :: u
  }
  deriving (Show)

type Artist = ArtistT ArtistID SpotifyArtistID Text UpdateJobInfo

type ArtistReadF = ArtistT ArtistIDReadF (Field SqlText) (Field SqlText) UpdateJobInfoF

type ArtistWriteF = ArtistT ArtistIDWriteF (Field SqlText) (Field SqlText) UpdateJobInfoF

$(makeAdaptorAndInstance "pArtist" ''ArtistT)

artistColumns :: TableFields ArtistWriteF ArtistReadF
artistColumns =
  pArtist
    ArtistT
      { artistID = pArtistID $ ArtistIDT $ optional "id",
        name = required "name",
        spotifyArtistID = required "spotify_artist_id",
        spotifyURL = required "spotify_url",
        updateJobInfo = updateJobInfoColumns
      }

artistTable :: Table ArtistWriteF ArtistReadF
artistTable = table "spotify_artist" artistColumns

-- "spotify_artist_listeners" table.

data ArtistListenersT aid t i = ArtistListenersT
  { listenersArtistID :: aid,
    createdAt :: t,
    monthlyListeners :: i
  }

type ArtistListeners = ArtistListenersT ArtistID UTCTime Int

type ArtistListenersReadF = ArtistListenersT ArtistIDReadF (Field SqlTimestamptz) (Field SqlInt4)

type ArtistListenersWriteF = ArtistListenersT ArtistIDReadF (Maybe (Field SqlTimestamptz)) (Field SqlInt4)

$(makeAdaptorAndInstance "pArtistListeners" ''ArtistListenersT)

artistListenersColumns :: TableFields ArtistListenersWriteF ArtistListenersReadF
artistListenersColumns =
  pArtistListeners
    ArtistListenersT
      { listenersArtistID = pArtistID $ ArtistIDT $ required "spotify_artist_id",
        createdAt = optional "created_at",
        monthlyListeners = required "monthly_listeners"
      }

artistListenersTable :: Table ArtistListenersWriteF ArtistListenersReadF
artistListenersTable = table "spotify_artist_listeners" artistListenersColumns
