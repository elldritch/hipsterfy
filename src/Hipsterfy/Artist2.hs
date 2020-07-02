module Hipsterfy.Artist2
  ( Artist (..),
    ArtistID,
    toDatabaseArtistID,
    fromDatabaseArtistID,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (Day)
import Hipsterfy.Database.Artist (ArtistIDReadF, ArtistIDT (..))
import qualified Hipsterfy.Database.Artist as D (ArtistID)
import Hipsterfy.Jobs (UpdateJobInfo)
import Hipsterfy.Spotify (SpotifyArtist)
import Opaleye.SqlTypes (sqlInt4)
import Relude

newtype ArtistID = ArtistID Int
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

data Artist = Artist
  { artistID :: ArtistID,
    spotifyArtist :: SpotifyArtist,
    monthlyListeners :: Map Day Int,
    updateJobInfo :: UpdateJobInfo
  }
  deriving (Show, Eq)

toDatabaseArtistID :: ArtistID -> ArtistIDReadF
toDatabaseArtistID (ArtistID i) = ArtistIDT $ sqlInt4 i

fromDatabaseArtistID :: D.ArtistID -> ArtistID
fromDatabaseArtistID (ArtistIDT i) = ArtistID i
