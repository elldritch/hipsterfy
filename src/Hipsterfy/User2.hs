{-# LANGUAGE Arrows #-}

module Hipsterfy.User2
  ( User (..),
    UserID,
    getUserByID,
    getUserBySpotifyID,
    getUserByFriendCode,
    refreshCredentialsIfNeeded,
    getFollowedArtists,
    setFollowedArtists,
  )
where

import Control.Arrow (returnA)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Data.Profunctor.Product (p2)
import Data.Time (UTCTime, getCurrentTime, utctDay)
import Hipsterfy.Application (Config (..), MonadApp)
import Hipsterfy.Artist2 (Artist (..), ArtistID, fromDatabaseArtistID, toDatabaseArtistID)
import Hipsterfy.Database (QueryParameters, runDelete, runInsert, runSelect, runSelectOne, runUpdate)
import Hipsterfy.Database.Artist (ArtistIDT (..), ArtistListenersF, ArtistListenersT (..), ArtistReadF, ArtistT (..), artistListenersTable, artistTable, pArtist, pArtistID, pArtistListeners)
import qualified Hipsterfy.Database.Artist as D (Artist)
import Hipsterfy.Database.Jobs (UpdateJobInfoT (..), pUpdateJobInfo)
import Hipsterfy.Database.User (SpotifyCredentialsT (..), UserArtistFollowT (..), UserIDReadF, UserIDT (..), UserReadF, UserT (..), userArtistFollowTable, userTable)
import qualified Hipsterfy.Database.User as D (User, UserID)
import Hipsterfy.Jobs (UpdateJobInfo (..))
import Hipsterfy.Spotify (SpotifyArtist (..), SpotifyUserID (..))
import Hipsterfy.Spotify.Auth (Scope, SpotifyCredentials (..), requestAccessTokenFromRefreshToken)
import Opaleye.Aggregate (aggregate, arrayAgg, groupBy)
import Opaleye.Field (Field)
import Opaleye.Manipulation (Delete (..), Insert (..), Update (..), rCount, updateEasy)
import Opaleye.Operators ((.==), (.===), restrict)
import Opaleye.Select (Select)
import Opaleye.SqlTypes (SqlArray, SqlBool, SqlInt4, SqlTimestamptz, sqlInt4, sqlStrictText, sqlUTCTime)
import Opaleye.Table (selectTable)
import Relude

-- Application data structures.

newtype UserID = UserID Int
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

data User = User
  { userID :: UserID,
    friendCode :: Text,
    spotifyUserID :: SpotifyUserID,
    spotifyUserName :: Text,
    spotifyCredentials :: SpotifyCredentials,
    updateJobInfo :: UpdateJobInfo
  }

toUser :: D.User -> User
toUser UserT {..} =
  let SpotifyCredentialsT {..} = spotifyCredentials
      UpdateJobInfoT {..} = updateJobInfo
   in User
        { userID = fromDatabaseUserID userID,
          friendCode,
          spotifyUserID,
          spotifyUserName,
          spotifyCredentials = SpotifyCredentials {accessToken, refreshToken, expiration},
          updateJobInfo = UpdateJobInfo {lastUpdateJobSubmitted, lastUpdateJobCompleted}
        }

toDatabaseUserID :: UserID -> UserIDReadF
toDatabaseUserID (UserID i) = UserIDT $ sqlInt4 i

fromDatabaseUserID :: D.UserID -> UserID
fromDatabaseUserID (UserIDT i) = UserID i

-- Named parameter sets for tracing.

newtype UserIDParam = UserIDParam {userID :: UserID} deriving (Generic)

instance QueryParameters UserIDParam

newtype SpotifyUserIDParam = SpotifyUserIDParam {spotifyUserID :: SpotifyUserID} deriving (Generic)

instance QueryParameters SpotifyUserIDParam

newtype FriendCodeParam = FriendCodeParam {friendCode :: Text} deriving (Generic)

instance QueryParameters FriendCodeParam

data FollowedArtistsParams = FollowedArtistsParams
  { userID :: UserID,
    artistIDs :: [ArtistID]
  }
  deriving (Generic)

instance QueryParameters FollowedArtistsParams


-- Signup and creation.

createOAuthRedirect :: (MonadApp m) => [Scope] -> m LText
createOAuthRedirect scopes = do
  undefined
  -- Config {postgres, spotifyApp} <- ask
  -- oauthState <- liftIO $ toText <$> randomWord randomASCII 20
  -- void $ liftIO $ execute postgres "INSERT INTO spotify_oauth_request (oauth2_state, created_at) VALUES (?, NOW())" (Only oauthState)
  -- return $ authorizationURL spotifyApp scopes oauthState

createUser :: (MonadApp m) => Text -> Text -> m (Either Text User)
createUser authCode oauthState =
  undefined
  -- runExceptT $ do
  --   Config {spotifyApp, postgres} <- ask
  --   -- Validate the OAuth state, then delete that state.
  --   oauthStateRows <- liftIO (query postgres "SELECT oauth2_state FROM spotify_oauth_request WHERE oauth2_state = ?" (Only oauthState) :: IO [Only Text])
  --   spotifyCredentials <- liftEither =<< case oauthStateRows of
  --     [_] -> do
  --       void $ liftIO $ execute postgres "DELETE FROM spotify_oauth_request WHERE oauth2_state = ?" (Only oauthState)
  --       Right <$> requestAccessTokenFromAuthorizationCode spotifyApp authCode
  --     _ -> throwError "invalid OAuth request state"

  --   -- Exchange OAuth authorization code for credentials.
  --   spotifyUser@SpotifyUser {spotifyUserID, spotifyUserName} <- liftIO $ getSpotifyUser spotifyCredentials

  --   -- Construct a user if one doesn't already exist.
  --   user <- lift $ getUserBySpotifyID spotifyUserID
  --   case user of
  --     Just u -> return u
  --     Nothing -> do
  --       friendCode <- liftIO $ toText <$> randomWord randomASCII 20
  --       userRows <- liftIO $ insertUser postgres friendCode spotifyUser spotifyCredentials
  --       case userRows of
  --         [Only userID] -> return $ User {userID, friendCode, spotifyUserID, spotifyUserName, spotifyCredentials, lastUpdated = Nothing}
  --         [] -> error "createUser: impossible: insert of single User returned 0 rows"
  --         _ -> error "createUser: impossible: insert of single User returned more than 1 row"
  -- where
  --   insertUser conn friendCode SpotifyUser {spotifyUserID, spotifyUserName} SpotifyCredentials {accessToken, expiration, refreshToken} =
  --     query
  --       conn
  --       "INSERT INTO hipsterfy_user\
  --       \ (friend_code,\
  --       \  spotify_user_id,\
  --       \  spotify_user_name,\
  --       \  spotify_access_token,\
  --       \  spotify_access_token_expiration,\
  --       \  spotify_refresh_token,\
  --       \  created_at)\
  --       \ VALUES (?, ?, ?, ?, ?, ?, NOW())\
  --       \ RETURNING id"
  --       ( friendCode,
  --         spotifyUserID,
  --         spotifyUserName,
  --         accessToken,
  --         expiration,
  --         refreshToken
  --       )

-- Retrieval.

getUserBy :: (UserReadF -> Field SqlBool) -> Select UserReadF
getUserBy restriction = proc () -> do
  user <- selectTable userTable -< ()
  restrict -< restriction user
  returnA -< user

getUserByID :: (MonadApp m) => UserID -> m (Maybe User)
getUserByID userID = do
  maybeUser <- runSelectOne "getUserByID" makeSelect $ UserIDParam userID
  return $ toUser <$> maybeUser
  where
    makeSelect :: UserIDParam -> Select UserReadF
    makeSelect UserIDParam {userID = paramUID} =
      getUserBy (\UserT {userID = rowUID} -> rowUID .=== toDatabaseUserID paramUID)

getUserBySpotifyID :: (MonadApp m) => SpotifyUserID -> m (Maybe User)
getUserBySpotifyID spotifyUserID = do
  maybeUser <- runSelectOne "getUserBySpotifyID" makeSelect $ SpotifyUserIDParam spotifyUserID
  return $ toUser <$> maybeUser
  where
    makeSelect :: SpotifyUserIDParam -> Select UserReadF
    makeSelect SpotifyUserIDParam {spotifyUserID = (SpotifyUserID paramSUID)} =
      getUserBy (\UserT {spotifyUserID = rowSUID} -> rowSUID .== sqlStrictText paramSUID)

getUserByFriendCode :: (MonadApp m) => Text -> m (Maybe User)
getUserByFriendCode friendCode = do
  maybeUser <- runSelectOne "getUserByFriendCode" makeSelect $ FriendCodeParam friendCode
  return $ toUser <$> maybeUser
  where
    makeSelect :: FriendCodeParam -> Select UserReadF
    makeSelect FriendCodeParam {friendCode = paramFC} =
      getUserBy (\UserT {friendCode = rowFC} -> rowFC .== sqlStrictText paramFC)

-- Operations.

refreshCredentialsIfNeeded :: (MonadApp m) => UserID -> SpotifyCredentials -> m SpotifyCredentials
refreshCredentialsIfNeeded userID creds@SpotifyCredentials {expiration} = do
  Config {spotifyApp} <- ask
  now <- liftIO getCurrentTime
  if now > expiration
    then do
      refreshedCreds <- requestAccessTokenFromRefreshToken spotifyApp creds
      updateCreds refreshedCreds
      return refreshedCreds
    else return creds
  where
    updateCreds :: (MonadApp m) => SpotifyCredentials -> m ()
    updateCreds refreshedCreds = void $ runUpdate "updateCreds" makeUpdate refreshedCreds
    makeUpdate :: SpotifyCredentials -> Update Int64
    makeUpdate SpotifyCredentials {accessToken, refreshToken, expiration = e} =
      Update
        { uTable = userTable,
          uUpdateWith = updateEasy $
            \u ->
              ( u
                  { spotifyCredentials =
                      SpotifyCredentialsT
                        { accessToken = sqlStrictText accessToken,
                          refreshToken = sqlStrictText refreshToken,
                          expiration = sqlUTCTime e
                        }
                  } ::
                  UserReadF
              ),
          uWhere = \UserT {userID = rowUID} -> rowUID .=== toDatabaseUserID userID,
          uReturning = rCount
        }

type ArtistListenersArrayAggF =
  ArtistListenersT
    (ArtistIDT (Field (SqlArray SqlInt4)))
    (Field (SqlArray SqlTimestamptz))
    (Field (SqlArray SqlInt4))

type ArtistListenersArrayAgg = ArtistListenersT (ArtistIDT [Int]) [UTCTime] [Int]

getFollowedArtists :: (MonadApp m) => UserID -> m [Artist]
getFollowedArtists userID = do
  (artistAndAggListeners :: [(D.Artist, ArtistListenersArrayAgg)]) <-
    runSelect "getFollowedArtists" makeSelect $ UserIDParam userID
  return $ map toArtist artistAndAggListeners
  where
    makeSelect :: UserIDParam -> Select (ArtistReadF, ArtistListenersArrayAggF)
    makeSelect (UserIDParam paramUID) = aggregateListeners getFollowedArtistsAndListeners
      where
        getFollowedArtistsAndListeners :: Select (ArtistReadF, ArtistListenersF)
        getFollowedArtistsAndListeners = proc () -> do
          UserT {userID = rowUID} <- selectTable userTable -< ()
          UserArtistFollowT {followUserID, followArtistID} <- selectTable userArtistFollowTable -< ()
          artist@ArtistT {artistID = rowAID} <- selectTable artistTable -< ()
          listeners@ArtistListenersT {listenersArtistID = listenerAID} <- selectTable artistListenersTable -< ()

          restrict -< rowUID .=== toDatabaseUserID paramUID
          restrict -< rowUID .=== followUserID
          restrict -< rowAID .=== followArtistID
          restrict -< rowAID .=== listenerAID

          returnA -< (artist, listeners)
        aggregateListeners :: Select (ArtistReadF, ArtistListenersF) -> Select (ArtistReadF, ArtistListenersArrayAggF)
        aggregateListeners =
          aggregate
            ( p2
                ( pArtist
                    ArtistT
                      { artistID = pArtistID $ ArtistIDT groupBy,
                        name = groupBy,
                        spotifyArtistID = groupBy,
                        spotifyURL = groupBy,
                        updateJobInfo =
                          pUpdateJobInfo $
                            UpdateJobInfoT
                              { lastUpdateJobSubmitted = groupBy,
                                lastUpdateJobCompleted = groupBy
                              }
                      },
                  pArtistListeners
                    ArtistListenersT
                      { listenersArtistID = pArtistID $ ArtistIDT arrayAgg,
                        createdAt = arrayAgg,
                        monthlyListeners = arrayAgg
                      }
                )
            )
    toArtist :: (D.Artist, ArtistListenersArrayAgg) -> Artist
    toArtist (ArtistT {..}, ArtistListenersT {..}) =
      let UpdateJobInfoT {..} = updateJobInfo
       in Artist
            { artistID = fromDatabaseArtistID artistID,
              spotifyArtist =
                SpotifyArtist
                  { spotifyArtistID,
                    spotifyURL,
                    name
                  },
              updateJobInfo = UpdateJobInfo {lastUpdateJobCompleted, lastUpdateJobSubmitted},
              monthlyListeners = Map.fromList $ zip (utctDay <$> createdAt) monthlyListeners
            }

setFollowedArtists :: (MonadApp m) => UserID -> [ArtistID] -> m ()
setFollowedArtists userID artistIDs = do
  -- Clear previous follows.
  void $ runDelete "deleteFollowedArtists" makeDelete $ UserIDParam userID
  -- Insert new follows.
  void $ runInsert "insertFollowedArtists" makeInserts $ FollowedArtistsParams userID artistIDs
  where
    makeDelete :: UserIDParam -> Delete Int64
    makeDelete (UserIDParam paramUID) =
      Delete
        { dTable = userArtistFollowTable,
          dWhere = \UserArtistFollowT {followUserID = rowUID} -> rowUID .=== toDatabaseUserID paramUID,
          dReturning = rCount
        }
    makeInserts :: FollowedArtistsParams -> Insert Int64
    makeInserts FollowedArtistsParams {userID = userID', artistIDs = artistIDs'} =
      Insert
        { iTable = userArtistFollowTable,
          iRows =
            map
              ( \artistID ->
                  UserArtistFollowT
                    { followUserID = toDatabaseUserID userID',
                      followArtistID = toDatabaseArtistID artistID
                    }
              )
              artistIDs',
          iReturning = rCount,
          iOnConflict = Nothing
        }
