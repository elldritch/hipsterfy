{-# LANGUAGE Arrows #-}

module Hipsterfy.User
  ( UserID,
    toDatabaseUserID,
    fromDatabaseUserID,
    User (..),
    toUser,
    createOAuthRedirect,
    createUser,
    getUserByID,
    getUserBySpotifyID,
    getUserByFriendCode,
    refreshCredentialsIfNeeded,
    getFollowedArtists,
    setFollowedArtists,
    setUpdateSubmitted,
    setUpdateCompleted,
  )
where

import Control.Arrow (returnA)
import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime (..), getCurrentTime)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Hipsterfy.Application (Config (..), MonadApp)
import Hipsterfy.Artist (Artist (..), ArtistID, ArtistListenersAgg, ArtistListenersAggF, getArtistsBy, toArtist, toDatabaseArtistID)
import Hipsterfy.Database (QueryParameters, runDelete, runInsert, runInsertOne, runSelect, runSelectOne, runTransaction, runUpdate)
import Hipsterfy.Database.Artist (ArtistReadF, ArtistT (..))
import qualified Hipsterfy.Database.Artist as D (Artist)
import Hipsterfy.Database.Jobs (UpdateJobInfoF, UpdateJobInfoT (..))
import Hipsterfy.Database.User
  ( SpotifyCredentialsT (..),
    SpotifyOAuthRequest,
    SpotifyOAuthRequestF,
    SpotifyOAuthRequestT (..),
    UserArtistFollowT (..),
    UserIDReadF,
    UserIDT (..),
    UserReadF,
    UserT (..),
    spotifyOAuthRequestTable,
    userArtistFollowTable,
    userTable,
  )
import qualified Hipsterfy.Database.User as D (User, UserID)
import Hipsterfy.Jobs (UpdateJobInfo (..))
import Hipsterfy.Spotify (SpotifyUser (..), SpotifyUserID (..), getSpotifyUser)
import Hipsterfy.Spotify.Auth
  ( Scope,
    SpotifyCredentials (..),
    authorizationURL,
    requestAccessTokenFromAuthorizationCode,
    requestAccessTokenFromRefreshToken,
  )
import Opaleye.Field (Field, null, toNullable)
import Opaleye.Manipulation (Delete (..), Insert (..), Update (..), rCount, rReturning, updateEasy)
import Opaleye.Operators ((.==), (.===), restrict)
import Opaleye.Select (Select)
import Opaleye.SqlTypes (SqlBool, sqlInt4, sqlStrictText, sqlUTCTime)
import Opaleye.Table (selectTable)
import Relude hiding (null)
import Test.RandomStrings (randomASCII, randomWord)

-- Application data structures.

newtype UserID = UserID Int
  deriving (Show, Eq, Ord, FromJSON, ToJSON, FromField, ToField)

data User = User
  { userID :: UserID,
    friendCode :: Text,
    spotifyUserID :: SpotifyUserID,
    spotifyUserName :: Text,
    spotifyCredentials :: SpotifyCredentials,
    updateJobInfo :: UpdateJobInfo
  }
  deriving (Show, Generic)

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

instance QueryParameters User

newtype UserIDParam = UserIDParam {userID :: UserID} deriving (Generic)

instance QueryParameters UserIDParam

newtype SpotifyUserIDParam = SpotifyUserIDParam {spotifyUserID :: SpotifyUserID} deriving (Generic)

instance QueryParameters SpotifyUserIDParam

newtype FriendCodeParam = FriendCodeParam {friendCode :: Text} deriving (Generic)

instance QueryParameters FriendCodeParam

newtype OAuth2StateParam = OAuth2StateParam {oauth2State :: Text} deriving (Generic)

instance QueryParameters OAuth2StateParam

data FollowedArtistsParams = FollowedArtistsParams
  { userID :: UserID,
    artistIDs :: [ArtistID]
  }
  deriving (Generic)

instance QueryParameters FollowedArtistsParams

data JobSubmittedParam = JobSubmittedParam
  { userID :: UserID,
    jobSubmitted :: UTCTime
  }
  deriving (Generic)

instance QueryParameters JobSubmittedParam

data JobCompletedParam = JobCompletedParam
  { userID :: UserID,
    jobCompleted :: UTCTime
  }
  deriving (Generic)

instance QueryParameters JobCompletedParam

-- Signup and creation.

createOAuthRedirect :: (MonadApp m) => [Scope] -> m LText
createOAuthRedirect scopes = do
  Config {spotifyApp} <- ask
  oauth2State <- liftIO $ toText <$> randomWord randomASCII 20
  void $ runInsert "insertOAuth2Request" makeInsert $ OAuth2StateParam oauth2State
  return $ authorizationURL spotifyApp scopes oauth2State
  where
    makeInsert :: OAuth2StateParam -> Insert Int64
    makeInsert (OAuth2StateParam oauth2State) =
      Insert
        { iTable = spotifyOAuthRequestTable,
          iRows = [SpotifyOAuthRequestT {oauth2State = sqlStrictText oauth2State}],
          iReturning = rCount,
          iOnConflict = Nothing
        }

createUser :: (MonadApp m) => Text -> Text -> m User
createUser authCode oauth2State = do
  runTransaction $ do
    Config {spotifyApp} <- ask
    -- Validate the OAuth state, then delete that OAuth request.
    maybeReq :: Maybe SpotifyOAuthRequest <- runSelectOne "getOAuthRequest" getOAuthRequest $ OAuth2StateParam oauth2State
    spotifyCredentials <- case maybeReq of
      Just _ -> do
        void $ runDelete "deleteOAuthRequest" deleteOAuthRequest $ OAuth2StateParam oauth2State
        requestAccessTokenFromAuthorizationCode spotifyApp authCode
      Nothing -> error "createUser: invalid OAuth request state"

    -- Exchange OAuth authorization code for credentials.
    SpotifyUser {spotifyUserID, spotifyUserName} <- liftIO $ getSpotifyUser spotifyCredentials

    -- Construct a user if one doesn't already exist.
    maybeUser <- getUserBySpotifyID spotifyUserID
    case maybeUser of
      Just user -> return user
      Nothing -> do
        friendCode <- liftIO $ toText <$> randomWord randomASCII 20
        let user =
              User
                { userID = error "createUser: impossible: userID not used during insertion",
                  friendCode,
                  spotifyUserID,
                  spotifyUserName,
                  spotifyCredentials,
                  updateJobInfo =
                    UpdateJobInfo
                      { lastUpdateJobSubmitted = Nothing,
                        lastUpdateJobCompleted = Nothing
                      }
                }
        uid <- runInsertOne "insertUser" insertUser user
        return (user {userID = fromDatabaseUserID uid} :: User)
  where
    getOAuthRequest :: OAuth2StateParam -> Select SpotifyOAuthRequestF
    getOAuthRequest (OAuth2StateParam paramState) = proc () -> do
      reqs@(SpotifyOAuthRequestT rowState) <- selectTable spotifyOAuthRequestTable -< ()
      restrict -< rowState .== sqlStrictText paramState
      returnA -< reqs
    deleteOAuthRequest :: OAuth2StateParam -> Delete Int64
    deleteOAuthRequest (OAuth2StateParam paramState) =
      Delete
        { dTable = spotifyOAuthRequestTable,
          dWhere = \SpotifyOAuthRequestT {oauth2State = rowState} -> rowState .== sqlStrictText paramState,
          dReturning = rCount
        }
    insertUser :: User -> Insert [D.UserID]
    insertUser User {..} =
      let SpotifyCredentials {..} = spotifyCredentials
          (SpotifyUserID suid) = spotifyUserID
       in Insert
            { iTable = userTable,
              iRows =
                [ UserT
                    { userID = UserIDT Nothing,
                      friendCode = sqlStrictText friendCode,
                      spotifyUserID = sqlStrictText suid,
                      spotifyUserName = sqlStrictText spotifyUserName,
                      spotifyCredentials =
                        SpotifyCredentialsT
                          { accessToken = sqlStrictText accessToken,
                            refreshToken = sqlStrictText refreshToken,
                            expiration = sqlUTCTime expiration
                          },
                      updateJobInfo =
                        UpdateJobInfoT
                          { lastUpdateJobSubmitted = null,
                            lastUpdateJobCompleted = null
                          }
                    }
                ],
              iReturning = rReturning (\UserT {userID = uid} -> uid),
              iOnConflict = Nothing
            }

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
      -- TODO: can I factor out this "equal to attribute" pattern?
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

getFollowedArtists :: (MonadApp m) => UserID -> m [Artist]
getFollowedArtists userID = do
  (artistAndAggListeners :: [(D.Artist, ArtistListenersAgg)]) <-
    runSelect "getFollowedArtists" makeSelect $ UserIDParam userID
  return $ map toArtist artistAndAggListeners
  where
    makeSelect :: UserIDParam -> Select (ArtistReadF, ArtistListenersAggF)
    -- makeSelect (UserIDParam paramUID) = aggregateListeners getFollowedArtistsAndListeners
    makeSelect (UserIDParam paramUID) =
      getArtistsBy $ proc ArtistT {artistID = rowAID} -> do
        UserT {userID = rowUID} <- selectTable userTable -< ()
        UserArtistFollowT {followUserID, followArtistID} <- selectTable userArtistFollowTable -< ()
        restrict -< rowUID .=== toDatabaseUserID paramUID
        restrict -< rowUID .=== followUserID
        restrict -< rowAID .=== followArtistID

setFollowedArtists :: (MonadApp m) => UserID -> [ArtistID] -> m ()
setFollowedArtists userID artistIDs = do
  runTransaction $ do
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

setUpdateSubmitted :: (MonadApp m) => UserID -> m ()
setUpdateSubmitted userID = do
  now <- liftIO getCurrentTime
  void $ runUpdate "userUpdateJobSubmitted" makeUpdate $ JobSubmittedParam {userID, jobSubmitted = now}
  where
    makeUpdate :: JobSubmittedParam -> Update Int64
    makeUpdate JobSubmittedParam {userID = paramUID, jobSubmitted} =
      Update
        { uTable = userTable,
          uUpdateWith = updateEasy $
            \u@UserT {updateJobInfo} ->
              (u {updateJobInfo = updateJobInfo {lastUpdateJobSubmitted = toNullable $ sqlUTCTime jobSubmitted} :: UpdateJobInfoF} :: UserReadF),
          uWhere = \UserT {userID = rowUID} -> rowUID .=== toDatabaseUserID paramUID,
          uReturning = rCount
        }

setUpdateCompleted :: (MonadApp m) => UserID -> m ()
setUpdateCompleted userID = do
  now <- liftIO getCurrentTime
  void $ runUpdate "userUpdateJobCompleted" makeUpdate $ JobCompletedParam {userID, jobCompleted = now}
  where
    makeUpdate :: JobCompletedParam -> Update Int64
    makeUpdate JobCompletedParam {userID = paramUID, jobCompleted} =
      Update
        { uTable = userTable,
          uUpdateWith = updateEasy $
            \u@UserT {updateJobInfo} ->
              -- TODO: how do I factor out this record update? Lenses?
              (u {updateJobInfo = updateJobInfo {lastUpdateJobCompleted = toNullable $ sqlUTCTime jobCompleted} :: UpdateJobInfoF} :: UserReadF),
          uWhere = \UserT {userID = rowUID} -> rowUID .=== toDatabaseUserID paramUID,
          uReturning = rCount
        }
