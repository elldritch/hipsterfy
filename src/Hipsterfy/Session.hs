{-# LANGUAGE Arrows #-}

module Hipsterfy.Session
  ( getSessionByCookieSecret,
    createSession,
    deleteSession,
  )
where

import Control.Arrow (returnA)
import Hipsterfy.Application (MonadApp)
import Hipsterfy.Database (QueryParameters, runDelete, runInsert, runSelectOne)
import Hipsterfy.Database.User (UserReadF, UserSessionT (..), UserT (..), userSessionTable, userTable)
import Hipsterfy.User (User (..), UserID, toDatabaseUserID, toUser)
import Opaleye.Manipulation (Delete (..), Insert (..), rCount)
import Opaleye.Operators ((.==), (.===), restrict)
import Opaleye.Select (Select)
import Opaleye.SqlTypes (sqlStrictText)
import Opaleye.Table (selectTable)
import Relude

-- Named parameter sets for tracing.

newtype CookieSecretParam = CookieSecretParam {cookieSecret :: Text} deriving (Generic)

instance QueryParameters CookieSecretParam

data UserCookieParam = UserCookieParam
  { cookieSecret :: Text,
    userID :: UserID
  }
  deriving (Generic)

instance QueryParameters UserCookieParam

-- Operations.

getSessionByCookieSecret :: (MonadApp m) => Text -> m (Maybe User)
getSessionByCookieSecret cookieSecret = do
  maybeUser <- runSelectOne "getSessionByCookieSecret" makeSelect $ CookieSecretParam cookieSecret
  return $ toUser <$> maybeUser
  where
    makeSelect :: CookieSecretParam -> Select UserReadF
    makeSelect CookieSecretParam {cookieSecret = paramCS} = proc () -> do
      UserSessionT {cookieSecret = rowCS, sessionUserID = cookieUID} <- selectTable userSessionTable -< ()
      users@UserT {userID = userUID} <- selectTable userTable -< ()
      restrict -< rowCS .== sqlStrictText paramCS
      restrict -< userUID .=== cookieUID
      returnA -< users

createSession :: (MonadApp m) => User -> Text -> m ()
createSession User {userID} cookieSecret =
  void $ runInsert "createSession" makeInsert $ UserCookieParam {userID, cookieSecret}
  where
    makeInsert :: UserCookieParam -> Insert Int64
    makeInsert UserCookieParam {userID = uid, cookieSecret = secret} =
      Insert
        { iTable = userSessionTable,
          iRows =
            [ UserSessionT
                { sessionUserID = toDatabaseUserID uid,
                  cookieSecret = sqlStrictText secret
                }
            ],
          iReturning = rCount,
          iOnConflict = Nothing
        }

deleteSession :: (MonadApp m) => Text -> m ()
deleteSession cookieSecret =
  void $ runDelete "deleteSession" makeDelete $ CookieSecretParam cookieSecret
  where
    makeDelete :: CookieSecretParam -> Delete Int64
    makeDelete CookieSecretParam {cookieSecret = paramCS} =
      Delete
        { dTable = userSessionTable,
          dWhere = \UserSessionT {cookieSecret = rowCS} -> rowCS .== sqlStrictText paramCS,
          dReturning = rCount
        }
