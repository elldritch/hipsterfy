module Hipsterfy.Server.Session (getSession, startSession, endSession) where

import Data.Time (secondsToDiffTime)
import Hipsterfy.Application (MonadApp)
import Hipsterfy.Server.Handlers (tagUser)
import Hipsterfy.Session (createSession, deleteSession, getSessionByCookieSecret)
import Hipsterfy.User (User (..))
import Relude
import Test.RandomStrings (randomASCII, randomWord)
import Web.Cookie (SetCookie (..), defaultSetCookie)
import Web.Scotty.Cookie (deleteCookie, getCookie, setCookie)
import Web.Scotty.Trans (ActionT, ScottyError)

hipsterfyCookieName :: Text
hipsterfyCookieName = "hipsterfy_user"

getSession :: (ScottyError e, MonadApp m) => ActionT e m (Maybe User)
getSession = do
  cookie <- getCookie hipsterfyCookieName
  maybeUser <- case cookie of
    Nothing -> return Nothing
    Just c -> lift $ getSessionByCookieSecret c
  case maybeUser of
    Nothing -> return Nothing
    Just user -> do
      tagUser user
      return $ Just user

startSession :: (ScottyError e, MonadApp m) => User -> ActionT e m ()
startSession user = do
  cookieSecret <- liftIO $ randomWord randomASCII 20
  createSession user $ toText cookieSecret
  setCookie
    ( defaultSetCookie
        { setCookieName = encodeUtf8 hipsterfyCookieName,
          setCookieValue = encodeUtf8 cookieSecret,
          setCookiePath = Just "/",
          setCookieMaxAge = Just $ secondsToDiffTime $ 60 * 60 * 24 * 365 * 2
        }
    )

endSession :: (ScottyError e, MonadApp m) => ActionT e m ()
endSession = do
  cookie <- getCookie hipsterfyCookieName
  case cookie of
    Nothing -> pass
    Just cookieSecret -> do
      deleteSession cookieSecret
      deleteCookie hipsterfyCookieName
