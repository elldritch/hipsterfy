module Hipsterfy.Server.Handlers
  ( get,
    post,
    tagUser,
  )
where

import Control.Monad.Trace.Class (MonadTrace, rootSpanWith)
import Hipsterfy.Application (MonadApp)
import Hipsterfy.Server.Internal.OrphanInstances ()
import Hipsterfy.Trace (spanKind, tagPairs)
import Hipsterfy.User (User (..))
import Monitor.Tracing (alwaysSampled)
import Monitor.Tracing.Zipkin (tag)
import Network.HTTP.Types (Status (..), StdMethod (..))
import Network.Wai (Request (..))
import Relude hiding (get, trace)
import qualified Relude as R (get)
import Relude.Extra (bimapF)
import Web.Scotty.Internal.Types (ActionT (..), srStatus)
import Web.Scotty.Trans (ScottyError (..), ScottyT, addroute, headers, params, raise, request, rescue)

get :: (ScottyError e, MonadApp m) => Text -> ActionT e m () -> ScottyT e m ()
get = handleRoute GET

post :: (ScottyError e, MonadApp m) => Text -> ActionT e m () -> ScottyT e m ()
post = handleRoute POST

handleRoute ::
  forall e m.
  (ScottyError e, MonadApp m) =>
  StdMethod ->
  Text ->
  ActionT e m () ->
  ScottyT e m ()
handleRoute method routePattern action =
  addroute method (fromString $ toString routePattern)
    $ rootSpanWith (spanKind "ROUTE") alwaysSampled spanName
    $ do
      req <- request
      -- TODO: can I trace the entire ScottyT stack at the WAI level instead?
      tag "http.requestIP" $ show $ remoteHost req
      tag "http.path" $ decodeUtf8 $ rawPathInfo req
      tag "http.route" routePattern
      tag "http.verb" (show method)
      hs <- headers
      lift $ tagPairs "http.headers." $ bimapF toText toText hs
      ps <- params
      lift $ tagPairs "http.params." $ bimapF toText toText ps
      tag "service.name" "hipsterfy-server"
      -- TODO: how do we make this work for exceptions in general? I can't fit `catch` into here
      action `rescue` tagError
      status <- getStatus
      tag "http.statusCode" $ show $ statusCode status
  where
    spanName = show method <> " " <> routePattern
    tagError :: e -> ActionT e m ()
    tagError err = do
      tag "error" $ toText $ showError err
      tag "http.statusCode" $ show (500 :: Int)
      raise err

getStatus :: (Monad m) => ActionT e m Status
getStatus = ActionT $ srStatus <$> R.get

tagUser :: (MonadTrace m) => User -> m ()
tagUser User {userID} = tag "user" $ show userID
