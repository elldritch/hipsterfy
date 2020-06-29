module Hipsterfy.Server.Handlers (handleRoute, tagUser) where

import Control.Monad.Trace.Class (Builder (..), MonadTrace (..), rootSpanWith)
import Data.Aeson (ToJSON (..))
import Hipsterfy.Server.Internal.OrphanInstances ()
import Hipsterfy.User (User (..))
import Monitor.Tracing (alwaysSampled)
import Monitor.Tracing.Zipkin (tag)
import Network.HTTP.Types (Status (..), StdMethod)
import Network.Wai (Request (..))
import Relude hiding (trace)
import Relude.Extra (bimapF, insert)
import Web.Scotty.Internal.Types (ActionT (..), srStatus)
import Web.Scotty.Trans (ScottyError (..), ScottyT, addroute, headers, params, raise, request, rescue)

handleRoute ::
  forall e m.
  (ScottyError e, MonadIO m, MonadTrace m) =>
  StdMethod ->
  Text ->
  ActionT e m () ->
  ScottyT e m ()
handleRoute method routePattern action =
  addroute method (fromString $ toString routePattern)
    -- TODO: can we should trace the entire ScottyT? How else do we pick up e.g. 404s?
    $ rootSpanWith (spanKind "ROUTE") alwaysSampled spanName
    $ do
      req <- request
      tag "http.requestIP" $ show $ remoteHost req
      tag "http.path" $ decodeUtf8 $ rawPathInfo req
      tag "http.route" routePattern
      tag "http.verb" (show method)
      hs <- headers
      lift $ tagPairs "http.headers." $ bimapF toText toText hs
      ps <- params
      lift $ tagPairs "http.params." $ bimapF toText toText ps
      tag "service.name" "hipsterfy-server"
      action `rescue` tagError
      status <- getStatus
      tag "http.statusCode" $ show $ statusCode status
  where
    spanKind :: Text -> Builder -> Builder
    spanKind kind = \b -> b {builderTags = insert "z.k" (toJSON kind) (builderTags b)}
    spanName = show method <> " " <> routePattern
    tagError :: e -> ActionT e m ()
    tagError err = do
      tag "error" $ toText $ showError err
      tag "http.statusCode" $ show (500 :: Int)
      raise err
    tagPairs :: Text -> [(Text, Text)] -> m ()
    tagPairs prefix = mapM_ (\(k, v) -> tag (prefix <> k) v)

getStatus :: (Monad m) => ActionT e m Status
getStatus = ActionT $ srStatus <$> get

tagUser :: (MonadTrace m) => User -> m ()
tagUser User {userID} = tag "user" $ show userID
