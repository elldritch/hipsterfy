{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hipsterfy.Server.Trace (withTracing) where

import Control.Monad.Trace.Class (Builder (..), MonadTrace (..), rootSpanWith)
import Data.Aeson (ToJSON (..))
import Monitor.Tracing (alwaysSampled)
import Monitor.Tracing.Zipkin (tag)
import Network.HTTP.Types (Status (..), StdMethod)
import Relude hiding (trace)
import Relude.Extra (bimapF, insert)
import Web.Scotty.Internal.Types (ActionT (..), srStatus)
import Web.Scotty.Trans (request, ScottyError (..), ScottyT, addroute, headers, params, raise, rescue)
import Network.Wai (Request(remoteHost))

instance (ScottyError e, MonadTrace m) => MonadTrace (ActionT e m) where
  trace name (ActionT actn) = ActionT $ trace name actn

withTracing ::
  forall e m.
  (ScottyError e, MonadIO m, MonadTrace m) =>
  StdMethod ->
  Text ->
  ActionT e m () ->
  ScottyT e m ()
withTracing method routePattern action =
  addroute method (fromString $ toString routePattern)
    $ rootSpanWith (spanKind "ROUTE") alwaysSampled spanName
    $ do
      req <- request
      tag "http.requestIP" $ show $ remoteHost req
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
