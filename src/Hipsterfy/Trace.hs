module Hipsterfy.Trace (spanKind, tagPairs) where

import Control.Monad.Trace.Class (Builder (..), MonadTrace)
import Data.Aeson (ToJSON (toJSON))
import Data.Map (insert)
import Monitor.Tracing.Zipkin (tag)
import Relude

spanKind :: Text -> Builder -> Builder
spanKind kind b = b {builderTags = insert "z.k" (toJSON kind) (builderTags b)}

tagPairs :: (MonadTrace m) => Text -> [(Text, Text)] -> m ()
tagPairs prefix = mapM_ (\(k, v) -> tag (prefix <> k) v)
