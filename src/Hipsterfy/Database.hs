module Hipsterfy.Database (runSelect, QueryParameters (..)) where

import Control.Monad.Trace.Class (childSpanWith)
import Data.Profunctor.Product.Default (Default)
import GHC.Generics ((:*:) (..), C, D, Generic (..), K1 (..), M1 (..), S, Selector, selName)
import Hipsterfy.Application (Config (..), MonadApp)
import Hipsterfy.Trace (spanKind, tagPairs)
import Monitor.Tracing.Zipkin (tag)
import Opaleye (FromFields)
import qualified Opaleye.RunSelect as O (runSelect)
import Opaleye.Select (Select)
import Relude hiding (optional)

class QueryParameters p where
  fields :: p -> [(Text, Text)]
  default fields :: (Generic p, GQueryParameters (Rep p)) => p -> [(Text, Text)]
  fields = gfields . from

class GQueryParameters f where
  gfields :: f a -> [(Text, Text)]

instance (GQueryParameters a, GQueryParameters b) => GQueryParameters (a :*: b) where
  gfields (a :*: b) = gfields a <> gfields b

instance (Selector s, Show p) => GQueryParameters (M1 S s (K1 i p)) where
  gfields meta@(M1 (K1 v)) = [(toText $ selName meta, show v)]

instance (GQueryParameters a) => GQueryParameters (M1 D c a) where
  gfields (M1 x) = gfields x

instance (GQueryParameters a) => GQueryParameters (M1 C c a) where
  gfields (M1 x) = gfields x

runSelect :: (Default FromFields fs hs, MonadApp m, QueryParameters p) => Text -> (p -> Select fs) -> p -> m [hs]
runSelect name makeQuery params = do
  Config {postgres} <- ask
  childSpanWith
    (spanKind "QUERY")
    ("QUERY " <> name)
    $ do
      -- TODO: tag the specific PostgreSQL pod this query is talking to?
      tag "service.name" "postgresql"
      tag "db.query" name
      tagPairs "db.params." $ fields params
      liftIO $ O.runSelect postgres $ makeQuery params
