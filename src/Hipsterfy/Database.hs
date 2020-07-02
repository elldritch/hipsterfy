module Hipsterfy.Database
  ( runSelect,
    runSelectOne,
    runUpdate,
    runInsert,
    runInsertOne,
    runDelete,
    QueryParameters (..),
    runTransaction,
  )
where

import Control.Monad.Trace.Class (childSpanWith)
import Data.Profunctor.Product.Default (Default)
import Database.PostgreSQL.Simple (Connection, withTransaction)
import GHC.Generics ((:*:) (..), C, D, Generic (..), K1 (..), M1 (..), S, Selector, selName)
import Hipsterfy.Application (AppT, Config (..), MonadApp, runApp)
import Hipsterfy.Spotify.Auth (SpotifyCredentials)
import Hipsterfy.Trace (spanKind, tagPairs)
import Monitor.Tracing.Zipkin (tag)
import Opaleye (FromFields)
import Opaleye.Manipulation (Delete, Insert, Update)
import qualified Opaleye.Manipulation as O (runDelete_, runInsert_, runUpdate_)
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

instance QueryParameters SpotifyCredentials

runDB :: (MonadApp m, QueryParameters p) => Text -> Text -> (Connection -> q t -> IO r) -> (p -> q t) -> p -> m r
runDB name queryType runner queryBuilder params = do
  Config {postgres} <- ask
  childSpanWith
    (spanKind "QUERY")
    ("QUERY " <> name)
    $ do
      -- TODO: tag the specific PostgreSQL pod this query is talking to?
      tag "service.name" "postgresql"
      tag "db.query" name
      tag "db.query.type" queryType
      tagPairs "db.params." $ fields params
      liftIO $ runner postgres $ queryBuilder params

renderDebugParams :: (QueryParameters p) => p -> Text
renderDebugParams = foldr (\(k, v) s -> s <> " " <> k <> ": " <> v) "" . fields

runSelect :: (Default FromFields fs hs, MonadApp m, QueryParameters p) => Text -> (p -> Select fs) -> p -> m [hs]
runSelect name = runDB name "SELECT" O.runSelect

runSelectOne :: (Default FromFields fs hs, MonadApp m, QueryParameters p) => Text -> (p -> Select fs) -> p -> m (Maybe hs)
runSelectOne name makeSelect params = do
  rows <- runSelect name makeSelect params
  return $ case rows of
    [row] -> Just row
    [] -> Nothing
    _ -> error $ "runSelectOne: " <> name <> ": more than one row returned with params " <> renderDebugParams params

runUpdate :: (MonadApp m, QueryParameters p) => Text -> (p -> Update hs) -> p -> m hs
runUpdate name = runDB name "UPDATE" O.runUpdate_

runInsert :: (MonadApp m, QueryParameters p) => Text -> (p -> Insert hs) -> p -> m hs
runInsert name = runDB name "INSERT" O.runInsert_

runInsertOne :: (MonadApp m, QueryParameters p) => Text -> (p -> Insert [hs]) -> p -> m hs
runInsertOne name makeInsert params = do
  rows <- runDB name "INSERT" O.runInsert_ makeInsert params
  return $ case rows of
    [row] -> row
    [] -> error $ "runInsertOne: " <> name <> ": zero rows returned with params " <> renderDebugParams params
    _ -> error $ "runInsertOne: " <> name <> ": more than one row returned with params " <> renderDebugParams params

runDelete :: (MonadApp m, QueryParameters p) => Text -> (p -> Delete hs) -> p -> m hs
runDelete name = runDB name "DELETE" O.runDelete_

runTransaction :: (MonadApp m) => AppT IO a -> m a
runTransaction action = do
  config@Config {postgres} <- ask
  liftIO $ withTransaction postgres $ do
    runApp config action
