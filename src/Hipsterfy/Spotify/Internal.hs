module Hipsterfy.Spotify.Internal (requestAsJSON) where

import Control.Lens ((^.))
import Data.Aeson (FromJSON, eitherDecode)
import Network.Wreq (Response, responseBody)
import Relude

requestAsJSON :: (MonadIO m, FromJSON t) => IO (Response LByteString) -> m t
requestAsJSON req = do
  res <- liftIO req
  let body = res ^. responseBody
  case eitherDecode body of
    Right v -> return v
    Left e -> do
      liftIO $ print res
      -- liftIO $ print body
      error $ "JSONError: " <> toText e
