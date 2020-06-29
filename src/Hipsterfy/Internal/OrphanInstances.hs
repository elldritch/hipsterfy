{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hipsterfy.Internal.OrphanInstances () where

import Control.Monad.Parallel (MonadParallel (..))
import Control.Monad.Trace (TraceT)
import Relude

instance (Monad m) => MonadParallel (TraceT m) where
  bindM2 f ma mb = do
    a <- ma
    b <- mb
    f a b
