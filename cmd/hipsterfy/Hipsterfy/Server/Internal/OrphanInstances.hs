{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hipsterfy.Server.Internal.OrphanInstances () where

import Control.Monad.Trace.Class (MonadTrace (..))
import Relude hiding (trace)
import Web.Scotty.Internal.Types (ActionT (..))
import Web.Scotty.Trans (ScottyError)

instance (ScottyError e, MonadTrace m) => MonadTrace (ActionT e m) where
  trace name (ActionT actn) = ActionT $ trace name actn
