{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Orphans.IOLike () where

import           Control.Monad.Base
import           Control.Monad.IOSim
import           Ouroboros.Consensus.Util.IOLike
import           Test.Util.Orphans.NoThunks ()

instance IOLike (IOSim s) where
  forgetSignKeyKES = const $ return ()

instance MonadBase (IOSim s) (IOSim s) where liftBase = id
