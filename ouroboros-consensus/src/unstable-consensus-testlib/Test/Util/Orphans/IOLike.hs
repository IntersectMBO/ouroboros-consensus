{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Orphans.IOLike () where

import Control.Monad.IOSim
import LeiosUtils.CallTrace (MonadAllocationCounter (..))
import Ouroboros.Consensus.Util.IOLike
import Test.Util.Orphans.NoThunks ()

instance IOLike (IOSim s) where
  forgetSignKeyKES = const $ return ()

-- TODO(bladyjoker): Is there a meaningful allocation counter in IOSim?
instance MonadAllocationCounter (IOSim s) where
  getAllocationCounter = pure 0
