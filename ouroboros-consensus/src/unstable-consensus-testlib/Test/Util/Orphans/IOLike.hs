{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Util.Orphans.IOLike () where

import           Control.Monad.IOSim
import qualified Control.Concurrent.Class.MonadSTM.Strict.TVar as X
import qualified Control.Concurrent.Class.MonadSTM.Strict.TVar.Checked as Y
import           NoThunks.Class
import           Ouroboros.Consensus.Util.IOLike
import           Test.Util.Orphans.NoThunks ()

-- TODO KLUDGE
instance NoThunks (X.StrictTVar m a) => NoThunks (Y.StrictTVar m a) where
  showTypeOf _ = "Checked.StrictTVar"

  wNoThunks ctxt = wNoThunks ("tvar":ctxt) . Y.unsafeToUncheckedStrictTVar

instance IOLike (IOSim s) where
  forgetSignKeyKES = const $ return ()
