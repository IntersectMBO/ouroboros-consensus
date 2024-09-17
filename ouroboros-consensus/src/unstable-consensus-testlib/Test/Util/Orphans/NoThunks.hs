{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Orphans.NoThunks () where

import           Control.Concurrent.Class.MonadMVar
import           Control.Concurrent.Class.MonadSTM.Strict.TVar.Checked
import           Control.Monad.IOSim
import           Control.Monad.ST.Lazy
import           Control.Monad.ST.Unsafe (unsafeSTToIO)
import           Data.Proxy
import           NoThunks.Class (NoThunks (..))
import           Ouroboros.Consensus.Util.MonadSTM.NormalForm
import           Ouroboros.Consensus.Util.NormalForm.StrictMVar
import           System.FS.API.Types
import           System.FS.Sim.FsTree
import           System.FS.Sim.MockFS

instance NoThunks a => NoThunks (StrictSVar (IOSim s) a) where
  showTypeOf _ = "StrictSVar IOSim"
  wNoThunks ctxt StrictSVar { tvar } = do
      a <- unsafeSTToIO $ lazyToStrictST $ inspectTVar (Proxy :: Proxy (IOSim s)) tvar
      noThunks ctxt a

instance NoThunks a => NoThunks (StrictMVar (IOSim s) a) where
  showTypeOf _ = "StrictMVar IOSim"
  wNoThunks ctxt mvar = do
      aMay <- unsafeSTToIO $ lazyToStrictST $ inspectMVar (Proxy :: Proxy (IOSim s)) (toLazyMVar mvar)
      noThunks ctxt aMay

instance NoThunks a => NoThunks (StrictTVar (IOSim s) a) where
  showTypeOf _ = "StrictTVar IOSim"
  wNoThunks ctxt tvar = do
      a <- unsafeSTToIO $ lazyToStrictST $ inspectTVar (Proxy :: Proxy (IOSim s)) $ toLazyTVar tvar
      noThunks ctxt a

{-------------------------------------------------------------------------------
  fs-sim
-------------------------------------------------------------------------------}

deriving instance NoThunks FsPath
deriving instance NoThunks MockFS
deriving instance NoThunks a => NoThunks (FsTree a)
deriving instance NoThunks HandleMock
deriving instance NoThunks HandleState
deriving instance NoThunks OpenHandleState
deriving instance NoThunks ClosedHandleState
deriving instance NoThunks FilePtr
