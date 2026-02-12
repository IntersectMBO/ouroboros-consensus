{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Creating a mempool
module Ouroboros.Consensus.Mempool.Init
  ( openMempool
  , openMempoolWithoutSyncThread
  ) where

import Control.Monad (void)
import Control.Monad.Class.MonadTimer.SI (MonadTimer)
import Control.ResourceRegistry
import Control.Tracer
import Data.Functor.Identity (runIdentity)
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Mempool.API (Mempool (..), MempoolTimeoutConfig)
import Ouroboros.Consensus.Mempool.Capacity
import Ouroboros.Consensus.Mempool.Impl.Common
import Ouroboros.Consensus.Mempool.Query
import Ouroboros.Consensus.Mempool.Update
import Ouroboros.Consensus.Storage.LedgerDB.Forker
import Ouroboros.Consensus.Util (whenJust)
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM

{-------------------------------------------------------------------------------
  Opening the mempool
-------------------------------------------------------------------------------}

-- | Create a @Mempool m blk@ in @m@ to manipulate the mempool. It will also
-- fork a thread that syncs the mempool and the ledger when the ledger changes.
openMempool ::
  ( IOLike m
  , MonadTimer m
  , LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  , ValidateEnvelope blk
  ) =>
  ResourceRegistry m ->
  LedgerInterface m blk ->
  LedgerConfig blk ->
  MempoolCapacityBytesOverride ->
  Maybe MempoolTimeoutConfig ->
  Tracer m (TraceEventMempool blk) ->
  m (Mempool m blk)
openMempool topLevelRegistry ledger cfg capacityOverride timeoutConfig tracer = do
  env <- initMempoolEnv ledger cfg capacityOverride timeoutConfig tracer
  forkSyncStateOnTipPointChange topLevelRegistry env
  return $ mkMempool topLevelRegistry env

-- | Spawn a thread which syncs the 'Mempool' state whenever the 'LedgerState'
-- changes.
forkSyncStateOnTipPointChange ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  , ValidateEnvelope blk
  ) =>
  ResourceRegistry m ->
  MempoolEnv m blk ->
  m ()
forkSyncStateOnTipPointChange reg menv =
  void $
    forkLinkedWatcher
      reg
      "Mempool.syncStateOnTipPointChange"
      Watcher
        { wFingerprint = id
        , wInitial = Nothing
        , wNotify = action
        , wReader = getCurrentTip
        }
 where
  action :: MempoolLedgerDBView m blk -> m ()
  action _a =
    void $ implSyncWithLedger menv

  -- Using the tip ('Point') allows for quicker equality checks
  getCurrentTip :: STM m (MempoolLedgerDBView m blk)
  getCurrentTip = getCurrentLedgerState (mpEnvLedger menv)

-- | Unlike 'openMempool', this function does not fork a background thread
-- that synchronises with the ledger state whenever the later changes.
--
-- Intended for testing purposes.
openMempoolWithoutSyncThread ::
  ( IOLike m
  , MonadTimer m
  , LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  , ValidateEnvelope blk
  ) =>
  ResourceRegistry m ->
  LedgerInterface m blk ->
  LedgerConfig blk ->
  MempoolCapacityBytesOverride ->
  Maybe MempoolTimeoutConfig ->
  Tracer m (TraceEventMempool blk) ->
  m (Mempool m blk)
openMempoolWithoutSyncThread registry ledger cfg capacityOverride timeoutConfig tracer =
  mkMempool registry <$> initMempoolEnv ledger cfg capacityOverride timeoutConfig tracer

mkMempool ::
  ( IOLike m
  , MonadTimer m
  , LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  , ValidateEnvelope blk
  ) =>
  ResourceRegistry m -> MempoolEnv m blk -> Mempool m blk
mkMempool registry mpEnv =
  Mempool
    { addTx = fmap runIdentity .: implAddTx mpEnv ProductionAddTx
    , removeTxsEvenIfValid = implRemoveTxsEvenIfValid mpEnv
    , getSnapshot = snapshotFromIS <$> readTMVar istate
    , getSnapshotFor = implGetSnapshotFor mpEnv
    , getCapacity = isCapacity <$> readTMVar istate
    , testSyncWithLedger = implSyncWithLedger mpEnv
    , testForkMempoolThread = forkLinkedThread registry
    , testTryAddTx = implAddTx mpEnv . TestingAddTx
    , closeMempool = do
        mFrk <- tryTakeMVar (mpEnvForker mpEnv)
        whenJust mFrk forkerRelease
    }
 where
  MempoolEnv
    { mpEnvStateVar = istate
    } = mpEnv
