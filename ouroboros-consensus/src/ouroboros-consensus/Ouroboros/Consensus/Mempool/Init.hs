{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Creating a mempool
module Ouroboros.Consensus.Mempool.Init
  ( openMempool
  , openMempoolWithoutSyncThread
  ) where

import Control.Monad (void)
import Control.ResourceRegistry
import Control.Tracer
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Mempool.API (Mempool (..))
import Ouroboros.Consensus.Mempool.Capacity
import Ouroboros.Consensus.Mempool.Impl.Common
import Ouroboros.Consensus.Mempool.Query
import Ouroboros.Consensus.Mempool.Update
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM

{-------------------------------------------------------------------------------
  Opening the mempool
-------------------------------------------------------------------------------}

-- | Create a @Mempool m blk@ in @m@ to manipulate the mempool. It will also
-- fork a thread that syncs the mempool and the ledger when the ledger changes.
openMempool ::
  ( IOLike m
  , LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  , HasTxHash (GenTx blk)
  , ValidateEnvelope blk
  ) =>
  ResourceRegistry m ->
  LedgerInterface m blk ->
  LedgerConfig blk ->
  MempoolCapacityBytesOverride ->
  Tracer m (TraceEventMempool blk) ->
  m (Mempool m blk)
openMempool topLevelRegistry ledger cfg capacityOverride tracer = do
  env <- initMempoolEnv ledger cfg capacityOverride tracer topLevelRegistry
  forkSyncStateOnTipPointChange env
  return $ mkMempool env

-- | Spawn a thread which syncs the 'Mempool' state whenever the 'LedgerState'
-- changes.
forkSyncStateOnTipPointChange ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  , ValidateEnvelope blk
  ) =>
  MempoolEnv m blk ->
  m ()
forkSyncStateOnTipPointChange menv =
  void $
    forkLinkedWatcher
      (mpEnvRegistry menv)
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
  getCurrentTip =
    getCurrentLedgerState (mpEnvLedger menv) (mpEnvRegistry menv)

-- | Unlike 'openMempool', this function does not fork a background thread
-- that synchronises with the ledger state whenever the later changes.
--
-- Intended for testing purposes.
openMempoolWithoutSyncThread ::
  ( IOLike m
  , LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  , HasTxHash (GenTx blk)
  , ValidateEnvelope blk
  ) =>
  ResourceRegistry m ->
  LedgerInterface m blk ->
  LedgerConfig blk ->
  MempoolCapacityBytesOverride ->
  Tracer m (TraceEventMempool blk) ->
  m (Mempool m blk)
openMempoolWithoutSyncThread registry ledger cfg capacityOverride tracer =
  mkMempool <$> initMempoolEnv ledger cfg capacityOverride tracer registry

mkMempool ::
  ( IOLike m
  , LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  , HasTxHash (GenTx blk)
  , ValidateEnvelope blk
  ) =>
  MempoolEnv m blk -> Mempool m blk
mkMempool mpEnv =
  Mempool
    { addTx = implAddTx mpEnv
    , removeTxsEvenIfValid = implRemoveTxsEvenIfValid mpEnv
    , getSnapshot = snapshotFromIS <$> readTMVar istate
    , getSnapshotFor = implGetSnapshotFor mpEnv
    , getCapacity = isCapacity <$> readTMVar istate
    , testSyncWithLedger = implSyncWithLedger mpEnv
    , testForkMempoolThread = forkLinkedThread (mpEnvRegistry mpEnv)
    }
 where
  MempoolEnv
    { mpEnvStateVar = istate
    } = mpEnv
