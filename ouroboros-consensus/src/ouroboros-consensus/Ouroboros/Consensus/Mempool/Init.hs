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
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import Ouroboros.Consensus.Mempool.Update
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM
import Ouroboros.Network.Block (Point)

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
  forkSyncStateOnTipPointChange env topLevelRegistry
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
  ResourceRegistry m ->
  m ()
forkSyncStateOnTipPointChange menv reg =
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
  action :: Point blk -> m ()
  action _a = implSyncWithLedger (const ()) menv

  -- Using the tip ('Point') allows for quicker equality checks
  getCurrentTip :: STM m (Point blk)
  getCurrentTip =
    ledgerTipPoint . mldViewState <$> getCurrentLedgerState (mpEnvLedger menv)

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
  LedgerInterface m blk ->
  LedgerConfig blk ->
  MempoolCapacityBytesOverride ->
  Maybe MempoolTimeoutConfig ->
  Tracer m (TraceEventMempool blk) ->
  m (Mempool m blk)
openMempoolWithoutSyncThread ledger cfg capacityOverride timeoutConfig tracer =
  mkMempool <$> initMempoolEnv ledger cfg capacityOverride timeoutConfig tracer

mkMempool ::
  ( IOLike m
  , MonadTimer m
  , LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  , ValidateEnvelope blk
  ) =>
  MempoolEnv m blk -> Mempool m blk
mkMempool mpEnv =
  Mempool
    { addTx = fmap runIdentity .: implAddTx mpEnv ProductionAddTx
    , removeTxsEvenIfValid = implRemoveTxsEvenIfValid mpEnv
    , getSnapshot = snapshotFromIS <$> readTMVar istate
    , getSnapshotFor = implGetSnapshotFor mpEnv
    , getCapacity = isCapacity <$> readTMVar istate
    , testSyncWithLedger = implSyncWithLedger snapshotFromIS mpEnv
    , testTryAddTx = implAddTx mpEnv . TestingAddTx
    }
 where
  snapshotFromIS is =
    snapshotFromValidTxs
      [ TxSeq.TxTicket tx tn tz
      | TxSeq.TxTicket (ValidatedTxWithDiffs tx _) tn tz <- TxSeq.toList $ isTxs is
      ]
      (isTip is)
      (isSlotNo is)

  MempoolEnv
    { mpEnvStateVar = istate
    } = mpEnv
