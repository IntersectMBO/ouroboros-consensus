{-# LANGUAGE FlexibleContexts #-}

-- | Queries to the mempool
module Ouroboros.Consensus.Mempool.Query (implGetSnapshotFor) where

import Control.Tracer (traceWith)
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Mempool.API
import Ouroboros.Consensus.Mempool.Impl.Common
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import Ouroboros.Consensus.Util.IOLike

implGetSnapshotFor ::
  ( IOLike m
  , LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  ) =>
  MempoolEnv m blk ->
  -- | Get snapshot for this slot number (usually the current slot)
  SlotNo ->
  -- | The ledger state at which we want the
  -- snapshot, ticked to @slot@.
  TickedLedgerState blk DiffMK ->
  -- | A function that returns values corresponding to the given keys for
  -- the unticked ledger state.
  (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  TxMeasure blk ->
  m (MempoolSnapshot blk)
implGetSnapshotFor mpEnv slot ticked readUntickedTables cap = do
  is <- atomically $ readTMVar istate
  if pointHash (isTip is) == castHash (getTipHash ticked)
    -- && isSlotNo is == slot -- FIXME(bladyjoker): Accepting producing bad blocks in order to speed this up (or enforce a min tx interval in the mempool)
    then do
      traceWith (mpEnvTracer mpEnv) $ TraceMempoolCacheHit (isTip is)
      -- We are looking for a snapshot xeactly for the ledger state we already
      -- have cached, then just return it.
      pure $ snapshotFromIS is -- WARN(bladyjoker): This is ignoring `cap`. I would rather we ditch the Snapshot idea and work with TxSeq or isTxs.
    else do
      traceWith (mpEnvTracer mpEnv) $ TraceMempoolCacheMiss (isTip is)
      let (capTxs, _) =
            TxSeq.splitAfterTxSize
              (isTxs is)
              (MkTxMeasureWithDiffTime cap InfiniteDiffTimeMeasure)
      let capKeys =
            foldMap
              (getTransactionKeySets . txForgetValidated . validatedTx . TxSeq.txTicketTx)
              (TxSeq.toList capTxs)
      values <- readUntickedTables capKeys -- FIXME(bladyjoker): This is not considering that we might have read a lot of UTxOs in the Mempool
      pure $
        computeSnapshot
          capacityOverride
          cfg
          slot
          ticked
          values
          (isLastTicketNo is)
          (TxSeq.toList capTxs)
 where
  MempoolEnv
    { mpEnvStateVar = istate
    , mpEnvLedgerCfg = cfg
    , mpEnvCapacityOverride = capacityOverride
    } = mpEnv
