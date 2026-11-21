{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Queries to the mempool
module Ouroboros.Consensus.Mempool.Query (implGetSnapshotFor) where

import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.Tables.Utils (restrictValues')
import Ouroboros.Consensus.Mempool.API
import Ouroboros.Consensus.Mempool.Impl.Common
import Ouroboros.Consensus.Mempool.Reapply
  ( ReapplyState (ReapplyState, appliedTxs)
  , reapplyUntilTimeout
  )
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
  (LedgerTables blk KeysMK -> m (LedgerTables blk ValuesMK)) ->
  m (MempoolSnapshot blk)
implGetSnapshotFor MempoolEnv{..} slot ticked readUntickedTables = do
  is@IS{..} <- atomically $ readTMVar mpEnvStateVar
  if pointHash isTip == castHash (getTipHash ticked)
    && isSlotNo == slot
    then
      -- We are looking for a snapshot exactly for the ledger state we already
      -- have cached, then just return it.
      pure $ snapshotFromIS is
    else do
      let canUseCache = pointHash isTip == castHash (getTipHash ticked)
      let resolveValues =
            if canUseCache
              -- We are looking for a snapshot at the same state ticked
              -- to a different slot, so we can reuse the cached values
              then pure . restrictValues' isTxValues
              -- We are looking for a snapshot at a different state, so we
              -- need to read the values from the ledgerdb.
              else readUntickedTables
      computeSnapshotUntilTimeout
        resolveValues
        snapshotStepTimeLimit
        mpEnvLedgerCfg
        slot
        ticked
        isTxs
 where
  snapshotStepTimeLimit = case mpEnvTimeoutConfig of
    Nothing -> snapshotStepTimeLimitDefault
    Just mempoolTimeoutCfg -> mempoolTimeoutCapacity mempoolTimeoutCfg / 10

snapshotStepTxsPerStep :: Int
snapshotStepTxsPerStep = 100

snapshotStepTimeLimitDefault :: DiffTime
snapshotStepTimeLimitDefault = 0.1

computeSnapshotUntilTimeout ::
  forall blk m.
  (IOLike m, LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  (LedgerTables blk KeysMK -> m (LedgerTables blk ValuesMK)) ->
  DiffTime ->
  LedgerConfig blk ->
  SlotNo ->
  TickedLedgerState blk DiffMK ->
  TxSeq.TxSeq (MempoolMeasure blk) (ValidatedTxWithDiffs blk) ->
  m (MempoolSnapshot blk)
computeSnapshotUntilTimeout resolveValues snapshotStepTimeLimit cfg slot baseLedgerStDiff txsToApply = do
  ReapplyState{..} <-
    reapplyUntilTimeout
      snapshotStepTimeLimit
      snapshotStepTxsPerStep
      resolveValues
      cfg
      slot
      baseLedgerStDiff
      txsToApply

  let tip = castPoint $ getTip baseLedgerStDiff
  return $! snapshotFromTxSeq appliedTxs tip slot
