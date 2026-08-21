{-# LANGUAGE FlexibleContexts #-}

-- | Queries to the mempool
module Ouroboros.Consensus.Mempool.Query
  ( implGetSnapshotFor
  , implGetSnapshotForNoCache
  ) where

import Control.Tracer (traceWith)
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Mempool.API
import Ouroboros.Consensus.Mempool.Impl.Common
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import Ouroboros.Consensus.Util.IOLike

-- | Whether the mempool snapshot may be served from the cached internal or needs re-computation.
--
-- 'UseCache' is the normal case:

-- — a state rebased onto a to-be-certified EB's closure so a freshly-announced
-- EB only carries txs valid /after/ that EB is applied
-- (input-output-hk/ouroboros-leios#838). There the cache must be bypassed, or
-- we would return the stale pre-rebase snapshot and silently drop the rebase.
data SnapshotCachePolicy
  = -- | same tip hash => same ledger state, so the cached snapshot is reusable.
    UseCache
  | -- | for the Leios EB certifiation+announcement path, where the tip and
    -- slot coincide with the cached mempool state but the ledger state differs.
    AlwaysRevalidate

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
  m (MempoolSnapshot blk)
implGetSnapshotFor = getSnapshotUsingPolicyFor UseCache

-- | Like 'implGetSnapshotFor', but never short-circuits to the cached
-- internal snapshot: it always revalidates the mempool against the supplied
-- ledger state. See 'AlwaysRevalidate'.
--
-- Values are read at the unticked parent, so the caller's @ticked@ diffs
-- (closure ⊕ tick) must be relative to that same state.
implGetSnapshotForNoCache ::
  ( IOLike m
  , LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  ) =>
  MempoolEnv m blk ->
  -- | Get snapshot for this slot number (usually the current slot)
  SlotNo ->
  -- | The ledger state at which we want the snapshot, ticked to @slot@.
  TickedLedgerState blk DiffMK ->
  -- | A function that returns values corresponding to the given keys for
  -- the unticked ledger state.
  (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  m (MempoolSnapshot blk)
implGetSnapshotForNoCache = getSnapshotUsingPolicyFor AlwaysRevalidate

-- | Shared implementation of 'implGetSnapshotFor' and 'implGetSnapshotForNoCache'.
getSnapshotUsingPolicyFor ::
  ( IOLike m
  , LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  ) =>
  SnapshotCachePolicy ->
  MempoolEnv m blk ->
  SlotNo ->
  TickedLedgerState blk DiffMK ->
  (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  m (MempoolSnapshot blk)
getSnapshotUsingPolicyFor policy mpEnv slot ticked readUntickedTables = do
  is <- atomically $ readTMVar istate
  if pointHash (isTip is) == castHash (getTipHash ticked)
    -- && isSlotNo is == slot -- FIXME(bladyjoker): Accepting producing bad blocks in order to speed this up
    then do
      traceWith (mpEnvTracer mpEnv) $ TraceMempoolCacheHit (isTip is)
      -- We are looking for a snapshot xeactly for the ledger state we already
      -- have cached, then just return it.
      pure $ snapshotFromIS is
    else do
      traceWith (mpEnvTracer mpEnv) $ TraceMempoolCacheMiss (isTip is)
      values <- readUntickedTables (isTxKeys is)
      pure $
        computeSnapshot
          capacityOverride
          cfg
          slot
          ticked
          values
          (isLastTicketNo is)
          (TxSeq.toList $ isTxs is)
 where
  MempoolEnv
    { mpEnvStateVar = istate
    , mpEnvLedgerCfg = cfg
    , mpEnvCapacityOverride = capacityOverride
    } = mpEnv
