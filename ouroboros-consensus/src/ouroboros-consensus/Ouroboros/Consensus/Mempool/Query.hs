{-# LANGUAGE FlexibleContexts #-}

-- | Queries to the mempool
module Ouroboros.Consensus.Mempool.Query (implGetSnapshotFor) where

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
  TickedLedgerState m blk ->
  -- | A function that returns values corresponding to the given keys for
  -- the unticked ledger state.
  m (MempoolSnapshot blk)
implGetSnapshotFor mpEnv slot ticked = do
  is <- atomically $ readTMVar istate
  let txs =
        [ TxSeq.TxTicket tx tn tz
        | TxSeq.TxTicket (ValidatedTxWithDiffs tx) tn tz <- TxSeq.toList $ isTxs is
        ]
  if pointHash (isTip is) == castHash (getTipHash ticked)
    && isSlotNo is == slot
    then
      -- We are looking for a snapshot exactly for the ledger state we already
      -- have cached, then just return it.
      pure $ snapshotFromValidTxs txs (castPoint $ isTip is) (isSlotNo is)
    else do
      _values <-
        if pointHash (isTip is) == castHash (getTipHash ticked)
          -- We are looking for a snapshot at the same state ticked
          -- to a different slot, so we can reuse the cached values
          then pure (isCache is)
          -- We are looking for a snapshot at a different state, so we
          -- need to read the values from the ledgerdb.
          else fillCache txs ticked (isCache is)
      computeSnapshot cfg slot ticked txs
 where
  MempoolEnv
    { mpEnvStateVar = istate
    , mpEnvLedgerCfg = cfg
    } = mpEnv

fillCache ::
  [TxSeq.TxTicket (TxMeasureWithDiffTime blk) (Validated (GenTx blk))] ->
  TickedLedgerState m blk ->
  MempoolCache blk ->
  m (MempoolCache blk)
fillCache = undefined
