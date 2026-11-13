{-# LANGUAGE FlexibleContexts #-}

-- | Queries to the mempool
module Ouroboros.Consensus.Mempool.Query (implGetSnapshotFor, implLookupLeiosTxs) where

import qualified Data.Map.Strict as Map
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Leios.Types (BytesSize, TxHash)
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
  (LedgerTables blk KeysMK -> m (LedgerTables blk ValuesMK)) ->
  m (MempoolSnapshot blk)
implGetSnapshotFor mpEnv slot ticked readUntickedTables = do
  is <- atomically $ readTMVar istate
  if pointHash (isTip is) == castHash (getTipHash ticked)
    && isSlotNo is == slot
    then
      -- We are looking for a snapshot exactly for the ledger state we already
      -- have cached, then just return it.
      pure $ snapshotFromIS is
    else do
      let txs =
            [ TxSeq.TxTicket tx tn tz
            | TxSeq.TxTicket ValidatedTxWithDiffs{validatedTx = tx} tn tz <- TxSeq.toList $ isTxs is
            ]
      values <-
        if pointHash (isTip is) == castHash (getTipHash ticked)
          -- We are looking for a snapshot at the same state ticked
          -- to a different slot, so we can reuse the cached values
          then pure (isTxValues is)
          -- We are looking for a snapshot at a different state, so we
          -- need to read the values from the ledgerdb.
          else readUntickedTables (isTxKeys is)
      pure $ computeSnapshot cfg slot ticked values txs
 where
  MempoolEnv
    { mpEnvStateVar = istate
    , mpEnvLedgerCfg = cfg
    } = mpEnv

-- | Borrow only the explicitly requested exact TXs. The reuse caller supplies
-- bounded slices, so no full index or mempool snapshot survives the STM read.
-- Force the selected strict map before returning: a deferred lookup would
-- retain the entire index until its eventual evaluation by the consumer.
implLookupLeiosTxs ::
  IOLike m =>
  MempoolEnv m blk -> [(TxHash, BytesSize)] -> STM m (Map.Map TxHash (Validated (GenTx blk)))
implLookupLeiosTxs env requested = do
  index <- isLeiosIndex <$> readTMVar (mpEnvStateVar env)
  pure $!
    Map.fromList
      [ (h, v)
      | (h, expected) <- requested
      , Just (actual, v) <- [Map.lookup h index]
      , actual == expected
      ]
