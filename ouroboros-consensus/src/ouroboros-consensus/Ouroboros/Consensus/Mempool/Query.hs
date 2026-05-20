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
  , MonadLedger m blk
  ) =>
  MempoolEnv m blk ->
  -- | Get snapshot for this slot number (usually the current slot)
  SlotNo ->
  -- | The ledger state at which we want the snapshot, ticked to @slot@.
  TickedStateHandle m blk ->
  m (MempoolSnapshot blk)
implGetSnapshotFor mpEnv slot ticked = do
  is <- atomically $ readTMVar istate
  let txs = TxSeq.toList $ isTxs is
  if pointHash (isTip is) == getTipHash (tickedState ticked)
    && isSlotNo is == slot
    then
      -- We are looking for a snapshot exactly for the ledger state we already
      -- have cached, then just return it.
      pure $ snapshotFromValidTxs txs (isTip is) (isSlotNo is)
    else
      -- Different state or different slot: start from a fresh cache.
      -- TODO: the "same tip, different slot" case could reuse @isCache
      -- is@ to skip re-reading values.
      computeSnapshot cfg slot ticked (mkMempoolCache ticked) txs
 where
  MempoolEnv
    { mpEnvStateVar = istate
    , mpEnvLedgerCfg = cfg
    } = mpEnv
