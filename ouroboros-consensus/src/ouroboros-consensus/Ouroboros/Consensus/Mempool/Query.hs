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
  -- | The ledger state at which we want the snapshot, ticked to @slot@.
  TickedLedgerState blk ->
  -- | The tick diff (from the unticked state to @ticked@), used to forward the
  -- read values up to the ticked state.
  Diff blk ->
  -- | A function that returns values corresponding to the given keys for
  -- the unticked ledger state.
  (Keys blk -> m (Values blk)) ->
  m (MempoolSnapshot blk)
implGetSnapshotFor mpEnv slot ticked tickDiff readUntickedTables = do
  is <- atomically $ readTMVar istate
  let txs =
        [ TxSeq.TxTicket tx tn tz
        | TxSeq.TxTicket (ValidatedTxWithDiffs tx _) tn tz <- TxSeq.toList $ isTxs is
        ]
  if pointHash (isTip is) == castHash (getTipHash ticked)
    && isSlotNo is == slot
    then
      -- We are looking for a snapshot exactly for the ledger state we already
      -- have cached, then just return it.
      pure $ snapshotFromValidTxs txs (castPoint $ isTip is) (isSlotNo is)
    else case txs of
      -- An empty mempool produces an empty snapshot regardless of the values,
      -- so there is nothing to read (and no keys to union).
      [] -> pure $ snapshotFromValidTxs [] (castPoint $ getTip ticked) slot
      _ -> do
        let keys =
              foldr1 (<>) $
                map
                  (\(TxSeq.TxTicket tx _ _) -> getTransactionKeySets (txForgetValidated tx))
                  txs
        values <- readUntickedTables keys
        pure $ computeSnapshot cfg slot ticked tickDiff values txs
 where
  MempoolEnv
    { mpEnvStateVar = istate
    , mpEnvLedgerCfg = cfg
    } = mpEnv
