{-# LANGUAGE FlexibleContexts #-}

-- | Queries to the mempool
module Ouroboros.Consensus.Mempool.Query (implGetSnapshotFor) where

import qualified Data.Foldable as Foldable
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Mempool.Impl.Common
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Util.IOLike

implGetSnapshotFor ::
     ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     )
  => MempoolEnv m blk
  -> SlotNo -- ^ Get snapshot for this slot number (usually the current slot)
  -> TickedLedgerState blk DiffMK -- ^ The ledger state at which we want the
                                  -- snapshot, ticked to @slot@.
  -> (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK))
      -- ^ A function that returns values corresponding to the given keys for
      -- the unticked ledger state.
  -> m (MempoolSnapshot blk)
implGetSnapshotFor mpEnv slot ticked readUntickedTables = do
  is <- atomically $ readTMVar istate
  if pointHash (isTip is) == castHash (getTipHash ticked) &&
     isSlotNo is == slot
    then
      -- We are looking for a snapshot exactly for the ledger state we already
      -- have cached, then just return it.
      pure . snapshotFromIS $ is
    else do
       let keys = Foldable.foldMap'
                    getTransactionKeySets
                    [ txForgetValidated . TxSeq.txTicketTx $ tx
                    | tx <- TxSeq.toList $ isTxs is
                    ]
       values <- readUntickedTables keys
       pure $ snapshotFromIS $
         if pointHash (isTip is) == castHash (getTipHash ticked) && isSlotNo is == slot
         then is
         else newInternalState
            $ revalidateTxsFor
                capacityOverride
                cfg
                slot
                ticked
                values
                (isLastTicketNo is)
                (TxSeq.toList $ isTxs is)
 where
    MempoolEnv { mpEnvStateVar         = istate
               , mpEnvLedgerCfg        = cfg
               , mpEnvCapacityOverride = capacityOverride
               } = mpEnv
