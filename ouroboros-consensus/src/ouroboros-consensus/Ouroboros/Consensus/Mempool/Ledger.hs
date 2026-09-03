{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Ledger reapplication on transaction sequence
module Ouroboros.Consensus.Mempool.Ledger
  ( reapplyUntilTimeout
  , ReapplyStepState (..)
  , reapply
  ) where

import Control.Monad.Except (runExcept)
import qualified Data.Foldable as Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence.Strict (StrictSeq, (|>))
import Data.Set (Set)
import qualified Data.Set as Set
import LeiosUtils.TimeBoundedLoop (iterateUntilOrTimeout_)
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Ouroboros.Consensus.Ledger.Tables.Utils
  ( prependDiffs
  )
import Ouroboros.Consensus.Mempool.API
import Ouroboros.Consensus.Mempool.Impl.Common
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import Ouroboros.Consensus.Util.IOLike

-- | Accumulator type threaded through each 'reapplyStep' call by top-level 'reapply'.
data ReapplyStepState blk = ReapplyStepState
  { currentLedgerStWithDiffs :: !(TickedLedgerState blk DiffMK)
  -- ^ Ticked ledger state reflecting the cumulative effect of all applied
  -- transactions so far ('appliedTxs').
  , remainingTxs :: !(TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk))
  -- ^ Remaining transactions to be applied. Starts as the full
  -- mempool sequence and is consumed 'reapplyStepTxsPerStep' at a time.
  -- Iteration terminates when this becomes empty.
  , unapplicableTxs :: !(StrictSeq (Invalidated blk))
  -- ^ Transactions that failed reapplication, accumulated in original mempool
  -- order.
  , appliedTxs :: !(TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk))
  -- ^ Transactions successfully applied so far to some base Ledger state that results in 'currentLedgerSt', in original mempool order.
  -- Becomes the transaction sequence of the final 'MempoolSnapshot'.
  , appliedTxIds :: !(Set.Set (GenTxId blk))
  -- ^ Set of all accepted transactions (view on 'appliedTxs').
  , deadOutputs :: !(Set (TxIn (LedgerState blk)))
  -- ^ UTxOs created by unapplicable transactions in 'unapplicableTxs'
  }

initReapplyStepState ::
  LedgerSupportsMempool blk =>
  TickedLedgerState blk DiffMK ->
  TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk) ->
  ReapplyStepState blk
initReapplyStepState startingLedgerStDiff txs =
  ReapplyStepState startingLedgerStDiff txs mempty TxSeq.Empty Set.empty Set.empty

reapply ::
  forall blk m.
  (IOLike m, LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  LedgerConfig blk ->
  SlotNo ->
  TickedLedgerState blk DiffMK ->
  TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk) ->
  m (ReapplyStepState blk)
reapply resolveValues cfg slot baseLedgerStDiff txsToApply =
  iterateUntilM
    (null . remainingTxs)
    (reapplyStep (length txsToApply) resolveValues cfg slot)
    (initReapplyStepState baseLedgerStDiff txsToApply)

reapplyUntilTimeout ::
  forall blk m.
  (IOLike m, LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  DiffTime ->
  Int ->
  (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  LedgerConfig blk ->
  SlotNo ->
  TickedLedgerState blk DiffMK ->
  TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk) ->
  m (ReapplyStepState blk)
reapplyUntilTimeout reapplyTimeout reapplyPerStep resolveValues cfg slot baseLedgerStDiff txsToApply =
  iterateUntilOrTimeout_
    reapplyTimeout
    (null . remainingTxs)
    (reapplyStep reapplyPerStep resolveValues cfg slot)
    (initReapplyStepState baseLedgerStDiff txsToApply)

reapplyStep ::
  forall blk m.
  (LedgerSupportsMempool blk, HasTxId (GenTx blk), IOLike m) =>
  Int ->
  (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  LedgerConfig blk ->
  SlotNo ->
  ReapplyStepState blk ->
  m (ReapplyStepState blk)
reapplyStep reapplyPerStep resolveValues cfg slot st@ReapplyStepState{..} = do
  let (!txsToApplyStep, !remainingTxsStep) = TxSeq.take reapplyPerStep remainingTxs

  -- Find boundary input keys
  let !(inputsStep, inputsToResolveStep) = findBoundaryInputs currentLedgerStWithDiffs deadOutputs txsToApplyStep

  !inputValuesResolvedStep <- resolveValues inputsToResolveStep

  -- Prepare the ledger state with values
  let
    !currentLedgerSt =
      applyMempoolDiffs inputValuesResolvedStep inputsStep currentLedgerStWithDiffs
    !stForStep =
      st
        { remainingTxs = remainingTxsStep
        }

  return $! reapplyTxs' cfg slot txsToApplyStep currentLedgerSt stForStep

reapplyTxs' ::
  (LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  LedgerConfig blk ->
  SlotNo ->
  TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk) ->
  TickedLedgerState blk ValuesMK ->
  ReapplyStepState blk ->
  ReapplyStepState blk
reapplyTxs' cfg slot txs startingLedgerSt stBefore =
  snd $
    Foldable.foldl'
      ( \(!curLedger, !st) !tkt ->
          let tx = TxSeq.txTicketTx tkt
              vtx = validatedTx tx
           in case runExcept (reapplyTx cfg slot vtx curLedger) of
                Left !err ->
                  ( curLedger
                  , st
                      { unapplicableTxs = unapplicableTxs st |> Invalidated vtx err
                      , deadOutputs = deadOutputs st `Set.union` (Map.keysSet . outputsFromTx $ tx)
                      }
                  )
                Right !ledgerStAfterTx ->
                  ( ledgerStAfterTx
                  , st
                      { appliedTxs = appliedTxs st TxSeq.:> tkt
                      , appliedTxIds = Set.insert (txId (txForgetValidated vtx)) (appliedTxIds st)
                      , currentLedgerStWithDiffs =
                          currentLedgerStWithDiffs st
                            `withLedgerTables` prependDiffs
                              (projectLedgerTables (currentLedgerStWithDiffs st))
                              (validatedTxDiffs tx)
                      }
                  )
      )
      (startingLedgerSt, stBefore)
      (TxSeq.toList txs)

-- * Helpers

iterateUntilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM p f a
  | p a = return a
  | otherwise = f a >>= iterateUntilM p f

diffFromLedgerState ::
  forall blk.
  LedgerSupportsMempool blk =>
  TickedLedgerState blk DiffMK -> Diff.Diff (TxIn (LedgerState blk)) (TxOut (LedgerState blk))
diffFromLedgerState = getDiffMK . getLedgerTables . projectLedgerTables

diffFromTx ::
  forall blk.
  ValidatedTxWithDiffs blk -> Diff.Diff (TxIn (LedgerState blk)) (TxOut (LedgerState blk))
diffFromTx = getDiffMK . getLedgerTables . validatedTxDiffs

inputsFromTx ::
  forall blk. LedgerSupportsMempool blk => ValidatedTxWithDiffs blk -> Set (TxIn (LedgerState blk))
inputsFromTx = getKeysMK . getLedgerTables . getTransactionKeySets . txForgetValidated . validatedTx

outputsFromTx ::
  forall blk.
  LedgerSupportsMempool blk =>
  ValidatedTxWithDiffs blk -> Map (TxIn (LedgerState blk)) (TxOut (LedgerState blk))
outputsFromTx = snd . Diff.deletedAndCreated . getDiffMK . getLedgerTables . validatedTxDiffs

-- | `let (boundaryInputs, boundaryInputsToResolve) = findBoundaryInputs ledgerStWithDiffs deadOutputs txs` finds __boundary inputs__ for a transaction batch 'txs'
-- and its subset 'boundaryInputsToResolve'.
-- A __boundary input__ is a tx input that wasn't created by any tx in 'txs'.
-- That's precisely the __input closure__ 'txs' require in the ledger state table before application.
-- 'boundaryInputsToResolve' are ommitting:
-- a) UTxOs that have been deleted or created in batch 'txs'
-- b) UTxOs that have been created or deleted in 'ledgerStWithDiffs'
findBoundaryInputs ::
  forall blk.
  LedgerSupportsMempool blk =>
  TickedLedgerState blk DiffMK ->
  Set (TxIn (LedgerState blk)) ->
  TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk) ->
  ((LedgerTables (LedgerState blk) KeysMK), LedgerTables (LedgerState blk) KeysMK)
findBoundaryInputs ledgerStWithDiffs deadOutputs txs =
  let !(inputsStep, inputsToResolveStep, _, _) =
        Foldable.foldl'
          (findBoundaryInputsStep (diffFromLedgerState ledgerStWithDiffs) deadOutputs)
          (Set.empty, Set.empty, mempty, Set.empty)
          [(inputsFromTx tx, diffFromTx tx) | tx <- Foldable.toList txs]
   in (LedgerTables (KeysMK inputsStep), LedgerTables (KeysMK inputsToResolveStep))

-- | Goal: Only resolve what is actually necessary
-- 1. Chained good txs [ ok "tx deletes A, reads B, creates C", ok "tx deletes C, reads D, creates E"] => resolves {A, B, D}
-- 2. Independent good txs [ok "tx deletes A, reads B, creates C", ok "tx deletes D, reads E, creates F"] => resolves {A, B, D, E}
-- 3. Chained bad txs with [bad "tx deletes A, reads B, creates C", bad "tx deletes C, reads D, creates E"] => resolves {A, B}
-- 4. Independent bad txs [bad "tx deletes A, reads B, creates C", bad "tx deletes D, reads E, creates F"] => resolves {A, B, D, E}
findBoundaryInputsStep ::
  Ord k =>
  Diff.Diff k v ->
  Set k ->
  (Set k, Set k, Diff.Diff k v, Set k) ->
  (Set k, Diff.Diff k v) ->
  (Set k, Set k, Diff.Diff k v, Set k)
findBoundaryInputsStep diffs deadKeys =
  ( \(batchInputs, batchToResolve, batchDiffs, batchDeadKeys) (txIns, txDiff) ->
      let newBatchInputs = batchInputs `Set.union` (txIns `Set.difference` Diff.keysSet batchDiffs) -- boundary inputs not created/deleted in this batch
          requiresKnownDeadKeys = not (txIns `Set.disjoint` batchDeadKeys && txIns `Set.disjoint` deadKeys)
          txOuts = snd . Diff.deletedAndCreated $ txDiff
       in if requiresKnownDeadKeys -- tx will fail
            then
              ( batchInputs `Set.union` newBatchInputs -- still adding
              , batchToResolve -- fetch nothing for tx that fails
              , batchDiffs -- unapplicable txs don't contribute their diffs
              , batchDeadKeys `Set.union` Map.keysSet txOuts -- add created UTxOs into deadKeys
              )
            else -- tx might succeeed
              ( batchInputs `Set.union` newBatchInputs
              , batchToResolve
                  `Set.union` ( txIns
                                  `Set.difference` Diff.keysSet batchDiffs -- created/deleted in this batch
                                  `Set.difference` Diff.keysSet diffs -- created/deleted in prior batches
                              )
              , batchDiffs <> txDiff
              , batchDeadKeys
              )
  )
