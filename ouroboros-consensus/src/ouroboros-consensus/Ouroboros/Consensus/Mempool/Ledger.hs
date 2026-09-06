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
import Ouroboros.Consensus.Block.Abstract (SlotNo)
import Ouroboros.Consensus.Ledger.Abstract
  ( DiffMK (getDiffMK)
  , HasLedgerTables (projectLedgerTables, withLedgerTables)
  , KeysMK (KeysMK, getKeysMK)
  , LedgerConfig
  , LedgerState
  , LedgerTables (LedgerTables, getLedgerTables)
  , TickedLedgerState
  , TxIn
  , TxOut
  , ValuesMK (ValuesMK)
  )
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( GenTx
  , GenTxId
  , HasTxId (txId)
  , Invalidated (Invalidated)
  , LedgerSupportsMempool (getTransactionKeySets, reapplyTx, txForgetValidated)
  )
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Ouroboros.Consensus.Ledger.Tables.Utils
  ( forgetLedgerTables
  )
import Ouroboros.Consensus.Mempool.API
import Ouroboros.Consensus.Mempool.Impl.Common
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import Ouroboros.Consensus.Util.IOLike

-- | Accumulator type threaded through each 'reapplyStep' call by top-level 'reapply' and 'reapplyUntilTimeout'.
data ReapplyStepState blk = ReapplyStepState
  { currentLedgerSt :: !(TickedLedgerState blk ValuesMK)
  -- ^ Ticked ledger state reflecting the cumulative effect of all applied transactions so far ('appliedTxs').
  , remainingTxs :: !(TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk))
  -- ^ Remaining transactions to be applied (__applicability condition holds__)
  , unapplicableTxs :: !(StrictSeq (Invalidated blk))
  -- ^ Transactions that failed reapplication, accumulated in original order of 'remainingTxs'.
  , appliedTxs :: !(TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk))
  -- ^ Transactions successfully applied to produce 'currentLedgerSt'.
  , appliedTxIds :: !(Set.Set (GenTxId blk))
  -- ^ Set of all accepted transactions (view on 'appliedTxs').
  , phantomUTxOs :: !(Set (TxIn (LedgerState blk)))
  -- ^ UTxOs created by unapplicable transactions in 'unapplicableTxs'.
  }

initReapplyStepState ::
  LedgerSupportsMempool blk =>
  TickedLedgerState blk DiffMK ->
  TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk) ->
  ReapplyStepState blk
initReapplyStepState startingLedgerStDiff txs =
  let (tickingCreated, tickingDeleted) = cdFromLedgerState startingLedgerStDiff
   in ReapplyStepState
        { currentLedgerSt =
            forgetLedgerTables startingLedgerStDiff `withLedgerTables` LedgerTables (ValuesMK tickingCreated)
        , remainingTxs = txs
        , unapplicableTxs = mempty
        , appliedTxs = TxSeq.Empty
        , appliedTxIds = Set.empty
        , phantomUTxOs = tickingDeleted -- REVIEW: handling ticking "deleted"
        }

-- | `let res = reapply resolve cfg slot ls txs` reapplies all 'txs' against 'ls' at 'slot' using 'resolve' to fetch values at 'ls' required by the transactions.
reapply ::
  forall blk m.
  (IOLike m, LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  LedgerConfig blk ->
  SlotNo ->
  TickedLedgerState blk DiffMK ->
  TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk) ->
  m (ReapplyStepState blk)
reapply resolveValues cfg slot baseLedgerStDiff txs =
  iterateUntilM
    (null . remainingTxs)
    (reapplyStep (length txs) resolveValues cfg slot)
    (initReapplyStepState baseLedgerStDiff txs)

-- | `let res = reapplyUntilTimeout ttl stepSize ...` performs 'reapplyStep' on 'stepSize' transactions per step until 'ttl' is exceeded.
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
reapplyUntilTimeout reapplyTimeout reapplyPerStep resolveValues cfg slot baseLedgerStDiff txs =
  iterateUntilOrTimeout_
    reapplyTimeout
    (null . remainingTxs)
    (reapplyStep reapplyPerStep resolveValues cfg slot)
    (initReapplyStepState baseLedgerStDiff txs)

-- | One iteration of the reapplication loop.
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

  let LedgerTables (ValuesMK currValues) = projectLedgerTables currentLedgerSt
      -- Find boundary input keys
      -- Reminder of how this works
      -- `({A, B, D, F}) = boundaryInputsForTxs [ "tx deletes A, reads B, creates C", "tx deletes C, reads D, creates E", "tx deletes E, reads F, creates G"]`
      !inputsStep = boundaryInputsForTxs txsToApplyStep
      -- Never resolves phantom inputs or in memory inputs.
      -- Note that because of applicability, txIns in 'inputsStep' can only refer to
      -- a) phantom UTxOs
      -- b) live UTxOs in current Ledger State (never dead)
      -- c) dead or live UTxO in 'resolveValues'
      !inputsToResolveStep = inputsStep `Set.difference` phantomUTxOs `Set.difference` Map.keysSet currValues

  LedgerTables (ValuesMK inputsResolvedStep) <-
    resolveValues (LedgerTables (KeysMK inputsToResolveStep))

  -- Prepare the ledger state with values
  let !allValues = LedgerTables (ValuesMK (currValues `Map.union` inputsResolvedStep))
      !stForStep =
        st
          { remainingTxs = remainingTxsStep
          , currentLedgerSt = currentLedgerSt `withLedgerTables` allValues
          }

  return $! reapplyTxs cfg slot txsToApplyStep stForStep

reapplyTxs ::
  (LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  LedgerConfig blk ->
  SlotNo ->
  TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk) ->
  ReapplyStepState blk ->
  ReapplyStepState blk
reapplyTxs cfg slot txs stBefore =
  Foldable.foldl'
    reapplyTxsStep
    stBefore
    (TxSeq.toList txs)
 where
  reapplyTxsStep !st !tkt =
    let tx = TxSeq.txTicketTx tkt
        vtx = validatedTx tx
        txCreated = createdFromTx tx
     in case runExcept (reapplyTx cfg slot vtx (currentLedgerSt st)) of
          Left !err ->
            st
              { unapplicableTxs = unapplicableTxs st |> Invalidated vtx err
              , phantomUTxOs = phantomUTxOs st `Set.union` Map.keysSet txCreated -- propagate phantom outputs
              }
          Right !ledgerStAfterTx ->
            st
              { appliedTxs = appliedTxs st TxSeq.:> tkt
              , appliedTxIds = Set.insert (txId (txForgetValidated vtx)) (appliedTxIds st)
              , currentLedgerSt = ledgerStAfterTx
              }

-- * Helpers

iterateUntilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM p f a
  | p a = return a
  | otherwise = f a >>= iterateUntilM p f

createdFromTx ::
  forall blk.
  LedgerSupportsMempool blk =>
  ValidatedTxWithDiffs blk ->
  Map (TxIn (LedgerState blk)) (TxOut (LedgerState blk))
createdFromTx = snd . Diff.deletedAndCreated . diffFromTx

diffFromTx ::
  forall blk.
  ValidatedTxWithDiffs blk -> Diff.Diff (TxIn (LedgerState blk)) (TxOut (LedgerState blk))
diffFromTx = getDiffMK . getLedgerTables . validatedTxDiffs

inputsFromTx ::
  forall blk. LedgerSupportsMempool blk => ValidatedTxWithDiffs blk -> Set (TxIn (LedgerState blk))
inputsFromTx = getKeysMK . getLedgerTables . getTransactionKeySets . txForgetValidated . validatedTx

cdFromLedgerState ::
  forall blk.
  LedgerSupportsMempool blk =>
  TickedLedgerState blk DiffMK ->
  (Map (TxIn (LedgerState blk)) (TxOut (LedgerState blk)), Set (TxIn (LedgerState blk)))
cdFromLedgerState st =
  let (deleted, created) = Diff.deletedAndCreated . getDiffMK . getLedgerTables . projectLedgerTables $ st
   in (created, deleted)

-- | `let boundaryInputs = boundaryInputsForTxs txs` finds __boundary inputs__ for a transaction batch 'txs'.
--
-- A __boundary input__ is a tx input that wasn't created by any tx in 'txs' (not an __intermediary input__).
-- That's precisely the __input closure__ 'txs' require in the ledger state table before application.
boundaryInputsForTxs ::
  forall blk.
  LedgerSupportsMempool blk =>
  TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk) ->
  Set (TxIn (LedgerState blk))
boundaryInputsForTxs txs =
  let (!boundaryInputs, _) =
        Foldable.foldl'
          boundaryInputsForTxsStep
          (Set.empty, mempty)
          [(inputsFromTx tx, diffFromTx tx) | tx <- Foldable.toList txs]
   in boundaryInputs

-- PRECONDITION: `_prop_applicable (Diff.deletedAndCreated batchDiff) (txIns, txDiff)`
boundaryInputsForTxsStep ::
  Ord k =>
  (Set k, Diff.Diff k v) -> -- (boundaryInputs, batchDiff)
  (Set k, Diff.Diff k v) -> -- (txIns, txDiff)
  (Set k, Diff.Diff k v)
boundaryInputsForTxsStep
  (!boundaryInputs, !batchDiff)
  (!txIns, !txDiff) =
    let
      -- if created in this batch then it's not a boundary input,
      -- if deleted in this batch then that means the 'Applicability precondition' was violated
      -- if read in this batch then it's already in boundaryInputs (re-added but Set.union is idempotent)
      !newBoundaryInputs = txIns `Set.difference` Diff.keysSet batchDiff
     in
      ( boundaryInputs `Set.union` newBoundaryInputs
      , batchDiff <> txDiff
      )

-- | Applicability property: `_prop_applicable st tx` holds when a transactions 'tx' can be applied to 'st'.
_prop_applicable ::
  Ord k =>
  (Set k, Map k v) ->
  (Set k, Diff.Diff k v) ->
  Bool
_prop_applicable (deletedL, createdL) (txInsR, txDiffR) =
  let (_, createdR) = Diff.deletedAndCreated txDiffR
   in Map.keysSet createdL `Set.disjoint` Map.keysSet createdR -- no double creation
        && txInsR `Set.disjoint` deletedL -- no reads or deletes of already-deleted keys
