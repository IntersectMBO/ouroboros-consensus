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
  , unionValues
  )
import Ouroboros.Consensus.Mempool.API
import Ouroboros.Consensus.Mempool.Impl.Common
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import Ouroboros.Consensus.Util.IOLike
import Prelude hiding (read)

-- | Accumulator type threaded through each 'reapplyStep' call by top-level 'reapply' and 'reapplyUntilTimeout'.
data ReapplyStepState blk = ReapplyStepState
  { currentLedgerStWithDiffs :: !(TickedLedgerState blk DiffMK)
  -- ^ Ticked ledger state reflecting the cumulative effect of all applied
  -- transactions so far ('appliedTxs').
  , remainingTxs :: !(TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk))
  -- ^ Remaining transactions to be applied.
  , unapplicableTxs :: !(StrictSeq (Invalidated blk))
  -- ^ Transactions that failed reapplication, accumulated in original order of 'remainingTxs'.
  , appliedTxs :: !(TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk))
  -- ^ Transactions successfully applied to produce 'currentLedgerStWithDiffs'.
  , appliedTxIds :: !(Set.Set (GenTxId blk))
  -- ^ Set of all accepted transactions (view on 'appliedTxs').
  , deadOutputs :: !(Set (TxIn (LedgerState blk)))
  -- ^ UTxOs created by unapplicable transactions in 'unapplicableTxs'.
  , readOnlyValues :: Map (TxIn (LedgerState blk)) (TxOut (LedgerState blk))
  -- ^ Cache for UTxOs that were "read only" by `appliedTxs` and therefore not in 'currentLedgerStWithDiffs'.
  , crd :: !(CRD (TxIn (LedgerState blk)) (TxOut (LedgerState blk)))
  }

initReapplyStepState ::
  LedgerSupportsMempool blk =>
  TickedLedgerState blk DiffMK ->
  TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk) ->
  ReapplyStepState blk
initReapplyStepState startingLedgerStDiff txs =
  ReapplyStepState
    { currentLedgerStWithDiffs = startingLedgerStDiff
    , remainingTxs = txs
    , unapplicableTxs = mempty
    , appliedTxs = TxSeq.Empty
    , appliedTxIds = Set.empty
    , deadOutputs = mempty
    , readOnlyValues = mempty
    , crd = crdFromLedgerState startingLedgerStDiff
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

  -- Find boundary input keys
  -- Reminder of how this works
  -- `({A, B}, {A}) = findBoundaryInputs {B : Insert 2} {C} [ "tx deletes A, reads B, creates C", "tx deletes C, reads D, creates E"]`
  let (!inputsStep, !inputsToResolveStep, !readOnlyValueStep) = findBoundaryInputs crd deadOutputs readOnlyValues txsToApplyStep

  !inputValuesResolvedStep <- resolveValues inputsToResolveStep -- Still don't have the read only keys!

  -- Prepare the ledger state with values
  let
    allInputValues = ltliftA2 unionValues inputValuesResolvedStep readOnlyValueStep
    -- Reminder of how this works
    -- `{A : 1, B : 2} = applyMempoolDiffs {A : 1} {A, B} {B : Insert 2}`
    !currentLedgerSt = applyMempoolDiffs allInputValues inputsStep currentLedgerStWithDiffs
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
              txCrd = crdFromTx tx
           in case runExcept (reapplyTx cfg slot vtx curLedger) of
                Left !err ->
                  ( curLedger
                  , st
                      { unapplicableTxs = unapplicableTxs st |> Invalidated vtx err
                      , deadOutputs = deadOutputs st `Set.union` (Map.keysSet . outputsFromTx $ tx)
                      }
                  )
                Right !ledgerStAfterTx ->
                  let
                    ValuesMK curVals = getLedgerTables (projectLedgerTables curLedger)
                    newReadOnlyKeys = read txCrd `Set.difference` Map.keysSet (created (crd st))
                   in
                    ( ledgerStAfterTx
                    , st
                        { appliedTxs = appliedTxs st TxSeq.:> tkt
                        , appliedTxIds = Set.insert (txId (txForgetValidated vtx)) (appliedTxIds st)
                        , currentLedgerStWithDiffs =
                            currentLedgerStWithDiffs st
                              `withLedgerTables` prependDiffs
                                (projectLedgerTables (currentLedgerStWithDiffs st))
                                (validatedTxDiffs tx)
                        , readOnlyValues =
                            (readOnlyValues st `Map.withoutKeys` deleted txCrd)
                              `Map.union` (curVals `Map.restrictKeys` newReadOnlyKeys)
                        , crd = crd st <> txCrd
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

-- | `let (boundaryInputs, boundaryInputsToResolve) = findBoundaryInputs ledgerStWithDiffs deadOutputs txs` finds __boundary inputs__ for a transaction batch 'txs'
-- and its subset 'boundaryInputsToResolve'.
--
-- A __boundary input__ is a tx input that wasn't created by any tx in 'txs' (not an __intermediary input__).
-- That's precisely the __input closure__ 'txs' require in the ledger state table before application.
--
-- A __dead transaction__ is the one that requires 'deadOutputs'. __Dead outputs__ are created by transactions that failed reapplication.
-- Its outputs are then also considered 'dead'.
-- Its __boundary inputs__ are added to 'boundaryInputs' but omitted from 'boundaryInputsToResolve'.
--
-- Properties:
-- 1. 'boundaryInputsToResolve' is subset of 'boundaryInputs'
-- 2. `boundaryInputs - boundaryInputsToResolve` is either keys in 'ledgerStWithDiffs' or keys required by dead txs.
findBoundaryInputs ::
  forall blk.
  LedgerSupportsMempool blk =>
  CRD (TxIn (LedgerState blk)) (TxOut (LedgerState blk)) ->
  Set (TxIn (LedgerState blk)) ->
  Map (TxIn (LedgerState blk)) (TxOut (LedgerState blk)) ->
  TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk) ->
  ( (LedgerTables (LedgerState blk) KeysMK)
  , LedgerTables (LedgerState blk) KeysMK
  , LedgerTables (LedgerState blk) ValuesMK
  )
findBoundaryInputs priorCRD deadOutputs readOnlyValues txs =
  let (inputsStep, inputsToResolveStep, _, _, readOnlyValues') =
        Foldable.foldl'
          (findBoundaryInputsStep2 priorCRD deadOutputs readOnlyValues)
          (Set.empty, Set.empty, mempty, Set.empty, Map.empty)
          [crdFromTx tx | tx <- Foldable.toList txs]
   in ( LedgerTables (KeysMK inputsStep)
      , LedgerTables (KeysMK inputsToResolveStep)
      , LedgerTables (ValuesMK readOnlyValues')
      )

-- Prop: keys always exclusive
data CRD k v = CRD
  { created :: !(Map k v)
  , read :: !(Set k)
  , deleted :: !(Set k)
  }

-- `let (CRD c r d) = CRD cl rl dl <> CRD cr rr dr`
-- Assumptions
-- 1. `crdL` and `crdR` must adhere to CRD props
-- 1. `crdL` and `crdR` must be 'applicable'
--   a) No double creation @Map.keysSet cl `Set.disjoint` Map.keysSet cr@
--   b) No reads of deleted keys @rr `Set.disjoint` dl@
--   c) No double spends @dr `Set.disjoint` dl@
-- Properties
-- 1. CRD prop, all keys exclusive
instance Ord k => Semigroup (CRD k v) where
  (CRD c r d) <> (CRD c' r' d') =
    CRD
      { created = (c `Map.withoutKeys` d') `Map.union` c'
      , read = (r `Set.difference` d') `Set.union` (r' `Set.difference` Map.keysSet c)
      , deleted = d `Set.union` d'
      }

instance Ord k => Monoid (CRD k v) where
  mempty = CRD mempty mempty mempty

crdFromTx ::
  forall blk.
  LedgerSupportsMempool blk =>
  ValidatedTxWithDiffs blk -> CRD (TxIn (LedgerState blk)) (TxOut (LedgerState blk))
crdFromTx tx =
  let (deletedKeys, createdMap) = Diff.deletedAndCreated (diffFromTx tx)
   in CRD
        { created = createdMap
        , read = inputsFromTx tx `Set.difference` deletedKeys
        , deleted = deletedKeys
        }
 where
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

crdFromLedgerState ::
  forall blk.
  LedgerSupportsMempool blk =>
  TickedLedgerState blk DiffMK -> CRD (TxIn (LedgerState blk)) (TxOut (LedgerState blk))
crdFromLedgerState st =
  let (deleted, created) = Diff.deletedAndCreated . getDiffMK . getLedgerTables . projectLedgerTables $ st
   in CRD created mempty deleted

-- |  `let (boundaryInputs, boundaryInputsToResolve, _, _) = findBoundaryInputsStep diffs deadKeys _ (txIns, txDiff)`
-- Only resolve what is actually necessary
-- 1. Chained good txs [ ok "tx deletes A, reads B, creates C", ok "tx deletes C, reads D, creates E"] => resolves {A, B, D}
-- 2. Independent good txs [ok "tx deletes A, reads B, creates C", ok "tx deletes D, reads E, creates F"] => resolves {A, B, D, E}
-- 3. Chained bad txs with [bad "tx deletes A, reads B, creates C", bad "tx deletes C, reads D, creates E"] => resolves {A, B}
-- 4. Independent bad txs [bad "tx deletes A, reads B, creates C", bad "tx deletes D, reads E, creates F"] => resolves {A, B, D, E}
findBoundaryInputsStep2 ::
  Ord k =>
  -- prior total CRD
  CRD k v ->
  -- prior dead keys
  Set k ->
  -- prior read only values cache
  Map k v ->
  -- acc
  (Set k, Set k, CRD k v, Set k, Map k v) ->
  -- Tx CRD
  (CRD k v) ->
  (Set k, Set k, CRD k v, Set k, Map k v)
findBoundaryInputsStep2 (CRD priorC priorR priorD) deadKeys readOnlyValues =
  \( !batchInputs
     , !batchToResolve
     , !batchCRD@(CRD batchC _batchR batchD)
     , !batchDeadKeys
     , !batchReadOnlyValues
     )
   txCRD@(CRD txC txR txD) ->
      let txIns = txR `Set.union` txD
          !newBatchInputs = txIns `Set.difference` (Map.keysSet batchC `Set.union` batchD) -- if created in this batch then it's not a boundary input, if deleted then either it was a boundary input (already in batchInputs) or wasn't a boundary input, if read in this batch then it's already in batchInputs
          !newBatchToResolve = newBatchInputs `Set.difference` (Map.keysSet priorC `Set.union` priorR `Set.union` priorD)
          !txReadOnlyValues = readOnlyValues `Map.restrictKeys` txR
          !requiresKnownDeadKeys = not (txIns `Set.disjoint` batchDeadKeys && txIns `Set.disjoint` deadKeys)
       in if requiresKnownDeadKeys -- tx will fail
            then
              ( batchInputs `Set.union` newBatchInputs -- always compute boundary inputs
              , batchToResolve -- fetch nothing for tx that fails
              , batchCRD -- unapplicable txs don't contribute their CRDs
              , batchDeadKeys `Set.union` Map.keysSet txC -- add created UTxOs into deadKeys
              , batchReadOnlyValues
              )
            else -- tx might succeeed
              ( batchInputs `Set.union` newBatchInputs -- always compute boundary inputs
              , batchToResolve `Set.union` newBatchToResolve -- if created in prior batches then it's in the diff, if deleted in prior batches then it's in diff, if read in prior batches then it is 'readOnlyValues'
              , batchCRD <> txCRD -- contribute the CRD (they are applicable)
              , batchDeadKeys
              , batchReadOnlyValues `Map.union` txReadOnlyValues
              )
