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
  ( forgetLedgerTables
  )
import Ouroboros.Consensus.Mempool.API
import Ouroboros.Consensus.Mempool.Impl.Common
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import Ouroboros.Consensus.Util.IOLike
import Prelude hiding (read)

-- | Accumulator type threaded through each 'reapplyStep' call by top-level 'reapply' and 'reapplyUntilTimeout'.
data ReapplyStepState blk = ReapplyStepState
  { currentLedgerSt :: !(TickedLedgerState blk EmptyMK)
  -- ^ Ticked ledger state reflecting the cumulative (internal, no tables) effect of all applied transactions so far ('appliedTxs').
  , remainingTxs :: !(TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk))
  -- ^ Remaining transactions to be applied.
  , unapplicableTxs :: !(StrictSeq (Invalidated blk))
  -- ^ Transactions that failed reapplication, accumulated in original order of 'remainingTxs'.
  , appliedTxs :: !(TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk))
  -- ^ Transactions successfully applied to produce 'currentLedgerSt'.
  , appliedTxIds :: !(Set.Set (GenTxId blk))
  -- ^ Set of all accepted transactions (view on 'appliedTxs').
  , deadOutputs :: !(Set (TxIn (LedgerState blk)))
  -- ^ UTxOs created by unapplicable transactions in 'unapplicableTxs'.
  , values :: Map (TxIn (LedgerState blk)) (TxOut (LedgerState blk))
  -- ^ Cache for UTxOs resolved by 'appliedTxs'.
  , crd :: !(CRD (TxIn (LedgerState blk)) (TxOut (LedgerState blk)))
  }

initReapplyStepState ::
  LedgerSupportsMempool blk =>
  TickedLedgerState blk DiffMK ->
  TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk) ->
  ReapplyStepState blk
initReapplyStepState startingLedgerStDiff txs =
  let initCRD = crdFromLedgerState startingLedgerStDiff
   in ReapplyStepState
        { currentLedgerSt = forgetLedgerTables startingLedgerStDiff
        , remainingTxs = txs
        , unapplicableTxs = mempty
        , appliedTxs = TxSeq.Empty
        , appliedTxIds = Set.empty
        , deadOutputs = deleted initCRD -- handling ticking "deleted"
        , values = mempty
        , crd = initCRD{deleted = Set.empty} -- handling ticking "created"
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
  -- `({A, B, D, F}) = findBoundaryInputs [ "tx deletes A, reads B, creates C", "tx deletes C, reads D, creates E", "tx deletes E, reads F, creates G"]`
  let !allInputsStep = findBoundaryInputs txsToApplyStep
      !inputsStep = allInputsStep `Set.difference` deadOutputs -- Due to applicability of txs we don't need to further subtract the `deleted crd`!
      !inputsInMemoryStep = created crd `Map.restrictKeys` inputsStep
      !inputsNotInMemoryStep = inputsStep `Set.difference` Map.keysSet inputsInMemoryStep
      !inputsInCacheStep = values `Map.restrictKeys` inputsNotInMemoryStep
      !inputsToResolveStep = inputsNotInMemoryStep `Set.difference` Map.keysSet inputsInCacheStep

  !(LedgerTables (ValuesMK inputsResolvedStep)) <-
    resolveValues (LedgerTables (KeysMK inputsToResolveStep))

  -- Prepare the ledger state with values
  let
    -- These inputs are all mutually exclusive
    !currentValuesStep = LedgerTables (ValuesMK $ Map.unions [inputsResolvedStep, inputsInCacheStep, inputsInMemoryStep])
    !currentLedgerStWitValues = currentLedgerSt `withLedgerTables` currentValuesStep
    !stForStep =
      st
        { remainingTxs = remainingTxsStep
        , values = values <> inputsResolvedStep
        }

  return $! reapplyTxs' cfg slot txsToApplyStep currentLedgerStWitValues stForStep

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
                      , deadOutputs = deadOutputs st `Set.union` (Map.keysSet . created $ txCrd)
                      }
                  )
                Right !ledgerStAfterTx ->
                  ( ledgerStAfterTx
                  , st
                      { appliedTxs = appliedTxs st TxSeq.:> tkt
                      , appliedTxIds = Set.insert (txId (txForgetValidated vtx)) (appliedTxIds st)
                      , currentLedgerSt = forgetLedgerTables curLedger
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

crdFromLedgerState ::
  forall blk.
  LedgerSupportsMempool blk =>
  TickedLedgerState blk DiffMK -> CRD (TxIn (LedgerState blk)) (TxOut (LedgerState blk))
crdFromLedgerState st =
  let (deleted, created) = Diff.deletedAndCreated . getDiffMK . getLedgerTables . projectLedgerTables $ st
   in CRD created mempty deleted

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
  TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk) ->
  Set (TxIn (LedgerState blk))
findBoundaryInputs txs =
  let (boundaryInputs, _) =
        Foldable.foldl'
          findBoundaryInputsStep
          (Set.empty, mempty)
          [crdFromTx tx | tx <- Foldable.toList txs]
   in boundaryInputs

findBoundaryInputsStep ::
  Ord k =>
  -- acc
  (Set k, CRD k v) ->
  -- Tx CRD
  CRD k v ->
  (Set k, CRD k v)
findBoundaryInputsStep =
  \(!batchInputs, !batchCRD@(CRD batchC _batchR batchD))
   txCRD@(CRD _txC txR txD) ->
      let txIns = txR `Set.union` txD
          -- if created in this batch then it's not a boundary input,
          -- if deleted then either it was a boundary input (already in batchInputs) or wasn't a boundary input (remove it)
          -- if read in this batch then it's already in batchInputs
          !newBatchInputs = txIns `Set.difference` (Map.keysSet batchC `Set.union` batchD)
       in ( batchInputs `Set.union` newBatchInputs
          , batchCRD <> txCRD
          )
