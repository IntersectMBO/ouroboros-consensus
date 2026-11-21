{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Reapplication of a transaction sequence against a new ledger state and slot.
--
-- The key features are:
--
-- * Time bounded
--   'reapplyUntilTimeout' stops after a wall-clock budget
--   is exceeded, returning however many transactions were successfully
--   reapplied so far.
--
-- * Optimal UTxO resolution
--   Each step resolves only the UTxOs that:
--     1. Are __boundary__ UTxOs (see `boundaryInputsForTxs`).
--     2. Are not __phantom__ UTxOs (see `phantomUTxOs`).
--     3. Are not in current live UTxO set (see `currentLedgerSt`).
module Ouroboros.Consensus.Mempool.Reapply
  ( reapplyUntilTimeout
  , ReapplyState (..)
  , reapply
  , reapplyFromState
  , reapplyStep
  , initReapplyState
  ) where

import qualified Data.Foldable as Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Measure (Measure)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Ouroboros.Consensus.Block.Abstract (SlotNo)
import Ouroboros.Consensus.Ledger.Abstract
  ( DiffMK (getDiffMK)
  , HasLedgerTables (projectLedgerTables, withLedgerTables)
  , KeysMK (KeysMK, getKeysMK)
  , LedgerConfig
  , LedgerTables (LedgerTables, getLedgerTables)
  , TickedLedgerState
  , TxIn
  , TxOut
  , ValuesMK (ValuesMK)
  )
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( GenTx
  , HasTxId (txId)
  , Invalidated (..)
  , LedgerSupportsMempool (getTransactionKeySets, txForgetValidated)
  , ReapplyTxsResult (..)
  , WhatToDoWithTxDiffs (..)
  )
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as LS
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Ouroboros.Consensus.Ledger.Tables.Utils
  ( forgetLedgerTables
  )
import Ouroboros.Consensus.Mempool.API
import Ouroboros.Consensus.Mempool.Impl.Common
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.TimeBoundedLoop (iterateUntilOrTimeout_)

-- | Accumulator type threaded through each 'reapplyStep' call by top-level 'reapply' and 'reapplyUntilTimeout'.
data ReapplyState blk = ReapplyState
  { currentLedgerSt :: !(TickedLedgerState blk ValuesMK)
  -- ^ Ticked ledger state reflecting the cumulative effect of all applied transactions so far ('appliedTxs').
  , remainingTxs :: !(TxSeq.TxSeq (MempoolMeasure blk) (ValidatedTxWithDiffs blk))
  -- ^ Remaining transactions to be applied (__applicability condition holds__)
  , unapplicableTxs :: !(StrictSeq (Invalidated blk))
  -- ^ Transactions that failed reapplication, accumulated in original order of 'remainingTxs'.
  , appliedTxs :: !(TxSeq.TxSeq (MempoolMeasure blk) (ValidatedTxWithDiffs blk))
  -- ^ Transactions successfully applied to produce 'currentLedgerSt'.
  , phantomUTxOs :: !(Set (TxIn blk))
  -- ^ UTxOs created by unapplicable transactions in 'unapplicableTxs'.
  , resolvedUTxOs :: !(Map (TxIn blk) (TxOut blk))
  -- ^ UTxOs resolved across all steps.
  --
  -- Invariant: @resolvedUTxOs == boundaryInputsForTxs appliedTxs \`Set.difference\` tickingCreated@
  -- where @tickingCreated@ are UTxOs created by the ticking operation (pre-loaded into
  -- 'currentLedgerSt' by 'initReapplyState' without a forker lookup).
  }

initReapplyState ::
  LedgerSupportsMempool blk =>
  TickedLedgerState blk DiffMK ->
  TxSeq.TxSeq (MempoolMeasure blk) (ValidatedTxWithDiffs blk) ->
  ReapplyState blk
initReapplyState startingLedgerStDiff txs =
  let (tickingCreated, tickingDeleted) = cdFromLedgerState startingLedgerStDiff
   in ReapplyState
        { currentLedgerSt =
            forgetLedgerTables startingLedgerStDiff `withLedgerTables` LedgerTables (ValuesMK tickingCreated)
        , remainingTxs = txs
        , unapplicableTxs = mempty
        , appliedTxs = TxSeq.Empty
        , phantomUTxOs = tickingDeleted
        , resolvedUTxOs = Map.empty
        }

-- | Like 'reapply' but continuing from an existing 'ReapplyState'.
reapplyFromState ::
  forall blk m.
  (IOLike m, LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  (LedgerTables blk KeysMK -> m (LedgerTables blk ValuesMK)) ->
  LedgerConfig blk ->
  SlotNo ->
  ReapplyState blk ->
  m (ReapplyState blk)
reapplyFromState resolveValues cfg slot st =
  iterateUntilM
    (null . remainingTxs)
    (reapplyStep (length (remainingTxs st)) resolveValues cfg slot)
    st

-- | `res <- reapply resolve cfg slot ls txs` reapplies all 'txs' against 'ls' at 'slot' using 'resolve' to fetch values at 'ls' required by the transactions.
reapply ::
  forall blk m.
  (IOLike m, LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  (LedgerTables blk KeysMK -> m (LedgerTables blk ValuesMK)) ->
  LedgerConfig blk ->
  SlotNo ->
  TickedLedgerState blk DiffMK ->
  TxSeq.TxSeq (MempoolMeasure blk) (ValidatedTxWithDiffs blk) ->
  m (ReapplyState blk)
reapply resolveValues cfg slot baseLedgerStDiff txs =
  reapplyFromState resolveValues cfg slot (initReapplyState baseLedgerStDiff txs)

-- | `res <- reapplyUntilTimeout ttl stepSize ...` performs 'reapplyStep' on 'stepSize' transactions per step until 'ttl' is exceeded.
reapplyUntilTimeout ::
  forall blk m.
  (IOLike m, LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  DiffTime ->
  Int ->
  (LedgerTables blk KeysMK -> m (LedgerTables blk ValuesMK)) ->
  LedgerConfig blk ->
  SlotNo ->
  TickedLedgerState blk DiffMK ->
  TxSeq.TxSeq (MempoolMeasure blk) (ValidatedTxWithDiffs blk) ->
  m (ReapplyState blk)
reapplyUntilTimeout reapplyTimeout reapplyPerStep resolveValues cfg slot baseLedgerStDiff txs =
  iterateUntilOrTimeout_
    reapplyTimeout
    (null . remainingTxs)
    (reapplyStep reapplyPerStep resolveValues cfg slot)
    (initReapplyState baseLedgerStDiff txs)

-- | One iteration of the reapplication loop.
reapplyStep ::
  forall blk m.
  (LedgerSupportsMempool blk, HasTxId (GenTx blk), IOLike m) =>
  Int ->
  (LedgerTables blk KeysMK -> m (LedgerTables blk ValuesMK)) ->
  LedgerConfig blk ->
  SlotNo ->
  ReapplyState blk ->
  m (ReapplyState blk)
reapplyStep reapplyPerStep resolveValues cfg slot st@ReapplyState{..} = do
  let (!txsToApplyStep, !remainingTxsStep) = txSeqTake reapplyPerStep remainingTxs

  let LedgerTables (ValuesMK currValues) = projectLedgerTables currentLedgerSt
      !inputsStep = boundaryInputsForTxs txsToApplyStep
      !inputsToResolveStep =
        inputsStep
          `Set.difference` phantomUTxOs
          `Set.difference` Map.keysSet currValues

  LedgerTables (ValuesMK inputsResolvedStep) <-
    resolveValues (LedgerTables (KeysMK inputsToResolveStep))

  let !allValues = LedgerTables (ValuesMK (currValues `Map.union` inputsResolvedStep))
      !stForStep =
        st
          { remainingTxs = remainingTxsStep
          , currentLedgerSt = currentLedgerSt `withLedgerTables` allValues
          , resolvedUTxOs = resolvedUTxOs `Map.union` inputsResolvedStep
          }

  return $! reapplyTxs cfg slot txsToApplyStep stForStep

reapplyTxs ::
  forall blk.
  (LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  LedgerConfig blk ->
  SlotNo ->
  TxSeq.TxSeq (MempoolMeasure blk) (ValidatedTxWithDiffs blk) ->
  ReapplyState blk ->
  ReapplyState blk
reapplyTxs cfg slot txs stBefore =
  let
    txTickets = TxSeq.toList txs
    input =
      [ (validatedTx . TxSeq.txTicketTx $ tkt, (), tkt)
      | tkt <- txTickets
      ]
    ReapplyTxsResult{invalidatedTxs, validatedTxs, resultingState} =
      LS.reapplyTxs @blk @Discard cfg slot input (currentLedgerSt stBefore)
    !validTxIds =
      Set.fromList [txId (txForgetValidated vtx) | (vtx, _, _) <- validatedTxs]
    !newPhantomUTxOs =
      Foldable.foldl'
        ( \acc tkt ->
            let txwd = TxSeq.txTicketTx tkt
             in if Set.member (txId (txForgetValidated (validatedTx txwd))) validTxIds
                  then acc
                  else acc `Set.union` Map.keysSet (createdFromTx txwd)
        )
        Set.empty
        txTickets
   in
    stBefore
      { currentLedgerSt = resultingState
      , unapplicableTxs = unapplicableTxs stBefore <> StrictSeq.fromList invalidatedTxs
      , appliedTxs = appliedTxs stBefore <> TxSeq.fromList [tkt | (_, _, tkt) <- validatedTxs]
      , phantomUTxOs = phantomUTxOs stBefore `Set.union` newPhantomUTxOs
      }

-- * Helpers

txSeqTake ::
  Measure sz =>
  Int ->
  TxSeq.TxSeq sz tx ->
  (TxSeq.TxSeq sz tx, TxSeq.TxSeq sz tx)
txSeqTake = go TxSeq.Empty
 where
  go acc 0 rest = (acc, rest)
  go acc _ TxSeq.Empty = (acc, TxSeq.Empty)
  go acc n (t TxSeq.:< rest) = go (acc TxSeq.:> t) (n - 1) rest

iterateUntilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM p f a
  | p a = return a
  | otherwise = f a >>= iterateUntilM p f

createdFromTx ::
  forall blk.
  LedgerSupportsMempool blk =>
  ValidatedTxWithDiffs blk ->
  Map (TxIn blk) (TxOut blk)
createdFromTx = snd . Diff.deletedAndCreated . diffFromTx

diffFromTx ::
  forall blk.
  ValidatedTxWithDiffs blk -> Diff.Diff (TxIn blk) (TxOut blk)
diffFromTx = getDiffMK . getLedgerTables . validatedTxDiffs

inputsFromTx ::
  forall blk. LedgerSupportsMempool blk => ValidatedTxWithDiffs blk -> Set (TxIn blk)
inputsFromTx = getKeysMK . getLedgerTables . getTransactionKeySets . txForgetValidated . validatedTx

cdFromLedgerState ::
  forall blk.
  LedgerSupportsMempool blk =>
  TickedLedgerState blk DiffMK ->
  (Map (TxIn blk) (TxOut blk), Set (TxIn blk))
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
  TxSeq.TxSeq (MempoolMeasure blk) (ValidatedTxWithDiffs blk) ->
  Set (TxIn blk)
boundaryInputsForTxs txs =
  let (!boundaryInputs, _) =
        Foldable.foldl'
          boundaryInputsForTxsStep
          (Set.empty, mempty)
          [(inputsFromTx tx, diffFromTx tx) | tx <- Foldable.toList txs]
   in boundaryInputs

boundaryInputsForTxsStep ::
  Ord k =>
  (Set k, Diff.Diff k v) ->
  (Set k, Diff.Diff k v) ->
  (Set k, Diff.Diff k v)
boundaryInputsForTxsStep
  (!boundaryInputs, !batchDiff)
  (!txIns, !txDiff) =
    let
      !newBoundaryInputs = txIns `Set.difference` Diff.keysSet batchDiff
     in
      ( boundaryInputs `Set.union` newBoundaryInputs
      , batchDiff <> txDiff
      )
