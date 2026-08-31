{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Ledger reapplication on transaction sequence
module Ouroboros.Consensus.Mempool.Ledger
  ( reapplyUntilTimeout
  , ReapplyStepState (..)
  , propUTxOsIsDisjoint
  , reapply
  ) where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExcept)
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
import Ouroboros.Consensus.Ledger.Tables.Utils (emptyLedgerTables, unionValues)
import Ouroboros.Consensus.Mempool.API
import Ouroboros.Consensus.Mempool.Impl.Common
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import Ouroboros.Consensus.Util.IOLike

-- | Accumulator type threaded through each 'reapplyStep' call by top-level 'reapply'.
data ReapplyStepState blk = ReapplyStepState
  { currentLedgerSt :: !(TickedLedgerState blk ValuesMK)
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
  , inUTxOs :: !(Set (TxIn (LedgerState blk)))
  -- ^ Set of boundary UTxO input keys
  , outUTxOs :: !(Map (TxIn (LedgerState blk)) (TxOut (LedgerState blk)))
  -- ^ Map of boundary UTxO output keys
  , unapplicableTxsUTxOs :: !(Set (TxIn (LedgerState blk)))
  -- ^ Set of UTxOs created by 'unapplicableTxs'
  }

reapply ::
  forall blk m.
  (IOLike m, LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  LedgerConfig blk ->
  SlotNo ->
  TickedLedgerState blk DiffMK ->
  TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk) ->
  m (ReapplyStepState blk)
reapply resolveValues cfg slot baseLedgerStDiff txsToApply = do
  let startingLedgerSt = baseLedgerStDiff `withLedgerTables` emptyLedgerTables

  iterateUntilM
    (null . remainingTxs)
    (reapplyStep (length txsToApply) resolveValues cfg slot baseLedgerStDiff)
    ( ReapplyStepState
        startingLedgerSt
        txsToApply
        mempty
        TxSeq.Empty
        Set.empty
        Set.empty
        Map.empty
        Set.empty
    )

iterateUntilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM p f a
  | p a = return a
  | otherwise = f a >>= iterateUntilM p f

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
reapplyUntilTimeout reapplyTimeout reapplyPerStep resolveValues cfg slot baseLedgerStDiff txsToApply = do
  let startingLedgerSt = baseLedgerStDiff `withLedgerTables` emptyLedgerTables

  iterateUntilOrTimeout_
    reapplyTimeout
    (null . remainingTxs)
    (reapplyStep reapplyPerStep resolveValues cfg slot baseLedgerStDiff)
    ( ReapplyStepState
        startingLedgerSt
        txsToApply
        mempty
        TxSeq.Empty
        Set.empty
        Set.empty
        Map.empty
        Set.empty
    )

reapplyStep ::
  forall blk m.
  (LedgerSupportsMempool blk, HasTxId (GenTx blk), IOLike m) =>
  Int ->
  (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  LedgerConfig blk ->
  SlotNo ->
  TickedLedgerState blk DiffMK ->
  ReapplyStepState blk ->
  m (ReapplyStepState blk)
reapplyStep reapplyPerStep resolveValues cfg slot baseLedgerStDiff st@ReapplyStepState{..} = do
  let (!txsToApplyStep, !remainingTxsStep) = TxSeq.take reapplyPerStep remainingTxs

  -- Resolve boundary input keys
  -- a) UTxOs that haven't been previously resolved
  -- b) UTxOs that weren't produced by unapplicable transactions
  --      -- omitted in 'inputValuesStep' so Ledger will report a proper "missing utxo" error
  let !(ins, _outs) = insAndOutsFromTxs (inUTxOs, outUTxOs) txsToApplyStep
      !unknownIns = (ins `Set.difference` inUTxOs) `Set.difference` unapplicableTxsUTxOs
      !inputKeysToResolve = LedgerTables (KeysMK unknownIns)

  !inputValuesResolvedForStep <- resolveValues inputKeysToResolve

  -- Prepare the ledger state with values
  -- TODO(bladyjoker): We're growing the 'inputValuesStep' and `currentLedgerSt` but that's unnecessary and might impact performance? Should we just provide the necessary UTxO closure?
  let
    !baseLedgerSt = applyMempoolDiffs inputValuesResolvedForStep inputKeysToResolve baseLedgerStDiff
    !inputValuesStep =
      ltliftA2
        unionValues
        (projectLedgerTables currentLedgerSt)
        (projectLedgerTables baseLedgerSt)
    !stForStep =
      st
        { currentLedgerSt = currentLedgerSt `withLedgerTables` inputValuesStep
        , remainingTxs = remainingTxsStep
        }

  return $! reapplyTxs' cfg slot txsToApplyStep stForStep

reapplyTxs' ::
  (LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  LedgerConfig blk ->
  SlotNo ->
  TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk) ->
  ReapplyStepState blk ->
  ReapplyStepState blk
reapplyTxs' cfg slot txs stBefore =
  Foldable.foldl'
    ( \st !tkt ->
        let tx = validatedTx . TxSeq.txTicketTx $ tkt
            txUTxOs = utxosFromTx $ TxSeq.txTicketTx tkt
            (inUTxOs', outUTxOs') = insAndOutsFromUTxOsStep (inUTxOs st, outUTxOs st) txUTxOs
         in case runExcept (reapplyTx cfg slot tx (currentLedgerSt st)) of
              Left !err ->
                st
                  { unapplicableTxs = unapplicableTxs st |> Invalidated tx err
                  , unapplicableTxsUTxOs =
                      unapplicableTxsUTxOs st `Set.union` (Map.keysSet . createdUTxOs $ txUTxOs)
                  }
              Right !ledgerStAfterTx ->
                st
                  { currentLedgerSt = ledgerStAfterTx
                  , appliedTxs = appliedTxs st TxSeq.:> tkt
                  , appliedTxIds = Set.insert (txId (txForgetValidated tx)) (appliedTxIds st)
                  , inUTxOs = inUTxOs'
                  , outUTxOs = outUTxOs'
                  }
    )
    stBefore
    (TxSeq.toList txs)

data UTxOs blk = UTxOs
  { createdUTxOs :: Map (TxIn (LedgerState blk)) (TxOut (LedgerState blk))
  , readUTxOs :: Set (TxIn (LedgerState blk))
  , deletedUTxOs :: Set (TxIn (LedgerState blk))
  }

propUTxOsIsDisjoint ::
  (LedgerSupportsMempool blk, Monad m) =>
  UTxOs blk -> ExceptT String m (UTxOs blk)
propUTxOsIsDisjoint utxos@(UTxOs cs rs ds) = do
  unless
    (rs `Set.disjoint` ds && rs `Set.disjoint` (Map.keysSet cs) && ds `Set.disjoint` (Map.keysSet cs))
    $ throwError "UTxO create, read and delete sets must be mutually disjoint"
  return utxos

utxosFromTx ::
  LedgerSupportsMempool blk =>
  ValidatedTxWithDiffs blk -> UTxOs blk
utxosFromTx tx =
  let
    LedgerTables (KeysMK allKeys) = getTransactionKeySets . txForgetValidated . validatedTx $ tx
    (deletedKeys, createdValues) = Diff.deletedAndCreated . getDiffMK . getLedgerTables . validatedTxDiffs $ tx
   in
    UTxOs
      { createdUTxOs = createdValues
      , readUTxOs = allKeys `Set.difference` deletedKeys
      , deletedUTxOs = deletedKeys
      }

insAndOutsFromUTxOss ::
  (LedgerSupportsMempool blk, Foldable t) =>
  (Set (TxIn (LedgerState blk)), Map (TxIn (LedgerState blk)) (TxOut (LedgerState blk))) ->
  t (UTxOs blk) ->
  (Set (TxIn (LedgerState blk)), Map (TxIn (LedgerState blk)) (TxOut (LedgerState blk)))
insAndOutsFromUTxOss ios utxos = Foldable.foldl' insAndOutsFromUTxOsStep ios utxos

insAndOutsFromUTxOsStep ::
  LedgerSupportsMempool blk =>
  (Set (TxIn (LedgerState blk)), Map (TxIn (LedgerState blk)) (TxOut (LedgerState blk))) ->
  UTxOs blk ->
  (Set (TxIn (LedgerState blk)), Map (TxIn (LedgerState blk)) (TxOut (LedgerState blk)))
insAndOutsFromUTxOsStep (ins, outs) (UTxOs cutxos rutxos dutxos) =
  let txIns = rutxos `Set.union` dutxos
      -- 1. Inputs required by this tx that were NOT output boundary keys produced by earlier txs
      unmatchedIns = txIns `Set.difference` Map.keysSet outs
      -- 2. Outputs deleted/spent by this tx
      remainingOuts = outs `Map.withoutKeys` dutxos
   in ( ins `Set.union` unmatchedIns
      , cutxos `Map.union` remainingOuts
      )

-- | `let insAndOuts' = insAndOutsFromTxs insAndOuts txs` computes the boundary (UTxO) input keys and (UTxO) output values for 'txs'.
-- Input (UTxO) keys are the ones that transactions in 'txs' requires but aren't created by any transactions in 'txs'.
-- Output (UTxO) values are the ones that transactions in 'txs' produces but aren't deleted by any transactions in 'txs'.
-- Examples:
-- `({A}, {C}) = insAndOutsFromTxs (mempty, mempty) [ <tx deletes A creates B>, <tx deletes B creates C>]`
-- `({A, C}, {B, D}) = insAndOutsFromTxs (mempty, mempty) [ <tx deletes A creates B>, <tx deletes C creates D>]`
-- `({A, C, E, G}, {B, F}) = insAndOutsFromTxs (mempty, mempty) [ <tx deletes A creates B reads C>, <tx deletes E creates F reads G>]`
insAndOutsFromTxs ::
  (LedgerSupportsMempool blk, Foldable t) =>
  (Set (TxIn (LedgerState blk)), Map (TxIn (LedgerState blk)) (TxOut (LedgerState blk))) ->
  t (ValidatedTxWithDiffs blk) ->
  (Set (TxIn (LedgerState blk)), Map (TxIn (LedgerState blk)) (TxOut (LedgerState blk)))
insAndOutsFromTxs ios txs = insAndOutsFromUTxOss ios (utxosFromTx <$> Foldable.toList txs)
