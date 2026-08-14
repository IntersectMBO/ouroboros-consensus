{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- | Queries to the mempool
module Ouroboros.Consensus.Mempool.Query
  ( implGetSnapshotFor
  , implGetSnapshotForNoCache
  ) where

import Control.Monad.Except (runExcept)
import Data.Foldable (Foldable (foldMap', foldl'))
import qualified Data.Set as Set
import LeiosUtils.TimeBoundedLoop (iterateUntilOrTimeout')
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.Tables.Utils (emptyLedgerTables, restrictValues', unionValues)
import Ouroboros.Consensus.Mempool.API
import Ouroboros.Consensus.Mempool.Impl.Common
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import Ouroboros.Consensus.Util.IOLike

-- | Whether the mempool snapshot may be served from the cached internal or needs re-computation.
--
-- 'UseCache' is the normal case:

-- — a state rebased onto a to-be-certified EB's closure so a freshly-announced
-- EB only carries txs valid /after/ that EB is applied
-- (input-output-hk/ouroboros-leios#838). There the cache must be bypassed, or
-- we would return the stale pre-rebase snapshot and silently drop the rebase.
data SnapshotCachePolicy
  = -- | same tip hash => same ledger state, so the cached snapshot is reusable.
    UseCache
  | -- | for the Leios EB certifiation+announcement path, where the tip and
    -- slot coincide with the cached mempool state but the ledger state differs.
    AlwaysRevalidate

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
  (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  m (MempoolSnapshot blk)
implGetSnapshotFor = getSnapshotUsingPolicyFor UseCache

-- | Like 'implGetSnapshotFor', but never short-circuits to the cached
-- internal snapshot: it always revalidates the mempool against the supplied
-- ledger state. See 'AlwaysRevalidate'.
--
-- Values are read at the unticked parent, so the caller's @ticked@ diffs
-- (closure ⊕ tick) must be relative to that same state.
implGetSnapshotForNoCache ::
  ( IOLike m
  , LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  ) =>
  MempoolEnv m blk ->
  -- | Get snapshot for this slot number (usually the current slot)
  SlotNo ->
  -- | The ledger state at which we want the snapshot, ticked to @slot@.
  TickedLedgerState blk DiffMK ->
  -- | A function that returns values corresponding to the given keys for
  -- the unticked ledger state.
  (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  m (MempoolSnapshot blk)
implGetSnapshotForNoCache = getSnapshotUsingPolicyFor AlwaysRevalidate

-- | Shared implementation of 'implGetSnapshotFor' and 'implGetSnapshotForNoCache'.
getSnapshotUsingPolicyFor ::
  ( IOLike m
  , LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  ) =>
  SnapshotCachePolicy ->
  MempoolEnv m blk ->
  SlotNo ->
  TickedLedgerState blk DiffMK ->
  (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  m (MempoolSnapshot blk)
getSnapshotUsingPolicyFor policy mpEnv slot ticked readUntickedTables = do
  is <- atomically $ readTMVar istate
  -- Whether we may trust the cached tables/snapshot: only when the tip
  -- coincides /and/ the policy permits it (the rebase path forbids it even
  -- though the tip coincides, since its ledger tables differ).
  let canUseCache = case policy of
        UseCache -> pointHash (isTip is) == castHash (getTipHash ticked)
        AlwaysRevalidate -> False
  if canUseCache && isSlotNo is == slot
    then
      -- We are looking for a snapshot exactly for the ledger state we already
      -- have cached, then just return it.
      pure $ snapshotFromIS is
    else do
      resolveValues <-
        return $
          if canUseCache
            -- We are looking for a snapshot at the same state ticked
            -- to a different slot, so we can reuse the cached values
            then mkResolveValues $ Left (isTxValues is)
            -- We are looking for a snapshot at a different state, so we
            -- need to read the values from the ledgerdb.
            else
              mkResolveValues $
                Right
                  readUntickedTables
      computeSnapshot
        resolveValues
        cfg
        slot
        ticked
        (isTxs is)
 where
  MempoolEnv
    { mpEnvStateVar = istate
    , mpEnvLedgerCfg = cfg
    } = mpEnv

computeSnapshot ::
  forall blk m.
  (IOLike m, LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  LedgerConfig blk ->
  SlotNo ->
  TickedLedgerState blk DiffMK ->
  TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk) ->
  m (MempoolSnapshot blk)
computeSnapshot resolveValues cfg slot tickedStDiff txTickets = do
  let !tickedSt = tickedStDiff `withLedgerTables` emptyLedgerTables

  (_, _, _, !applied, !txIds) <-
    iterateUntilOrTimeout'
      0.1
      (\(_, txsToApply, _, _, _) -> null txsToApply)
      (snapshotStep resolveValues cfg slot tickedStDiff)
      (tickedSt, txTickets, [], TxSeq.Empty, Set.empty)

  let tip = castPoint $ getTip tickedStDiff
  return $ snapshot slot tip txIds applied

type SnapshotStepState blk =
  ( TickedLedgerState blk ValuesMK
  , TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk)
  , [Invalidated blk]
  , TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk)
  , Set.Set (GenTxId blk)
  )

snapshotStep ::
  forall blk m.
  (LedgerSupportsMempool blk, HasTxId (GenTx blk), IOLike m) =>
  (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  LedgerConfig blk ->
  SlotNo ->
  TickedLedgerState blk DiffMK ->
  SnapshotStepState blk ->
  m (SnapshotStepState blk)
snapshotStep resolveValues cfg slot tickedStDiffBase (!tickedSt, !txsToApply, !unapplicable, !applied, !appliedTxIds) = do
  let (!tickets, !txsToApply') = TxSeq.take 100 txsToApply
      !inputKeys =
        foldMap'
          (\(!tkt) -> getTransactionKeySets . txForgetValidated . validatedTx . TxSeq.txTicketTx $ tkt)
          tickets

  -- TODO(bladyjoker): We're fetching inputsKeys many times, introduce a cache
  -- TODO(bladyjoker): We don't actually need to get values, we just need to check whether the keys are still there. Much cheaper.
  !inputValues <- resolveValues inputKeys

  let !tickedSt' =
        tickedSt
          `withLedgerTables` ltliftA2
            unionValues
            (projectLedgerTables tickedSt)
            (projectLedgerTables (applyMempoolDiffs inputValues inputKeys tickedStDiffBase))

      (!tickedSt'', !unapplicable', !applied', !appliedTxIds') =
        reapplyTxs' cfg slot tickets tickedSt' applied
  _ <- evaluate $ projectLedgerTables tickedSt''
  return
    ( tickedSt''
    , txsToApply'
    , unapplicable <> reverse unapplicable'
    , applied'
    , appliedTxIds `Set.union` appliedTxIds'
    )

type ReApplyState blk =
  ( TickedLedgerState blk ValuesMK
  , [Invalidated blk]
  , TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk)
  , Set.Set (GenTxId blk)
  )

reapplyTxs' ::
  (LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  LedgerConfig blk ->
  SlotNo ->
  [TxSeq.TxTicket (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk)] ->
  TickedLedgerState blk ValuesMK ->
  TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk) ->
  ReApplyState blk
reapplyTxs' cfg slot toApplyTickets tickedSt applied0 =
  foldl'
    ( \(tickedSt', unapplicable, applied, txIds) !ticket ->
        let tx = validatedTx (TxSeq.txTicketTx ticket)
         in case runExcept (reapplyTx cfg slot tx tickedSt') of
              Left err -> (tickedSt', Invalidated tx err : unapplicable, applied, txIds)
              Right !tickedSt'' ->
                let !applied' = applied TxSeq.:> ticket
                    !txIds' = Set.insert (txId (txForgetValidated tx)) txIds
                 in (tickedSt'', unapplicable, applied', txIds')
    )
    (tickedSt, [], applied0, Set.empty)
    toApplyTickets

mkResolveValues ::
  (Monad m, LedgerSupportsMempool blk) =>
  Either
    (LedgerTables (LedgerState blk) ValuesMK)
    (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  LedgerTables (LedgerState blk) KeysMK ->
  m (LedgerTables (LedgerState blk) ValuesMK)
mkResolveValues (Left cachedValues) keys = return $ restrictValues' cachedValues keys
mkResolveValues (Right readTables) keys = readTables keys
