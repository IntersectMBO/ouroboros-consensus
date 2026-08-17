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
computeSnapshot resolveValues cfg slot tickedStDiff txsToApply = do
  let !tickedSt = tickedStDiff `withLedgerTables` emptyLedgerTables

  (_, _, _, !appliedTxs, !appliedTxIds) <-
    iterateUntilOrTimeout'
      0.1
      (\(_, txsToApply', _, _, _) -> null txsToApply')
      (snapshotStep resolveValues cfg slot tickedStDiff)
      (tickedSt, txsToApply, [], TxSeq.Empty, Set.empty)

  let !tip = castPoint $ getTip tickedStDiff
  return $ snapshot slot tip appliedTxIds appliedTxs

txsPerStep :: Int
txsPerStep = 100

type SnapshotStepState blk =
  ( TickedLedgerState blk ValuesMK -- ledger state to apply on
  , TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk) -- txs to apply
  , [Invalidated blk] -- unapplicable txs
  , TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk) -- applied txs
  , Set.Set (GenTxId blk) -- applied tx ids
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
snapshotStep resolveValues cfg slot tickedStDiffBase (!tickedSt, !txsToApply, !unapplicableTxs, !appliedTxs, !appliedTxIds) = do
  let (!txsToApplyStep, !txsToApplyRest) = TxSeq.take txsPerStep txsToApply
      !inputKeysStep =
        foldMap'
          (getTransactionKeySets . txForgetValidated . validatedTx . TxSeq.txTicketTx)
          txsToApplyStep

  -- TODO(bladyjoker): We're fetching inputsKeys many times, can we introduce a cache? Additionally, we don't actually need to get values,
  -- we just need to check whether the keys are still there? Should be much cheaper.
  -- Javier made an important point, we can for UTxOs but not for "updateable state variables" like Accounts and such. So doing this here is
  -- not future proof as it assumes UTxO like state variables.
  -- So there's opportunities here to minimize disk access significanly, but to do it in a future proof manner we need to introduce and manage
  -- distinction between updateable and non-updateable state variables.
  -- In other words, state variables can be: created, read, updated, deleted...CRUD
  -- However, some variables like UTxOs can only be: created, read and deleted...CRD
  -- Whereas, others like Accounts can be: created, read, updated and deleted...CRUD
  -- Perhaps, there're other types in Cardano?
  -- Sounds like working with this distinction could be an optimization point!
  !inputValuesStep <- resolveValues inputKeysStep

  let
    -- TODO(bladyjoker): Please review. The idea is to construct the LedgerState with values that are necessary for applying the transactions in this step.
    -- The `applyMempoolDiffs` uses the base LedgerState that contains diffs (I suspect from ticking? What values are affected by ticking?)
    -- and builds up a LedgerState with values for this step only.
    -- The lhs starts empty so it begins with only the rhs, after that because of the union the lhs entries override whatever is conflicting in rhs.
    !inputValuesStep' =
      ltliftA2
        unionValues
        (projectLedgerTables tickedSt)
        (projectLedgerTables (applyMempoolDiffs inputValuesStep inputKeysStep tickedStDiffBase))
    !tickedStBeforeStep = tickedSt `withLedgerTables` inputValuesStep'

    (!tickedStAfterStep, !unapplicableTxsStep, !appliedTxsStep) = reapplyTxs' cfg slot txsToApplyStep tickedStBeforeStep

  -- TODO(bladyjoker): Keep? _ <- evaluate $ projectLedgerTables tickedStAfterStep

  return
    ( tickedStAfterStep
    , txsToApplyRest
    , unapplicableTxs <> reverse unapplicableTxsStep
    , appliedTxs `TxSeq.append` appliedTxsStep
    , appliedTxIds
        `Set.union` ( Set.fromList $
                        txId . txForgetValidated . validatedTx . TxSeq.txTicketTx <$> TxSeq.toList appliedTxsStep
                    )
    )

reapplyTxs' ::
  LedgerSupportsMempool blk =>
  LedgerConfig blk ->
  SlotNo ->
  [TxSeq.TxTicket (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk)] ->
  TickedLedgerState blk ValuesMK ->
  ( TickedLedgerState blk ValuesMK
  , [Invalidated blk]
  , TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk)
  )
reapplyTxs' cfg slot toApplyTxs tickedSt =
  foldl'
    ( \(!tickedSt', !unapplicableTxs, !appliedTxs) !tkt ->
        let tx = validatedTx (TxSeq.txTicketTx tkt)
         in case runExcept (reapplyTx cfg slot tx tickedSt') of
              Left !err -> (tickedSt', Invalidated tx err : unapplicableTxs, appliedTxs)
              Right !tickedSt'' ->
                let !appliedTxs' = appliedTxs TxSeq.:> tkt
                 in (tickedSt'', unapplicableTxs, appliedTxs')
    )
    (tickedSt, [], TxSeq.Empty)
    toApplyTxs

mkResolveValues ::
  (Monad m, LedgerSupportsMempool blk) =>
  Either
    (LedgerTables (LedgerState blk) ValuesMK)
    (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  LedgerTables (LedgerState blk) KeysMK ->
  m (LedgerTables (LedgerState blk) ValuesMK)
mkResolveValues (Left cachedValues) keys = return $ restrictValues' cachedValues keys
mkResolveValues (Right readTables) keys = readTables keys
