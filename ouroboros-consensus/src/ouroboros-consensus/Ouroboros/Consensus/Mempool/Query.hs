{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Queries to the mempool
module Ouroboros.Consensus.Mempool.Query
  ( implGetSnapshotFor
  , implGetSnapshotForNoCache
  ) where

import Control.Monad.Except (runExcept)
import qualified Data.Foldable as Foldable
import Data.Sequence.Strict (StrictSeq, (|>))
import qualified Data.Set as Set
import LeiosUtils.TimeBoundedLoop (iterateUntilOrTimeout_)
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
            then pure . restrictValues' (isTxValues is)
            -- We are looking for a snapshot at a different state, so we
            -- need to read the values from the ledgerdb.
            else readUntickedTables
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

snapshotStepTimeLimitSeconds :: DiffTime
snapshotStepTimeLimitSeconds = 0.1

snapshotStepTxsPerStep :: Int
snapshotStepTxsPerStep = 100

computeSnapshot ::
  forall blk m.
  (IOLike m, LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  LedgerConfig blk ->
  SlotNo ->
  TickedLedgerState blk DiffMK ->
  TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk) ->
  m (MempoolSnapshot blk)
computeSnapshot resolveValues cfg slot baseLedgerStDiff txsToApply = do
  let startingLedgerSt = baseLedgerStDiff `withLedgerTables` emptyLedgerTables

  SnapshotStepState{..} <-
    iterateUntilOrTimeout_
      snapshotStepTimeLimitSeconds
      (null . remainingTxs)
      (snapshotStep resolveValues cfg slot baseLedgerStDiff)
      (SnapshotStepState startingLedgerSt txsToApply mempty TxSeq.Empty Set.empty)

  let tip = castPoint $ getTip baseLedgerStDiff
  return $ snapshot slot tip appliedTxIds appliedTxs

-- | Accumulator type threaded through each 'snapshotStep' call by 'computeSnapshot'.
data SnapshotStepState blk = SnapshotStepState
  { currentLedgerSt :: !(TickedLedgerState blk ValuesMK)
  -- ^ Ticked ledger state reflecting the cumulative effect of all applied
  -- transactions so far ('appliedTxs').
  , remainingTxs :: !(TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk))
  -- ^ Remaining transactions to be applied. Starts as the full
  -- mempool sequence and is consumed 'snapshotStepTxsPerStep' at a time.
  -- Iteration terminates when this becomes empty.
  , unapplicableTxs :: !(StrictSeq (Invalidated blk))
  -- ^ Transactions that failed reapplication, accumulated in original mempool
  -- order.
  , appliedTxs :: !(TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk))
  -- ^ Transactions successfully applied so far to some base Ledger state that results in 'currentLedgerSt', in original mempool order.
  -- Becomes the transaction sequence of the final 'MempoolSnapshot'.
  , appliedTxIds :: !(Set.Set (GenTxId blk))
  -- ^ Set of all accepted transactions (view on 'appliedTxs').
  }

snapshotStep ::
  forall blk m.
  (LedgerSupportsMempool blk, HasTxId (GenTx blk), IOLike m) =>
  (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  LedgerConfig blk ->
  SlotNo ->
  TickedLedgerState blk DiffMK ->
  SnapshotStepState blk ->
  m (SnapshotStepState blk)
snapshotStep resolveValues cfg slot baseLedgerStDiff st@SnapshotStepState{..} = do
  let (!txsToApplyStep, !remainingTxsStep) = TxSeq.take snapshotStepTxsPerStep remainingTxs
      !inputKeysStep =
        Foldable.foldMap'
          (getTransactionKeySets . txForgetValidated . validatedTx)
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
  !inputValuesStepForKeys <- resolveValues inputKeysStep

  let
    -- TODO(bladyjoker): Please review. The idea is to construct the LedgerState with values that are necessary for applying the transactions in this step.
    -- The `applyMempoolDiffs` uses the base LedgerState that contains diffs (I suspect from ticking? What values are affected by ticking?)
    -- and builds up a LedgerState with values for this step only.
    -- The `currentLedgerSt` starts empty so it begins with only the values in `baseLedgerSt`, after that because of the union the `currentLedgerSt` entries override whatever is conflicting in `baseLedgerSt`.
    !baseLedgerSt = applyMempoolDiffs inputValuesStepForKeys inputKeysStep baseLedgerStDiff
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
  SnapshotStepState blk ->
  SnapshotStepState blk
reapplyTxs' cfg slot toApplyTxs stBefore =
  Foldable.foldl'
    ( \st !tkt ->
        let tx = validatedTx . TxSeq.txTicketTx $ tkt
         in case runExcept (reapplyTx cfg slot tx (currentLedgerSt st)) of
              Left !err ->
                st
                  { unapplicableTxs = unapplicableTxs st |> Invalidated tx err
                  }
              Right !ledgerStAfterTx ->
                st
                  { currentLedgerSt = ledgerStAfterTx
                  , appliedTxs = appliedTxs st TxSeq.:> tkt
                  , appliedTxIds = Set.insert (txId (txForgetValidated tx)) (appliedTxIds st)
                  }
    )
    stBefore
    (TxSeq.toList toApplyTxs)
