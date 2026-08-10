{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Queries to the mempool
module Ouroboros.Consensus.Mempool.Query
  ( implGetSnapshotFor
  , implGetSnapshotForNoCache
  ) where

import Control.Monad.Except (runExcept)
import Data.Foldable (Foldable (foldMap', foldl'))
import qualified Data.Set as Set
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.Tables.Utils
  ( emptyLedgerTables
  , restrictValues'
  , unionValues
  )
import Ouroboros.Consensus.Mempool.API
import Ouroboros.Consensus.Mempool.Capacity (MempoolCapacityBytesOverride, computeMempoolCapacity)
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
      let resolveValues =
            if canUseCache
              -- We are looking for a snapshot at the same state ticked
              -- to a different slot, so we can reuse the cached values
              then mkResolveValues $ Left (isTxValues is)
              -- We are looking for a snapshot at a different state, so we
              -- need to read the values from the ledgerdb.
              else mkResolveValues $ Right readUntickedTables

      computeSnapshot2
        resolveValues
        capacityOverride
        cfg
        slot
        ticked
        (isLastTicketNo is)
        (TxSeq.toList $ isTxs is)
 where
  MempoolEnv
    { mpEnvStateVar = istate
    , mpEnvLedgerCfg = cfg
    , mpEnvCapacityOverride = capacityOverride
    } = mpEnv

mkResolveValues ::
  (Monad m, LedgerSupportsMempool blk) =>
  Either
    (LedgerTables (LedgerState blk) ValuesMK)
    (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  LedgerTables (LedgerState blk) KeysMK ->
  m (LedgerTables (LedgerState blk) ValuesMK)
mkResolveValues (Left cachedValues) keys = return $ restrictValues' cachedValues keys
mkResolveValues (Right readTables) keys = readTables keys

computeSnapshot2 ::
  forall blk m.
  (IOLike m, LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  MempoolCapacityBytesOverride ->
  LedgerConfig blk ->
  SlotNo ->
  TickedLedgerState blk DiffMK ->
  TicketNo ->
  [TxSeq.TxTicket (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk)] ->
  m (MempoolSnapshot blk)
computeSnapshot2 resolveValues capacityOverride cfg slot tickedStDiff lastTicketNo txTickets = do
  let tickedSt = tickedStDiff `withLedgerTables` emptyLedgerTables
  ((tickedSt', _, _, applied), _) <-
    callWithinBudget
      0.1
      (tickedSt, txTickets, [], [])
      (computeSnapshot3 resolveValues cfg slot tickedStDiff)
  return $
    snapshotFromIS $
      IS
        { isTxs = TxSeq.fromList applied
        , isTxIds = Set.fromList $ map (txId . txForgetValidated . validatedTx . TxSeq.txTicketTx) applied
        , isTxKeys = emptyLedgerTables
        , isTxValues = emptyLedgerTables
        , isLedgerState = tickedSt' `withLedgerTables` emptyLedgerTables
        , isTip = castPoint $ getTip tickedStDiff
        , isSlotNo = slot
        , isLastTicketNo = lastTicketNo
        , isCapacity = computeMempoolCapacity cfg tickedSt' capacityOverride
        }

type Y blk =
  ( TickedLedgerState blk ValuesMK
  , [TxSeq.TxTicket (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk)]
  , [Invalidated blk]
  , [TxSeq.TxTicket (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk)]
  )

computeSnapshot3 ::
  forall blk m.
  (LedgerSupportsMempool blk, Monad m) =>
  (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  LedgerConfig blk ->
  SlotNo ->
  TickedLedgerState blk DiffMK ->
  Y blk ->
  m (Bool, Y blk)
computeSnapshot3 _ _ _ _ (tickedSt, [], unapplicable, applied) = return (True, (tickedSt, [], unapplicable, applied))
computeSnapshot3 resolveValues cfg slot tickedStDiffBase (tickedSt, txsToApply, unapplicable, applied) = do
  let (tickets, txsToApply') = splitAt 100 txsToApply
      inputKeys = foldMap' (getTransactionKeySets . txForgetValidated . validatedTx . TxSeq.txTicketTx) tickets

  inputValues <- resolveValues inputKeys

  let !tickedSt' =
        tickedSt
          `withLedgerTables` ltliftA2
            unionValues
            (projectLedgerTables tickedSt)
            (projectLedgerTables (applyMempoolDiffs inputValues inputKeys tickedStDiffBase))

      (!tickedSt'', !unapplicable', !applied') = reapplyTxs2 cfg slot tickets tickedSt'

  return
    ( False
    , (tickedSt'', txsToApply', unapplicable <> reverse unapplicable', applied <> reverse applied')
    )

type X blk =
  ( TickedLedgerState blk ValuesMK
  , [Invalidated blk]
  , [TxSeq.TxTicket (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk)]
  )

reapplyTxs2 ::
  LedgerSupportsMempool blk =>
  LedgerConfig blk ->
  SlotNo ->
  [TxSeq.TxTicket (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk)] ->
  TickedLedgerState blk ValuesMK ->
  X blk
reapplyTxs2 cfg slot toApplyTickets tickedSt =
  foldl'
    ( \(tickedSt', unapplicable, applied) ticket ->
        let tx = validatedTx (TxSeq.txTicketTx ticket)
         in case runExcept (reapplyTx cfg slot tx tickedSt') of
              Left err -> (tickedSt', Invalidated tx err : unapplicable, applied)
              Right !tickedSt'' -> (tickedSt'', unapplicable, ticket : applied)
    )
    (tickedSt, [], [])
    toApplyTickets

data CallDoneType = TimerDone | StepperDone

callWithinBudget ::
  IOLike m => DiffTime -> a -> (a -> m (Bool, a)) -> m (a, CallDoneType)
callWithinBudget delay initSt stepAction = do
  stVar <- uncheckedNewTVarM initSt

  raceRes <- race (threadDelay delay) (goStepper stVar)

  lastSt <- atomically $ readTVar stVar
  return (lastSt, either (const TimerDone) (const StepperDone) raceRes)
 where
  goStepper stVar = do
    stVal <- atomically $ readTVar stVar
    (isDone, stVal') <- stepAction stVal
    atomically $ writeTVar stVar stVal'
    if isDone then return () else goStepper stVar
