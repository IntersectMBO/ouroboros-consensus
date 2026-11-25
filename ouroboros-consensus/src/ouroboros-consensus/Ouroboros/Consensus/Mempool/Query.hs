{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Queries to the mempool
module Ouroboros.Consensus.Mempool.Query
  ( implGetSnapshotFor

    -- * Time-capped reapplication
  , revalidateUntilTimeout
  ) where

import Control.Tracer (traceWith)
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.Tables.Utils (restrictValues')
import Ouroboros.Consensus.Mempool.API
import Ouroboros.Consensus.Mempool.Impl.Common
import Ouroboros.Consensus.Mempool.TxSeq (TxTicket)
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.TimeBoundedLoop
  ( DoneType (..)
  , StepCount
  , iterateUntilOrTimeout
  )

-- | Get a snapshot of the mempool's transactions reapplied against the given
-- ticked ledger state.
--
-- NOTE: the reapplication is time-capped (see 'snapshotTimeLimitDefault' and
-- 'mempoolTimeoutCapacity'), so when the budget runs out before every
-- transaction is reapplied, the snapshot contains only a /prefix/ of the
-- mempool's transactions.
-- TODO(bladyjoker): Consider configuring timeouts at call sites and passing as argument.
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
  (LedgerTables blk KeysMK -> m (LedgerTables blk ValuesMK)) ->
  m (MempoolSnapshot blk)
implGetSnapshotFor mpEnv@MempoolEnv{mpEnvCapacityOverride, mpEnvLedgerCfg, mpEnvTracer, mpEnvStateVar, mpEnvTimeoutConfig} slot tickedLedgerSt readTables = do
  start <- getMonotonicTime
  (snapshot, cachedOrComputed) <- computeSnapshot
  end <- getMonotonicTime

  traceWith mpEnvTracer $
    TraceGetSnapshotFor
      snapshotTimeLimit
      slot
      (castHash (getTipHash tickedLedgerSt))
      (end `diffTime` start)
      cachedOrComputed

  return snapshot
 where
  computeSnapshot = do
    is@IS{isTip, isSlotNo, isTxValues, isLastTicketNo} <-
      atomically $ readTMVar mpEnvStateVar
    let sameTip = pointHash isTip == castHash (getTipHash tickedLedgerSt)
    if sameTip && isSlotNo == slot
      then
        -- We are looking for a snapshot exactly for the ledger state we
        -- already have cached, then just return it.
        pure $ (snapshotFromIS is, Nothing)
      else do
        let resolveValues
              | sameTip =
                  -- We are looking for a snapshot at the same state ticked to
                  -- a different slot: every key the mempool's transactions
                  -- need is already cached in 'isTxValues', so just restrict
                  -- the cache to the requested keys.
                  pure . restrictValues' isTxValues
              | otherwise =
                  -- We are looking for a snapshot at a different state, so we
                  -- need to read the values from the LedgerDB.
                  readTables

        let initRevalidateTxsResult =
              RevalidateTxsResult
                (initInternalState mpEnvCapacityOverride isLastTicketNo mpEnvLedgerCfg slot tickedLedgerSt)
                []
        (finalRevalidateTxsResult, stepCount, doneType) <-
          revalidateUntilTimeout
            mpEnv
            snapshotTimeLimit
            snapshotTxsPerStep
            resolveValues
            initRevalidateTxsResult
            (TxSeq.toList (isTxs is))

        pure $ (snapshotFromIS . newInternalState $ finalRevalidateTxsResult, Just (stepCount, doneType))

  snapshotTimeLimit = case mpEnvTimeoutConfig of
    Nothing -> snapshotTimeLimitDefault
    Just mempoolTimeoutCfg -> max snapshotTimeLimitDefault (mempoolTimeoutCapacity mempoolTimeoutCfg / 10)

-- | How many transactions to reapply in each step between wall-clock checks.
--
-- The time limit is only checked between steps and an in-flight step is never
-- cancelled, so the limit can be overshot by at most one step's duration: the
-- step size bounds the worst-case overshoot. At an assumed 50-100µs per
-- reapplied transaction, 100 transactions keep the maximum overshoot around
-- 5-10ms, an order of magnitude below the smallest time limit
-- ('snapshotTimeLimitDefault').
--
-- The step cannot be made arbitrarily small either: each step pays fixed
-- costs, dominated by one batched LedgerDB read of the step's inputs, which
-- are amortised over the step's transactions. Fewer transactions per step
-- would spend more of the time limit on per-step overhead and fewer
-- transactions would make it into the snapshot.
snapshotTxsPerStep :: Int
snapshotTxsPerStep = 100

-- | The time limit floor for building a snapshot. Used when no 'MempoolTimeoutConfig'
-- was given (e.g. db-analyser and ThreadNet) or it's configured to a smaller value.
snapshotTimeLimitDefault :: DiffTime
snapshotTimeLimitDefault = 0.1

-- | Reapply the given transactions against the given ticked ledger state,
-- stopping early once the time limit is exceeded.
--
-- Returns the @RevalidateTxsResult blk@ built so far, and whether the loop stopped
-- because all transactions were processed ('EndReached') or because the time limit
-- ran out ('TimeoutReached').
revalidateUntilTimeout ::
  forall blk m.
  (IOLike m, LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  MempoolEnv m blk ->
  -- | Time limit.
  DiffTime ->
  -- | Transactions reapplied per step, bounding the time limit overshoot.
  Int ->
  -- | Read the values for the given keys at the (unticked) base ledger state.
  (LedgerTables blk KeysMK -> m (LedgerTables blk ValuesMK)) ->
  -- | Revalidate state to step.
  RevalidateTxsResult blk ->
  -- | The transactions to reapply, in ticket order.
  [TxTicket (MempoolMeasure blk) (ValidatedTxWithDiffs blk)] ->
  m
    ( RevalidateTxsResult blk
    , StepCount
    , DoneType
    )
revalidateUntilTimeout MempoolEnv{mpEnvLedgerCfg, mpEnvCapacityOverride} budget txsPerStep readTables revalidateTxsResultBefore txTickets = do
  ((revalidateTxsResultAfter, _remaining), stepCount, done) <-
    iterateUntilOrTimeout
      budget
      (\(_, remaining) -> null remaining)
      step
      (revalidateTxsResultBefore, txTickets)
  pure (revalidateTxsResultAfter, stepCount, done)
 where
  step ::
    (RevalidateTxsResult blk, [TxTicket (MempoolMeasure blk) (ValidatedTxWithDiffs blk)]) ->
    m (RevalidateTxsResult blk, [TxTicket (MempoolMeasure blk) (ValidatedTxWithDiffs blk)])
  step (rtrBefore, remaining) = do
    let (txsForStep, remaining') = splitAt txsPerStep remaining
        IS{isSlotNo, isLastTicketNo} = newInternalState rtrBefore
    rtrEnd' <-
      revalidateTxsFor'
        readTables
        mpEnvCapacityOverride
        mpEnvLedgerCfg
        isSlotNo
        rtrBefore
        isLastTicketNo
        txsForStep
    pure (rtrEnd', remaining')
