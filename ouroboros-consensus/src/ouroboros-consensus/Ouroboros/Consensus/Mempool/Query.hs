{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Queries to the mempool
module Ouroboros.Consensus.Mempool.Query
  ( implGetSnapshotFor
  , implGetSnapshotForNoCache
  ) where

import Control.Monad (unless)
import Control.Monad.Class.MonadTimer.SI (MonadTimer (timeout))
import Control.Monad.Except (runExcept)
import Data.Foldable (Foldable (foldMap', foldl'))
import Data.Measure (Measure)
import qualified Data.Set as Set
import qualified Debug.Trace as Debug
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.Tables.Utils
  ( emptyLedgerTables
  , restrictValues'
  , unionValues
  )
import Ouroboros.Consensus.Mempool.API
import Ouroboros.Consensus.Mempool.Capacity (MempoolSize (..))
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
  , MonadTimer m
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
  , MonadTimer m
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
  , MonadTimer m
  ) =>
  SnapshotCachePolicy ->
  MempoolEnv m blk ->
  SlotNo ->
  TickedLedgerState blk DiffMK ->
  (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  m (MempoolSnapshot blk)
getSnapshotUsingPolicyFor policy mpEnv slot ticked readUntickedTables = do
  is <- raceWithError "read-tmvar" 0.1 (atomically $ readTMVar istate)
  -- Whether we may trust the cached tables/snapshot: only when the tip
  -- coincides /and/ the policy permits it (the rebase path forbids it even
  -- though the tip coincides, since its ledger tables differ).
  !canUseCache <- raceWithError "can-use-cache" 0.1 $ case policy of
    UseCache -> return $ pointHash (isTip is) == castHash (getTipHash ticked)
    AlwaysRevalidate -> return False

  if canUseCache && isSlotNo is == slot
    then
      -- We are looking for a snapshot exactly for the ledger state we already
      -- have cached, then just return it.
      pure $ snapshotFromIS is
    else do
      resolveValues <-
        raceWithError "comp-snap-2" 0.2 $
          return $
            if canUseCache
              -- We are looking for a snapshot at the same state ticked
              -- to a different slot, so we can reuse the cached values
              then mkResolveValues $ Left (isTxValues is)
              -- We are looking for a snapshot at a different state, so we
              -- need to read the values from the ledgerdb.
              else mkResolveValues $ Right readUntickedTables

      raceWithError "comp-snap-2-a" 0.7 $
        computeSnapshot2
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

raceWithError :: (MonadAsync m, MonadDelay m) => String -> DiffTime -> m b -> m b
raceWithError msg d act = do
  timeoutOrRes <- race (threadDelay d) act
  case timeoutOrRes of
    Left _err -> error msg
    Right res -> return res

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
  (IOLike m, LedgerSupportsMempool blk, HasTxId (GenTx blk), MonadTimer m) =>
  (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  LedgerConfig blk ->
  SlotNo ->
  TickedLedgerState blk DiffMK ->
  TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk) ->
  m (MempoolSnapshot blk)
computeSnapshot2 resolveValues cfg slot tickedStDiff txTickets = do
  !tickedSt <- raceWithError "a" 0.2 $ return $ tickedStDiff `withLedgerTables` emptyLedgerTables

  res <- raceWithError "cwb" 0.2 $ do
    iterateUntilOrTimeoutNaive2
      0.1
      (tickedSt, txTickets, [], TxSeq.Empty, Set.empty, mempty)
      (computeSnapshot3 resolveValues cfg slot tickedStDiff)

  raceWithError "return-comp" 0.2 $ do
    let ((_, _, _, applied, txIds, mempoolSize), _) = res
        tip = castPoint $ getTip tickedStDiff
        txsAfter n =
          [ (validatedTx a, b, forgetTxMeasureWithDiffTime c)
          | (a, b, c) <- TxSeq.toTuples (snd (TxSeq.splitAfterTicketNo applied n))
          ]
    return $
      MempoolSnapshot
        { snapshotTxs = txsAfter TxSeq.zeroTicketNo
        , snapshotTxsAfter = txsAfter
        , snapshotLookupTx = \n -> fmap validatedTx (TxSeq.lookupByTicketNo applied n)
        , snapshotHasTx = \tid -> Set.member tid txIds
        , snapshotMempoolSize = mempoolSize
        , snapshotSlotNo = slot
        , snapshotStateHash = pointHash tip
        , snapshotTake = \limit ->
            let (x, _) = TxSeq.splitAfterTxSize applied $ MkTxMeasureWithDiffTime limit InfiniteDiffTimeMeasure
             in (map (validatedTx . TxSeq.txTicketTx) (TxSeq.toList x), TxSeq.toSize x)
        , snapshotPoint = tip
        }

-- | Stepper state for 'computeSnapshot3': ledger state, remaining txs to
-- revalidate, invalidated txs, validated tx sequence (built incrementally),
-- and validated tx id set.
type Y blk =
  ( TickedLedgerState blk ValuesMK
  , TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk)
  , [Invalidated blk]
  , TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk)
  , Set.Set (GenTxId blk)
  , MempoolSize
  )

computeSnapshot3 ::
  forall blk m.
  (LedgerSupportsMempool blk, HasTxId (GenTx blk), IOLike m) =>
  (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
  LedgerConfig blk ->
  SlotNo ->
  TickedLedgerState blk DiffMK ->
  Y blk ->
  m (Bool, Y blk)
computeSnapshot3 _ _ _ _ (tickedSt, TxSeq.Empty, unapplicable, applied, txIds, mempoolSize) =
  return (True, (tickedSt, TxSeq.Empty, unapplicable, applied, txIds, mempoolSize))
computeSnapshot3 resolveValues cfg slot tickedStDiffBase (!tickedSt, !txsToApply, !unapplicable, !applied, !txIds, !mempoolSize) = do
  startX <- getMonotonicTime

  let (!tickets, !txsToApply') = txSeqTake 100 txsToApply
      inputKeys =
        foldMap'
          (\(!tkt) -> getTransactionKeySets . txForgetValidated . validatedTx . TxSeq.txTicketTx $ tkt)
          tickets

  -- TODO(bladyjoker): We're fetching inputsKeys many times, introduce a cache
  -- TODO(bladyjoker): We don't actually need to get values, we just need to check whether the keys are still there. Much cheaper.
  start <- getMonotonicTime
  !inputValues <- resolveValues inputKeys
  end <- getMonotonicTime
  _ <- Debug.trace ("resolveValues: " <> show (end `diffTime` start)) $ return ()

  startA <- getMonotonicTime
  let !tickedSt' =
        tickedSt
          `withLedgerTables` ltliftA2
            unionValues
            (projectLedgerTables tickedSt)
            (projectLedgerTables (applyMempoolDiffs inputValues inputKeys tickedStDiffBase))

      (!tickedSt'', !unapplicable', !applied', !txIds', !mempoolSize') =
        reapplyTxs2 cfg slot tickets tickedSt' applied txIds mempoolSize
  _ <- evaluate $ projectLedgerTables tickedSt''
  endA <- getMonotonicTime
  _ <- Debug.trace ("reapplyTxs: " <> show (endA `diffTime` startA)) $ return ()

  endX <- getMonotonicTime
  _ <- Debug.trace ("cs3-inner: " <> show (endX `diffTime` startX)) $ return ()
  return
    ( False
    ,
      ( tickedSt''
      , txsToApply'
      , unapplicable <> reverse unapplicable'
      , applied'
      , txIds'
      , mempoolSize'
      )
    )

-- | Fold state for 'reapplyTxs2': ledger state, invalidated txs,
-- validated tx sequence, and validated tx id set.
type X blk =
  ( TickedLedgerState blk ValuesMK
  , [Invalidated blk]
  , TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk)
  , Set.Set (GenTxId blk)
  , MempoolSize
  )

reapplyTxs2 ::
  (LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  LedgerConfig blk ->
  SlotNo ->
  [TxSeq.TxTicket (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk)] ->
  TickedLedgerState blk ValuesMK ->
  TxSeq.TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk) ->
  Set.Set (GenTxId blk) ->
  MempoolSize ->
  X blk
reapplyTxs2 cfg slot toApplyTickets tickedSt applied0 txIds0 mempoolSize0 =
  foldl'
    ( \(tickedSt', unapplicable, applied, txIds, mempoolSize) !ticket ->
        let tx = validatedTx (TxSeq.txTicketTx ticket)
         in case runExcept (reapplyTx cfg slot tx tickedSt') of
              Left err -> (tickedSt', Invalidated tx err : unapplicable, applied, txIds, mempoolSize)
              Right !tickedSt'' ->
                let !applied' = applied TxSeq.:> ticket
                    !txIds' = Set.insert (txId (txForgetValidated tx)) txIds
                    !txSz =
                      MempoolSize
                        { msNumTxs = 1
                        , msNumBytes = txMeasureByteSize $ forgetTxMeasureWithDiffTime $ TxSeq.txTicketSize ticket
                        }
                    !mempoolSize' = mempoolSize <> txSz
                 in (tickedSt'', unapplicable, applied', txIds', mempoolSize')
    )
    (tickedSt, [], applied0, txIds0, mempoolSize0)
    toApplyTickets

-- | Take up to @n@ tickets from the front of a 'TxSeq', returning them as a
-- list in original order alongside the remainder.
txSeqTake ::
  Measure sz =>
  Int ->
  TxSeq.TxSeq sz tx ->
  ([TxSeq.TxTicket sz tx], TxSeq.TxSeq sz tx)
txSeqTake = go []
 where
  go acc 0 rest = (reverse acc, rest)
  go acc _ TxSeq.Empty = (reverse acc, TxSeq.Empty)
  go acc n (t TxSeq.:< rest) = go (t : acc) (n - 1) rest

data CallDoneType = TimerDone | StepperDone

_iterateUntilOrTimeoutCorrect ::
  IOLike m => DiffTime -> a -> (a -> m (Bool, a)) -> m (a, CallDoneType)
_iterateUntilOrTimeoutCorrect delay initSt stepAction = do
  stVar <- uncheckedNewTVarM initSt
  expiredVar <- uncheckedNewTVarM False

  startR <- getMonotonicTime

  -- Timer sets the flag; goStepper checks it cooperatively at each step
  -- boundary. This avoids relying on async exception delivery, which blocks
  -- in throwTo until the target thread reaches a GHC safepoint — potentially
  -- hundreds of ms into a tight pure computation.
  _ <- withAsync (threadDelay delay >> atomically (writeTVar expiredVar True)) $ \_ ->
    goStepper expiredVar stVar

  endR <- getMonotonicTime
  lastSt <- atomically $ readTVar stVar
  expired <- atomically $ readTVar expiredVar
  let doneType = if expired then TimerDone else StepperDone

  _ <-
    Debug.trace
      ( case doneType of
          TimerDone -> "timer-done " <> show (endR `diffTime` startR)
          StepperDone -> "stepper-deon " <> show (endR `diffTime` startR)
      )
      (return ())
  return (lastSt, doneType)
 where
  goStepper expiredVar stVar = do
    expired <- atomically $ readTVar expiredVar
    unless expired $ do
      st <- atomically $ readTVar stVar
      start <- getMonotonicTime
      (isDone, st') <- stepAction st
      end <- getMonotonicTime
      _ <- Debug.trace ("stepper-act " <> show (end `diffTime` start)) $ return ()
      atomically $ writeTVar stVar st'
      unless isDone $ goStepper expiredVar stVar

iterateUntilOrTimeoutNaive ::
  IOLike m => DiffTime -> a -> (a -> m (Bool, a)) -> m (a, CallDoneType)
iterateUntilOrTimeoutNaive delay initSt stepAction = do
  stVar <- uncheckedNewTVarM initSt

  startR <- getMonotonicTime

  raceRes <-
    race
      (threadDelay delay)
      (goStepper stVar [])
  endR <- getMonotonicTime
  _ <-
    Debug.trace
      ( case raceRes of
          Left _ -> "timer-done " <> show (endR `diffTime` startR)
          Right steps -> "stepper-done " <> show (endR `diffTime` startR) <> " " <> show (length steps)
      )
      (return ())

  lastSt <- atomically $ readTVar stVar

  return (lastSt, either (const TimerDone) (const StepperDone) raceRes)
 where
  goStepper stVar acc = do
    stVal <- atomically $ readTVar stVar
    start <- getMonotonicTime
    (isDone, stVal') <- stepAction stVal
    end <- getMonotonicTime
    _ <- Debug.trace ("stepper-act " <> show (end `diffTime` start)) $ return ()
    atomically $ writeTVar stVar stVal'
    if isDone then return acc else goStepper stVar (() : acc)

iterateUntilOrTimeoutNaive2 ::
  (IOLike m, MonadTimer m) => DiffTime -> a -> (a -> m (Bool, a)) -> m (a, CallDoneType)
iterateUntilOrTimeoutNaive2 delay initSt stepAction = do
  ms <- getMaskingState
  Debug.traceM ("masking " <> show ms)

  stVar <- uncheckedNewTVarM initSt

  startR <- getMonotonicTime

  raceRes <-
    timeout
      delay
      ( asyncWithUnmask $ do
          ms <- getMaskingState
          Debug.traceM ("masking " <> show ms)
          goStepper stVar []
      )

  endR <- getMonotonicTime
  _ <-
    Debug.trace
      ( case raceRes of
          Nothing -> "timer-done " <> show (endR `diffTime` startR)
          Just steps -> "stepper-done " <> show (endR `diffTime` startR) <> " " <> show (length steps)
      )
      (return ())

  lastSt <- atomically $ readTVar stVar

  return (lastSt, maybe TimerDone (const StepperDone) raceRes)
 where
  goStepper stVar acc = do
    stVal <- atomically $ readTVar stVar
    start <- getMonotonicTime
    (isDone, stVal') <- stepAction stVal
    end <- getMonotonicTime
    _ <- Debug.trace ("stepper-act " <> show (end `diffTime` start)) $ return ()
    atomically $ writeTVar stVar stVal'
    if isDone then return acc else goStepper stVar (() : acc)
