{-# LANGUAGE FlexibleContexts #-}

-- | Coordinator wakeups from coalesced readiness, an absolute policy deadline,
-- or the idle fallback. No request deadline is created or renewed here.
--
-- Rationale (repository-root path):
-- @docs/fetch-policy/cancellation-contract.md#agreed-coordinator-wakeups@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Wait
  ( Readiness
  , newReadiness
  , notifyReadiness
  , WakeReason (..)
  , Wakeup (..)
  , waitForWakeup
  ) where

import Control.Applicative (Alternative)
import Control.Monad (unless, when)
import Control.Monad.Class.MonadSTM (MonadSTM (STM), atomically, check, retry)
import Control.Monad.Class.MonadThrow (MonadMask, bracket)
import Control.Monad.Class.MonadTime.SI (Time, addTime, diffTime, getMonotonicTime)
import Control.Monad.Class.MonadTimer.SI (MonadTimer (registerDelayCancellable), TimeoutState (..))
import Data.Functor (($>))
import Data.Monoid.Synchronisation (FirstToFinish (..))
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds, picosecondsToDiffTime)
import Ouroboros.Consensus.Util.NormalForm.StrictTVar (StrictTVar, newTVar, readTVar, writeTVar)

-- | A coalescing signal for one coordinator. Producers publish changed state
-- before notifying; for STM-owned state, use the same transaction. The wait
-- consumes the signal atomically before the planning snapshot. Notifications
-- during planning remain pending for the next wait.
newtype Readiness m = Readiness (StrictTVar m Bool)

-- | Create an initially clear signal. It is independent of request lifetimes
-- and is reused across successive coordinator waits.
newReadiness :: MonadSTM m => STM m (Readiness m)
newReadiness = Readiness <$> newTVar False

-- | Publish readiness without accumulating one wake per notification. Signal
-- changes to the selected deadline/cooldown as well as newly available data.
notifyReadiness :: MonadSTM m => Readiness m -> STM m ()
notifyReadiness (Readiness ready) = do
  pending <- readTVar ready
  unless pending (writeTVar ready True)

-- | Which condition won the wait. The caller still reconciles all observed
-- events and all deadlines now due, irrespective of the winning condition.
data WakeReason
  = -- | Newly published state should be considered by the next planning pass.
    ReadinessWake
  | -- | The selected absolute policy deadline/cooldown has become due.
    DeadlineWake
  | -- | No earlier condition won before the 500 ms idle fallback.
    FallbackWake
  deriving (Eq, Show)

-- | Result of one wait. Readiness takes precedence when both conditions are
-- available. The caller must still process every due deadline on every wake.
data Wakeup = Wakeup
  { wakeReason :: !WakeReason
  -- ^ Winning condition; this is not a filter on the work processed next.
  , readinessConsumed :: !Bool
  -- ^ Whether pending readiness was consumed. A notification arriving
  -- afterwards remains set for the next wait.
  }
  deriving (Eq, Show)

-- | Wait for readiness, the nearest fixed absolute deadline/cooldown, or 500 ms.
-- Pass only unhandled deadlines: the timing ledger's @nextDeadline@ omits fired
-- or locally deferred timers. An already-due deadline returns immediately;
-- the caller must process/retire it before waiting again.
--
-- One cancellable registration covers the earlier of the deadline and fallback.
-- It is cancelled on every normal or exceptional exit. No timer is needed for
-- already-ready/already-due work, and the registered delay never exceeds 500 ms
-- apart from upward rounding to the timer API's microsecond resolution.
waitForWakeup ::
  (MonadTimer m, MonadMask m, Alternative (STM m)) =>
  Readiness m -> Maybe Time -> m Wakeup
waitForWakeup (Readiness ready) deadline = do
  started <- getMonotonicTime
  let fallbackAt = addTime 0.5 started
      (target, timerReason) = case deadline of
        Just at | at <= fallbackAt -> (at, DeadlineWake)
        _ -> (fallbackAt, FallbackWake)
      consume reason = do
        pending <- readTVar ready
        when pending (writeTVar ready False)
        pure (Wakeup reason pending)
  immediate <- atomically $ do
    pending <- readTVar ready
    if pending
      then Just <$> consume ReadinessWake
      else if target <= started then Just <$> consume timerReason else pure Nothing
  case immediate of
    Just wakeup -> pure wakeup
    Nothing -> withTimerAt target $ \awaitTimer -> atomically $ do
      let onReady = FirstToFinish ((readTVar ready >>= check) $> ReadinessWake)
          onTimer = FirstToFinish (awaitTimer $> timerReason)
      runFirstToFinish (onReady <> onTimer) >>= consume

-- | Register only the remaining duration, rounding upwards because the IO timer
-- API narrows SI seconds to whole microseconds. Never register a zero/negative
-- delay or wake early through that conversion.
withTimerAt :: (MonadTimer m, MonadMask m) => Time -> (STM m () -> m a) -> m a
withTimerAt target action = bracket acquire (\(_, cancelTimer) -> cancelTimer) $ \(readTimer, _) ->
  action $ do
    state <- readTimer
    case state of
      TimeoutFired -> pure ()
      _ -> retry
 where
  acquire = do
    now <- getMonotonicTime
    if target <= now
      then pure (pure TimeoutFired, pure ())
      else registerDelayCancellable (roundUpMicrosecond (diffTime target now))

roundUpMicrosecond :: DiffTime -> DiffTime
roundUpMicrosecond duration =
  picosecondsToDiffTime (((diffTimeToPicoseconds duration + 999999) `div` 1000000) * 1000000)
