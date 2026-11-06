{-# LANGUAGE NamedFieldPuns #-}

-- | Per-EB closure retry pressure and absolute cooldowns. Physical requests,
-- their frozen allowances and body retry state belong to other components.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#8-checkpoint-deadlines-and-policy-reactions@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Retry
  ( RetryConfig
  , RetryConfigError (..)
  , mkRetryConfig
  , defaultRetryConfig
  , RetryState
  , RetrySnapshot (..)
  , newRetryState
  , retrySnapshot
  , retryFactor
  , recordRetryFailure
  , recoverRetryService
  , advanceRetry
  , retryEligible
  , nextRetryDeadline
  ) where

import Control.Monad.Class.MonadTime.SI (DiffTime, Time, addTime)
import Data.Word (Word32)

-- | Validated local policy, shared as configuration rather than wire negotiation.
-- Logs are precomputed once; live pressure is bounded in log space.
data RetryConfig = RetryConfig
  { factorMaximum :: !Rational
  -- ^ Exact upper bound returned for a saturated multiplier.
  , logMaximum :: !Double
  -- ^ Upper bound on stored log pressure.
  , logGrowth :: !Double
  -- ^ Positive log increment per actual rescue failure.
  , logRecovery :: !Double
  -- ^ Negative log increment per nominal checkpoint of uniquely useful bytes.
  , failureGroupSize :: !Word32
  -- ^ Rescue expiries per cooldown; primary and body expiries do not count.
  , cooldownInitial :: !DiffTime
  -- ^ Duration of the first cooldown, in SI seconds.
  , cooldownMaximum :: !DiffTime
  -- ^ Upper bound on each cooldown duration, in SI seconds.
  }
  deriving (Eq, Show)

-- | Reject invalid or unrepresentable local settings before installing policy.
data RetryConfigError = InvalidRetryFactors | InvalidCooldown deriving (Eq, Show)

-- | Maximum factor, growth factor, per-checkpoint recovery factor, failure
-- group size, initial cooldown and maximum cooldown. Floating conversions must
-- stay finite and preserve the strict growth/recovery inequalities.
mkRetryConfig ::
  Rational ->
  Rational ->
  Rational ->
  Word32 ->
  DiffTime ->
  DiffTime ->
  Either RetryConfigError RetryConfig
mkRetryConfig maximumFactor growth recovery group initial cooldownCap
  | maximumFactor < 1 || growth <= 1 || recovery <= 0 || recovery >= 1 = Left InvalidRetryFactors
  | any invalid [maximumD, growthD, recoveryD]
      || maximumD < 1
      || growthD <= 1
      || recoveryD <= 0
      || recoveryD >= 1 =
      Left InvalidRetryFactors
  | group == 0 || initial <= 0 || cooldownCap < initial = Left InvalidCooldown
  | otherwise =
      Right
        (RetryConfig maximumFactor (log maximumD) (log growthD) (log recoveryD) group initial cooldownCap)
 where
  maximumD = fromRational maximumFactor
  growthD = fromRational growth
  recoveryD = fromRational recovery
  invalid x = isNaN x || isInfinite x

-- | Agreed initial policy: 1.5 growth, 0.7 recovery per nominal checkpoint,
-- maximum factor 16, and groups of five failures with 1–16 second cooldowns.
defaultRetryConfig :: RetryConfig
defaultRetryConfig = RetryConfig 16 (log 16) (log 1.5) (log 0.7) 5 1 16

-- | One acquisition's bounded retry state. Useful service changes pressure,
-- never the grouped failure history or an already running cooldown.
data RetryState = RetryState
  { pressure :: !Double
  -- ^ Log multiplier in [0, log(maximum)]. No unbounded rational products grow.
  , failuresInGroup :: !Word32
  -- ^ Actual rescue expiries since the last completed group.
  , nextCooldown :: !DiffTime
  -- ^ Duration to use at the next group boundary, capped after doubling.
  , cooldownUntil :: !(Maybe Time)
  -- ^ Fixed absolute expiry; new offers, progress and wakes do not renew it.
  }
  deriving (Eq, Show)

-- | Read-only retry evidence with an ordinary multiplier and SI durations.
data RetrySnapshot = RetrySnapshot
  { snapshotRetryFactor :: !Rational
  -- ^ Multiplier to freeze into a newly admitted rescue, at least one.
  , snapshotFailuresInGroup :: !Word32
  -- ^ Grouped rescue failures; not cumulative peer-selection failures.
  , snapshotNextCooldown :: !DiffTime
  -- ^ Duration of the next grouped cooldown.
  , snapshotCooldownUntil :: !(Maybe Time)
  -- ^ Current absolute cooldown expiry, absent after it has been reconciled.
  }
  deriving (Eq, Show)

newRetryState :: RetryConfig -> RetryState
newRetryState config = RetryState 0 0 (cooldownInitial config) Nothing

-- | Bounded approximation for fractional useful-byte recovery. Exact bounds
-- are returned at zero and saturation; rational conversion occurs only when a
-- snapshot is needed, without storing a growing numerator/denominator.
retryFactor :: RetryConfig -> RetryState -> Rational
retryFactor config state
  | pressure state <= 0 = 1
  | pressure state >= logMaximum config = factorMaximum config
  | otherwise =
      min
        (factorMaximum config)
        (toRational (min (fromRational (factorMaximum config)) (exp (pressure state))))

retrySnapshot :: RetryConfig -> RetryState -> RetrySnapshot
retrySnapshot config state =
  RetrySnapshot
    (retryFactor config state)
    (failuresInGroup state)
    (nextCooldown state)
    (cooldownUntil state)

-- | Charge one actual still-wanted rescue expiry at the time it is handled.
-- Each full group starts a fixed cooldown; only the next duration doubles.
recordRetryFailure :: RetryConfig -> Time -> RetryState -> RetryState
recordRetryFailure config now state =
  if failuresInGroup state + 1 < failureGroupSize config
    then grown{failuresInGroup = failuresInGroup state + 1}
    else
      grown
        { failuresInGroup = 0
        , cooldownUntil = Just (addTime (nextCooldown state) now)
        , nextCooldown = min (cooldownMaximum config) (2 * nextCooldown state)
        }
 where
  grown = state{pressure = min (logMaximum config) (pressure state + logGrowth config)}

-- | Reduce pressure in proportion to actual unique payload bytes. A zero target
-- or zero delivery is inert; the closure arena rejects a zero nominal target.
-- This must not be called for duplicates, reclaimed retries or initial data.
recoverRetryService :: RetryConfig -> Word32 -> Word32 -> RetryState -> RetryState
recoverRetryService config nominal bytes state
  | nominal == 0 || bytes == 0 = state
  | otherwise =
      state
        { pressure = max 0 (pressure state + fromIntegral bytes / fromIntegral nominal * logRecovery config)
        }

-- | Retire a due cooldown once, without resetting its history or extending it.
advanceRetry :: Time -> RetryState -> RetryState
advanceRetry now state = case cooldownUntil state of
  Just deadline | now >= deadline -> state{cooldownUntil = Nothing}
  _ -> state

-- | A past expiry is already eligible even before reconciliation clears it.
retryEligible :: Time -> RetryState -> Bool
retryEligible now state = maybe True (now >=) (cooldownUntil state)

-- | Only a future expiry belongs in the coordinator's next wait. A due expiry
-- is handled by 'advanceRetry', never repeatedly registered in the past.
nextRetryDeadline :: Time -> RetryState -> Maybe Time
nextRetryDeadline now state = case cooldownUntil state of
  Just deadline | now < deadline -> Just deadline
  _ -> Nothing
