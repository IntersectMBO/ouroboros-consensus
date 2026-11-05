{-# LANGUAGE NamedFieldPuns #-}

-- | Pure request patience calculations. All durations are SI seconds; rational
-- calculations are rounded upwards to picoseconds only at the timer boundary.
--
-- Rationale (repository-root path):
-- @docs/fetch-policy/cancellation-contract.md#service-benchmark-and-general-startup-allowance@.
-- That section maps the types and calculations to precise contract clauses.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Deadline
  ( TimingConfig
  , mkTimingConfig
  , chainFollowingRate
  , TimingError (..)
  , RttObservation (..)
  , RttAllowance
  , getRttAllowance
  , conservativeRtt
  , TimingSnapshot
  , snapshotTiming
  , snapshotPayload
  , snapshotRtt
  , transferAllowance
  , coldAllowance
  , initialRounds
  , PipelineState (..)
  , DataDeadline (..)
  , dataDeadline
  , ackDeadline
  , hedgeAllowance
  ) where

import Control.Monad (when)
import qualified Data.List.NonEmpty as NE
import Data.Ratio ((%))
import Data.Time.Clock (DiffTime, picosecondsToDiffTime)
import Numeric.Natural (Natural)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Descriptor
import Ouroboros.Consensus.Util.IOLike (Time, addTime, diffTime)

-- | Validated receiver-policy parameters for per-attempt timing snapshots.
-- Construct with 'mkTimingConfig', which checks parameter bounds and worst-case
-- compatibility with the fatal message guard. Each 'TimingSnapshot' freezes the
-- values relevant to its attempt.
--
-- Rationale: @docs/fetch-policy/cancellation-contract.md#service-benchmark-and-general-startup-allowance@.
data TimingConfig = TimingConfig
  { configRate :: !Rational
  -- ^ Positive receiver service benchmark in bits per second, independent of
  -- upstream claims and measured transport throughput.
  , configWindow :: !Natural
  -- ^ Positive reference initial window in bytes, used to count startup rounds.
  -- This is a policy yardstick, not an observation of the sender's TCP window.
  , configColdCap :: !DiffTime
  -- ^ Maximum startup allowance in seconds for a fresh or drained pipeline.
  -- The payload-transfer allowance can still exceed this cap.
  , configHedgeCeiling :: !DiffTime
  -- ^ Configured hedge-backoff ceiling in seconds. The effective ceiling is at
  -- least the unscaled base allowance, so it cannot shorten a large request.
  , configMaximumPayload :: !Natural
  -- ^ Largest timer payload in bytes accepted by 'snapshotTiming'. Used to
  -- validate worst-case patience against the fatal guard; must cover every
  -- body and checkpoint timer size permitted by the caller.
  }
  deriving (Eq, Show)

data TimingError
  = InvalidServiceRate
  | InvalidInitialWindow
  | InvalidDuration
  | InvalidMaximumPayload
  | IncompatibleFatalGuard
  | PayloadBeyondMaximum
  | InvalidBackoffFactor
  | InvalidCalibration
  deriving (Eq, Show)

-- | Check the worst fresh base, ACK allowance and clipped hedge allowance
-- against the fatal head-message guard. Reject incompatible settings rather
-- than silently clipping a large valid request to the hedge ceiling.
mkTimingConfig ::
  Rational ->
  Natural ->
  DiffTime ->
  DiffTime ->
  DiffTime ->
  PayloadBytes ->
  Either TimingError TimingConfig
mkTimingConfig rate window coldCap hedgeCap fatalGuard (PayloadBytes maximumPayload) = do
  when (rate <= 0) (Left InvalidServiceRate)
  when (window == 0) (Left InvalidInitialWindow)
  when (coldCap < 0 || hedgeCap <= 0 || fatalGuard <= 0) (Left InvalidDuration)
  when (maximumPayload == 0) (Left InvalidMaximumPayload)
  rounds <- initialRounds window maximumPayload
  let config = TimingConfig rate window coldCap hedgeCap maximumPayload
      transfer = secondsUp (8 * fromIntegral maximumPayload / rate)
      cold = min coldCap (fromIntegral rounds * rttCap)
      worstBase = rttCap + maximum [transfer, cold, rttCap]
  when (max hedgeCap worstBase >= fatalGuard) (Left IncompatibleFatalGuard)
  pure config

-- | Exact benchmark from trusted deployment sizing. The initial safety factor
-- is 2; callers may calibrate it or override the resulting service rate. Peer
-- announcements do not participate in this calculation.
chainFollowingRate ::
  PayloadBytes ->
  PayloadBytes ->
  PayloadBytes ->
  DiffTime ->
  Rational ->
  Either TimingError Rational
chainFollowingRate (PayloadBytes praos) (PayloadBytes closure) (PayloadBytes body) interval safety = do
  when (interval <= 0 || safety <= 0 || praos + closure + body == 0) (Left InvalidCalibration)
  pure (safety * 8 * fromIntegral (praos + closure + body) / toRational interval)

data RttObservation = RttObservation
  { rawRoundTripTime :: !DiffTime
  , measurementCompletedAt :: !Time
  }
  deriving (Eq, Show)

newtype RttAllowance = RttAllowance {getRttAllowance :: DiffTime}
  deriving (Eq, Show)

rttCap :: DiffTime
rttCap = 0.5

-- | Sparse successful KeepAlive measurements, not the smoothed GSV estimate.
-- A future, zero, negative, absent or older-than-30s sample uses the fallback.
conservativeRtt :: Time -> Maybe RttObservation -> RttAllowance
conservativeRtt now sample = RttAllowance $ case sample of
  Just RttObservation{rawRoundTripTime, measurementCompletedAt}
    | rawRoundTripTime > 0
    , let age = diffTime now measurementCompletedAt
    , age >= 0 && age <= 30 ->
        min rttCap (secondsUp (3 % 2 * toRational rawRoundTripTime))
  _ -> rttCap

-- | Immutable timing inputs for one data-request attempt, constructed by
-- 'snapshotTiming'. Freezes its payload size, RTT, service/startup allowances
-- and hedge backoff. FIFO activation later supplies the head time and pipeline
-- classification to 'dataDeadline'; progress and new samples do not renew it.
--
-- Rationale: @docs/fetch-policy/cancellation-contract.md#service-benchmark-and-general-startup-allowance@.
data TimingSnapshot = TimingSnapshot
  { snapshotPayload :: !PayloadBytes
  -- ^ Original payload byte total used for patience: the offered body size or
  -- the exact whole-TX batch total, including overhang. It does not shrink when
  -- some data is acquired elsewhere, and differs from a body's reservation.
  , snapshotRtt :: !RttAllowance
  -- ^ Conservative RTT allowance frozen for this attempt after applying
  -- observation freshness, headroom and the cap or fallback policy.
  , transferAllowance :: !DiffTime
  -- ^ Payload-transfer allowance in seconds: @8 * bytes / rate@, rounded
  -- upwards to picoseconds using the configured service benchmark.
  , coldAllowance :: !DiffTime
  -- ^ Startup allowance in seconds for a fresh or drained pipeline:
  -- @min(coldCap, K * R)@. Competes with 'transferAllowance'; a continuous
  -- pipeline uses the RTT allowance instead of this startup term.
  , snapshotFactor :: !Rational
  -- ^ Dimensionless hedge-backoff multiplier, at least one. Applied to the
  -- complete base allowance after FIFO activation; one leaves it unchanged.
  , snapshotCeiling :: !DiffTime
  -- ^ Configured backoff ceiling in seconds. Its effective value is at least
  -- the unscaled base, so applying it cannot shorten that base allowance.
  }
  deriving (Eq, Show)

-- | Freeze service/RTT/backoff inputs once per attempt. Bodies use the offered
-- timer size, not their larger trusted admission reservation. TXs use their
-- actual immutable sizes, including the last transaction's overhang.
snapshotTiming ::
  TimingConfig ->
  Time ->
  Maybe RttObservation ->
  Rational ->
  PreparedRequest ->
  Either TimingError TimingSnapshot
snapshotTiming config now sample factor request = do
  when (factor < 1) (Left InvalidBackoffFactor)
  when (bytes > configMaximumPayload config) (Left PayloadBeyondMaximum)
  rounds <- initialRounds (configWindow config) bytes
  pure
    TimingSnapshot
      { snapshotPayload = PayloadBytes bytes
      , snapshotRtt = rtt
      , transferAllowance = secondsUp (8 * fromIntegral bytes / configRate config)
      , coldAllowance = min (configColdCap config) (fromIntegral rounds * getRttAllowance rtt)
      , snapshotFactor = factor
      , snapshotCeiling = configHedgeCeiling config
      }
 where
  rtt = conservativeRtt now sample
  bytes = case preparedDescriptor request of
    BodyRequest BodyDescriptor{bodyAnnouncedBytes} -> fromIntegral bodyAnnouncedBytes
    BatchRequest{} -> sum [n | PayloadBytes n <- NE.toList (preparedElements request)]

-- | Smallest positive K for I*(2^K-1) >= B. The accumulator never overflows.
-- A zero reference window is rejected instead of looping or dividing by zero.
initialRounds :: Natural -> Natural -> Either TimingError Natural
initialRounds 0 _ = Left InvalidInitialWindow
initialRounds window bytes = Right (go 1 window)
 where
  go k delivered
    | delivered >= bytes = k
    | otherwise = go (k + 1) (2 * delivered + window)

data PipelineState = FreshOrDrained | Continuous deriving (Eq, Show)

-- | Fixed timing decision for one data reply at the FIFO head. Combines the
-- attempt's snapshot with observed send and predecessor-terminal times to
-- determine its base allowance, effective hedge allowance and absolute expiry.
-- Obtaining the payload elsewhere leaves this wire reply's deadline intact.
--
-- Rationale: @docs/fetch-policy/cancellation-contract.md#service-benchmark-and-general-startup-allowance@.
data DataDeadline = DataDeadline
  { deadlineHeadAt :: !Time
  -- ^ Monotonic FIFO head time: the later of this request's send-return
  -- observation and its predecessor's terminal receipt observation.
  , deadlineBase :: !DiffTime
  -- ^ Unscaled allowance in seconds from 'deadlineHeadAt', including remaining
  -- request-transit grace and the applicable payload-service allowance.
  , deadlineAllowance :: !DiffTime
  -- ^ Allowance in seconds after applying snapshotted hedge backoff and its
  -- base-preserving ceiling, measured from 'deadlineHeadAt'.
  , deadlineExpiresAt :: !Time
  -- ^ Absolute monotonic expiry: head time plus 'deadlineAllowance'. Progress,
  -- later offers and logical reassignment do not move this deadline.
  , deadlinePipeline :: !PipelineState
  -- ^ Receiver-observed data backlog at send return. Only an unresolved prior
  -- data reply makes the pipeline continuous; ACK-only backlog does not.
  }
  deriving (Eq, Show)

-- | Head activation is derived from observed predecessors, not planner time.
-- If the predecessor completed before send returned, there was no unresolved
-- predecessor at that boundary and head time is the request's own send time.
dataDeadline :: TimingSnapshot -> Time -> Maybe Time -> Maybe Time -> DataDeadline
dataDeadline snapshot sent previousTerminal previousDataTerminal =
  DataDeadline headAt base allowance (addTime allowance headAt) pipeline
 where
  headAt = maybe sent (max sent) previousTerminal
  pipeline = case previousDataTerminal of
    Just finished | finished > sent -> Continuous
    _ -> FreshOrDrained
  rtt = getRttAllowance (snapshotRtt snapshot)
  service = max (transferAllowance snapshot) $ case pipeline of
    FreshOrDrained -> coldAllowance snapshot
    Continuous -> rtt
  base = max 0 (diffTime (addTime rtt sent) headAt) + service
  allowance = hedgeAllowance base (snapshotFactor snapshot) (snapshotCeiling snapshot)

ackDeadline :: RttAllowance -> Time -> Maybe Time -> Time
ackDeadline rtt sent previousTerminal =
  addTime (getRttAllowance rtt) (maybe sent (max sent) previousTerminal)

-- | Clip before multiplication. Even a very large exact factor cannot overflow
-- an intermediate floating-point value or shorten a larger base allowance.
hedgeAllowance :: DiffTime -> Rational -> DiffTime -> DiffTime
hedgeAllowance base factor hedgeCap
  | base <= 0 = 0
  | factor >= toRational effectiveCeiling / toRational base = effectiveCeiling
  | otherwise = secondsUp (toRational base * max 1 factor)
 where
  effectiveCeiling = max base hedgeCap

secondsUp :: Rational -> DiffTime
secondsUp seconds = picosecondsToDiffTime (ceiling (seconds * 1000000000000))
