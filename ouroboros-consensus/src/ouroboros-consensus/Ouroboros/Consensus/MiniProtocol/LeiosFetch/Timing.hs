{-# LANGUAGE NamedFieldPuns #-}

-- | Frozen request patience and FIFO head activation. No payload progress or
-- later RTT update renews a clock. The interpreter suppresses head activation
-- while its preceding local reply handler prevents reception.
-- Rationale: @docs/fetch-policy/leiosfetch-improved.md#timing-and-fifo-eligibility@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Timing
  ( RttObservation (..)
  , RttSource (..)
  , conservativeRttWithSource
  , initialRounds
  , Snapshot (..)
  , Clock (..)
  , snapshot
  , newClock
  , sent
  , activate
  , received
  , deadline
  , expire
  , lateReply
  ) where

import Control.Monad.Class.MonadTime.SI (DiffTime, Time, addTime, diffTime)
import Data.Word (Word32, Word64)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Config

-- | Original successful KeepAlive observation from this connection.
data RttObservation = RttObservation
  { rawRoundTripTime :: !DiffTime
  , measurementCompletedAt :: !Time
  }
  deriving (Eq, Show)

-- | Detached provenance for the allowance selected at admission.
data RttSource = KeepAliveSample | MissingRtt | StaleRtt | InvalidRtt
  deriving (Eq, Show)

-- | Sparse samples get 50% headroom, capped at 500 ms. Invalid, future or
-- older-than-30-second samples use the same bounded fallback.
conservativeRttWithSource :: Time -> Maybe RttObservation -> (DiffTime, RttSource)
conservativeRttWithSource now sample = case sample of
  Nothing -> (0.5, MissingRtt)
  Just RttObservation{rawRoundTripTime, measurementCompletedAt}
    | rawRoundTripTime <= 0 || age < 0 -> (0.5, InvalidRtt)
    | age > 30 -> (0.5, StaleRtt)
    | otherwise -> (min 0.5 (1.5 * rawRoundTripTime), KeepAliveSample)
   where
    age = diffTime now measurementCompletedAt

-- | Smallest positive K for I*(2^K-1) >= B. Input sizes are bounded to
-- Word32; widening the cumulative window to Word64 prevents overflow.
initialRounds :: Word32 -> Word32 -> Either String Word32
initialRounds 0 _ = Left "zero initial window"
initialRounds window bytes = Right (go 1 (fromIntegral window :: Word64))
 where
  go k delivered
    | delivered >= fromIntegral bytes = k
    | otherwise = go (k + 1) (2 * delivered + fromIntegral window)

-- | Immutable admission inputs. Startup competes with transfer patience;
-- it does not add another transfer period or repeated transit allowance.
data Snapshot = Snapshot
  { snapshotRtt :: !DiffTime
  -- ^ Conservative frozen RTT allowance in seconds.
  , snapshotTransfer :: !DiffTime
  -- ^ Actual requested bytes / benchmark rate.
  , snapshotCold :: !DiffTime
  -- ^ Reference-window startup, capped at 500 ms.
  , snapshotRttSource :: !RttSource
  -- ^ Provenance frozen with the chosen allowance, including capped real samples.
  }
  deriving (Eq, Show)

-- | One immutable request's timing observations. A terminal can precede its
-- send-return callback. Local handling readiness and predecessor boundaries
-- come from the interpreter, never from a peer's dribble or decoder scheduling.
data Clock = Clock
  { clockSnapshot :: !Snapshot
  , clockSent :: !(Maybe (Time, Bool))
  -- ^ Return time and continuous-pipeline classification.
  , clockHead :: !(Maybe (Maybe Time, Maybe Time))
  -- ^ Prior terminal and local-ready observations.
  , clockReceipt :: !(Maybe Time)
  -- ^ Original read observation of the terminal byte.
  , clockFired :: !Bool
  -- ^ An expiry is reconciled at most once.
  }
  deriving (Eq, Show)

snapshot :: LeiosFetchStaticEnv -> Time -> Maybe RttObservation -> Word32 -> Either String Snapshot
snapshot cfg now sample bytes = do
  if minimumServiceRate cfg == 0 then Left "zero service rate" else pure ()
  rounds <- initialRounds 14600 bytes
  let (rtt, source) = conservativeRttWithSource now sample
  pure
    ( Snapshot
        rtt
        (fromRational (8 * fromIntegral bytes / fromIntegral (minimumServiceRate cfg)))
        (min 0.5 (fromIntegral rounds * rtt))
        source
    )

newClock :: Snapshot -> Clock
newClock inputs = Clock inputs Nothing Nothing Nothing False

-- | The channel may deliver a reply before this observation arrives.
sent :: Time -> Bool -> Clock -> Clock
sent at continuous clock = clock{clockSent = Just (at, continuous)}

-- | Activate only the actual FIFO head when local handling permits reception.
-- Repeated calls must preserve the original activation inputs.
activate :: Maybe Time -> Maybe Time -> Clock -> Clock
activate predecessor ready clock = case clockHead clock of
  Just _ -> clock
  Nothing -> clock{clockHead = Just (predecessor, ready)}

received :: Time -> Clock -> Clock
received at clock = clock{clockReceipt = Just at}

-- | Transit already elapsed behind the previous response is not charged again.
-- A local handler may move the service start to its own completion, preserving
-- any earlier terminal observation for lateness classification.
deadline :: Clock -> Maybe Time
deadline Clock{clockSnapshot = Snapshot{snapshotRtt, snapshotTransfer, snapshotCold}, clockSent, clockHead} = do
  (at, continuous) <- clockSent
  (prior, ready) <- clockHead
  let start = maximum (at : addTime snapshotRtt at : [t | Just t <- [prior, ready]])
      service = max snapshotTransfer (if continuous then snapshotRtt else snapshotCold)
  pure (addTime service start)

-- | Reconcile only an unresolved active clock. Known original receipts take
-- precedence over when their callbacks happened to be consumed by the planner.
expire :: Time -> Clock -> (Bool, Clock)
expire now clock
  | clockFired clock || clockReceipt clock /= Nothing = (False, clock)
  | Just at <- deadline clock, now >= at = (True, clock{clockFired = True})
  | otherwise = (False, clock)

lateReply :: Clock -> Bool
lateReply clock = case (deadline clock, clockReceipt clock) of
  (Just limit, Just at) -> at > limit
  _ -> False
