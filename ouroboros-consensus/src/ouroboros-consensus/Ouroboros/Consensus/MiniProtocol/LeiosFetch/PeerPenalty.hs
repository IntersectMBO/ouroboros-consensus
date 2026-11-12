-- | Recent closure-service evidence for one live upstream registration.
-- Reads are pure; only a qualifying own-head stall records fresh evidence.
-- Rationale: @docs/fetch-policy/leiosfetch-improved.md#source-selection-and-progressive-recovery@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.PeerPenalty
  ( Penalty
  , Score
  , Snapshot (..)
  , emptyPenalty
  , zeroScore
  , halfLife
  , scoreValue
  , effectiveScore
  , chargePenalty
  , penaltySnapshot
  ) where

import Control.Monad.Class.MonadTime.SI (DiffTime, Time, diffTime)
import Data.Word (Word32)

-- | Finite non-negative ranking evidence. The hidden constructor prevents
-- NaN/infinity from violating ordering. This is local policy, never a wire count.
newtype Score = Score Double
  deriving (Eq, Ord, Show)

-- | Last charged evidence only: no per-stall history or aging timer is retained.
data Penalty = Empty | Penalty !Score !Time
  deriving (Eq, Show)

-- | Detached saved evidence; its timestamp is not refreshed by observation.
data Snapshot = Snapshot
  { savedScore :: !Score
  -- ^ Stored score immediately after the last qualifying charge, or zero.
  , savedAt :: !(Maybe Time)
  -- ^ Monotonic baseline, absent before the first charge.
  }
  deriving (Eq, Show)

emptyPenalty :: Penalty
emptyPenalty = Empty

zeroScore :: Score
zeroScore = Score 0

-- | Provisional local policy: ten minutes to halve the remaining evidence.
halfLife :: DiffTime
halfLife = 600

scoreValue :: Score -> Double
scoreValue (Score value) = value

-- | Observe without changing the baseline. A stale supplied time cannot amplify
-- the saved score. At/below half a stall, the remaining evidence is forgiven.
effectiveScore :: Time -> Penalty -> Score
effectiveScore _ Empty = zeroScore
effectiveScore now (Penalty (Score score) at) =
  let elapsed = realToFrac (diffTime (max now at) at) :: Double
      aged = score * 2 ** (negate elapsed / realToFrac halfLife)
   in if aged <= 0.5 then zeroScore else Score aged

-- | Age/cutoff before adding one; preserve the baseline if supplied time is
-- stale. The former Word32 ceiling bounds numeric storage, not useful service.
chargePenalty :: Time -> Penalty -> Penalty
chargePenalty now previous =
  let score = min (fromIntegral (maxBound :: Word32)) (scoreValue (effectiveScore now previous) + 1)
      at = case previous of Empty -> now; Penalty _ old -> max now old
   in Penalty (Score score) at

penaltySnapshot :: Penalty -> Snapshot
penaltySnapshot Empty = Snapshot zeroScore Nothing
penaltySnapshot (Penalty score at) = Snapshot score (Just at)
