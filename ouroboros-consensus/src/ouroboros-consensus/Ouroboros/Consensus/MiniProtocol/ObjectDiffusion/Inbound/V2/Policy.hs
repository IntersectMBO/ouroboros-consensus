{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Policy
  ( DecisionPolicy (..)
  , defaultDecisionPolicy
  , chooseGeometricWithMedian
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types (ObjectMultiplicity (..))
import Ouroboros.Network.Protocol.ObjectDiffusion.Type
  ( NumObjectIdsReq (..)
  , NumObjectsOutstanding (..)
  , NumObjectsReq (..)
  )
import Test.QuickCheck (Arbitrary (..), Gen, choose)

-- | Policy for making decisions
data DecisionPolicy = DecisionPolicy
  { dpMaxNumObjectIdsReq :: !NumObjectIdsReq
  -- ^ a maximal number of objectIds requested at once.
  , dpMaxNumObjectsOutstanding :: !NumObjectsOutstanding
  -- ^ maximal number of objects in the outstanding FIFO.
  , dpMaxNumObjectsInflightPerPeer :: !NumObjectsReq
  -- ^ a limit of objects in-flight from a single peer.
  , dpMaxNumObjectsInflightTotal :: !NumObjectsReq
  -- ^ a limit of objects in-flight from all peers for this node.
  , dpTargetObjectRedundancy :: !ObjectMultiplicity
  -- ^ from how many peers download the `objectId` simultaneously
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, NoThunks)

instance Arbitrary DecisionPolicy where
  arbitrary =
    let DecisionPolicy
          { dpMaxNumObjectIdsReq
          , dpMaxNumObjectsOutstanding
          , dpMaxNumObjectsInflightPerPeer
          , dpMaxNumObjectsInflightTotal
          , dpTargetObjectRedundancy
          } = defaultDecisionPolicy
     in DecisionPolicy
          <$> (chooseGeometricWithMedian dpMaxNumObjectIdsReq)
          <*> (chooseGeometricWithMedian dpMaxNumObjectsOutstanding)
          <*> (chooseGeometricWithMedian dpMaxNumObjectsInflightPerPeer)
          <*> (chooseGeometricWithMedian dpMaxNumObjectsInflightTotal)
          <*> (chooseGeometricWithMedian dpTargetObjectRedundancy)

defaultDecisionPolicy :: DecisionPolicy
defaultDecisionPolicy =
  DecisionPolicy
    { dpMaxNumObjectIdsReq = 3
    , dpMaxNumObjectsOutstanding = 10 -- must be the same as the outbound peer's value
    , dpMaxNumObjectsInflightPerPeer = 6
    , dpMaxNumObjectsInflightTotal = 20
    , dpTargetObjectRedundancy = 2
    }

-- TODO: this needs to be tested and inspected

-- | Geometric-decay generator over [1 .. maxBound - 1] for the type 'a'.
--   Smaller values are more likely; the (lower) median is ~ medianTarget.
--   Works for any Integral + Bounded numeric type (e.g., Int, Word32, Int64).
chooseGeometricWithMedian :: forall a. (Integral a, Bounded a) => a -> Gen a
chooseGeometricWithMedian medianTarget
  | (maxBound @a) <= 1 =
      error "Type's maxBound <= 1: no room for [1..maxBound-1]"
  | medianTarget < 1 || medianTarget >= maxBound =
      error "medianTarget must be in [1 .. maxBound-1]"
  | otherwise = do
      let lo = 1
          hi = maxBound - 1
          -- use Integer for counts, Double for CDF inversion
          nI = toInteger (hi - lo + 1)
          mI = toInteger (medianTarget - lo + 1)
          n = fromIntegral nI :: Double
          m = fromIntegral mI :: Double
          p = 1 - 2 ** (-1 / m) -- set so P(X ≤ median) ≈ 0.5
          q = 1 - p -- decay factor
          qn = q ** n -- truncation term
      u <- choose (0, 1 :: Double)
      let y = 1 - u * (1 - qn)
          k = floor (log y / log q) -- inverse truncated geometric CDF
          k' = max 0 (min (floor (n - 1)) k)
      pure (lo + fromInteger k')
