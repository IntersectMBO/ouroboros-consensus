{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Testing utilities for the Peras voting rules
module Test.Consensus.Peras.Voting.Util
  ( genPerasRoundNo
  , genPerasParams
  , TestCert (..)
  , genTestCert
  , genWithOrigin
  ) where

import Cardano.Slotting.Time (RelativeTime (..))
import GHC.Generics (Generic)
import Ouroboros.Consensus.Block (HasPerasCertRound (..))
import Ouroboros.Consensus.Block.Abstract (WithOrigin (..))
import Ouroboros.Consensus.Block.SupportsPeras (PerasRoundNo (..))
import Ouroboros.Consensus.Peras.Params
  ( PerasBlockMinSlots (..)
  , PerasCertArrivalThreshold (..)
  , PerasCooldownRounds (..)
  , PerasIgnoranceRounds (..)
  , PerasParams (..)
  )
import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , Positive (..)
  , Small (..)
  , choose
  , frequency
  )
import Test.Util.Orphans.Arbitrary (genNominalDiffTime50Years)
import Test.Util.QuickCheck (geometric)

{-------------------------------------------------------------------------------
  Arbitrary helpers
-------------------------------------------------------------------------------}

-- * Peras round numbers

genPerasRoundNo :: Gen PerasRoundNo
genPerasRoundNo = do
  Positive (Small n) <- arbitrary
  pure (PerasRoundNo n)

-- * Peras parameters

-- NOTE: we use a geometric distribution to bias towards smaller values.
-- This increases the chance of covering all the voting rules more evenly,
-- while still allowing for larger values to be generated occasionally.
--
-- Moreover, geometric(0.5) + 1 means that:
--  - 50% chance of being 1
--  - 25% chance of being 2
--  - 12.5% chance of being 3
--  ... and so on
genPerasParams :: Gen PerasParams
genPerasParams = do
  _L <- fromIntegral . (+ 1) <$> geometric 0.5
  _X <- fromIntegral . (+ 1) <$> geometric 0.5
  _R <- fromIntegral . (+ 1) <$> geometric 0.5
  _K <- fromIntegral . (+ 1) <$> geometric 0.5
  pure
    PerasParams
      { perasBlockMinSlots = PerasBlockMinSlots _L
      , perasCertArrivalThreshold = PerasCertArrivalThreshold _X
      , perasIgnoranceRounds = PerasIgnoranceRounds _R
      , perasCooldownRounds = PerasCooldownRounds _K
      }

-- | Lift a generator into a 'WithOrigin' generator
genWithOrigin :: Gen a -> Gen (WithOrigin a)
genWithOrigin gen = frequency [(1, pure Origin), (9, NotOrigin <$> gen)]

{-------------------------------------------------------------------------------
  Mocked certificate type
-------------------------------------------------------------------------------}

-- NOTE: we could also use the real 'WithArrivalTime (ValidatedPerasCert blk)'
-- here. However, this one is much easier to derive a 'Function' instance for,
-- so we can actually generate the methods needed by 'PerasVotingView'.

data TestCert
  = TestCert
  { tcArrivalTime :: RelativeTime
  , tcRoundNo :: PerasRoundNo
  }
  deriving (Show, Eq, Generic)

instance HasPerasCertRound TestCert where
  getPerasCertRound = tcRoundNo

-- | Generate a test certificate
--
-- NOTE: to improve the probabilities of covering all the paths in the code,
-- we generate certificates relative to a given Peras round (the current one).
genTestCert :: PerasRoundNo -> Gen TestCert
genTestCert roundNo = do
  arrivalTime <- RelativeTime <$> genNominalDiffTime50Years
  offset <- choose @Integer (-3, 2)
  -- NOTE: here we need to be careful not to underflow the round number or we
  -- will get an exception later on when trying to evaluate 'succ maxBound'
  let roundNo' =
        PerasRoundNo $
          fromIntegral $
            max 0 $
              toInteger (unPerasRoundNo roundNo) + offset
  pure $
    TestCert
      { tcArrivalTime = arrivalTime
      , tcRoundNo = roundNo'
      }
