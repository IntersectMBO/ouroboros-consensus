{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Smoke test for Peras voting rules using mocked up 'PerasVotingView's.
module Test.Consensus.Peras.Voting.Smoke (tests) where

import GHC.Generics (Generic)
import GHC.Word (Word64)
import Ouroboros.Consensus.Block.Abstract
  ( SlotNo (..)
  , WithOrigin (..)
  )
import Ouroboros.Consensus.Block.SupportsPeras
  ( HasPerasCertRound (..)
  , PerasRoundNo (..)
  , getPerasCertRound
  )
import Ouroboros.Consensus.BlockchainTime
  ( RelativeTime (..)
  )
import Ouroboros.Consensus.Peras.Params
  ( PerasBlockMinSlots (..)
  , PerasCertArrivalThreshold (..)
  , PerasCooldownRounds (..)
  , PerasIgnoranceRounds (..)
  , PerasParams (..)
  )
import Ouroboros.Consensus.Peras.Voting
  ( PerasVotingView (..)
  , isPerasVotingAllowed
  )
import Ouroboros.Consensus.Util.Pred (explainShallow)
import qualified Test.Consensus.Peras.Voting.Model as Model
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Arbitrary (..)
  , CoArbitrary
  , Fun (..)
  , Function
  , Gen
  , Positive (..)
  , Property
  , Small (..)
  , Testable (..)
  , applyFun
  , choose
  , counterexample
  , frequency
  , tabulate
  , testProperty
  )
import Test.Util.Orphans.Arbitrary (genNominalDiffTime50Years)
import Test.Util.QuickCheck (geometric)
import Test.Util.TestEnv (adjustQuickCheckTests)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests =
  adjustQuickCheckTests (* 100) $
    testGroup
      "Peras.Voting.Smoke"
      [ testProperty "isPerasVotingAllowed smoke test" prop_isPerasVotingAllowed
      ]

{-------------------------------------------------------------------------------
  Model conformance test property
-------------------------------------------------------------------------------}

-- | Test that the Peras voting rules can correctly decide when to vote based
-- on a simplified model that doesn't use anything fancy to evaluate the rules.
prop_isPerasVotingAllowed :: PerasVotingView' TestCert -> Property
prop_isPerasVotingAllowed pvv' = do
  -- Unpack the generated Peras voting view
  let pvv = toPerasVotingView pvv'
  -- Determine whether we should vote according to the model
  let (shouldVote, votingRules@(vr1a, vr1b, vr2a, vr2b)) =
        Model.isPerasVotingAllowed pvv
  -- Some helper functions to report success/failure
  let chain = flip (foldr ($)) . reverse
  let ok desc =
        chain
          [ tabulate "VR-1A" [show vr1a]
          , tabulate "VR-1B" [show vr1b]
          , tabulate "VR-2A" [show vr2a]
          , tabulate "VR-2B" [show vr2b]
          , tabulate "VR-(1A|1B|2A|2B)" [show votingRules]
          , tabulate "Should vote according to model" [show shouldVote]
          , tabulate "Actual result" [desc]
          ]
          $ property True
  let failure desc =
        counterexample desc $
          property False
  -- Now check that the real implementation agrees with the model
  case isPerasVotingAllowed pvv of
    Right voteReason
      | shouldVote ->
          ok $ "VoteReason(" <> explainShallow voteReason <> ")"
      | otherwise ->
          failure $ "Expected not to vote, but got: " <> show voteReason
    Left noVoteReason
      | not shouldVote ->
          ok $ "NoVoteReason(" <> explainShallow noVoteReason <> ")"
      | otherwise ->
          failure $ "Expected to vote, but got: " <> show noVoteReason

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

-- * Mocked certificate type

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

genWithOrigin :: Gen a -> Gen (WithOrigin a)
genWithOrigin gen = frequency [(1, pure Origin), (9, NotOrigin <$> gen)]

-- * Peras voting views

-- | A version of 'PerasVotingView' with all functions lifted to 'Fun'
data PerasVotingView' cert = PerasVotingView'
  { perasParams' :: PerasParams
  , currRoundNo' :: PerasRoundNo
  , latestCertSeen' :: WithOrigin cert
  , latestCertOnChain' :: WithOrigin cert
  , arrivalSlot' :: Fun cert (Small Word64)
  , certRoundStart' :: Fun cert (Small Word64)
  , candidateExtendsCert' :: Fun cert Bool
  }
  deriving (Show, Generic) -- the whole reason to have this type

instance Arbitrary (PerasVotingView' TestCert) where
  arbitrary = do
    roundNo <- genPerasRoundNo
    PerasVotingView'
      <$> genPerasParams
      <*> pure roundNo
      <*> genWithOrigin (genTestCert roundNo)
      <*> genWithOrigin (genTestCert roundNo)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

  -- NOTE: arbitrary functions can only be shown after shrinking them first
  shrink pvv' =
    [ pvv'
        { arrivalSlot' = shrunkArrivalSlot
        , certRoundStart' = shrunkCertRoundStart
        , candidateExtendsCert' = shrunkCandidateExtendsCert
        }
    | shrunkArrivalSlot <- shrink (arrivalSlot' pvv')
    , shrunkCertRoundStart <- shrink (certRoundStart' pvv')
    , shrunkCandidateExtendsCert <- shrink (candidateExtendsCert' pvv')
    ]

toPerasVotingView :: PerasVotingView' TestCert -> PerasVotingView TestCert
toPerasVotingView pvv' =
  PerasVotingView
    { perasParams = perasParams' pvv'
    , currRoundNo = currRoundNo' pvv'
    , latestCertSeen = latestCertSeen' pvv'
    , latestCertOnChain = latestCertOnChain' pvv'
    , arrivalSlot = SlotNo . getSmall <$> applyFun (arrivalSlot' pvv')
    , certRoundStart = SlotNo . getSmall <$> applyFun (certRoundStart' pvv')
    , candidateExtendsCert = applyFun (candidateExtendsCert' pvv')
    }

-- * Orphan instances needed for 'Function' constraints

instance Function RelativeTime
instance Function PerasRoundNo
instance Function TestCert

instance CoArbitrary RelativeTime
instance CoArbitrary PerasRoundNo
instance CoArbitrary TestCert
