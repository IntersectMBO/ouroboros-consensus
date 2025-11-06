{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Ouroboros.Consensus.Block.SupportsPeras (PerasRoundNo (..))
import Ouroboros.Consensus.BlockchainTime (RelativeTime (..))
import Ouroboros.Consensus.Peras.Params (PerasParams)
import Ouroboros.Consensus.Peras.Voting
  ( PerasVotingView (..)
  , isPerasVotingAllowed
  )
import Ouroboros.Consensus.Util.Pred (explainShallow)
import qualified Test.Consensus.Peras.Voting.Model as Model
import Test.Consensus.Peras.Voting.Util
  ( TestCert
  , genPerasParams
  , genPerasRoundNo
  , genTestCert
  , genWithOrigin
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Arbitrary (..)
  , CoArbitrary
  , Fun
  , Function
  , Property
  , Small (..)
  , Testable (..)
  , applyFun
  , counterexample
  , tabulate
  , testProperty
  )
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
