{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test that the Peras voting rules can correctly decide when to vote.
--
-- NOTE: in this file, we use uncommon variable names such as `_L` or `_X`
-- because that is their name in the CIP-0140, and we can't have variable names
-- starting with capital letters. Contrary to typical Haskell conventions, those
-- do not denote ignored variables.
module Test.Consensus.Peras.Voting.Rules (tests) where

import GHC.Generics (Generic)
import Ouroboros.Consensus.Block.Abstract
  ( SlotNo (..)
  , WithOrigin (..)
  )
import Ouroboros.Consensus.Block.SupportsPeras
  ( HasPerasCertRound (..)
  , PerasRoundNo (..)
  , getPerasCertRound
  , onPerasRoundNo
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
  , mkPerasParams
  )
import Ouroboros.Consensus.Peras.Voting.Rules
  ( PerasVotingRulesDecision (..)
  , isPerasVotingAllowed
  )
import Ouroboros.Consensus.Peras.Voting.View
  ( LatestCertOnChainView (..)
  , LatestCertSeenView (..)
  , PerasVotingView (..)
  )
import Ouroboros.Consensus.Util.Pred (Evidence (..), explainShallow)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Arbitrary (..)
  , Gen
  , Property
  , Testable (..)
  , choose
  , counterexample
  , forAll
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
  adjustQuickCheckTests (* 1000) $
    testGroup
      "Peras voting rules"
      [ testProperty "isPerasVotingAllowed" prop_isPerasVotingAllowed
      ]

{-------------------------------------------------------------------------------
  Model conformance test property
-------------------------------------------------------------------------------}

data PerasVotingRulesDecisionModel
  = PerasVotingDecisionModel
  { shouldVote :: Bool
  , vr1a :: Bool
  , vr1b :: Bool
  , vr2a :: Bool
  , vr2b :: Bool
  }

-- | A simplified model of the Peras voting rules, used to compare against the
-- real implementation. The main difference is that this model computes the
-- result of the predicate directly over the inputs, rather than using the
-- 'Pred' combinators to produce evidence in either direction.
--
-- NOTE: this predicate could be lifted directly from the agda specification.
isPerasVotingAllowedModel ::
  PerasVotingView TestCert ->
  PerasVotingRulesDecisionModel
isPerasVotingAllowedModel
  PerasVotingView
    { perasParams
    , currRoundNo
    , latestCertSeen
    , latestCertOnChain
    } =
    PerasVotingDecisionModel
      { shouldVote = vr1a && vr1b || vr2a && vr2b
      , vr1a = vr1a
      , vr1b = vr1b
      , vr2a = vr2a
      , vr2b = vr2b
      }
   where
    vr1a =
      vr1a1 && vr1a2
    vr1a1 =
      case latestCertSeen of
        NotOrigin cert ->
          currRoundNo == getPerasCertRound (lcsCert cert) + 1
        Origin ->
          currRoundNo == PerasRoundNo 0
    vr1a2 =
      case latestCertSeen of
        NotOrigin cert ->
          lcsArrivalSlot cert <= lcsRoundStartSlot cert + _X
        Origin ->
          True
    vr1b =
      case latestCertSeen of
        NotOrigin cert ->
          lcsCandidateBlockExtendsCert cert
        Origin ->
          True
    vr2a =
      case latestCertSeen of
        NotOrigin cert ->
          getPerasCertRound (lcsCert cert) + _R <= currRoundNo
        Origin ->
          _R <= currRoundNo
    vr2b =
      case latestCertOnChain of
        NotOrigin cert ->
          (currRoundNo > getPerasCertRound (lcocCert cert))
            && ( (currRoundNo `rmod` _K)
                   == (getPerasCertRound (lcocCert cert) `rmod` _K)
               )
        Origin ->
          currRoundNo `rmod` _K == _K - 1

    _X =
      SlotNo $
        unPerasCertArrivalThreshold $
          perasCertArrivalThreshold $
            perasParams
    _R =
      PerasRoundNo $
        unPerasIgnoranceRounds $
          perasIgnoranceRounds $
            perasParams
    _K =
      PerasRoundNo $
        unPerasCooldownRounds $
          perasCooldownRounds $
            perasParams

    rmod = onPerasRoundNo mod

-- | Test that the Peras voting rules can correctly decide when to vote based
-- on a simplified model that doesn't use anything fancy to evaluate the rules.
prop_isPerasVotingAllowed :: Property
prop_isPerasVotingAllowed = forAll genPerasVotingView $ \pvv -> do
  -- Determine whether we should vote according to the model
  let PerasVotingDecisionModel{shouldVote, vr1a, vr1b, vr2a, vr2b} =
        isPerasVotingAllowedModel pvv
  -- Some helper functions to report success/failure
  let chain = flip (foldr ($)) . reverse
  let ok desc =
        chain
          [ tabulate "VR-1A" [show vr1a]
          , tabulate "VR-1B" [show vr1b]
          , tabulate "VR-2A" [show vr2a]
          , tabulate "VR-2B" [show vr2b]
          , tabulate "VR-(1A|1B|2A|2B)" [show (vr1a, vr1b, vr2a, vr2b)]
          , tabulate "Should vote according to model" [show shouldVote]
          , tabulate "Actual result" [desc]
          ]
          $ property True
  let failure desc =
        counterexample desc $
          property False
  -- Now check that the real implementation agrees with the model
  let votingDecision = isPerasVotingAllowed pvv
  case votingDecision of
    Vote (ETrue voteReason)
      | shouldVote ->
          ok $ "Vote(" <> explainShallow voteReason <> ")"
      | otherwise ->
          failure $ "Expected not to vote, but got: " <> show votingDecision
    NoVote (EFalse noVoteReason)
      | not shouldVote ->
          ok $ "NoVote(" <> explainShallow noVoteReason <> ")"
      | otherwise ->
          failure $ "Expected to vote, but got: " <> show votingDecision

{-------------------------------------------------------------------------------
  Arbitrary helpers
-------------------------------------------------------------------------------}

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
    mkPerasParams
      { perasBlockMinSlots = PerasBlockMinSlots _L
      , perasCertArrivalThreshold = PerasCertArrivalThreshold _X
      , perasIgnoranceRounds = PerasIgnoranceRounds _R
      , perasCooldownRounds = PerasCooldownRounds _K
      }

-- * Slot numbers

genSlotNo :: Gen SlotNo
genSlotNo = do
  n <- arbitrary
  pure (SlotNo n)

-- * Peras round numbers

genPerasRoundNo :: Gen PerasRoundNo
genPerasRoundNo = do
  n <- arbitrary
  pure (PerasRoundNo n)

-- * Mocked certificate type

-- | A mocked certificate type for testing, so we don't have to deal with
-- development changes in the real certificate type.
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
  offset <- choose @Integer (-10, 3)
  -- NOTE: here we need to be careful not to underflow the round number
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

-- * Certificate and voting views

genLatestCertSeen :: PerasRoundNo -> Gen (LatestCertSeenView TestCert)
genLatestCertSeen roundNo = do
  cert <- genTestCert roundNo
  arrivalSlot <- genSlotNo
  roundStartSlot <- genSlotNo
  candidateBlockExtendsCert <- arbitrary
  pure
    LatestCertSeenView
      { lcsCert = cert
      , lcsArrivalSlot = arrivalSlot
      , lcsRoundStartSlot = roundStartSlot
      , lcsCandidateBlockExtendsCert = candidateBlockExtendsCert
      }

genLatestCertOnChain :: PerasRoundNo -> Gen (LatestCertOnChainView TestCert)
genLatestCertOnChain roundNo = do
  cert <- genTestCert roundNo
  pure $
    LatestCertOnChainView
      { lcocCert = cert
      }

genPerasVotingView :: Gen (PerasVotingView TestCert)
genPerasVotingView = do
  perasParams <- genPerasParams
  currRoundNo <- genPerasRoundNo
  latestCertSeen <- genWithOrigin (genLatestCertSeen currRoundNo)
  latestCertOnChain <- genWithOrigin (genLatestCertOnChain currRoundNo)
  pure
    PerasVotingView
      { perasParams
      , currRoundNo
      , latestCertSeen = latestCertSeen
      , latestCertOnChain = latestCertOnChain
      }
 where
  genWithOrigin gen =
    frequency
      [ (1, pure Origin)
      , (9, NotOrigin <$> gen)
      ]
