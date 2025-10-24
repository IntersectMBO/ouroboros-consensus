{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test that the Peras voting rules can correctly decide when to vote.
module Test.Consensus.Peras.Voting (tests) where

import Data.Coerce (coerce)
import GHC.Generics (Generic)
import GHC.Word (Word64)
import Ouroboros.Consensus.Block.Abstract (SlotNo (..))
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
  )
import Ouroboros.Consensus.Peras.Voting (PerasVotingView (..), isPerasVotingAllowed)
import Ouroboros.Consensus.Util.Pred (explainShallow)
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
  , tabulate
  , testProperty
  )
import Test.Util.Orphans.Arbitrary (genNominalDiffTime50Years)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests =
  testGroup
    "Peras voting rules"
    [ testProperty "isPerasVotingAllowed" prop_isPerasVotingAllowed
    ]

{-------------------------------------------------------------------------------
  Model conformance test property
-------------------------------------------------------------------------------}

isPerasVotingAllowedModel ::
  PerasVotingView TestCert ->
  (Bool, (Bool, Bool, Bool, Bool))
isPerasVotingAllowedModel pvv =
  ( vr1a && vr1b || vr2a && vr2b
  , (vr1a, vr1b, vr2a, vr2b)
  )
 where
  vr1a =
    pvvCurrRoundNo pvv == latestCertSeenRoundNo <> _1
      && latestCertSeenArrivalSlot <= latestCertSeenRoundStart + _X
  vr1b =
    pvvCandidateExtendsCert pvv (pvvLatestCertSeen pvv)
  vr2a =
    latestCertSeenRoundNo <> _R <= currRoundNo
  vr2b =
    currRoundNo > latestCertOnChainRoundNo
      && (currRoundNo `rmod` _K == latestCertOnChainRoundNo `rmod` _K)

  _1 = coerce @Word64 1
  _X = coerce (perasCertArrivalThreshold (pvvPerasParams pvv))
  _R = coerce (perasIgnoranceRounds (pvvPerasParams pvv))
  _K = coerce (perasCooldownRounds (pvvPerasParams pvv))

  currRoundNo = pvvCurrRoundNo pvv
  latestCertSeenRoundNo = getPerasCertRound (pvvLatestCertSeen pvv)
  latestCertSeenRoundStart = pvvRoundStart pvv latestCertSeenRoundNo
  latestCertSeenArrivalSlot = pvvArrivalSlot pvv (pvvLatestCertSeen pvv)
  latestCertOnChainRoundNo = getPerasCertRound (pvvLatestCertOnChain pvv)

  rmod = onPerasRoundNo mod

-- | Test that the Peras voting rules can correctly decide when to vote based
-- on a simplified model that doesn't use anything fancy to evaluate the rules.
prop_isPerasVotingAllowed :: PerasVotingView' TestCert -> Property
prop_isPerasVotingAllowed pvv' = do
  -- Unpack the generated Peras voting view
  let pvv = toPerasVotingView pvv'
  -- Determine whether we should vote according to the model
  let (shouldVote, votingRules@(vr1a, vr1b, vr2a, vr2b)) =
        isPerasVotingAllowedModel pvv
  -- Prepare helper functions to report success/failure
  let compose = flip (foldr ($)) . reverse
  let ok desc =
        compose
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

genPerasParams :: Gen PerasParams
genPerasParams = do
  Positive (Small _L) <- arbitrary
  Positive (Small _X) <- arbitrary
  Positive (Small _R) <- arbitrary
  Positive (Small _K) <- arbitrary
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
  offset <- choose @Int (-5, 2)
  let roundNo' = PerasRoundNo (unPerasRoundNo roundNo + fromIntegral offset)
  pure $
    TestCert
      { tcArrivalTime = arrivalTime
      , tcRoundNo = roundNo'
      }

-- * Peras voting views

-- | A version of 'PerasVotingView' with all functions lifted to 'Fun'
data PerasVotingView' cert = PerasVotingView'
  { pvvPerasParams' :: PerasParams
  , pvvCurrRoundNo' :: PerasRoundNo
  , pvvLatestCertSeen' :: cert
  , pvvLatestCertOnChain' :: cert
  , pvvArrivalSlot' :: Fun cert (Small Word64)
  , pvvRoundStart' :: Fun PerasRoundNo (Small Word64)
  , pvvCandidateExtendsCert' :: Fun cert Bool
  }
  deriving Show -- the whole reason to have this type

instance Arbitrary (PerasVotingView' TestCert) where
  arbitrary = do
    perasParams <- genPerasParams
    currRoundNo <- genPerasRoundNo
    latestCertSeen <- genTestCert currRoundNo
    latestCertOnChain <- genTestCert currRoundNo
    arrivalSlot <- arbitrary
    roundStart <- arbitrary
    candidateExtendsCert <- arbitrary
    pure
      PerasVotingView'
        { pvvPerasParams' = perasParams
        , pvvCurrRoundNo' = currRoundNo
        , pvvLatestCertSeen' = latestCertSeen
        , pvvLatestCertOnChain' = latestCertOnChain
        , pvvArrivalSlot' = arrivalSlot
        , pvvRoundStart' = roundStart
        , pvvCandidateExtendsCert' = candidateExtendsCert
        }

toPerasVotingView :: PerasVotingView' TestCert -> PerasVotingView TestCert
toPerasVotingView pvv' =
  PerasVotingView
    { pvvPerasParams = pvvPerasParams' pvv'
    , pvvCurrRoundNo = pvvCurrRoundNo' pvv'
    , pvvLatestCertSeen = pvvLatestCertSeen' pvv'
    , pvvLatestCertOnChain = pvvLatestCertOnChain' pvv'
    , pvvArrivalSlot = SlotNo . getSmall <$> applyFun (pvvArrivalSlot' pvv')
    , pvvRoundStart = SlotNo . getSmall <$> applyFun (pvvRoundStart' pvv')
    , pvvCandidateExtendsCert = applyFun (pvvCandidateExtendsCert' pvv')
    }

-- * Orphan instances needed for 'Function' constraints

instance Function RelativeTime
instance Function PerasRoundNo
instance Function TestCert

instance CoArbitrary RelativeTime
instance CoArbitrary PerasRoundNo
instance CoArbitrary TestCert
