{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test that the Peras certificate inclusion rules can correctly decide when
-- to include a certificate
--
-- NOTE: in this file, we use uncommon variable names such as `_A` because that
-- is their name in the CIP-0140, and we can't have variable names starting
-- with capital letters. Contrary to typical Haskell conventions, those do not
-- denote ignored variables.
module Test.Consensus.Peras.Cert.Inclusion (tests) where

import GHC.Generics (Generic)
import Ouroboros.Consensus.Block (WithOrigin (..))
import Ouroboros.Consensus.Block.SupportsPeras
  ( HasPerasCertRound (..)
  , PerasRoundNo (..)
  , getPerasCertRound
  )
import Ouroboros.Consensus.Peras.Cert.Inclusion
  ( LatestCertOnChainView (..)
  , LatestCertSeenView (..)
  , PerasCertInclusionRulesDecision (..)
  , PerasCertInclusionView (..)
  , needCert
  )
import Ouroboros.Consensus.Peras.Params
  ( PerasCertMaxRounds (..)
  , PerasParams (..)
  , mkPerasParams
  )
import Ouroboros.Consensus.Storage.PerasCertDB.API (PerasCertSnapshot (..))
import Ouroboros.Consensus.Util.Pred (Evidence (..))
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
import Test.Util.QuickCheck (geometric)
import Test.Util.TestEnv (adjustQuickCheckTests)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests =
  adjustQuickCheckTests (* 1000) $
    testGroup
      "Peras certificate inclusion rules"
      [ testProperty "needCert" prop_needCert
      ]

{-------------------------------------------------------------------------------
  Model conformance test property
-------------------------------------------------------------------------------}

data PerasCertInclusionRulesDecisionModel
  = PerasCertInclusionDecisionModel
  { shouldIncludeCert :: Bool
  , noCertsFromTwoRoundsAgo :: Bool
  , latestCertSeenIsNotExpired :: Bool
  , latestCertSeenIsNewerThanLatestCertOnChain :: Bool
  }

-- | A simplified model of the Peras certificate inclusion rules, used to compare
-- against the real implementation. The main difference is that this model computes
-- the result of the predicate directly over the inputs, rather than using the
-- 'Pred' combinators to produce evidence in either direction.
--
-- NOTE: this predicate could be lifted directly from the agda specification.
needCertModel ::
  PerasCertInclusionView TestCert TestBlk ->
  PerasCertInclusionRulesDecisionModel
needCertModel
  PerasCertInclusionView
    { perasParams
    , currRoundNo
    , latestCertSeen
    , latestCertOnChain
    , certSnapshot
    } =
    PerasCertInclusionDecisionModel
      { shouldIncludeCert =
          noCertsFromTwoRoundsAgo
            && latestCertSeenIsNotExpired
            && latestCertSeenIsNewerThanLatestCertOnChain
      , noCertsFromTwoRoundsAgo =
          noCertsFromTwoRoundsAgo
      , latestCertSeenIsNotExpired =
          latestCertSeenIsNotExpired
      , latestCertSeenIsNewerThanLatestCertOnChain =
          latestCertSeenIsNewerThanLatestCertOnChain
      }
   where
    noCertsFromTwoRoundsAgo =
      if currRoundNo < 2
        then False
        else not (containsCert certSnapshot (currRoundNo - 2))

    latestCertSeenIsNotExpired =
      currRoundNo
        <= _A + getPerasCertRound (lcsCert latestCertSeen)

    latestCertSeenIsNewerThanLatestCertOnChain =
      case latestCertOnChain of
        Origin -> True
        NotOrigin lcoc ->
          getPerasCertRound (lcsCert latestCertSeen) > lcocRoundNo lcoc

    _A =
      PerasRoundNo $
        unPerasCertMaxRounds $
          perasCertMaxRounds $
            perasParams

-- | Test that the Peras certificate inclusion rules can correctly decide when
-- to include a certificate based on a simplified model that doesn't use anything
-- fancy to evaluate the rules.
prop_needCert :: Property
prop_needCert = forAll genPerasCertInclusionView $ \pciv -> do
  -- Determine whether we should include a cert according to the model
  let PerasCertInclusionDecisionModel
        { shouldIncludeCert
        , noCertsFromTwoRoundsAgo
        , latestCertSeenIsNotExpired
        , latestCertSeenIsNewerThanLatestCertOnChain
        } =
          needCertModel pciv
  -- Some helper functions to report success/failure
  let chain = flip (foldr ($)) . reverse
  let ok desc =
        chain
          [ tabulate "NoCertsFromTwoRoundsAgo" [show noCertsFromTwoRoundsAgo]
          , tabulate "LatestCertSeenIsNotExpired" [show latestCertSeenIsNotExpired]
          , tabulate
              "LatestCertSeenIsNewerThanLatestCertOnChain"
              [show latestCertSeenIsNewerThanLatestCertOnChain]
          , tabulate
              "NoCertsFromTwoRoundsAgo|LatestCertSeenIsNotExpired|LatestCertSeenIsNewerThanLatestCertOnChain"
              [ show
                  ( noCertsFromTwoRoundsAgo
                  , latestCertSeenIsNotExpired
                  , latestCertSeenIsNewerThanLatestCertOnChain
                  )
              ]
          , tabulate "Should include cert according to model" [show shouldIncludeCert]
          , tabulate "Actual result" [desc]
          ]
          $ property True
  let failure desc =
        counterexample desc $
          property False
  -- Now check that the real implementation agrees with the model
  let certInclusionDecision = needCert pciv
  case certInclusionDecision of
    IncludeCert (ETrue _includeCertReason) _cert
      | shouldIncludeCert ->
          ok $ certInclusionDecisionTag certInclusionDecision
      | otherwise ->
          failure $ "Expected not to include cert, but got: " <> show certInclusionDecision
    DoNotIncludeCert (EFalse _doNotIncludeCertReason)
      | not shouldIncludeCert ->
          ok $ certInclusionDecisionTag certInclusionDecision
      | otherwise ->
          failure $ "Expected to include cert, but got: " <> show certInclusionDecision

-- | Tag for the certificate inclusion decision result, used for tabulation.
--
-- We use this instead of 'explainShallow' to avoid creating too many unique
-- strings for tabulation (explainShallow includes round numbers which would
-- create millions of distinct cases, making the tabulation output useless).
certInclusionDecisionTag :: PerasCertInclusionRulesDecision cert -> String
certInclusionDecisionTag = \case
  IncludeCert{} -> "IncludeCert"
  DoNotIncludeCert{} -> "DoNotIncludeCert"

{-------------------------------------------------------------------------------
  Arbitrary helpers
-------------------------------------------------------------------------------}

-- * Peras parameters

-- NOTE: we use a geometric distribution to bias towards smaller values.
-- This increases the chance of covering all the inclusion rules more evenly,
-- while still allowing for larger values to be generated occasionally.
--
-- Moreover, geometric(0.5) + 1 means that:
--  - 50% chance of being 1
--  - 25% chance of being 2
--  - 12.5% chance of being 3
--  ... and so on
genPerasParams :: Gen PerasParams
genPerasParams = do
  _A <- fromIntegral . (+ 1) <$> geometric 0.5
  pure
    mkPerasParams
      { perasCertMaxRounds = PerasCertMaxRounds _A
      }

-- * Peras round numbers

-- | Generate a Peras round number.
--
-- We skew the distribution towards the first two rounds to cover the edge cases
-- in the certificate inclusion rules a bit more often.
genPerasRoundNo :: Gen PerasRoundNo
genPerasRoundNo =
  frequency
    [ (1, pure (PerasRoundNo 0))
    , (1, pure (PerasRoundNo 0))
    , (8, PerasRoundNo <$> arbitrary)
    ]

-- * Mocked certificate type

-- | A mocked certificate type for testing, so we don't have to deal with
-- development changes in the real certificate type.
data TestCert
  = TestCert
  { tcRoundNo :: PerasRoundNo
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
  offset <- choose @Integer (-10, 3)
  -- NOTE: here we need to be careful not to underflow the round number
  let roundNo' =
        PerasRoundNo $
          fromIntegral $
            max 0 $
              toInteger (unPerasRoundNo roundNo) + offset
  pure $
    TestCert
      { tcRoundNo = roundNo'
      }

-- * Mocked block type

-- | A mocked block type for testing
data TestBlk
  = TestBlk
  deriving (Show, Eq, Generic)

-- * Certificate and inclusion views

genLatestCertSeen :: PerasRoundNo -> Gen (LatestCertSeenView TestCert)
genLatestCertSeen roundNo = do
  cert <- genTestCert roundNo
  pure
    LatestCertSeenView
      { lcsCert = cert
      , lcsCertRound = getPerasCertRound cert
      }

genLatestCertOnChain :: PerasRoundNo -> Gen (LatestCertOnChainView TestCert)
genLatestCertOnChain roundNo = do
  cert <- genTestCert roundNo
  let certRoundNo = getPerasCertRound cert
  pure $
    LatestCertOnChainView
      { lcocRoundNo = certRoundNo
      }

genPerasCertSnapshot :: PerasRoundNo -> Gen (PerasCertSnapshot TestBlk)
genPerasCertSnapshot currRoundNo = do
  -- Decide whether to include a cert from two rounds ago
  containsCertFromTwoRoundsAgo <- arbitrary
  pure $
    PerasCertSnapshot
      { containsCert = \roundNo ->
          containsCertFromTwoRoundsAgo
            && roundNo == currRoundNo - 2
      , getCertsAfter = \_ ->
          mempty
      }

genPerasCertInclusionView :: Gen (PerasCertInclusionView TestCert TestBlk)
genPerasCertInclusionView = do
  perasParams <- genPerasParams
  currRoundNo <- genPerasRoundNo
  latestCertSeen <- genLatestCertSeen currRoundNo
  latestCertOnChain <- genWithOrigin (genLatestCertOnChain currRoundNo)
  certSnapshopt <- genPerasCertSnapshot currRoundNo
  pure
    PerasCertInclusionView
      { perasParams
      , currRoundNo
      , latestCertSeen = latestCertSeen
      , latestCertOnChain = latestCertOnChain
      , certSnapshot = certSnapshopt
      }
 where
  genWithOrigin gen =
    frequency
      [ (1, pure Origin)
      , (9, NotOrigin <$> gen)
      ]
