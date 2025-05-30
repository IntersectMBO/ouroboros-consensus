{-# LANGUAGE NamedFieldPuns #-}

module Test.Consensus.Cardano.SupportsSanityCheck (tests) where

import Cardano.Ledger.BaseTypes (nonZero, nonZeroOr, unNonZero)
import Ouroboros.Consensus.Cardano (CardanoHardForkTriggers)
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Combinator.Basics
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Test.Consensus.Cardano.ProtocolInfo
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Gen as Gen
import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Test.ThreadNet.Infra.Shelley as Shelley
import Test.Util.SanityCheck

tests :: TestTree
tests =
  testGroup
    "SupportsSanityCheck"
    [ testProperty "cardano block top level config passes a sanity check" prop_cardanoBlockSanityChecks
    , testProperty
        "intentionally-misconfigured top level config fails a sanity check"
        prop_intentionallyBrokenConfigDoesNotSanityCheck
    ]

prop_cardanoBlockSanityChecks :: QC.Property
prop_cardanoBlockSanityChecks =
  forAllBlind genSimpleTestProtocolInfo (prop_sanityChecks . pInfoConfig)

prop_intentionallyBrokenConfigDoesNotSanityCheck :: QC.Property
prop_intentionallyBrokenConfigDoesNotSanityCheck =
  forAllBlind genSimpleTestProtocolInfo $ \pinfo ->
    let saneTopLevelConfig =
          pInfoConfig pinfo
        brokenConfig = breakTopLevelConfig saneTopLevelConfig
     in expectFailure $ prop_sanityChecks brokenConfig

breakTopLevelConfig ::
  TopLevelConfig (CardanoBlock StandardCrypto) -> TopLevelConfig (CardanoBlock StandardCrypto)
breakTopLevelConfig tlc =
  let TopLevelConfig{topLevelConfigProtocol} = tlc
      HardForkConsensusConfig{hardForkConsensusConfigK} = topLevelConfigProtocol
      k = unNonZero $ maxRollbacks hardForkConsensusConfigK
   in tlc
        { topLevelConfigProtocol =
            topLevelConfigProtocol
              { hardForkConsensusConfigK =
                  SecurityParam $
                    nonZeroOr (succ k) $
                      error "Impossible! In breakTopLevelConfig, found zero, expected a positive number."
              }
        }

genSimpleTestProtocolInfo :: Gen (ProtocolInfo (CardanoBlock StandardCrypto))
genSimpleTestProtocolInfo = do
  setup <- arbitrary
  pure $
    mkSimpleTestProtocolInfo
      (decentralizationParam setup)
      (securityParam setup)
      (byronSlotLength setup)
      (shelleySlotLength setup)
      protocolVersionZero
      (hardForkTriggers setup)

data SimpleTestProtocolInfoSetup = SimpleTestProtocolInfoSetup
  { decentralizationParam :: Shelley.DecentralizationParam
  , securityParam :: SecurityParam
  , byronSlotLength :: ByronSlotLengthInSeconds
  , shelleySlotLength :: ShelleySlotLengthInSeconds
  , hardForkTriggers :: CardanoHardForkTriggers
  }

instance Arbitrary SimpleTestProtocolInfoSetup where
  arbitrary = do
    SimpleTestProtocolInfoSetup
      <$> arbitrary
      <*> genSecurityParam
      <*> genByronSlotLength
      <*> genShelleySlotLength
      <*> genHardForkTriggers
   where
    genSecurityParam =
      SecurityParam <$> Gen.choose (8, 12) `suchThatMap` nonZero
    genByronSlotLength =
      ByronSlotLengthInSeconds <$> Gen.choose (1, 4)
    genShelleySlotLength =
      ShelleySlotLengthInSeconds <$> Gen.choose (1, 4)
    genHardForkTriggers =
      hardForkInto <$> Gen.chooseEnum (Byron, Conway)
