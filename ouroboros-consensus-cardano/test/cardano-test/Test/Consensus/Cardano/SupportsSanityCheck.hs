{-# LANGUAGE NamedFieldPuns #-}
module Test.Consensus.Cardano.SupportsSanityCheck (tests) where

import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Node.ProtocolInfo
import Test.Consensus.Cardano.ProtocolInfo
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Util.SanityCheck
import qualified Test.QuickCheck.Gen as Gen
import qualified Test.QuickCheck as QC
import qualified Test.ThreadNet.Infra.Shelley as Shelley
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Combinator.Basics

tests :: TestTree
tests = testGroup "SupportsSanityCheck"
  [ testProperty "cardano block top level config passes a sanity check" prop_cardanoBlockSanityChecks
  , testProperty "intentionally-misconfigured top level config fails a sanity check" prop_intentionallyBrokenConfigDoesNotSanityCheck
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

breakTopLevelConfig :: TopLevelConfig (CardanoBlock StandardCrypto) -> TopLevelConfig (CardanoBlock StandardCrypto)
breakTopLevelConfig tlc =
  let TopLevelConfig{topLevelConfigProtocol} = tlc
      HardForkConsensusConfig{hardForkConsensusConfigK} = topLevelConfigProtocol
      SecurityParam k = hardForkConsensusConfigK
  in tlc
    { topLevelConfigProtocol = topLevelConfigProtocol
      { hardForkConsensusConfigK = SecurityParam (succ k)
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
      (hardForkSpec setup)

data SimpleTestProtocolInfoSetup = SimpleTestProtocolInfoSetup
  { decentralizationParam :: Shelley.DecentralizationParam
  , securityParam :: SecurityParam
  , byronSlotLength :: ByronSlotLengthInSeconds
  , shelleySlotLength :: ShelleySlotLengthInSeconds
  , hardForkSpec :: HardForkSpec
  }

instance Arbitrary SimpleTestProtocolInfoSetup where
  arbitrary = do
    SimpleTestProtocolInfoSetup
      <$> arbitrary
      <*> genSecurityParam
      <*> genByronSlotLength
      <*> genShelleySlotLength
      <*> genHardForkSpec
    where
      genSecurityParam =
        SecurityParam <$> Gen.choose (8, 12)
      genByronSlotLength =
        ByronSlotLengthInSeconds <$> Gen.choose (1, 4)
      genShelleySlotLength =
        ShelleySlotLengthInSeconds <$> Gen.choose (1, 4)
      genHardForkSpec =
        hardForkInto <$> Gen.chooseEnum (Byron, Conway)
