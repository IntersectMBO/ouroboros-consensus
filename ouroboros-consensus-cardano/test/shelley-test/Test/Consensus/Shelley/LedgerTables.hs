{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.Shelley.LedgerTables (tests) where

import           Cardano.Crypto.Hash (ShortHash)
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Protocol.Praos (Praos)
import           Ouroboros.Consensus.Protocol.TPraos (TPraos)
import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.HFEras ()
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import           Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import           Test.Cardano.Ledger.Babbage.Arbitrary ()
import           Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import           Test.Cardano.Ledger.Conway.Arbitrary ()
import           Test.Consensus.Shelley.Generators ()
import           Test.Consensus.Shelley.MockCrypto (CanMock, MockCrypto)
import           Test.LedgerTables
import           Test.Tasty
import           Test.Tasty.QuickCheck

type Crypto = MockCrypto ShortHash
type Proto  = TPraos Crypto

tests :: TestTree
tests = testGroup "LedgerTables"
  [ testGroup "Shelley"
    [ testProperty "Stowable laws" (prop_stowable_laws @(ShelleyBlock Proto (ShelleyEra Crypto)))
    , testProperty "HasLedgerTables laws" (prop_hasledgertables_laws @(ShelleyBlock Proto (ShelleyEra Crypto)))
    ]
  , testGroup "Allegra"
    [ testProperty "Stowable laws" (prop_stowable_laws @(ShelleyBlock Proto (AllegraEra Crypto)))
    , testProperty "HasLedgerTables laws" (prop_hasledgertables_laws @(ShelleyBlock Proto (AllegraEra Crypto)))
    ]
  , testGroup "Mary"
    [ testProperty "Stowable laws" (prop_stowable_laws @(ShelleyBlock Proto (MaryEra Crypto)))
    , testProperty "HasLedgerTables laws" (prop_hasledgertables_laws @(ShelleyBlock Proto (MaryEra Crypto)))
    ]
  , testGroup "Alonzo"
    [ testProperty "Stowable laws" (prop_stowable_laws @(ShelleyBlock Proto (AlonzoEra Crypto)))
    , testProperty "HasLedgerTables laws" (prop_hasledgertables_laws @(ShelleyBlock Proto (AlonzoEra Crypto)))
    ]
  , testGroup "Babbage"
    [ testProperty "Stowable laws" (prop_stowable_laws @(ShelleyBlock (Praos StandardCrypto) (BabbageEra StandardCrypto)))
    , testProperty "HasLedgerTables laws" (prop_hasledgertables_laws @(ShelleyBlock (Praos StandardCrypto) (BabbageEra StandardCrypto)))
    ]
  , testGroup "Conway"
    [ testProperty "Stowable laws" (prop_stowable_laws @(ShelleyBlock (Praos StandardCrypto) (ConwayEra StandardCrypto)))
    , testProperty "HasLedgerTables laws" (prop_hasledgertables_laws @(ShelleyBlock (Praos StandardCrypto) (ConwayEra StandardCrypto)))
    ]
  ]


instance ( CanMock proto era
         , Arbitrary (LedgerState (ShelleyBlock proto era) EmptyMK)
         ) => Arbitrary (LedgerTables (LedgerState (ShelleyBlock proto era)) ValuesMK) where
  arbitrary = projectLedgerTables . unstowLedgerTables <$> arbitrary
