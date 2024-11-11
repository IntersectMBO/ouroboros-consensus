{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.Shelley.LedgerTables (tests) where

import qualified Cardano.Ledger.Api.Era as L
import           Data.Proxy
import           Data.SOP.BasicFunctors
import           Data.SOP.Constraint
import           Data.SOP.Strict
import           Ouroboros.Consensus.Cardano.Block (CardanoShelleyEras)
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.HFEras ()
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import           Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import           Test.Cardano.Ledger.Babbage.Arbitrary ()
import           Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import           Test.Cardano.Ledger.Conway.Arbitrary ()
import           Test.Consensus.Shelley.Generators ()
import           Test.Consensus.Shelley.MockCrypto (CanMock)
import           Test.LedgerTables
import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: TestTree
tests =
      testGroup "LedgerTables"
    . hcollapse
    . hcmap (Proxy @TestLedgerTables) (K . f)
    $ (hpure Proxy :: NP Proxy (CardanoShelleyEras StandardCrypto))
  where
    f :: forall blk. TestLedgerTables blk => Proxy blk -> TestTree
    f _ = testGroup (L.eraName @(ShelleyBlockLedgerEra blk))
        [ testProperty "Stowable laws" (prop_stowable_laws @blk)
        , testProperty "HasLedgerTables laws" (prop_hasledgertables_laws @blk)
        ]

class
  ( HasLedgerTables (LedgerState blk)
  , CanStowLedgerTables (LedgerState blk)
  , (Show `And` Arbitrary) (LedgerState blk EmptyMK)
  , (Show `And` Arbitrary) (LedgerState blk ValuesMK)
  , (Show `And` Arbitrary) (LedgerTables (LedgerState blk) ValuesMK)
  , L.Era (ShelleyBlockLedgerEra blk)
  ) => TestLedgerTables blk

instance
  ( HasLedgerTables (LedgerState blk)
  , CanStowLedgerTables (LedgerState blk)
  , (Show `And` Arbitrary) (LedgerState blk EmptyMK)
  , (Show `And` Arbitrary) (LedgerState blk ValuesMK)
  , (Show `And` Arbitrary) (LedgerTables (LedgerState blk) ValuesMK)
  , L.Era (ShelleyBlockLedgerEra blk)
  ) => TestLedgerTables blk

instance ( CanMock proto era
         , Arbitrary (LedgerState (ShelleyBlock proto era) EmptyMK)
         ) => Arbitrary (LedgerTables (LedgerState (ShelleyBlock proto era)) ValuesMK) where
  arbitrary = projectLedgerTables . unstowLedgerTables <$> arbitrary
