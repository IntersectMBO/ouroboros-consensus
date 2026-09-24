{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | No Cardano era allows an endorser block to hold anything.
module Test.Consensus.Cardano.Capacity (tests) where

import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Genesis as Genesis
import Cardano.Ledger.Shelley.API (ShelleyGenesis (..))
import Cardano.Ledger.Shelley.Translation
  ( emptyFromByronTranslationContext
  )
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import qualified Data.Measure as Measure
import Data.Proxy (Proxy (..))
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( slotLengthFromSec
  )
import Ouroboros.Consensus.Byron.Ledger
  ( ByronBlock
  , byronLedgerState
  , byronLedgerTransition
  )
import Ouroboros.Consensus.Byron.Ledger.Ledger
  ( Ticked (TickedByronLedgerState)
  )
import Ouroboros.Consensus.Ledger.Basics
  ( LedgerConfig
  , LedgerState
  , TickedLedgerState
  )
import Ouroboros.Consensus.Ledger.SupportsMempool (TxLimits (..))
import Ouroboros.Consensus.Ledger.Tables
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Shelley.Eras
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger
  ( ShelleyBlock
  , ShelleyLedgerConfig
  , mkShelleyLedgerConfig
  )
import Ouroboros.Consensus.Shelley.Ledger.Ledger
  ( LedgerState (ShelleyLedgerState)
  , Ticked (TickedShelleyLedgerState)
  )
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Examples (testShelleyGenesis)
import Test.Consensus.Byron.Generators
  ( genByronLedgerConfig
  , genByronLedgerState
  )
import Test.Consensus.Cardano.MockCrypto (MockCryptoCompatByron)
import Test.Consensus.Shelley.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck

type Crypto = MockCryptoCompatByron

tests :: TestTree
tests =
  testGroup
    "Endorser-block capacity"
    [ testProperty "Byron" prop_byron
    , testProperty "Shelley" $
        prop_shelleyBased @(TPraos Crypto) @ShelleyEra
          (pure emptyFromByronTranslationContext)
    , testProperty "Allegra" $
        prop_shelleyBased @(TPraos Crypto) @AllegraEra (pure Genesis.NoGenesis)
    , testProperty "Mary" $
        prop_shelleyBased @(TPraos Crypto) @MaryEra (pure Genesis.NoGenesis)
    , testProperty "Alonzo" $
        prop_shelleyBased @(TPraos Crypto) @AlonzoEra arbitrary
    , testProperty "Babbage" $
        prop_shelleyBased @(Praos Crypto) @BabbageEra (pure Genesis.NoGenesis)
    , testProperty "Conway" $
        prop_shelleyBased @(Praos Crypto) @ConwayEra arbitrary
    , testProperty "Dijkstra" $
        prop_shelleyBased @(Praos Crypto) @DijkstraEra arbitrary
    ]

-- | Both endorser-block measures are zero.
prop_capacityHasNoEndorserBlock ::
  forall blk mk.
  TxLimits blk =>
  LedgerConfig blk ->
  TickedLedgerState blk mk ->
  Property
prop_capacityHasNoEndorserBlock cfg st =
  conjoin
    [ counterexample "endorser-block capacity" $
        ebCapacityTxMeasure cfg st === Measure.zero
    , counterexample "mempool reservation for an endorser block" $
        mempoolEbReservation (Proxy @blk) (ebCapacityTxMeasure cfg st) === Measure.zero
    ]

prop_byron :: Property
prop_byron =
  forAll genByronLedgerConfig $ \cfg ->
    forAll genByronLedgerState $ \st ->
      prop_capacityHasNoEndorserBlock @ByronBlock cfg (tickByron st)

prop_shelleyBased ::
  forall proto era.
  TxLimits (ShelleyBlock proto era) =>
  Gen (Core.TranslationContext era) ->
  LedgerState (ShelleyBlock proto era) EmptyMK ->
  Property
prop_shelleyBased genTranslationContext st =
  forAllBlind genTranslationContext $ \translationContext ->
    prop_capacityHasNoEndorserBlock @(ShelleyBlock proto era)
      (fixedShelleyLedgerConfig translationContext)
      (tickShelley st)

{-------------------------------------------------------------------------------
  Fixtures
-------------------------------------------------------------------------------}

fixedShelleyLedgerConfig ::
  Core.TranslationContext era ->
  ShelleyLedgerConfig era
fixedShelleyLedgerConfig translationContext =
  mkShelleyLedgerConfig
    testShelleyGenesis
    translationContext
    (fixedEpochInfo (sgEpochLength testShelleyGenesis) (slotLengthFromSec 2))

tickByron :: LedgerState ByronBlock mk -> TickedLedgerState ByronBlock mk
tickByron st =
  TickedByronLedgerState
    (byronLedgerState st)
    (byronLedgerTransition st)

tickShelley ::
  LedgerState (ShelleyBlock proto era) EmptyMK ->
  TickedLedgerState (ShelleyBlock proto era) EmptyMK
tickShelley (ShelleyLedgerState tip state transition ledgerTables) =
  TickedShelleyLedgerState tip transition state ledgerTables
