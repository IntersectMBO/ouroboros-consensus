{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Only Dijkstra allows an endorser block to hold anything, and it reads the
-- endorser-block capacity from its protocol parameters.
module Test.Consensus.Cardano.Capacity (tests) where

import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Dijkstra.PParams
  ( ppMaxEndorserBlockExUnitsL
  , ppMaxEndorserBlockReferencesSizeL
  , ppMaxEndorserBlockTxsSizeL
  , ppMaxRefScriptSizePerEndorserBlockL
  )
import qualified Cardano.Ledger.Genesis as Genesis
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..), OrdExUnits (..))
import Cardano.Ledger.Shelley.API (ShelleyGenesis (..))
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL, nesEsL)
import Cardano.Ledger.Shelley.Translation
  ( emptyFromByronTranslationContext
  )
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import qualified Data.Measure as Measure
import Data.Proxy (Proxy (..))
import Data.Word (Word32)
import Lens.Micro ((&), (.~))
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
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( ByteSize32 (..)
  , IgnoringOverflow (..)
  , TxLimits (..)
  , TxMeasure (..)
  )
import Ouroboros.Consensus.Ledger.Tables
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Protocol.Praos2 (Praos2)
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
import Ouroboros.Consensus.Shelley.Ledger.Mempool
  ( AlonzoMeasure (..)
  , DijkstraEbMeasure (..)
  , RefScriptSize (..)
  , fromExUnits
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
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
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
    , testProperty "Dijkstra" prop_dijkstra
    , testProperty
        "Dijkstra has no endorser block when the four endorser-block parameters are zero"
        prop_dijkstraDormant
    , testCase "Dijkstra transaction" test_dijkstraTxEbMeasure
    , testProperty
        "Dijkstra: an endorser-block measure fits only if each of its dimensions fits"
        prop_dijkstraEbMeasureFitsPerDimension
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

-- | Each of the four endorser-block parameters lands in its own field of the
-- endorser-block capacity. The values differ, so a swapped lens fails. The
-- references capacity is the parameter minus the 5 bytes of the widest CBOR map
-- header.
--
-- Few runs: setting the parameters forces the whole arbitrary ledger state,
-- which is slow to generate, and the result depends only on the parameters.
prop_dijkstra ::
  LedgerState (ShelleyBlock (Praos2 Crypto) DijkstraEra) EmptyMK ->
  Property
prop_dijkstra st =
  withNumTests 10 $
    forAllBlind arbitrary $ \translationContext ->
      let capacity =
            ebCapacityTxMeasure
              (fixedShelleyLedgerConfig translationContext)
              (withEndorserBlockParams 1001 (ExUnits 2002 3003) 4004 5005 (tickShelley st))
       in conjoin
            [ counterexample "endorser-block capacity" $
                capacity
                  === DijkstraEbMeasure
                    { ebClosureMeasure = TxMeasure closureAlonzo closureRefScripts
                    , txReferencesSize = IgnoringOverflow (ByteSize32 5000)
                    }
            , counterexample "mempool reservation for an endorser block" $
                mempoolEbReservation (Proxy @(ShelleyBlock (Praos2 Crypto) DijkstraEra)) capacity
                  === TxMeasure closureAlonzo closureRefScripts
            ]
 where
  closureAlonzo =
    AlonzoMeasure
      { byteSize = IgnoringOverflow (ByteSize32 1001)
      , exUnits = fromExUnits (ExUnits 2002 3003)
      }
  closureRefScripts = RefScriptSize (IgnoringOverflow (ByteSize32 4004))

-- | With the four endorser-block parameters at zero, Dijkstra has no endorser
-- block, as in Byron to Conway.
prop_dijkstraDormant ::
  LedgerState (ShelleyBlock (Praos Crypto) DijkstraEra) EmptyMK ->
  Property
prop_dijkstraDormant st =
  withNumTests 10 $
    forAllBlind arbitrary $ \translationContext ->
      prop_capacityHasNoEndorserBlock @(ShelleyBlock (Praos Crypto) DijkstraEra)
        (fixedShelleyLedgerConfig translationContext)
        (withEndorserBlockParams 0 (ExUnits 0 0) 0 0 (tickShelley st))

-- | A Dijkstra transaction costs its block measure in the closure, and the
-- reference 'Ouroboros.Consensus.Leios.Types.encodeLeiosEb' writes for its
-- byte size. The reference-scripts size differs from the byte size, so
-- reading the wrong field fails.
test_dijkstraTxEbMeasure :: Assertion
test_dijkstraTxEbMeasure =
  txEbMeasure
    (Proxy @(ShelleyBlock (Praos2 Crypto) DijkstraEra))
    (TxMeasure alonzo refScripts)
    @?= DijkstraEbMeasure
      { ebClosureMeasure = TxMeasure alonzo refScripts
      , -- 34 bytes for the hash, 3 bytes for a size of 300
        txReferencesSize = IgnoringOverflow (ByteSize32 37)
      }
 where
  alonzo =
    AlonzoMeasure
      { byteSize = IgnoringOverflow (ByteSize32 300)
      , exUnits = fromExUnits (ExUnits 1 2)
      }
  refScripts = RefScriptSize (IgnoringOverflow (ByteSize32 10))

-- | '<=' on 'DijkstraEbMeasure' holds only if each of its five dimensions is at
-- most the matching dimension on the right. So 'snapshotPartition' stops the
-- endorser-block part at the first transaction that overflows any
-- endorser-block parameter.
prop_dijkstraEbMeasureFitsPerDimension :: Property
prop_dijkstraEbMeasureFitsPerDimension =
  forAll genNumbers $ \xs ->
    forAll genNumbers $ \ys ->
      (ebMeasure xs Measure.<= ebMeasure ys) === and (zipWith (<=) xs ys)
 where
  -- Small numbers, so that a pair of numbers is often equal, smaller or larger.
  genNumbers = vectorOf 5 (choose (0, 2 :: Word32))

  -- The closure bytes, memory, steps and reference-script bytes, then the
  -- references size.
  ebMeasure [bytes, mem, steps, refScripts, refs] =
    DijkstraEbMeasure
      { ebClosureMeasure =
          TxMeasure
            AlonzoMeasure
              { byteSize = IgnoringOverflow (ByteSize32 bytes)
              , exUnits = fromExUnits (ExUnits (fromIntegral mem) (fromIntegral steps))
              }
            (RefScriptSize (IgnoringOverflow (ByteSize32 refScripts)))
      , txReferencesSize = IgnoringOverflow (ByteSize32 refs)
      }
  ebMeasure _ = error "ebMeasure: expected five numbers"

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

-- | Set the endorser-block txs size, ExUnits, reference-script size and
-- references size, in that order.
withEndorserBlockParams ::
  Word32 ->
  ExUnits ->
  Word32 ->
  Word32 ->
  TickedLedgerState (ShelleyBlock proto DijkstraEra) mk ->
  TickedLedgerState (ShelleyBlock proto DijkstraEra) mk
withEndorserBlockParams
  txsSize
  exUnits
  refScriptSize
  referencesSize
  (TickedShelleyLedgerState tip transition nes ledgerTables) =
    TickedShelleyLedgerState
      tip
      transition
      ( nes
          & nesEsL . curPParamsEpochStateL . ppMaxEndorserBlockTxsSizeL .~ txsSize
          & nesEsL . curPParamsEpochStateL . ppMaxEndorserBlockExUnitsL .~ OrdExUnits exUnits
          & nesEsL . curPParamsEpochStateL . ppMaxRefScriptSizePerEndorserBlockL .~ refScriptSize
          & nesEsL . curPParamsEpochStateL . ppMaxEndorserBlockReferencesSizeL .~ referencesSize
      )
      ledgerTables
