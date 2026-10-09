{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Only Dijkstra allows an endorser block to hold anything, and it reads the
-- endorser-block capacity from its protocol parameters.
--
-- Projecting the injection of a Cardano measure gives back that measure.
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
import Data.SOP.BasicFunctors (K (..), (:.:) (..))
import Data.SOP.Index (Index, hcimap, injectNS, projectNP)
import Data.SOP.Strict (NP (..), NS, hcollapse)
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
import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.HardFork.Combinator
  ( CanHardFork (..)
  , SingleEraBlock (..)
  , SingleEraInfo (..)
  , proxySingle
  )
import Ouroboros.Consensus.Ledger.Basics
  ( LedgerConfig
  , LedgerState
  , TickedLedgerState
  )
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( ByteSize32 (..)
  , IgnoringOverflow (..)
  , TrivialTxMeasurePhase2 (..)
  , TxLimits (..)
  , TxMeasure (..)
  )
import Ouroboros.Consensus.Ledger.Tables
import Ouroboros.Consensus.Protocol.Leios (Leios)
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
import Ouroboros.Consensus.Shelley.Ledger.Mempool
  ( AlonzoMeasure (..)
  , DijkstraEbMeasure (..)
  , RefScriptSize (..)
  , fromExUnits
  )
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.TypeFamilyWrappers
  ( WrapTxEbMeasure (..)
  , WrapTxMeasurePhase1 (..)
  , WrapTxMeasurePhase2 (..)
  )
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
import qualified Test.Util.QuickCheck as QC

type Crypto = MockCryptoCompatByron

tests :: TestTree
tests =
  testGroup
    "Measures"
    [ testGroup
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
        , testCase "Dijkstra transaction" test_dijkstraTxEbMeasure
        ]
    , testProperty "Projecting an injection gives back the measure" prop_projectionsGiveBackMeasures
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
  LedgerState (ShelleyBlock (Leios Crypto) DijkstraEra) EmptyMK ->
  Property
prop_dijkstra st =
  QC.withNumTests 10 $
    forAllBlind arbitrary $ \translationContext ->
      let capacity =
            ebCapacityTxMeasure
              (fixedShelleyLedgerConfig translationContext)
              (withEndorserBlockParams (tickShelley st))
       in conjoin
            [ counterexample "endorser-block capacity" $
                capacity
                  === DijkstraEbMeasure
                    { ebClosureMeasure = TxMeasure closureAlonzo closureRefScripts
                    , txReferencesSize = IgnoringOverflow (ByteSize32 5000)
                    }
            , counterexample "mempool reservation for an endorser block" $
                mempoolEbReservation (Proxy @(ShelleyBlock (Leios Crypto) DijkstraEra)) capacity
                  === TxMeasure closureAlonzo closureRefScripts
            ]
 where
  closureAlonzo =
    AlonzoMeasure
      { byteSize = IgnoringOverflow (ByteSize32 1001)
      , exUnits = fromExUnits (ExUnits 2002 3003)
      }
  closureRefScripts = RefScriptSize (IgnoringOverflow (ByteSize32 4004))

  withEndorserBlockParams (TickedShelleyLedgerState tip transition nes ledgerTables) =
    TickedShelleyLedgerState
      tip
      transition
      ( nes
          & nesEsL . curPParamsEpochStateL . ppMaxEndorserBlockTxsSizeL .~ 1001
          & nesEsL . curPParamsEpochStateL . ppMaxEndorserBlockExUnitsL
            .~ OrdExUnits (ExUnits 2002 3003)
          & nesEsL . curPParamsEpochStateL . ppMaxRefScriptSizePerEndorserBlockL .~ 4004
          & nesEsL . curPParamsEpochStateL . ppMaxEndorserBlockReferencesSizeL .~ 5005
      )
      ledgerTables

-- | A Dijkstra transaction costs its block measure in the closure, and the
-- reference 'Ouroboros.Consensus.Leios.Types.encodeLeiosEb' writes for its
-- byte size. The reference-scripts size differs from the byte size, so
-- reading the wrong field fails.
test_dijkstraTxEbMeasure :: Assertion
test_dijkstraTxEbMeasure =
  txEbMeasure (Proxy @(ShelleyBlock (Leios Crypto) DijkstraEra)) (TxMeasure alonzo refScripts)
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

{-------------------------------------------------------------------------------
  Measure projections
-------------------------------------------------------------------------------}

type Eras = CardanoEras Crypto

-- | For every era position, the projection of the injection of a measure of
-- that era gives back the measure. The generators give arbitrary execution
-- units and sizes, so a projection that drops or swaps a field fails.
prop_projectionsGiveBackMeasures :: Property
prop_projectionsGiveBackMeasures =
  conjoin
    [ counterexample "phase 1" $
        prop_projectionGivesBackMeasure
          hardForkInjTxMeasurePhase1
          hardForkProjTxMeasurePhase1
          (\x y -> unwrapTxMeasurePhase1 x === unwrapTxMeasurePhase1 y)
          genPhase1
    , counterexample "phase 2" $
        prop_projectionGivesBackMeasure
          hardForkInjTxMeasurePhase2
          hardForkProjTxMeasurePhase2
          (\x y -> unwrapTxMeasurePhase2 x === unwrapTxMeasurePhase2 y)
          genPhase2
    , counterexample "endorser block" $
        prop_projectionGivesBackMeasure
          hardForkInjTxEbMeasure
          hardForkProjTxEbMeasure
          (\x y -> unwrapTxEbMeasure x === unwrapTxEbMeasure y)
          genEb
    ]

prop_projectionGivesBackMeasure ::
  forall f m.
  (NS f Eras -> m) ->
  (m -> NP f Eras) ->
  (forall blk. SingleEraBlock blk => f blk -> f blk -> Property) ->
  NP (Gen :.: f) Eras ->
  Property
prop_projectionGivesBackMeasure inj proj eq gens =
  conjoin . hcollapse $ hcimap proxySingle check gens
 where
  check :: SingleEraBlock blk => Index Eras blk -> (Gen :.: f) blk -> K Property blk
  check idx (Comp gen) =
    K . counterexample (show (singleEraName (singleEraInfo idx))) $
      forAllBlind gen $ \x ->
        projectNP idx (proj (inj (injectNS idx x))) `eq` x

genPhase1 :: NP (Gen :.: WrapTxMeasurePhase1) Eras
genPhase1 =
  gen genByteSize
    :* gen genByteSize
    :* gen genByteSize
    :* gen genByteSize
    :* gen genAlonzoMeasure
    :* gen genAlonzoMeasure
    :* gen genAlonzoMeasure
    :* gen genAlonzoMeasure
    :* Nil
 where
  gen :: Gen (TxMeasurePhase1 blk) -> (Gen :.: WrapTxMeasurePhase1) blk
  gen = Comp . fmap WrapTxMeasurePhase1

genPhase2 :: NP (Gen :.: WrapTxMeasurePhase2) Eras
genPhase2 =
  gen genTrivial
    :* gen genTrivial
    :* gen genTrivial
    :* gen genTrivial
    :* gen genTrivial
    :* gen genTrivial
    :* gen genRefScriptSize
    :* gen genRefScriptSize
    :* Nil
 where
  gen :: Gen (TxMeasurePhase2 blk) -> (Gen :.: WrapTxMeasurePhase2) blk
  gen = Comp . fmap WrapTxMeasurePhase2

genEb :: NP (Gen :.: WrapTxEbMeasure) Eras
genEb =
  gen (TxMeasure <$> genByteSize <*> genTrivial)
    :* gen (TxMeasure <$> genByteSize <*> genTrivial)
    :* gen (TxMeasure <$> genByteSize <*> genTrivial)
    :* gen (TxMeasure <$> genByteSize <*> genTrivial)
    :* gen (TxMeasure <$> genAlonzoMeasure <*> genTrivial)
    :* gen (TxMeasure <$> genAlonzoMeasure <*> genTrivial)
    :* gen (TxMeasure <$> genAlonzoMeasure <*> genRefScriptSize)
    :* gen
      ( DijkstraEbMeasure
          <$> (TxMeasure <$> genAlonzoMeasure <*> genRefScriptSize)
          <*> genByteSize
      )
    :* Nil
 where
  gen :: Gen (TxEbMeasure blk) -> (Gen :.: WrapTxEbMeasure) blk
  gen = Comp . fmap WrapTxEbMeasure

genByteSize :: Gen (IgnoringOverflow ByteSize32)
genByteSize = IgnoringOverflow . ByteSize32 <$> arbitrary

genAlonzoMeasure :: Gen AlonzoMeasure
genAlonzoMeasure = AlonzoMeasure <$> genByteSize <*> (fromExUnits <$> arbitrary)

genRefScriptSize :: Gen RefScriptSize
genRefScriptSize = RefScriptSize <$> genByteSize

genTrivial :: Gen TrivialTxMeasurePhase2
genTrivial = pure TrivialTxMeasurePhase2

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
