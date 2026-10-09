{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-x-ord-preserving-coercions #-}
#if __GLASGOW_HASKELL__ < 908
{-# OPTIONS_GHC -Wno-unrecognised-warning-flags #-}
#endif

module Test.Consensus.Cardano.Translation (forecastTests, tests) where

import qualified Cardano.Chain.Block as Byron
import qualified Cardano.Chain.UTxO as Byron
import Cardano.Ledger.Alonzo ()
import Cardano.Ledger.BaseTypes (TxIx (..))
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Genesis as Genesis
import Cardano.Ledger.Shelley.API
  ( NewEpochState (stashedAVVMAddresses)
  , ShelleyGenesis (..)
  , TxIn (..)
  , translateCompactTxOutByronToShelley
  , translateTxIdByronToShelley
  )
import Cardano.Ledger.Shelley.LedgerState
  ( esLState
  , lsUTxOState
  , nesEs
  , utxosUtxo
  )
import Cardano.Ledger.Shelley.Translation
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Slot (EpochNo (..))
import qualified Data.Map.Strict as Map
import Data.SOP.InPairs (RequiringBoth (..), provideBoth)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime (..)
  , slotLengthFromSec
  )
import Ouroboros.Consensus.Ledger.Abstract (ledgerTipSlot)
import Ouroboros.Consensus.Byron.Ledger (ByronBlock, byronLedgerState)
import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.Cardano.CanHardFork ()
import Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis (..))
import Cardano.Ledger.Dijkstra.PParams (UpgradeDijkstraPParams (..))
import Control.Monad.Except (runExcept)
import Ouroboros.Consensus.HardFork.History (Bound (..))
import Ouroboros.Consensus.Block (PerasRoundNo (..), succWithOrigin)
import Ouroboros.Consensus.HardFork.Combinator
  ( EraTranslation (..)
  , InPairs (..)
  , hardForkEraTranslation
  )
import Ouroboros.Consensus.HardFork.Combinator.State.Types
  ( CrossEraForecaster (..)
  , TranslateLedgerState (..)
  )
import Ouroboros.Consensus.Ledger.Basics
  ( LedgerCfg
  , LedgerConfig
  , LedgerState
  )
import Ouroboros.Consensus.Ledger.Tables hiding (TxIn)
import Ouroboros.Consensus.Ledger.Tables.Diff (Diff)
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Ouroboros.Consensus.Protocol.Praos
import Ouroboros.Consensus.Protocol.Praos2 (LeiosOnly (Praos2HasLeios), Praos2)
import qualified Ouroboros.Consensus.Protocol.Praos.Views as Views
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Shelley.Eras
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger
  ( BigEndianTxIn (..)
  , ShelleyBlock
  , ShelleyLedgerConfig
  , ShelleyLedgerConfig (..)
  , mkShelleyLedgerConfig
  , shelleyLedgerState
  , shelleyLedgerTables
  )
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util
import Test.Cardano.Ledger.Alonzo.Binary.Twiddle ()
import Test.Cardano.Ledger.Babbage.Binary.Twiddle ()
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Examples
import Test.Consensus.Byron.Generators
  ( genByronLedgerConfig
  , genByronLedgerState
  )
import Test.Consensus.Cardano.MockCrypto (MockCryptoCompatByron)
import Test.Consensus.Shelley.Generators ()
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

-- Definitions to make the signatures a bit less unwieldy
type Crypto = MockCryptoCompatByron
type Proto = TPraos Crypto

tests :: TestTree
tests =
  testGroup
    "UpdateTablesOnEraTransition"
    [ testTablesTranslation
        "Byron to Shelley"
        byronToShelleyLedgerStateTranslation
        byronUtxosAreInsertsInShelleyUtxoDiff
        ( \st ->
            cover 50 (nonEmptyUtxosByron st) "UTxO set is not empty"
              -- The Byron ledger generators are very
              -- unlikely to generate an empty UTxO, but we
              -- want to test with the empty UTxO as well.
              -- See 'Test.Cardano.Chain.UTxO.Gen.genUTxO'
              -- and the @Arbitrary
              -- 'Cardano.Chain.UTxO.UTxO'@ instance in
              -- "Test.Consensus.Byron.Generators".
              . cover 0.1 (not $ nonEmptyUtxosByron st) "UTxO set is empty"
        )
    , testTablesTranslation
        "Shelley to Allegra"
        shelleyToAllegraLedgerStateTranslation
        shelleyAvvmAddressesAreDeletesInUtxoDiff
        (\st -> cover 50 (nonEmptyAvvmAddresses st) "AVVM set is not empty")
    , testTablesTranslation
        "Allegra to Mary"
        allegraToMaryLedgerStateTranslation
        utxoTablesAreEmpty
        (\st -> cover 50 (nonEmptyUtxosShelley st) "UTxO set is not empty")
    , testTablesTranslation
        "Mary to Alonzo"
        maryToAlonzoLedgerStateTranslation
        utxoTablesAreEmpty
        (\st -> cover 50 (nonEmptyUtxosShelley st) "UTxO set is not empty")
    , testTablesTranslation
        "Alonzo to Babbage"
        alonzoToBabbageLedgerStateTranslation
        utxoTablesAreEmpty
        (\st -> cover 50 (nonEmptyUtxosShelley st) "UTxO set is not empty")
    , testTablesTranslation
        "Babbage to Conway"
        babbageToConwayLedgerStateTranslation
        utxoTablesAreEmpty
        (\st -> cover 50 (nonEmptyUtxosShelley st) "UTxO set is not empty")
    , testTablesTranslation
        "Conway to Dijkstra"
        conwayToDijkstraLedgerStateTranslation
        utxoTablesAreEmpty
        (\st -> cover 50 (nonEmptyUtxosShelley st) "UTxO set is not empty")
    ]

{-------------------------------------------------------------------------------
  Ledger-state translations between eras that we test in this module
-------------------------------------------------------------------------------}

-- | TODO: we should simply expose 'translateLedgerStateByronToShelleyWrapper'
-- and other translations in ' Ouroboros.Consensus.Cardano.CanHardFork'.
byronToShelleyLedgerStateTranslation ::
  RequiringBoth
    WrapLedgerConfig
    TranslateLedgerState
    ByronBlock
    (ShelleyBlock (TPraos Crypto) ShelleyEra)
shelleyToAllegraLedgerStateTranslation ::
  RequiringBoth
    WrapLedgerConfig
    TranslateLedgerState
    (ShelleyBlock (TPraos Crypto) ShelleyEra)
    (ShelleyBlock (TPraos Crypto) AllegraEra)
allegraToMaryLedgerStateTranslation ::
  RequiringBoth
    WrapLedgerConfig
    TranslateLedgerState
    (ShelleyBlock (TPraos Crypto) AllegraEra)
    (ShelleyBlock (TPraos Crypto) MaryEra)
maryToAlonzoLedgerStateTranslation ::
  RequiringBoth
    WrapLedgerConfig
    TranslateLedgerState
    (ShelleyBlock (TPraos Crypto) MaryEra)
    (ShelleyBlock (TPraos Crypto) AlonzoEra)
alonzoToBabbageLedgerStateTranslation ::
  RequiringBoth
    WrapLedgerConfig
    TranslateLedgerState
    (ShelleyBlock (TPraos Crypto) AlonzoEra)
    (ShelleyBlock (Praos Crypto) BabbageEra)
babbageToConwayLedgerStateTranslation ::
  RequiringBoth
    WrapLedgerConfig
    TranslateLedgerState
    (ShelleyBlock (Praos Crypto) BabbageEra)
    (ShelleyBlock (Praos Crypto) ConwayEra)
conwayToDijkstraLedgerStateTranslation ::
  RequiringBoth
    WrapLedgerConfig
    TranslateLedgerState
    (ShelleyBlock (Praos Crypto) ConwayEra)
    (ShelleyBlock (Praos2 Crypto) DijkstraEra)
PCons
  byronToShelleyLedgerStateTranslation
  ( PCons
      shelleyToAllegraLedgerStateTranslation
      ( PCons
          allegraToMaryLedgerStateTranslation
          ( PCons
              maryToAlonzoLedgerStateTranslation
              ( PCons
                  alonzoToBabbageLedgerStateTranslation
                  ( PCons
                      babbageToConwayLedgerStateTranslation
                      ( PCons
                          conwayToDijkstraLedgerStateTranslation
                          PNil
                        )
                    )
                )
            )
        )
    ) = tls
   where
    tls ::
      InPairs
        (RequiringBoth WrapLedgerConfig TranslateLedgerState)
        (CardanoEras Crypto)
    tls = translateLedgerState hardForkEraTranslation

-- | Check that the tables are correctly translated from one era to the next.
testTablesTranslation ::
  forall srcBlk dstBlk.
  ( Arbitrary (TestSetup srcBlk dstBlk)
  , Show (LedgerCfg LedgerState srcBlk)
  , Show (LedgerCfg LedgerState dstBlk)
  , Show (LedgerState srcBlk EmptyMK)
  ) =>
  -- | Property label
  String ->
  RequiringBoth
    WrapLedgerConfig
    TranslateLedgerState
    srcBlk
    dstBlk ->
  (LedgerState srcBlk EmptyMK -> LedgerState dstBlk DiffMK -> Bool) ->
  -- | Coverage testing function
  (LedgerState srcBlk EmptyMK -> Property -> Property) ->
  TestTree
testTablesTranslation propLabel translateWithConfig translationShouldSatisfy ledgerStateShouldCover =
  testProperty propLabel withTestSetup
 where
  withTestSetup :: TestSetup srcBlk dstBlk -> Property
  withTestSetup ts =
    checkCoverage $
      ledgerStateShouldCover tsSrcLedgerState $
        property $
          translationShouldSatisfy tsSrcLedgerState destState
   where
    TestSetup{tsSrcLedgerConfig, tsDestLedgerConfig, tsSrcLedgerState, tsEpochNo} = ts
    destState = translateLedgerStateWith translation tsEpochNo tsSrcLedgerState
     where
      translation :: TranslateLedgerState srcBlk dstBlk
      translation =
        provideBoth
          translateWithConfig
          (WrapLedgerConfig tsSrcLedgerConfig)
          (WrapLedgerConfig tsDestLedgerConfig)


{-------------------------------------------------------------------------------
  Forecasting across an era boundary
-------------------------------------------------------------------------------}

forecastTests :: TestTree
forecastTests =
  testGroup
    "CrossEraForecast"
    [ testProperty
        "Conway to Dijkstra takes the Leios parameters from the Dijkstra genesis"
        prop_conwayToDijkstraForecast
    ]


-- | The combinator's own Conway-to-Dijkstra forecaster, as 'CanHardFork' wires it.
conwayToDijkstraForecast ::
  RequiringBoth
    WrapLedgerConfig
    (CrossEraForecaster LedgerState WrapLedgerView)
    (ShelleyBlock (Praos Crypto) ConwayEra)
    (ShelleyBlock (Praos2 Crypto) DijkstraEra)
PCons _ (PCons _ (PCons _ (PCons _ (PCons _ (PCons _ (PCons conwayToDijkstraForecast PNil)))))) =
  crossEraForecast (hardForkEraTranslation :: EraTranslation (CardanoEras Crypto))

-- | Forecasting a Dijkstra ledger view from a Conway ledger state, for the
-- first slot past a boundary just after its tip, gives the Leios parameters
-- the Dijkstra genesis sets.
prop_conwayToDijkstraForecast ::
  TestSetup
    (ShelleyBlock (Praos Crypto) ConwayEra)
    (ShelleyBlock (Praos2 Crypto) DijkstraEra) ->
  Property
prop_conwayToDijkstraForecast ts =
  case runExcept forecast of
    Left err -> counterexample (show err) False
    Right (WrapLedgerView lv) -> leiosParameters lv === expected
 where
  TestSetup{tsSrcLedgerConfig, tsDestLedgerConfig, tsSrcLedgerState, tsEpochNo} = ts
  slot = succWithOrigin (ledgerTipSlot tsSrcLedgerState)
  transition =
    Bound
      { boundTime = RelativeTime 0
      , boundSlot = slot
      , boundEpoch = tsEpochNo
      , boundNextPerasRound = PerasRoundNo 0
      }
  forecast =
    crossEraForecastWith
      ( provideBoth
          conwayToDijkstraForecast
          (WrapLedgerConfig tsSrcLedgerConfig)
          (WrapLedgerConfig tsDestLedgerConfig)
      )
      transition
      slot
      tsSrcLedgerState
  DijkstraGenesis{dgUpgradePParams = upgrade} = shelleyLedgerTranslationContext tsDestLedgerConfig
  expected =
    ( udppMaxEndorserBlockReferencesSize upgrade
    , udppMaxEndorserBlockTxsSize upgrade
    , udppLeiosAnnouncementPeriodLength upgrade
    , udppLeiosVotePeriodLength upgrade
    , udppLeiosDiffusionPeriodLength upgrade
    , udppLeiosQuorumStakeThreshold upgrade
    )
  leiosParameters
    Views.PraosLedgerView
      { Views.plvMaxEbBodySize = Praos2HasLeios maxEbBodySize
      , Views.plvMaxEbTxsSize = Praos2HasLeios maxEbTxsSize
      , Views.plvAnnouncementPeriodLength = Praos2HasLeios announcementPeriodLength
      , Views.plvVotePeriodLength = Praos2HasLeios votePeriodLength
      , Views.plvDiffusionPeriodLength = Praos2HasLeios diffusionPeriodLength
      , Views.plvQuorumStakeThreshold = Praos2HasLeios quorumStakeThreshold
      } =
      ( maxEbBodySize
      , maxEbTxsSize
      , announcementPeriodLength
      , votePeriodLength
      , diffusionPeriodLength
      , quorumStakeThreshold
      )

{-------------------------------------------------------------------------------
    Specific predicates
-------------------------------------------------------------------------------}

byronUtxosAreInsertsInShelleyUtxoDiff ::
  LedgerState ByronBlock EmptyMK ->
  LedgerState (ShelleyBlock Proto ShelleyEra) DiffMK ->
  Bool
byronUtxosAreInsertsInShelleyUtxoDiff srcLedgerState destLedgerState =
  toNextUtxoDiff srcLedgerState == extractUtxoDiff destLedgerState
 where
  toNextUtxoDiff ::
    LedgerState ByronBlock mk ->
    Diff.Diff BigEndianTxIn (Core.TxOut ShelleyEra)
  toNextUtxoDiff ledgerState =
    let
      Byron.UTxO utxo = Byron.cvsUtxo $ byronLedgerState ledgerState
      keyFn = translateTxInByronToShelley . Byron.fromCompactTxIn
      valFn = Diff.Insert . translateCompactTxOutByronToShelley
     in
      Diff.Diff $ Map.map valFn $ Map.mapKeys keyFn utxo

  translateTxInByronToShelley :: Byron.TxIn -> BigEndianTxIn
  translateTxInByronToShelley byronTxIn =
    let
      Byron.TxInUtxo txId txIx = byronTxIn
      shelleyTxId' = translateTxIdByronToShelley txId
     in
      BigEndianTxIn $ TxIn shelleyTxId' (TxIx txIx)

shelleyAvvmAddressesAreDeletesInUtxoDiff ::
  LedgerState (ShelleyBlock Proto ShelleyEra) EmptyMK ->
  LedgerState (ShelleyBlock Proto AllegraEra) DiffMK ->
  Bool
shelleyAvvmAddressesAreDeletesInUtxoDiff srcLedgerState destLedgerState =
  toNextUtxoDiff srcLedgerState == extractUtxoDiff destLedgerState
 where
  toNextUtxoDiff ::
    LedgerState (ShelleyBlock Proto ShelleyEra) EmptyMK ->
    Diff.Diff BigEndianTxIn (Core.TxOut AllegraEra)
  toNextUtxoDiff = avvmAddressesToUtxoDiff . stashedAVVMAddresses . shelleyLedgerState
  avvmAddressesToUtxoDiff (UTxO m) = Diff.Diff $ coerceMapKeys $ Map.map (\_ -> Diff.Delete) m

utxoTablesAreEmpty ::
  LedgerState (ShelleyBlock srcProto srcEra) EmptyMK ->
  LedgerState (ShelleyBlock destProto destEra) DiffMK ->
  Bool
utxoTablesAreEmpty _ destLedgerState = Diff.null $ extractUtxoDiff destLedgerState

nonEmptyUtxosByron :: LedgerState ByronBlock EmptyMK -> Bool
nonEmptyUtxosByron ledgerState =
  let Byron.UTxO utxo = Byron.cvsUtxo $ byronLedgerState ledgerState
   in not $ Map.null utxo

nonEmptyUtxosShelley :: LedgerState (ShelleyBlock proto era) EmptyMK -> Bool
nonEmptyUtxosShelley ledgerState =
  let UTxO m = utxosUtxo $ lsUTxOState $ esLState $ nesEs $ shelleyLedgerState ledgerState
   in not $ Map.null m

nonEmptyAvvmAddresses :: LedgerState (ShelleyBlock Proto ShelleyEra) EmptyMK -> Bool
nonEmptyAvvmAddresses ledgerState =
  let UTxO m = stashedAVVMAddresses $ shelleyLedgerState ledgerState
   in not $ Map.null m

{-------------------------------------------------------------------------------
    Utilities
-------------------------------------------------------------------------------}

extractUtxoDiff ::
  LedgerState (ShelleyBlock proto era) DiffMK ->
  Diff BigEndianTxIn (Core.TxOut era)
extractUtxoDiff shelleyLedgerState =
  let DiffMK tables = getLedgerTables $ shelleyLedgerTables shelleyLedgerState
   in tables

{-------------------------------------------------------------------------------
    TestSetup
-------------------------------------------------------------------------------}

data TestSetup src dest = TestSetup
  { tsSrcLedgerConfig :: LedgerConfig src
  , tsDestLedgerConfig :: LedgerConfig dest
  , tsSrcLedgerState :: LedgerState src EmptyMK
  , tsEpochNo :: EpochNo
  }

deriving instance
  ( Show (LedgerConfig src)
  , Show (LedgerConfig dest)
  , Show (LedgerState src EmptyMK)
  ) =>
  Show (TestSetup src dest)

instance Arbitrary (TestSetup ByronBlock (ShelleyBlock Proto ShelleyEra)) where
  arbitrary =
    let ledgerConfig = fixedShelleyLedgerConfig emptyFromByronTranslationContext
     in TestSetup
          <$> genByronLedgerConfig
          <*> pure ledgerConfig
          <*> genByronLedgerState
          <*> (EpochNo <$> arbitrary)

instance
  Arbitrary
    ( TestSetup
        (ShelleyBlock Proto ShelleyEra)
        (ShelleyBlock Proto AllegraEra)
    )
  where
  arbitrary =
    TestSetup
      (fixedShelleyLedgerConfig emptyFromByronTranslationContext)
      (fixedShelleyLedgerConfig Genesis.NoGenesis)
      <$> arbitrary
      <*> (EpochNo <$> arbitrary)

instance
  Arbitrary
    ( TestSetup
        (ShelleyBlock Proto AllegraEra)
        (ShelleyBlock Proto MaryEra)
    )
  where
  arbitrary =
    TestSetup
      (fixedShelleyLedgerConfig Genesis.NoGenesis)
      (fixedShelleyLedgerConfig Genesis.NoGenesis)
      <$> arbitrary
      <*> (EpochNo <$> arbitrary)

instance
  Arbitrary
    ( TestSetup
        (ShelleyBlock Proto MaryEra)
        (ShelleyBlock Proto AlonzoEra)
    )
  where
  arbitrary =
    TestSetup (fixedShelleyLedgerConfig Genesis.NoGenesis)
      <$> (fixedShelleyLedgerConfig <$> arbitrary)
      <*> arbitrary
      <*> (EpochNo <$> arbitrary)

instance
  Arbitrary
    ( TestSetup
        (ShelleyBlock (TPraos Crypto) AlonzoEra)
        (ShelleyBlock (Praos Crypto) BabbageEra)
    )
  where
  arbitrary =
    TestSetup
      <$> (fixedShelleyLedgerConfig <$> arbitrary)
      <*> (pure $ fixedShelleyLedgerConfig Genesis.NoGenesis)
      <*> arbitrary
      <*> (EpochNo <$> arbitrary)

instance
  Arbitrary
    ( TestSetup
        (ShelleyBlock (Praos Crypto) BabbageEra)
        (ShelleyBlock (Praos Crypto) ConwayEra)
    )
  where
  arbitrary =
    TestSetup
      <$> (pure $ fixedShelleyLedgerConfig Genesis.NoGenesis)
      <*> (fixedShelleyLedgerConfig <$> arbitrary)
      <*> arbitrary
      <*> (EpochNo <$> arbitrary)

instance
  Arbitrary
    ( TestSetup
        (ShelleyBlock (Praos Crypto) ConwayEra)
        (ShelleyBlock (Praos2 Crypto) DijkstraEra)
    )
  where
  arbitrary =
    TestSetup
      <$> (fixedShelleyLedgerConfig <$> arbitrary)
      <*> (fixedShelleyLedgerConfig <$> arbitrary)
      <*> arbitrary
      <*> (EpochNo <$> arbitrary)

{-------------------------------------------------------------------------------
    Generators
-------------------------------------------------------------------------------}

-- | A fixed ledger config should be sufficient as the updating of the ledger
-- tables on era transitions does not depend on the configurations of any of
-- the ledgers involved.
fixedShelleyLedgerConfig ::
  Core.TranslationContext era ->
  ShelleyLedgerConfig era
fixedShelleyLedgerConfig translationContext =
  mkShelleyLedgerConfig
    testShelleyGenesis
    translationContext
    (fixedEpochInfo (sgEpochLength testShelleyGenesis) (slotLengthFromSec 2))
