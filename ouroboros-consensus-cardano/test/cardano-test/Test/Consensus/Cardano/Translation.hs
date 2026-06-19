{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-x-ord-preserving-coercions #-}
#if __GLASGOW_HASKELL__ < 908
{-# OPTIONS_GHC -Wno-unrecognised-warning-flags #-}
#endif

module Test.Consensus.Cardano.Translation (tests) where

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
import Cardano.Ledger.Shelley.Translation
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Slot (EpochNo (..))
import qualified Data.Map.Strict as Map
import Data.SOP.InPairs (RequiringBoth (..), provideBoth)
import Data.Void (Void)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( slotLengthFromSec
  )
import Ouroboros.Consensus.Byron.Ledger (ByronBlock, byronLedgerState)
import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.Cardano.CanHardFork ()
import Ouroboros.Consensus.HardFork.Combinator
  ( InPairs (..)
  , hardForkEraTranslation
  , translateLedgerState
  , translateValues
  )
import Ouroboros.Consensus.HardFork.Combinator.State.Types
  ( TranslateLedgerState (..)
  , TranslateValues (..)
  )
import Ouroboros.Consensus.Ledger.Basics
  ( BlockSupportsUTxOHD (Diff)
  , LedgerCfg
  , LedgerConfig
  , LedgerState
  )
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Ouroboros.Consensus.Protocol.Praos
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Shelley.Eras
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger
  ( ShelleyBlock
  , ShelleyLedgerConfig
  , mkShelleyLedgerConfig
  , shelleyLedgerState
  , shelleyLedgerTranslationContext
  )
import Ouroboros.Consensus.Shelley.Ledger.LedgerCallShim (splitUTxO)
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.TypeFamilyWrappers
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
    , testValuesUpgrade
        "Shelley to Allegra (value upgrade)"
        shelleyToAllegraValuesTranslation
    , testUtxoUpgradeTranslation
        "Allegra to Mary"
        allegraToMaryLedgerStateTranslation
        allegraToMaryValuesTranslation
    , testUtxoUpgradeTranslation
        "Mary to Alonzo"
        maryToAlonzoLedgerStateTranslation
        maryToAlonzoValuesTranslation
    , testUtxoUpgradeTranslation
        "Alonzo to Babbage"
        alonzoToBabbageLedgerStateTranslation
        alonzoToBabbageValuesTranslation
    , testUtxoUpgradeTranslation
        "Babbage to Conway"
        babbageToConwayLedgerStateTranslation
        babbageToConwayValuesTranslation
    , testUtxoUpgradeTranslation
        "Conway to Dijkstra"
        conwayToDijkstraLedgerStateTranslation
        conwayToDijkstraValuesTranslation
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
    (ShelleyBlock (Praos Crypto) DijkstraEra)
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

-- | The per-boundary on-disk value translations, exactly as wired into the
-- production 'EraTranslation'. We assert these against the expected 'TxOut'
-- upgrade so that a mis-wired 'translateValues' (e.g. 'id', the wrong upgrade,
-- or a dropped boundary) is caught — this is the path the node actually uses to
-- promote on-disk values across an era transition.
shelleyToAllegraValuesTranslation ::
  TranslateValues
    (ShelleyBlock (TPraos Crypto) ShelleyEra)
    (ShelleyBlock (TPraos Crypto) AllegraEra)
allegraToMaryValuesTranslation ::
  TranslateValues
    (ShelleyBlock (TPraos Crypto) AllegraEra)
    (ShelleyBlock (TPraos Crypto) MaryEra)
maryToAlonzoValuesTranslation ::
  TranslateValues
    (ShelleyBlock (TPraos Crypto) MaryEra)
    (ShelleyBlock (TPraos Crypto) AlonzoEra)
alonzoToBabbageValuesTranslation ::
  TranslateValues
    (ShelleyBlock (TPraos Crypto) AlonzoEra)
    (ShelleyBlock (Praos Crypto) BabbageEra)
babbageToConwayValuesTranslation ::
  TranslateValues
    (ShelleyBlock (Praos Crypto) BabbageEra)
    (ShelleyBlock (Praos Crypto) ConwayEra)
conwayToDijkstraValuesTranslation ::
  TranslateValues
    (ShelleyBlock (Praos Crypto) ConwayEra)
    (ShelleyBlock (Praos Crypto) DijkstraEra)
PCons
  -- Byron->Shelley moves the Byron UTxO into the Shelley tables via the diff
  -- (tested by 'byronUtxosAreInsertsInShelleyUtxoDiff'); its value translation
  -- is @const mempty@, so there is nothing to assert here.
  _
  ( PCons
      shelleyToAllegraValuesTranslation
      ( PCons
          allegraToMaryValuesTranslation
          ( PCons
              maryToAlonzoValuesTranslation
              ( PCons
                  alonzoToBabbageValuesTranslation
                  ( PCons
                      babbageToConwayValuesTranslation
                      ( PCons
                          conwayToDijkstraValuesTranslation
                          PNil
                        )
                    )
                )
            )
        )
    ) = tvs
   where
    tvs :: InPairs TranslateValues (CardanoEras Crypto)
    tvs = translateValues hardForkEraTranslation

-- | Check that the tables are correctly translated from one era to the next.
testTablesTranslation ::
  forall srcBlk dstBlk.
  ( Arbitrary (TestSetup srcBlk dstBlk)
  , Show (LedgerCfg LedgerState srcBlk)
  , Show (LedgerCfg LedgerState dstBlk)
  , Show (LedgerState srcBlk)
  ) =>
  -- | Property label
  String ->
  RequiringBoth
    WrapLedgerConfig
    TranslateLedgerState
    srcBlk
    dstBlk ->
  (LedgerState srcBlk -> Diff dstBlk -> Bool) ->
  -- | Coverage testing function
  (LedgerState srcBlk -> Property -> Property) ->
  TestTree
testTablesTranslation propLabel translateWithConfig translationShouldSatisfy ledgerStateShouldCover =
  testProperty propLabel withTestSetup
 where
  withTestSetup :: TestSetup srcBlk dstBlk -> Property
  withTestSetup ts =
    checkCoverage $
      ledgerStateShouldCover tsSrcLedgerState $
        property $
          translationShouldSatisfy tsSrcLedgerState destDiff
   where
    TestSetup{tsSrcLedgerConfig, tsDestLedgerConfig, tsSrcLedgerState, tsEpochNo} = ts
    -- The translation now returns the new state paired with the @'Diff'@ that
    -- records the table changes induced by the era transition; we only test the
    -- diff here.
    destDiff = snd $ translateLedgerStateWith translation tsEpochNo tsSrcLedgerState
     where
      translation :: TranslateLedgerState srcBlk dstBlk
      translation =
        provideBoth
          translateWithConfig
          (WrapLedgerConfig tsSrcLedgerConfig)
          (WrapLedgerConfig tsDestLedgerConfig)

-- | Check a pure-upgrade era boundary by /injecting/ a populated UTxO and
-- /ejecting/ it again around the translation.
--
-- The mk-free canonical ledger state is UTxO-free (the values live in the
-- backend), so we cannot read a meaningful UTxO out of @'tsSrcLedgerState'@.
-- Instead we generate a source-era 'NewEpochState' that /does/ carry a UTxO,
-- run the ledger's cross-era translation on it (the same 'SL.translateEra'' the
-- consensus layer uses), and split the resulting UTxO back out. We assert that
-- every entry survives with its 'TxOut' upgraded (the 'TxIn' key is era-stable),
-- which is precisely what the on-disk values translation must reproduce.
--
-- We additionally assert that the consensus state translation itself emits no
-- table diff for these boundaries (the values are upgraded lazily when read,
-- not folded into a diff).
testUtxoUpgradeTranslation ::
  forall srcProto srcEra dstProto dstEra.
  ( Arbitrary (TestSetup (ShelleyBlock srcProto srcEra) (ShelleyBlock dstProto dstEra))
  , Arbitrary (NewEpochState srcEra)
  , Show (LedgerState (ShelleyBlock srcProto srcEra))
  , Show (LedgerConfig (ShelleyBlock srcProto srcEra))
  , Show (LedgerConfig (ShelleyBlock dstProto dstEra))
  , Show (NewEpochState srcEra)
  , Core.EraTxOut srcEra
  , Core.EraTxOut dstEra
  , Core.TranslateEra dstEra NewEpochState
  , Core.TranslationError dstEra NewEpochState ~ Void
  , Core.PreviousEra dstEra ~ srcEra
  , Eq (Core.TxOut dstEra)
  , Show (Core.TxOut dstEra)
  ) =>
  String ->
  RequiringBoth
    WrapLedgerConfig
    TranslateLedgerState
    (ShelleyBlock srcProto srcEra)
    (ShelleyBlock dstProto dstEra) ->
  -- | The on-disk value translation as wired in the production 'EraTranslation'.
  TranslateValues (ShelleyBlock srcProto srcEra) (ShelleyBlock dstProto dstEra) ->
  TestTree
testUtxoUpgradeTranslation propLabel translateWithConfig valuesTranslation =
  testProperty propLabel $
    \(ts :: TestSetup (ShelleyBlock srcProto srcEra) (ShelleyBlock dstProto dstEra))
     (srcNes :: NewEpochState srcEra) ->
    let TestSetup{tsSrcLedgerConfig, tsDestLedgerConfig, tsSrcLedgerState, tsEpochNo} = ts
        translation :: TranslateLedgerState (ShelleyBlock srcProto srcEra) (ShelleyBlock dstProto dstEra)
        translation =
          provideBoth
            translateWithConfig
            (WrapLedgerConfig tsSrcLedgerConfig)
            (WrapLedgerConfig tsDestLedgerConfig)
        -- The consensus state translation must emit no UTxO table diff.
        destDiff :: Diff (ShelleyBlock dstProto dstEra)
        destDiff = snd $ translateLedgerStateWith translation tsEpochNo tsSrcLedgerState
        -- Inject a populated UTxO, translate, eject.
        (_, srcValues) = splitUTxO srcNes
        dstNes :: NewEpochState dstEra
        dstNes = Core.translateEra' (shelleyLedgerTranslationContext tsDestLedgerConfig) srcNes
        (_, dstValues) = splitUTxO dstNes
        expected = Map.map Core.upgradeTxOut srcValues
     in checkCoverage $
          cover 50 (not (Map.null srcValues)) "UTxO set is not empty" $
            conjoin
              [ -- The consensus state translation emits no table diff here.
                property (Diff.null destDiff)
              , -- The ledger's era translation upgrades the UTxO entries.
                dstValues === expected
              , -- The production on-disk value translation does the same: this
                -- is the path the node uses to promote values across the era.
                translateValuesWith valuesTranslation srcValues === expected
              ]

-- | Assert only that a boundary's on-disk value translation upgrades each
-- 'TxOut' (the 'TxIn' key is era-stable). Unlike 'testUtxoUpgradeTranslation'
-- this makes no claim about the state-level diff, so it also fits boundaries
-- that carry diff-level changes (e.g. Shelley->Allegra deletes the AVVM
-- addresses) yet still upgrade the surviving values.
testValuesUpgrade ::
  forall srcProto srcEra dstProto dstEra.
  ( Arbitrary (NewEpochState srcEra)
  , Show (NewEpochState srcEra)
  , Core.EraTxOut srcEra
  , Core.EraTxOut dstEra
  , Core.PreviousEra dstEra ~ srcEra
  , Eq (Core.TxOut dstEra)
  , Show (Core.TxOut dstEra)
  ) =>
  String ->
  TranslateValues (ShelleyBlock srcProto srcEra) (ShelleyBlock dstProto dstEra) ->
  TestTree
testValuesUpgrade propLabel valuesTranslation =
  testProperty propLabel $ \(srcNes :: NewEpochState srcEra) ->
    let (_, srcValues) = splitUTxO srcNes
     in checkCoverage $
          cover 50 (not (Map.null srcValues)) "UTxO set is not empty" $
            translateValuesWith valuesTranslation srcValues
              === Map.map Core.upgradeTxOut srcValues

{-------------------------------------------------------------------------------
    Specific predicates
-------------------------------------------------------------------------------}

byronUtxosAreInsertsInShelleyUtxoDiff ::
  LedgerState ByronBlock ->
  Diff (ShelleyBlock Proto ShelleyEra) ->
  Bool
byronUtxosAreInsertsInShelleyUtxoDiff srcLedgerState destDiff =
  toNextUtxoDiff srcLedgerState == destDiff
 where
  toNextUtxoDiff ::
    LedgerState ByronBlock ->
    Diff.Diff TxIn (Core.TxOut ShelleyEra)
  toNextUtxoDiff ledgerState =
    let
      Byron.UTxO utxo = Byron.cvsUtxo $ byronLedgerState ledgerState
      keyFn = translateTxInByronToShelley . Byron.fromCompactTxIn
      valFn = Diff.Insert . translateCompactTxOutByronToShelley
     in
      Diff.Diff $ Map.map valFn $ Map.mapKeys keyFn utxo

  translateTxInByronToShelley :: Byron.TxIn -> TxIn
  translateTxInByronToShelley byronTxIn =
    let
      Byron.TxInUtxo txId txIx = byronTxIn
      shelleyTxId' = translateTxIdByronToShelley txId
     in
      TxIn shelleyTxId' (TxIx txIx)

shelleyAvvmAddressesAreDeletesInUtxoDiff ::
  LedgerState (ShelleyBlock Proto ShelleyEra) ->
  Diff (ShelleyBlock Proto AllegraEra) ->
  Bool
shelleyAvvmAddressesAreDeletesInUtxoDiff srcLedgerState destDiff =
  toNextUtxoDiff srcLedgerState == destDiff
 where
  toNextUtxoDiff ::
    LedgerState (ShelleyBlock Proto ShelleyEra) ->
    Diff.Diff TxIn (Core.TxOut AllegraEra)
  toNextUtxoDiff = avvmAddressesToUtxoDiff . stashedAVVMAddresses . shelleyLedgerState
  -- The AVVM addresses are deleted on the Shelley->Allegra transition; their
  -- 'TxOut's are upgraded to Allegra so the diff types line up (the 'Delete'
  -- itself carries no value).
  avvmAddressesToUtxoDiff (UTxO m) = Diff.fromMapDeletes $ Map.map Core.upgradeTxOut m

nonEmptyUtxosByron :: LedgerState ByronBlock -> Bool
nonEmptyUtxosByron ledgerState =
  let Byron.UTxO utxo = Byron.cvsUtxo $ byronLedgerState ledgerState
   in not $ Map.null utxo

nonEmptyAvvmAddresses :: LedgerState (ShelleyBlock Proto ShelleyEra) -> Bool
nonEmptyAvvmAddresses ledgerState =
  let UTxO m = stashedAVVMAddresses $ shelleyLedgerState ledgerState
   in not $ Map.null m

{-------------------------------------------------------------------------------
    TestSetup
-------------------------------------------------------------------------------}

data TestSetup src dest = TestSetup
  { tsSrcLedgerConfig :: LedgerConfig src
  , tsDestLedgerConfig :: LedgerConfig dest
  , tsSrcLedgerState :: LedgerState src
  , tsEpochNo :: EpochNo
  }

deriving instance
  ( Show (LedgerConfig src)
  , Show (LedgerConfig dest)
  , Show (LedgerState src)
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
        (ShelleyBlock (Praos Crypto) DijkstraEra)
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
