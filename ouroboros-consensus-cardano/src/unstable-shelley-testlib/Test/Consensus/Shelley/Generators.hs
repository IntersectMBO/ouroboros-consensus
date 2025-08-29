{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.Shelley.Generators (SomeResult (..)) where

import Cardano.Ledger.Core (TranslationContext)
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Ledger.State (InstantStake)
import Cardano.Protocol.Crypto (Crypto)
import qualified Cardano.Protocol.TPraos.BHeader as SL
import Cardano.Slotting.EpochInfo
import Control.Monad (replicateM)
import Data.Coerce (coerce)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Query
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Protocol.Praos (Praos)
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import qualified Ouroboros.Consensus.Protocol.Praos.Header as Praos
import Ouroboros.Consensus.Protocol.TPraos (TPraos, TPraosState (..))
import Ouroboros.Consensus.Shelley.Eras
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Protocol.Praos ()
import Ouroboros.Consensus.Shelley.Protocol.TPraos ()
import Ouroboros.Network.Block (mkSerialised)
import Test.Cardano.Ledger.AllegraEraGen ()
import Test.Cardano.Ledger.Alonzo.AlonzoEraGen ()
import Test.Cardano.Ledger.MaryEraGen ()
import Test.Cardano.Ledger.Shelley.Constants
  ( defaultConstants
  , numCoreNodes
  )
import Test.Cardano.Ledger.Shelley.Generator.Presets (genIssuerKeys)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators
  ( genCoherentBlock
  )
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.Cardano.Protocol.TPraos.Arbitrary (genBlock)
import Test.Consensus.Protocol.Serialisation.Generators ()
import Test.Consensus.Shelley.MockCrypto (CanMock)
import Test.QuickCheck hiding (Result)
import Test.Util.Orphans.Arbitrary ()
import Test.Util.Serialisation.Roundtrip
  ( Coherent (..)
  , WithVersion (..)
  )
import Test.Util.Serialisation.SomeResult (SomeResult (..))

{-------------------------------------------------------------------------------
  Generators

  These are generators for roundtrip tests, so the generated values are not
  necessarily valid
-------------------------------------------------------------------------------}

-- | The upstream 'Arbitrary' instance for Shelley blocks does not generate
-- coherent blocks, so neither does this.
instance
  CanMock (TPraos crypto) era =>
  Arbitrary (ShelleyBlock (TPraos crypto) era)
  where
  arbitrary = do
    allPoolKeys <-
      replicateM (fromIntegral $ numCoreNodes defaultConstants) $
        genIssuerKeys defaultConstants
    mkShelleyBlock <$> genBlock allPoolKeys

instance
  (Praos.PraosCrypto crypto, CanMock (Praos crypto) era) =>
  Arbitrary (ShelleyBlock (Praos crypto) era)
  where
  arbitrary = mkShelleyBlock <$> blk
   where
    blk = SL.Block <$> arbitrary <*> arbitrary

-- | This uses a different upstream generator to ensure the header and block
-- body relate as expected.
instance
  CanMock (TPraos crypto) era =>
  Arbitrary (Coherent (ShelleyBlock (TPraos crypto) era))
  where
  arbitrary = do
    allPoolKeys <-
      replicateM (fromIntegral $ numCoreNodes defaultConstants) $
        genIssuerKeys defaultConstants
    Coherent . mkShelleyBlock <$> genCoherentBlock allPoolKeys

-- | Create a coherent Praos block
--
--   TODO Establish a coherent block without doing this translation from a
--   TPraos header.
instance
  CanMock (Praos crypto) era =>
  Arbitrary (Coherent (ShelleyBlock (Praos crypto) era))
  where
  arbitrary = do
    allPoolKeys <-
      replicateM (fromIntegral $ numCoreNodes defaultConstants) $
        genIssuerKeys defaultConstants
    blk <- genCoherentBlock allPoolKeys
    Coherent . mkBlk <$> pure blk
   where
    mkBlk sleBlock =
      mkShelleyBlock $
        let
          SL.Block hdr1 bdy = sleBlock
         in
          SL.Block (translateHeader hdr1) bdy

    translateHeader :: Crypto c => SL.BHeader c -> Praos.Header c
    translateHeader (SL.BHeader bhBody bhSig) =
      Praos.Header hBody hSig
     where
      hBody =
        Praos.HeaderBody
          { Praos.hbBlockNo = SL.bheaderBlockNo bhBody
          , Praos.hbSlotNo = SL.bheaderSlotNo bhBody
          , Praos.hbPrev = SL.bheaderPrev bhBody
          , Praos.hbVk = SL.bheaderVk bhBody
          , Praos.hbVrfVk = SL.bheaderVrfVk bhBody
          , Praos.hbVrfRes = coerce $ SL.bheaderEta bhBody
          , Praos.hbBodySize = SL.bsize bhBody
          , Praos.hbBodyHash = SL.bhash bhBody
          , Praos.hbOCert = SL.bheaderOCert bhBody
          , Praos.hbProtVer = SL.bprotver bhBody
          }
      hSig = coerce bhSig

instance
  CanMock (TPraos crypto) era =>
  Arbitrary (Header (ShelleyBlock (TPraos crypto) era))
  where
  arbitrary = getHeader <$> arbitrary

instance
  CanMock (Praos crypto) era =>
  Arbitrary (Header (ShelleyBlock (Praos crypto) era))
  where
  arbitrary = do
    hdr <- arbitrary
    pure $ ShelleyHeader hdr (ShelleyHash $ Praos.headerHash hdr)

instance Arbitrary ShelleyHash where
  arbitrary = ShelleyHash <$> arbitrary

instance CanMock proto era => Arbitrary (GenTx (ShelleyBlock proto era)) where
  arbitrary = mkShelleyTx <$> arbitrary

instance CanMock proto era => Arbitrary (GenTxId (ShelleyBlock proto era)) where
  arbitrary = ShelleyTxId <$> arbitrary

instance CanMock proto era => Arbitrary (SomeBlockQuery (BlockQuery (ShelleyBlock proto era))) where
  arbitrary =
    oneof
      [ pure $ SomeBlockQuery GetLedgerTip
      , pure $ SomeBlockQuery GetEpochNo
      , SomeBlockQuery . GetNonMyopicMemberRewards <$> arbitrary
      , pure $ SomeBlockQuery GetCurrentPParams
      , pure $ SomeBlockQuery DebugEpochState
      , (\(SomeBlockQuery q) -> SomeBlockQuery (GetCBOR q)) <$> arbitrary
      , SomeBlockQuery . GetFilteredDelegationsAndRewardAccounts <$> arbitrary
      , pure $ SomeBlockQuery GetGenesisConfig
      , pure $ SomeBlockQuery DebugNewEpochState
      , pure $ SomeBlockQuery GetStakeDistribution2
      ]

instance (Arbitrary (InstantStake era), CanMock proto era) => Arbitrary (SomeResult (ShelleyBlock proto era)) where
  arbitrary =
    oneof
      [ SomeResult GetLedgerTip <$> arbitrary
      , SomeResult GetEpochNo <$> arbitrary
      , SomeResult <$> (GetNonMyopicMemberRewards <$> arbitrary) <*> arbitrary
      , SomeResult GetCurrentPParams <$> arbitrary
      , SomeResult DebugEpochState <$> arbitrary
      , ( \(SomeResult q r) ->
            SomeResult (GetCBOR q) (mkSerialised (encodeShelleyResult maxBound q) r)
        )
          <$> arbitrary
      , SomeResult <$> (GetFilteredDelegationsAndRewardAccounts <$> arbitrary) <*> arbitrary
      , SomeResult GetGenesisConfig . compactGenesis <$> arbitrary
      , SomeResult DebugNewEpochState <$> arbitrary
      , SomeResult GetStakeDistribution2 <$> arbitrary
      ]

instance Arbitrary NonMyopicMemberRewards where
  arbitrary = NonMyopicMemberRewards <$> arbitrary

instance CanMock proto era => Arbitrary (Point (ShelleyBlock proto era)) where
  arbitrary = BlockPoint <$> arbitrary <*> arbitrary

instance Arbitrary TPraosState where
  arbitrary = do
    lastSlot <-
      frequency
        [ (1, return Origin)
        , (5, NotOrigin . SlotNo <$> choose (0, 100))
        ]
    TPraosState lastSlot <$> arbitrary

instance CanMock proto era => Arbitrary (ShelleyTip proto era) where
  arbitrary =
    ShelleyTip
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary ShelleyTransition where
  arbitrary = ShelleyTransitionInfo <$> arbitrary

instance
  (Arbitrary (InstantStake era), CanMock proto era) =>
  Arbitrary (LedgerState (ShelleyBlock proto era) EmptyMK)
  where
  arbitrary =
    ShelleyLedgerState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> pure (LedgerTables EmptyMK)

instance
  (Arbitrary (InstantStake era), CanMock proto era) =>
  Arbitrary (LedgerState (ShelleyBlock proto era) ValuesMK)
  where
  arbitrary =
    ShelleyLedgerState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (LedgerTables . ValuesMK <$> arbitrary)

deriving newtype instance Arbitrary BigEndianTxIn

instance CanMock proto era => Arbitrary (AnnTip (ShelleyBlock proto era)) where
  arbitrary =
    AnnTip
      <$> arbitrary
      <*> (BlockNo <$> arbitrary)
      <*> arbitrary

instance Arbitrary ShelleyNodeToNodeVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ShelleyNodeToClientVersion where
  arbitrary = arbitraryBoundedEnum

instance
  ShelleyBasedEra era =>
  Arbitrary (SomeSecond (NestedCtxt f) (ShelleyBlock proto era))
  where
  arbitrary = return (SomeSecond indexIsTrivial)

{-------------------------------------------------------------------------------
  Generators for shelley ledger config
-------------------------------------------------------------------------------}

-- | Generate a 'ShelleyLedgerConfig' with a fixed 'EpochInfo' (see
-- 'arbitraryGlobalsWithFixedEpochInfo').
instance
  Arbitrary (TranslationContext era) =>
  Arbitrary (ShelleyLedgerConfig era)
  where
  arbitrary =
    ShelleyLedgerConfig
      <$> arbitrary
      <*> arbitraryGlobalsWithFixedEpochInfo
      <*> arbitrary

instance Arbitrary CompactGenesis where
  arbitrary = compactGenesis <$> arbitrary

-- | Generate 'Globals' with a fixed 'EpochInfo'. A fixed 'EpochInfo' is
-- comprehensive in the case of generating a 'ShelleyLedgerConfig' (see the
-- documentation for 'shelleyLedgerGlobals').
arbitraryGlobalsWithFixedEpochInfo :: Gen SL.Globals
arbitraryGlobalsWithFixedEpochInfo =
  SL.Globals
    <$> arbitraryFixedEpochInfo
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

arbitraryFixedEpochInfo :: Monad m => Gen (EpochInfo m)
arbitraryFixedEpochInfo = fixedEpochInfo <$> arbitrary <*> arbitrary

{-------------------------------------------------------------------------------
  Versioned generators for serialisation
-------------------------------------------------------------------------------}

-- | Some 'Query's are only supported by 'ShelleyNodeToClientVersion2', so we
-- make sure to not generate those queries in combination with
-- 'ShelleyNodeToClientVersion1'.
instance
  CanMock proto era =>
  Arbitrary
    (WithVersion ShelleyNodeToClientVersion (SomeBlockQuery (BlockQuery (ShelleyBlock proto era))))
  where
  arbitrary = do
    query@(SomeBlockQuery q) <- arbitrary
    version <- arbitrary `suchThat` blockQueryIsSupportedOnVersion q
    return $ WithVersion version query
