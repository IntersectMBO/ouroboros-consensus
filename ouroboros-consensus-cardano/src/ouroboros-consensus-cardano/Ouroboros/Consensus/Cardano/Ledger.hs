{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Cardano.Ledger (
    CardanoTxOut (..)
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Shelley.API as SL
import           Data.SOP.Index
import qualified Data.SOP.InPairs as InPairs
import           Data.Void
import           GHC.Generics
import           NoThunks.Class
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Protocol.Praos (Praos)
import           Ouroboros.Consensus.Protocol.TPraos (TPraos)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)

instance CardanoHardForkConstraints c
      => HasCanonicalTxIn (CardanoEras c) where
  newtype instance CanonicalTxIn (CardanoEras c) = CardanoTxIn {
      getCardanoTxIn :: SL.TxIn c
    }
    deriving stock (Show, Eq, Ord)
    deriving newtype NoThunks

  injectCanonicalTxIn IZ       byronTxIn   = absurd byronTxIn
  injectCanonicalTxIn (IS idx) shelleyTxIn = case idx of
      IZ                               -> CardanoTxIn shelleyTxIn
      IS IZ                            -> CardanoTxIn shelleyTxIn
      IS (IS IZ)                       -> CardanoTxIn shelleyTxIn
      IS (IS (IS IZ))                  -> CardanoTxIn shelleyTxIn
      IS (IS (IS (IS IZ)))             -> CardanoTxIn shelleyTxIn
      IS (IS (IS (IS (IS IZ))))        -> CardanoTxIn shelleyTxIn
      IS (IS (IS (IS (IS (IS idx'))))) -> case idx' of {}

  distribCanonicalTxIn IZ _                 =
      error "distribCanonicalTxIn: Byron has no TxIns"
  distribCanonicalTxIn (IS idx) cardanoTxIn = case idx of
      IZ                               -> getCardanoTxIn cardanoTxIn
      IS IZ                            -> getCardanoTxIn cardanoTxIn
      IS (IS IZ)                       -> getCardanoTxIn cardanoTxIn
      IS (IS (IS IZ))                  -> getCardanoTxIn cardanoTxIn
      IS (IS (IS (IS IZ)))             -> getCardanoTxIn cardanoTxIn
      IS (IS (IS (IS (IS IZ))))        -> getCardanoTxIn cardanoTxIn
      IS (IS (IS (IS (IS (IS idx'))))) -> case idx' of {}

  encodeCanonicalTxIn   = Core.toEraCBOR @(ShelleyEra c) . getCardanoTxIn

  decodeCanonicalTxIn = CardanoTxIn <$> Core.fromEraCBOR @(ShelleyEra c)

-- Unpacking the fields of the era-specific TxOuts could save a chunk of memory.
-- However, unpacking of sum types is only possible on @ghc-9.6.1@ and later, so
-- before @ghc-9.6.1@ we only unpack the TxOuts for eras before Alonzo.
--
-- For more information on the @UNPACK@ pragma, see
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/pragmas.html#unpack-pragma
data CardanoTxOut c =
#if MIN_VERSION_GLASGOW_HASKELL(9,6,1,0)
    ShelleyTxOut {-# UNPACK #-} !(Value (LedgerState (ShelleyBlock (TPraos c) (ShelleyEra c))))
  | AllegraTxOut {-# UNPACK #-} !(Value (LedgerState (ShelleyBlock (TPraos c) (AllegraEra c))))
  | MaryTxOut    {-# UNPACK #-} !(Value (LedgerState (ShelleyBlock (TPraos c) (MaryEra c))))
  | AlonzoTxOut  {-# UNPACK #-} !(Value (LedgerState (ShelleyBlock (TPraos c) (AlonzoEra c))))
  | BabbageTxOut {-# UNPACK #-} !(Value (LedgerState (ShelleyBlock (Praos c) (BabbageEra c))))
  | ConwayTxOut  {-# UNPACK #-} !(Value (LedgerState (ShelleyBlock (Praos c) (ConwayEra c))))
#else
    ShelleyTxOut {-# UNPACK #-} !(Value (LedgerState (ShelleyBlock (TPraos c) (ShelleyEra c))))
  | AllegraTxOut {-# UNPACK #-} !(Value (LedgerState (ShelleyBlock (TPraos c) (AllegraEra c))))
  | MaryTxOut    {-# UNPACK #-} !(Value (LedgerState (ShelleyBlock (TPraos c) (MaryEra c))))
  | AlonzoTxOut  !(Value (LedgerState (ShelleyBlock (TPraos c) (AlonzoEra c))))
  | BabbageTxOut !(Value (LedgerState (ShelleyBlock (Praos c) (BabbageEra c))))
  | ConwayTxOut  !(Value (LedgerState (ShelleyBlock (Praos c) (ConwayEra c))))
#endif
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

instance CanHardFork (CardanoEras c) => HasHardForkTxOut (CardanoEras c) where
  type instance HardForkTxOut (CardanoEras c) = CardanoTxOut c
  injectHardForkTxOut IZ _txOut = error "Impossible: injecting TxOut from Byron"
  injectHardForkTxOut (IS IZ) txOut = ShelleyTxOut txOut
  injectHardForkTxOut (IS (IS IZ)) txOut = AllegraTxOut txOut
  injectHardForkTxOut (IS (IS (IS IZ))) txOut = MaryTxOut txOut
  injectHardForkTxOut (IS (IS (IS (IS IZ)))) txOut = AlonzoTxOut txOut
  injectHardForkTxOut (IS (IS (IS (IS (IS IZ))))) txOut = BabbageTxOut txOut
  injectHardForkTxOut (IS (IS (IS (IS (IS (IS IZ)))))) txOut = ConwayTxOut txOut
  injectHardForkTxOut (IS (IS (IS (IS (IS (IS (IS idx))))))) _txOut = case idx of {}

  distribHardForkTxOut IZ = error "Impossible: distributing TxOut to Byron"
  distribHardForkTxOut (IS IZ) = \case
    ShelleyTxOut txout -> txout
    _ -> error "Anachrony"
  distribHardForkTxOut (IS (IS IZ)) = \case
    ShelleyTxOut txout ->
      case translateLedgerTables (hardForkEraTranslation @(CardanoEras c)) of
        InPairs.PCons _ (InPairs.PCons p _) -> translateTxOutWith p txout
    AllegraTxOut txout -> txout
    _ -> error "Anachrony"
  distribHardForkTxOut (IS (IS (IS IZ))) = \case
    ShelleyTxOut txout ->
      case translateLedgerTables (hardForkEraTranslation @(CardanoEras c)) of
        InPairs.PCons _ (InPairs.PCons p1 (InPairs.PCons p2 _)) -> translateTxOutWith p2 $ translateTxOutWith p1 txout
    AllegraTxOut txout ->
      case translateLedgerTables (hardForkEraTranslation @(CardanoEras c)) of
        InPairs.PCons _ (InPairs.PCons _ (InPairs.PCons p2 _)) -> translateTxOutWith p2 txout
    MaryTxOut txout -> txout
    _ -> error "Anachrony"
  distribHardForkTxOut (IS (IS (IS (IS IZ)))) = \case
    ShelleyTxOut txout ->
      case translateLedgerTables (hardForkEraTranslation @(CardanoEras c)) of
        InPairs.PCons _ (InPairs.PCons p1 (InPairs.PCons p2 (InPairs.PCons p3 _))) -> translateTxOutWith p3 $ translateTxOutWith p2 $ translateTxOutWith p1 txout
    AllegraTxOut txout ->
      case translateLedgerTables (hardForkEraTranslation @(CardanoEras c)) of
        InPairs.PCons _ (InPairs.PCons _ (InPairs.PCons p2 (InPairs.PCons p3 _))) -> translateTxOutWith p3 $ translateTxOutWith p2 txout
    MaryTxOut txout ->
      case translateLedgerTables (hardForkEraTranslation @(CardanoEras c)) of
        InPairs.PCons _ (InPairs.PCons _ (InPairs.PCons _ (InPairs.PCons p3 _))) -> translateTxOutWith p3 txout
    AlonzoTxOut txout -> txout
    _ -> error "Anachrony"
  distribHardForkTxOut (IS (IS (IS (IS (IS IZ))))) = \case
    ShelleyTxOut txout ->
      case translateLedgerTables (hardForkEraTranslation @(CardanoEras c)) of
        InPairs.PCons _ (InPairs.PCons p1 (InPairs.PCons p2 (InPairs.PCons p3 (InPairs.PCons p4 _)))) -> translateTxOutWith p4 $ translateTxOutWith p3 $ translateTxOutWith p2 $ translateTxOutWith p1 txout
    AllegraTxOut txout ->
      case translateLedgerTables (hardForkEraTranslation @(CardanoEras c)) of
        InPairs.PCons _ (InPairs.PCons _ (InPairs.PCons p2 (InPairs.PCons p3 (InPairs.PCons p4 _)))) -> translateTxOutWith p4 $ translateTxOutWith p3 $ translateTxOutWith p2 txout
    MaryTxOut txout ->
      case translateLedgerTables (hardForkEraTranslation @(CardanoEras c)) of
        InPairs.PCons _ (InPairs.PCons _ (InPairs.PCons _ (InPairs.PCons p3 (InPairs.PCons p4 _)))) -> translateTxOutWith p4 $ translateTxOutWith p3 txout
    AlonzoTxOut txout ->
      case translateLedgerTables (hardForkEraTranslation @(CardanoEras c)) of
        InPairs.PCons _ (InPairs.PCons _ (InPairs.PCons _ (InPairs.PCons _ (InPairs.PCons p4 _)))) -> translateTxOutWith p4 txout
    BabbageTxOut txout -> txout
    _ -> error "Anachrony"
  distribHardForkTxOut (IS (IS (IS (IS (IS (IS IZ)))))) = \case
    ShelleyTxOut txout ->
      case translateLedgerTables (hardForkEraTranslation @(CardanoEras c)) of
        InPairs.PCons _ (InPairs.PCons p1 (InPairs.PCons p2 (InPairs.PCons p3 (InPairs.PCons p4 (InPairs.PCons p5 _))))) -> translateTxOutWith p5 $ translateTxOutWith p4 $ translateTxOutWith p3 $ translateTxOutWith p2 $ translateTxOutWith p1 txout
    AllegraTxOut txout ->
      case translateLedgerTables (hardForkEraTranslation @(CardanoEras c)) of
        InPairs.PCons _ (InPairs.PCons _ (InPairs.PCons p2 (InPairs.PCons p3 (InPairs.PCons p4 (InPairs.PCons p5 _))))) -> translateTxOutWith p5 $ translateTxOutWith p4 $ translateTxOutWith p3 $ translateTxOutWith p2 txout
    MaryTxOut txout ->
      case translateLedgerTables (hardForkEraTranslation @(CardanoEras c)) of
        InPairs.PCons _ (InPairs.PCons _ (InPairs.PCons _ (InPairs.PCons p3 (InPairs.PCons p4 (InPairs.PCons p5 _))))) -> translateTxOutWith p5 $ translateTxOutWith p4 $ translateTxOutWith p3 txout
    AlonzoTxOut txout ->
      case translateLedgerTables (hardForkEraTranslation @(CardanoEras c)) of
        InPairs.PCons _ (InPairs.PCons _ (InPairs.PCons _ (InPairs.PCons _ (InPairs.PCons p4 (InPairs.PCons p5 _))))) -> translateTxOutWith p5 $ translateTxOutWith p4 txout
    BabbageTxOut txout ->
      case translateLedgerTables (hardForkEraTranslation @(CardanoEras c)) of
        InPairs.PCons _ (InPairs.PCons _ (InPairs.PCons _ (InPairs.PCons _ (InPairs.PCons _ (InPairs.PCons p5 _))))) -> translateTxOutWith p5 txout
    ConwayTxOut txout -> txout
  distribHardForkTxOut (IS (IS (IS (IS (IS (IS (IS idx))))))) = case idx of {}

instance CardanoHardForkConstraints c => SerializeHardForkTxOut (CardanoEras c) where
  encodeHardForkTxOut _ (ShelleyTxOut txout) =
    CBOR.encodeListLen 2
      <> CBOR.encodeWord8 1
      <> encodeValue (getLedgerTables $ codecLedgerTables @(LedgerState (ShelleyBlock (TPraos c) (ShelleyEra c)))) txout
  encodeHardForkTxOut _ (AllegraTxOut txout) =
    CBOR.encodeListLen 2
      <> CBOR.encodeWord8 2
      <> encodeValue (getLedgerTables $ codecLedgerTables @(LedgerState (ShelleyBlock (TPraos c) (AllegraEra c)))) txout
  encodeHardForkTxOut _ (MaryTxOut txout) =
    CBOR.encodeListLen 2
      <> CBOR.encodeWord8 3
      <> encodeValue (getLedgerTables $ codecLedgerTables @(LedgerState (ShelleyBlock (TPraos c) (MaryEra c)))) txout
  encodeHardForkTxOut _ (AlonzoTxOut txout) =
    CBOR.encodeListLen 2
      <> CBOR.encodeWord8 4
      <> encodeValue (getLedgerTables $ codecLedgerTables @(LedgerState (ShelleyBlock (TPraos c) (AlonzoEra c)))) txout
  encodeHardForkTxOut _ (BabbageTxOut txout) =
    CBOR.encodeListLen 2
      <> CBOR.encodeWord8 5
      <> encodeValue (getLedgerTables $ codecLedgerTables @(LedgerState (ShelleyBlock (Praos c)  (BabbageEra c)))) txout
  encodeHardForkTxOut _ (ConwayTxOut txout) =
    CBOR.encodeListLen 2
      <> CBOR.encodeWord8 6
      <> encodeValue (getLedgerTables $ codecLedgerTables @(LedgerState (ShelleyBlock (Praos c)  (ConwayEra c)))) txout

  decodeHardForkTxOut _ = do
    CBOR.decodeListLenOf 2
    tag <- CBOR.decodeWord8
    case tag of
      1 -> ShelleyTxOut <$> decodeValue (getLedgerTables $ codecLedgerTables @(LedgerState (ShelleyBlock (TPraos c) (ShelleyEra c))))
      2 -> AllegraTxOut <$> decodeValue (getLedgerTables $ codecLedgerTables @(LedgerState (ShelleyBlock (TPraos c) (AllegraEra c))))
      3 -> MaryTxOut    <$> decodeValue (getLedgerTables $ codecLedgerTables @(LedgerState (ShelleyBlock (TPraos c) (MaryEra c))))
      4 -> AlonzoTxOut  <$> decodeValue (getLedgerTables $ codecLedgerTables @(LedgerState (ShelleyBlock (TPraos c) (AlonzoEra c))))
      5 -> BabbageTxOut <$> decodeValue (getLedgerTables $ codecLedgerTables @(LedgerState (ShelleyBlock (Praos c)  (BabbageEra c))))
      6 -> ConwayTxOut  <$> decodeValue (getLedgerTables $ codecLedgerTables @(LedgerState (ShelleyBlock (Praos c)  (ConwayEra c))))
      _ -> fail $ "Unkown TxOut tag: " <> show tag
