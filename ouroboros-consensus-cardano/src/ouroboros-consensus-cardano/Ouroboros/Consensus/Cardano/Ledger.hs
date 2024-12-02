{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

#if __GLASGOW_HASKELL__ <= 906
{-# OPTIONS_GHC -Wno-incomplete-patterns
                -Wno-incomplete-uni-patterns
                -Wno-incomplete-record-updates
                -Wno-overlapping-patterns #-}
#endif

module Ouroboros.Consensus.Cardano.Ledger (
    CardanoTxOut (..)
  , eliminateCardanoTxOut
  ) where

import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Data.SOP.Index
import qualified Data.SOP.InPairs as InPairs
import           Data.Void
import           GHC.Generics
import           NoThunks.Class
import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Protocol.Praos (Praos)
import           Ouroboros.Consensus.Protocol.TPraos (TPraos)
import           Ouroboros.Consensus.Shelley.Ledger (IsShelleyBlock,
                     ShelleyBlock, ShelleyBlockLedgerEra)
import           Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtoCrypto)
import           Ouroboros.Consensus.TypeFamilyWrappers

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

  ejectCanonicalTxIn IZ _                 =
      error "ejectCanonicalTxIn: Byron has no TxIns"
  ejectCanonicalTxIn (IS idx) cardanoTxIn = case idx of
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
    ShelleyTxOut {-# UNPACK #-} !(TxOut (LedgerState (ShelleyBlock (TPraos c) (ShelleyEra c))))
  | AllegraTxOut {-# UNPACK #-} !(TxOut (LedgerState (ShelleyBlock (TPraos c) (AllegraEra c))))
  | MaryTxOut    {-# UNPACK #-} !(TxOut (LedgerState (ShelleyBlock (TPraos c) (MaryEra c))))
  | AlonzoTxOut  {-# UNPACK #-} !(TxOut (LedgerState (ShelleyBlock (TPraos c) (AlonzoEra c))))
  | BabbageTxOut {-# UNPACK #-} !(TxOut (LedgerState (ShelleyBlock (Praos c) (BabbageEra c))))
  | ConwayTxOut  {-# UNPACK #-} !(TxOut (LedgerState (ShelleyBlock (Praos c) (ConwayEra c))))
#else
    ShelleyTxOut {-# UNPACK #-} !(TxOut (LedgerState (ShelleyBlock (TPraos c) (ShelleyEra c))))
  | AllegraTxOut {-# UNPACK #-} !(TxOut (LedgerState (ShelleyBlock (TPraos c) (AllegraEra c))))
  | MaryTxOut    {-# UNPACK #-} !(TxOut (LedgerState (ShelleyBlock (TPraos c) (MaryEra c))))
  | AlonzoTxOut  !(TxOut (LedgerState (ShelleyBlock (TPraos c) (AlonzoEra c))))
  | BabbageTxOut !(TxOut (LedgerState (ShelleyBlock (Praos c) (BabbageEra c))))
  | ConwayTxOut  !(TxOut (LedgerState (ShelleyBlock (Praos c) (ConwayEra c))))
#endif
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

-- | Eliminate the wrapping of CardanoTxOut with the provided function. Similar
-- to 'hcimap' on an 'NS'.
eliminateCardanoTxOut ::
     forall r c. CardanoHardForkConstraints c
  => (forall x.
         -- TODO ProtoCrypto constraint should be in IsShelleyBlock
         ( IsShelleyBlock x
         , ProtoCrypto (BlockProtocol x) ~ EraCrypto (ShelleyBlockLedgerEra x)
         , EraCrypto (ShelleyBlockLedgerEra x) ~ c
         )
      => Index (CardanoEras c) x
      -> TxOut (LedgerState x)
      -> r
     )
  -> CardanoTxOut c -> r
eliminateCardanoTxOut f = \case
  ShelleyTxOut txout -> f (IS IZ) txout
  AllegraTxOut txout -> f (IS (IS IZ)) txout
  MaryTxOut txout    -> f (IS (IS (IS IZ))) txout
  AlonzoTxOut txout  -> f (IS (IS (IS (IS IZ)))) txout
  BabbageTxOut txout -> f (IS (IS (IS (IS (IS IZ))))) txout
  ConwayTxOut txout  -> f (IS (IS (IS (IS (IS (IS IZ)))))) txout

instance CardanoHardForkConstraints c => HasHardForkTxOut (CardanoEras c) where

  type instance HardForkTxOut (CardanoEras c) = CardanoTxOut c

  injectHardForkTxOut idx txOut = case idx of
    IZ                                    -> case txOut of {}
    IS IZ                                 -> ShelleyTxOut txOut
    IS (IS IZ)                            -> AllegraTxOut txOut
    IS (IS (IS IZ))                       -> MaryTxOut    txOut
    IS (IS (IS (IS IZ)))                  -> AlonzoTxOut  txOut
    IS (IS (IS (IS (IS IZ))))             -> BabbageTxOut txOut
    IS (IS (IS (IS (IS (IS IZ)))))        -> ConwayTxOut  txOut
    IS (IS (IS (IS (IS (IS (IS idx')))))) -> case idx' of {}

  ejectHardForkTxOut ::
       forall y.
       Index (CardanoEras c) y
    -> HardForkTxOut (CardanoEras c)
    -> TxOut (LedgerState y)
  ejectHardForkTxOut targetIdx txOut =
    let composeFromTo' :: Index (CardanoEras c) x -> WrapTxOut x -> Maybe (WrapTxOut y)
        composeFromTo' originIdx =
           InPairs.composeFromTo originIdx targetIdx
             (InPairs.hmap
                (\translator -> InPairs.Fn2 $ WrapTxOut . translateTxOutWith translator . unwrapTxOut )
                (translateLedgerTables (hardForkEraTranslation @(CardanoEras c))))
    in maybe (error "Anachrony") unwrapTxOut $
        eliminateCardanoTxOut @(Maybe (WrapTxOut y)) (\idx -> composeFromTo' idx . WrapTxOut) txOut

instance CardanoHardForkConstraints c => SerializeHardForkTxOut (CardanoEras c) where
  encodeHardForkTxOut _ txOut =
    let (idx, value) = case txOut of
          ShelleyTxOut txOut' -> (1, encodeValue (getLedgerTables $ codecLedgerTables @(LedgerState (ShelleyBlock (TPraos c) (ShelleyEra c)))) txOut')
          AllegraTxOut txOut' -> (2, encodeValue (getLedgerTables $ codecLedgerTables @(LedgerState (ShelleyBlock (TPraos c) (AllegraEra c)))) txOut')
          MaryTxOut    txOut' -> (3, encodeValue (getLedgerTables $ codecLedgerTables @(LedgerState (ShelleyBlock (TPraos c) (MaryEra c)))) txOut')
          AlonzoTxOut  txOut' -> (4, encodeValue (getLedgerTables $ codecLedgerTables @(LedgerState (ShelleyBlock (TPraos c) (AlonzoEra c)))) txOut')
          BabbageTxOut txOut' -> (5, encodeValue (getLedgerTables $ codecLedgerTables @(LedgerState (ShelleyBlock (Praos c) (BabbageEra c)))) txOut')
          ConwayTxOut  txOut' -> (6, encodeValue (getLedgerTables $ codecLedgerTables @(LedgerState (ShelleyBlock (Praos c) (ConwayEra c)))) txOut')
    in CBOR.encodeListLen 2
       <> CBOR.encodeWord8 idx
       <> value

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
