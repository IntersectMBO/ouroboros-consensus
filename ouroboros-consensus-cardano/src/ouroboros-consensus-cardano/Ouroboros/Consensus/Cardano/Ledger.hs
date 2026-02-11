{-# LANGUAGE BangPatterns #-}
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
{-# LANGUAGE UndecidableInstances #-}
-- TODO: can we un-orphan this module?
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Cardano.Ledger
  ( CardanoTxOut (..)
  , eliminateCardanoTxOut
  ) where

import Cardano.Ledger.Binary.Decoding hiding (Decoder)
import Cardano.Ledger.Binary.Encoding hiding (Encoding)
import Cardano.Ledger.Core (Era, eraDecoder, eraProtVerLow)
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Ledger.Shelley.LedgerState as SL
  ( dsUnifiedL
  , esLStateL
  , lsCertStateL
  , nesEsL
  )
import qualified Cardano.Ledger.UMap as SL
import Codec.CBOR.Decoding
import Codec.CBOR.Encoding
import qualified Data.Map as Map
import Data.MemPack
import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Functors
import Data.SOP.Index
import Data.SOP.Strict
import qualified Data.SOP.Tails as Tails
import qualified Data.SOP.Telescope as Telescope
import Data.Void
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.CanHardFork
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.HardFork.Combinator.State.Types
import Ouroboros.Consensus.Ledger.Tables
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Shelley.Ledger
  ( IsShelleyBlock
  , ShelleyBlock
  , ShelleyCompatible
  , shelleyLedgerState
  )
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util.IndexedMemPack

instance
  CardanoHardForkConstraints c =>
  HasCanonicalTxIn (CardanoEras c)
  where
  newtype CanonicalTxIn (CardanoEras c) = CardanoTxIn
    { getCardanoTxIn :: SL.TxIn
    }
    deriving stock (Show, Eq, Ord)
    deriving newtype NoThunks

  injectCanonicalTxIn IZ byronTxIn = absurd byronTxIn
  injectCanonicalTxIn (IS idx) shelleyTxIn = case idx of
    IZ -> CardanoTxIn shelleyTxIn
    IS IZ -> CardanoTxIn shelleyTxIn
    IS (IS IZ) -> CardanoTxIn shelleyTxIn
    IS (IS (IS IZ)) -> CardanoTxIn shelleyTxIn
    IS (IS (IS (IS IZ))) -> CardanoTxIn shelleyTxIn
    IS (IS (IS (IS (IS IZ)))) -> CardanoTxIn shelleyTxIn
    IS (IS (IS (IS (IS (IS idx'))))) -> case idx' of {}

  ejectCanonicalTxIn IZ _ =
    error "ejectCanonicalTxIn: Byron has no TxIns"
  ejectCanonicalTxIn (IS idx) cardanoTxIn = case idx of
    IZ -> getCardanoTxIn cardanoTxIn
    IS IZ -> getCardanoTxIn cardanoTxIn
    IS (IS IZ) -> getCardanoTxIn cardanoTxIn
    IS (IS (IS IZ)) -> getCardanoTxIn cardanoTxIn
    IS (IS (IS (IS IZ))) -> getCardanoTxIn cardanoTxIn
    IS (IS (IS (IS (IS IZ)))) -> getCardanoTxIn cardanoTxIn
    IS (IS (IS (IS (IS (IS idx'))))) -> case idx' of {}

instance CardanoHardForkConstraints c => MemPack (CanonicalTxIn (CardanoEras c)) where
  packM = packM . getCardanoTxIn
  packedByteCount = packedByteCount . getCardanoTxIn
  unpackM = CardanoTxIn <$> unpackM

data CardanoTxOut c
  = ShelleyTxOut {-# UNPACK #-} !(TxOut (LedgerState (ShelleyBlock (TPraos c) ShelleyEra)))
  | AllegraTxOut {-# UNPACK #-} !(TxOut (LedgerState (ShelleyBlock (TPraos c) AllegraEra)))
  | MaryTxOut {-# UNPACK #-} !(TxOut (LedgerState (ShelleyBlock (TPraos c) MaryEra)))
  | AlonzoTxOut !(TxOut (LedgerState (ShelleyBlock (TPraos c) AlonzoEra)))
  | BabbageTxOut !(TxOut (LedgerState (ShelleyBlock (Praos c) BabbageEra)))
  | ConwayTxOut !(TxOut (LedgerState (ShelleyBlock (Praos c) ConwayEra)))
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

-- | Eliminate the wrapping of CardanoTxOut with the provided function. Similar
-- to 'hcimap' on an 'NS'.
eliminateCardanoTxOut ::
  forall r c.
  CardanoHardForkConstraints c =>
  ( forall x.
    -- TODO ProtoCrypto constraint should be in IsShelleyBlock
    IsShelleyBlock x =>
    Index (CardanoEras c) x ->
    TxOut (LedgerState x) ->
    r
  ) ->
  CardanoTxOut c ->
  r
eliminateCardanoTxOut f = \case
  ShelleyTxOut txout -> f (IS IZ) txout
  AllegraTxOut txout -> f (IS (IS IZ)) txout
  MaryTxOut txout -> f (IS (IS (IS IZ))) txout
  AlonzoTxOut txout -> f (IS (IS (IS (IS IZ)))) txout
  BabbageTxOut txout -> f (IS (IS (IS (IS (IS IZ))))) txout
  ConwayTxOut txout -> f (IS (IS (IS (IS (IS (IS IZ)))))) txout

instance CardanoHardForkConstraints c => HasHardForkTxOut (CardanoEras c) where
  type HardForkTxOut (CardanoEras c) = CardanoTxOut c

  injectHardForkTxOut idx !txOut = case idx of
    IS IZ -> ShelleyTxOut txOut
    IS (IS IZ) -> AllegraTxOut txOut
    IS (IS (IS IZ)) -> MaryTxOut txOut
    IS (IS (IS (IS IZ))) -> AlonzoTxOut txOut
    IS (IS (IS (IS (IS IZ)))) -> BabbageTxOut txOut
    IS (IS (IS (IS (IS (IS IZ))))) -> ConwayTxOut txOut
    IS (IS (IS (IS (IS (IS (IS idx')))))) -> case idx' of {}

  ejectHardForkTxOut ::
    forall y.
    Index (CardanoEras c) y ->
    HardForkTxOut (CardanoEras c) ->
    TxOut (LedgerState y)
  ejectHardForkTxOut targetIdx =
    eliminateCardanoTxOut
      ( \origIdx ->
          unwrapTxOut
            . maybe (error "anachrony") id
            . Tails.extendWithTails origIdx targetIdx txOutTranslations
            . WrapTxOut
      )

instance
  CardanoHardForkConstraints c =>
  IndexedMemPack (LedgerState (HardForkBlock (CardanoEras c)) EmptyMK) (CardanoTxOut c)
  where
  indexedTypeName _ = "CardanoTxOut"
  indexedPackM _ = eliminateCardanoTxOut (const packM)
  indexedPackedByteCount _ = eliminateCardanoTxOut (const packedByteCount)
  indexedUnpackM (HardForkLedgerState (HardForkState idx)) = do
    let
      -- These could be made into a CAF to avoid recomputing it, but
      -- it is only used in serialization so it is not critical.
      np =
        ( (Fn $ const $ error "unpacking a byron txout")
            :* (Fn $ const $ Comp $ K . ShelleyTxOut <$> unpackM)
            :* (Fn $ const $ Comp $ K . AllegraTxOut <$> unpackM)
            :* (Fn $ const $ Comp $ K . MaryTxOut <$> unpackM)
            :* (Fn $ const $ Comp $ K . AlonzoTxOut <$> unpackM)
            :* (Fn $ const $ Comp $ K . BabbageTxOut <$> unpackM)
            :* (Fn $ const $ Comp $ K . ConwayTxOut <$> unpackM)
            :* Nil
        )
    hcollapse <$> (hsequence' $ hap np $ Telescope.tip idx)

instance
  CardanoHardForkConstraints c =>
  SerializeTablesWithHint (LedgerState (HardForkBlock (CardanoEras c)))
  where
  encodeTablesWithHint (HardForkLedgerState (HardForkState idx)) (LedgerTables (ValuesMK tbs)) =
    let
      -- These could be made into a CAF to avoid recomputing it, but
      -- it is only used in serialization so it is not critical.
      np =
        (Fn $ const $ K $ Codec.CBOR.Encoding.encodeMapLen 0)
          :* (Fn $ const $ K $ encOne (Proxy @ShelleyEra))
          :* (Fn $ const $ K $ encOne (Proxy @AllegraEra))
          :* (Fn $ const $ K $ encOne (Proxy @MaryEra))
          :* (Fn $ const $ K $ encOne (Proxy @AlonzoEra))
          :* (Fn $ const $ K $ encOne (Proxy @BabbageEra))
          :* (Fn $ const $ K $ encOne (Proxy @ConwayEra))
          :* Nil
     in
      hcollapse $ hap np $ Telescope.tip idx
   where
    encOne :: forall era. Era era => Proxy era -> Encoding
    encOne _ =
      toPlainEncoding (eraProtVerLow @era) $
        encodeMap encodeMemPack (eliminateCardanoTxOut (const encodeMemPack)) tbs

  decodeTablesWithHint ::
    forall s.
    LedgerState (HardForkBlock (CardanoEras c)) EmptyMK ->
    Decoder s (LedgerTables (LedgerState (HardForkBlock (CardanoEras c))) ValuesMK)
  decodeTablesWithHint (HardForkLedgerState (HardForkState idx)) =
    let
      -- These could be made into a CAF to avoid recomputing it, but
      -- it is only used in serialization so it is not critical.
      np =
        ( Fn $
            const $
              Comp $
                K . LedgerTables @(LedgerState (HardForkBlock (CardanoEras c))) . ValuesMK <$> pure Map.empty
        )
          :* (Fn $ Comp . fmap K . getOne ShelleyTxOut . unFlip . currentState)
          :* (Fn $ Comp . fmap K . getOne AllegraTxOut . unFlip . currentState)
          :* (Fn $ Comp . fmap K . getOne MaryTxOut . unFlip . currentState)
          :* (Fn $ Comp . fmap K . getOne AlonzoTxOut . unFlip . currentState)
          :* (Fn $ Comp . fmap K . getOne BabbageTxOut . unFlip . currentState)
          :* (Fn $ Comp . fmap K . getOne ConwayTxOut . unFlip . currentState)
          :* Nil
     in
      hcollapse <$> (hsequence' $ hap np $ Telescope.tip idx)
   where
    getOne ::
      forall proto era.
      ShelleyCompatible proto era =>
      (TxOut (LedgerState (ShelleyBlock proto era)) -> CardanoTxOut c) ->
      LedgerState (ShelleyBlock proto era) EmptyMK ->
      Decoder s (LedgerTables (LedgerState (HardForkBlock (CardanoEras c))) ValuesMK)
    getOne toCardanoTxOut st =
      let certInterns =
            internsFromMap $
              shelleyLedgerState st
                ^. SL.nesEsL
                  . SL.esLStateL
                  . SL.lsCertStateL
                  . SL.certDStateL
                  . SL.dsUnifiedL
                  . SL.umElemsL
       in LedgerTables . ValuesMK
            <$> eraDecoder @era (decodeMap decodeMemPack (toCardanoTxOut <$> decShareCBOR certInterns))
