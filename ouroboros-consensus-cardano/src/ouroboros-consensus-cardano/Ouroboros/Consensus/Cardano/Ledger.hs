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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO: can we un-orphan this module?
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

import qualified Data.SOP.Tails as Tails
import qualified Cardano.Ledger.Shelley.API as SL
import           Data.Maybe
import           Data.MemPack
import           Data.SOP.BasicFunctors
import           Data.SOP.Index
import           Data.SOP.Strict
import qualified Data.SOP.Telescope as Telescope
import           Data.Void
import           GHC.Generics (Generic)
import           NoThunks.Class
import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Protocol.Praos (Praos)
import           Ouroboros.Consensus.Protocol.TPraos (TPraos)
import           Ouroboros.Consensus.Shelley.Ledger (IsShelleyBlock,
                     ShelleyBlock, ShelleyBlockLedgerEra, ShelleyTxIn (..))
import           Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtoCrypto)
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.IndexedMemPack

instance CardanoHardForkConstraints c
      => HasCanonicalTxIn (CardanoEras c) where
  newtype instance CanonicalTxIn (CardanoEras c) = CardanoTxIn {
      getCardanoTxIn :: SL.TxIn c
    }
    deriving stock (Show, Eq, Ord)
    deriving newtype NoThunks

  injectCanonicalTxIn IZ       byronTxIn   = absurd byronTxIn
  injectCanonicalTxIn (IS idx) shelleyTxIn = case idx of
      IZ                               -> CardanoTxIn $ getShelleyTxIn shelleyTxIn
      IS IZ                            -> CardanoTxIn $ getShelleyTxIn shelleyTxIn
      IS (IS IZ)                       -> CardanoTxIn $ getShelleyTxIn shelleyTxIn
      IS (IS (IS IZ))                  -> CardanoTxIn $ getShelleyTxIn shelleyTxIn
      IS (IS (IS (IS IZ)))             -> CardanoTxIn $ getShelleyTxIn shelleyTxIn
      IS (IS (IS (IS (IS IZ))))        -> CardanoTxIn $ getShelleyTxIn shelleyTxIn
      IS (IS (IS (IS (IS (IS idx'))))) -> case idx' of {}

  ejectCanonicalTxIn IZ _                 =
      error "ejectCanonicalTxIn: Byron has no TxIns"
  ejectCanonicalTxIn (IS idx) cardanoTxIn = case idx of
      IZ                               -> ShelleyTxIn $ getCardanoTxIn cardanoTxIn
      IS IZ                            -> ShelleyTxIn $ getCardanoTxIn cardanoTxIn
      IS (IS IZ)                       -> ShelleyTxIn $ getCardanoTxIn cardanoTxIn
      IS (IS (IS IZ))                  -> ShelleyTxIn $ getCardanoTxIn cardanoTxIn
      IS (IS (IS (IS IZ)))             -> ShelleyTxIn $ getCardanoTxIn cardanoTxIn
      IS (IS (IS (IS (IS IZ))))        -> ShelleyTxIn $ getCardanoTxIn cardanoTxIn
      IS (IS (IS (IS (IS (IS idx'))))) -> case idx' of {}

instance CardanoHardForkConstraints c => MemPack (CanonicalTxIn (CardanoEras c)) where
  packM = packM . getCardanoTxIn
  packedByteCount = packedByteCount . getCardanoTxIn
  unpackM = CardanoTxIn <$> unpackM

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
  MaryTxOut    txout -> f (IS (IS (IS IZ))) txout
  AlonzoTxOut  txout -> f (IS (IS (IS (IS IZ)))) txout
  BabbageTxOut txout -> f (IS (IS (IS (IS (IS IZ))))) txout
  ConwayTxOut  txout -> f (IS (IS (IS (IS (IS (IS IZ)))))) txout

instance CardanoHardForkConstraints c => HasHardForkTxOut (CardanoEras c) where

  type instance HardForkTxOut (CardanoEras c) = CardanoTxOut c

  injectHardForkTxOut idx !txOut = case idx of
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
  ejectHardForkTxOut targetIdx =
    eliminateCardanoTxOut
      (\origIdx ->
           unwrapTxOut
         . maybe (error "anachrony") id
         . Tails.extendWithTails origIdx targetIdx txOutTranslations
         . WrapTxOut
      )

instance CardanoHardForkConstraints c => MemPack (CardanoTxOut c) where
  packM = eliminateCardanoTxOut (\idx txout -> do
                                    packM (toWord8 idx)
                                    packM txout
                                )

  packedByteCount = eliminateCardanoTxOut (\_ txout -> 1 + packedByteCount txout)

  unpackM = do
    tag <- unpackM
    let
      np = ( (Fn $ const $ error "unpacking a byron txout")
          :* (Fn $ const $ Comp $ K . ShelleyTxOut <$> unpackM)
          :* (Fn $ const $ Comp $ K . AllegraTxOut <$> unpackM)
          :* (Fn $ const $ Comp $ K . MaryTxOut    <$> unpackM)
          :* (Fn $ const $ Comp $ K . AlonzoTxOut  <$> unpackM)
          :* (Fn $ const $ Comp $ K . BabbageTxOut <$> unpackM)
          :* (Fn $ const $ Comp $ K . ConwayTxOut  <$> unpackM)
          :* Nil
          )
    hcollapse <$>
      (hsequence'
      $ hap np
      $ fromMaybe (error "Unknown tag") (nsFromIndex tag :: Maybe (NS (K ()) (CardanoEras c))))

instance CardanoHardForkConstraints c
      => IndexedMemPack (LedgerState (HardForkBlock (CardanoEras c)) EmptyMK) (CardanoTxOut c) where
  indexedTypeName _ = typeName @(CardanoTxOut c)
  indexedPackM _ = eliminateCardanoTxOut (const packM)
  indexedPackedByteCount _ = eliminateCardanoTxOut (const packedByteCount)
  indexedUnpackM (HardForkLedgerState (HardForkState idx)) = do
    let
      np = ( (Fn $ const $ error "unpacking a byron txout")
          :* (Fn $ const $ Comp $ K . ShelleyTxOut <$> unpackM)
          :* (Fn $ const $ Comp $ K . AllegraTxOut <$> unpackM)
          :* (Fn $ const $ Comp $ K . MaryTxOut    <$> unpackM)
          :* (Fn $ const $ Comp $ K . AlonzoTxOut  <$> unpackM)
          :* (Fn $ const $ Comp $ K . BabbageTxOut <$> unpackM)
          :* (Fn $ const $ Comp $ K . ConwayTxOut  <$> unpackM)
          :* Nil
          )
    hcollapse <$> (hsequence' $ hap np $ Telescope.tip idx)
