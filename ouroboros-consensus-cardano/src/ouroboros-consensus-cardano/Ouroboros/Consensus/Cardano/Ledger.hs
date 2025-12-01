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

module Ouroboros.Consensus.Cardano.Ledger
  ( CardanoTxOut (..)
  , eliminateCardanoTxOut
  ) where

import qualified Cardano.Crypto.KES.Class as KES
import Cardano.Ledger.Binary.Decoding hiding (Decoder)
import Cardano.Ledger.Binary.Encoding hiding (Encoding)
import Cardano.Ledger.Compactible
import qualified Cardano.Ledger.Conway.State as SL
import Cardano.Ledger.Core (Era, eraDecoder, eraProtVerLow)
import qualified Cardano.Ledger.Crypto.Internal as KES
import Cardano.Ledger.Shelley.LedgerState as SL
  ( esLStateL
  , lsCertStateL
  , nesEsL
  )
import Codec.CBOR.Decoding
import Codec.CBOR.Encoding
import Data.Kind (Type)
import qualified Data.List.Singletons as S
import qualified Data.Map as Map
import Data.MemPack
import Data.Proxy
import Data.SOP.BasicFunctors
import qualified Data.SOP.Dict as Dict
import Data.SOP.Functors
import Data.SOP.Index
import Data.SOP.Strict
import qualified Data.SOP.Tails as Tails
import qualified Data.SOP.Telescope as Telescope
import qualified Data.Singletons as S
import Data.Void
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class
import Ouroboros.Consensus.Byron.Ledger
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.CanHardFork
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.HardFork.Combinator.State.Types
import Ouroboros.Consensus.Ledger.Extended
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
import Ouroboros.Consensus.Util.TypeLevel

-- instance
--   CardanoHardForkConstraints c =>
--   HasCanonicalTxIn (CardanoEras c)
--   where
--   newtype CanonicalTxIn (CardanoEras c) = CardanoTxIn
--     { getCardanoTxIn :: BigEndianTxIn
--     }
--     deriving stock (Show, Eq, Ord)
--     deriving newtype NoThunks

--   injectCanonicalTxIn IZ byronTxIn = absurd byronTxIn
--   injectCanonicalTxIn (IS idx) shelleyTxIn = case idx of
--     IZ -> CardanoTxIn shelleyTxIn
--     IS IZ -> CardanoTxIn shelleyTxIn
--     IS (IS IZ) -> CardanoTxIn shelleyTxIn
--     IS (IS (IS IZ)) -> CardanoTxIn shelleyTxIn
--     IS (IS (IS (IS IZ))) -> CardanoTxIn shelleyTxIn
--     IS (IS (IS (IS (IS IZ)))) -> CardanoTxIn shelleyTxIn
--     IS (IS (IS (IS (IS (IS IZ))))) -> CardanoTxIn shelleyTxIn
--     IS (IS (IS (IS (IS (IS (IS idx')))))) -> case idx' of {}

--   ejectCanonicalTxIn IZ _ =
--     error "ejectCanonicalTxIn: Byron has no TxIns"
--   ejectCanonicalTxIn (IS idx) cardanoTxIn = case idx of
--     IZ -> getCardanoTxIn cardanoTxIn
--     IS IZ -> getCardanoTxIn cardanoTxIn
--     IS (IS IZ) -> getCardanoTxIn cardanoTxIn
--     IS (IS (IS IZ)) -> getCardanoTxIn cardanoTxIn
--     IS (IS (IS (IS IZ))) -> getCardanoTxIn cardanoTxIn
--     IS (IS (IS (IS (IS IZ)))) -> getCardanoTxIn cardanoTxIn
--     IS (IS (IS (IS (IS (IS IZ))))) -> getCardanoTxIn cardanoTxIn
--     IS (IS (IS (IS (IS (IS (IS idx')))))) -> case idx' of {}

-- instance CardanoHardForkConstraints c => MemPack (CanonicalTxIn (CardanoEras c)) where
--   packM = packM . getCardanoTxIn
--   packedByteCount = packedByteCount . getCardanoTxIn
--   unpackM = CardanoTxIn <$> unpackM

foo ::
  CardanoHardForkConstraints c =>
  Dict.Dict (TableConstraints (HardForkBlock (CardanoEras c))) UTxOTable
foo = Dict.Dict

bar ::
  CardanoHardForkConstraints c =>
  Dict.Dict (TableConstraints (HardForkBlock (CardanoEras c))) InstantStakeTable
bar = Dict.Dict

baz0 ::
  Dict.Dict ((~) (TablesForBlock ByronBlock)) '[]
baz0 = Dict.Dict

baz1 ::
  Dict.Dict ((~) (TablesForBlock (ShelleyBlock (TPraos c) ShelleyEra))) '[UTxOTable]
baz1 = Dict.Dict

baz2 ::
  Dict.Dict ((~) (TablesForBlock (ShelleyBlock (TPraos c) AllegraEra))) '[UTxOTable]
baz2 = Dict.Dict

baz3 ::
  Dict.Dict ((~) (TablesForBlock (ShelleyBlock (TPraos c) MaryEra))) '[UTxOTable]
baz3 = Dict.Dict

baz4 ::
  Dict.Dict ((~) (TablesForBlock (ShelleyBlock (TPraos c) AlonzoEra))) '[UTxOTable]
baz4 = Dict.Dict

baz5 ::
  Dict.Dict ((~) (TablesForBlock (ShelleyBlock (TPraos c) BabbageEra))) '[UTxOTable]
baz5 = Dict.Dict

baz6 ::
  Dict.Dict ((~) (TablesForBlock (ShelleyBlock (TPraos c) ConwayEra))) '[UTxOTable]
baz6 = Dict.Dict

baz7 ::
  Dict.Dict
    ((~) (TablesForBlock (ShelleyBlock (TPraos c) DijkstraEra)))
    '[UTxOTable, InstantStakeTable]
baz7 = Dict.Dict

baz8 ::
  forall c.
  Dict.Dict
    ( (~)
        ( NormalizeTablesForHardFork
            (Xs c)
        )
    )
    '[UTxOTable]
baz8 = Dict.Dict

type Xs c =
  [ ByronBlock
  , ShelleyBlock (TPraos c) ShelleyEra
  , ShelleyBlock (TPraos c) AllegraEra
  ]

type family NormalizeTablesForHardFork (xs :: [Type]) :: [TABLE] where
  NormalizeTablesForHardFork
    '[ ByronBlock
     , ShelleyBlock (TPraos c) ShelleyEra
     , ShelleyBlock (TPraos c) AllegraEra
     ] =
    '[UTxOTable]
  -- other concrete patterns...
  NormalizeTablesForHardFork xs = S.Nub (Unions (S.Map MapTablesForBlock xs)) -- fallback if you want

type instance TablesForBlock (HardForkBlock xs) = NormalizeTablesForHardFork xs

-- type DefaultHardForkCoin xs = NS WrapCoin xs

-- This is just necessary because GHC fails to parse the instance below otherwise
--
-- Try:
-- type instance TablesForBlock (HardForkBlock xs)
--   = S.Nub (Unions (S.Map (S.TyCon1 TablesForBlock) xs))
data MapTablesForBlock :: Type S.~> [TABLE]
type instance S.Apply MapTablesForBlock x = TablesForBlock x

baz ::
  Dict.Dict ((~) (TablesForBlock (HardForkBlock (CardanoEras c)))) '[UTxOTable, InstantStakeTable]
baz = Dict.Dict

data CardanoTxOut c
  = ShelleyTxOut {-# UNPACK #-} !(TxOut (ShelleyBlock (TPraos c) ShelleyEra))
  | AllegraTxOut {-# UNPACK #-} !(TxOut (ShelleyBlock (TPraos c) AllegraEra))
  | MaryTxOut {-# UNPACK #-} !(TxOut (ShelleyBlock (TPraos c) MaryEra))
  | AlonzoTxOut !(TxOut (ShelleyBlock (TPraos c) AlonzoEra))
  | BabbageTxOut !(TxOut (ShelleyBlock (Praos c) BabbageEra))
  | ConwayTxOut !(TxOut (ShelleyBlock (Praos c) ConwayEra))
  | DijkstraTxOut !(TxOut (ShelleyBlock (Praos c) DijkstraEra))
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
    TxOut x ->
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
  DijkstraTxOut txout -> f (IS (IS (IS (IS (IS (IS (IS IZ))))))) txout

instance CardanoHardForkConstraints c => HasHardForkTxOut (CardanoEras c) where
  type HardForkTxOut (CardanoEras c) = CardanoTxOut c

  injectHardForkTxOut idx !txOut = case idx of
    IS IZ -> ShelleyTxOut txOut
    IS (IS IZ) -> AllegraTxOut txOut
    IS (IS (IS IZ)) -> MaryTxOut txOut
    IS (IS (IS (IS IZ))) -> AlonzoTxOut txOut
    IS (IS (IS (IS (IS IZ)))) -> BabbageTxOut txOut
    IS (IS (IS (IS (IS (IS IZ))))) -> ConwayTxOut txOut
    IS (IS (IS (IS (IS (IS (IS IZ)))))) -> DijkstraTxOut txOut
    IS (IS (IS (IS (IS (IS (IS (IS idx'))))))) -> case idx' of {}

  ejectHardForkTxOut ::
    forall y.
    Index (CardanoEras c) y ->
    HardForkTxOut (CardanoEras c) ->
    TxOut y
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
  IndexedMemPack LedgerState (HardForkBlock (CardanoEras c)) UTxOTable
  where
  type
    IndexedValue LedgerState UTxOTable (HardForkBlock (CardanoEras c)) =
      Value UTxOTable (HardForkBlock (CardanoEras c))
  indexedTypeName _ _ _ = "CardanoTxOut"
  indexedPackM _ _ _ _ = eliminateCardanoTxOut (const packM)
  indexedPackedByteCount _ _ _ _ = eliminateCardanoTxOut (const packedByteCount)
  indexedUnpackM _ _ _ (HardForkLedgerState (HardForkState idx)) = do
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
            :* (Fn $ const $ Comp $ K . DijkstraTxOut <$> unpackM)
            :* Nil
        )
    hcollapse <$> (hsequence' $ hap np $ Telescope.tip idx)

instance
  CardanoHardForkConstraints c =>
  IndexedMemPack LedgerState (HardForkBlock (CardanoEras c)) InstantStakeTable
  where
  type
    IndexedValue LedgerState InstantStakeTable (HardForkBlock (CardanoEras c)) =
      Value InstantStakeTable (HardForkBlock (CardanoEras c))
  indexedTypeName _ _ _ = typeName @(CompactForm Coin)
  indexedPackM _ _ _ _ = packM
  indexedPackedByteCount _ _ _ _ = packedByteCount
  indexedUnpackM _ _ _ _ = unpackM

instance
  CardanoHardForkConstraints c =>
  SerializeTablesWithHint LedgerState (HardForkBlock (CardanoEras c)) UTxOTable
  where
  encodeTablesWithHint (HardForkLedgerState (HardForkState idx)) (Table (ValuesMK tbs)) =
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
          :* (Fn $ const $ K $ encOne (Proxy @DijkstraEra))
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
    Decoder s (Table ValuesMK (HardForkBlock (CardanoEras c)) UTxOTable)
  decodeTablesWithHint (HardForkLedgerState (HardForkState idx)) =
    let
      -- These could be made into a CAF to avoid recomputing it, but
      -- it is only used in serialization so it is not critical.
      np =
        ( Fn $
            const $
              Comp $
                K . Table . ValuesMK
                  <$> (Codec.CBOR.Decoding.decodeMapLen >> pure Map.empty)
        )
          :* (Fn $ Comp . fmap K . getOne ShelleyTxOut . unFlip . currentState)
          :* (Fn $ Comp . fmap K . getOne AllegraTxOut . unFlip . currentState)
          :* (Fn $ Comp . fmap K . getOne MaryTxOut . unFlip . currentState)
          :* (Fn $ Comp . fmap K . getOne AlonzoTxOut . unFlip . currentState)
          :* (Fn $ Comp . fmap K . getOne BabbageTxOut . unFlip . currentState)
          :* (Fn $ Comp . fmap K . getOne ConwayTxOut . unFlip . currentState)
          :* (Fn $ Comp . fmap K . getOne DijkstraTxOut . unFlip . currentState)
          :* Nil
     in
      hcollapse <$> (hsequence' $ hap np $ Telescope.tip idx)
   where
    getOne ::
      forall proto era.
      ShelleyCompatible proto era =>
      (TxOut (ShelleyBlock proto era) -> CardanoTxOut c) ->
      LedgerState (ShelleyBlock proto era) EmptyMK ->
      Decoder s (Table ValuesMK (HardForkBlock (CardanoEras c)) UTxOTable)
    getOne toCardanoTxOut st =
      let certInterns =
            internsFromMap $
              shelleyLedgerState st
                ^. SL.nesEsL
                  . SL.esLStateL
                  . SL.lsCertStateL
                  . SL.certDStateL
                  . SL.accountsL
                  . SL.accountsMapL
       in Table . ValuesMK
            <$> eraDecoder @era (decodeMap decodeMemPack (toCardanoTxOut <$> decShareCBOR certInterns))
