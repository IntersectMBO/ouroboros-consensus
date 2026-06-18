{-# LANGUAGE BangPatterns #-}
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
-- TODO: can we un-orphan this module?
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Cardano.Ledger
  ( CardanoTxOut (..)
  , eliminateCardanoTxOut
  ) where

import Data.MemPack
import Data.SOP.BasicFunctors
import Data.SOP.Index
import Data.SOP.Strict
import qualified Data.SOP.Telescope as Telescope
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.CanHardFork
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.Ledger.Basics (TxOut)
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Shelley.Ledger
  ( IsShelleyBlock
  , ShelleyBlock
  )
import Ouroboros.Consensus.Util.IndexedMemPack

-- TODO @js: 'CardanoTxOut' is used only by the (deferred) snapshot-conversion
-- tool, which decodes on-disk entries into it. Consensus proper does not need
-- it: the HFC values are era-tagged (@'NS' 'WrapValues'@). Delete this whole
-- module together with the 'Util.IndexedMemPack' instances once that tool is
-- ported to per-era (de)serialisation.
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

instance
  CardanoHardForkConstraints c =>
  IndexedMemPack LedgerState (HardForkBlock (CardanoEras c)) (CardanoTxOut c)
  where
  indexedTypeName _ _ = "CardanoTxOut"
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
            :* (Fn $ const $ Comp $ K . DijkstraTxOut <$> unpackM)
            :* Nil
        )
    hcollapse <$> (hsequence' $ hap np $ Telescope.tip idx)
