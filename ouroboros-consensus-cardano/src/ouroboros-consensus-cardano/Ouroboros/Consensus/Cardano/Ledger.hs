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
  , toNS
  ) where

import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Shelley.API as SL
import           Data.SOP.Index
import           Data.SOP.Strict
import           Data.Void
import           GHC.Generics
import           NoThunks.Class
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Protocol.Praos (Praos)
import           Ouroboros.Consensus.Protocol.TPraos (TPraos)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import           Ouroboros.Consensus.TypeFamilyWrappers (WrapTxOut (..))

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

toNS :: CardanoTxOut c -> NS WrapTxOut (CardanoEras c)
toNS = \case
    ShelleyTxOut txout ->                     S $ Z $ WrapTxOut txout
    AllegraTxOut txout ->                 S $ S $ Z $ WrapTxOut txout
    MaryTxOut txout    ->             S $ S $ S $ Z $ WrapTxOut txout
    AlonzoTxOut txout  ->         S $ S $ S $ S $ Z $ WrapTxOut txout
    BabbageTxOut txout ->     S $ S $ S $ S $ S $ Z $ WrapTxOut txout
    ConwayTxOut txout  -> S $ S $ S $ S $ S $ S $ Z $ WrapTxOut txout

fromNS :: NS WrapTxOut (CardanoEras c) -> CardanoTxOut c
fromNS = \case
  S (Z (WrapTxOut txOut)) -> ShelleyTxOut txOut
  S (S (Z (WrapTxOut txOut))) -> AllegraTxOut txOut
  S (S (S (Z (WrapTxOut txOut)))) -> MaryTxOut txOut
  S (S (S (S (Z (WrapTxOut txOut))))) -> AlonzoTxOut txOut
  S (S (S (S (S (Z (WrapTxOut txOut)))))) -> BabbageTxOut txOut
  S (S (S (S (S (S (Z (WrapTxOut txOut))))))) -> ConwayTxOut txOut

-- TODO: the indirection through the NS is not ideal in terms of performance,
-- but it does mean we can reuse a lot of the general hardfork machinery. We
-- should investigate whether it is worth it to provide a more direct
-- implementation.
instance CanHardFork (CardanoEras c) => HasHardForkTxOut (CardanoEras c) where
  type instance HardForkTxOut (CardanoEras c) = CardanoTxOut c
  injectHardForkTxOut idx txOut = fromNS $ injectHardForkTxOutDefault idx txOut
  distribHardForkTxOut idx cardanoTxOut = distribHardForkTxOutDefault idx $ toNS cardanoTxOut

-- TODO: the indirection through the NS is not ideal in terms of performance,
-- but it does mean we can reuse a lot of the general hardfork machinery. We
-- should investigate whether it is worth it to provide a more direct
-- implementation.
instance CardanoHardForkConstraints c => SerializeHardForkTxOut (CardanoEras c) where
  encodeHardForkTxOut _ txOut = encodeHardForkTxOutDefault $ toNS txOut
  decodeHardForkTxOut _ = fromNS <$> decodeHardForkTxOutDefault
