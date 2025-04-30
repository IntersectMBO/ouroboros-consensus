{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Hard fork eras.
--
--   Compare this to 'Ouroboros.Consensus.Shelley.Eras', which defines ledger
--   eras. This module defines hard fork eras, which are a combination of a
--   ledger era and a protocol.
module Ouroboros.Consensus.Shelley.HFEras (
    StandardAllegraBlock
  , StandardAlonzoBlock
  , StandardBabbageBlock
  , StandardConwayBlock
  , StandardMaryBlock
  , StandardShelleyBlock
  ) where

import           Cardano.Protocol.Crypto
import           Ouroboros.Consensus.Protocol.Praos (Praos)
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import           Ouroboros.Consensus.Protocol.TPraos (TPraos)
import qualified Ouroboros.Consensus.Protocol.TPraos as TPraos
import           Ouroboros.Consensus.Shelley.Eras (AllegraEra, AlonzoEra,
                     BabbageEra, ConwayEra, MaryEra, ShelleyEra)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock,
                     ShelleyCompatible)
import           Ouroboros.Consensus.Shelley.Ledger.Protocol ()
import           Ouroboros.Consensus.Shelley.Protocol.Praos ()
import           Ouroboros.Consensus.Shelley.Protocol.TPraos ()
import           Ouroboros.Consensus.Shelley.ShelleyHFC ()

{-------------------------------------------------------------------------------
  Hard fork eras
-------------------------------------------------------------------------------}

type StandardShelleyBlock = ShelleyBlock (TPraos StandardCrypto) ShelleyEra

type StandardAllegraBlock = ShelleyBlock (TPraos StandardCrypto) AllegraEra

type StandardMaryBlock = ShelleyBlock (TPraos StandardCrypto) MaryEra

type StandardAlonzoBlock = ShelleyBlock (TPraos StandardCrypto) AlonzoEra

type StandardBabbageBlock = ShelleyBlock (Praos StandardCrypto) BabbageEra

type StandardConwayBlock = ShelleyBlock (Praos StandardCrypto) ConwayEra

{-------------------------------------------------------------------------------
  ShelleyCompatible
-------------------------------------------------------------------------------}

instance
  TPraos.PraosCrypto c =>
  ShelleyCompatible (TPraos c) ShelleyEra

instance
  TPraos.PraosCrypto c =>
  ShelleyCompatible (TPraos c) AllegraEra

instance
  TPraos.PraosCrypto c =>
  ShelleyCompatible (TPraos c) MaryEra

instance
  TPraos.PraosCrypto c =>
  ShelleyCompatible (TPraos c) AlonzoEra

-- This instance is required since the ledger view forecast function for
-- Praos/Babbage still goes through the forecast for TPraos. Once this is
-- addressed, we could remove this instance.
instance
  (Praos.PraosCrypto c, TPraos.PraosCrypto c) =>
  ShelleyCompatible (TPraos c) BabbageEra

instance
  Praos.PraosCrypto c => ShelleyCompatible (Praos c) BabbageEra

-- This instance is required since the ledger view forecast function for
-- Praos/Conway still goes through the forecast for TPraos. Once this is
-- addressed, we could remove this instance.
instance
  (Praos.PraosCrypto c, TPraos.PraosCrypto c) =>
  ShelleyCompatible (TPraos c) ConwayEra

instance
  Praos.PraosCrypto c => ShelleyCompatible (Praos c) ConwayEra
