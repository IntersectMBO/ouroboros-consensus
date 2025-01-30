{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
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

import           Cardano.Crypto.DSIGN (Signable)
import           Cardano.Crypto.Hash (Hash)
import           Cardano.Ledger.Hashes (EraIndependentTxBody, HASH)
import           Cardano.Ledger.Keys (DSIGN)
import           Ouroboros.Consensus.Protocol.Praos (Praos)
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import           Ouroboros.Consensus.Protocol.TPraos (StandardCrypto, TPraos)
import qualified Ouroboros.Consensus.Protocol.TPraos as TPraos
import           Ouroboros.Consensus.Shelley.Eras (AllegraEra, AlonzoEra,
                     BabbageEra, ConwayEra, MaryEra, ShelleyEra,
                     StandardAllegra, StandardAlonzo, StandardBabbage,
                     StandardConway, StandardMary, StandardShelley)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock,
                     ShelleyCompatible)
import           Ouroboros.Consensus.Shelley.Ledger.Protocol ()
import           Ouroboros.Consensus.Shelley.Protocol.Praos ()
import           Ouroboros.Consensus.Shelley.Protocol.TPraos ()
import           Ouroboros.Consensus.Shelley.ShelleyHFC ()

{-------------------------------------------------------------------------------
  Hard fork eras
-------------------------------------------------------------------------------}

type StandardShelleyBlock = ShelleyBlock (TPraos StandardCrypto) StandardShelley

type StandardAllegraBlock = ShelleyBlock (TPraos StandardCrypto) StandardAllegra

type StandardMaryBlock = ShelleyBlock (TPraos StandardCrypto) StandardMary

type StandardAlonzoBlock = ShelleyBlock (TPraos StandardCrypto) StandardAlonzo

type StandardBabbageBlock = ShelleyBlock (Praos StandardCrypto) StandardBabbage

type StandardConwayBlock = ShelleyBlock (Praos StandardCrypto) StandardConway

{-------------------------------------------------------------------------------
  ShelleyCompatible
-------------------------------------------------------------------------------}

instance
  (TPraos.PraosCrypto c, c ~ StandardCrypto, Signable DSIGN (Hash HASH EraIndependentTxBody)) =>
  ShelleyCompatible (TPraos c) ShelleyEra

instance
  (TPraos.PraosCrypto c, c ~ StandardCrypto, Signable DSIGN (Hash HASH EraIndependentTxBody)) =>
  ShelleyCompatible (TPraos c) AllegraEra

instance
  (TPraos.PraosCrypto c, c ~ StandardCrypto, Signable DSIGN (Hash HASH EraIndependentTxBody)) =>
  ShelleyCompatible (TPraos c) MaryEra

instance
  (TPraos.PraosCrypto c, c ~ StandardCrypto, Signable DSIGN (Hash HASH EraIndependentTxBody)) =>
  ShelleyCompatible (TPraos c) AlonzoEra

-- This instance is required since the ledger view forecast function for
-- Praos/Babbage still goes through the forecast for TPraos. Once this is
-- addressed, we could remove this instance.
instance
  (Praos.PraosCrypto c, c ~ StandardCrypto, TPraos.PraosCrypto c, c ~ StandardCrypto) =>
  ShelleyCompatible (TPraos c) BabbageEra

instance
  (Praos.PraosCrypto c, c ~ StandardCrypto) => ShelleyCompatible (Praos c) BabbageEra

-- This instance is required since the ledger view forecast function for
-- Praos/Conway still goes through the forecast for TPraos. Once this is
-- addressed, we could remove this instance.
instance
  (Praos.PraosCrypto c, c ~ StandardCrypto, TPraos.PraosCrypto c, c ~ StandardCrypto) =>
  ShelleyCompatible (TPraos c) ConwayEra

instance
  (Praos.PraosCrypto c, c ~ StandardCrypto) => ShelleyCompatible (Praos c) ConwayEra
