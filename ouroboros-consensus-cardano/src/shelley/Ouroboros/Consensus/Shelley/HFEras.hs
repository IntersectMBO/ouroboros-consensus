{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Hard fork eras.
--
--   Compare this to 'Ouroboros.Consensus.Shelley.Eras', which defines ledger
--   eras. This module defines hard fork eras, which are a combination of a
--   ledger era and a protocol.
module Ouroboros.Consensus.Shelley.HFEras
  ( StandardAllegraBlock
  , StandardAlonzoBlock
  , StandardBabbageBlock
  , StandardConwayBlock
  , StandardDijkstraBlock
  , StandardMaryBlock
  , StandardShelleyBlock
  ) where

import Cardano.Protocol.Crypto
import Ouroboros.Consensus.Protocol.Leios (Leios)
import qualified Ouroboros.Consensus.Protocol.Leios as Leios
import Ouroboros.Consensus.Protocol.Praos (Praos)
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import qualified Ouroboros.Consensus.Protocol.TPraos as TPraos
import Ouroboros.Consensus.Shelley.Eras
  ( AllegraEra
  , AlonzoEra
  , BabbageEra
  , ConwayEra
  , DijkstraEra
  , MaryEra
  , ShelleyEra
  )
import Ouroboros.Consensus.Shelley.Ledger.Block
  ( ShelleyBlock
  , ShelleyCompatible
  )
import Ouroboros.Consensus.Shelley.Ledger.Protocol ()
import Ouroboros.Consensus.Shelley.Protocol.Leios ()
import Ouroboros.Consensus.Shelley.Protocol.Praos ()
import Ouroboros.Consensus.Shelley.Protocol.TPraos ()
import Ouroboros.Consensus.Shelley.ShelleyHFC ()

{-------------------------------------------------------------------------------
  Hard fork eras
-------------------------------------------------------------------------------}

type StandardShelleyBlock = ShelleyBlock (TPraos StandardCrypto) ShelleyEra

type StandardAllegraBlock = ShelleyBlock (TPraos StandardCrypto) AllegraEra

type StandardMaryBlock = ShelleyBlock (TPraos StandardCrypto) MaryEra

type StandardAlonzoBlock = ShelleyBlock (TPraos StandardCrypto) AlonzoEra

type StandardBabbageBlock = ShelleyBlock (Praos StandardCrypto) BabbageEra

type StandardConwayBlock = ShelleyBlock (Praos StandardCrypto) ConwayEra

type StandardDijkstraBlock = ShelleyBlock (Leios StandardCrypto) DijkstraEra

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

instance Praos.PraosCrypto c => ShelleyCompatible (Praos c) BabbageEra

instance Praos.PraosCrypto c => ShelleyCompatible (Praos c) ConwayEra

instance Leios.LeiosCrypto c => ShelleyCompatible (Leios c) DijkstraEra
