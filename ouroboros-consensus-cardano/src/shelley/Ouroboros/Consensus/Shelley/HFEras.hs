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

import Cardano.Ledger.BaseTypes (ProtVer (..))
import Cardano.Ledger.Binary (getVersion32, mkVersion32)
import Cardano.Ledger.Block
  ( BlockHeaderVersionInfo (..)
  , LeiosEraBlockHeader (..)
  , PraosEraBlockHeader (..)
  )
import Cardano.Protocol.Crypto
import Cardano.Protocol.Praos.BlockHeader (Header)
import Data.Maybe (fromMaybe)
import Data.Maybe.Strict (StrictMaybe (SNothing))
import Lens.Micro (lens)
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

type StandardDijkstraBlock = ShelleyBlock (Praos StandardCrypto) DijkstraEra

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

instance Praos.PraosCrypto c => ShelleyCompatible (Praos c) DijkstraEra

instance Leios.LeiosCrypto c => ShelleyCompatible (Leios.Leios c) DijkstraEra

-- | The ledger expects Dijkstra blocks to carry a Leios block header, but
-- consensus still uses the Praos header for the Dijkstra era, so this instance
-- adapts the Praos header to the Leios interface:
--
-- * The version info is the header's protocol version, which has the same wire
--   format: the major version is the highest supported major version and the
--   minor version is the self-reported software tag. A major version that is
--   not a valid 'Version' is clamped to 'maxBound' when set.
--
-- * A Praos header never announces Endorser Block references, so the
--   announcement always reads as 'SNothing' and setting it has no effect.
--
-- * The previous nonce uses the class default ('NeutralNonce'), which the
--   ledger itself describes as a stub until Peras is implemented.
--
-- TODO @js: use the Leios block header from @cardano-protocol@ for the Dijkstra
-- era and remove this instance.
instance Crypto c => LeiosEraBlockHeader (Header c) DijkstraEra where
  versionInfoBlockHeaderL =
    protVerBlockHeaderL
      . lens
        (\(ProtVer major minor) -> BlockHeaderVersionInfo (getVersion32 major) minor)
        (\_ (BlockHeaderVersionInfo major tag) -> ProtVer (fromMaybe maxBound (mkVersion32 major)) tag)
  ebReferencesAnnouncementBlockHeaderL = lens (const SNothing) const
