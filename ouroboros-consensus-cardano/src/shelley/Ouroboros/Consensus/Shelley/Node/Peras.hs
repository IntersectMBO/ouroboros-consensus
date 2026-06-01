{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Mocked Peras support for Shelley.
--
-- NOTE: this module exists solely because the orphan module
-- 'Ouroboros.Consensus.Shelley.Node.Serialisation' needs this instance, but
-- defining it there would be too confusing.
module Ouroboros.Consensus.Shelley.Node.Peras () where

import Cardano.Ledger.Api
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  )
import Ouroboros.Consensus.Committee.WFALS (WFALS)
import qualified Ouroboros.Consensus.Peras.Cert.V1 as V1
import qualified Ouroboros.Consensus.Peras.Crypto.BLS as BLS
import qualified Ouroboros.Consensus.Peras.Error.V1 as V1
import qualified Ouroboros.Consensus.Peras.Vote.V1 as V1
import Ouroboros.Consensus.Protocol.Praos.Peras
  ( PraosStateSupportsPerasVoting (..)
  , praosStatePerasVotingCommitteeInputV1
  )
import Ouroboros.Consensus.Shelley.Ledger.Block
  ( ShelleyBlock
  , ShelleyCompatible
  )

{-------------------------------------------------------------------------------
  BlockSupportsPeras
-------------------------------------------------------------------------------}

-- Peras support starts with DijkstraEra, so earlier eras use the default void
-- implementation.

instance ShelleyCompatible proto ShelleyEra => BlockSupportsPeras (ShelleyBlock proto ShelleyEra)
instance ShelleyCompatible proto AllegraEra => BlockSupportsPeras (ShelleyBlock proto AllegraEra)
instance ShelleyCompatible proto MaryEra => BlockSupportsPeras (ShelleyBlock proto MaryEra)
instance ShelleyCompatible proto AlonzoEra => BlockSupportsPeras (ShelleyBlock proto AlonzoEra)
instance ShelleyCompatible proto BabbageEra => BlockSupportsPeras (ShelleyBlock proto BabbageEra)
instance ShelleyCompatible proto ConwayEra => BlockSupportsPeras (ShelleyBlock proto ConwayEra)

instance
  ShelleyCompatible proto DijkstraEra =>
  BlockSupportsPeras (ShelleyBlock proto DijkstraEra)
  where
  type PerasVote (ShelleyBlock proto DijkstraEra) = V1.PerasVote (ShelleyBlock proto DijkstraEra)
  type PerasCert (ShelleyBlock proto DijkstraEra) = V1.PerasCert (ShelleyBlock proto DijkstraEra)
  type PerasError (ShelleyBlock proto DijkstraEra) = V1.PerasError (ShelleyBlock proto DijkstraEra)
  type PerasCrypto (ShelleyBlock proto DijkstraEra) = BLS.PerasBLSCrypto
  type PerasVotingCommitteeScheme (ShelleyBlock proto DijkstraEra) = WFALS

  -- TODO: extract actual Peras certificates from blocks
  getPerasCertInBlock _ = Nothing

instance
  ShelleyCompatible proto DijkstraEra =>
  PraosStateSupportsPerasVoting (ShelleyBlock proto DijkstraEra)
  where
  praosStatePerasVotingCommitteeInput = praosStatePerasVotingCommitteeInputV1
