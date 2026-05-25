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
  , EmptyPerasError
  )
import Ouroboros.Consensus.Peras.Cert.Mock
  ( MockPerasCert (..)
  , forgeMockPerasCert
  , validateMockPerasCert
  )
import Ouroboros.Consensus.Peras.Vote.Mock
  ( MockPerasVote (..)
  , validateMockPerasVote
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

-- NOTE: this is a mocked up implementation without crypto!
-- TODO: replace this with a concrete implementation using 'Peras.Vote.V1' and
-- 'Peras.Cert.V1' for era >= DijkstraEra.
instance
  ShelleyCompatible proto DijkstraEra =>
  BlockSupportsPeras (ShelleyBlock proto DijkstraEra)
  where
  type PerasVote (ShelleyBlock proto DijkstraEra) = MockPerasVote (ShelleyBlock proto DijkstraEra)
  type PerasCert (ShelleyBlock proto DijkstraEra) = MockPerasCert (ShelleyBlock proto DijkstraEra)
  type PerasError (ShelleyBlock proto DijkstraEra) = EmptyPerasError (ShelleyBlock proto DijkstraEra)

  validatePerasVote = validateMockPerasVote
  validatePerasCert = validateMockPerasCert
  forgePerasCert = forgeMockPerasCert

  -- TODO: extract actual Peras certificates from blocks
  getPerasCertInBlock _ = Nothing
