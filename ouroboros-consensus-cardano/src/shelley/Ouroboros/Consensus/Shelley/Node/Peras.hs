{-# LANGUAGE DerivingStrategies #-}
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

import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , VoidPerasError
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

-- NOTE: this is a mocked up implementation without crypto!

-- TODO: replace this with a concrete implementation using 'Peras.Vote.V1' and
-- 'Peras.Cert.V1', either forall eras or for @forall era. era >= DijkstraEra@.

instance
  ShelleyCompatible proto era =>
  BlockSupportsPeras (ShelleyBlock proto era)
  where
  type PerasVote (ShelleyBlock proto era) = MockPerasVote (ShelleyBlock proto era)
  type PerasCert (ShelleyBlock proto era) = MockPerasCert (ShelleyBlock proto era)
  type PerasError (ShelleyBlock proto era) = VoidPerasError (ShelleyBlock proto era)

  validatePerasVote = validateMockPerasVote
  validatePerasCert = validateMockPerasCert
  forgePerasCert = forgeMockPerasCert

  -- TODO: extract actual Peras certificates from blocks
  getPerasCertInBlock _ = Nothing
