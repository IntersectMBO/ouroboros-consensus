{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Empty Peras support for Byron.
--
-- NOTE: this module exists solely because the orphan module
-- 'Ouroboros.Consensus.Byron.Node.Serialisation' needs some of these instances,
-- but defining them there would be too confusing.
module Ouroboros.Consensus.Byron.Node.Peras () where

import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , VoidPerasCert
  , VoidPerasCrypto
  , VoidPerasError
  , VoidPerasVote
  , VoidPerasVotingCommitteeScheme
  , defaultForgePerasCert
  , defaultForgePerasVoteIfEligible
  , defaultVerifyPerasCert
  , defaultVerifyPerasVote
  )
import Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)

{-------------------------------------------------------------------------------
  BlockSupportsPeras
-------------------------------------------------------------------------------}

instance BlockSupportsPeras ByronBlock where
  type PerasVote ByronBlock = VoidPerasVote ByronBlock
  type PerasCert ByronBlock = VoidPerasCert ByronBlock
  type PerasError ByronBlock = VoidPerasError ByronBlock
  type PerasCrypto ByronBlock = VoidPerasCrypto ByronBlock
  type PerasVotingCommitteeScheme ByronBlock = VoidPerasVotingCommitteeScheme
  forgePerasVoteIfEligible = defaultForgePerasVoteIfEligible
  verifyPerasVote = defaultVerifyPerasVote
  forgePerasCert = defaultForgePerasCert
  verifyPerasCert = defaultVerifyPerasCert
  getPerasCertInBlock _ = Right Nothing
