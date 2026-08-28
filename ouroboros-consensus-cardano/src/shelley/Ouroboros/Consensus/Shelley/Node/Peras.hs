{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Peras support for Shelley.
--
-- NOTE: this module exists solely because the orphan module
-- 'Ouroboros.Consensus.Shelley.Node.Serialisation' needs some of these
-- instances, but defining them there would be too confusing.
module Ouroboros.Consensus.Shelley.Node.Peras () where

import Cardano.Ledger.Api
import Data.Typeable (Typeable)
import Ouroboros.Consensus.Block.Abstract (ConvertRawHash)
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
import Ouroboros.Consensus.HardFork.History (EpochToPerasRoundInfo, forgetEraIndex)
import qualified Ouroboros.Consensus.Peras.Cert.V1 as V1
import Ouroboros.Consensus.Peras.Context
  ( StateSupportsPerasEpochContext (..)
  , mkBoundedPerasEpochContextWith
  )
import qualified Ouroboros.Consensus.Peras.Crypto.BLS as BLS
import qualified Ouroboros.Consensus.Peras.Error.V1 as V1
import qualified Ouroboros.Consensus.Peras.Vote.V1 as V1
import qualified Ouroboros.Consensus.Peras.Voting.V1 as V1
import Ouroboros.Consensus.Protocol.Abstract
  ( ChainDepStateSupportsPeras
  , ConsensusProtocol (..)
  )
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock (..))
import Ouroboros.Consensus.Shelley.Ledger.Ledger ()
import Ouroboros.Consensus.Ticked (Ticked)

{-------------------------------------------------------------------------------
  StateSupportsPerasEpochContext
-------------------------------------------------------------------------------}

instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto ShelleyEra)
  where
  type
    MaybeEraIndexedEpochToPerasRoundInfo (ShelleyBlock proto ShelleyEra) =
      EpochToPerasRoundInfo
  toMaybeEraIndexedEpochToPerasRoundInfo _ = forgetEraIndex
  fromMaybeEraIndexedEpochToPerasRoundInfo _ = id
  mkBoundedPerasEpochContext = error "mkBoundedPerasEpochContext: ShelleyEra does not support Peras"

instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto AllegraEra)
  where
  type
    MaybeEraIndexedEpochToPerasRoundInfo (ShelleyBlock proto AllegraEra) =
      EpochToPerasRoundInfo
  toMaybeEraIndexedEpochToPerasRoundInfo _ = forgetEraIndex
  fromMaybeEraIndexedEpochToPerasRoundInfo _ = id
  mkBoundedPerasEpochContext = error "mkBoundedPerasEpochContext: AllegraEra does not support Peras"

instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto MaryEra)
  where
  type
    MaybeEraIndexedEpochToPerasRoundInfo (ShelleyBlock proto MaryEra) =
      EpochToPerasRoundInfo
  toMaybeEraIndexedEpochToPerasRoundInfo _ = forgetEraIndex
  fromMaybeEraIndexedEpochToPerasRoundInfo _ = id
  mkBoundedPerasEpochContext = error "mkBoundedPerasEpochContext: MaryEra does not support Peras"

instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto AlonzoEra)
  where
  type
    MaybeEraIndexedEpochToPerasRoundInfo (ShelleyBlock proto AlonzoEra) =
      EpochToPerasRoundInfo
  toMaybeEraIndexedEpochToPerasRoundInfo _ = forgetEraIndex
  fromMaybeEraIndexedEpochToPerasRoundInfo _ = id
  mkBoundedPerasEpochContext = error "mkBoundedPerasEpochContext: AlonzoEra does not support Peras"

instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto BabbageEra)
  where
  type
    MaybeEraIndexedEpochToPerasRoundInfo (ShelleyBlock proto BabbageEra) =
      EpochToPerasRoundInfo
  toMaybeEraIndexedEpochToPerasRoundInfo _ = forgetEraIndex
  fromMaybeEraIndexedEpochToPerasRoundInfo _ = id
  mkBoundedPerasEpochContext = error "mkBoundedPerasEpochContext: BabbageEra does not support Peras"

instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto ConwayEra)
  where
  type
    MaybeEraIndexedEpochToPerasRoundInfo (ShelleyBlock proto ConwayEra) =
      EpochToPerasRoundInfo
  toMaybeEraIndexedEpochToPerasRoundInfo _ = forgetEraIndex
  fromMaybeEraIndexedEpochToPerasRoundInfo _ = id
  mkBoundedPerasEpochContext = error "mkBoundedPerasEpochContext: ConwayEra does not support Peras"

instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto DijkstraEra)
  where
  type
    MaybeEraIndexedEpochToPerasRoundInfo (ShelleyBlock proto DijkstraEra) =
      EpochToPerasRoundInfo
  toMaybeEraIndexedEpochToPerasRoundInfo _ = forgetEraIndex
  fromMaybeEraIndexedEpochToPerasRoundInfo _ = id
  mkBoundedPerasEpochContext = mkBoundedPerasEpochContextWith V1.mkPerasVotingCommitteeInput

{-------------------------------------------------------------------------------
  BlockSupportsPeras
-------------------------------------------------------------------------------}

instance Typeable proto => BlockSupportsPeras (ShelleyBlock proto ShelleyEra) where
  type PerasVote (ShelleyBlock proto ShelleyEra) = VoidPerasVote (ShelleyBlock proto ShelleyEra)
  type PerasCert (ShelleyBlock proto ShelleyEra) = VoidPerasCert (ShelleyBlock proto ShelleyEra)
  type PerasError (ShelleyBlock proto ShelleyEra) = VoidPerasError (ShelleyBlock proto ShelleyEra)
  type PerasCrypto (ShelleyBlock proto ShelleyEra) = VoidPerasCrypto (ShelleyBlock proto ShelleyEra)
  type PerasVotingCommitteeScheme (ShelleyBlock proto ShelleyEra) = VoidPerasVotingCommitteeScheme
  forgePerasVoteIfEligible = defaultForgePerasVoteIfEligible
  verifyPerasVote = defaultVerifyPerasVote
  forgePerasCert = defaultForgePerasCert
  verifyPerasCert = defaultVerifyPerasCert
  getPerasCertInBlock _ = Right Nothing

instance Typeable proto => BlockSupportsPeras (ShelleyBlock proto AllegraEra) where
  type PerasVote (ShelleyBlock proto AllegraEra) = VoidPerasVote (ShelleyBlock proto AllegraEra)
  type PerasCert (ShelleyBlock proto AllegraEra) = VoidPerasCert (ShelleyBlock proto AllegraEra)
  type PerasError (ShelleyBlock proto AllegraEra) = VoidPerasError (ShelleyBlock proto AllegraEra)
  type PerasCrypto (ShelleyBlock proto AllegraEra) = VoidPerasCrypto (ShelleyBlock proto AllegraEra)
  type PerasVotingCommitteeScheme (ShelleyBlock proto AllegraEra) = VoidPerasVotingCommitteeScheme
  forgePerasVoteIfEligible = defaultForgePerasVoteIfEligible
  verifyPerasVote = defaultVerifyPerasVote
  forgePerasCert = defaultForgePerasCert
  verifyPerasCert = defaultVerifyPerasCert
  getPerasCertInBlock _ = Right Nothing

instance Typeable proto => BlockSupportsPeras (ShelleyBlock proto MaryEra) where
  type PerasVote (ShelleyBlock proto MaryEra) = VoidPerasVote (ShelleyBlock proto MaryEra)
  type PerasCert (ShelleyBlock proto MaryEra) = VoidPerasCert (ShelleyBlock proto MaryEra)
  type PerasError (ShelleyBlock proto MaryEra) = VoidPerasError (ShelleyBlock proto MaryEra)
  type PerasCrypto (ShelleyBlock proto MaryEra) = VoidPerasCrypto (ShelleyBlock proto MaryEra)
  type PerasVotingCommitteeScheme (ShelleyBlock proto MaryEra) = VoidPerasVotingCommitteeScheme
  forgePerasVoteIfEligible = defaultForgePerasVoteIfEligible
  verifyPerasVote = defaultVerifyPerasVote
  forgePerasCert = defaultForgePerasCert
  verifyPerasCert = defaultVerifyPerasCert
  getPerasCertInBlock _ = Right Nothing

instance Typeable proto => BlockSupportsPeras (ShelleyBlock proto AlonzoEra) where
  type PerasVote (ShelleyBlock proto AlonzoEra) = VoidPerasVote (ShelleyBlock proto AlonzoEra)
  type PerasCert (ShelleyBlock proto AlonzoEra) = VoidPerasCert (ShelleyBlock proto AlonzoEra)
  type PerasError (ShelleyBlock proto AlonzoEra) = VoidPerasError (ShelleyBlock proto AlonzoEra)
  type PerasCrypto (ShelleyBlock proto AlonzoEra) = VoidPerasCrypto (ShelleyBlock proto AlonzoEra)
  type PerasVotingCommitteeScheme (ShelleyBlock proto AlonzoEra) = VoidPerasVotingCommitteeScheme
  forgePerasVoteIfEligible = defaultForgePerasVoteIfEligible
  verifyPerasVote = defaultVerifyPerasVote
  forgePerasCert = defaultForgePerasCert
  verifyPerasCert = defaultVerifyPerasCert
  getPerasCertInBlock _ = Right Nothing

instance Typeable proto => BlockSupportsPeras (ShelleyBlock proto BabbageEra) where
  type PerasVote (ShelleyBlock proto BabbageEra) = VoidPerasVote (ShelleyBlock proto BabbageEra)
  type PerasCert (ShelleyBlock proto BabbageEra) = VoidPerasCert (ShelleyBlock proto BabbageEra)
  type PerasError (ShelleyBlock proto BabbageEra) = VoidPerasError (ShelleyBlock proto BabbageEra)
  type PerasCrypto (ShelleyBlock proto BabbageEra) = VoidPerasCrypto (ShelleyBlock proto BabbageEra)
  type PerasVotingCommitteeScheme (ShelleyBlock proto BabbageEra) = VoidPerasVotingCommitteeScheme
  forgePerasVoteIfEligible = defaultForgePerasVoteIfEligible
  verifyPerasVote = defaultVerifyPerasVote
  forgePerasCert = defaultForgePerasCert
  verifyPerasCert = defaultVerifyPerasCert
  getPerasCertInBlock _ = Right Nothing

instance Typeable proto => BlockSupportsPeras (ShelleyBlock proto ConwayEra) where
  type PerasVote (ShelleyBlock proto ConwayEra) = VoidPerasVote (ShelleyBlock proto ConwayEra)
  type PerasCert (ShelleyBlock proto ConwayEra) = VoidPerasCert (ShelleyBlock proto ConwayEra)
  type PerasError (ShelleyBlock proto ConwayEra) = VoidPerasError (ShelleyBlock proto ConwayEra)
  type PerasCrypto (ShelleyBlock proto ConwayEra) = VoidPerasCrypto (ShelleyBlock proto ConwayEra)
  type PerasVotingCommitteeScheme (ShelleyBlock proto ConwayEra) = VoidPerasVotingCommitteeScheme
  forgePerasVoteIfEligible = defaultForgePerasVoteIfEligible
  verifyPerasVote = defaultVerifyPerasVote
  forgePerasCert = defaultForgePerasCert
  verifyPerasCert = defaultVerifyPerasCert
  getPerasCertInBlock _ = Right Nothing

instance
  ( Typeable proto
  , ConvertRawHash (ShelleyBlock proto DijkstraEra)
  ) =>
  BlockSupportsPeras (ShelleyBlock proto DijkstraEra)
  where
  type PerasVote (ShelleyBlock proto DijkstraEra) = V1.PerasVote (ShelleyBlock proto DijkstraEra)
  type PerasCert (ShelleyBlock proto DijkstraEra) = V1.PerasCert (ShelleyBlock proto DijkstraEra)
  type PerasError (ShelleyBlock proto DijkstraEra) = V1.PerasError (ShelleyBlock proto DijkstraEra)
  type PerasCrypto (ShelleyBlock proto DijkstraEra) = BLS.PerasBLSCrypto
  type PerasVotingCommitteeScheme (ShelleyBlock proto DijkstraEra) = V1.PerasVotingCommitteeScheme
  forgePerasVoteIfEligible = defaultForgePerasVoteIfEligible
  verifyPerasVote = defaultVerifyPerasVote
  forgePerasCert = defaultForgePerasCert
  verifyPerasCert = defaultVerifyPerasCert
  getPerasCertInBlock _ = Right Nothing
