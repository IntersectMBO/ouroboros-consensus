{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Peras support for Shelley.
--
-- NOTE: this module exists solely because the orphan module
-- 'Ouroboros.Consensus.Shelley.Node.Serialisation' needs this instance, but
-- defining it there would be too confusing.
module Ouroboros.Consensus.Shelley.Node.Peras () where

import Cardano.Ledger.Api
import qualified Cardano.Ledger.Dijkstra.BlockBody as Dijkstra
import qualified Cardano.Ledger.Dijkstra.BlockBody as Ledger
import qualified Cardano.Ledger.Shelley.API as SL
import Data.Maybe.Strict (strictMaybeToMaybe)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Lens.Micro ((^.))
import Ouroboros.Consensus.Block.Abstract (ConvertRawHash)
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  )
import Ouroboros.Consensus.Peras.Cert.Opaque (OpaquePerasCert (..), fromOpaquePerasCert)
import qualified Ouroboros.Consensus.Peras.Cert.V1 as V1
import Ouroboros.Consensus.Peras.Context
  ( StateSupportsPerasEpochContext (..)
  , mkBoundedPerasEpochContextWith
  )
import qualified Ouroboros.Consensus.Peras.Crypto.BLS as BLS
import Ouroboros.Consensus.Peras.Crypto.BLS.Unsafe (unsafePerasBLSPrivateKeyFromEnv)
import qualified Ouroboros.Consensus.Peras.Error.V1 as V1
import qualified Ouroboros.Consensus.Peras.State.V1 as V1
import qualified Ouroboros.Consensus.Peras.Vote.V1 as V1
import qualified Ouroboros.Consensus.Peras.Voting.V1 as V1
import Ouroboros.Consensus.Protocol.Abstract
  ( ChainDepStateSupportsPeras
  , ConsensusProtocol (ChainDepState)
  )
import Ouroboros.Consensus.Shelley.Ledger.Block
  ( ShelleyBlock (..)
  )
import Ouroboros.Consensus.Shelley.Ledger.Ledger ()
import Ouroboros.Consensus.Ticked (Ticked)

{-------------------------------------------------------------------------------
  BlockSupportsPeras
-------------------------------------------------------------------------------}

-- Peras support starts with DijkstraEra, so earlier eras use the default void
-- implementation.

instance Typeable proto => BlockSupportsPeras (ShelleyBlock proto ShelleyEra)
instance Typeable proto => BlockSupportsPeras (ShelleyBlock proto AllegraEra)
instance Typeable proto => BlockSupportsPeras (ShelleyBlock proto MaryEra)
instance Typeable proto => BlockSupportsPeras (ShelleyBlock proto AlonzoEra)
instance Typeable proto => BlockSupportsPeras (ShelleyBlock proto BabbageEra)
instance Typeable proto => BlockSupportsPeras (ShelleyBlock proto ConwayEra)

instance
  (Typeable proto, ConvertRawHash (ShelleyBlock proto DijkstraEra)) =>
  BlockSupportsPeras (ShelleyBlock proto DijkstraEra)
  where
  type PerasVote (ShelleyBlock proto DijkstraEra) = V1.PerasVote (ShelleyBlock proto DijkstraEra)
  type PerasCert (ShelleyBlock proto DijkstraEra) = V1.PerasCert (ShelleyBlock proto DijkstraEra)
  type PerasError (ShelleyBlock proto DijkstraEra) = V1.PerasError (ShelleyBlock proto DijkstraEra)
  type PerasCrypto (ShelleyBlock proto DijkstraEra) = BLS.PerasBLSCrypto
  type PerasVotingCommitteeScheme (ShelleyBlock proto DijkstraEra) = V1.PerasVotingCommitteeScheme

  -- [TODO PERAS CERTS IN BLOCKS] this is a temporary solution. In the future,
  -- the ledger will likely be in charge of giving us a proper PerasCert.
  -- That said, validation could still happen on the consensus side, since that
  -- requires access to the committee composition.
  getPerasCertInBlock blk = do
    Ledger.PerasCert byteArray <-
      strictMaybeToMaybe $
        SL.blockBody (shelleyBlockRaw blk)
          ^. Dijkstra.perasCertBlockBodyL
    -- NOTE: could also return 'Nothing', but for now we probably don't want to
    -- silently ignore deserialization errors.
    either (error . show) Just $
      fromOpaquePerasCert
        (Proxy @(ShelleyBlock proto DijkstraEra))
        (OpaquePerasCert byteArray)

  readPerasPrivateKeyFromEnv _proxy = unsafePerasBLSPrivateKeyFromEnv

instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto ShelleyEra)
instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto AllegraEra)
instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto MaryEra)
instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto AlonzoEra)
instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto BabbageEra)
instance
  ( Typeable proto
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto ConwayEra)

instance
  ( Typeable proto
  , ConvertRawHash (ShelleyBlock proto DijkstraEra)
  , ChainDepStateSupportsPeras (ChainDepState proto)
  , ChainDepStateSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto DijkstraEra)
  where
  mkBoundedPerasEpochContext = mkBoundedPerasEpochContextWith V1.mkPerasVotingCommitteeInput
