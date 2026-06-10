{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Mocked Peras support for Shelley.
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
import Data.Typeable (Typeable)
import Lens.Micro ((^.))
import Ouroboros.Consensus.Block.Abstract (ConvertRawHash)
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  )
import qualified Ouroboros.Consensus.Peras.Cert.V1 as V1
import Ouroboros.Consensus.Peras.Context
  ( StateSupportsPerasEpochContext (..)
  , V1PerasEpochContextResolver
  )
import qualified Ouroboros.Consensus.Peras.Crypto.BLS as BLS
import Ouroboros.Consensus.Peras.Crypto.BLS.Unsafe (unsafePerasBLSPrivateKeyFromEnv)
import qualified Ouroboros.Consensus.Peras.Error.V1 as V1
import qualified Ouroboros.Consensus.Peras.State.V1 as V1
import qualified Ouroboros.Consensus.Peras.Vote.V1 as V1
import qualified Ouroboros.Consensus.Peras.Voting.V1 as V1
import Ouroboros.Consensus.Protocol.Abstract
  ( AChainDepSupportsPeras
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

  getPerasCertInBlock blk = do
    -- [TODO PERAS CERTS IN BLOCKS] there will be a bytearray here after
    -- integrating a newer version of Ledger. From there, we would need to
    -- deserialize into a V1.PerasCert. For now, we could simply blow up if
    -- the decoding fails.
    Ledger.PerasCert _ <-
      strictMaybeToMaybe $
        SL.blockBody (shelleyBlockRaw blk)
          ^. Dijkstra.perasCertBlockBodyL

    Nothing -- to be replaced with the deserialized certificate

  readPerasPrivateKeyFromEnv _proxy = unsafePerasBLSPrivateKeyFromEnv

  blockDoesReallySupportsPeras _proxy = True

instance
  ( Typeable proto
  , AChainDepSupportsPeras (ChainDepState proto)
  , AChainDepSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto ShelleyEra)
instance
  ( Typeable proto
  , AChainDepSupportsPeras (ChainDepState proto)
  , AChainDepSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto AllegraEra)
instance
  ( Typeable proto
  , AChainDepSupportsPeras (ChainDepState proto)
  , AChainDepSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto MaryEra)
instance
  ( Typeable proto
  , AChainDepSupportsPeras (ChainDepState proto)
  , AChainDepSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto AlonzoEra)
instance
  ( Typeable proto
  , AChainDepSupportsPeras (ChainDepState proto)
  , AChainDepSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto BabbageEra)
instance
  ( Typeable proto
  , AChainDepSupportsPeras (ChainDepState proto)
  , AChainDepSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto ConwayEra)

instance
  ( Typeable proto
  , ConvertRawHash (ShelleyBlock proto DijkstraEra)
  , AChainDepSupportsPeras (ChainDepState proto)
  , AChainDepSupportsPeras (Ticked (ChainDepState proto))
  ) =>
  StateSupportsPerasEpochContext (ShelleyBlock proto DijkstraEra)
  where
  type
    PerasEpochContextResolver (ShelleyBlock proto DijkstraEra) =
      V1PerasEpochContextResolver (ShelleyBlock proto DijkstraEra)

  mkPerasVotingCommitteeInput = V1.mkPerasVotingCommitteeInput
