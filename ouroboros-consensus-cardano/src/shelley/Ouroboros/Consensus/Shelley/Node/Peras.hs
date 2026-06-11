{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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
import Data.Proxy (Proxy (..))
import Lens.Micro ((^.))
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  )
import qualified Ouroboros.Consensus.Peras.Cert.V1 as V1
import Ouroboros.Consensus.Peras.Context
  ( LedgerStateHeaderStateSupportsPerasVoting (..)
  , V1PerasEpochContextResolver
  , unsafeBoundedPerasEpochContextWithMinMaxBounds
  , v1AbsorbErrorInResolver
  , v1InitPerasEpochContextResolver
  , v1ResolveRoundNo
  )
import qualified Ouroboros.Consensus.Peras.Crypto.BLS as BLS
import Ouroboros.Consensus.Peras.Crypto.BLS.Unsafe (unsafePerasBLSPrivateKeyFromEnv)
import qualified Ouroboros.Consensus.Peras.Error.V1 as V1
import qualified Ouroboros.Consensus.Peras.State.V1 as V1
import qualified Ouroboros.Consensus.Peras.Vote.V1 as V1
import qualified Ouroboros.Consensus.Peras.Voting.V1 as V1
import Ouroboros.Consensus.Shelley.Ledger.Block
  ( ShelleyBlock (..)
  , ShelleyCompatible
  )
import Ouroboros.Consensus.Shelley.Ledger.Ledger ()

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
    case ( V1.fromOpaqueByteArray
             (Proxy @(ShelleyBlock proto DijkstraEra))
             byteArray
         ) of
      Right cert -> Just cert
      -- NOTE: could also return 'Nothing', but for now we probably don't want
      -- to silently ignore deserialization errors.
      Left err -> error err

  readPerasPrivateKeyFromEnv _proxy = unsafePerasBLSPrivateKeyFromEnv

  blockDoesReallySupportsPeras _proxy = True

instance
  ShelleyCompatible proto ShelleyEra =>
  LedgerStateHeaderStateSupportsPerasVoting (ShelleyBlock proto ShelleyEra)
instance
  ShelleyCompatible proto AllegraEra =>
  LedgerStateHeaderStateSupportsPerasVoting (ShelleyBlock proto AllegraEra)
instance
  ShelleyCompatible proto MaryEra =>
  LedgerStateHeaderStateSupportsPerasVoting
    ( ShelleyBlock
        proto
        MaryEra
    )
instance
  ShelleyCompatible proto AlonzoEra =>
  LedgerStateHeaderStateSupportsPerasVoting (ShelleyBlock proto AlonzoEra)
instance
  ShelleyCompatible proto BabbageEra =>
  LedgerStateHeaderStateSupportsPerasVoting (ShelleyBlock proto BabbageEra)
instance
  ShelleyCompatible proto ConwayEra =>
  LedgerStateHeaderStateSupportsPerasVoting (ShelleyBlock proto ConwayEra)

instance
  ShelleyCompatible proto DijkstraEra =>
  LedgerStateHeaderStateSupportsPerasVoting (ShelleyBlock proto DijkstraEra)
  where
  type
    PerasEpochContextResolver (ShelleyBlock proto DijkstraEra) =
      V1PerasEpochContextResolver (ShelleyBlock proto DijkstraEra)

  ledgerStateHeaderStateMkPerasVotingCommitteeInput = V1.ledgerStateHeaderStateMkPerasVotingCommitteeInput

  ledgerStateHeaderStateMkPerasEpochContextResolver ledgerState headerState =
    v1AbsorbErrorInResolver $
      v1InitPerasEpochContextResolver . unsafeBoundedPerasEpochContextWithMinMaxBounds
        <$> (ledgerStateHeaderStateMkPerasEpochContext ledgerState headerState)

  resolveRoundNo = v1ResolveRoundNo
