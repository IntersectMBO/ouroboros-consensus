{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion (
    ShelleyNodeToClientVersion (..)
  , ShelleyNodeToNodeVersion (..)
  ) where

import qualified Data.Map.Strict as Map
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Shelley.Ledger.Block

data ShelleyNodeToNodeVersion = ShelleyNodeToNodeVersion1
  deriving (Show, Eq, Ord, Enum, Bounded)

data ShelleyNodeToClientVersion =
    -- | New queries introduced: GetConstitutionHash, GetFilteredVoteDelegatees
    ShelleyNodeToClientVersion8

    -- | New queries introduced: GetProposals, GetRatifyState
  | ShelleyNodeToClientVersion9

    -- | New queries introduced: GetFuturePParams
  | ShelleyNodeToClientVersion10

    -- | New queries introduced: GetBigLedgerPeerSnapshot
  | ShelleyNodeToClientVersion11

    -- | New queries introduced: QueryStakePoolDefaultVote
    -- Queries deprecated: GetProposedPParamsUpdates
  | ShelleyNodeToClientVersion12
  deriving (Show, Eq, Ord, Enum, Bounded)

instance HasNetworkProtocolVersion (ShelleyBlock proto era) where
  type BlockNodeToNodeVersion   (ShelleyBlock proto era) = ShelleyNodeToNodeVersion
  type BlockNodeToClientVersion (ShelleyBlock proto era) = ShelleyNodeToClientVersion

-- TODO #2668 make this era-specific
instance SupportedNetworkProtocolVersion (ShelleyBlock proto era) where
  supportedNodeToNodeVersions   _ = Map.fromList [
        (NodeToNodeV_13, ShelleyNodeToNodeVersion1)
      , (NodeToNodeV_14, ShelleyNodeToNodeVersion1)
      , (NodeToNodeV_15, ShelleyNodeToNodeVersion1)
      ]
  supportedNodeToClientVersions _ = Map.fromList [
        (NodeToClientV_16, ShelleyNodeToClientVersion8)
      , (NodeToClientV_17, ShelleyNodeToClientVersion9)
      , (NodeToClientV_18, ShelleyNodeToClientVersion10)
      , (NodeToClientV_19, ShelleyNodeToClientVersion11)
      , (NodeToClientV_20, ShelleyNodeToClientVersion12)
      ]

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault
