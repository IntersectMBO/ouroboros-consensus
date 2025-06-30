{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion
  ( ShelleyNodeToClientVersion (..)
  , ShelleyNodeToNodeVersion (..)
  , ledgerPeerSnpahotSupportsSRV
  ) where

import qualified Data.Map.Strict as Map
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Shelley.Ledger.Block
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (LedgerPeerSnapshotSRVSupport (..))

data ShelleyNodeToNodeVersion = ShelleyNodeToNodeVersion1
  deriving (Show, Eq, Ord, Enum, Bounded)

data ShelleyNodeToClientVersion
  = -- | New queries introduced: GetConstitutionHash, GetFilteredVoteDelegatees
    ShelleyNodeToClientVersion8
  | -- | New queries introduced: GetProposals, GetRatifyState
    ShelleyNodeToClientVersion9
  | -- | New queries introduced: GetFuturePParams
    ShelleyNodeToClientVersion10
  | -- | New queries introduced: GetBigLedgerPeerSnapshot
    ShelleyNodeToClientVersion11
  | -- | New queries introduced: QueryStakePoolDefaultVote
    -- Queries deprecated: GetProposedPParamsUpdates
    ShelleyNodeToClientVersion12
  | -- | New encoder for PParams, CompactGenesis
    ShelleyNodeToClientVersion13
  | -- | Support SRV in GetBigLedgerPeerSnapshot
    ShelleyNodeToClientVersion14
  deriving (Show, Eq, Ord, Enum, Bounded)

ledgerPeerSnpahotSupportsSRV :: ShelleyNodeToClientVersion -> LedgerPeerSnapshotSRVSupport
ledgerPeerSnpahotSupportsSRV v
  | v < ShelleyNodeToClientVersion14 = LedgerPeerSnapshotDoesntSupportSRV
  | otherwise = LedgerPeerSnapshotSupportsSRV

instance HasNetworkProtocolVersion (ShelleyBlock proto era) where
  type BlockNodeToNodeVersion (ShelleyBlock proto era) = ShelleyNodeToNodeVersion
  type BlockNodeToClientVersion (ShelleyBlock proto era) = ShelleyNodeToClientVersion

-- TODO #2668 make this era-specific
instance SupportedNetworkProtocolVersion (ShelleyBlock proto era) where
  supportedNodeToNodeVersions _ =
    Map.fromList
      [ (NodeToNodeV_14, ShelleyNodeToNodeVersion1)
      , (NodeToNodeV_15, ShelleyNodeToNodeVersion1)
      ]
  supportedNodeToClientVersions _ =
    Map.fromList
      [ (NodeToClientV_16, ShelleyNodeToClientVersion8)
      , (NodeToClientV_17, ShelleyNodeToClientVersion9)
      , (NodeToClientV_18, ShelleyNodeToClientVersion10)
      , (NodeToClientV_19, ShelleyNodeToClientVersion11)
      , (NodeToClientV_20, ShelleyNodeToClientVersion12)
      , (NodeToClientV_21, ShelleyNodeToClientVersion13)
      , (NodeToClientV_22, ShelleyNodeToClientVersion14)
      ]

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault
