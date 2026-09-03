{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion
  ( ShelleyNodeToClientVersion (..)
  , ShelleyNodeToNodeVersion (..)
  , ledgerPeerSnapshotSupportsSRV
  ) where

import qualified Data.Map.Strict as Map
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Shelley.Ledger.Block
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (LedgerPeerSnapshotSRVSupport (..))

data ShelleyNodeToNodeVersion = ShelleyNodeToNodeVersion1
  deriving (Show, Eq, Ord, Enum, Bounded)

data ShelleyNodeToClientVersion
  = -- | Support retrieving all ledger peers by GetLedgerPeerSnapshot
    -- New queries introduced: QueryDRepDelegations
    ShelleyNodeToClientVersion15
  deriving (Show, Eq, Ord, Enum, Bounded)

ledgerPeerSnapshotSupportsSRV :: ShelleyNodeToClientVersion -> LedgerPeerSnapshotSRVSupport
ledgerPeerSnapshotSupportsSRV ShelleyNodeToClientVersion15 = LedgerPeerSnapshotSupportsSRV

instance HasNetworkProtocolVersion (ShelleyBlock proto era) where
  type BlockNodeToNodeVersion (ShelleyBlock proto era) = ShelleyNodeToNodeVersion
  type BlockNodeToClientVersion (ShelleyBlock proto era) = ShelleyNodeToClientVersion

-- TODO #2668 make this era-specific
instance SupportedNetworkProtocolVersion (ShelleyBlock proto era) where
  supportedNodeToNodeVersions _ =
    Map.fromList
      [ (NodeToNodeV_14, ShelleyNodeToNodeVersion1)
      , (NodeToNodeV_15, ShelleyNodeToNodeVersion1)
      , (NodeToNodeV_16, ShelleyNodeToNodeVersion1)
      ]
  supportedNodeToClientVersions _ =
    Map.fromList
      [ (NodeToClientV_23, ShelleyNodeToClientVersion15)
      ]

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault
