{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Byron.Ledger.NetworkProtocolVersion
  ( ByronNodeToClientVersion (..)
  , ByronNodeToNodeVersion (..)
  ) where

import qualified Data.Map.Strict as Map
import Ouroboros.Consensus.Byron.Ledger.Block
import Ouroboros.Consensus.Node.NetworkProtocolVersion

data ByronNodeToNodeVersion
  = -- | We send headers without a size hint
    ByronNodeToNodeVersion1
  | -- | We send headers /with/ a size hint
    ByronNodeToNodeVersion2
  deriving (Show, Eq, Ord, Enum, Bounded)

data ByronNodeToClientVersion
  = ByronNodeToClientVersion1
  deriving (Show, Eq, Ord, Enum, Bounded)

instance HasNetworkProtocolVersion ByronBlock where
  type BlockNodeToNodeVersion ByronBlock = ByronNodeToNodeVersion
  type BlockNodeToClientVersion ByronBlock = ByronNodeToClientVersion

-- | This instance isn't used apart from tests; we therefore make our life easy
-- below.
instance SupportedNetworkProtocolVersion ByronBlock where
  supportedNodeToNodeVersions _ =
    Map.fromList
      [ (v, ByronNodeToNodeVersion2)
      | v <- [minBound .. maxBound]
      ]

  supportedNodeToClientVersions _ =
    Map.fromList
      [ (v, ByronNodeToClientVersion1)
      | v <- [minBound .. maxBound]
      ]

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault
