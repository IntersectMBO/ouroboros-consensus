module Ouroboros.Consensus.Ledger.Query.Version
  ( QueryVersion (..)
  , nodeToClientVersionToQueryVersion
  ) where

import Ouroboros.Network.NodeToClient.Version

-- | Version of the `Query blk` type.
--
-- Multiple top level queries are now supported. The encoding now has
-- constructor tags for the different top level queries for QueryVersion1 onwards.
data QueryVersion
  = -- Adds support for 'GetSystemStart'.
    QueryVersion1
  | -- Adds support for 'GetChainBlockNo' and 'GetChainPoint'.
    QueryVersion2
  | -- Adds support for 'DebugLedgerConfig'
    QueryVersion3
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Get the @QueryVersion@ supported by this @NodeToClientVersion@.
nodeToClientVersionToQueryVersion :: NodeToClientVersion -> QueryVersion
nodeToClientVersionToQueryVersion x = case x of
  NodeToClientV_16 -> QueryVersion2
  NodeToClientV_17 -> QueryVersion2
  NodeToClientV_18 -> QueryVersion2
  NodeToClientV_19 -> QueryVersion2
  NodeToClientV_20 -> QueryVersion3
  NodeToClientV_21 -> QueryVersion3
  NodeToClientV_22 -> QueryVersion3
