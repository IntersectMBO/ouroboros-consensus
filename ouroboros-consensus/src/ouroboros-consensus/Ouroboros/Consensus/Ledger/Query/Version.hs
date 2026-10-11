module Ouroboros.Consensus.Ledger.Query.Version
  ( QueryVersion (..)
  , nodeToClientVersionToQueryVersion
  ) where

import Cardano.Network.NodeToClient.Version

-- | Version of the `Query blk` type.
--
-- Multiple top level queries are supported. The encoding has constructor tags
-- for the different top level queries.
data QueryVersion
  = -- Adds support for @DebugLedgerConfig@
    QueryVersion3
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Get the @QueryVersion@ supported by this @NodeToClientVersion@.
nodeToClientVersionToQueryVersion :: NodeToClientVersion -> QueryVersion
nodeToClientVersionToQueryVersion x = case x of
  NodeToClientV_23 -> QueryVersion3
