-- | Running LeiosFetch as a node-to-node mini-protocol.
module Ouroboros.Consensus.Network.LeiosFetch
  ( leiosFetchMiniProtocolNum
  , leiosFetchPipelineDepth
  , maxLeiosFetchIngressBytes
  ) where

import qualified Network.Mux.Types as Mux

leiosFetchMiniProtocolNum :: Mux.MiniProtocolNum
leiosFetchMiniProtocolNum = Mux.MiniProtocolNum 19

-- | The most LeiosFetch requests to leave outstanding on one connection: enough
-- to fetch the five closures 'maxLeiosFetchIngressBytes' allows for, with some
-- headroom. A 12 MB closure takes 24 requests of
-- 'Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type.maxLeiosTxsRequestBytesSize',
-- or a few more, since a request cannot split a transaction.
--
-- TODO Revisit once the client requests transactions.
leiosFetchPipelineDepth :: Int
leiosFetchPipelineDepth = 150

-- | The most reply bytes the node's client may have requested and not yet
-- consumed on one connection: five endorser blocks' closures of the largest
-- size, 12 MB each, CIP-0164's @S_EB-tx@. So a big-ledger peer can be asked
-- for several whole closures at once, while an adversarial peer still cannot
-- drown the node.
--
-- The client must never leave more requested bytes outstanding than this, else
-- the mux tears the connection down (@Network.Mux.Ingress@'s
-- @IngressQueueOverRun@).
--
-- TODO Make it the constitution's guardrail on @maxEndorserBlockTxsSize@, as for
-- 'Ouroboros.Consensus.Leios.Types.maxLeiosEbBytesSize'.
--
-- TODO The ledger's example Dijkstra genesis, which this repository's tools and
-- test genesis copy, sets @maxEndorserBlockTxsSize@ to 12 MiB rather than
-- 12 MB. Remove this note once it corrects that.
maxLeiosFetchIngressBytes :: Int
maxLeiosFetchIngressBytes = 5 * 12 * 1000 * 1000
