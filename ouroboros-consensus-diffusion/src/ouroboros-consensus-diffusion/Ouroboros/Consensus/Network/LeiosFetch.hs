-- | Running LeiosFetch as a node-to-node mini-protocol.
module Ouroboros.Consensus.Network.LeiosFetch
  ( leiosFetchMiniProtocolNum
  , leiosFetchPipelineDepth
  , maxLeiosFetchIngressBytes
  , LeiosFetchProtocol (..)
  , LeiosFetchProtocolError (..)
  , negotiatedLeiosFetchProtocol
  , requireLegacyLeiosFetch
  ) where

import Cardano.Network.NodeToNode.Version
  ( LeiosFetchRequestLimit
  , NodeToNodeVersion (..)
  )
import Control.Monad.Class.MonadThrow (Exception, MonadThrow, throwIO)
import qualified Network.Mux.Types as Mux

-- | Immutable fetch capability selected once from negotiated connection data.
data LeiosFetchProtocol
  = -- | Version 16 and older use their unchanged legacy path where available.
    LegacyLeiosFetch
  | -- | The positive minimum agreed by both sides, shared by client and server.
    AtomicLeiosFetch !LeiosFetchRequestLimit
  deriving (Eq, Show)

-- | Invalid version/capability pairs never silently select another wire format.
data LeiosFetchProtocolError = InvalidLeiosFetchCapability | AtomicLeiosFetchNotActivated
  deriving (Eq, Show)

instance Exception LeiosFetchProtocolError

-- | Freeze the negotiated count for this session. The experimental selection
-- is available to fixtures before normal capability advertisement is enabled.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#10-admission-shutdown-and-compatibility@.
negotiatedLeiosFetchProtocol ::
  NodeToNodeVersion ->
  Maybe LeiosFetchRequestLimit ->
  Either LeiosFetchProtocolError LeiosFetchProtocol
negotiatedLeiosFetchProtocol version count = case (version, count) of
  (NodeToNodeV_17, Just d) -> Right (AtomicLeiosFetch d)
  (NodeToNodeV_17, Nothing) -> Left InvalidLeiosFetchCapability
  (_, Nothing) -> Right LegacyLeiosFetch
  (_, Just _) -> Left InvalidLeiosFetchCapability

-- | Temporary activation guard for the existing client/server. Their atomic
-- implementations are later execution stages; explicit version 17 fixtures
-- must not accidentally run the legacy codec on an atomic connection.
requireLegacyLeiosFetch :: MonadThrow m => NodeToNodeVersion -> Maybe LeiosFetchRequestLimit -> m ()
requireLegacyLeiosFetch version count = case negotiatedLeiosFetchProtocol version count of
  Left err -> throwIO err
  Right LegacyLeiosFetch -> pure ()
  Right AtomicLeiosFetch{} -> throwIO AtomicLeiosFetchNotActivated

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
-- 'Ouroboros.Consensus.Leios.Types.maxEndorserBlockBytesSize'.
--
-- TODO The ledger's example Dijkstra genesis, which this repository's tools and
-- test genesis copy, sets @maxEndorserBlockTxsSize@ to 12 MiB rather than
-- 12 MB. Remove this note once it corrects that.
maxLeiosFetchIngressBytes :: Int
maxLeiosFetchIngressBytes = 5 * 12 * 1000 * 1000
