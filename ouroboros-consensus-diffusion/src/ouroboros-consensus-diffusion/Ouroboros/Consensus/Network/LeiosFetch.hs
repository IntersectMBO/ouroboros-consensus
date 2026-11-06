{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Running LeiosFetch as a node-to-node mini-protocol.
module Ouroboros.Consensus.Network.LeiosFetch
  ( leiosFetchMiniProtocolNum
  , leiosFetchPipelineDepth
  , maxLeiosFetchIngressBytes
  , AtomicIngressLimits (..)
  , atomicIngressLimits
  , LeiosFetchProtocol (..)
  , LeiosFetchProtocolError (..)
  , negotiatedLeiosFetchProtocol
  , requireLegacyLeiosFetch
  , runLookaheadPeerWithAnnotatedLimits
  ) where

import Cardano.Network.NodeToNode.Version
  ( LeiosFetchRequestLimit
  , NodeToNodeVersion (..)
  )
import Control.DeepSeq (NFData)
import Control.Monad.Class.MonadThrow (Exception, MonadThrow, throwIO)
import Control.Monad.Class.MonadTimer.SI (MonadTimer)
import Control.Tracer (Tracer)
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word16, Word32, Word64)
import Network.Mux.Timeout (withTimeoutSerial)
import qualified Network.Mux.Types as Mux
import Network.TypedProtocol.Codec (AnnotatedCodec)
import Network.TypedProtocol.Core (Protocol (..))
import Network.TypedProtocol.Driver (runLookaheadPeerWithDriver)
import Network.TypedProtocol.Peer (PeerLookahead)
import Ouroboros.Consensus.Leios.Types (maxEndorserBlockBytesSize)
import Ouroboros.Consensus.Util.IOLike (IOLike)
import Ouroboros.Network.Channel (Channel)
import Ouroboros.Network.Driver.Limits (TraceSendRecv, annotatedDriverWithLimits)
import Ouroboros.Network.Protocol.Limits (ProtocolSizeLimits, ProtocolTimeLimits, smallByteLimit)
import Ouroboros.Network.Util.ShowProxy (ShowProxy)

-- | Directional atomic input bounds, independent of the legacy 60 MB budget.
-- The common mux limit covers both directions; the progress reader needs only
-- the response direction. Neither value is a complete connection heap bound.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#10-admission-shutdown-and-compatibility@.
data AtomicIngressLimits = AtomicIngressLimits
  { atomicClientChunkBytes :: !Word32
  -- ^ Reserved replies plus bounded original/cancel terminals and one SDU.
  , atomicMuxIngressBytes :: !Int
  -- ^ Larger of response and request directions, including one SDU allowance.
  }
  deriving (Eq, Show)

-- | Size input from a local response-byte budget and the negotiated D. Request
-- messages retain their existing 65,535 B protocol guard; each fixed control
-- fits in 64 B even with widest CBOR integers/list lengths. At most D data and
-- D cancel requests, followed by Done, may be queued. A 65,536 B margin covers
-- one maximum mux SDU. Widen all arithmetic before checked narrowing.
atomicIngressLimits :: Word32 -> Word16 -> Either LeiosFetchProtocolError AtomicIngressLimits
atomicIngressLimits responseBytes count
  | count == 0 || count > maxBound `div` 2 = Left InvalidAtomicFetchLimits
  | fromIntegral responseBytes < (fromIntegral maxEndorserBlockBytesSize + 3 :: Word64) =
      Left InvalidAtomicFetchLimits
  | muxBytes > fromIntegral (maxBound :: Word32) || muxBytes > fromIntegral (maxBound :: Int) =
      Left InvalidAtomicFetchLimits
  | otherwise = Right (AtomicIngressLimits (fromIntegral replyBytes) (fromIntegral muxBytes))
 where
  d = fromIntegral count :: Word64
  response = fromIntegral responseBytes :: Word64
  replies = response + 2 * d * 64
  requests = d * fromIntegral smallByteLimit + d * 64 + 64
  replyBytes = replies + 65536
  muxBytes = max replies requests + 65536

-- | Immutable fetch capability selected once from negotiated connection data.
data LeiosFetchProtocol
  = -- | Version 16 and older use their unchanged legacy path where available.
    LegacyLeiosFetch
  | -- | The positive minimum agreed by both sides, shared by client and server.
    AtomicLeiosFetch !LeiosFetchRequestLimit
  deriving (Eq, Show)

-- | Invalid version/capability pairs never silently select another wire format.
data LeiosFetchProtocolError
  = InvalidLeiosFetchCapability
  | AtomicLeiosFetchNotActivated
  | InvalidAtomicFetchLimits
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

-- | Temporary activation guard for ordinary client/server assembly. Explicit
-- atomic server fixtures have their own runner; normal version-17 assembly
-- stays disabled until the new client and its integration are ready. It must
-- never silently run the legacy codec on an atomic connection.
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

-- | Variable-sender counterpart of the existing LeiosNotify limits runner.
-- Use the ordinary annotated driver for framing, original body bytes, trailing
-- input and fatal wire guards. Application admission bounds the deferred sends;
-- the interpreter owns and joins its one sender thread on every exit.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#5-cancellation-races-and-the-response-commit-point@.
runLookaheadPeerWithAnnotatedLimits ::
  forall ps st pr failure m a.
  ( IOLike m
  , MonadTimer m
  , NFData a
  , NFData failure
  , Show failure
  , ShowProxy ps
  , forall (st' :: ps) tok. tok ~ StateToken st' => Show tok
  ) =>
  Tracer m (TraceSendRecv ps) ->
  AnnotatedCodec ps failure m BL.ByteString ->
  ProtocolSizeLimits ps BL.ByteString ->
  ProtocolTimeLimits ps ->
  Channel m BL.ByteString ->
  PeerLookahead ps pr st m a ->
  m (a, Maybe BL.ByteString)
runLookaheadPeerWithAnnotatedLimits tracer codec sizes times channel peer =
  withTimeoutSerial $ \timeoutFn ->
    runLookaheadPeerWithDriver
      (annotatedDriverWithLimits tracer timeoutFn codec sizes times channel)
      peer
