{-# LANGUAGE NamedFieldPuns #-}

-- | Concrete-byte atomic fetch assembly. The node brackets this runtime around
-- diffusion before advertising its cancellation-capable version. Generic and
-- identity-codec applications retain the separate legacy assembly.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#agreed-initial-sizing-and-validation@.
module Ouroboros.Consensus.Network.NodeToNode.AtomicFetch
  ( atomicFetchApps
  , withAtomicFetchApps
  ) where

import Cardano.Network.NodeToNode.Version (getLeiosFetchRequestLimit)
import Control.Monad (void, when)
import Control.Monad.Class.MonadTimer.SI (MonadTimer)
import Control.Tracer (Tracer, contramap)
import qualified Data.ByteString.Lazy as BL
import Ouroboros.Consensus.Ledger.SupportsMempool (LedgerSupportsMempool)
import Ouroboros.Consensus.Leios.Types
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Client as C
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Handoff as H
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.LocalReuse as Local
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Node
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Server (atomicServerPeer)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Server.State (defaultServerLimits)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Type
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Kernel (withLeiosFetchPeer)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type (maxLeiosTxsRequestBytesSize)
import Ouroboros.Consensus.Network.LeiosFetch
import Ouroboros.Consensus.Network.NodeToNode (Apps (..))
import Ouroboros.Consensus.Network.NodeToNode.LeiosFetch (withLeiosFetchAtomicServer)
import Ouroboros.Consensus.Node.ExitPolicy (NodeToNodeInitiatorResult (..))
import qualified Ouroboros.Consensus.Node.Tracers as Node
import Ouroboros.Consensus.NodeKernel (NodeKernel (..))
import Ouroboros.Consensus.Storage.LeiosDB.API (withReader)
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.BlockFetch (TraceLabelPeer (..))
import Ouroboros.Network.BlockFetch.ClientRegistry (lookupKeepAliveRtt)
import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.Context (ExpandedInitiatorContext (..), ResponderContext (..))
import Ouroboros.Network.ControlMessage (ControlMessage (..))
import Ouroboros.Network.Driver.Limits (TraceSendRecv)
import Ouroboros.Network.Mux (MiniProtocolLimits (..))

-- | One node-owned runtime shared by every concrete-byte fetch application.
-- Validate local transport and indivisible-body capacity before peers start.
-- The action owns/joins its connections; accepted storage then drains on exit.
withAtomicFetchApps ::
  (IOLike m, MonadTimer m, Ord addr, LedgerSupportsMempool blk) =>
  AtomicNodeConfig ->
  NodeKernel m addr addrNTC blk ->
  Tracer
    m
    ( TraceLabelPeer
        (ConnectionId addr)
        (TraceSendRecv (LeiosFetchAtomic LeiosPoint EndorserBlock LeiosTx))
    ) ->
  (version -> Apps m addr bCS bBF bTX bPCD bPVD bKA bPS bLN BL.ByteString NodeToNodeInitiatorResult ()) ->
  ( AtomicFetchNode m addr (ConnectionId addr) blk ->
    (version -> Apps m addr bCS bBF bTX bPCD bPVD bKA bPS bLN BL.ByteString NodeToNodeInitiatorResult ()) ->
    m a
  ) ->
  m a
withAtomicFetchApps config kernel tracer legacy action = do
  -- Validate for every legal negotiated count, not only the current local 64.
  void $ either throwIO pure (atomicIngressLimits (nodeResponseBytes config) 32767)
  when (H.storageBytesPerOwner (nodeStorageLimits config) < maxEndorserBlockBytesSize) $
    throwIO InvalidAtomicFetchLimits
  when (H.localStorageBytes (nodeLocalStorageLimits config) < maxLeiosTxsRequestBytesSize) $
    throwIO InvalidAtomicFetchLimits
  withAtomicFetchNodeWithLocal
    config
    ((\cache -> (cache, Local.mempoolLocalSource (getMempool kernel))) <$> getLeiosTxCache kernel)
    (getLeiosFetchKernel kernel)
    $ \node ->
      action node (atomicFetchApps node kernel tracer . legacy)

-- | Replace the fetch pair and its input limits together. Legacy selection
-- delegates to its original app with the shared durable-receipt bridge. Both
-- peers' frozen negotiated count and this runtime's byte budget size transport.
atomicFetchApps ::
  (IOLike m, MonadTimer m, Ord addr) =>
  AtomicFetchNode m addr (ConnectionId addr) blk ->
  NodeKernel m addr addrNTC blk ->
  Tracer
    m
    ( TraceLabelPeer
        (ConnectionId addr)
        (TraceSendRecv (LeiosFetchAtomic LeiosPoint EndorserBlock LeiosTx))
    ) ->
  Apps m addr bCS bBF bTX bPCD bPVD bKA bPS bLN BL.ByteString NodeToNodeInitiatorResult () ->
  Apps m addr bCS bBF bTX bPCD bPVD bKA bPS bLN BL.ByteString NodeToNodeInitiatorResult ()
atomicFetchApps node kernel tracer legacy =
  legacy
    { aLeiosFetchProtocolLimits = \count version ->
        case negotiatedLeiosFetchProtocol version count of
          Right (AtomicLeiosFetch d) -> case atomicIngressLimits (atomicResponseBudget node) (getLeiosFetchRequestLimit d) of
            Right limits -> MiniProtocolLimits (atomicMuxIngressBytes limits)
            Left _ -> MiniProtocolLimits 0
          Right LegacyLeiosFetch -> aLeiosFetchProtocolLimits legacy count version
          -- Invalid negotiation gets no ingress; the app reports its error.
          Left _ -> MiniProtocolLimits 0
    , aLeiosFetchClient = \count version ctx@ExpandedInitiatorContext{eicConnectionId = conn, eicControlMessage, eicIsBigLedgerPeer} channel -> do
        selected <- either throwIO pure (negotiatedLeiosFetchProtocol version count)
        withLeiosFetchPeer (getLeiosFetchKernel kernel) conn eicIsBigLedgerPeer $ \lifetime _ ->
          case selected of
            LegacyLeiosFetch -> withNodeLegacyClient node lifetime (aLeiosFetchClient legacy count version ctx channel)
            AtomicLeiosFetch limit -> do
              bounds <-
                either
                  throwIO
                  pure
                  (atomicIngressLimits (atomicResponseBudget node) (getLeiosFetchRequestLimit limit))
              -- Capture once. Missing/retired registrations and missing samples
              -- retain explicit fallback; never borrow a replacement's sample.
              rtt <- atomically $ lookupKeepAliveRtt (getKeepAliveRegistry kernel) conn
              withNodeAtomicClient node (remoteAddress conn) lifetime (getLeiosFetchRequestLimit limit) rtt $ \client ->
                withAsync
                  ( do
                      atomically $ eicControlMessage >>= check . (== Terminate)
                      atomically $ C.stopAtomicClient client
                  )
                  $ \_ -> do
                    trailing <-
                      C.runAtomicClient
                        (contramap (TraceLabelPeer conn) tracer)
                        20000
                        (atomicClientChunkBytes bounds)
                        channel
                        client
                    pure (NoInitiatorResult, trailing)
    , aLeiosFetchServer = \count version ctx@ResponderContext{rcConnectionId = conn} channel -> do
        selected <- either throwIO pure (negotiatedLeiosFetchProtocol version count)
        case selected of
          LegacyLeiosFetch -> aLeiosFetchServer legacy count version ctx channel
          AtomicLeiosFetch limit ->
            withReader (getLeiosDB kernel) $ \reader ->
              withLeiosFetchAtomicServer
                (contramap (TraceLabelPeer conn) (Node.leiosFetchServerTracer (getTracers kernel)))
                (defaultServerLimits (getLeiosFetchRequestLimit limit))
                reader
                $ \server ->
                  runLookaheadPeerWithAnnotatedLimits
                    (contramap (TraceLabelPeer conn) tracer)
                    codec
                    (byteLimitsLeiosFetchAtomic (fromIntegral . BL.length))
                    timeLimitsLeiosFetchAtomic
                    channel
                    (atomicServerPeer server)
    }
 where
  codec =
    codecLeiosFetchAtomic
      20000
      encodeLeiosPoint
      decodeLeiosPoint
      encodeEndorserBlock
      decodeEndorserBlock
      encodeLeiosTx
      decodeLeiosTx
