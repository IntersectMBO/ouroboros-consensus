{-# LANGUAGE NamedFieldPuns #-}

-- | Opt-in concrete-byte node assembly for the experimental atomic protocol.
-- Keeping this separate preserves the generic/identity-codec legacy assembly
-- and normal supported-version maps until activation is explicitly approved.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#10-admission-shutdown-and-compatibility@.
module Ouroboros.Consensus.Network.NodeToNode.AtomicFetch (atomicFetchApps) where

import Cardano.Network.NodeToNode.Version (getLeiosFetchRequestLimit)
import Control.Monad.Class.MonadTimer.SI (MonadTimer)
import Control.Tracer (Tracer, nullTracer)
import qualified Data.ByteString.Lazy as BL
import Ouroboros.Consensus.Leios.Types
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Client as C
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Node
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Server (atomicServerPeer)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Server.State (defaultServerLimits)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Type
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Kernel (withLeiosFetchPeer)
import Ouroboros.Consensus.Network.LeiosFetch
import Ouroboros.Consensus.Network.NodeToNode (Apps (..))
import Ouroboros.Consensus.Network.NodeToNode.LeiosFetch (withLeiosFetchAtomicServer)
import Ouroboros.Consensus.Node.ExitPolicy (NodeToNodeInitiatorResult (..))
import Ouroboros.Consensus.NodeKernel (NodeKernel (..))
import Ouroboros.Consensus.Storage.LeiosDB.API (withReader)
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.BlockFetch.ClientRegistry (lookupKeepAliveRtt)
import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.Context (ExpandedInitiatorContext (..), ResponderContext (..))
import Ouroboros.Network.ControlMessage (ControlMessage (..))
import Ouroboros.Network.Driver.Limits (TraceSendRecv)

-- | Replace only the LeiosFetch pair of explicitly selected development apps.
-- The negotiated capability is frozen independently on each connection. A
-- legacy selection delegates to its original app with the shared receipt bridge.
-- The caller brackets the entire connection lifetime inside 'withAtomicFetchNode'.
atomicFetchApps ::
  (IOLike m, MonadTimer m, Ord addr) =>
  AtomicFetchNode m addr (ConnectionId addr) blk ->
  NodeKernel m addr addrNTC blk ->
  Tracer m (TraceSendRecv (LeiosFetchAtomic LeiosPoint EndorserBlock LeiosTx)) ->
  Apps m addr bCS bBF bTX bPCD bPVD bKA bPS bLN BL.ByteString NodeToNodeInitiatorResult () ->
  Apps m addr bCS bBF bTX bPCD bPVD bKA bPS bLN BL.ByteString NodeToNodeInitiatorResult ()
atomicFetchApps node kernel tracer legacy =
  legacy
    { aLeiosFetchClient = \count version ctx@ExpandedInitiatorContext{eicConnectionId = conn, eicControlMessage, eicIsBigLedgerPeer} channel -> do
        selected <- either throwIO pure (negotiatedLeiosFetchProtocol version count)
        withLeiosFetchPeer (getLeiosFetchKernel kernel) conn eicIsBigLedgerPeer $ \lifetime _ ->
          case selected of
            LegacyLeiosFetch -> withNodeLegacyClient node lifetime (aLeiosFetchClient legacy count version ctx channel)
            AtomicLeiosFetch limit -> do
              -- Capture once. Missing/retired registration and missing samples
              -- use the timing module's explicit fallback, never GSV smoothing.
              rtt <- atomically $ lookupKeepAliveRtt (getKeepAliveRegistry kernel) conn
              withNodeAtomicClient node (remoteAddress conn) lifetime (getLeiosFetchRequestLimit limit) rtt $ \client ->
                withAsync
                  ( do
                      atomically $ eicControlMessage >>= check . (== Terminate)
                      atomically $ C.stopAtomicClient client
                  )
                  $ \_ -> do
                    trailing <- C.runAtomicClient tracer 20000 (fromIntegral maxLeiosFetchIngressBytes) channel client
                    pure (NoInitiatorResult, trailing)
    , aLeiosFetchServer = \count version ctx@ResponderContext{rcConnectionId = _conn} channel -> do
        selected <- either throwIO pure (negotiatedLeiosFetchProtocol version count)
        case selected of
          LegacyLeiosFetch -> aLeiosFetchServer legacy count version ctx channel
          AtomicLeiosFetch limit ->
            withReader (getLeiosDB kernel) $ \reader ->
              withLeiosFetchAtomicServer nullTracer (defaultServerLimits (getLeiosFetchRequestLimit limit)) reader $ \server ->
                runLookaheadPeerWithAnnotatedLimits
                  tracer
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
