{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

-- | The node's peers, as run by the Leios node-vs-environment tests.
--
-- A peer connects to the node as the node's diffusion layer would have it: over
-- an in-memory bearer, the two run the handshake and then a mux each. The
-- node's end runs its whole negotiated bundle, starting every initiator as for
-- a hot peer and every responder as the bundle says. The peer's end answers
-- each of the node's initiators:
--
-- - KeepAlive and PeerSharing with the servers a node runs;
-- - ChainSync by finding the intersection at genesis and then never sending a
--   header;
-- - BlockFetch and TxSubmission by never saying anything, which is all the node
--   sees of a peer with nothing to offer;
-- - LeiosNotify and LeiosFetch with whatever the test gives it.
module Test.Consensus.Leios.Environment
  ( EnvPeer (..)
  , LeiosFetchCodec
  , LeiosNotifyCodec
  , NodeVsEnvConnection (..)
  , envLeiosFetchCodec
  , envLeiosNotifyCodec
  , refusingLeiosFetchServer
  , withNodeVsEnvConnection
  ) where

import Cardano.Network.NodeToNode
  ( chainSyncMiniProtocolNum
  , keepAliveMiniProtocolNum
  , nodeToNodeHandshakeCodec
  , nodeToNodeVersionDataCodec
  , peerSharingMiniProtocolNum
  )
import Cardano.Network.NodeToNode.Version (NodeToNodeVersion, NodeToNodeVersionData)
import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable (..))
import Codec.CBOR.Read (DeserialiseFailure)
import qualified Control.Concurrent.Class.MonadSTM.Strict as Strict
import Control.Monad.IOSim (IOSim)
import Control.Tracer (nullTracer)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Traversable (for)
import qualified Network.Mux as Mx
import Network.Mux.Bearer.Queues (QueueChannel (..), queueChannelAsBearer)
import Network.TypedProtocol.Codec (Codec)
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Block (Header, Point, pattern BlockPoint, pattern GenesisPoint)
import Ouroboros.Consensus.Config (configCodec)
import Ouroboros.Consensus.Leios.Types (LeiosEb, LeiosPoint, LeiosTx, LeiosVote)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Server
  ( LeiosFetchRequestHandler (..)
  , leiosFetchServerPeer
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type (LeiosFetch)
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Type (LeiosNotify)
import Ouroboros.Consensus.Network.LeiosFetch (leiosFetchMiniProtocolNum)
import Ouroboros.Consensus.Network.LeiosNotify (leiosNotifyMiniProtocolNum)
import qualified Ouroboros.Consensus.Network.NodeToNode as NTN
import Ouroboros.Consensus.Node.ExitPolicy (NodeToNodeInitiatorResult)
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.Block (Tip (..))
import Ouroboros.Network.Channel (Channel)
import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.ConnectionManager.Types (DataFlow (..), Provenance (..))
import Ouroboros.Network.Context
  ( ExpandedInitiatorContext (..)
  , ResponderContext (..)
  )
import Ouroboros.Network.ControlMessage (ControlMessage (..))
import Ouroboros.Network.Driver.Simple (runPeer)
import Ouroboros.Network.KeepAlive (keepAliveServer)
import Ouroboros.Network.Mux
  ( MiniProtocol (..)
  , MiniProtocolCb (..)
  , OuroborosApplication (..)
  , RunMiniProtocol (..)
  , fromOuroborosBundle
  , mkMiniProtocolInfos
  )
import Ouroboros.Network.PeerSelection.Governor.Types (makePublicPeerSelectionStateVar)
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (IsBigLedgerPeer (..))
import Ouroboros.Network.PeerSharing
  ( PeerSharingAPI
  , newPeerSharingAPI
  , peerSharingServer
  , ps_POLICY_PEER_SHARE_MAX_PEERS
  , ps_POLICY_PEER_SHARE_STICKY_TIME
  )
import Ouroboros.Network.Protocol.ChainSync.Server
  ( ChainSyncServer (..)
  , ServerStIdle (..)
  , ServerStIntersect (..)
  , chainSyncServerPeer
  )
import Ouroboros.Network.Protocol.Handshake
  ( HandshakeArguments (..)
  , HandshakeResult (..)
  , Versions
  , acceptableVersion
  , combineVersions
  , queryVersion
  , runHandshakeClient
  , runHandshakeServer
  , simpleSingletonVersions
  , timeLimitsHandshake
  )
import Ouroboros.Network.Protocol.KeepAlive.Server (keepAliveServerPeer)
import Ouroboros.Network.Protocol.PeerSharing.Server (peerSharingServerPeer)
import System.Random (StdGen)
import Test.Consensus.Leios.NodeUnderTest

type LeiosNotifyCodec s =
  Codec (LeiosNotify LeiosPoint (Header Blk) LeiosVote) DeserialiseFailure (IOSim s) ByteString

type LeiosFetchCodec s =
  Codec (LeiosFetch LeiosPoint LeiosEb LeiosTx) DeserialiseFailure (IOSim s) ByteString

-- | What a peer runs for the Leios mini-protocols.
data EnvPeer m = EnvPeer
  { epLeiosNotifyServer :: Channel m ByteString -> m ()
  -- ^ Answers the node's client.
  , epLeiosNotifyClient :: Maybe (Channel m ByteString -> m ())
  -- ^ Talks to the node's server, if the test has the peer do so.
  , epLeiosFetchServer :: Channel m ByteString -> m ()
  -- ^ Answers the node's LeiosFetch client.
  , epLeiosFetchClient :: Maybe (Channel m ByteString -> m ())
  -- ^ Talks to the node's LeiosFetch server, if the test has the peer do so.
  }

-- | A connection from the node to a peer, once the handshake has settled on a
-- version.
data NodeVsEnvConnection m = NodeVsEnvConnection
  { connTerminate :: m ()
  -- ^ Tell the node's initiators to wind down, as when the node demotes the
  -- peer.
  , connNodeInitiators ::
      Map Mx.MiniProtocolNum (STM m (Either SomeException NodeToNodeInitiatorResult))
  -- ^ The result of each of the node's initiators.
  , connNodeResponders :: Map Mx.MiniProtocolNum (STM m (Either SomeException ()))
  -- ^ The result of each of the node's responders.
  , connPeerLeiosNotifyClient :: Maybe (STM m (Either SomeException ()))
  -- ^ The result of the peer's LeiosNotify client, if it runs one.
  , connPeerLeiosFetchClient :: Maybe (STM m (Either SomeException ()))
  -- ^ The result of the peer's LeiosFetch client, if it runs one.
  }

-- | Connect the node to the peer at the given address, run the body, then shut
-- both ends down.
withNodeVsEnvConnection ::
  NodeUnderTest (IOSim s) ->
  PeerAddr ->
  EnvPeer (IOSim s) ->
  (NodeVsEnvConnection (IOSim s) -> IOSim s a) ->
  IOSim s a
withNodeVsEnvConnection nut addr envPeer body = do
  nodeToPeer <- Strict.newTBQueueIO bearerQueueCapacity
  peerToNode <- Strict.newTBQueueIO bearerQueueCapacity
  let nodeBearer = queueChannelAsBearer sduSize QueueChannel{readQueue = peerToNode, writeQueue = nodeToPeer}
      peerBearer = queueChannelAsBearer sduSize QueueChannel{readQueue = nodeToPeer, writeQueue = peerToNode}
      nodeConnId = ConnectionId{localAddress = nodeAddr, remoteAddress = addr}
      peerConnId = ConnectionId{localAddress = addr, remoteAddress = nodeAddr}
  (bundle, version) <-
    concurrently
      (runHandshakeClient nodeBearer nodeConnId handshakeArguments (nutVersions nut))
      (runHandshakeServer peerBearer peerConnId handshakeArguments peerVersions)
      >>= \case
        (Right (HandshakeNegotiationResult bundle version _), Right HandshakeNegotiationResult{}) ->
          pure (bundle, version)
        _ -> throwIO $ userError "the node and the peer did not agree on a version"
  controlMessage <- Strict.newTVarIO Continue
  peerSharingAPI <- do
    publicPeerSelectionStateVar <- makePublicPeerSelectionStateVar
    newPeerSharingAPI
      publicPeerSelectionStateVar
      (seedEnvPeerSharing (nutSeeds nut))
      ps_POLICY_PEER_SHARE_STICKY_TIME
      ps_POLICY_PEER_SHARE_MAX_PEERS
  let protocols = getOuroborosApplication (fromOuroborosBundle bundle)
      infos = mkMiniProtocolInfos (\_ _ -> Nothing) Outbound Duplex bundle
      codecs = envCodecs nut version
  nodeMux <- Mx.new Mx.nullTracers infos
  peerMux <- Mx.new Mx.nullTracers infos
  withAsync (Mx.run nodeMux nodeBearer) $ \_ ->
    withAsync (Mx.run peerMux peerBearer) $ \_ -> do
      for_ protocols $ \MiniProtocol{miniProtocolNum} ->
        Mx.runMiniProtocol
          peerMux
          miniProtocolNum
          Mx.ResponderDirection
          Mx.StartOnDemand
          (peerResponder codecs peerSharingAPI envPeer miniProtocolNum)
      connPeerLeiosNotifyClient <-
        for (epLeiosNotifyClient envPeer) $ \client ->
          Mx.runMiniProtocol
            peerMux
            leiosNotifyMiniProtocolNum
            Mx.InitiatorDirection
            Mx.StartEagerly
            (fmap (,Nothing) . client)
      connPeerLeiosFetchClient <-
        for (epLeiosFetchClient envPeer) $ \client ->
          Mx.runMiniProtocol
            peerMux
            leiosFetchMiniProtocolNum
            Mx.InitiatorDirection
            Mx.StartEagerly
            (fmap (,Nothing) . client)
      nodeProtocols <-
        for protocols $ \MiniProtocol{miniProtocolNum, miniProtocolStart, miniProtocolRun} ->
          case miniProtocolRun of
            InitiatorAndResponderProtocol initiator responder -> do
              initiatorResult <-
                Mx.runMiniProtocol
                  nodeMux
                  miniProtocolNum
                  Mx.InitiatorDirection
                  Mx.StartEagerly
                  ( runMiniProtocolCb
                      initiator
                      ExpandedInitiatorContext
                        { eicConnectionId = nodeConnId
                        , eicControlMessage = Strict.readTVar controlMessage
                        , eicIsBigLedgerPeer = IsBigLedgerPeer
                        , eicExtraFlags = IsNotTrustable
                        }
                  )
              responderResult <-
                Mx.runMiniProtocol
                  nodeMux
                  miniProtocolNum
                  Mx.ResponderDirection
                  miniProtocolStart
                  (runMiniProtocolCb responder ResponderContext{rcConnectionId = nodeConnId})
              pure ((miniProtocolNum, initiatorResult), (miniProtocolNum, responderResult))
      result <-
        body
          NodeVsEnvConnection
            { connTerminate = atomically $ Strict.writeTVar controlMessage Terminate
            , connNodeInitiators = Map.fromList (map fst nodeProtocols)
            , connNodeResponders = Map.fromList (map snd nodeProtocols)
            , connPeerLeiosNotifyClient
            , connPeerLeiosFetchClient
            }
      Mx.stop nodeMux
      Mx.stop peerMux
      pure result

-- | The node's own address.
nodeAddr :: PeerAddr
nodeAddr = PeerAddr 0

-- | How many SDUs each direction of the bearer holds. When it is full, the
-- sending mux waits, as it would on a full socket buffer, so any size works.
bearerQueueCapacity :: Natural
bearerQueueCapacity = 10

-- | The SDU size of the socket bearer a node uses.
sduSize :: Mx.SDUSize
sduSize = Mx.SDUSize 12288

-- | What a node uses for the handshake, without its tracers.
handshakeArguments ::
  HandshakeArguments (ConnectionId PeerAddr) NodeToNodeVersion NodeToNodeVersionData (IOSim s)
handshakeArguments =
  HandshakeArguments
    { haHandshakeTracer = nullTracer
    , haBearerTracer = nullTracer
    , haHandshakeCodec = nodeToNodeHandshakeCodec
    , haVersionDataCodec = nodeToNodeVersionDataCodec
    , haAcceptVersion = acceptableVersion
    , haQueryVersion = queryVersion
    , haTimeLimits = timeLimitsHandshake
    }

-- | The peer offers every version, with the version data the node itself
-- offers.
peerVersions :: Versions NodeToNodeVersion NodeToNodeVersionData ()
peerVersions =
  combineVersions
    [ simpleSingletonVersions version (nodeVersionData version) (const ())
    | version <- [minBound .. maxBound]
    ]

-- | The peer's answer to the node's initiator with the given number.
peerResponder ::
  NodeCodecs s ->
  PeerSharingAPI PeerAddr StdGen (IOSim s) ->
  EnvPeer (IOSim s) ->
  Mx.MiniProtocolNum ->
  Channel (IOSim s) ByteString ->
  IOSim s ((), Maybe ByteString)
peerResponder codecs peerSharingAPI envPeer num chan
  | num == chainSyncMiniProtocolNum =
      runPeer nullTracer (NTN.cChainSyncCodec codecs) chan (chainSyncServerPeer quietChainSyncServer)
  | num == keepAliveMiniProtocolNum =
      runPeer nullTracer (NTN.cKeepAliveCodec codecs) chan (keepAliveServerPeer keepAliveServer)
  | num == peerSharingMiniProtocolNum =
      runPeer
        nullTracer
        (NTN.cPeerSharingCodec codecs)
        chan
        (peerSharingServerPeer (peerSharingServer peerSharingAPI))
  | num == leiosNotifyMiniProtocolNum =
      (,Nothing) <$> epLeiosNotifyServer envPeer chan
  | num == leiosFetchMiniProtocolNum =
      (,Nothing) <$> epLeiosFetchServer envPeer chan
  | otherwise =
      atomically retry

-- | A ChainSync server whose chain is empty and never grows.
--
-- The node gives a peer that is not trustable a random 135 to 269 seconds to
-- send its next header, and then disconnects, so a test that keeps a
-- connection open longer than 135 seconds loses it. That is probably not what
-- such a test intends, so it should serve a chain that grows instead.
quietChainSyncServer :: ChainSyncServer (Header Blk) (Point Blk) (Tip Blk) (IOSim s) ()
quietChainSyncServer = ChainSyncServer $ pure idle
 where
  idle =
    ServerStIdle
      { recvMsgRequestNext = pure $ Right $ atomically retry
      , recvMsgFindIntersect = \points ->
          pure $
            if any isGenesis points
              then SendMsgIntersectFound GenesisPoint TipGenesis quietChainSyncServer
              else SendMsgIntersectNotFound TipGenesis quietChainSyncServer
      , recvMsgDoneClient = pure ()
      }
  isGenesis = \case
    GenesisPoint -> True
    BlockPoint{} -> False

-- | The codecs the peer speaks, which are the node's.
-- | The node's codecs, at every mini-protocol's bytes.
type NodeCodecs s =
  NTN.Codecs
    Blk
    PeerAddr
    DeserialiseFailure
    (IOSim s)
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString

envCodecs :: NodeUnderTest (IOSim s) -> NodeToNodeVersion -> NodeCodecs s
envCodecs nut =
  NTN.defaultCodecs
    (configCodec (nutTopLevelConfig nut))
    ()
    (const encodePeerAddr)
    (const decodePeerAddr)

envLeiosNotifyCodec :: NodeUnderTest (IOSim s) -> LeiosNotifyCodec s
envLeiosNotifyCodec nut = NTN.cLeiosNotifyCodec (envCodecs nut maxBound)

envLeiosFetchCodec :: NodeUnderTest (IOSim s) -> LeiosFetchCodec s
envLeiosFetchCodec nut = NTN.cLeiosFetchCodec (envCodecs nut maxBound)

-- | A LeiosFetch server that disconnects a client that sends any request.
refusingLeiosFetchServer :: NodeUnderTest (IOSim s) -> Channel (IOSim s) ByteString -> IOSim s ()
refusingLeiosFetchServer nut chan =
  fst <$> runPeer nullTracer (envLeiosFetchCodec nut) chan (leiosFetchServerPeer refuse)
 where
  refuse = MkLeiosFetchRequestHandler $ \_request ->
    throwIO $ userError "the environment's LeiosFetch server serves nothing"
