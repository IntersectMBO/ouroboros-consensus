{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

-- | Intended for qualified import
module Ouroboros.Consensus.Network.NodeToNode (
    -- * Handlers
    Handlers (..)
  , mkHandlers
    -- * Codecs
  , Codecs (..)
  , defaultCodecs
  , identityCodecs
    -- * Byte Limits
  , ByteLimits
  , byteLimits
  , noByteLimits
    -- * Tracers
  , Tracers
  , Tracers' (..)
  , nullTracers
  , showTracers
    -- * Applications
  , Apps (..)
  , ClientApp
  , ServerApp
  , mkApps
    -- ** Projections
  , initiator
  , initiatorAndResponder
    -- * Re-exports
  , ChainSyncTimeout (..)
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.CBOR.Read (DeserialiseFailure)
import           Control.Concurrent.Class.MonadSTM.Strict.TVar as TVar.Unchecked
import           Control.Monad.Class.MonadTime.SI (MonadTime)
import           Control.Monad.Class.MonadTimer.SI (MonadTimer)
import           Control.Tracer
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Hashable (Hashable)
import           Data.Int (Int64)
import           Data.Map.Strict (Map)
import           Data.Void (Void)
import           Network.TypedProtocol.Codec
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client as CsClient
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
import           Ouroboros.Consensus.Node.ExitPolicy
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Serialisation
import qualified Ouroboros.Consensus.Node.Tracers as Node
import           Ouroboros.Consensus.NodeKernel
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Storage.Serialisation (SerialisedHeader)
import           Ouroboros.Consensus.Util (ShowProxy)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Network.Block (Serialised (..), decodePoint,
                     decodeTip, encodePoint, encodeTip)
import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.BlockFetch.Client (BlockFetchClient,
                     blockFetchClient)
import           Ouroboros.Network.Channel
import           Ouroboros.Network.Context
import           Ouroboros.Network.DeltaQ
import           Ouroboros.Network.Driver
import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.KeepAlive
import           Ouroboros.Network.Mux
import           Ouroboros.Network.NodeToNode
import           Ouroboros.Network.NodeToNode.Version (isPipeliningEnabled)
import           Ouroboros.Network.PeerSelection.PeerMetric.Type
                     (FetchedMetricsTracer, ReportPeerMetrics (..))
import qualified Ouroboros.Network.PeerSelection.PeerSharing as PSTypes
import           Ouroboros.Network.PeerSharing (PeerSharingController,
                     bracketPeerSharingClient, peerSharingClient,
                     peerSharingServer)
import           Ouroboros.Network.Protocol.BlockFetch.Codec
import           Ouroboros.Network.Protocol.BlockFetch.Server (BlockFetchServer,
                     blockFetchServerPeer)
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch (..))
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.KeepAlive.Client
import           Ouroboros.Network.Protocol.KeepAlive.Codec
import           Ouroboros.Network.Protocol.KeepAlive.Server
import           Ouroboros.Network.Protocol.KeepAlive.Type
import           Ouroboros.Network.Protocol.PeerSharing.Client
                     (PeerSharingClient, peerSharingClientPeer)
import           Ouroboros.Network.Protocol.PeerSharing.Codec
                     (byteLimitsPeerSharing, codecPeerSharing,
                     codecPeerSharingId, timeLimitsPeerSharing)
import           Ouroboros.Network.Protocol.PeerSharing.Server
                     (PeerSharingServer, peerSharingServerPeer)
import           Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharing)
import           Ouroboros.Network.Protocol.TxSubmission2.Client
import           Ouroboros.Network.Protocol.TxSubmission2.Codec
import           Ouroboros.Network.Protocol.TxSubmission2.Server
import           Ouroboros.Network.Protocol.TxSubmission2.Type
import           Ouroboros.Network.TxSubmission.Inbound
import           Ouroboros.Network.TxSubmission.Mempool.Reader
                     (mapTxSubmissionMempoolReader)
import           Ouroboros.Network.TxSubmission.Outbound

{-------------------------------------------------------------------------------
  Handlers
-------------------------------------------------------------------------------}

-- | Protocol handlers for node-to-node (remote) communication
data Handlers m addr blk = Handlers {
      hChainSyncClient
        :: ConnectionId addr
        -> IsBigLedgerPeer
        -> CsClient.DynamicEnv m blk
        -> ChainSyncClientPipelined (Header blk) (Point blk) (Tip blk) m
             CsClient.ChainSyncClientResult
        -- TODO: we should reconsider bundling these context parameters into a
        -- record, perhaps instead extending the protocol handler
        -- representation to support bracket-style initialisation so that we
        -- could have the closure include these and not need to be explicit
        -- about them here.

    , hChainSyncServer
        :: ConnectionId addr
        -> NodeToNodeVersion
        -> ChainDB.Follower m blk (ChainDB.WithPoint blk (SerialisedHeader blk))
        -> ChainSyncServer (SerialisedHeader blk) (Point blk) (Tip blk) m ()

    -- TODO block fetch client does not have GADT view of the handlers.
    , hBlockFetchClient
        :: NodeToNodeVersion
        -> ControlMessageSTM m
        -> FetchedMetricsTracer m
        -> BlockFetchClient (Header blk) blk m ()

    , hBlockFetchServer
        :: ConnectionId addr
        -> NodeToNodeVersion
        -> ResourceRegistry m
        -> BlockFetchServer (Serialised blk) (Point blk) m ()

    , hTxSubmissionClient
        :: NodeToNodeVersion
        -> ControlMessageSTM m
        -> ConnectionId addr
        -> TxSubmissionClient (GenTxId blk) (GenTx blk) m ()

    , hTxSubmissionServer
        :: NodeToNodeVersion
        -> ConnectionId addr
        -> TxSubmissionServerPipelined (GenTxId blk) (GenTx blk) m ()

    , hKeepAliveClient
        :: NodeToNodeVersion
        -> ControlMessageSTM m
        -> ConnectionId addr
        -> TVar.Unchecked.StrictTVar m (Map (ConnectionId addr) PeerGSV)
        -> KeepAliveInterval
        -> KeepAliveClient m ()

    , hKeepAliveServer
        :: NodeToNodeVersion
        -> ConnectionId addr
        -> KeepAliveServer m ()

    , hPeerSharingClient
        :: NodeToNodeVersion
        -> ControlMessageSTM m
        -> ConnectionId addr
        -> PeerSharingController addr m
        -> m (PeerSharingClient addr m ())

    , hPeerSharingServer
        :: NodeToNodeVersion
        -> ConnectionId addr
        -> PeerSharingServer addr m
    }

mkHandlers ::
     forall m blk addrNTN addrNTC.
     ( IOLike m
     , MonadTime m
     , MonadTimer m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , LedgerSupportsProtocol blk
     , Ord addrNTN
     , Hashable addrNTN
     )
  => NodeKernelArgs m addrNTN addrNTC blk
  -> NodeKernel     m addrNTN addrNTC blk
  -> Handlers       m addrNTN           blk
mkHandlers
      NodeKernelArgs {chainSyncFutureCheck, keepAliveRng, miniProtocolParameters}
      NodeKernel {getChainDB, getMempool, getTopLevelConfig, getTracers = tracers, getPeerSharingAPI} =
    Handlers {
        hChainSyncClient = \peer _isBigLedgerpeer dynEnv ->
          CsClient.chainSyncClient
            CsClient.ConfigEnv {
                CsClient.cfg                     = getTopLevelConfig
              , CsClient.someHeaderInFutureCheck = chainSyncFutureCheck
              , CsClient.chainDbView             =
                  CsClient.defaultChainDbView getChainDB
              , CsClient.mkPipelineDecision0     = pipelineDecisionLowHighMark
                  (chainSyncPipeliningLowMark  miniProtocolParameters)
                  (chainSyncPipeliningHighMark miniProtocolParameters)
              , CsClient.tracer                  =
                  contramap (TraceLabelPeer peer) (Node.chainSyncClientTracer tracers)
              }
            dynEnv
      , hChainSyncServer = \peer _version ->
          chainSyncHeadersServer
            (contramap (TraceLabelPeer peer) (Node.chainSyncServerHeaderTracer tracers))
            getChainDB
      , hBlockFetchClient =
          blockFetchClient
      , hBlockFetchServer = \peer version ->
          blockFetchServer
            (contramap (TraceLabelPeer peer) (Node.blockFetchServerTracer tracers))
            getChainDB
            version
      , hTxSubmissionClient = \version controlMessageSTM peer ->
          txSubmissionOutbound
            (contramap (TraceLabelPeer peer) (Node.txOutboundTracer tracers))
            (txSubmissionMaxUnacked miniProtocolParameters)
            (mapTxSubmissionMempoolReader txForgetValidated $ getMempoolReader getMempool)
            version
            controlMessageSTM
      , hTxSubmissionServer = \version peer ->
          txSubmissionInbound
            (contramap (TraceLabelPeer peer) (Node.txInboundTracer tracers))
            (txSubmissionMaxUnacked miniProtocolParameters)
            (mapTxSubmissionMempoolReader txForgetValidated $ getMempoolReader getMempool)
            (getMempoolWriter getMempool)
            version
      , hKeepAliveClient = \_version -> keepAliveClient (Node.keepAliveClientTracer tracers) keepAliveRng
      , hKeepAliveServer = \_version _peer -> keepAliveServer
      , hPeerSharingClient = \_version controlMessageSTM _peer -> peerSharingClient controlMessageSTM
      , hPeerSharingServer = \_version _peer -> peerSharingServer getPeerSharingAPI
      }

{-------------------------------------------------------------------------------
  Codecs
-------------------------------------------------------------------------------}

-- | Node-to-node protocol codecs needed to run 'Handlers'.
data Codecs blk addr e m bCS bSCS bBF bSBF bTX bKA bPS = Codecs {
      cChainSyncCodec            :: Codec (ChainSync (Header blk) (Point blk) (Tip blk))           e m bCS
    , cChainSyncCodecSerialised  :: Codec (ChainSync (SerialisedHeader blk) (Point blk) (Tip blk)) e m bSCS
    , cBlockFetchCodec           :: Codec (BlockFetch blk (Point blk))                             e m bBF
    , cBlockFetchCodecSerialised :: Codec (BlockFetch (Serialised blk) (Point blk))                e m bSBF
    , cTxSubmission2Codec        :: Codec (TxSubmission2 (GenTxId blk) (GenTx blk))                e m bTX
    , cKeepAliveCodec            :: Codec KeepAlive                                                e m bKA
    , cPeerSharingCodec          :: Codec (PeerSharing addr)                                       e m bPS
    }

-- | Protocol codecs for the node-to-node protocols
defaultCodecs :: forall m blk addr.
                ( IOLike m
                , SerialiseNodeToNodeConstraints blk
                )
              => CodecConfig       blk
              -> BlockNodeToNodeVersion blk
              -> (NodeToNodeVersion -> addr -> CBOR.Encoding)
              -> (NodeToNodeVersion -> forall s . CBOR.Decoder s addr)
              -> NodeToNodeVersion
              -> Codecs blk addr DeserialiseFailure m
                   ByteString ByteString ByteString ByteString ByteString ByteString ByteString
defaultCodecs ccfg version encAddr decAddr nodeToNodeVersion = Codecs {
      cChainSyncCodec =
        codecChainSync
          enc
          dec
          (encodePoint (encodeRawHash p))
          (decodePoint (decodeRawHash p))
          (encodeTip   (encodeRawHash p))
          (decodeTip   (decodeRawHash p))

    , cChainSyncCodecSerialised =
        codecChainSync
          enc
          dec
          (encodePoint (encodeRawHash p))
          (decodePoint (decodeRawHash p))
          (encodeTip   (encodeRawHash p))
          (decodeTip   (decodeRawHash p))

    , cBlockFetchCodec =
        codecBlockFetch
          enc
          dec
          (encodePoint (encodeRawHash p))
          (decodePoint (decodeRawHash p))

    , cBlockFetchCodecSerialised =
        codecBlockFetch
          enc
          dec
          (encodePoint (encodeRawHash p))
          (decodePoint (decodeRawHash p))

    , cTxSubmission2Codec =
        codecTxSubmission2
          enc
          dec
          enc
          dec

    , cKeepAliveCodec = codecKeepAlive_v2

    , cPeerSharingCodec = codecPeerSharing (encAddr nodeToNodeVersion) (decAddr nodeToNodeVersion)
    }
  where
    p :: Proxy blk
    p = Proxy

    enc :: SerialiseNodeToNode blk a => a -> Encoding
    enc = encodeNodeToNode ccfg version

    dec :: SerialiseNodeToNode blk a => forall s. Decoder s a
    dec = decodeNodeToNode ccfg version

-- | Identity codecs used in tests.
identityCodecs :: Monad m
               => Codecs blk addr CodecFailure m
                    (AnyMessage (ChainSync (Header blk) (Point blk) (Tip blk)))
                    (AnyMessage (ChainSync (SerialisedHeader blk) (Point blk) (Tip blk)))
                    (AnyMessage (BlockFetch blk (Point blk)))
                    (AnyMessage (BlockFetch (Serialised blk) (Point blk)))
                    (AnyMessage (TxSubmission2 (GenTxId blk) (GenTx blk)))
                    (AnyMessage KeepAlive)
                    (AnyMessage (PeerSharing addr))
identityCodecs = Codecs {
      cChainSyncCodec            = codecChainSyncId
    , cChainSyncCodecSerialised  = codecChainSyncId
    , cBlockFetchCodec           = codecBlockFetchId
    , cBlockFetchCodecSerialised = codecBlockFetchId
    , cTxSubmission2Codec        = codecTxSubmission2Id
    , cKeepAliveCodec            = codecKeepAliveId
    , cPeerSharingCodec          = codecPeerSharingId
    }

{-------------------------------------------------------------------------------
  Tracers
-------------------------------------------------------------------------------}

-- | A record of 'Tracer's for the different protocols.
type Tracers m peer blk e =
     Tracers'  peer blk e (Tracer m)

data Tracers' peer blk e f = Tracers {
      tChainSyncTracer            :: f (TraceLabelPeer peer (TraceSendRecv (ChainSync (Header blk) (Point blk) (Tip blk))))
    , tChainSyncSerialisedTracer  :: f (TraceLabelPeer peer (TraceSendRecv (ChainSync (SerialisedHeader blk) (Point blk) (Tip blk))))
    , tBlockFetchTracer           :: f (TraceLabelPeer peer (TraceSendRecv (BlockFetch blk (Point blk))))
    , tBlockFetchSerialisedTracer :: f (TraceLabelPeer peer (TraceSendRecv (BlockFetch (Serialised blk) (Point blk))))
    , tTxSubmission2Tracer        :: f (TraceLabelPeer peer (TraceSendRecv (TxSubmission2 (GenTxId blk) (GenTx blk))))
    }

instance (forall a. Semigroup (f a)) => Semigroup (Tracers' peer blk e f) where
  l <> r = Tracers {
        tChainSyncTracer            = f tChainSyncTracer
      , tChainSyncSerialisedTracer  = f tChainSyncSerialisedTracer
      , tBlockFetchTracer           = f tBlockFetchTracer
      , tBlockFetchSerialisedTracer = f tBlockFetchSerialisedTracer
      , tTxSubmission2Tracer        = f tTxSubmission2Tracer
      }
    where
      f :: forall a. Semigroup a
        => (Tracers' peer blk e f -> a)
        -> a
      f prj = prj l <> prj r

-- | Use a 'nullTracer' for each protocol.
nullTracers :: Monad m => Tracers m peer blk e
nullTracers = Tracers {
      tChainSyncTracer            = nullTracer
    , tChainSyncSerialisedTracer  = nullTracer
    , tBlockFetchTracer           = nullTracer
    , tBlockFetchSerialisedTracer = nullTracer
    , tTxSubmission2Tracer        = nullTracer
    }

showTracers :: ( Show blk
               , Show peer
               , Show (Header blk)
               , Show (GenTx blk)
               , Show (GenTxId blk)
               , HasHeader blk
               , HasNestedContent Header blk
               )
            => Tracer m String -> Tracers m peer blk e
showTracers tr = Tracers {
      tChainSyncTracer            = showTracing tr
    , tChainSyncSerialisedTracer  = showTracing tr
    , tBlockFetchTracer           = showTracing tr
    , tBlockFetchSerialisedTracer = showTracing tr
    , tTxSubmission2Tracer        = showTracing tr
    }

{-------------------------------------------------------------------------------
  Applications
-------------------------------------------------------------------------------}

-- | A node-to-node application
type ClientApp m addr bytes a =
     NodeToNodeVersion
  -> ExpandedInitiatorContext addr m
  -> Channel m bytes
  -> m (a, Maybe bytes)

type ServerApp m addr bytes a =
     NodeToNodeVersion
  -> ResponderContext addr
  -> Channel m bytes
  -> m (a, Maybe bytes)

-- | Applications for the node-to-node protocols
--
-- See 'Network.Mux.Types.MuxApplication'
data Apps m addr bCS bBF bTX bKA bPS a b = Apps {
      -- | Start a chain sync client that communicates with the given upstream
      -- node.
      aChainSyncClient     :: ClientApp m addr bCS a

      -- | Start a chain sync server.
    , aChainSyncServer     :: ServerApp m addr bCS b

      -- | Start a block fetch client that communicates with the given
      -- upstream node.
    , aBlockFetchClient    :: ClientApp m addr bBF a

      -- | Start a block fetch server.
    , aBlockFetchServer    :: ServerApp m addr bBF b

      -- | Start a transaction submission v2 client that communicates with the
      -- given upstream node.
    , aTxSubmission2Client :: ClientApp m addr bTX a

      -- | Start a transaction submission v2 server.
    , aTxSubmission2Server :: ServerApp m addr bTX b

      -- | Start a keep-alive client.
    , aKeepAliveClient     :: ClientApp m addr bKA a

      -- | Start a keep-alive server.
    , aKeepAliveServer     :: ServerApp m addr bKA b

      -- | Start a peer-sharing client.
    , aPeerSharingClient   :: ClientApp m addr bPS a

      -- | Start a peer-sharing server.
    , aPeerSharingServer   :: ServerApp m addr bPS b
    }


-- | Per mini-protocol byte limits;  For each mini-protocol they provide
-- per-state byte size limits, i.e. how much data can arrive from the network.
--
-- They don't depend on the instantiation of the protocol parameters (which
-- block type is used, etc.), hence the use of 'RankNTypes'.
--
data ByteLimits bCS bBF bTX bKA = ByteLimits {
      blChainSync     :: forall header point tip.
                         ProtocolSizeLimits
                           (ChainSync  header point tip)
                           bCS

    , blBlockFetch    :: forall block point.
                         ProtocolSizeLimits
                           (BlockFetch block point)
                           bBF

    , blTxSubmission2 :: forall txid tx.
                         ProtocolSizeLimits
                           (TxSubmission2 txid tx)
                           bTX

    , blKeepAlive     :: ProtocolSizeLimits
                           KeepAlive
                           bKA

    }

noByteLimits :: ByteLimits bCS bBF bTX bKA
noByteLimits = ByteLimits {
    blChainSync     = byteLimitsChainSync     (const 0)
  , blBlockFetch    = byteLimitsBlockFetch    (const 0)
  , blTxSubmission2 = byteLimitsTxSubmission2 (const 0)
  , blKeepAlive     = byteLimitsKeepAlive     (const 0)
  }

byteLimits :: ByteLimits ByteString ByteString ByteString ByteString
byteLimits = ByteLimits {
      blChainSync     = byteLimitsChainSync     size
    , blBlockFetch    = byteLimitsBlockFetch    size
    , blTxSubmission2 = byteLimitsTxSubmission2 size
    , blKeepAlive     = byteLimitsKeepAlive     size
    }
  where
    size :: ByteString -> Word
    size = (fromIntegral :: Int64 -> Word)
         . BSL.length

-- | Construct the 'NetworkApplication' for the node-to-node protocols
mkApps ::
     forall m addrNTN addrNTC blk e bCS bBF bTX bKA bPS.
     ( IOLike m
     , MonadTimer m
     , Ord addrNTN
     , Exception e
     , LedgerSupportsProtocol blk
     , ShowProxy blk
     , ShowProxy (Header blk)
     , ShowProxy (TxId (GenTx blk))
     , ShowProxy (GenTx blk)
     )
  => NodeKernel m addrNTN addrNTC blk -- ^ Needed for bracketing only
  -> Tracers m (ConnectionId addrNTN) blk e
  -> (NodeToNodeVersion -> Codecs blk addrNTN e m bCS bCS bBF bBF bTX bKA bPS)
  -> ByteLimits bCS bBF bTX bKA
  -> m ChainSyncTimeout
  -> CsClient.ChainSyncLoPBucketConfig
  -> ReportPeerMetrics m (ConnectionId addrNTN)
  -> Handlers m addrNTN blk
  -> Apps m addrNTN bCS bBF bTX bKA bPS NodeToNodeInitiatorResult ()
mkApps kernel Tracers {..} mkCodecs ByteLimits {..} genChainSyncTimeout lopBucketConfig ReportPeerMetrics {..} Handlers {..} =
    Apps {..}
  where
    aChainSyncClient
      :: NodeToNodeVersion
      -> ExpandedInitiatorContext addrNTN m
      -> Channel m bCS
      -> m (NodeToNodeInitiatorResult, Maybe bCS)
    aChainSyncClient version ExpandedInitiatorContext {
                               eicConnectionId    = them,
                               eicControlMessage  = controlMessageSTM,
                               eicIsBigLedgerPeer = isBigLedgerPeer
                             }
                             channel = do
      labelThisThread "ChainSyncClient"
      -- Note that it is crucial that we sync with the fetch client "outside"
      -- of registering the state for the sync client. This is needed to
      -- maintain a state invariant required by the block fetch logic: that for
      -- each candidate chain there is a corresponding block fetch client that
      -- can be used to fetch blocks for that chain.
      bracketSyncWithFetchClient
        (getFetchClientRegistry kernel) them $
        CsClient.bracketChainSyncClient
            (contramap (TraceLabelPeer them) (Node.chainSyncClientTracer (getTracers kernel)))
            (CsClient.defaultChainDbView (getChainDB kernel))
            (getNodeCandidates kernel)
            (getNodeIdlers     kernel)
            (getChainSyncHandles kernel)
            them
            version
            lopBucketConfig
            $ \varCandidate (startIdling, stopIdling) (pauseLoPBucket, resumeLoPBucket, grantLoPToken) setTheirTip setLatestSlot -> do
              chainSyncTimeout <- genChainSyncTimeout
              (r, trailing) <-
                runPipelinedPeerWithLimits
                  (contramap (TraceLabelPeer them) tChainSyncTracer)
                  (cChainSyncCodec (mkCodecs version))
                  blChainSync
                  (timeLimitsChainSync chainSyncTimeout)
                  channel
                  $ chainSyncClientPeerPipelined
                  $ hChainSyncClient
                      them
                      isBigLedgerPeer
                      CsClient.DynamicEnv {
                          CsClient.version
                        , CsClient.controlMessageSTM
                        , CsClient.headerMetricsTracer = TraceLabelPeer them `contramap` reportHeader
                        , CsClient.varCandidate
                        , CsClient.startIdling
                        , CsClient.stopIdling
                        , CsClient.pauseLoPBucket
                        , CsClient.resumeLoPBucket
                        , CsClient.grantLoPToken
                        , CsClient.setTheirTip
                        , CsClient.setLatestSlot
                        }
              return (ChainSyncInitiatorResult r, trailing)

    aChainSyncServer
      :: NodeToNodeVersion
      -> ResponderContext addrNTN
      -> Channel m bCS
      -> m ((), Maybe bCS)
    aChainSyncServer version ResponderContext { rcConnectionId = them } channel = do
      labelThisThread "ChainSyncServer"
      chainSyncTimeout <- genChainSyncTimeout
      bracketWithPrivateRegistry
        (chainSyncHeaderServerFollower
           (getChainDB kernel)
           ( case isPipeliningEnabled version of
              ReceivingTentativeBlocks    -> ChainDB.TentativeChain
              NotReceivingTentativeBlocks -> ChainDB.SelectedChain
           )
        )
        ChainDB.followerClose
        $ \flr ->
          runPeerWithLimits
            (contramap (TraceLabelPeer them) tChainSyncSerialisedTracer)
            (cChainSyncCodecSerialised (mkCodecs version))
            blChainSync
            (timeLimitsChainSync chainSyncTimeout)
            channel
            $ chainSyncServerPeer
            $ hChainSyncServer them version flr

    aBlockFetchClient
      :: NodeToNodeVersion
      -> ExpandedInitiatorContext addrNTN m
      -> Channel m bBF
      -> m (NodeToNodeInitiatorResult, Maybe bBF)
    aBlockFetchClient version ExpandedInitiatorContext {
                                eicConnectionId   = them,
                                eicControlMessage = controlMessageSTM
                              }
                              channel = do
      labelThisThread "BlockFetchClient"
      bracketFetchClient (getFetchClientRegistry kernel) version
                         isPipeliningEnabled them $ \clientCtx -> do
        ((), trailing) <- runPipelinedPeerWithLimits
          (contramap (TraceLabelPeer them) tBlockFetchTracer)
          (cBlockFetchCodec (mkCodecs version))
          blBlockFetch
          timeLimitsBlockFetch
          channel
          $ hBlockFetchClient version controlMessageSTM
                              (TraceLabelPeer them `contramap` reportFetch) clientCtx
        return (NoInitiatorResult, trailing)

    aBlockFetchServer
      :: NodeToNodeVersion
      -> ResponderContext addrNTN
      -> Channel m bBF
      -> m ((), Maybe bBF)
    aBlockFetchServer version ResponderContext { rcConnectionId = them } channel = do
      labelThisThread "BlockFetchServer"
      withRegistry $ \registry ->
        runPeerWithLimits
          (contramap (TraceLabelPeer them) tBlockFetchSerialisedTracer)
          (cBlockFetchCodecSerialised (mkCodecs version))
          blBlockFetch
          timeLimitsBlockFetch
          channel
          $ blockFetchServerPeer
          $ hBlockFetchServer them version registry

    aTxSubmission2Client
      :: NodeToNodeVersion
      -> ExpandedInitiatorContext addrNTN m
      -> Channel m bTX
      -> m (NodeToNodeInitiatorResult, Maybe bTX)
    aTxSubmission2Client version ExpandedInitiatorContext {
                                   eicConnectionId   = them,
                                   eicControlMessage = controlMessageSTM
                                 }
                                 channel = do
      labelThisThread "TxSubmissionClient"
      ((), trailing) <- runPeerWithLimits
        (contramap (TraceLabelPeer them) tTxSubmission2Tracer)
        (cTxSubmission2Codec (mkCodecs version))
        blTxSubmission2
        timeLimitsTxSubmission2
        channel
        (txSubmissionClientPeer (hTxSubmissionClient version controlMessageSTM them))
      return (NoInitiatorResult, trailing)

    aTxSubmission2Server
      :: NodeToNodeVersion
      -> ResponderContext addrNTN
      -> Channel m bTX
      -> m ((), Maybe bTX)
    aTxSubmission2Server version ResponderContext { rcConnectionId = them } channel = do
      labelThisThread "TxSubmissionServer"
      runPipelinedPeerWithLimits
        (contramap (TraceLabelPeer them) tTxSubmission2Tracer)
        (cTxSubmission2Codec (mkCodecs version))
        blTxSubmission2
        timeLimitsTxSubmission2
        channel
        (txSubmissionServerPeerPipelined (hTxSubmissionServer version them))

    aKeepAliveClient
      :: NodeToNodeVersion
      -> ExpandedInitiatorContext addrNTN m
      -> Channel m bKA
      -> m (NodeToNodeInitiatorResult, Maybe bKA)
    aKeepAliveClient version ExpandedInitiatorContext {
                               eicConnectionId   = them,
                               eicControlMessage = controlMessageSTM
                             }
                             channel = do
      labelThisThread "KeepAliveClient"
      let kacApp = \dqCtx ->
                       runPeerWithLimits
                         nullTracer
                         (cKeepAliveCodec (mkCodecs version))
                         blKeepAlive
                         timeLimitsKeepAlive
                         channel
                         $ keepAliveClientPeer
                         $ hKeepAliveClient version controlMessageSTM them dqCtx
                             (KeepAliveInterval 10)

      ((), trailing) <- bracketKeepAliveClient (getFetchClientRegistry kernel) them kacApp
      return (NoInitiatorResult, trailing)

    aKeepAliveServer
      :: NodeToNodeVersion
      -> ResponderContext addrNTN
      -> Channel m bKA
      -> m ((), Maybe bKA)
    aKeepAliveServer version _responderCtx channel = do
      labelThisThread "KeepAliveServer"
      runPeerWithLimits
        nullTracer
        (cKeepAliveCodec (mkCodecs version))
        (byteLimitsKeepAlive (const 0)) -- TODO: Real Bytelimits, see #1727
        timeLimitsKeepAlive
        channel
        $ keepAliveServerPeer
        $ keepAliveServer


    aPeerSharingClient
      :: NodeToNodeVersion
      -> ExpandedInitiatorContext addrNTN m
      -> Channel m bPS
      -> m (NodeToNodeInitiatorResult, Maybe bPS)
    aPeerSharingClient version ExpandedInitiatorContext {
                                 eicConnectionId   = them,
                                 eicControlMessage = controlMessageSTM
                               }
                               channel = do
      labelThisThread "PeerSharingClient"
      bracketPeerSharingClient (getPeerSharingRegistry kernel) (remoteAddress them)
        $ \controller -> do
          psClient <- hPeerSharingClient version controlMessageSTM them controller
          ((), trailing) <- runPeerWithLimits
            nullTracer
            (cPeerSharingCodec (mkCodecs version))
            (byteLimitsPeerSharing (const 0))
            timeLimitsPeerSharing
            channel
            (peerSharingClientPeer psClient)
          return (NoInitiatorResult, trailing)

    aPeerSharingServer
      :: NodeToNodeVersion
      -> ResponderContext addrNTN
      -> Channel m bPS
      -> m ((), Maybe bPS)
    aPeerSharingServer version ResponderContext { rcConnectionId = them } channel = do
      labelThisThread "PeerSharingServer"
      runPeerWithLimits
        nullTracer
        (cPeerSharingCodec (mkCodecs version))
        (byteLimitsPeerSharing (const 0))
        timeLimitsPeerSharing
        channel
        $ peerSharingServerPeer
        $ hPeerSharingServer version them

{-------------------------------------------------------------------------------
  Projections from 'Apps'
-------------------------------------------------------------------------------}

-- | A projection from 'NetworkApplication' to a client-side
-- 'OuroborosApplication' for the node-to-node protocols.
--
-- Implementation note: network currently doesn't enable protocols conditional
-- on the protocol version, but it eventually may; this is why @_version@ is
-- currently unused.
initiator ::
     MiniProtocolParameters
  -> NodeToNodeVersion
  -> PSTypes.PeerSharing
  -> Apps m addr b b b b b a c
  -> OuroborosBundleWithExpandedCtx 'InitiatorMode addr b m a Void
initiator miniProtocolParameters version ownPeerSharing Apps {..} =
    nodeToNodeProtocols
      miniProtocolParameters
      -- TODO: currently consensus is using 'ConnectionId' for its 'peer' type.
      -- This is currently ok, as we might accept multiple connections from the
      -- same ip address, however this will change when we will switch to
      -- p2p-governor & connection-manager.  Then consensus can use peer's ip
      -- address & port number, rather than 'ConnectionId' (which is
      -- a quadruple uniquely determining a connection).
      (NodeToNodeProtocols {
          chainSyncProtocol =
            (InitiatorProtocolOnly (MiniProtocolCb (\ctx -> aChainSyncClient version ctx))),
          blockFetchProtocol =
            (InitiatorProtocolOnly (MiniProtocolCb (\ctx -> aBlockFetchClient version ctx))),
          txSubmissionProtocol =
            (InitiatorProtocolOnly (MiniProtocolCb (\ctx -> aTxSubmission2Client version ctx))),
          keepAliveProtocol =
            (InitiatorProtocolOnly (MiniProtocolCb (\ctx -> aKeepAliveClient version ctx))),
          peerSharingProtocol =
            (InitiatorProtocolOnly (MiniProtocolCb (\ctx -> aPeerSharingClient version ctx)))
        })
      version
      ownPeerSharing

-- | A bi-directional network application.
--
-- Implementation note: network currently doesn't enable protocols conditional
-- on the protocol version, but it eventually may; this is why @_version@ is
-- currently unused.
initiatorAndResponder ::
     MiniProtocolParameters
  -> NodeToNodeVersion
  -> PSTypes.PeerSharing
  -> Apps m addr b b b b b a c
  -> OuroborosBundleWithExpandedCtx 'InitiatorResponderMode addr b m a c
initiatorAndResponder miniProtocolParameters version ownPeerSharing Apps {..} =
    nodeToNodeProtocols
      miniProtocolParameters
      (NodeToNodeProtocols {
          chainSyncProtocol =
            (InitiatorAndResponderProtocol
              (MiniProtocolCb (\initiatorCtx -> aChainSyncClient version initiatorCtx))
              (MiniProtocolCb (\responderCtx -> aChainSyncServer version responderCtx))),
          blockFetchProtocol =
            (InitiatorAndResponderProtocol
              (MiniProtocolCb (\initiatorCtx -> aBlockFetchClient version initiatorCtx))
              (MiniProtocolCb (\responderCtx -> aBlockFetchServer version responderCtx))),
          txSubmissionProtocol =
            (InitiatorAndResponderProtocol
              (MiniProtocolCb (\initiatorCtx -> aTxSubmission2Client version initiatorCtx))
              (MiniProtocolCb (\responderCtx -> aTxSubmission2Server version responderCtx))),
          keepAliveProtocol =
            (InitiatorAndResponderProtocol
              (MiniProtocolCb (\initiatorCtx -> aKeepAliveClient version initiatorCtx))
              (MiniProtocolCb (\responderCtx -> aKeepAliveServer version responderCtx))),

          peerSharingProtocol =
            (InitiatorAndResponderProtocol
              (MiniProtocolCb (\initiatorCtx -> aPeerSharingClient version initiatorCtx))
              (MiniProtocolCb (\responderCtx -> aPeerSharingServer version responderCtx)))
        })
      version
      ownPeerSharing
