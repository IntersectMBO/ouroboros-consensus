{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Intended for qualified import
module Ouroboros.Consensus.Network.NodeToNode
  ( -- * Handlers
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

    -- * Leios mini-protocol limits
  , leiosNotifyProtocolLimits
  , leiosFetchProtocolLimits
  ) where

import Cardano.Network.NodeToNode
import Cardano.Network.PeerSelection (PeerTrustable (..))
import Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import Codec.CBOR.Read (DeserialiseFailure)
import Control.Applicative ((<|>))
import qualified Control.Concurrent.Class.MonadMVar as MVar
import Control.Concurrent.Class.MonadSTM.Strict (readTChan)
import qualified Control.Concurrent.Class.MonadSTM.Strict.TVar as TVar.Unchecked
import Control.DeepSeq (NFData)
import Control.Monad (forM_, void)
import Control.Monad.Class.MonadTime.SI (MonadTime)
import Control.Monad.Class.MonadTimer.SI (MonadTimer)
import Control.ResourceRegistry
import Control.Tracer
import Data.ByteString.Lazy (ByteString)
import Data.Functor ((<&>))
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Void (Void)
import LeiosDemoDb
  ( LeiosDbHandle (subscribeEbNotifications)
  , LeiosEbNotification (..)
  )
import qualified LeiosDemoDb as LeiosDb
import qualified LeiosDemoLogic as Leios
import LeiosDemoOnlyTestFetch
  ( LeiosFetch
  , LeiosFetchClientPeerPipelined
  , LeiosFetchServerPeer
  , byteLimitsLeiosFetch
  , codecLeiosFetch
  , codecLeiosFetchId
  , leiosFetchClientPeerPipelined
  , leiosFetchMiniProtocolNum
  , leiosFetchServerPeer
  , timeLimitsLeiosFetch
  , toLeiosFetchClientPeerPipelined
  )
import LeiosDemoOnlyTestNotify
  ( LeiosNotify
  , LeiosNotifyClientPeerPipelined
  , LeiosNotifyServerPeer
  , Message
    ( MsgLeiosBlockAnnouncement
    , MsgLeiosBlockOffer
    , MsgLeiosBlockTxsOffer
    , MsgLeiosVotes
    )
  , byteLimitsLeiosNotify
  , codecLeiosNotify
  , codecLeiosNotifyId
  , leiosNotifyClientPeerPipelined
  , leiosNotifyMiniProtocolNum
  , leiosNotifyServerPeer
  , timeLimitsLeiosNotify
  , toLeiosNotifyClientPeerPipelined
  )
import qualified LeiosDemoOnlyTestNotify
import LeiosDemoTypes
  ( LeiosEb
  , LeiosPoint (..)
  , LeiosTx
  , LeiosVote
  , TraceLeiosKernel (..)
  , TraceLeiosPeer (..)
  )
import qualified LeiosDemoTypes as Leios
import LeiosVoteState
  ( LeiosVoteState (..)
  , LeiosVoteSubscription (..)
  , subscribeVotes
  )
import qualified Network.Mux as Mux
import Network.TypedProtocol.Codec
import Network.TypedProtocol.Peer (Peer (Effect))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config (DiffusionPipeliningSupport (..))
import Ouroboros.Consensus.HeaderValidation (HeaderWithTime)
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client
  ( ChainSyncStateView (..)
  )
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client as CsClient
import Ouroboros.Consensus.MiniProtocol.ChainSync.Server
import Ouroboros.Consensus.Node.ExitPolicy
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Node.Run
import Ouroboros.Consensus.Node.Serialisation
import qualified Ouroboros.Consensus.Node.Tracers as Node
import Ouroboros.Consensus.NodeKernel
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.Serialisation (SerialisedHeader)
import Ouroboros.Consensus.Util (ShowProxy)
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.Orphans ()
import Ouroboros.Network.Block
  ( Serialised (..)
  , decodePoint
  , decodeTip
  , encodePoint
  , encodeTip
  )
import Ouroboros.Network.BlockFetch
import Ouroboros.Network.BlockFetch.Client
  ( BlockFetchClient
  , blockFetchClient
  )
import Ouroboros.Network.Channel
import Ouroboros.Network.DeltaQ
import Ouroboros.Network.Driver
import Ouroboros.Network.Driver.Limits
import Ouroboros.Network.KeepAlive
import Ouroboros.Network.Mux
import Ouroboros.Network.PeerSelection.PeerMetric.Type
  ( FetchedMetricsTracer
  , ReportPeerMetrics (..)
  )
import Ouroboros.Network.PeerSharing
  ( PeerSharingController
  , bracketPeerSharingClient
  , peerSharingClient
  , peerSharingServer
  )
import Ouroboros.Network.Protocol.BlockFetch.Codec
import Ouroboros.Network.Protocol.BlockFetch.Server
  ( BlockFetchServer
  , blockFetchServerPeer
  )
import Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch (..))
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
import Ouroboros.Network.Protocol.ChainSync.Codec
import Ouroboros.Network.Protocol.ChainSync.PipelineDecision
import Ouroboros.Network.Protocol.ChainSync.Server
import Ouroboros.Network.Protocol.ChainSync.Type
import Ouroboros.Network.Protocol.KeepAlive.Client
import Ouroboros.Network.Protocol.KeepAlive.Codec
import Ouroboros.Network.Protocol.KeepAlive.Server
import Ouroboros.Network.Protocol.KeepAlive.Type
import Ouroboros.Network.Protocol.Limits (BearerBytes)
import Ouroboros.Network.Protocol.PeerSharing.Client
  ( PeerSharingClient
  , peerSharingClientPeer
  )
import Ouroboros.Network.Protocol.PeerSharing.Codec
  ( byteLimitsPeerSharing
  , codecPeerSharing
  , codecPeerSharingId
  , timeLimitsPeerSharing
  )
import Ouroboros.Network.Protocol.PeerSharing.Server
  ( PeerSharingServer
  , peerSharingServerPeer
  )
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharing)
import Ouroboros.Network.Protocol.TxSubmission2.Client
import Ouroboros.Network.Protocol.TxSubmission2.Codec
import Ouroboros.Network.Protocol.TxSubmission2.Server
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Inbound.V1
import Ouroboros.Network.TxSubmission.Inbound.V2
  ( PeerTxAPI
  , TraceTxLogic
  , TxDecisionPolicy (..)
  , TxSubmissionLogicVersion (..)
  , defaultTxDecisionPolicy
  , txSubmissionInboundV2
  , withPeer
  )
import Ouroboros.Network.TxSubmission.Mempool.Reader
  ( mapTxSubmissionMempoolReader
  )
import Ouroboros.Network.TxSubmission.Outbound
import qualified Ouroboros.Network.Util.ShowProxy as NUS
import System.Random (StdGen, splitGen)

{-------------------------------------------------------------------------------
  Handlers
-------------------------------------------------------------------------------}

-- | Protocol handlers for node-to-node (remote) communication
instance NUS.ShowProxy () where
  showProxy _ = "()"

data Handlers m addr blk = Handlers
  { hChainSyncClient ::
      ConnectionId addr ->
      IsBigLedgerPeer ->
      CsClient.DynamicEnv m blk ->
      ChainSyncClientPipelined
        (Header blk)
        (Point blk)
        (Tip blk)
        m
        CsClient.ChainSyncClientResult
  , -- TODO: we should reconsider bundling these context parameters into a
    -- record, perhaps instead extending the protocol handler
    -- representation to support bracket-style initialisation so that we
    -- could have the closure include these and not need to be explicit
    -- about them here.

    hChainSyncServer ::
      ConnectionId addr ->
      NodeToNodeVersion ->
      ChainDB.Follower m blk (ChainDB.WithPoint blk (SerialisedHeader blk)) ->
      ChainSyncServer (SerialisedHeader blk) (Point blk) (Tip blk) m ()
  , -- TODO block fetch client does not have GADT view of the handlers.
    hBlockFetchClient ::
      NodeToNodeVersion ->
      ControlMessageSTM m ->
      FetchedMetricsTracer m ->
      BlockFetchClient (HeaderWithTime blk) blk m ()
  , hBlockFetchServer ::
      ConnectionId addr ->
      NodeToNodeVersion ->
      ResourceRegistry m ->
      BlockFetchServer (Serialised blk) (Point blk) m ()
  , hTxSubmissionClient ::
      NodeToNodeVersion ->
      ControlMessageSTM m ->
      ConnectionId addr ->
      TxSubmissionClient (GenTxId blk) (GenTx blk) m ()
  , hTxSubmissionServer ::
      NodeToNodeVersion ->
      ConnectionId addr ->
      Either
        (TxSubmissionServerPipelined (GenTxId blk) (GenTx blk) m ())
        ( PeerTxAPI m (GenTxId blk) (GenTx blk) ->
          TxSubmissionServerPipelined (GenTxId blk) (GenTx blk) m ()
        )
  -- ^ Either we use the legacy tx submission protocol or the newest one
  -- which require PeerTxAPI. This is decided by
  -- 'EnableNewTxSubmissionProtocol' flag.
  , hKeepAliveClient ::
      NodeToNodeVersion ->
      ControlMessageSTM m ->
      ConnectionId addr ->
      TVar.Unchecked.StrictTVar m (Map (ConnectionId addr) PeerGSV) ->
      KeepAliveInterval ->
      KeepAliveClient m ()
  , hKeepAliveServer ::
      NodeToNodeVersion ->
      ConnectionId addr ->
      KeepAliveServer m ()
  , hPeerSharingClient ::
      NodeToNodeVersion ->
      ControlMessageSTM m ->
      ConnectionId addr ->
      PeerSharingController addr m ->
      m (PeerSharingClient addr m ())
  , hPeerSharingServer ::
      NodeToNodeVersion ->
      ConnectionId addr ->
      PeerSharingServer addr m
  , hLeiosNotifyClient ::
      NodeToNodeVersion ->
      ControlMessageSTM m ->
      ConnectionId addr ->
      Leios.LeiosPeerVars m ->
      LeiosNotifyClientPeerPipelined LeiosPoint () LeiosVote m ()
  , hLeiosNotifyServer ::
      NodeToNodeVersion ->
      ConnectionId addr ->
      LeiosNotifyServerPeer LeiosPoint () LeiosVote m ()
  , hLeiosFetchClient ::
      NodeToNodeVersion ->
      ControlMessageSTM m ->
      ConnectionId addr ->
      LeiosFetchClientPeerPipelined LeiosPoint LeiosEb LeiosTx m ()
  , hLeiosFetchServer ::
      NodeToNodeVersion ->
      ConnectionId addr ->
      LeiosFetchServerPeer LeiosPoint LeiosEb LeiosTx m ()
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
  ) =>
  NodeKernelArgs m addrNTN addrNTC blk ->
  NodeKernel m addrNTN addrNTC blk ->
  TxSubmissionLogicVersion ->
  Handlers m addrNTN blk
mkHandlers
  NodeKernelArgs
    { chainSyncFutureCheck
    , chainSyncHistoricityCheck
    , keepAliveRng
    , miniProtocolParameters
    , getDiffusionPipeliningSupport
    , txSubmissionInitDelay
    }
  nodeKernel@NodeKernel
    { getChainDB
    , getMempool
    , getTopLevelConfig
    , getTracers = tracers
    , getPeerSharingAPI
    , getGsmState
    }
  txSubmissionLogicVersion =
    Handlers
      { hChainSyncClient = \peer _isBigLedgerpeer dynEnv ->
          CsClient.chainSyncClient
            CsClient.ConfigEnv
              { CsClient.cfg = getTopLevelConfig
              , CsClient.someHeaderInFutureCheck = chainSyncFutureCheck
              , CsClient.historicityCheck = chainSyncHistoricityCheck (atomically getGsmState)
              , CsClient.chainDbView =
                  CsClient.defaultChainDbView getChainDB
              , CsClient.mkPipelineDecision0 =
                  pipelineDecisionLowHighMark
                    (chainSyncPipeliningLowMark miniProtocolParameters)
                    (chainSyncPipeliningHighMark miniProtocolParameters)
              , CsClient.tracer =
                  contramap (TraceLabelPeer peer) (Node.chainSyncClientTracer tracers)
              , CsClient.getDiffusionPipeliningSupport = getDiffusionPipeliningSupport
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
            ( NumTxIdsToAck $
                getNumTxIdsToReq $
                  maxUnacknowledgedTxIds $
                    txDecisionPolicy $
                      miniProtocolParameters
            )
            (mapTxSubmissionMempoolReader txForgetValidated $ getMempoolReader getMempool)
            version
            controlMessageSTM
      , hTxSubmissionServer = \version peer ->
          case txSubmissionLogicVersion of
            TxSubmissionLogicV2 ->
              Right $ \api ->
                txSubmissionInboundV2
                  ( contramap
                      (TraceLabelPeer peer)
                      (Node.txInboundTracer tracers)
                  )
                  txSubmissionInitDelay
                  (getMempoolWriter getMempool)
                  api
            TxSubmissionLogicV1 ->
              Left $
                txSubmissionInbound
                  (contramap (TraceLabelPeer peer) (Node.txInboundTracer tracers))
                  txSubmissionInitDelay
                  ( NumTxIdsToAck $
                      getNumTxIdsToReq $
                        maxUnacknowledgedTxIds $
                          txDecisionPolicy $
                            miniProtocolParameters
                  )
                  (mapTxSubmissionMempoolReader txForgetValidated $ getMempoolReader getMempool)
                  (getMempoolWriter getMempool)
                  version
      , hKeepAliveClient = \_version -> keepAliveClient (Node.keepAliveClientTracer tracers) keepAliveRng
      , hKeepAliveServer = \_version _peer -> keepAliveServer
      , hPeerSharingClient = \_version controlMessageSTM _peer -> peerSharingClient controlMessageSTM
      , hPeerSharingServer = \_version _peer -> peerSharingServer getPeerSharingAPI
      , hLeiosNotifyClient = \_version controlMessageSTM peer peerVars -> toLeiosNotifyClientPeerPipelined $ Effect $ do
          let tracer = leiosPeerTracer peer
              kernelTracer = Node.leiosKernelTracer tracers
              LeiosVoteState{addVote} = leiosVoteState
          pure $
            leiosNotifyClientPeerPipelined
              ( atomically controlMessageSTM <&> \case
                  Terminate -> Left ()
                  _ -> Right 10 {- TODO magic number -}
              )
              ( pure $ \case
                  MsgLeiosBlockAnnouncement{} -> error "Demo does not send EB announcements!"
                  MsgLeiosBlockOffer point ebBytesSize -> do
                    traceWith tracer $ MkTraceLeiosPeer $ "MsgLeiosBlockOffer " <> Leios.prettyLeiosPoint point
                    let MkLeiosPoint{pointEbHash = ebHash} = point
                    -- FIXME: EB announcements are not implemented. The
                    -- fetch state is built entirely from peer offers,
                    -- which is the wrong source of truth — the
                    -- authoritative size lives in
                    -- 'headerLeiosAnnouncement' on the parent RB
                    -- header (signed by the forger). Until announcement
                    -- handling lands, the sanitisation below is the
                    -- best we can do against malformed offers:
                    -- drop a zero-sized offer outright (no honest
                    -- forger ever announces a 0-byte EB) and refuse to
                    -- overwrite an existing entry that shares the same
                    -- content hash, so the first-seen (slot, size)
                    -- wins. The per-peer 'offerings' below is still
                    -- updated so the peer remains a valid serving
                    -- candidate.
                    MVar.modifyMVar_ getLeiosOutstanding $ \outstanding ->
                      pure $
                        if ebBytesSize == 0
                          || Set.member ebHash (Leios.acquiredEbBodies outstanding)
                          || any
                            ((== ebHash) . pointEbHash)
                            (Map.keys (Leios.missingEbBodies outstanding))
                          then outstanding
                          else
                            outstanding
                              { Leios.missingEbBodies =
                                  Map.insert point ebBytesSize (Leios.missingEbBodies outstanding)
                              }
                    MVar.modifyMVar_ (Leios.offerings peerVars) $ \(offers1, offers2) -> do
                      let !offers1' = Set.insert ebHash offers1
                      pure (offers1', offers2)
                    void $ MVar.tryPutMVar getLeiosReady ()
                  MsgLeiosBlockTxsOffer p -> do
                    traceWith tracer $ MkTraceLeiosPeer $ "MsgLeiosBlockTxsOffer " <> Leios.prettyLeiosPoint p
                    let MkLeiosPoint{pointEbHash = ebHash} = p
                    MVar.modifyMVar_ (Leios.offerings peerVars) $ \(offers1, offers2) -> do
                      let !offers2' = Set.insert ebHash offers2
                      pure (offers1, offers2')
                    void $ MVar.tryPutMVar getLeiosReady ()
                  MsgLeiosVotes vs -> do
                    traceWith tracer $ MkTraceLeiosPeer $ "MsgLeiosVotes " <> show vs
                    mapM_ addVote vs
                    forM_ vs $ \vote ->
                      traceWith kernelTracer TraceLeiosVoteAcquired{vote}
              )
      , hLeiosNotifyServer = \_version _peer -> Effect $ do
          chan <- subscribeEbNotifications leiosDB
          let processEbNotification ::
                STM
                  m
                  ( LeiosDemoOnlyTestNotify.Message
                      (LeiosNotify LeiosPoint () LeiosVote)
                      LeiosDemoOnlyTestNotify.StBusy
                      LeiosDemoOnlyTestNotify.StIdle
                  )
              processEbNotification =
                readTChan chan >>= \case
                  AcquiredEb point ebSize ->
                    pure $ MsgLeiosBlockOffer point ebSize
                  AcquiredEbTxs point ->
                    pure $ MsgLeiosBlockTxsOffer point

          LeiosVoteSubscription{getNextVote} <- subscribeVotes leiosVoteState
          let processVote ::
                STM
                  m
                  ( LeiosDemoOnlyTestNotify.Message
                      (LeiosNotify LeiosPoint () LeiosVote)
                      LeiosDemoOnlyTestNotify.StBusy
                      LeiosDemoOnlyTestNotify.StIdle
                  )
              processVote = do
                vote <- getNextVote
                pure $ MsgLeiosVotes [vote]

          pure . leiosNotifyServerPeer $
            atomically $
              processEbNotification <|> processVote
      , hLeiosFetchClient = \_version controlMessageSTM peer -> toLeiosFetchClientPeerPipelined $ Effect $ do
          reqVar <-
            -- Wait for the LeiosNotify client to publish this peer's vars. We
            -- also watch 'controlMessageSTM': if the peer is being disconnected
            -- from before the entry ever appears, it never will, and this loop
            -- would otherwise spin forever. On 'Terminate' we hand back an empty
            -- queue, so 'nextLeiosFetchClientCommand' observes 'stopSTM' on its
            -- first transaction and terminates the client immediately.
            let loop = do
                  terminating <- atomically $ (== Terminate) <$> controlMessageSTM
                  if terminating
                    then TVar.Unchecked.newTVarIO Seq.empty
                    else do
                      leiosPeersVars <- MVar.readMVar getLeiosPeersVars
                      case Map.lookup (Leios.MkPeerId peer) leiosPeersVars of
                        Just x -> pure $ Leios.requestsToSend x
                        Nothing -> do
                          threadDelay (0.010 :: DiffTime)
                          loop
             in loop
          leiosConn <- LeiosDb.open leiosDB
          pure $
            leiosFetchClientPeerPipelined $
              Leios.nextLeiosFetchClientCommand
                (Node.leiosKernelTracer tracers)
                (leiosPeerTracer peer)
                ((== Terminate) <$> controlMessageSTM)
                (getLeiosOutstanding, getLeiosReady)
                leiosConn
                (Leios.MkPeerId peer)
                reqVar
      , hLeiosFetchServer = \_version peer -> Effect $ do
          leiosFetchContext <-
            Leios.newLeiosFetchContext
              leiosDB
          pure $
            leiosFetchServerPeer
              (pure $ Leios.leiosFetchHandler (leiosPeerTracer peer) leiosFetchContext)
      }
   where
    NodeKernel
      { getLeiosDB = leiosDB
      , getLeiosVoteState = leiosVoteState
      , getLeiosPeersVars
      , getLeiosOutstanding
      , getLeiosReady
      } = nodeKernel

    leiosPeerTracer peer = TraceLabelPeer peer `contramap` Node.leiosPeerTracer tracers

{-------------------------------------------------------------------------------
  Codecs
-------------------------------------------------------------------------------}

-- | Node-to-node protocol codecs needed to run 'Handlers'.
data Codecs blk addr e m bCS bSCS bBF bSBF bTX bKA bPS bLN bLF = Codecs
  { cChainSyncCodec :: Codec (ChainSync (Header blk) (Point blk) (Tip blk)) e m bCS
  , cChainSyncCodecSerialised ::
      Codec (ChainSync (SerialisedHeader blk) (Point blk) (Tip blk)) e m bSCS
  , cBlockFetchCodec :: Codec (BlockFetch blk (Point blk)) e m bBF
  , cBlockFetchCodecSerialised ::
      Codec (BlockFetch (Serialised blk) (Point blk)) e m bSBF
  , cTxSubmission2Codec :: Codec (TxSubmission2 (GenTxId blk) (GenTx blk)) e m bTX
  , cKeepAliveCodec :: Codec KeepAlive e m bKA
  , cPeerSharingCodec :: Codec (PeerSharing addr) e m bPS
  , cLeiosNotifyCodec :: Codec (LeiosNotify LeiosPoint () LeiosVote) e m bLN
  , cLeiosFetchCodec :: Codec (LeiosFetch LeiosPoint LeiosEb LeiosTx) e m bLF
  }

-- | Protocol codecs for the node-to-node protocols
defaultCodecs ::
  forall m blk addr.
  ( IOLike m
  , SerialiseNodeToNodeConstraints blk
  ) =>
  CodecConfig blk ->
  BlockNodeToNodeVersion blk ->
  (NodeToNodeVersion -> addr -> CBOR.Encoding) ->
  (NodeToNodeVersion -> forall s. CBOR.Decoder s addr) ->
  NodeToNodeVersion ->
  Codecs
    blk
    addr
    DeserialiseFailure
    m
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString
defaultCodecs ccfg version encAddr decAddr nodeToNodeVersion =
  Codecs
    { cChainSyncCodec =
        codecChainSync
          enc
          dec
          (encodePoint (encodeRawHash p))
          (decodePoint (decodeRawHash p))
          (encodeTip (encodeRawHash p))
          (decodeTip (decodeRawHash p))
    , cChainSyncCodecSerialised =
        codecChainSync
          enc
          dec
          (encodePoint (encodeRawHash p))
          (decodePoint (decodeRawHash p))
          (encodeTip (encodeRawHash p))
          (decodeTip (decodeRawHash p))
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
    , cLeiosNotifyCodec =
        codecLeiosNotify
          Leios.encodeLeiosPoint
          Leios.decodeLeiosPoint
          (\() -> CBOR.encodeNull)
          CBOR.decodeNull
          Leios.encodeLeiosVote
          Leios.decodeLeiosVote
    , cLeiosFetchCodec =
        codecLeiosFetch
          Leios.encodeLeiosPoint
          Leios.decodeLeiosPoint
          Leios.encodeLeiosEb
          Leios.decodeLeiosEb
          Leios.encodeLeiosTx
          Leios.decodeLeiosTx
    }
 where
  p :: Proxy blk
  p = Proxy

  enc :: SerialiseNodeToNode blk a => a -> Encoding
  enc = encodeNodeToNode ccfg version

  dec :: SerialiseNodeToNode blk a => forall s. Decoder s a
  dec = decodeNodeToNode ccfg version

-- | Identity codecs used in tests.
identityCodecs ::
  Monad m =>
  Codecs
    blk
    addr
    CodecFailure
    m
    (AnyMessage (ChainSync (Header blk) (Point blk) (Tip blk)))
    (AnyMessage (ChainSync (SerialisedHeader blk) (Point blk) (Tip blk)))
    (AnyMessage (BlockFetch blk (Point blk)))
    (AnyMessage (BlockFetch (Serialised blk) (Point blk)))
    (AnyMessage (TxSubmission2 (GenTxId blk) (GenTx blk)))
    (AnyMessage KeepAlive)
    (AnyMessage (PeerSharing addr))
    (AnyMessage (LeiosNotify LeiosPoint () LeiosVote))
    (AnyMessage (LeiosFetch LeiosPoint LeiosEb LeiosTx))
identityCodecs =
  Codecs
    { cChainSyncCodec = codecChainSyncId
    , cChainSyncCodecSerialised = codecChainSyncId
    , cBlockFetchCodec = codecBlockFetchId
    , cBlockFetchCodecSerialised = codecBlockFetchId
    , cTxSubmission2Codec = codecTxSubmission2Id
    , cKeepAliveCodec = codecKeepAliveId
    , cPeerSharingCodec = codecPeerSharingId
    , cLeiosNotifyCodec = codecLeiosNotifyId
    , cLeiosFetchCodec = codecLeiosFetchId
    }

{-------------------------------------------------------------------------------
  Tracers
-------------------------------------------------------------------------------}

-- | A record of 'Tracer's for the different protocols.
type Tracers m ntnAddr blk e =
  Tracers' (ConnectionId ntnAddr) ntnAddr blk e (Tracer m)

data Tracers' peer ntnAddr blk e f = Tracers
  { tChainSyncTracer ::
      f (TraceLabelPeer peer (TraceSendRecv (ChainSync (Header blk) (Point blk) (Tip blk))))
  , tChainSyncSerialisedTracer ::
      f (TraceLabelPeer peer (TraceSendRecv (ChainSync (SerialisedHeader blk) (Point blk) (Tip blk))))
  , tBlockFetchTracer :: f (TraceLabelPeer peer (TraceSendRecv (BlockFetch blk (Point blk))))
  , tBlockFetchSerialisedTracer ::
      f (TraceLabelPeer peer (TraceSendRecv (BlockFetch (Serialised blk) (Point blk))))
  , tTxSubmission2Tracer ::
      f (TraceLabelPeer peer (TraceSendRecv (TxSubmission2 (GenTxId blk) (GenTx blk))))
  , tKeepAliveTracer :: f (TraceLabelPeer peer (TraceSendRecv KeepAlive))
  , tPeerSharingTracer :: f (TraceLabelPeer peer (TraceSendRecv (PeerSharing ntnAddr)))
  , tTxLogicTracer :: f (TraceLabelPeer peer (TraceTxLogic peer (GenTxId blk) (GenTx blk)))
  , tLeiosNotifyTracer ::
      f (TraceLabelPeer peer (TraceSendRecv (LeiosNotify LeiosPoint () LeiosVote)))
  , tLeiosFetchTracer ::
      f (TraceLabelPeer peer (TraceSendRecv (LeiosFetch LeiosPoint LeiosEb LeiosTx)))
  }

instance (forall a. Semigroup (f a)) => Semigroup (Tracers' peer ntnAddr blk e f) where
  l <> r =
    Tracers
      { tChainSyncTracer = f tChainSyncTracer
      , tChainSyncSerialisedTracer = f tChainSyncSerialisedTracer
      , tBlockFetchTracer = f tBlockFetchTracer
      , tBlockFetchSerialisedTracer = f tBlockFetchSerialisedTracer
      , tTxSubmission2Tracer = f tTxSubmission2Tracer
      , tKeepAliveTracer = f tKeepAliveTracer
      , tPeerSharingTracer = f tPeerSharingTracer
      , tTxLogicTracer = f tTxLogicTracer
      , tLeiosNotifyTracer = f tLeiosNotifyTracer
      , tLeiosFetchTracer = f tLeiosFetchTracer
      }
   where
    f ::
      forall a.
      Semigroup a =>
      (Tracers' peer ntnAddr blk e f -> a) ->
      a
    f prj = prj l <> prj r

-- | Use a 'nullTracer' for each protocol.
nullTracers :: Monad m => Tracers m ntnAddr blk e
nullTracers =
  Tracers
    { tChainSyncTracer = nullTracer
    , tChainSyncSerialisedTracer = nullTracer
    , tBlockFetchTracer = nullTracer
    , tBlockFetchSerialisedTracer = nullTracer
    , tTxSubmission2Tracer = nullTracer
    , tKeepAliveTracer = nullTracer
    , tPeerSharingTracer = nullTracer
    , tTxLogicTracer = nullTracer
    , tLeiosNotifyTracer = nullTracer
    , tLeiosFetchTracer = nullTracer
    }

showTracers ::
  ( Show blk
  , Show ntnAddr
  , Show (Header blk)
  , Show (GenTx blk)
  , Show (GenTxId blk)
  , HasHeader blk
  , HasNestedContent Header blk
  ) =>
  Tracer m String -> Tracers m ntnAddr blk e
showTracers tr =
  Tracers
    { tChainSyncTracer = showTracing tr
    , tChainSyncSerialisedTracer = showTracing tr
    , tBlockFetchTracer = showTracing tr
    , tBlockFetchSerialisedTracer = showTracing tr
    , tTxSubmission2Tracer = showTracing tr
    , tKeepAliveTracer = showTracing tr
    , tPeerSharingTracer = showTracing tr
    , tTxLogicTracer = showTracing tr
    , tLeiosNotifyTracer = showTracing tr
    , tLeiosFetchTracer = showTracing tr
    }

{-------------------------------------------------------------------------------
  Applications
-------------------------------------------------------------------------------}

-- | A node-to-node application
type ClientApp m addr bytes a =
  NodeToNodeVersion ->
  ExpandedInitiatorContext addr PeerTrustable m ->
  Channel m bytes ->
  m (a, Maybe (Reception bytes))

type ServerApp m addr bytes a =
  NodeToNodeVersion ->
  ResponderContext addr ->
  Channel m bytes ->
  m (a, Maybe (Reception bytes))

-- | Applications for the node-to-node protocols
--
-- See 'Network.Mux.Types.MuxApplication'
data Apps m addr bCS bBF bTX bKA bPS bLN bLF a b = Apps
  { aChainSyncClient :: ClientApp m addr bCS a
  , aChainSyncServer :: ServerApp m addr bCS b
  , aBlockFetchClient :: ClientApp m addr bBF a
  , aBlockFetchServer :: ServerApp m addr bBF b
  , aTxSubmission2Client :: ClientApp m addr bTX a
  , aTxSubmission2Server :: ServerApp m addr bTX b
  -- ^ Start a transaction submission v2 server.
  , aKeepAliveClient :: ClientApp m addr bKA a
  , aKeepAliveServer :: ServerApp m addr bKA b
  , aPeerSharingClient :: ClientApp m addr bPS a
  , aPeerSharingServer :: ServerApp m addr bPS b
  , aLeiosNotifyClient :: ClientApp m addr bLN a
  -- ^ Start a Leios notify client.
  , aLeiosNotifyServer :: ServerApp m addr bLN b
  , aLeiosFetchClient :: ClientApp m addr bLF a
  -- ^ Start a Leios fetch client.
  , aLeiosFetchServer :: ServerApp m addr bLF b
  }

-- | Per mini-protocol byte limits;  For each mini-protocol they provide
-- per-state byte size limits, i.e. how much data can arrive from the network.
--
-- They don't depend on the instantiation of the protocol parameters (which
-- block type is used, etc.), hence the use of 'RankNTypes'.
data ByteLimits bCS bBF bTX bKA bPS bLN bLF = ByteLimits
  { blChainSync ::
      forall header point tip.
      ProtocolSizeLimits
        (ChainSync header point tip)
        bCS
  , blBlockFetch ::
      forall block point.
      ProtocolSizeLimits
        (BlockFetch block point)
        bBF
  , blTxSubmission2 ::
      forall txid tx.
      ProtocolSizeLimits
        (TxSubmission2 txid tx)
        bTX
  , blKeepAlive ::
      ProtocolSizeLimits
        KeepAlive
        bKA
  , blPeerSharing ::
      forall addr.
      ProtocolSizeLimits
        (PeerSharing addr)
        bPS
  , blLeiosNotify ::
      forall ebpoint v vote.
      ProtocolSizeLimits
        (LeiosNotify ebpoint v vote)
        bLN
  , blLeiosFetch ::
      forall ebpoint eb tx.
      ProtocolSizeLimits
        (LeiosFetch ebpoint eb tx)
        bLF
  }

noByteLimits :: ByteLimits bCS bBF bTX bKA bPS bLN bLF
noByteLimits =
  ByteLimits
    { blChainSync = byteLimitsChainSync
    , blBlockFetch = byteLimitsBlockFetch
    , blTxSubmission2 = byteLimitsTxSubmission2
    , blKeepAlive = byteLimitsKeepAlive
    , blPeerSharing = byteLimitsPeerSharing
    , blLeiosNotify = byteLimitsLeiosNotify
    , blLeiosFetch = byteLimitsLeiosFetch
    }

byteLimits ::
  ByteLimits ByteString ByteString ByteString ByteString ByteString ByteString ByteString
byteLimits =
  ByteLimits
    { blChainSync = byteLimitsChainSync
    , blBlockFetch = byteLimitsBlockFetch
    , blTxSubmission2 = byteLimitsTxSubmission2
    , blKeepAlive = byteLimitsKeepAlive
    , blPeerSharing = byteLimitsPeerSharing
    , blLeiosNotify = byteLimitsLeiosNotify
    , blLeiosFetch = byteLimitsLeiosFetch
    }

-- | Construct the 'NetworkApplication' for the node-to-node protocols
mkApps ::
  forall m addrNTN addrNTC blk e bCS bBF bTX bKA bPS bLN bLF.
  ( IOLike m
  , MonadTimer m
  , Ord addrNTN
  , Exception e
  , NFData e
  , LedgerSupportsProtocol blk
  , ShowProxy blk
  , ShowProxy (Header blk)
  , ShowProxy (TxId (GenTx blk))
  , ShowProxy (GenTx blk)
  , Show addrNTN
  , LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  , BearerBytes bCS
  , BearerBytes bBF
  , BearerBytes bTX
  , BearerBytes bKA
  , BearerBytes bPS
  , BearerBytes bLN
  , BearerBytes bLF
  ) =>
  -- | Needed for bracketing only
  NodeKernel m addrNTN addrNTC blk ->
  StdGen ->
  Tracers m addrNTN blk e ->
  (NodeToNodeVersion -> Codecs blk addrNTN e m bCS bCS bBF bBF bTX bKA bPS bLN bLF) ->
  ByteLimits bCS bBF bTX bKA bPS bLN bLF ->
  -- Chain-Sync timeouts for chain-sync client (using `Header blk`) as well as
  -- the server (`SerialisedHeader blk`).
  (forall header. PeerTrustable -> ProtocolTimeLimitsWithRnd (ChainSync header (Point blk) (Tip blk))) ->
  CsClient.ChainSyncLoPBucketConfig ->
  CsClient.CSJConfig ->
  ReportPeerMetrics m (ConnectionId addrNTN) ->
  Handlers m addrNTN blk ->
  Apps m addrNTN bCS bBF bTX bKA bPS bLN bLF NodeToNodeInitiatorResult ()
mkApps kernel rng Tracers{..} mkCodecs ByteLimits{..} chainSyncTimeouts lopBucketConfig csjConfig ReportPeerMetrics{..} Handlers{..} =
  Apps{..}
 where
  (chainSyncRng, chainSyncRng') = splitGen rng
  NodeKernel{getDiffusionPipeliningSupport} = kernel

  aChainSyncClient ::
    NodeToNodeVersion ->
    ExpandedInitiatorContext addrNTN PeerTrustable m ->
    Channel m bCS ->
    m (NodeToNodeInitiatorResult, Maybe (Reception bCS))
  aChainSyncClient
    version
    ExpandedInitiatorContext
      { eicConnectionId = them
      , eicControlMessage = controlMessageSTM
      , eicIsBigLedgerPeer = isBigLedgerPeer
      , eicExtraFlags = peerTrustable
      }
    channel = do
      labelThisThread "ChainSyncClient"
      -- Note that it is crucial that we sync with the fetch client "outside"
      -- of registering the state for the sync client. This is needed to
      -- maintain a state invariant required by the block fetch logic: that for
      -- each candidate chain there is a corresponding block fetch client that
      -- can be used to fetch blocks for that chain.
      bracketSyncWithFetchClient
        (getFetchClientRegistry kernel)
        them
        $ CsClient.bracketChainSyncClient
          (contramap (TraceLabelPeer them) (Node.chainSyncClientTracer (getTracers kernel)))
          (contramap (TraceLabelPeer them) (Node.csjTracer (getTracers kernel)))
          (CsClient.defaultChainDbView (getChainDB kernel))
          (getChainSyncHandles kernel)
          (getGsmState kernel)
          them
          version
          lopBucketConfig
          csjConfig
          getDiffusionPipeliningSupport
        $ \csState -> do
          (r, trailing) <-
            runPipelinedPeerWithLimitsRnd
              (contramap (TraceLabelPeer them) tChainSyncTracer)
              chainSyncRng
              (cChainSyncCodec (mkCodecs version))
              blChainSync
              (chainSyncTimeouts peerTrustable)
              channel
              $ chainSyncClientPeerPipelined
              $ hChainSyncClient
                them
                isBigLedgerPeer
                CsClient.DynamicEnv
                  { CsClient.version
                  , CsClient.controlMessageSTM
                  , CsClient.headerMetricsTracer = TraceLabelPeer them `contramap` reportHeader
                  , CsClient.setCandidate = csvSetCandidate csState
                  , CsClient.idling = csvIdling csState
                  , CsClient.loPBucket = csvLoPBucket csState
                  , CsClient.setLatestSlot = csvSetLatestSlot csState
                  , CsClient.jumping = csvJumping csState
                  }
          return (ChainSyncInitiatorResult r, trailing)

  aChainSyncServer ::
    NodeToNodeVersion ->
    ResponderContext addrNTN ->
    Channel m bCS ->
    m ((), Maybe (Reception bCS))
  aChainSyncServer version ResponderContext{rcConnectionId = them} channel = do
    labelThisThread "ChainSyncServer"
    bracketWithPrivateRegistry
      ( chainSyncHeaderServerFollower
          (getChainDB kernel)
          ( case getDiffusionPipeliningSupport of
              DiffusionPipeliningOn -> ChainDB.TentativeChain
              DiffusionPipeliningOff -> ChainDB.SelectedChain
          )
      )
      ChainDB.followerClose
      $ \flr ->
        runPeerWithLimitsRnd
          (contramap (TraceLabelPeer them) tChainSyncSerialisedTracer)
          chainSyncRng'
          (cChainSyncCodecSerialised (mkCodecs version))
          blChainSync
          (chainSyncTimeouts IsNotTrustable)
          channel
          $ chainSyncServerPeer
          $ hChainSyncServer them version flr

  aBlockFetchClient ::
    NodeToNodeVersion ->
    ExpandedInitiatorContext addrNTN PeerTrustable m ->
    Channel m bBF ->
    m (NodeToNodeInitiatorResult, Maybe (Reception bBF))
  aBlockFetchClient
    version
    ExpandedInitiatorContext
      { eicConnectionId = them
      , eicControlMessage = controlMessageSTM
      }
    channel = do
      labelThisThread "BlockFetchClient"
      bracketFetchClient
        (getFetchClientRegistry kernel)
        version
        them
        $ \clientCtx -> do
          ((), trailing) <-
            runPipelinedPeerWithLimits
              (contramap (TraceLabelPeer them) tBlockFetchTracer)
              (cBlockFetchCodec (mkCodecs version))
              blBlockFetch
              timeLimitsBlockFetch
              channel
              $ hBlockFetchClient
                version
                controlMessageSTM
                (TraceLabelPeer them `contramap` reportFetch)
                clientCtx
          return (NoInitiatorResult, trailing)

  aBlockFetchServer ::
    NodeToNodeVersion ->
    ResponderContext addrNTN ->
    Channel m bBF ->
    m ((), Maybe (Reception bBF))
  aBlockFetchServer version ResponderContext{rcConnectionId = them} channel = do
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

  aTxSubmission2Client ::
    NodeToNodeVersion ->
    ExpandedInitiatorContext addrNTN PeerTrustable m ->
    Channel m bTX ->
    m (NodeToNodeInitiatorResult, Maybe (Reception bTX))
  aTxSubmission2Client
    version
    ExpandedInitiatorContext
      { eicConnectionId = them
      , eicControlMessage = controlMessageSTM
      }
    channel = do
      labelThisThread "TxSubmissionClient"
      ((), trailing) <-
        runPeerWithLimits
          (contramap (TraceLabelPeer them) tTxSubmission2Tracer)
          (cTxSubmission2Codec (mkCodecs version))
          blTxSubmission2
          timeLimitsTxSubmission2
          channel
          (txSubmissionClientPeer (hTxSubmissionClient version controlMessageSTM them))
      return (NoInitiatorResult, trailing)

  aTxSubmission2Server ::
    NodeToNodeVersion ->
    ResponderContext addrNTN ->
    Channel m bTX ->
    m ((), Maybe (Reception bTX))
  aTxSubmission2Server version ResponderContext{rcConnectionId = them} channel = do
    labelThisThread "TxSubmissionServer"

    let runServer serverApi =
          runPipelinedPeerWithLimits
            (contramap (TraceLabelPeer them) tTxSubmission2Tracer)
            (cTxSubmission2Codec (mkCodecs version))
            blTxSubmission2
            timeLimitsTxSubmission2
            channel
            (txSubmissionServerPeerPipelined serverApi)

    case hTxSubmissionServer version them of
      Left legacyTxSubmissionServer ->
        runServer legacyTxSubmissionServer
      Right newTxSubmissionServer ->
        withPeer
          (TraceLabelPeer them `contramap` tTxLogicTracer)
          (getTxChannelsVar kernel)
          (getTxMempoolSem kernel)
          defaultTxDecisionPolicy
          (getSharedTxStateVar kernel)
          ( mapTxSubmissionMempoolReader txForgetValidated $
              getMempoolReader (getMempool kernel)
          )
          (getMempoolWriter (getMempool kernel))
          txWireSize
          them
          $ \api ->
            runServer (newTxSubmissionServer api)

  aKeepAliveClient ::
    NodeToNodeVersion ->
    ExpandedInitiatorContext addrNTN PeerTrustable m ->
    Channel m bKA ->
    m (NodeToNodeInitiatorResult, Maybe (Reception bKA))
  aKeepAliveClient
    version
    ExpandedInitiatorContext
      { eicConnectionId = them
      , eicControlMessage = controlMessageSTM
      }
    channel = do
      labelThisThread "KeepAliveClient"
      let kacApp = \dqCtx ->
            runPeerWithLimits
              (TraceLabelPeer them `contramap` tKeepAliveTracer)
              (cKeepAliveCodec (mkCodecs version))
              blKeepAlive
              timeLimitsKeepAlive
              channel
              $ keepAliveClientPeer
              $ hKeepAliveClient
                version
                controlMessageSTM
                them
                dqCtx
                (KeepAliveInterval 10)

      ((), trailing) <- bracketKeepAliveClient (getFetchClientRegistry kernel) them kacApp
      return (NoInitiatorResult, trailing)

  aKeepAliveServer ::
    NodeToNodeVersion ->
    ResponderContext addrNTN ->
    Channel m bKA ->
    m ((), Maybe (Reception bKA))
  aKeepAliveServer version ResponderContext{rcConnectionId = them} channel = do
    labelThisThread "KeepAliveServer"
    runPeerWithLimits
      (TraceLabelPeer them `contramap` tKeepAliveTracer)
      (cKeepAliveCodec (mkCodecs version))
      blKeepAlive
      timeLimitsKeepAlive
      channel
      $ keepAliveServerPeer
      $ keepAliveServer

  aPeerSharingClient ::
    NodeToNodeVersion ->
    ExpandedInitiatorContext addrNTN PeerTrustable m ->
    Channel m bPS ->
    m (NodeToNodeInitiatorResult, Maybe (Reception bPS))
  aPeerSharingClient
    version
    ExpandedInitiatorContext
      { eicConnectionId = them
      , eicControlMessage = controlMessageSTM
      }
    channel = do
      labelThisThread "PeerSharingClient"
      bracketPeerSharingClient (getPeerSharingRegistry kernel) (remoteAddress them) $
        \controller -> do
          psClient <- hPeerSharingClient version controlMessageSTM them controller
          ((), trailing) <-
            runPeerWithLimits
              (TraceLabelPeer them `contramap` tPeerSharingTracer)
              (cPeerSharingCodec (mkCodecs version))
              blPeerSharing
              timeLimitsPeerSharing
              channel
              (peerSharingClientPeer psClient)
          return (NoInitiatorResult, trailing)

  aPeerSharingServer ::
    NodeToNodeVersion ->
    ResponderContext addrNTN ->
    Channel m bPS ->
    m ((), Maybe (Reception bPS))
  aPeerSharingServer version ResponderContext{rcConnectionId = them} channel = do
    labelThisThread "PeerSharingServer"
    runPeerWithLimits
      (TraceLabelPeer them `contramap` tPeerSharingTracer)
      (cPeerSharingCodec (mkCodecs version))
      blPeerSharing
      timeLimitsPeerSharing
      channel
      $ peerSharingServerPeer
      $ hPeerSharingServer version them

  -- | Owns the per-peer 'LeiosPeerVars' entry in 'getLeiosPeersVars': allocates
  -- and registers it on connect and, on every exit path, unregisters it and
  -- refunds that peer's outstanding fetch requests. Modelled on
  -- 'bracketFetchClient'. This bracket is the sole writer of the
  -- 'getLeiosPeersVars' map itself; the LeiosNotify client (which fills in the
  -- entry's 'offerings') runs inside it, while the LeiosFetch client and the
  -- fetch decision loop read the entry and write its 'requestsToSend'.
  --
  -- The unregister's 'removePeerFromOutstanding' (which takes
  -- 'getLeiosOutstanding') is safe against the fetch decision loop: that loop
  -- re-reads the live peers *inside* its own 'getLeiosOutstanding' critical
  -- section and only increments those, so the refund is serialised with the
  -- increment. Since this unregister deletes the 'getLeiosPeersVars' entry
  -- before refunding, the loop either still sees the peer (and the refund runs
  -- afterwards, cancelling the increment) or no longer sees it (and never
  -- increments it) -- so it cannot re-insert a departed peer's accounting after
  -- teardown has refunded it.
  bracketLeiosPeer ::
    ConnectionId addrNTN ->
    (Leios.LeiosPeerVars m -> m a) ->
    m a
  bracketLeiosPeer them =
    bracket
      ( do
          peerVars <- Leios.newLeiosPeerVars
          MVar.modifyMVar_ (getLeiosPeersVars kernel) $
            pure . Map.insert pid peerVars
          pure peerVars
      )
      ( \_peerVars -> do
          MVar.modifyMVar_ (getLeiosPeersVars kernel) $ pure . Map.delete pid
          MVar.modifyMVar_ (getLeiosOutstanding kernel) $
            pure . Leios.removePeerFromOutstanding pid
      )
   where
    pid = Leios.MkPeerId them

  aLeiosNotifyClient ::
    NodeToNodeVersion ->
    ExpandedInitiatorContext addrNTN PeerTrustable m ->
    Channel m bLN ->
    m (NodeToNodeInitiatorResult, Maybe (Reception bLN))
  aLeiosNotifyClient
    version
    ExpandedInitiatorContext
      { eicConnectionId = them
      , eicControlMessage = controlMessageSTM
      }
    channel = do
      labelThisThread "LeiosNotifyClient"
      bracketLeiosPeer them $ \peerVars -> do
        ((), trailing) <-
          runPipelinedPeerWithLimits
            (TraceLabelPeer them `contramap` tLeiosNotifyTracer)
            (cLeiosNotifyCodec (mkCodecs version))
            blLeiosNotify
            timeLimitsLeiosNotify
            channel
            $ hLeiosNotifyClient version controlMessageSTM them peerVars
        pure (NoInitiatorResult, trailing)

  aLeiosNotifyServer ::
    NodeToNodeVersion ->
    ResponderContext addrNTN ->
    Channel m bLN ->
    m ((), Maybe (Reception bLN))
  aLeiosNotifyServer version ResponderContext{rcConnectionId = them} channel = do
    labelThisThread "LeiosNotifyServer"
    runPeerWithLimits
      (TraceLabelPeer them `contramap` tLeiosNotifyTracer)
      (cLeiosNotifyCodec (mkCodecs version))
      blLeiosNotify
      timeLimitsLeiosNotify
      channel
      $ hLeiosNotifyServer version them

  aLeiosFetchClient ::
    NodeToNodeVersion ->
    ExpandedInitiatorContext addrNTN PeerTrustable m ->
    Channel m bLF ->
    m (NodeToNodeInitiatorResult, Maybe (Reception bLF))
  aLeiosFetchClient
    version
    ExpandedInitiatorContext
      { eicConnectionId = them
      , eicControlMessage = controlMessageSTM
      }
    channel = do
      labelThisThread "LeiosFetchClient"
      ((), trailing) <-
        runPipelinedPeerWithLimits
          (TraceLabelPeer them `contramap` tLeiosFetchTracer)
          (cLeiosFetchCodec (mkCodecs version))
          blLeiosFetch
          timeLimitsLeiosFetch
          channel
          $ hLeiosFetchClient version controlMessageSTM them
      pure (NoInitiatorResult, trailing)

  aLeiosFetchServer ::
    NodeToNodeVersion ->
    ResponderContext addrNTN ->
    Channel m bLF ->
    m ((), Maybe (Reception bLF))
  aLeiosFetchServer version ResponderContext{rcConnectionId = them} channel = do
    labelThisThread "LeiosFetchServer"
    runPeerWithLimits
      (TraceLabelPeer them `contramap` tLeiosFetchTracer)
      (cLeiosFetchCodec (mkCodecs version))
      blLeiosFetch
      timeLimitsLeiosFetch
      channel
      $ hLeiosFetchServer version them

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
  MiniProtocolParameters ->
  NodeToNodeVersion ->
  NodeToNodeVersionData ->
  Apps m addr b b b b b b b a c ->
  OuroborosBundleWithExpandedCtx 'Mux.InitiatorMode addr PeerTrustable b m a Void
initiator miniProtocolParameters version versionData Apps{..} =
  nodeToNodeProtocols
    miniProtocolParameters
    -- TODO: currently consensus is using 'ConnectionId' for its 'peer' type.
    -- This is currently ok, as we might accept multiple connections from the
    -- same ip address, however this will change when we will switch to
    -- p2p-governor & connection-manager.  Then consensus can use peer's ip
    -- address & port number, rather than 'ConnectionId' (which is
    -- a quadruple uniquely determining a connection).
    ( NodeToNodeProtocols
        { chainSyncProtocol =
            (InitiatorProtocolOnly (MiniProtocolCb (\ctx -> aChainSyncClient version ctx)))
        , blockFetchProtocol =
            (InitiatorProtocolOnly (MiniProtocolCb (\ctx -> aBlockFetchClient version ctx)))
        , txSubmissionProtocol =
            (InitiatorProtocolOnly (MiniProtocolCb (\ctx -> aTxSubmission2Client version ctx)))
        , keepAliveProtocol =
            (InitiatorProtocolOnly (MiniProtocolCb (\ctx -> aKeepAliveClient version ctx)))
        , peerSharingProtocol =
            (InitiatorProtocolOnly (MiniProtocolCb (\ctx -> aPeerSharingClient version ctx)))
        }
    )
    version
    versionData
    <> mempty
      { withHot =
          WithHot
            [ MiniProtocol
                { miniProtocolNum = leiosNotifyMiniProtocolNum
                , miniProtocolStart = StartOnDemand
                , miniProtocolLimits = leiosNotifyProtocolLimits
                , miniProtocolRun =
                    InitiatorProtocolOnly
                      (MiniProtocolCb (\initiatorCtx -> aLeiosNotifyClient version initiatorCtx))
                }
            , MiniProtocol
                { miniProtocolNum = leiosFetchMiniProtocolNum
                , miniProtocolStart = StartOnDemand
                , miniProtocolLimits = leiosFetchProtocolLimits
                , miniProtocolRun =
                    InitiatorProtocolOnly
                      (MiniProtocolCb (\initiatorCtx -> aLeiosFetchClient version initiatorCtx))
                }
            ]
      }

-- | A bi-directional network application.
--
-- Implementation note: network currently doesn't enable protocols conditional
-- on the protocol version, but it eventually may; this is why @_version@ is
-- currently unused.
initiatorAndResponder ::
  MiniProtocolParameters ->
  NodeToNodeVersion ->
  NodeToNodeVersionData ->
  Apps m addr b b b b b b b a c ->
  OuroborosBundleWithExpandedCtx 'Mux.InitiatorResponderMode addr PeerTrustable b m a c
initiatorAndResponder miniProtocolParameters version versionData Apps{..} =
  nodeToNodeProtocols
    miniProtocolParameters
    ( NodeToNodeProtocols
        { chainSyncProtocol =
            ( InitiatorAndResponderProtocol
                (MiniProtocolCb (\initiatorCtx -> aChainSyncClient version initiatorCtx))
                (MiniProtocolCb (\responderCtx -> aChainSyncServer version responderCtx))
            )
        , blockFetchProtocol =
            ( InitiatorAndResponderProtocol
                (MiniProtocolCb (\initiatorCtx -> aBlockFetchClient version initiatorCtx))
                (MiniProtocolCb (\responderCtx -> aBlockFetchServer version responderCtx))
            )
        , txSubmissionProtocol =
            ( InitiatorAndResponderProtocol
                (MiniProtocolCb (\initiatorCtx -> aTxSubmission2Client version initiatorCtx))
                (MiniProtocolCb (\responderCtx -> aTxSubmission2Server version responderCtx))
            )
        , keepAliveProtocol =
            ( InitiatorAndResponderProtocol
                (MiniProtocolCb (\initiatorCtx -> aKeepAliveClient version initiatorCtx))
                (MiniProtocolCb (\responderCtx -> aKeepAliveServer version responderCtx))
            )
        , peerSharingProtocol =
            ( InitiatorAndResponderProtocol
                (MiniProtocolCb (\initiatorCtx -> aPeerSharingClient version initiatorCtx))
                (MiniProtocolCb (\responderCtx -> aPeerSharingServer version responderCtx))
            )
        }
    )
    version
    versionData
    <> mempty
      { withHot =
          WithHot
            [ MiniProtocol
                { miniProtocolNum = leiosNotifyMiniProtocolNum
                , miniProtocolStart = StartOnDemand
                , miniProtocolLimits = leiosNotifyProtocolLimits
                , miniProtocolRun =
                    InitiatorAndResponderProtocol
                      (MiniProtocolCb (\initiatorCtx -> aLeiosNotifyClient version initiatorCtx))
                      (MiniProtocolCb (\responderCtx -> aLeiosNotifyServer version responderCtx))
                }
            , MiniProtocol
                { miniProtocolNum = leiosFetchMiniProtocolNum
                , miniProtocolStart = StartOnDemand
                , miniProtocolLimits = leiosFetchProtocolLimits
                , miniProtocolRun =
                    InitiatorAndResponderProtocol
                      (MiniProtocolCb (\initiatorCtx -> aLeiosFetchClient version initiatorCtx))
                      (MiniProtocolCb (\responderCtx -> aLeiosFetchServer version responderCtx))
                }
            ]
      }

leiosNotifyProtocolLimits :: MiniProtocolLimits
leiosNotifyProtocolLimits =
  MiniProtocolLimits
    { maximumIngressQueue =
        addSafetyMargin $
          fromIntegral $
            Leios.maxLeiosNotifyIngressQueue Leios.demoLeiosFetchStaticEnv
    }

leiosFetchProtocolLimits :: MiniProtocolLimits
leiosFetchProtocolLimits =
  MiniProtocolLimits
    { maximumIngressQueue =
        addSafetyMargin $
          fromIntegral $
            Leios.maxLeiosFetchIngressQueue Leios.demoLeiosFetchStaticEnv
    }
