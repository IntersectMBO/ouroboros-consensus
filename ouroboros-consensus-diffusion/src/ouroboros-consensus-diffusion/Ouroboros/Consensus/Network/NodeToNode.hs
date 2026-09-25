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
  , leiosFetchPipelineDepth
  , leiosNotifyPipelineDepth
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
import qualified Control.Concurrent.Class.MonadSTM as LazySTM
import Control.Concurrent.Class.MonadSTM.Strict (readTChan)
import qualified Control.Concurrent.Class.MonadSTM.Strict.TVar as TVar.Unchecked
import Control.DeepSeq (NFData)
import Control.Monad (forM_, forever, when)
import Control.Monad.Class.MonadTime.SI (MonadTime)
import Control.Monad.Class.MonadTimer.SI (MonadTimer)
import Control.Monad.Except (runExceptT)
import Control.ResourceRegistry
import Control.Tracer
import Data.ByteString.Lazy (ByteString)
import Data.Functor ((<&>))
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (SJust))
import qualified Data.Primitive.MutVar as Prim
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Void (Void)
import LeiosDemoDb
  ( LeiosDbHandle (subscribeEbNotifications)
  , LeiosDbReader
  , LeiosDbWriter
  , LeiosEbNotification (..)
  , withReader
  , withWriter
  )
import qualified LeiosDemoLogic as Leios
import qualified LeiosDemoLogic.Announcements as Announcements
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
  , LeiosNotifyServerPeerLookahead
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
  , leiosNotifyServerPeerLookahead
  , runLookaheadFixedSenderPeerWithLimits
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
  ( AddVoteResult (..)
  , LeiosVoteState (..)
  , LeiosVoteSubscription (..)
  , VoteTally (..)
  , subscribeVotes
  )
import qualified Network.Mux as Mux
import Network.TypedProtocol.Codec
import Network.TypedProtocol.Peer (Peer (Effect))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime (getCurrentSlot)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( diffRelTime
  , systemTimeCurrent
  )
import Ouroboros.Consensus.Config (DiffusionPipeliningSupport (..))
import Ouroboros.Consensus.HeaderValidation (HeaderWithTime)
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Mempool.API (getLeiosTxIndex)
import qualified Ouroboros.Consensus.MiniProtocol.BlockFetch.ClientInterface as BlockFetchClientInterface
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
import Ouroboros.Consensus.Storage.LedgerDB.Forker
  ( ResolveLeiosBlock
  , leiosTxBytesOfGenTx
  )
import Ouroboros.Consensus.Storage.Serialisation (SerialisedHeader)
import Ouroboros.Consensus.Util (ShowProxy, whenJust)
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
import Ouroboros.Network.Tx (HasRawTxId)
import Ouroboros.Network.TxSubmission.Inbound.V1
import Ouroboros.Network.TxSubmission.Inbound.V2
  ( PeerTxAPI
  , TraceTxLogic
  , TxDecisionPolicy (..)
  , TxSubmissionLogicVersion (..)
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
      Leios.LeiosPeerVars m ->
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
      BlockFetchClient (HeaderWithTime blk) blk (BlockFetchClientInterface.MatchedBlock blk) m ()
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
      LeiosNotifyClientPeerPipelined LeiosPoint (Header blk) LeiosVote m ()
  , hLeiosNotifyServer ::
      NodeToNodeVersion ->
      ConnectionId addr ->
      m
        ( LeiosNotifyServerPeerLookahead LeiosPoint (Header blk) LeiosVote m ()
        , m Void
        )
  , hLeiosFetchClient ::
      LeiosDbWriter m ->
      NodeToNodeVersion ->
      ControlMessageSTM m ->
      ConnectionId addr ->
      Leios.LeiosPeerVars m ->
      LeiosFetchClientPeerPipelined LeiosPoint LeiosEb LeiosTx m ()
  , hLeiosFetchServer ::
      LeiosDbReader m ->
      NodeToNodeVersion ->
      ConnectionId addr ->
      LeiosFetchServerPeer LeiosPoint LeiosEb LeiosTx m ()
  }

-- | One peer's outgoing LeiosNotify queue, and what it has been announced.
--
-- The two travel together so that the pure step the central relay logic uses
-- to enqueue an announcement is also what records it; see 'qav'.
data OutgoingLeiosNotify blk = MkOutgoingLeiosNotify
  { olnMessages ::
      !( Seq.Seq
           ( LeiosDemoOnlyTestNotify.Message
               (LeiosNotify LeiosPoint (Header blk) LeiosVote)
               LeiosDemoOnlyTestNotify.StBusy
               LeiosDemoOnlyTestNotify.StIdle
           )
       )
  , olnAnnounced :: !(Set.Set LeiosPoint)
  -- ^ The endorser blocks whose announcement we have enqueued to this peer,
  -- pruned to our immutable tip; see 'offer'.
  }

emptyOutgoingLeiosNotify :: OutgoingLeiosNotify blk
emptyOutgoingLeiosNotify = MkOutgoingLeiosNotify Seq.empty Set.empty

mkHandlers ::
  forall m blk addrNTN addrNTC.
  ( IOLike m
  , MonadTime m
  , MonadTimer m
  , ConvertRawHash blk
  , LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  , LedgerSupportsProtocol blk
  , ResolveLeiosBlock blk
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
    , systemTime
    }
  nodeKernel@NodeKernel
    { getChainDB
    , getMempool
    , getTopLevelConfig
    , getTracers = tracers
    , getPeerSharingAPI
    , getGsmState
    , getBlockchainTime
    }
  txSubmissionLogicVersion =
    Handlers
      { hChainSyncClient = \peer _isBigLedgerpeer dynEnv peerVars ->
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
              , CsClient.leiosMsgRollForwardCallback = \hdr hdrSlotTime cds -> do
                  Leios.checkMsgRollForwardForLeiosOffers
                    (getLeiosOutstanding, getLeiosReady)
                    peerVars
                    hdr
                    cds
                  -- Feed any EB this header announces into the central
                  -- announcement state (relay + dedup + txCache), central-only:
                  -- a roll-forward is not this peer announcing over LeiosNotify,
                  -- so no PeerState is touched. Date it from the header slot's
                  -- onset (its ChainSync arrival latency).
                  whenJust (Leios.mkAnnouncingHeader hdr) $ \ancHdr -> do
                    now <- systemTimeCurrent systemTime
                    Leios.processAnnouncementCentrally
                      (Node.leiosKernelTracer tracers)
                      getLeiosCentralState
                      (getLeiosOutstanding, getLeiosReady)
                      getLeiosTxCache
                      (Just peer)
                      Leios.ReceivedViaChainSync
                      Announcements.DoRelay
                      (SJust hdrSlotTime)
                      (Just (diffRelTime now hdrSlotTime))
                      ancHdr
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
                  (txDecisionPolicy miniProtocolParameters)
                  (getMempoolWriter getMempool)
                  txWireSize
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
          -- Per-upstream-peer announcement accountability (dedup and
          -- equivocation counting). The pipelined-client handler is a stateless
          -- callback, so this state lives in a (single-threaded) ref.
          peerStateVar <- Prim.newMutVar Leios.emptyLeiosNotifyPeerState
          -- Run one of the offer checks, committing what it accepted or
          -- disconnecting the peer.
          --
          -- Pruning first is what keeps the too-old bound on the immutable tip
          -- itself rather than on wherever this peer's last announcement left
          -- it. It cannot strand a legal offer: the announcements a prune
          -- drops are those of elections below the immutable tip, an announced
          -- point's slot is its election's slot, and so an offer relying on one
          -- is rejected as too old by the very tip that dropped it.
          let checkOffer ::
                ( Leios.LeiosNotifyPeerState blk ->
                  Either Leios.ExnLeiosInvalidOffer (Leios.LeiosNotifyPeerState blk)
                ) ->
                m ()
              checkOffer runCheck = do
                immLedger <- atomically $ ChainDB.getImmutableLedger getChainDB
                peerSt <-
                  Leios.pruneLeiosNotifyPeerStateToImmTip immLedger
                    <$> Prim.readMutVar peerStateVar
                case runCheck peerSt of
                  Left err -> throwIO err
                  Right peerSt' -> Prim.writeMutVar peerStateVar peerSt'
          pure $
            leiosNotifyClientPeerPipelined
              ( atomically $
                  controlMessageSTM >>= \case
                    Terminate -> pure (Left ())
                    _ -> do
                      -- Gate on the immutable tip being able to forecast to the
                      -- current wall clock.
                      Leios.awaitImmTipCanForecastNow
                        getTopLevelConfig
                        (ChainDB.getImmutableLedger getChainDB)
                        (getCurrentSlot getBlockchainTime)
                      pure $ Right leiosNotifyPipelineDepth
              )
              ( pure $ \case
                  MsgLeiosBlockAnnouncement hdr -> do
                    -- A header relayed as an announcement must carry one;
                    -- disconnect the peer otherwise.
                    anc <- case Leios.mkAnnouncingHeader hdr of
                      Nothing -> throwIO Leios.ExnLeiosBlockAnnouncementMissing
                      Just x -> pure x
                    immLedger <- atomically $ ChainDB.getImmutableLedger getChainDB
                    peerSt0 <- Prim.readMutVar peerStateVar
                    res <-
                      runExceptT $
                        Announcements.onAnnouncement
                          (contramap Leios.tracePeerAnnouncement tracer)
                          Leios.ancElId
                          ( \ancH ->
                              Leios.announcementValidity
                                systemTime
                                chainSyncFutureCheck
                                getTopLevelConfig
                                immLedger
                                (Leios.ancHeader ancH)
                          )
                          -- central part of the processing
                          ( \ancHdr (shouldRelay, onset, age, (p, _sz)) -> do
                              traceWith tracer $
                                MkTraceLeiosPeer $
                                  "MsgLeiosBlockAnnouncement new: " <> Leios.prettyLeiosPoint p
                              Leios.processAnnouncementCentrally
                                kernelTracer
                                getLeiosCentralState
                                (getLeiosOutstanding, getLeiosReady)
                                getLeiosTxCache
                                (Just peer)
                                Leios.ReceivedViaLeiosNotify
                                shouldRelay
                                (SJust onset)
                                (Just age)
                                ancHdr
                          )
                          (Leios.lnpsAnnouncements peerSt0)
                          anc
                    announcements <- case res of
                      Left err -> throwIO $ Leios.ReactToAnnouncementError err
                      Right x -> pure x
                    Prim.writeMutVar peerStateVar $!
                      Leios.pruneLeiosNotifyPeerStateToImmTip
                        immLedger
                        peerSt0{Leios.lnpsAnnouncements = announcements}
                  MsgLeiosBlockOffer point ebBytesSize -> do
                    traceWith tracer $ MkTraceLeiosPeer $ "MsgLeiosBlockOffer " <> Leios.prettyLeiosPoint point
                    checkOffer $ Leios.checkLeiosBlockOffer point ebBytesSize
                    Leios.recordEbBodyOffer getLeiosReady peerVars (point, ebBytesSize)
                  MsgLeiosBlockTxsOffer point -> do
                    traceWith tracer $ MkTraceLeiosPeer $ "MsgLeiosBlockTxsOffer " <> Leios.prettyLeiosPoint point
                    checkOffer $ Leios.checkLeiosClosureOffer point
                    Leios.recordEbClosureOffer getLeiosReady peerVars point
                  MsgLeiosVotes vs -> do
                    -- TODO no LeiosNotify message may simply be ignored, or
                    -- the peer can send it without bound. Votes still can be:
                    -- 'addVote' silently returns 'AlreadyKnown' for a
                    -- duplicate, and says nothing at all about a vote for an
                    -- election we have no interest in. They want the same
                    -- shape the announcements have:
                    --
                    --  * the age limits announcements use, a sending bound
                    --    comfortably below a receiving bound, so that an
                    --    honest relayer never trips ours;
                    --
                    --  * a bitfield per /election/ per peer --- not per
                    --    announcement, since the committee is seated by the
                    --    election's slot --- so that a peer that votes twice
                    --    as one committee member is disconnected;
                    --
                    --  * the same bitfield per election centrally, so that a
                    --    vote we have already seen from elsewhere, or one
                    --    that equivocates, is silently not relayed rather
                    --    than relayed on to everyone.
                    --
                    -- No peer-level trace here: 'TraceLeiosVoteAcquired' below
                    -- reports every vote with structured fields, and votes are
                    -- the one Leios message whose count scales with committee
                    -- size, so rendering each one cost more than the vote.
                    forM_ vs $ \vote -> do
                      result <- addVote vote
                      -- Traced on 'Added' only, so one line per distinct vote
                      -- rather than per arrival: the same vote reaches us from
                      -- every peer that has it, and those duplicates return
                      -- 'AlreadyKnown' with no tally to report.
                      --
                      -- A remote vote can be the one that tips this node's
                      -- tally past the threshold, so certification is traced
                      -- whenever 'addVote' surfaces a cert for the point.
                      case result of
                        Added VoteTally{vtWeight, vtTally, vtThreshold} mCert -> do
                          traceWith kernelTracer $
                            TraceLeiosVoteAcquired
                              { vote
                              , weight = vtWeight
                              , tally = vtTally
                              , threshold = vtThreshold
                              }
                          case mCert of
                            Just _ ->
                              traceWith kernelTracer TraceLeiosCertified{rbHash = Leios.announcingRbHash vote}
                            Nothing -> pure ()
                        _ -> pure ()
              )
      , hLeiosNotifyServer = \_version peer -> do
          chan <- subscribeEbNotifications leiosDB
          LeiosVoteSubscription{getNextVote} <- subscribeVotes leiosVoteState

          -- This peer's single outgoing LeiosNotify queue and its credit
          -- counter, shared by all three sources (announcements, offers, votes)
          -- and served strictly FIFO. The node-wide relay logic
          -- ('onAnnouncementCentral') appends announcements through the
          -- 'QueueAnnouncementView' we register below; offers and votes are
          -- moved in by the 'pump'.
          --
          -- TODO the EB-offer and vote sources should eventually /register/
          -- this peer with those components too, rather than the pump draining
          -- fresh per-peer subscriptions.
          credits <- TVar.Unchecked.newTVarIO (0 :: Int)
          queue <- TVar.Unchecked.newTVarIO emptyOutgoingLeiosNotify

          -- The view the central relay logic uses to enqueue announcements.
          --
          -- It also records what it announced, which is what lets an offer be
          -- held back until this peer has been told the endorser block exists:
          -- we require exactly that of our own upstream peers, so offering one
          -- we never announced would have an honest peer disconnect us. The
          -- record is written here, by the same pure step that enqueues, so it
          -- cannot claim an announcement that never went out --- in particular
          -- the central logic drops the announcement outright when this peer
          -- has no credits, and then this does not record it.
          let qav =
                Announcements.MkQueueAnnouncementView
                  credits
                  ( \out anc ->
                      let fields = Leios.ancAnnouncementFields anc
                       in MkOutgoingLeiosNotify
                            { olnMessages =
                                olnMessages out
                                  Seq.|> MsgLeiosBlockAnnouncement (Leios.ancHeader anc)
                            , olnAnnounced =
                                Set.insert
                                  (Leios.announcementLeiosPoint fields)
                                  (olnAnnounced out)
                            }
                  )
                  queue

          let
            -- 'incr' adds a credit per received request; 'next' hands the
            -- sender thread the next queued message (FIFO across all three
            -- sources); 'pump' moves offers/votes into the queue, dropping a
            -- message when there are no free credits.
            incr = atomically $ do
              n <- TVar.Unchecked.readTVar credits
              if n == leiosNotifyPipelineDepth
                then pure LeiosDemoOnlyTestNotify.ExcessiveRequests
                else do
                  TVar.Unchecked.writeTVar credits $! n + 1
                  pure LeiosDemoOnlyTestNotify.NotExcessiveRequests
            next = atomically $ do
              out <- TVar.Unchecked.readTVar queue
              case Seq.viewl (olnMessages out) of
                Seq.EmptyL -> LazySTM.retry
                msg Seq.:< msgs' -> do
                  TVar.Unchecked.writeTVar queue out{olnMessages = msgs'}
                  pure msg

            -- 'Nothing' when the notification it consumed is one we must not
            -- relay (eg because it'd be too old): the caller commits that
            -- anyway, so the channel drains.
            pumpNext ::
              STM
                m
                ( Maybe
                    ( LeiosDemoOnlyTestNotify.Message
                        (LeiosNotify LeiosPoint (Header blk) LeiosVote)
                        LeiosDemoOnlyTestNotify.StBusy
                        LeiosDemoOnlyTestNotify.StIdle
                    )
                )
            pumpNext = do
              pruneAnnounced
              ( readTChan chan >>= \case
                  AcquiredEb point ebSize ->
                    offer point $ MsgLeiosBlockOffer point ebSize
                  AcquiredEbTxs point ->
                    offer point $ MsgLeiosBlockTxsOffer point
                )
                <|> (getNextVote <&> \vote -> Just $ MsgLeiosVotes [vote])

            -- Keep 'olnAnnounced' to our immutable tip, so it does not grow for
            -- the life of the connection. Nothing below that is ever offered
            -- anyway, since the relay decision holds offers back to
            -- 'leiosMinOfferLead' above it.
            --
            -- Runs for every message the pump produces --- offers, suppressed
            -- notifications and votes alike --- rather than only for the ones
            -- that consult the record, since it is announcements that grow it
            -- and a peer can be relayed those without our acquiring anything.
            pruneAnnounced = do
              immTipSlot <- withOrigin (SlotNo 0) id <$> getImmTipSlot nodeKernel
              out <- TVar.Unchecked.readTVar queue
              TVar.Unchecked.writeTVar queue $
                out
                  { olnAnnounced =
                      Set.dropWhileAntitone
                        ((< immTipSlot) . Leios.pointSlotNo)
                        (olnAnnounced out)
                  }

            -- An offer goes out only if it is fresh enough to relay and this
            -- peer has been told the announcement it is for.
            relayDecision =
              Leios.leiosOfferRelayDecision
                (getLeiosMinOfferLead nodeKernel)
                (getImmTipSlot nodeKernel)
            offer point msg = do
              shouldRelay <- relayDecision (Leios.pointSlotNo point)
              case shouldRelay of
                Announcements.DoNotRelay -> pure Nothing
                Announcements.DoRelay -> do
                  out <- TVar.Unchecked.readTVar queue
                  pure $ if Set.member point (olnAnnounced out) then Just msg else Nothing

            pump =
              ( do
                  MVar.modifyMVar_ getLeiosCentralState $
                    pure . Announcements.insertPeerCentral peer qav
                  forever $ atomically $ do
                    mMsg <- pumpNext
                    c <- TVar.Unchecked.readTVar credits
                    -- FIXME: Is this dropping messages when we run out of credits?
                    forM_ mMsg $ \msg ->
                      when (c > 0) $ do
                        TVar.Unchecked.writeTVar credits $! c - 1
                        out <- TVar.Unchecked.readTVar queue
                        TVar.Unchecked.writeTVar
                          queue
                          out{olnMessages = olnMessages out Seq.|> msg}
              )
                `finally` ( MVar.modifyMVar_ getLeiosCentralState $
                              pure . Announcements.deletePeerCentral peer
                          )

          pure (leiosNotifyServerPeerLookahead incr next, pump)
      , hLeiosFetchClient = \writer _version controlMessageSTM peer peerVars -> toLeiosFetchClientPeerPipelined $ Effect $ do
          let reqVar = Leios.requestsToSend peerVars
          pure $
            ( leiosFetchClientPeerPipelined leiosFetchPipelineDepth $
                Leios.nextLeiosFetchClientCommand
                  (Node.leiosKernelTracer tracers)
                  (leiosPeerTracer peer)
                  ((== Terminate) <$> controlMessageSTM)
                  (getLeiosOutstanding, getLeiosReady)
                  getLeiosTxCache
                  writer
                  systemTime
                  ( Leios.mkMempoolPull
                      (atomically (getLeiosTxIndex getMempool))
                      (leiosTxBytesOfGenTx . txForgetValidated)
                  )
                  (Leios.MkPeerId peer)
                  reqVar
            )
      , hLeiosFetchServer = \reader _version peer -> Effect $ do
          leiosFetchContext <- Leios.newLeiosFetchContext reader
          pure $
            leiosFetchServerPeer
              (pure $ Leios.leiosFetchHandler (leiosPeerTracer peer) leiosFetchContext)
      }
   where
    NodeKernel
      { getLeiosDB = leiosDB
      , getLeiosVoteState = leiosVoteState
      , getLeiosOutstanding
      , getLeiosReady
      , getLeiosCentralState
      , getLeiosTxCache
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
  , cLeiosNotifyCodec :: Codec (LeiosNotify LeiosPoint (Header blk) LeiosVote) e m bLN
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
          enc
          dec
          Leios.encodeLeiosVote
          Leios.decodeLeiosVote
    , cLeiosFetchCodec =
        codecLeiosFetch
          Leios.maxTxsPerEb
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
    (AnyMessage (LeiosNotify LeiosPoint (Header blk) LeiosVote))
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
      f (TraceLabelPeer peer (TraceSendRecv (LeiosNotify LeiosPoint (Header blk) LeiosVote)))
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
  , HasRawTxId (GenTxId blk)
  , HasHeader blk
  , HasNestedContent Header blk
  , Monad m
  ) =>
  Tracer m String -> Tracers m ntnAddr blk e
showTracers tr =
  Tracers
    { tChainSyncTracer = show >$< tr
    , tChainSyncSerialisedTracer = show >$< tr
    , tBlockFetchTracer = show >$< tr
    , tBlockFetchSerialisedTracer = show >$< tr
    , tTxSubmission2Tracer = show >$< tr
    , tKeepAliveTracer = show >$< tr
    , tPeerSharingTracer = show >$< tr
    , tTxLogicTracer = show >$< tr
    , tLeiosNotifyTracer = show >$< tr
    , tLeiosFetchTracer = show >$< tr
    }

{-------------------------------------------------------------------------------
  Applications
-------------------------------------------------------------------------------}

-- | A node-to-node application
type ClientApp m addr bytes a =
  NodeToNodeVersion ->
  ExpandedInitiatorContext addr PeerTrustable m ->
  Channel m bytes ->
  m (a, Maybe bytes)

type ServerApp m addr bytes a =
  NodeToNodeVersion ->
  ResponderContext addr ->
  Channel m bytes ->
  m (a, Maybe bytes)

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
  , HasRawTxId (GenTxId blk)
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
mkApps kernel rng Tracers{tTxLogicTracer = _, ..} mkCodecs ByteLimits{..} chainSyncTimeouts lopBucketConfig csjConfig ReportPeerMetrics{..} Handlers{..} =
  Apps{..}
 where
  (chainSyncRng, chainSyncRng') = splitGen rng
  NodeKernel{getDiffusionPipeliningSupport, getLeiosDB = leiosDB} = kernel

  aChainSyncClient ::
    NodeToNodeVersion ->
    ExpandedInitiatorContext addrNTN PeerTrustable m ->
    Channel m bCS ->
    m (NodeToNodeInitiatorResult, Maybe bCS)
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
        $ \csState ->
          bracketLeiosPeer them isBigLedgerPeer $ \peerVars -> do
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
                  peerVars
            return (ChainSyncInitiatorResult r, trailing)

  aChainSyncServer ::
    NodeToNodeVersion ->
    ResponderContext addrNTN ->
    Channel m bCS ->
    m ((), Maybe bCS)
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
    m (NodeToNodeInitiatorResult, Maybe bBF)
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
        (getKeepAliveRegistry kernel)
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
    m ((), Maybe bBF)
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
    m (NodeToNodeInitiatorResult, Maybe bTX)
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
    m ((), Maybe bTX)
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
          (getTxDecisionPolicy kernel)
          ( mapTxSubmissionMempoolReader txForgetValidated $
              getMempoolReader (getMempool kernel)
          )
          (getSharedTxStateVar kernel)
          (getPeerTxRegistry kernel)
          (getTxCountersVar kernel)
          them
          $ \api ->
            runServer (newTxSubmissionServer api)

  aKeepAliveClient ::
    NodeToNodeVersion ->
    ExpandedInitiatorContext addrNTN PeerTrustable m ->
    Channel m bKA ->
    m (NodeToNodeInitiatorResult, Maybe bKA)
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

      ((), trailing) <- bracketKeepAliveClient (getKeepAliveRegistry kernel) them kacApp
      return (NoInitiatorResult, trailing)

  aKeepAliveServer ::
    NodeToNodeVersion ->
    ResponderContext addrNTN ->
    Channel m bKA ->
    m ((), Maybe bKA)
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
    m (NodeToNodeInitiatorResult, Maybe bPS)
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
    m ((), Maybe bPS)
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

  -- \| Bracket owning this connection's shared per-peer 'LeiosPeerVars' entry in
  -- 'getLeiosPeersVars'. Every mini-protocol that needs the peer vars (ChainSync,
  -- LeiosNotify, LeiosFetch) wraps itself in this. On entry it get-or-creates the
  -- entry -- the first to run allocates and registers it, the rest share it -- and
  -- on exit it unregisters the entry and refunds that peer's outstanding fetch
  -- requests ('removePeerFromOutstanding'). There is no ref count: a hot peer's
  -- mini-protocols tear down together, so whichever exits first runs the cleanup
  -- and the rest are idempotent no-ops; a straggler that allocated its own entry
  -- cleans it up on its own exit. Modelled on 'bracketFetchClient'.
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
    IsBigLedgerPeer ->
    (Leios.LeiosPeerVars m -> m a) ->
    m a
  bracketLeiosPeer them isBigLedgerPeer =
    bracket
      -- Get-or-create: any peer-vars mini-protocol can be the first to run and
      -- allocate; the others share the existing vars. No ref count: a hot peer's
      -- mini-protocols tear down together, so whichever exits first runs the full
      -- cleanup below, and the rest find it already gone (idempotent). A straggler
      -- that allocated its own entry cleans that up on its own exit.
      ( do
          fresh <- Leios.newLeiosPeerVars isBigLedgerPeer
          atomically $ do
            peersVars <- LazySTM.readTVar (getLeiosPeersVars kernel)
            case Map.lookup pid peersVars of
              Just existing -> pure existing
              Nothing -> do
                LazySTM.writeTVar (getLeiosPeersVars kernel) $ Map.insert pid fresh peersVars
                pure fresh
      )
      ( \_peerVars -> do
          atomically $ LazySTM.modifyTVar' (getLeiosPeersVars kernel) $ Map.delete pid
          MVar.modifyMVar_ (getLeiosOutstanding kernel) $
            pure . Leios.removePeerFromOutstanding pid
      )
   where
    pid = Leios.MkPeerId them

  aLeiosNotifyClient ::
    NodeToNodeVersion ->
    ExpandedInitiatorContext addrNTN PeerTrustable m ->
    Channel m bLN ->
    m (NodeToNodeInitiatorResult, Maybe bLN)
  aLeiosNotifyClient
    version
    ExpandedInitiatorContext
      { eicConnectionId = them
      , eicControlMessage = controlMessageSTM
      , eicIsBigLedgerPeer = isBigLedgerPeer
      }
    channel = do
      labelThisThread "LeiosNotifyClient"
      bracketLeiosPeer them isBigLedgerPeer $ \peerVars -> do
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
    m ((), Maybe bLN)
  aLeiosNotifyServer version ResponderContext{rcConnectionId = them} channel = do
    labelThisThread "LeiosNotifyServer"
    (peer, pump) <- hLeiosNotifyServer version them
    -- The pump lives exactly as long as the peer; 'link' surfaces a pump
    -- crash instead of silently leaving the peer unable to send.
    withAsync pump $ \pumpThread -> do
      link pumpThread
      runLookaheadFixedSenderPeerWithLimits
        (TraceLabelPeer them `contramap` tLeiosNotifyTracer)
        (cLeiosNotifyCodec (mkCodecs version))
        blLeiosNotify
        timeLimitsLeiosNotify
        channel
        peer

  aLeiosFetchClient ::
    NodeToNodeVersion ->
    ExpandedInitiatorContext addrNTN PeerTrustable m ->
    Channel m bLF ->
    m (NodeToNodeInitiatorResult, Maybe bLF)
  aLeiosFetchClient
    version
    ExpandedInitiatorContext
      { eicConnectionId = them
      , eicControlMessage = controlMessageSTM
      , eicIsBigLedgerPeer = isBigLedgerPeer
      }
    channel = do
      labelThisThread "LeiosFetchClient"
      bracketLeiosPeer them isBigLedgerPeer $ \peerVars ->
        withWriter leiosDB $ \writer -> do
          ((), trailing) <-
            runPipelinedPeerWithLimits
              (TraceLabelPeer them `contramap` tLeiosFetchTracer)
              (cLeiosFetchCodec (mkCodecs version))
              blLeiosFetch
              timeLimitsLeiosFetch
              channel
              $ hLeiosFetchClient writer version controlMessageSTM them peerVars
          pure (NoInitiatorResult, trailing)

  aLeiosFetchServer ::
    NodeToNodeVersion ->
    ResponderContext addrNTN ->
    Channel m bLF ->
    m ((), Maybe bLF)
  aLeiosFetchServer version ResponderContext{rcConnectionId = them} channel = do
    labelThisThread "LeiosFetchServer"
    withReader leiosDB $ \reader ->
      runPeerWithLimits
        (TraceLabelPeer them `contramap` tLeiosFetchTracer)
        (cLeiosFetchCodec (mkCodecs version))
        blLeiosFetch
        timeLimitsLeiosFetch
        channel
        $ hLeiosFetchServer reader version them

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
    Set.empty -- TODO: use feature flags for Leios
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
        , perasCertDiffusionProtocol = perasUnsupportedInitiator
        , perasVoteDiffusionProtocol = perasUnsupportedInitiator
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
            -- TODO: Also move the leios protocols into NodeToNodeProtocols?
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
    Set.empty
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
        , perasCertDiffusionProtocol = perasUnsupportedInitiatorResponder
        , perasVoteDiffusionProtocol = perasUnsupportedInitiatorResponder
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

-- | Placeholder for the Peras certificate/vote diffusion mini-protocols. The
-- network layer only invokes these when the negotiated 'NodeToNodeVersionData'
-- advertises 'PerasSupported'; consensus currently negotiates
-- 'PerasUnsupported', so these slots are never run. If/when Peras lands at the
-- consensus layer, replace these with real applications.
perasUnsupportedInitiator ::
  RunMiniProtocol 'Mux.InitiatorMode initiatorCtx responderCtx bytes m a Void
perasUnsupportedInitiator =
  InitiatorProtocolOnly
    (MiniProtocolCb (\_ _ -> error "Peras diffusion protocol invoked without PerasSupported"))

perasUnsupportedInitiatorResponder ::
  RunMiniProtocol 'Mux.InitiatorResponderMode initiatorCtx responderCtx bytes m a c
perasUnsupportedInitiatorResponder =
  InitiatorAndResponderProtocol
    (MiniProtocolCb (\_ _ -> error "Peras diffusion protocol invoked without PerasSupported"))
    (MiniProtocolCb (\_ _ -> error "Peras diffusion protocol invoked without PerasSupported"))

leiosNotifyPipelineDepth :: Int
leiosNotifyPipelineDepth = 1000 -- TODO magic number

-- | The most LeiosFetch requests to leave outstanding on one connection.
--
-- Sized from what an honest client needs, which is set by
-- 'maxRequestedBytesSizePerBigLedgerPeer': enough to ask a big-ledger peer for
-- five endorser blocks of the largest size the protocol allows. Each such
-- closure is 12 MiB, which is 192 jobs of 'maxJobBytesSize', and a request
-- carries 7 of those --- the 8th would pass 'maxRequestBytesSize', and a job
-- may not span two requests. So 28 requests per endorser block, 140 for five,
-- rounded up for headroom.
--
-- Far below what the mini-protocol's ingress queue could hold, so this is a
-- statement about fetch concurrency rather than about buffering.
--
-- At the limit the client collects one response for each request it sends. A
-- low-water mark would instead let it drain before refilling, so that it is
-- not blocked on a response between one send and the next --- but each send
-- still hands the driver a single message either way, so whether that changes
-- anything on the wire is a question for measurement rather than for this
-- comment. Left out until someone has one.
leiosFetchPipelineDepth :: Int
leiosFetchPipelineDepth = 150

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
