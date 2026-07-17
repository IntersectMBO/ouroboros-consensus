{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.NodeKernel
  ( -- * Node kernel
    MempoolCapacityBytesOverride (..)
  , NodeKernel (..)
  , NodeKernelArgs (..)
  , TraceForgeEvent (..)
  , getImmTipSlot
  , getMempoolReader
  , getMempoolWriter
  , getPeersFromCurrentLedger
  , getPeersFromCurrentLedgerAfterSlot
  , initNodeKernel
  , toConsensusMode
  ) where

import Cardano.Base.FeatureFlags (CardanoFeatureFlag)
import Cardano.Network.ConsensusMode (ConsensusMode (..))
import Cardano.Network.LedgerStateJudgement (LedgerStateJudgement (..))
import Cardano.Network.NodeToNode
  ( ConnectionId
  , MiniProtocolParameters (..)
  )
import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers)
import Cardano.Network.PeerSelection.LocalRootPeers
  ( OutboundConnectionsState (..)
  )
import qualified Control.Concurrent.Class.MonadMVar as MVar
import qualified Control.Concurrent.Class.MonadSTM as LazySTM
import qualified Control.Concurrent.Class.MonadSTM.Strict as StrictSTM
import Control.DeepSeq (force)
import Control.Monad
import qualified Control.Monad.Class.MonadTimer.SI as SI
import Control.ResourceRegistry
import Control.Tracer
import Data.Bifunctor (second)
import Data.Data (Typeable)
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Void (Void)
import LeiosDemoDb
  ( LeiosDbHandle (..)
  )
import qualified LeiosDemoDb as LeiosDb
import qualified LeiosDemoLogic as Leios
import LeiosDemoTypes
  ( LeiosOutstanding
  , LeiosPeerVars
  , TraceLeiosKernel (..)
  )
import qualified LeiosDemoTypes as Leios
import LeiosUtils.CallTrace
  ( SomeJsonCallTrace (SomeJsonCallTrace)
  , callTraceSameThread
  , rootCallCtx
  )
import LeiosVoteState (LeiosVoteState (..), newLeiosVoteState)
import LeiosVoting (getLeiosCommittee, runLeiosVoting)
import Ouroboros.Consensus.Block hiding (blockMatchesHeader)
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Genesis.Governor (gddWatcher)
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.SupportsPeerSelection
import Ouroboros.Consensus.Mempool
import qualified Ouroboros.Consensus.MiniProtocol.BlockFetch.ClientInterface as BlockFetchClientInterface
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client
  ( ChainSyncClientHandle (..)
  , ChainSyncClientHandleCollection (..)
  , ChainSyncState (..)
  , newChainSyncClientHandleCollection
  )
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client.HistoricityCheck
  ( HistoricityCheck
  )
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck
  ( SomeHeaderInFutureCheck
  )
import Ouroboros.Consensus.Node.GSM (GsmNodeKernelArgs (..))
import qualified Ouroboros.Consensus.Node.GSM as GSM
import Ouroboros.Consensus.Node.Genesis
  ( GenesisNodeKernelArgs (..)
  , LoEAndGDDConfig (..)
  , LoEAndGDDNodeKernelArgs (..)
  , setGetLoEFragment
  )
import Ouroboros.Consensus.Node.Run
import Ouroboros.Consensus.Node.Tracers
import Ouroboros.Consensus.NodeKernel.Forge (forge)
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Storage.ChainDB.API
  ( ChainDB
  )
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.ChainDB.Init (InitChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Init as InitChainDB
import Ouroboros.Consensus.Util.AnchoredFragment
  ( preferAnchoredCandidate
  )
import Ouroboros.Consensus.Util.EarlyExit hiding (callTraceSameThread)
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.LeakyBucket
  ( atomicallyWithMonotonicTime
  )
import Ouroboros.Consensus.Util.Orphans ()
import Ouroboros.Consensus.Util.STM
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block (castTip, tipFromHeader)
import Ouroboros.Network.BlockFetch
import Ouroboros.Network.BlockFetch.ClientState
  ( mapTraceFetchClientState
  )
import Ouroboros.Network.BlockFetch.Decision.Trace
  ( TraceDecisionEvent (..)
  )
import Ouroboros.Network.PeerSelection.Governor.Types
  ( PublicPeerSelectionState
  )
import Ouroboros.Network.PeerSharing
  ( PeerSharingAPI
  , PeerSharingRegistry
  , newPeerSharingAPI
  , newPeerSharingRegistry
  , ps_POLICY_PEER_SHARE_MAX_PEERS
  , ps_POLICY_PEER_SHARE_STICKY_TIME
  )
import Ouroboros.Network.TxSubmission.Inbound.V1
  ( TxSubmissionInitDelay
  , TxSubmissionMempoolWriter
  )
import qualified Ouroboros.Network.TxSubmission.Inbound.V1 as Inbound
import Ouroboros.Network.TxSubmission.Inbound.V2 (TxDecisionPolicy)
import Ouroboros.Network.TxSubmission.Inbound.V2.Registry
  ( PeerTxRegistry
  , SharedTxStateVar
  , TxSubmissionCountersVar
  , newPeerTxRegistry
  , newSharedTxStateVar
  , newTxSubmissionCountersVar
  , txCountersThreadV2
  )
import Ouroboros.Network.TxSubmission.Inbound.V2.Types
  ( emptySharedTxState
  )
import Ouroboros.Network.TxSubmission.Mempool.Reader
  ( TxSubmissionMempoolReader
  )
import qualified Ouroboros.Network.TxSubmission.Mempool.Reader as MempoolReader
import System.Random (StdGen)

{-------------------------------------------------------------------------------
  Relay node
-------------------------------------------------------------------------------}

-- | Interface against running relay node
data NodeKernel m addrNTN addrNTC blk = NodeKernel
  { getChainDB :: ChainDB m blk
  -- ^ The 'ChainDB' of the node
  , getMempool :: Mempool m blk
  -- ^ The node's mempool
  , getTopLevelConfig :: TopLevelConfig blk
  -- ^ The node's top-level static configuration
  , getFetchClientRegistry :: FetchClientRegistry (ConnectionId addrNTN) (HeaderWithTime blk) blk m
  -- ^ The fetch client registry, used for the block fetch clients.
  , getKeepAliveRegistry :: KeepAliveRegistry (ConnectionId addrNTN) m
  -- ^ The keep-alive registry, used by block-fetch decision logic to read
  -- per-peer GSV measurements collected by the keep-alive mini-protocol.
  , getFetchMode :: STM m FetchMode
  -- ^ The fetch mode, used by diffusion.
  , getGsmState :: STM m GSM.GsmState
  -- ^ The GSM state, used by diffusion. A ledger judgement can be derived
  -- from it with 'GSM.gsmStateToLedgerJudgement'.
  , getChainSyncHandles :: ChainSyncClientHandleCollection (ConnectionId addrNTN) m blk
  -- ^ The kill handle and exposed state for each ChainSync client.
  , getPeerSharingRegistry :: PeerSharingRegistry addrNTN m
  -- ^ Read the current peer sharing registry, used for interacting with
  -- the PeerSharing protocol
  , getTracers :: Tracers m (ConnectionId addrNTN) addrNTC blk
  -- ^ The node's tracers
  , setBlockForging :: [MkBlockForging m blk] -> m ()
  -- ^ Set block forging
  --
  -- When set with the empty list '[]' block forging will be disabled.
  , getPeerSharingAPI :: PeerSharingAPI addrNTN StdGen m
  , getOutboundConnectionsState ::
      StrictTVar m OutboundConnectionsState
  , getDiffusionPipeliningSupport ::
      DiffusionPipeliningSupport
  , getBlockchainTime :: BlockchainTime m
  , getPeerTxRegistry :: PeerTxRegistry m (ConnectionId addrNTN)
  -- ^ Per-peer tx-submission state: in-flight tracking and counters.
  , getSharedTxStateVar :: SharedTxStateVar m (ConnectionId addrNTN) (GenTxId blk)
  -- ^ Shared state of all `TxSubmission` clients.
  , getTxCountersVar :: TxSubmissionCountersVar m
  -- ^ Accumulator for tx-submission counters of disconnected peers.
  , getTxDecisionPolicy :: TxDecisionPolicy
  -- ^ Tx-submission decision policy.
  , -- The following fields contain the information in the Leios model exe's
    -- @LeiosFetchDynamicEnv@ and @LeiosFetchState@ data structures.
    --
    -- See 'LeiosPeerVars' for the write patterns.
    getLeiosDB :: LeiosDbHandle m
  -- ^ Factory for opening per-thread connections to the Leios demo DB
  -- and subscribing to EB-notification events.
  , getLeiosVoteState :: LeiosVoteState m
  -- ^ Aggregated vote state across all peers. Empty in S4; populated
  -- by the voting thread in S5.
  , getLeiosPeersVars ::
      LazySTM.TVar m (Map.Map (Leios.PeerId (ConnectionId addrNTN)) (LeiosPeerVars m))
  -- ^ Per-peer offerings + outgoing request queues, keyed by
  -- peer. Maintained by @bracketLeiosPeer@ (in the diffusion layer):
  -- get-or-created by the first of this peer's peer-vars
  -- mini-protocols (ChainSync, LeiosNotify, LeiosFetch) to start and
  -- removed by the first to exit (if some start after others exit,
  -- then they'll clean-up after themselves with /they/ exist). Read
  -- by the fetch logic. A lazy 'TVar' because the contents (the
  -- per-peer 'MVar's) are not 'NoThunks'.
  , getLeiosOutstanding :: MVar.MVar m (LeiosOutstanding (ConnectionId addrNTN))
  -- ^ Outstanding EB / TX work — written by the fetch logic, the
  -- LeiosNotify / LeiosFetch clients, and the LeiosCopier.
  , getLeiosReady :: MVar.MVar m ()
  -- ^ Filled by anyone who makes a change that might unblock a new
  -- fetch decision; the fetch logic 'MVar.takeMVar's before it runs.
  }

-- | Arguments required when initializing a node
data NodeKernelArgs m addrNTN addrNTC blk = NodeKernelArgs
  { tracers :: Tracers m (ConnectionId addrNTN) addrNTC blk
  , registry :: ResourceRegistry m
  , cfg :: TopLevelConfig blk
  , featureFlags :: Set CardanoFeatureFlag
  , btime :: BlockchainTime m
  , systemTime :: SystemTime m
  , chainDB :: ChainDB m blk
  , initChainDB :: StorageConfig blk -> InitChainDB m blk -> m ()
  , chainSyncFutureCheck :: SomeHeaderInFutureCheck m blk
  , chainSyncHistoricityCheck ::
      m GSM.GsmState ->
      HistoricityCheck m blk
  -- ^ See 'HistoricityCheck' for details.
  , blockFetchSize :: Header blk -> SizeInBytes
  , mempoolCapacityOverride :: MempoolCapacityBytesOverride
  , mempoolTimeoutConfig :: Maybe MempoolTimeoutConfig
  , miniProtocolParameters :: MiniProtocolParameters
  , blockFetchConfiguration :: BlockFetchConfiguration
  , keepAliveRng :: StdGen
  , gsmArgs :: GsmNodeKernelArgs m blk
  , getUseBootstrapPeers :: STM m UseBootstrapPeers
  , peerSharingRng :: StdGen
  , txSubmissionInitDelay :: TxSubmissionInitDelay
  , publicPeerSelectionStateVar ::
      StrictSTM.StrictTVar m (PublicPeerSelectionState addrNTN)
  , genesisArgs :: GenesisNodeKernelArgs m blk
  , getDiffusionPipeliningSupport :: DiffusionPipeliningSupport
  , leiosDB :: LeiosDbHandle m
  -- ^ Factory for opening per-thread Leios DB connections. Each consumer
  -- (forge loop, leios fetch logic, LeiosNotify / LeiosFetch handlers)
  -- opens its own connection from this handle. 'LeiosDbConnection' is
  -- documented as not thread-safe, so connections must not be shared.
  }

initNodeKernel ::
  forall m addrNTN addrNTC blk.
  ( IOLike m
  , SI.MonadTimer m
  , RunNode blk
  , Ord addrNTN
  , Hashable addrNTN
  , Show addrNTN
  , Typeable addrNTN
  ) =>
  NodeKernelArgs m addrNTN addrNTC blk ->
  m (NodeKernel m addrNTN addrNTC blk)
initNodeKernel
  args@NodeKernelArgs
    { registry
    , cfg
    , tracers
    , chainDB
    , initChainDB
    , blockFetchConfiguration
    , btime
    , gsmArgs
    , peerSharingRng
    , publicPeerSelectionStateVar
    , genesisArgs
    , getDiffusionPipeliningSupport
    , miniProtocolParameters
    , leiosDB
    } = do
    -- using a lazy 'TVar', 'BlockForging' does not have a 'NoThunks' instance.
    blockForgingVar :: LazySTM.TMVar m [MkBlockForging m blk] <- LazySTM.newTMVarIO []
    initChainDB (configStorage cfg) (InitChainDB.fromFull chainDB)

    st <- initInternalState args
    let IS
          { blockFetchInterface
          , fetchClientRegistry
          , mempool
          , peerSharingRegistry
          , varChainSyncHandles
          , varGsmState
          , leiosOutstanding = getLeiosOutstanding
          , leiosReady = getLeiosReady
          , leiosPeersVars = getLeiosPeersVars
          , leiosVoteState
          } = st

    varOutboundConnectionsState <- newTVarIO UntrustedState
    keepAliveRegistry <- newKeepAliveRegistry

    do
      let GsmNodeKernelArgs{..} = gsmArgs
          gsmTracerArgs =
            ( castTip . either AF.anchorToTip tipFromHeader . AF.head . fst
            , gsmTracer tracers
            )

      let gsm =
            GSM.realGsmEntryPoints
              gsmTracerArgs
              GSM.GsmView
                { GSM.antiThunderingHerd = Just gsmAntiThunderingHerd
                , GSM.getCandidateOverSelection = do
                    weights <- ChainDB.getPerasWeightSnapshot chainDB
                    pure $ \(headers, _lst) state ->
                      case AF.intersectionPoint headers (csCandidate state) of
                        Nothing -> GSM.CandidateDoesNotIntersect
                        Just{} ->
                          GSM.WhetherCandidateIsBetter $ -- precondition requires intersection
                            shouldSwitch
                              ( preferAnchoredCandidate
                                  (configBlock cfg)
                                  (forgetFingerprint weights)
                                  headers
                                  (csCandidate state)
                              )
                , GSM.peerIsIdle = csIdling
                , GSM.durationUntilTooOld =
                    gsmDurationUntilTooOld
                      <&> \wd (_headers, lst) ->
                        GSM.getDurationUntilTooOld wd (getTipSlot lst)
                , GSM.equivalent = (==) `on` (AF.headPoint . fst)
                , GSM.getChainSyncStates = fmap cschState <$> cschcMap varChainSyncHandles
                , GSM.getCurrentSelection = do
                    headers <- ChainDB.getCurrentChainWithTime chainDB
                    extLedgerState <- ChainDB.getCurrentLedger chainDB
                    return (headers, ledgerState extLedgerState)
                , GSM.minCaughtUpDuration = gsmMinCaughtUpDuration
                , GSM.setCaughtUpPersistentMark = \upd ->
                    (if upd then GSM.touchMarkerFile else GSM.removeMarkerFile)
                      gsmMarkerFileView
                , GSM.writeGsmState = \gsmState ->
                    atomicallyWithMonotonicTime $ \time -> do
                      writeTVar varGsmState gsmState
                      handles <- cschcMap varChainSyncHandles
                      traverse_ (($ time) . ($ gsmState) . cschOnGsmStateChanged) handles
                , GSM.isHaaSatisfied = do
                    readTVar varOutboundConnectionsState <&> \case
                      -- See the upstream Haddocks for the exact conditions under
                      -- which the diffusion layer is in this state.
                      TrustedStateWithExternalPeers -> True
                      UntrustedState -> False
                }
      judgment <- GSM.gsmStateToLedgerJudgement <$> readTVarIO varGsmState
      void $ forkLinkedThread registry "NodeKernel.GSM" $ case judgment of
        TooOld -> GSM.enterPreSyncing gsm
        YoungEnough -> GSM.enterCaughtUp gsm

    peerSharingAPI <-
      newPeerSharingAPI
        publicPeerSelectionStateVar
        peerSharingRng
        ps_POLICY_PEER_SHARE_STICKY_TIME
        ps_POLICY_PEER_SHARE_MAX_PEERS

    case gnkaLoEAndGDDArgs genesisArgs of
      LoEAndGDDDisabled -> pure ()
      LoEAndGDDEnabled lgArgs -> do
        varLoEFragment <- newTVarIO $ AF.Empty AF.AnchorGenesis
        setGetLoEFragment
          (readTVar varGsmState)
          (readTVar varLoEFragment)
          (lgnkaLoEFragmentTVar lgArgs)

        void $
          forkLinkedWatcher registry "NodeKernel.GDD" $
            gddWatcher
              cfg
              (gddTracer tracers)
              chainDB
              (lgnkaGDDRateLimit lgArgs)
              (readTVar varGsmState)
              (cschcMap varChainSyncHandles)
              varLoEFragment

    void $
      forkLinkedThread registry "NodeKernel.blockForging" $
        blockForgingController st (LazySTM.takeTMVar blockForgingVar)

    -- Run the block fetch logic in the background. This will call
    -- 'addFetchedBlock' whenever a new block is downloaded.
    void $
      forkLinkedThread registry "NodeKernel.blockFetchLogic" $
        blockFetchLogic
          (contramap castTraceFetchDecision $ blockFetchDecisionTracer tracers)
          (contramap (fmap castTraceFetchClientState) $ blockFetchClientTracer tracers)
          blockFetchInterface
          fetchClientRegistry
          keepAliveRegistry
          blockFetchConfiguration

    sharedTxStateVar <- newSharedTxStateVar emptySharedTxState
    peerTxRegistry <- newPeerTxRegistry
    txCountersVar <- newTxSubmissionCountersVar mempty

    void $
      forkLinkedThread registry "NodeKernel.txCountersThreadV2" $
        txCountersThreadV2
          (txDecisionPolicy miniProtocolParameters)
          (txCountersTracer tracers)
          (txLogicTracer tracers)
          txCountersVar
          sharedTxStateVar
          peerTxRegistry

    -- Leios fetch-logic state ('leiosOutstanding', 'leiosReady',
    -- 'leiosPeersVars') is created in 'initInternalState' and
    -- destructured from the IS-level vars above.

    -- The Leios fetch logic: 0.5s loop that takes 'getLeiosReady', reads
    -- the peers' offerings, runs 'leiosFetchLogicIteration' to decide what
    -- to fetch next from each peer, and pushes the resulting requests onto
    -- each peer's outgoing queue. Wakers (LeiosNotify clients receiving
    -- announcements, LeiosFetch clients on response, etc.) 'tryPutMVar' on
    -- 'getLeiosReady' to schedule another iteration.
    void $
      forkLinkedThread registry "NodeKernel.leiosFetchLogic" $ do
        leiosConn <- snd <$> allocate registry (const (LeiosDb.open leiosDB)) LeiosDb.close
        forever $ do
          let leiosTr = leiosKernelTracer tracers
          traceWith leiosTr $ MkTraceLeiosKernel "leiosFetchLogic: wait for leios ready"
          () <- MVar.takeMVar getLeiosReady
          iterationStart <- getMonotonicTime
          leiosPeersVars <- LazySTM.readTVarIO getLeiosPeersVars
          offerings <- mapM (MVar.readMVar . Leios.offerings) leiosPeersVars
          let livePeers = Map.keysSet leiosPeersVars
          newDecisions <- MVar.modifyMVar getLeiosOutstanding $ \outstanding -> do
            -- Re-read the live peers while holding the -- 'getLeiosOutstanding'
            -- lock. This is used to avoid losing an update to
            -- 'getLeiosOutstanding' that 'removePeerFromOutstanding' may have
            -- made (due to a 'bracketLeiosPeer' exiting) after the peers were
            -- read just above. Basically: don't assign new requests to a peer
            -- that just disconnected, since the replies would never arrive /AND/
            -- those requests would then remain in 'getLeiosOutstanding' forever.
            stillLivePeers <- LazySTM.readTVarIO getLeiosPeersVars
            filteredOutstanding <-
              Leios.filterMissingWork leiosConn outstanding
            traceWith leiosTr $
              MkTraceLeiosKernel $
                "leiosFetchLogic: outstanding "
                  <> Leios.prettyLeiosOutstanding filteredOutstanding
            traceWith leiosTr $
              MkTraceLeiosKernel $
                "leiosFetchLogic: offerings "
                  <> Leios.prettyOfferings offerings
            let (!outstanding', decisions) =
                  Leios.leiosFetchLogicIteration
                    Leios.demoLeiosFetchStaticEnv
                    (Map.restrictKeys offerings (Map.keysSet stillLivePeers))
                    filteredOutstanding
            pure (outstanding', decisions)
          traceWith leiosTr $ MkTraceLeiosKernel "leiosFetchLogic: decided"
          let newRequests =
                Leios.packRequests Leios.demoLeiosFetchStaticEnv newDecisions
              decisionsTargetedKeys = Map.keysSet newRequests
              droppableKeys =
                decisionsTargetedKeys `Set.difference` livePeers
          traceWith leiosTr $
            MkTraceLeiosKernel $
              "leiosFetchLogic: "
                ++ show (sum (fmap length newRequests))
                ++ " new reqs"
          unless (Set.null droppableKeys) $
            traceWith leiosTr $
              MkTraceLeiosKernel $
                "leiosFetchLogic: WARNING dropping "
                  ++ show (Set.size droppableKeys)
                  ++ " peer-targeted decisions because target not in leiosPeersVars"
          (\f -> sequence_ $ Map.intersectionWith f leiosPeersVars newRequests) $ \vars reqs ->
            atomically $
              StrictSTM.modifyTVar (Leios.requestsToSend vars) (<> reqs)
          iterationEnd <- getMonotonicTime
          let loopInterval = 0.5 :: SI.DiffTime
              duration = iterationEnd `diffTime` iterationStart
          traceWith leiosTr $ MkTraceLeiosKernel $ "leiosFetchLogic: duration " ++ show duration
          threadDelay $ loopInterval - duration

    -- The Leios voting thread: when this node has a voting key, subscribe
    -- to local "EB closure acquired" notifications and emit a vote for
    -- each acquired EB (which the LeiosNotify server then publishes to
    -- peers). 'Nothing' disables voting on this node.
    void $
      forkLinkedThread registry "NodeKernel.leiosVoting" $
        runLeiosVoting
          (leiosKernelTracer tracers)
          chainDB
          btime
          leiosDB
          leiosVoteState
          (topLevelConfigVotingKey cfg)

    return
      NodeKernel
        { getChainDB = chainDB
        , getMempool = mempool
        , getTopLevelConfig = cfg
        , getFetchClientRegistry = fetchClientRegistry
        , getKeepAliveRegistry = keepAliveRegistry
        , getFetchMode = readFetchMode blockFetchInterface
        , getGsmState = readTVar varGsmState
        , getChainSyncHandles = varChainSyncHandles
        , getPeerSharingRegistry = peerSharingRegistry
        , getTracers = tracers
        , setBlockForging = \a -> atomically . LazySTM.putTMVar blockForgingVar $! a
        , getPeerSharingAPI = peerSharingAPI
        , getOutboundConnectionsState =
            varOutboundConnectionsState
        , getDiffusionPipeliningSupport
        , getBlockchainTime = btime
        , getPeerTxRegistry = peerTxRegistry
        , getSharedTxStateVar = sharedTxStateVar
        , getTxCountersVar = txCountersVar
        , getTxDecisionPolicy = txDecisionPolicy miniProtocolParameters
        , getLeiosDB = leiosDB
        , getLeiosVoteState = leiosVoteState
        , getLeiosPeersVars = getLeiosPeersVars
        , getLeiosOutstanding = getLeiosOutstanding
        , getLeiosReady = getLeiosReady
        }
   where
    blockForgingController ::
      InternalState m remotePeer localPeer blk ->
      STM m [MkBlockForging m blk] ->
      m Void
    blockForgingController st getBlockForging = go []
     where
      go :: [Thread m Void] -> m Void
      go !forgingThreads = do
        blockForging <- atomically getBlockForging
        traverse_ cancelThread forgingThreads
        blockForging' <- traverse (forkBlockForging st) blockForging
        go blockForging'

castTraceFetchDecision ::
  forall remotePeer blk.
  TraceDecisionEvent remotePeer (HeaderWithTime blk) -> TraceDecisionEvent remotePeer (Header blk)
castTraceFetchDecision = \case
  PeersFetch xs -> PeersFetch (map (fmap (second (map castPoint))) xs)
  PeerStarvedUs peer -> PeerStarvedUs peer

castTraceFetchClientState ::
  forall blk.
  HasHeader (Header blk) =>
  TraceFetchClientState (HeaderWithTime blk) -> TraceFetchClientState (Header blk)
castTraceFetchClientState = mapTraceFetchClientState hwtHeader

{-------------------------------------------------------------------------------
  Internal node components
-------------------------------------------------------------------------------}

data InternalState m addrNTN addrNTC blk = IS
  { tracers :: Tracers m (ConnectionId addrNTN) addrNTC blk
  , cfg :: TopLevelConfig blk
  , registry :: ResourceRegistry m
  , btime :: BlockchainTime m
  , chainDB :: ChainDB m blk
  , blockFetchInterface ::
      BlockFetchConsensusInterface (ConnectionId addrNTN) (HeaderWithTime blk) blk m
  , fetchClientRegistry :: FetchClientRegistry (ConnectionId addrNTN) (HeaderWithTime blk) blk m
  , varChainSyncHandles :: ChainSyncClientHandleCollection (ConnectionId addrNTN) m blk
  , varGsmState :: StrictTVar m GSM.GsmState
  , mempool :: Mempool m blk
  , peerSharingRegistry :: PeerSharingRegistry addrNTN m
  , leiosDB :: LeiosDbHandle m
  , -- Leios fetch-logic state; consumed in 'initNodeKernel'.
    leiosOutstanding :: MVar.MVar m (LeiosOutstanding (ConnectionId addrNTN))
  , leiosReady :: MVar.MVar m ()
  , leiosPeersVars ::
      LazySTM.TVar m (Map.Map (Leios.PeerId (ConnectionId addrNTN)) (LeiosPeerVars m))
  , leiosVoteState :: LeiosVoteState m
  -- ^ Accumulator for Leios votes; assembles certificates once a
  -- point's tally crosses 'minCertificationThreshold'. Source of
  -- 'fbLeiosVoteState' threaded into 'ForgeBlockArgs'.
  }

initInternalState ::
  forall m addrNTN addrNTC blk.
  ( IOLike m
  , SI.MonadTimer m
  , Ord addrNTN
  , Typeable addrNTN
  , RunNode blk
  ) =>
  NodeKernelArgs m addrNTN addrNTC blk ->
  m (InternalState m addrNTN addrNTC blk)
initInternalState
  NodeKernelArgs
    { tracers
    , chainDB
    , registry
    , cfg
    , blockFetchSize
    , btime
    , mempoolCapacityOverride
    , mempoolTimeoutConfig
    , gsmArgs
    , getUseBootstrapPeers
    , getDiffusionPipeliningSupport
    , genesisArgs
    , leiosDB
    } = do
    varGsmState <- do
      let GsmNodeKernelArgs{..} = gsmArgs
      gsmState <-
        GSM.initializationGsmState
          (atomically $ ledgerState <$> ChainDB.getCurrentLedger chainDB)
          gsmDurationUntilTooOld
          gsmMarkerFileView
      newTVarIO gsmState

    varChainSyncHandles <- atomically newChainSyncClientHandleCollection
    mempool <-
      openMempool
        registry
        (chainDBLedgerInterface chainDB)
        (configLedger cfg)
        mempoolCapacityOverride
        mempoolTimeoutConfig
        (mempoolTracer tracers)

    fetchClientRegistry <- newFetchClientRegistry

    leiosPeersVars <- LazySTM.newTVarIO Map.empty
    leiosOutstanding <- MVar.newMVar Leios.emptyLeiosOutstanding
    leiosReady <- MVar.newEmptyMVar

    let readFetchMode =
          BlockFetchClientInterface.readFetchModeDefault
            (toConsensusMode $ gnkaLoEAndGDDArgs genesisArgs)
            btime
            (ChainDB.getCurrentChain chainDB)
            getUseBootstrapPeers
            (GSM.gsmStateToLedgerJudgement <$> readTVar varGsmState)
        chainDbView =
          BlockFetchClientInterface.defaultChainDbView chainDB
        blockFetchInterface ::
          BlockFetchConsensusInterface (ConnectionId addrNTN) (HeaderWithTime blk) blk m
        blockFetchInterface =
          BlockFetchClientInterface.mkBlockFetchConsensusInterface
            (dbfTracer tracers)
            (configBlock cfg)
            chainDbView
            varChainSyncHandles
            blockFetchSize
            readFetchMode
            getDiffusionPipeliningSupport

    peerSharingRegistry <- newPeerSharingRegistry

    leiosVoteState <-
      newLeiosVoteState
        (getLeiosCommittee . ledgerState <$> ChainDB.getCurrentLedger chainDB)

    return IS{..}

toConsensusMode :: forall a. LoEAndGDDConfig a -> ConsensusMode
toConsensusMode = \case
  LoEAndGDDDisabled -> PraosMode
  LoEAndGDDEnabled _ -> GenesisMode

forkBlockForging ::
  forall m addrNTN addrNTC blk.
  (IOLike m, RunNode blk) =>
  InternalState m addrNTN addrNTC blk ->
  MkBlockForging m blk ->
  m (Thread m Void)
forkBlockForging IS{..} (MkBlockForging blockForgingM) =
  forkLinkedWatcherAllocate
    registry
    label
    allocateForging
    finalizeForging
    ( \(bf, leiosConn, rootCCtx) -> do
        knownSlotWatcher btime $
          \currentSlot ->
            callTraceSameThread
              ( traceWith (forgeTracer tracers)
                  . TraceLabelCreds (forgeLabel bf)
                  . TraceCall
                  . SomeJsonCallTrace
              )
              rootCCtx
              "forge"
              currentSlot
              $ \forgeCCtx ->
                withEarlyExit_ $
                  forge
                    (forgeTracer tracers)
                    (forgeStateInfoTracer tracers)
                    (leiosKernelTracer tracers)
                    forgeCCtx
                    cfg
                    chainDB
                    mempool
                    leiosVoteState
                    bf
                    leiosConn
                    currentSlot
    )
 where
  label :: String
  label = "NodeKernel.blockForging"

  -- 'LeiosDbConnection' is not thread-safe, so we open one per
  -- forge-credentials thread (and close it when the thread exits).
  allocateForging = do
    bf <- blockForgingM
    labelThisThread $ Text.unpack $ forgeLabel bf
    leiosConn <- LeiosDb.open leiosDB
    rootCCtx <- rootCallCtx "Forge"
    pure (bf, leiosConn, rootCCtx)

  finalizeForging (bf, leiosConn, _) = do
    LeiosDb.close leiosConn
    finalize bf

{-------------------------------------------------------------------------------
  TxSubmission integration
-------------------------------------------------------------------------------}

getMempoolReader ::
  forall m blk.
  ( LedgerSupportsMempool blk
  , IOLike m
  , HasTxId (GenTx blk)
  ) =>
  Mempool m blk ->
  TxSubmissionMempoolReader (GenTxId blk) (Validated (GenTx blk)) TicketNo m
getMempoolReader mempool =
  MempoolReader.TxSubmissionMempoolReader
    { mempoolZeroIdx = zeroTicketNo
    , mempoolGetSnapshot = convertSnapshot <$> getSnapshot mempool
    }
 where
  convertSnapshot ::
    MempoolSnapshot blk ->
    MempoolReader.MempoolSnapshot (GenTxId blk) (Validated (GenTx blk)) TicketNo
  convertSnapshot
    MempoolSnapshot
      { snapshotTxsAfter
      , snapshotLookupTx
      , snapshotHasTx
      } =
      MempoolReader.MempoolSnapshot
        { mempoolTxIdsAfter = \idx ->
            [ ( txId (txForgetValidated tx)
              , idx'
              , txWireSize $ txForgetValidated tx
              )
            | (tx, idx', _msr) <- snapshotTxsAfter idx
            ]
        , mempoolLookupTx = snapshotLookupTx
        , mempoolHasTx = snapshotHasTx
        }

getMempoolWriter ::
  forall blk m.
  ( LedgerSupportsMempool blk
  , IOLike m
  , HasTxId (GenTx blk)
  ) =>
  Mempool m blk ->
  TxSubmissionMempoolWriter (GenTxId blk) (GenTx blk) TicketNo m ()
getMempoolWriter mempool =
  Inbound.TxSubmissionMempoolWriter
    { Inbound.txId = txId
    , mempoolAddTxs = \txs ->
        partitionTxIds <$> addTxs mempool txs
    }
 where
  partitionTxIds ::
    [MempoolAddTxResult blk] -> ([TxId (GenTx blk)], [(TxId (GenTx blk), ())])
  partitionTxIds = partitionEithers . map getTxId

  getTxId :: MempoolAddTxResult blk -> Either (TxId (GenTx blk)) (TxId (GenTx blk), ())
  getTxId = \case
    MempoolTxAdded tx _ -> Left $ txId (txForgetValidated tx)
    MempoolTxRejected tx _reason -> Right $ (txId tx, ())

{-------------------------------------------------------------------------------
  PeerSelection integration
-------------------------------------------------------------------------------}

-- | Retrieve the peers registered in the current chain/ledger state by
-- descending stake.
--
-- For example, for Shelley, this will return the stake pool relays ordered by
-- descending stake.
--
-- Only returns a 'Just' when the given predicate returns 'True'. This predicate
-- can for example check whether the slot of the ledger state is older or newer
-- than some slot number.
--
-- We don't use the ledger state at the tip of the chain, but the ledger state
-- @k@ blocks back, i.e., at the tip of the immutable chain, because any stake
-- pools registered in that ledger state are guaranteed to be stable. This
-- justifies merging the future and current stake pools.
getPeersFromCurrentLedger ::
  (IOLike m, LedgerSupportsPeerSelection blk) =>
  NodeKernel m addrNTN addrNTC blk ->
  (LedgerState blk EmptyMK -> Bool) ->
  STM m (Maybe [(PoolStake, NonEmpty LedgerRelayAccessPoint)])
getPeersFromCurrentLedger kernel p = do
  immutableLedger <-
    ledgerState <$> ChainDB.getImmutableLedger (getChainDB kernel)
  return $ do
    guard (p immutableLedger)
    return $
      map (second (fmap stakePoolRelayAccessPoint)) $
        force $
          getPeers immutableLedger

-- | Like 'getPeersFromCurrentLedger' but with a \"after slot number X\"
-- condition.
getPeersFromCurrentLedgerAfterSlot ::
  forall m blk addrNTN addrNTC.
  ( IOLike m
  , LedgerSupportsPeerSelection blk
  , UpdateLedger blk
  ) =>
  NodeKernel m addrNTN addrNTC blk ->
  SlotNo ->
  STM m (Maybe [(PoolStake, NonEmpty LedgerRelayAccessPoint)])
getPeersFromCurrentLedgerAfterSlot kernel slotNo =
  getPeersFromCurrentLedger kernel afterSlotNo
 where
  afterSlotNo :: LedgerState blk mk -> Bool
  afterSlotNo st =
    case ledgerTipSlot st of
      Origin -> False
      NotOrigin tip -> tip > slotNo

-- | Retrieve the slot of the immutable tip
getImmTipSlot ::
  ( IOLike m
  , UpdateLedger blk
  ) =>
  NodeKernel m addrNTN addrNTC blk ->
  STM m (WithOrigin SlotNo)
getImmTipSlot kernel =
  getTipSlot
    <$> ChainDB.getImmutableLedger (getChainDB kernel)
