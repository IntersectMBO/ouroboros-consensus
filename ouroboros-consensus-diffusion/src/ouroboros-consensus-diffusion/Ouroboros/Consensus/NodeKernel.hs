{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Text as Text
import Data.Void (Void)
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
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.State
  ( ObjectDiffusionInboundHandleCollection
  , newObjectDiffusionInboundHandleCollection
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.PerasCert
  ( PerasCertDiffusionInboundHandleCollection
  )
import Ouroboros.Consensus.Node.GSM (GsmNodeKernelArgs (..))
import qualified Ouroboros.Consensus.Node.GSM as GSM
import Ouroboros.Consensus.Node.GSM.PeerState (gsmPeerIsIdle, maybeChainSyncState, mkGsmPeerStates)
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
import Ouroboros.Consensus.Util.EarlyExit
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
  -- ^ The keep alive registry, used for the block fetch clients.
  , getFetchMode :: STM m FetchMode
  -- ^ The fetch mode, used by diffusion.
  , getGsmState :: STM m GSM.GsmState
  -- ^ The GSM state, used by diffusion. A ledger judgement can be derived
  -- from it with 'GSM.gsmStateToLedgerJudgement'.
  , getChainSyncHandles :: ChainSyncClientHandleCollection (ConnectionId addrNTN) m blk
  -- ^ The kill handle and exposed state for each ChainSync client.
  , getPerasCertDiffusionHandles ::
      ObjectDiffusionInboundHandleCollection (ConnectionId addrNTN) m blk
  -- ^ The exposed state for each Peras CertDiffusion client.
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
  }

initNodeKernel ::
  forall m addrNTN addrNTC blk.
  ( IOLike m
  , SI.MonadTimer m
  , RunNode blk
  , Ord addrNTN
  , Hashable addrNTN
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
    } = do
    -- using a lazy 'TVar', 'BlockForging' does not have a 'NoThunks' instance.
    blockForgingVar :: LazySTM.TMVar m [MkBlockForging m blk] <- LazySTM.newTMVarIO []
    initChainDB (configStorage cfg) (InitChainDB.fromFull chainDB)

    st <- initInternalState args
    let IS
          { blockFetchInterface
          , fetchClientRegistry
          , keepAliveRegistry
          , mempool
          , peerSharingRegistry
          , varChainSyncHandles
          , varPerasCertDiffusionHandles
          , varGsmState
          } = st

    varOutboundConnectionsState <- newTVarIO UntrustedState

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
                    weights <- forgetFingerprint <$> ChainDB.getPerasWeightSnapshot chainDB
                    pure $ \(headers, _lst) peerState -> do
                      case csCandidate <$> maybeChainSyncState peerState of
                        Just candidate
                          -- The candidate does not intersect with our current chain.
                          -- This is a precondition for 'WhetherCandidateIsBetter'.
                          | isNothing (AF.intersectionPoint headers candidate) ->
                              GSM.CandidateDoesNotIntersect
                          -- The candidate is better than our current chain.
                          | shouldSwitch $ preferAnchoredCandidate (configBlock cfg) weights headers candidate ->
                              GSM.WhetherCandidateIsBetter True
                          -- The candidate is not better than our current chain.
                          | otherwise ->
                              GSM.WhetherCandidateIsBetter False
                        Nothing ->
                          -- We don't have an established ChainSync connection with this peer.
                          -- We conservatively assume that its candidate is not better than ours.
                          GSM.WhetherCandidateIsBetter False
                , GSM.peerIsIdle = gsmPeerIsIdle
                , GSM.durationUntilTooOld =
                    gsmDurationUntilTooOld
                      <&> \wd (_headers, lst) ->
                        GSM.getDurationUntilTooOld wd (getTipSlot lst)
                , GSM.equivalent = (==) `on` (AF.headPoint . fst)
                , GSM.getPeerStates = mkGsmPeerStates varChainSyncHandles varPerasCertDiffusionHandles
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

    sharedTxStateVar <- newSharedTxStateVar emptySharedTxState
    peerTxRegistry <- newPeerTxRegistry
    txCountersVar <- newTxSubmissionCountersVar mempty

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

    void $
      forkLinkedThread registry "NodeKernel.txCountersThreadV2" $
        txCountersThreadV2
          (txDecisionPolicy miniProtocolParameters)
          (txCountersTracer tracers)
          (txLogicTracer tracers)
          txCountersVar
          sharedTxStateVar
          peerTxRegistry

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
        , getPerasCertDiffusionHandles = varPerasCertDiffusionHandles
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
  , keepAliveRegistry :: KeepAliveRegistry (ConnectionId addrNTN) m
  , varChainSyncHandles :: ChainSyncClientHandleCollection (ConnectionId addrNTN) m blk
  , varPerasCertDiffusionHandles ::
      PerasCertDiffusionInboundHandleCollection (ConnectionId addrNTN) m blk
  , varGsmState :: StrictTVar m GSM.GsmState
  , mempool :: Mempool m blk
  , peerSharingRegistry :: PeerSharingRegistry addrNTN m
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
    varPerasCertDiffusionHandles <- atomically newObjectDiffusionInboundHandleCollection

    mempool <-
      openMempool
        registry
        (chainDBLedgerInterface chainDB)
        (configLedger cfg)
        mempoolCapacityOverride
        mempoolTimeoutConfig
        (mempoolTracer tracers)

    fetchClientRegistry <- newFetchClientRegistry
    keepAliveRegistry <- newKeepAliveRegistry

    let readFetchMode =
          BlockFetchClientInterface.readFetchModeDefault
            (toConsensusMode $ gnkaLoEAndGDDArgs genesisArgs)
            btime
            (ChainDB.getCurrentChain chainDB)
            getUseBootstrapPeers
            (GSM.gsmStateToLedgerJudgement <$> readTVar varGsmState)
        blockFetchInterface ::
          BlockFetchConsensusInterface (ConnectionId addrNTN) (HeaderWithTime blk) blk m
        blockFetchInterface =
          BlockFetchClientInterface.mkBlockFetchConsensusInterface
            (dbfTracer tracers)
            (configBlock cfg)
            (BlockFetchClientInterface.defaultChainDbView chainDB)
            varChainSyncHandles
            blockFetchSize
            readFetchMode
            getDiffusionPipeliningSupport

    peerSharingRegistry <- newPeerSharingRegistry

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
    blockForgingMLabel
    finalize
    ( \bf -> knownSlotWatcher btime $
        \currentSlot ->
          withEarlyExit_ $
            forge
              (forgeTracer tracers)
              (forgeStateInfoTracer tracers)
              cfg
              chainDB
              mempool
              bf
              currentSlot
    )
 where
  label :: String
  label = "NodeKernel.blockForging"

  blockForgingMLabel = do
    bf <- blockForgingM
    labelThisThread $ Text.unpack $ forgeLabel bf
    pure bf

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
