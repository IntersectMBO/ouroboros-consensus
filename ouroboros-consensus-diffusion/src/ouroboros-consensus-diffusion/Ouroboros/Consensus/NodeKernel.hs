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
  , leiosDeadlinePruner
  , leiosDeadlinePrunerInterval
  , NodeLeiosTxCache
  , newNodeLeiosTxCache
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
import Data.Foldable (for_, traverse_)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime)
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
import Ouroboros.Consensus.Leios.Acquisitions (LeiosAcquisitions (..))
import Ouroboros.Consensus.Leios.AnnounceState
  ( AnnounceState
  , newAnnounceState
  )
import Ouroboros.Consensus.Leios.Cert (LeiosClaim)
import Ouroboros.Consensus.Leios.NotifyLog
  ( NotifyLog
  , leiosOfferRelay
  , newNotifyLog
  , pruneNotifyLog
  )
import Ouroboros.Consensus.Leios.Protocol (LeiosAnnouncement, LeiosLedgerView)
import Ouroboros.Consensus.Leios.TxCache
  ( LeiosTxCache
  , PackedEndorserBlock
  , defaultLeiosTxCacheShift
  , newHashTableLeiosTxCache
  )
import Ouroboros.Consensus.Leios.Types (ForgedEndorserBlock)
import Ouroboros.Consensus.Leios.VoteState (VoteState, newVoteState, pruneVotes)
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
  ( ClockSkew
  , SomeHeaderInFutureCheck
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies (LeiosFetchClientContext (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Config (LeiosFetchStaticEnv)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Kernel
  ( LeiosFetchKernel
  , LeiosPeerLifetimeId (..)
  , leiosFetchCertificateFocuser
  , leiosFetchDecisionLoop
  , newLeiosFetchKernel
  , onForgedLeiosEb
  , pruneLeiosFetchToImmTip
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.LocalReuse (mempoolLocalSource, newLocalReuse)
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.ReceiptTasks as Receipts
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( LeiosAnnouncementAgeLimits
  , validHeaderAnnouncement
  )
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.State
  ( LeiosNotifyPeerState
  )
import Ouroboros.Consensus.Network.NodeToNode.LeiosNotify (pruneLeiosNotifyToImmTip)
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
import Ouroboros.Consensus.Storage.LeiosDB.API (LeiosDbHandle, allocateWriter)
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
import Ouroboros.Consensus.Util.Time (nominalDelay)
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
  , localRootAddrs
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
import System.Random (StdGen, splitGen, uniform)

{-------------------------------------------------------------------------------
  Relay node
-------------------------------------------------------------------------------}

-- | Interface against running relay node
data NodeKernel m addrNTN addrNTC blk = NodeKernel
  { getChainDB :: ChainDB m blk
  -- ^ The 'ChainDB' of the node
  , getLeiosDB :: LeiosDbHandle m
  -- ^ The node's LeiosDB
  , getMempool :: Mempool m blk
  -- ^ The node's mempool
  , getTopLevelConfig :: TopLevelConfig blk
  -- ^ The node's top-level static configuration
  , getFetchClientRegistry ::
      FetchClientRegistry
        (ConnectionId addrNTN)
        (HeaderWithTime blk)
        blk
        (BlockFetchClientInterface.MatchedBlock blk)
        m
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
  , getIsLocalRoot :: addrNTN -> STM m Bool
  -- ^ Live local-root membership for transaction-submission peer tiers.
  , getLeiosNotifyClientStates ::
      StrictTVar m (Map (ConnectionId addrNTN) (StrictTVar m (LeiosNotifyPeerState blk)))
  -- ^ The state of each LeiosNotify client, by upstream peer, which
  -- 'leiosImmutableTipPruner' prunes.
  , getLeiosAnnounceState :: AnnounceState m blk
  -- ^ The announcements the node has accepted, from whichever peer.
  , getLeiosVoteState :: VoteState m blk
  -- ^ The votes the node has accepted from any peer, which decides which of
  -- them it relays.
  , getLeiosNotifyLog :: NotifyLog m blk
  -- ^ What the node relays to its downstream peers over LeiosNotify.
  , getLeiosFetchKernel :: LeiosFetchKernel m (ConnectionId addrNTN) blk
  -- ^ What the node's LeiosFetch clients share.
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
  , leiosDB :: LeiosDbHandle m
  -- ^ Whoever opened it closes it, once the node kernel's threads have stopped.
  , leiosAcquisitions :: LeiosAcquisitions m
  -- ^ Where the node's LeiosFetch emits what it acquires.
  , nextNewLeiosValidClaims :: STM m (NonEmpty (LeiosClaim, LeiosAnnouncement))
  -- ^ The claims of the new Leios certifications validated since this last
  -- returned, each with the announcement it certifies, blocking until there is
  -- one; see 'Ouroboros.Consensus.Leios.NewValidClaims.NewLeiosValidClaims'.
  -- The node kernel is its only consumer. Pending notifications are volatile,
  -- separate from accepted payload work; see
  -- @docs/fetch-policy/leiosfetch-improved.md#verified-certificate-notifications@.
  , leiosFetchRng :: StdGen
  -- ^ Root whose left split seeds fetch decisions. Early cache allocation
  -- takes only its right split for salts; neither consumer uses the root directly.
  , leiosTxCache :: NodeLeiosTxCache m
  -- ^ Allocated before opening ChainDB, with eviction wired before database promotion.
  , leiosFetchConfig :: LeiosFetchStaticEnv
  -- ^ Validated ordinary fetch credit and patience settings.
  , leiosAnnouncementAgeLimits :: LeiosAnnouncementAgeLimits
  -- ^ How old an announcement the node relays or accepts.
  , initChainDB :: StorageConfig blk -> InitChainDB m blk -> m ()
  , chainSyncFutureCheck :: SomeHeaderInFutureCheck m blk
  , maxClockSkew :: ClockSkew
  -- ^ The most that an honest peer's clock may differ from ours.
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
  , txSubmissionRng :: StdGen
  -- ^ Independent random stream for transaction-submission peer selection.
  , txSubmissionInitDelay :: TxSubmissionInitDelay
  , publicPeerSelectionStateVar ::
      StrictSTM.StrictTVar m (PublicPeerSelectionState addrNTN)
  , genesisArgs :: GenesisNodeKernelArgs m blk
  , getDiffusionPipeliningSupport :: DiffusionPipeliningSupport
  }

-- | Cache shared by forging, fetching, validation and database retirement.
type NodeLeiosTxCache m = LeiosTxCache m () VersionWord16 PackedEndorserBlock

-- | Allocate before opening ChainDB so no background promotion can predate cache
-- retirement. Supply the same LeiosFetch root passed in 'NodeKernelArgs':
-- decisions use its left split and the cache exclusively consumes its right.
-- Peers choose the keys, so keep the salted mutable table.
newNodeLeiosTxCache :: IOLike m => StdGen -> m (NodeLeiosTxCache m)
newNodeLeiosTxCache leiosFetchRoot = do
  -- Reserve the formerly unused branch for cache salts. Do not hand this branch
  -- to KeepAlive or any other observable node activity.
  -- TODO: For stronger protection, supply an explicit 128-bit secret generated
  -- securely at startup instead of deriving the cache salts from StdGen.
  let (_, cacheRng) = splitGen leiosFetchRoot
      (salt0, rest) = uniform cacheRng
      (salt1, _) = uniform rest
  newHashTableLeiosTxCache defaultLeiosTxCacheShift salt0 salt1

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
    , leiosDB
    , leiosTxCache
    , leiosAcquisitions
    , nextNewLeiosValidClaims
    , leiosFetchRng
    , leiosAnnouncementAgeLimits
    , leiosFetchConfig
    , initChainDB
    , blockFetchConfiguration
    , btime
    , gsmArgs
    , peerSharingRng
    , txSubmissionRng
    , publicPeerSelectionStateVar
    , genesisArgs
    , getDiffusionPipeliningSupport
    , miniProtocolParameters
    , systemTime
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

    sharedTxStateVar <- newSharedTxStateVar emptySharedTxState
    peerTxRegistry <- newPeerTxRegistry txSubmissionRng
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

    -- Before anything that acquires runs, so that the offer relay misses no
    -- acquisition.
    nextLeiosAcquisition <- subscribeLeiosAcquisitions leiosAcquisitions

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

    leiosNotifyLog <- newNotifyLog
    leiosAnnounceState <- newAnnounceState leiosAnnouncementAgeLimits leiosNotifyLog
    leiosVoteState <- newVoteState (leiosVoteStateTracer tracers) leiosAnnounceState leiosNotifyLog
    leiosNotifyClientStates <- newTVarIO Map.empty
    void $
      forkLinkedThread registry "NodeKernel.leiosDeadlinePruner" $
        leiosDeadlinePruner systemTime leiosNotifyLog leiosVoteState

    -- The right branch was reserved by early cache allocation; decisions
    -- keep their existing left branch, independent of salt draws.
    let (leiosFetchPrng, _) = splitGen leiosFetchRng
    receiptTasks <- Receipts.newReceiptTasks
    leiosWriter <- allocateWriter registry leiosDB
    let localReuse = newLocalReuse (mempoolLocalSource mempool)
    leiosFetchKernel <-
      newLeiosFetchKernel
        chainDB
        leiosDB
        leiosFetchConfig
        (Receipts.receiptClosing receiptTasks)
        leiosFetchPrng
        $ \outstanding ready runtime bodies retired ->
          MkLeiosFetchClientContext
            { lfcStaticEnv = leiosFetchConfig
            , lfcOutstanding = outstanding
            , lfcReady = ready
            , lfcRuntime = runtime
            , lfcTxCache = leiosTxCache
            , lfcReceiptTasks = receiptTasks
            , lfcLocalReuse = localReuse
            , lfcBodyWork = bodies
            , lfcBodyRetired = retired
            , lfcWriter = leiosWriter
            , lfcNotify = emitLeiosAcquisition leiosAcquisitions
            , lfcSystemTime = systemTime
            , lfcDetailTracer =
                contramap
                  (fmap (\(MkLeiosPeerLifetimeId connection _registration) -> connection))
                  (leiosFetchDetailTracer tracers)
            , -- Each trace names the connection, not which of its registrations.
              lfcTracer =
                contramap
                  (fmap (\(MkLeiosPeerLifetimeId connection _registration) -> connection))
                  (leiosFetchClientTracer tracers)
            }
    void $
      forkLinkedThread registry "NodeKernel.leiosFetchDecisionLoop" $
        leiosFetchDecisionLoop cfg chainDB btime leiosFetchKernel
    void $
      forkLinkedThread registry "NodeKernel.leiosImmutableTipPruner" $
        leiosImmutableTipPruner
          chainDB
          leiosAnnounceState
          leiosVoteState
          leiosNotifyClientStates
          leiosFetchKernel
    -- A dequeued batch cannot be recovered by a duplicate certificate.
    -- Keep unexpected focuser failure linked to the node invocation.
    void $
      forkLinkedThread registry "NodeKernel.leiosFetchCertificateFocuser" $
        leiosFetchCertificateFocuser (atomically nextNewLeiosValidClaims) leiosFetchKernel
    void $
      forkLinkedThread registry "NodeKernel.leiosOfferRelay" $
        leiosOfferRelay nextLeiosAcquisition leiosNotifyLog

    -- Take in each forged endorser block, with its announcing header, through
    -- the same acquisition path as data from a peer. Storage work belongs to
    -- the existing node-owned workers: waiting for LeiosDB must not hold up
    -- submission of the ranking block after bounded admission, and replacing
    -- forging credentials must not interrupt an already accepted operation.
    --
    -- Acceptance, metadata and payload admission, and the exact-point local
    -- claim commit together. The claim suppresses network fetches even before
    -- the worker marks ReceivingFromForge; another ranking block in the same
    -- slot can announce this EB and make the honest chain need it meanwhile.
    -- A worker waits for metadata, submits the combined body/TX write and
    -- settles positive confirmation before offering either half to peers.
    --
    -- Queued, running and receipt-waiting payloads share four slots. A full
    -- queue backpressures the forge before acceptance; otherwise this callback
    -- returns without waiting for writer admission or durability. Normal node
    -- shutdown drains accepted work before storage closes. Exceptional shutdown
    -- cancels and joins its owners; callback return never proves possession.
    --
    -- Retain these bounds, including the separate 128-entry metadata queue.
    -- Saturation can delay submission of the ranking block before acceptance.
    -- TODO(consensus team): A guarantee that EB handling never delays that
    -- submission needs an explicit forging policy, such as reserving capacity
    -- before constructing the EB and deciding what to forge when capacity is
    -- unavailable. Moving the wait to an unbounded queue or forked thread hides
    -- the backlog rather than settling that policy.
    let onForgedEb onset hdr leiosView forged = do
          mbValid <- validHeaderAnnouncement systemTime cfg hdr onset leiosView
          -- A forged header announces the endorser block forged with it.
          for_ mbValid $ \valid -> do
            onForgedLeiosEb leiosFetchKernel leiosAnnounceState valid forged
    void $
      forkLinkedThread registry "NodeKernel.blockForging" $
        blockForgingController st onForgedEb (LazySTM.takeTMVar blockForgingVar)

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
        , getLeiosDB = leiosDB
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
        , getIsLocalRoot = \peer -> Set.member peer . localRootAddrs <$> StrictSTM.readTVar publicPeerSelectionStateVar
        , getLeiosNotifyClientStates = leiosNotifyClientStates
        , getLeiosAnnounceState = leiosAnnounceState
        , getLeiosVoteState = leiosVoteState
        , getLeiosNotifyLog = leiosNotifyLog
        , getLeiosFetchKernel = leiosFetchKernel
        }
   where
    blockForgingController ::
      InternalState m remotePeer localPeer blk ->
      (RelativeTime -> Header blk -> StrictMaybe LeiosLedgerView -> ForgedEndorserBlock -> m ()) ->
      STM m [MkBlockForging m blk] ->
      m Void
    blockForgingController st onForgedEb getBlockForging = go []
     where
      go :: [Thread m Void] -> m Void
      go !forgingThreads = do
        blockForging <- atomically getBlockForging
        traverse_ cancelThread forgingThreads
        blockForging' <- traverse (forkBlockForging st onForgedEb) blockForging
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
  , leiosDB :: LeiosDbHandle m
  , blockFetchInterface ::
      BlockFetchConsensusInterface
        (ConnectionId addrNTN)
        (HeaderWithTime blk)
        blk
        (BlockFetchClientInterface.MatchedBlock blk)
        m
  , fetchClientRegistry ::
      FetchClientRegistry
        (ConnectionId addrNTN)
        (HeaderWithTime blk)
        blk
        (BlockFetchClientInterface.MatchedBlock blk)
        m
  , keepAliveRegistry :: KeepAliveRegistry (ConnectionId addrNTN) m
  , varChainSyncHandles :: ChainSyncClientHandleCollection (ConnectionId addrNTN) m blk
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
    , leiosDB
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
          BlockFetchConsensusInterface
            (ConnectionId addrNTN)
            (HeaderWithTime blk)
            blk
            (BlockFetchClientInterface.MatchedBlock blk)
            m
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
  -- | What 'forge' does with each endorser block it produces, given the
  -- wall-clock onset of the slot it forged it in
  (RelativeTime -> Header blk -> StrictMaybe LeiosLedgerView -> ForgedEndorserBlock -> m ()) ->
  MkBlockForging m blk ->
  m (Thread m Void)
forkBlockForging IS{..} onForgedEb (MkBlockForging blockForgingM) =
  forkLinkedWatcherAllocate
    registry
    label
    blockForgingMLabel
    finalize
    ( \bf -> knownSlotWatcher btime $
        \currentSlot slotOnset ->
          withEarlyExit_ $
            forge
              (forgeTracer tracers)
              (forgeStateInfoTracer tracers)
              cfg
              chainDB
              mempool
              bf
              (onForgedEb slotOnset)
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
  Leios pruning
-------------------------------------------------------------------------------}

-- | Retire node metadata at each advancing immutable slot: LeiosNotify client
-- state, accepted announcements and certificates in one transaction, then fetch
-- state and cache metadata at that same slot. Storage promotion independently
-- fences the cache through its wrapped handle; this loop does not perform GC.
-- See @docs/fetch-policy/leiosfetch-improved.md#immutable-tip-pruning@.
leiosImmutableTipPruner ::
  (IOLike m, RunNode blk) =>
  ChainDB m blk ->
  AnnounceState m blk ->
  VoteState m blk ->
  StrictTVar m (Map peer (StrictTVar m (LeiosNotifyPeerState blk))) ->
  LeiosFetchKernel m peer' blk ->
  m Void
leiosImmutableTipPruner chainDB announceState voteState notifyStatesVar leiosFetchKernel =
  go (SlotNo 0)
 where
  go prunedSlot = do
    immTipSlot <- atomically $ do
      immLedger <- ChainDB.getImmutableLedger chainDB
      case getTipSlot (ledgerState immLedger) of
        NotOrigin slot
          | prunedSlot < slot -> do
              pruneLeiosNotifyToImmTip announceState voteState notifyStatesVar slot
              pure slot
        _ -> retry
    pruneLeiosFetchToImmTip leiosFetchKernel immTipSlot
    go immTipSlot

-- | Prune what the node relays only until a deadline, the notify log and the
-- votes, every 'leiosDeadlinePrunerInterval'.
leiosDeadlinePruner ::
  IOLike m =>
  SystemTime m ->
  NotifyLog m blk ->
  VoteState m blk ->
  m Void
leiosDeadlinePruner systemTime notifyLog voteState = forever $ do
  now <- systemTimeCurrent systemTime
  -- Votes first, so that 'recordVote' rejects every vote 'appendNotifyLog' would drop.
  pruneVotes voteState now
  atomically $ pruneNotifyLog notifyLog now
  threadDelay (nominalDelay leiosDeadlinePrunerInterval)

-- | How often 'leiosDeadlinePruner' runs, so how long after its deadline the
-- node may still relay a notification. It must stay far below the grace
-- period a downstream peer gives before it rejects one.
leiosDeadlinePrunerInterval :: NominalDiffTime
leiosDeadlinePrunerInterval = 1

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
