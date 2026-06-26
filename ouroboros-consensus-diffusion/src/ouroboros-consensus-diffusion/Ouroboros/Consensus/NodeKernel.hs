{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

import Cardano.Base.FeatureFlags (CardanoFeatureFlag (..))
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
import Control.Monad.Except
import Control.Monad.Trans.Maybe (hoistMaybe, runMaybeT)
import Control.Monad.Writer (runWriterT, tell)
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
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust, isNothing)
import Data.Proxy
import Data.Set (Set, member)
import qualified Data.Text as Text
import Data.Void (Void)
import Ouroboros.Consensus.Block hiding (blockMatchesHeader)
import qualified Ouroboros.Consensus.Block as Block
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Forecast
import Ouroboros.Consensus.Genesis.Governor (gddWatcher)
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.SupportsPeerSelection
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Ledger.Tables.Utils (forgetLedgerTables)
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
import Ouroboros.Consensus.Peras.Cert.Inclusion
  ( PerasCertInclusionRulesDecision (..)
  , needCertInContext
  )
import Ouroboros.Consensus.Peras.Cert.Opaque (toOpaquePerasCert)
import Ouroboros.Consensus.Peras.Context
  ( StateSupportsPerasEpochContext
  , forgePerasVoteIfEligibleInContext
  )
import Ouroboros.Consensus.Peras.Time (forgetEraIndex)
import qualified Ouroboros.Consensus.Peras.Time as Time
import Ouroboros.Consensus.Peras.Voting.Rules
  ( PerasVotingRulesDecision (..)
  , isPerasVotingAllowedInContext
  )
import Ouroboros.Consensus.Peras.Voting.Trace (TracePerasVoteForgingEvent (..))
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Storage.ChainDB.API
  ( AddBlockResult (..)
  , ChainDB
  )
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as InvalidBlockPunishment
import Ouroboros.Consensus.Storage.ChainDB.Init (InitChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Init as InitChainDB
import Ouroboros.Consensus.Storage.LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import Ouroboros.Consensus.Util (whenJust)
import Ouroboros.Consensus.Util.AnchoredFragment
  ( preferAnchoredCandidate
  )
import Ouroboros.Consensus.Util.EarlyExit
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.LeakyBucket
  ( atomicallyWithMonotonicTime
  )
import Ouroboros.Consensus.Util.Orphans ()
import Ouroboros.Consensus.Util.Pred (Explainable (explain), ExplanationMode (..))
import Ouroboros.Consensus.Util.STM
import Ouroboros.Network.AnchoredFragment
  ( AnchoredFragment
  , AnchoredSeq (..)
  )
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
import Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (..))
import Ouroboros.Network.TxSubmission.Inbound.V1
  ( TxSubmissionInitDelay
  , TxSubmissionMempoolWriter
  )
import qualified Ouroboros.Network.TxSubmission.Inbound.V1 as Inbound
import Ouroboros.Network.TxSubmission.Inbound.V2.Registry
  ( SharedTxStateVar
  , TxChannelsVar
  , TxMempoolSem
  , decisionLogicThreads
  , newSharedTxStateVar
  , newTxChannelsVar
  , newTxMempoolSem
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
  , getTxChannelsVar :: TxChannelsVar m (ConnectionId addrNTN) (GenTxId blk) (GenTx blk)
  -- ^ Communication channels between `TxSubmission` client mini-protocol and
  -- decision logic.
  , getSharedTxStateVar :: SharedTxStateVar m (ConnectionId addrNTN) (GenTxId blk) (GenTx blk)
  -- ^ Shared state of all `TxSubmission` clients.
  , getTxMempoolSem :: TxMempoolSem m
  -- ^ A semaphore used by tx-submission for submitting `tx`s to the mempool.
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
  , txSubmissionRng :: StdGen
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
    , featureFlags
    , tracers
    , chainDB
    , initChainDB
    , blockFetchConfiguration
    , btime
    , systemTime
    , gsmArgs
    , peerSharingRng
    , txSubmissionRng
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

    txChannelsVar <- newTxChannelsVar
    sharedTxStateVar <- newSharedTxStateVar txSubmissionRng
    txMempoolSem <- newTxMempoolSem

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
      forkLinkedThread registry "NodeKernel.decisionLogicThreads" $
        decisionLogicThreads
          (txLogicTracer tracers)
          (txCountersTracer tracers)
          (txDecisionPolicy miniProtocolParameters)
          txChannelsVar
          sharedTxStateVar

    whenPerasEnabled $
      void $
        forkLinkedWatcher registry "NodeKernel.perasVoteForging" $
          knownSlotWatcher btime $
            \currentSlot -> withEarlyExit_ $ perasVoteForgingController systemTime st currentSlot

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
        , getTxChannelsVar = txChannelsVar
        , getSharedTxStateVar = sharedTxStateVar
        , getTxMempoolSem = txMempoolSem
        }
   where
    -- Start a thread conditionally when the PerasFlag is provided and the
    -- current block type supports Peras.
    whenPerasEnabled =
      when $
        PerasFlag `member` featureFlags
          && blockDoesReallySupportsPeras (Proxy @blk)

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

perasVoteForgingController ::
  forall m remotePeer localPeer blk.
  ( IOLike m
  , BlockSupportsPeras blk
  , StateSupportsPerasEpochContext blk
  ) =>
  SystemTime m ->
  InternalState m remotePeer localPeer blk ->
  SlotNo ->
  WithEarlyExit m ()
perasVoteForgingController systemTime IS{chainDB, tracers} slotNo = do
  -- Get crypto data from environment variables
  -- TODO: move the code outside of the vote forging controller once
  -- they are properly obtained from the ledger/context.
  poolId <- case readPerasPoolIdFromEnv (Proxy @blk) of
    Left err -> do
      trace $ TracePerasVotingCantReadEnv err
      exitEarly
    Right poolId -> pure poolId

  privateKey <- case readPerasPrivateKeyFromEnv (Proxy @blk) of
    Left err -> do
      trace $ TracePerasVotingCantReadEnv err
      exitEarly
    Right privateKey -> pure privateKey

  -- We run all 3 STM computations in a WriterT monad so that we can have proper logging,
  -- while keeping everything in the same transaction. We also use MaybeT because there is
  -- a natural abort/continue logic within the transaction. Unfortunately, we can't leverage
  -- the outer WithEarlyExit monad, because we _always_ want to get the trace.
  (mVote, traceEvents :: [TracePerasVoteForgingEvent blk]) <- lift $ atomically $ runWriterT $ runMaybeT $ do
    -- Resolve Peras round number
    -- TODO: When we have benchmarks for Peras, it might be beneficial to have some bit of state
    -- to check whether we must vote for this round, instead of always trying to resolve the slot
    -- number at each iteration. We might store the next eligible slot as @perasRoundLength@ past
    -- the previous one, or precompute every voting slot at epoch boundaries.
    roundInfo <-
      dyel $
        forgetEraIndex
          <$> Time.resolveSlotToPerasRoundInfoWithHandle
            (ChainDB.getTimeResolutionContextHandle chainDB)
            slotNo
    let roundNo = Time.stpriPerasRoundNo roundInfo
        slotInRound = Time.stpriSlotsSpentInPerasRound roundInfo

    when (not $ Time.stpriIsFirstSlotOfPerasRound roundInfo) $ do
      tell [TracePerasVotingNoVoteAfterFirstSlotInRound roundNo slotInRound]
      hoistMaybe Nothing

    -- Do the voting rules state that we should vote?
    votingDecision <-
      dyel $
        isPerasVotingAllowedInContext
          (ChainDB.getPerasVotingViewHandle chainDB)
          roundNo
    tell [TracePerasVotingRuleEvent votingDecision]
    candidateBlock <- case votingDecision of
      NoVote _ -> hoistMaybe Nothing
      Vote _ block -> pure block

    -- Forge the vote, if allowed
    mVote <-
      dyel $
        forgePerasVoteIfEligibleInContext
          (ChainDB.getPerasEpochContextResolverHandle chainDB)
          poolId
          privateKey
          roundNo
          candidateBlock
    when (isNothing mVote) $ tell $ [TracePerasVotingNotAVoterInRound roundNo]
    hoistMaybe mVote

  traverse_ trace traceEvents
  vote <- maybe exitEarly pure mVote
  tickedVote <- lift $ addArrivalTime systemTime vote
  trace $ TracePerasVotingForgedVote tickedVote
  -- Add vote and potential cert to the DB
  (addVoteResult, mAddCertChainSelOutcome) <- lift $ ChainDB.addPerasVoteSync chainDB tickedVote
  trace $ TracePerasVotingAddVoteResult addVoteResult
  traverse_ (trace . TracePerasVotingAddCertChainSelOutcome) mAddCertChainSelOutcome
 where
  trace :: TracePerasVoteForgingEvent blk -> WithEarlyExit m ()
  trace = lift . traceWith (perasVoteForgingTracer tracers)

  -- Do you even lift, bro?
  dyel = lift . lift

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
        \currentSlot -> withEarlyExit_ $ go bf currentSlot
    )
 where
  label :: String
  label = "NodeKernel.blockForging"

  blockForgingMLabel = do
    bf <- blockForgingM
    labelThisThread $ Text.unpack $ forgeLabel bf
    pure bf

  go :: BlockForging m blk -> SlotNo -> WithEarlyExit m ()
  go blockForging currentSlot = do
    trace blockForging $ TraceStartLeadershipCheck currentSlot

    -- Figure out which block to connect to
    --
    -- Normally this will be the current block at the tip, but it may be the
    -- /previous/ block, if there were multiple slot leaders
    BlockContext{bcBlockNo, bcPrevPoint} <- do
      eBlkCtx <-
        lift $
          atomically $
            mkCurrentBlockContext currentSlot
              <$> ChainDB.getCurrentChain chainDB
      case eBlkCtx of
        Right blkCtx -> return blkCtx
        Left failure -> do
          trace blockForging failure
          exitEarly

    trace blockForging $ TraceBlockContext currentSlot bcBlockNo bcPrevPoint

    -- Get forker corresponding to bcPrevPoint
    --
    -- This might fail if, in between choosing 'bcPrevPoint' and this call to
    -- 'ChainDB.withReadOnlyForkerAtPoint', we switched to a fork where 'bcPrevPoint'
    -- is no longer on our chain. When that happens, we simply give up on the
    -- chance to produce a block.
    (txs, txssz, proof, snapSize, tickedLedgerState, forgingOnTopOf) <-
      ChainDB.withReadOnlyForkerAtPoint chainDB (SpecificPoint bcPrevPoint) $ \case
        Left _ -> do
          trace blockForging $ TraceNoLedgerState currentSlot bcPrevPoint
          exitEarly
        Right forker -> do
          unticked <- lift $ atomically $ LedgerDB.roforkerGetLedgerState forker

          trace blockForging $ TraceLedgerState currentSlot bcPrevPoint

          -- We require the ticked ledger view in order to construct the ticked
          -- 'ChainDepState'.
          ledgerView <-
            case runExcept $
              forecastFor
                ( ledgerViewForecastAt
                    (configLedger cfg)
                    (ledgerState unticked)
                )
                currentSlot of
              Left err -> do
                -- There are so many empty slots between the tip of our chain and the
                -- current slot that we cannot get an ledger view anymore In
                -- principle, this is no problem; we can still produce a block (we use
                -- the ticked ledger state). However, we probably don't /want/ to
                -- produce a block in this case; we are most likely missing a blocks
                -- on our chain.
                trace blockForging $ TraceNoLedgerView currentSlot err
                exitEarly
              Right lv ->
                return lv

          trace blockForging $ TraceLedgerView currentSlot

          -- Tick the 'ChainDepState' for the 'SlotNo' we're producing a block for. We
          -- only need the ticked 'ChainDepState' to check the whether we're a leader.
          -- This is much cheaper than ticking the entire 'ExtLedgerState'.
          let tickedChainDepState :: Ticked (ChainDepState (BlockProtocol blk))
              tickedChainDepState =
                tickChainDepState
                  (configConsensus cfg)
                  ledgerView
                  currentSlot
                  (headerStateChainDep (headerState unticked))

          -- Check if we are the leader
          proof <- do
            shouldForge <-
              lift $
                checkShouldForge
                  blockForging
                  ( contramap
                      (TraceLabelCreds (forgeLabel blockForging))
                      (forgeStateInfoTracer tracers)
                  )
                  cfg
                  currentSlot
                  tickedChainDepState
            case shouldForge of
              ForgeStateUpdateError err -> do
                trace blockForging $ TraceForgeStateUpdateError currentSlot err
                exitEarly
              CannotForge cannotForge -> do
                trace blockForging $ TraceNodeCannotForge currentSlot cannotForge
                exitEarly
              NotLeader -> do
                trace blockForging $ TraceNodeNotLeader currentSlot
                exitEarly
              ShouldForge p -> return p

          -- At this point we have established that we are indeed slot leader
          trace blockForging $ TraceNodeIsLeader currentSlot

          -- Tick the ledger state for the 'SlotNo' we're producing a block for
          let tickedLedgerState :: Ticked LedgerState blk DiffMK
              tickedLedgerState =
                applyChainTick
                  OmitLedgerEvents
                  (configLedger cfg)
                  currentSlot
                  (ledgerState unticked)

          _ <- evaluate tickedLedgerState
          trace blockForging $ TraceForgeTickedLedgerState currentSlot bcPrevPoint

          -- Get a snapshot of the mempool that is consistent with the ledger
          --
          -- NOTE: It is possible that due to adoption of new blocks the
          -- /current/ ledger will have changed. This doesn't matter: we will
          -- produce a block that fits onto the ledger we got above; if the
          -- ledger in the meantime changes, the block we produce here may or
          -- may not be adopted, but it won't be invalid.
          (mempoolHash, mempoolSlotNo) <- lift $ atomically $ do
            snap <- getSnapshot mempool -- only used for its tip-like information
            pure (castHash $ snapshotStateHash snap, snapshotSlotNo snap)

          mempoolSnapshot <-
            lift $
              getSnapshotFor
                mempool
                currentSlot
                tickedLedgerState
                (roforkerReadTables forker)

          let (txs, txssz) =
                snapshotTake mempoolSnapshot $
                  blockCapacityTxMeasure (configLedger cfg) tickedLedgerState
          -- NB respect the capacity of the ledger state we're extending,
          -- which is /not/ 'snapshotLedgerState'

          -- force the mempool's computation before the tracer event
          _ <- evaluate (length txs)
          _ <- evaluate mempoolHash

          trace blockForging $ TraceForgingMempoolSnapshot currentSlot bcPrevPoint mempoolHash mempoolSlotNo

          pure
            ( txs
            , txssz
            , proof
            , snapshotMempoolSize mempoolSnapshot
            , forgetLedgerTables tickedLedgerState
            , ledgerTipPoint (ledgerState unticked)
            )

    -- Decide if we need to include a Peras certificate in this block.
    (currentRoundNo, perasCertDecision) <- lift $ atomically $ do
      let timeResolverHandle = ChainDB.getTimeResolutionContextHandle chainDB
      let certInclusionViewHandle = ChainDB.getPerasCertInclusionViewHandle chainDB

      currentRoundNo <-
        Time.stpriPerasRoundNo . forgetEraIndex
          <$> Time.resolveSlotToPerasRoundInfoWithHandle
            timeResolverHandle
            currentSlot

      decision <-
        needCertInContext certInclusionViewHandle currentRoundNo

      pure (currentRoundNo, decision)

    mbPerasCert <-
      case perasCertDecision of
        -- NOTE: if constructing a certificate inclusion decision fails, this
        -- indicates that we have not seen any certificate we could include in
        -- a block yet, so we can just ignore this case.
        Nothing -> do
          tracePerasCertInclusion $
            TracePerasCertInclusionNoCertToInclude
              currentSlot
          pure Nothing
        Just decision@(DoNotIncludeCert _) -> do
          tracePerasCertInclusion $
            TracePerasCertInclusionShouldNotIncludeCert
              (explain Deep decision)
              currentSlot
          pure Nothing
        Just decision@(IncludeCert _ cert) ->
          case toOpaquePerasCert (vpcCert (forgetArrivalTime cert)) of
            Left opaquePerasCertError -> do
              tracePerasCertInclusion $
                TracePerasCertInclusionFailedToConstructOpaqueCert
                  (explain Deep decision)
                  currentSlot
                  currentRoundNo
                  opaquePerasCertError
              pure Nothing
            Right opaquePerasCert -> do
              tracePerasCertInclusion $
                TracePerasCertInclusionShouldIncludeCert
                  (explain Deep decision)
                  currentSlot
                  currentRoundNo
                  opaquePerasCert
              pure $ Just opaquePerasCert

    -- Actually produce the block
    newBlock <-
      lift $
        Block.forgeBlock
          blockForging
          cfg
          bcBlockNo
          currentSlot
          mbPerasCert
          tickedLedgerState
          txs
          proof

    trace blockForging $
      TraceForgedBlock
        currentSlot
        forgingOnTopOf
        newBlock
        snapSize
        txssz

    -- Add the block to the chain DB
    let noPunish = InvalidBlockPunishment.noPunishment -- no way to punish yourself
    -- Make sure that if an async exception is thrown while a block is
    -- added to the chain db, we will remove txs from the mempool.

    -- 'addBlockAsync' is a non-blocking action, so `mask_` would suffice,
    -- but the finalizer is a blocking operation, hence we need to use
    -- 'uninterruptibleMask_' to make sure that async exceptions do not
    -- interrupt it.
    uninterruptibleMask_ $ do
      result <- lift $ ChainDB.addBlockAsync chainDB noPunish newBlock
      -- Block until we have processed the block
      mbCurTip <- lift $ atomically $ ChainDB.blockProcessed result

      -- Check whether we adopted our block
      when (mbCurTip /= SuccesfullyAddedBlock (blockPoint newBlock)) $ do
        isInvalid <-
          lift $
            atomically $
              ($ blockHash newBlock) . forgetFingerprint
                <$> ChainDB.getIsInvalidBlock chainDB
        case isInvalid of
          Nothing ->
            trace blockForging $ TraceDidntAdoptBlock currentSlot newBlock
          Just reason -> do
            trace blockForging $ TraceForgedInvalidBlock currentSlot newBlock reason
            -- We just produced a block that is invalid according to the
            -- ledger in the ChainDB, while the mempool said it is valid.
            -- There is an inconsistency between the two!
            --
            -- Remove all the transactions in that block, otherwise we'll
            -- run the risk of forging the same invalid block again. This
            -- means that we'll throw away some good transactions in the
            -- process.
            whenJust
              (NE.nonEmpty (map (txId . txForgetValidated) txs))
              (lift . removeTxsEvenIfValid mempool)
        exitEarly

      -- We successfully produced /and/ adopted a block
      --
      -- NOTE: we are tracing the transactions we retrieved from the Mempool,
      -- not the transactions actually /in the block/.
      -- The transactions in the block should be a prefix of the transactions
      -- in the mempool. If this is not the case, this is a bug.
      -- Unfortunately, we can't
      -- assert this here because the ability to extract transactions from a
      -- block, i.e., the @HasTxs@ class, is not implementable by all blocks,
      -- e.g., @DualBlock@.
      trace blockForging $ TraceAdoptedBlock currentSlot newBlock txs

  trace :: BlockForging m blk -> TraceForgeEvent blk -> WithEarlyExit m ()
  trace blockForging =
    lift
      . traceWith (forgeTracer tracers)
      . TraceLabelCreds (forgeLabel blockForging)

  tracePerasCertInclusion :: TracePerasCertInclusionEvent -> WithEarlyExit m ()
  tracePerasCertInclusion =
    lift
      . traceWith (perasCertInclusionTracer tracers)

-- | Context required to forge a block
data BlockContext blk = BlockContext
  { bcBlockNo :: !BlockNo
  -- ^ the block number of the block to be forged
  , bcPrevPoint :: !(Point blk)
  -- ^ the point of /the predecessor of/ the block
  --
  -- Note that a block/header stores the hash of its predecessor but not the
  -- slot.
  }

-- | Create the 'BlockContext' from the header of the previous block
blockContextFromPrevHeader ::
  HasHeader (Header blk) =>
  Header blk -> BlockContext blk
blockContextFromPrevHeader hdr =
  -- Recall that an EBB has the same block number as its predecessor, so this
  -- @succ@ is even correct when @hdr@ is an EBB.
  BlockContext (succ (blockNo hdr)) (headerPoint hdr)

-- | Determine the 'BlockContext' for a block about to be forged from the
-- current slot, ChainDB chain fragment, and ChainDB tip block number
--
-- The 'bcPrevPoint' will either refer to the header at the tip of the current
-- chain or, in case there is already a block in this slot (e.g. another node
-- was also elected leader and managed to produce a block before us), the tip's
-- predecessor. If the chain is empty, then it will refer to the chain's anchor
-- point, which may be genesis.
mkCurrentBlockContext ::
  forall blk.
  RunNode blk =>
  -- | the current slot, i.e. the slot of the block about to be forged
  SlotNo ->
  -- | the current chain fragment
  --
  -- Recall that the anchor point is the tip of the ImmutableDB.
  AnchoredFragment (Header blk) ->
  -- | the event records the cause of the failure
  Either (TraceForgeEvent blk) (BlockContext blk)
mkCurrentBlockContext currentSlot c = case c of
  Empty AF.AnchorGenesis ->
    -- The chain is entirely empty.
    Right $ BlockContext (expectedFirstBlockNo (Proxy @blk)) GenesisPoint
  Empty (AF.Anchor anchorSlot anchorHash anchorBlockNo) ->
    let p :: Point blk = BlockPoint anchorSlot anchorHash
     in if anchorSlot < currentSlot
          then Right $ BlockContext (succ anchorBlockNo) p
          else Left $ TraceSlotIsImmutable currentSlot p anchorBlockNo
  c' :> hdr -> case blockSlot hdr `compare` currentSlot of
    -- The block at the tip of our chain has a slot number /before/ the
    -- current slot number. This is the common case, and we just want to
    -- connect our new block to the block at the tip.
    LT -> Right $ blockContextFromPrevHeader hdr
    -- The block at the tip of our chain has a slot that lies in the
    -- future. Although the chain DB should not contain blocks from the
    -- future, if the volatile DB contained such blocks on startup
    -- (due to a node clock misconfiguration) this invariant may be
    -- violated. See: https://github.com/IntersectMBO/ouroboros-consensus/blob/main/docs/website/contents/for-developers/HandlingBlocksFromTheFuture.md#handling-blocks-from-the-future
    -- Also note that if the
    -- system is under heavy load, it is possible (though unlikely) that
    -- one or more slots have passed after @currentSlot@ that we got from
    -- @onSlotChange@ and before we queried the chain DB for the block
    -- at its tip. At the moment, we simply don't produce a block if this
    -- happens.

    -- TODO: We may wish to produce a block here anyway, treating this
    -- as similar to the @EQ@ case below, but we should be careful:
    --
    -- 1. We should think about what slot number to use.
    -- 2. We should be careful to distinguish between the case where we
    --    need to drop a block from the chain and where we don't.
    -- 3. We should be careful about slot numbers and EBBs.
    -- 4. We should probably not produce a block if the system is under
    --    very heavy load (e.g., if a lot of blocks have been produced
    --    after @currentTime@).
    --
    -- See <https://github.com/IntersectMBO/ouroboros-network/issues/1462>
    GT -> Left $ TraceBlockFromFuture currentSlot (blockSlot hdr)
    -- The block at the tip has the same slot as the block we're going to
    -- produce (@currentSlot@).
    EQ ->
      Right $
        if isJust (headerIsEBB hdr)
          -- We allow forging a block that is the successor of an EBB in the
          -- same slot.
          then blockContextFromPrevHeader hdr
          -- If @hdr@ is not an EBB, then forge an alternative to @hdr@: same
          -- block no and same predecessor.
          else BlockContext (blockNo hdr) $ castPoint $ AF.headPoint c'

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
