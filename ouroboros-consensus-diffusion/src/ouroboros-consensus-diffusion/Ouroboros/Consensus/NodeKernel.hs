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
{-# LANGUAGE TypeApplications #-}
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
import Control.Applicative ((<|>))
import qualified Control.Concurrent.Class.MonadMVar as MVar
import qualified Control.Concurrent.Class.MonadSTM as LazySTM
import qualified Control.Concurrent.Class.MonadSTM.Strict as StrictSTM
import Control.DeepSeq (force)
import Control.Monad
import qualified Control.Monad.Class.MonadTimer.SI as SI
import Control.Monad.Except
import Control.ResourceRegistry
import Control.Tracer
import Data.Bifunctor (second)
import Data.Data (Typeable)
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.Hashable (Hashable)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Measure
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Void (Void)
import LeiosDemoDb
  ( LeiosDbConnection (..)
  , LeiosDbHandle (..)
  , withLeiosDb
  )
import qualified LeiosDemoDb as LeiosDb
import qualified LeiosDemoLogic as Leios
import LeiosDemoTypes
  ( BytesSize
  , LeiosOutstanding
  , LeiosPeerVars
  , LeiosPoint (..)
  , TraceLeiosKernel (..)
  , pointEbHash
  )
import qualified LeiosDemoTypes as Leios
import LeiosStagingArea
  ( LeiosStagingArea (..)
  , StagedCertRB (..)
  , newLeiosStagingArea
  , runStagingAreaDrain
  )
import LeiosVoteState (LeiosVoteState (..), newLeiosVoteState)
import LeiosVoting (getLeiosCommittee, runLeiosVoting)
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
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Storage.ChainDB.API
  ( AddBlockPromise (..)
  , AddBlockResult (..)
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
  , getLeiosPeersVars :: MVar.MVar m (Map.Map (Leios.PeerId (ConnectionId addrNTN)) (LeiosPeerVars m))
  -- ^ Per-peer offerings + outgoing request queues. Written to by
  -- the LeiosNotify clients and read by the fetch logic.
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
          , leiosCertRbStaging
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

    -- Leios fetch-logic state is created up-front in 'initInternalState'
    -- because the wrapped 'ChainDbView' (issue #890 CertRB staging) needs
    -- to register fetch work and wake the fetch loop. We just destructure
    -- the IS-level vars above.

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
          leiosPeersVars <- MVar.readMVar getLeiosPeersVars
          offerings <- mapM (MVar.readMVar . Leios.offerings) leiosPeersVars
          let livePeers = Map.keysSet leiosPeersVars
          -- Synthesise inputs from the CertRB staging area (issue #890).
          -- Each currently-staged CertRB becomes a high-priority body
          -- to fetch ('synthMissing') plus an offer-by-each-staged-peer
          -- ('synthOfferings'). The EB hash goes into *both* halves of
          -- the per-peer offer pair, since a peer that has the CertRB
          -- applied locally holds the full closure (body AND txs), so
          -- 'choosePeerEb' (body) and 'choosePeerTx' (closure) both
          -- pick them up.
          --
          -- We deliberately do *not* synthesise 'missingEbTxs' /
          -- 'reverseEbIndexByTx' here. Once the body arrives,
          -- 'msgLeiosBlock' populates those from the body's tx list;
          -- until then there's nothing to fetch. The previous per-tick
          -- 'leiosDbQueryFetchWork' was an O(|leios.db|) workaround for
          -- the restart case (body in DB, tx closure partial). Restart
          -- recovery is deferred to a follow-up startup-bootstrap of
          -- 'LeiosOutstanding'; until that lands, an interrupted
          -- closure fetch is not resumed across restart.
          --
          -- Re-derived each tick from the staging snapshot + current
          -- ChainSync candidates, so peers that connect after staging
          -- still become eligible.
          stagedNow <- atomically $ stagedSnapshot leiosCertRbStaging
          (stagedAugmented, synthMissing, synthOfferings) <-
            if Map.null stagedNow
              then pure (Map.empty, Map.empty, Map.empty)
              else do
                candidates <- atomically $ do
                  handlesNow <- cschcMap varChainSyncHandles
                  -- Re-key by 'PeerId' so the map lines up with how the
                  -- staging area and the offerings map identify peers.
                  traverse
                    (fmap csCandidate . readTVar . cschState)
                    (Map.mapKeys Leios.MkPeerId handlesNow)
                let augmented = augmentStagedPeers candidates stagedNow
                    -- Only synth offerings for peers we can actually
                    -- send to (in 'leiosPeersVars'). Otherwise the
                    -- decision logic would target peers whose request
                    -- slot is missing from the eventual
                    -- 'Map.intersectionWith' send step → silent drop,
                    -- while 'requestedEbPeers' / 'requestedTxPeers' is
                    -- already updated, locking the EB out from future
                    -- retries.
                    offers =
                      (`Map.restrictKeys` livePeers) $
                        Map.fromListWith
                          (\(a, b) (c, d) -> (a <> c, b <> d))
                          [ ( peer
                            , let ebs = Set.singleton (pointEbHash point)
                               in (ebs, ebs)
                            )
                          | (point, entry) <- Map.toList augmented
                          , peer <- Set.toList (stagedPeers entry)
                          ]
                pure (augmented, Map.map stagedSize augmented, offers)
          let augmentedOfferings =
                Map.unionWith
                  (\(a, b) (c, d) -> (a <> c, b <> d))
                  offerings
                  synthOfferings
          newDecisions <- MVar.modifyMVar getLeiosOutstanding $ \outstanding -> do
            -- Short-circuit synth-add for EBs whose body is already in
            -- 'acquiredEbBodies' (analogous to MsgLeiosBlockOffer's
            -- check at 'NodeToNode.hs'). Avoids per-tick "synth re-adds,
            -- filterMissingWork removes" round-trips during the
            -- post-body, pre-closure-complete window.
            let synthMissing' =
                  Map.filterWithKey
                    ( \point _ ->
                        Set.notMember
                          (pointEbHash point)
                          (Leios.acquiredEbBodies outstanding)
                    )
                    synthMissing
                -- Persist the synth-added body entries directly. The
                -- natural cleanup paths handle them: 'msgLeiosBlock'
                -- deletes from 'missingEbBodies' on body arrival;
                -- 'filterMissingWork' moves the entry to
                -- 'acquiredEbBodies' if the body was already in DB
                -- (e.g. from forging or a prior session). Left-biased
                -- so the real entry's in-flight-request bookkeeping is
                -- preserved.
                augmentedOutstanding =
                  outstanding
                    { Leios.missingEbBodies =
                        Map.union
                          (Leios.missingEbBodies outstanding)
                          synthMissing'
                    }
            filteredOutstanding <-
              Leios.filterMissingWork leiosConn augmentedOutstanding
            traceWith leiosTr $ MkTraceLeiosKernel "leiosFetchLogic: filtered"
            let (!outstanding', decisions) =
                  Leios.leiosFetchLogicIteration
                    Leios.demoLeiosFetchStaticEnv
                    augmentedOfferings
                    filteredOutstanding
            pure (outstanding', decisions)
          traceWith leiosTr $ MkTraceLeiosKernel "leiosFetchLogic: decided"
          let newRequests =
                Leios.packRequests Leios.demoLeiosFetchStaticEnv newDecisions
              numStaged = Map.size stagedNow
              numLiveStagedPeers =
                sum
                  [ Set.size (stagedPeers e)
                  | e <- Map.elems stagedAugmented
                  ]
              decisionsTargetedKeys = Map.keysSet newRequests
              droppableKeys =
                decisionsTargetedKeys `Set.difference` livePeers
          traceWith leiosTr $
            MkTraceLeiosKernel $
              "leiosFetchLogic: "
                ++ show (sum (fmap length newRequests))
                ++ " new reqs"
          unless (Map.null stagedNow) $
            traceWith leiosTr $
              MkTraceLeiosKernel $
                "leiosFetchLogic: staged="
                  ++ show numStaged
                  ++ " liveStagedPeerLinks="
                  ++ show numLiveStagedPeers
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

    -- CertRB staging drain (issue #890): on every EB closure arrival,
    -- check whether a staged CertRB was waiting for it; if so, hand it to
    -- 'ChainDB.addBlockAsync' so ChainSel can finally consider it.
    void $
      forkLinkedThread registry "NodeKernel.leiosCertRbDrain" $ do
        let drainTr = leiosKernelTracer tracers
        chan <- subscribeEbNotifications leiosDB
        runStagingAreaDrain leiosCertRbStaging chan $ \point blk -> do
          traceWith drainTr TraceLeiosCertRBReleased{releasedEbPoint = point}
          _ <-
            ChainDB.addBlockAsync
              chainDB
              InvalidBlockPunishment.noPunishment
              blk
          pure ()

    -- The Leios voting thread: when this node has a voting key, subscribe
    -- to local "EB closure acquired" notifications and emit a vote for
    -- each acquired EB (which the LeiosNotify server then publishes to
    -- peers). 'Nothing' disables voting on this node.
    void $
      forkLinkedThread registry "NodeKernel.leiosVoting" $
        runLeiosVoting
          (leiosKernelTracer tracers)
          chainDB
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
  , -- Leios state created here so the wrapped 'ChainDbView' (issue #890
    -- CertRB staging) can register fetch work; consumed in 'initNodeKernel'.
    leiosOutstanding :: MVar.MVar m (LeiosOutstanding (ConnectionId addrNTN))
  , leiosReady :: MVar.MVar m ()
  , leiosPeersVars ::
      MVar.MVar m (Map.Map (Leios.PeerId (ConnectionId addrNTN)) (LeiosPeerVars m))
  , leiosCertRbStaging ::
      LeiosStagingArea m (Leios.PeerId (ConnectionId addrNTN)) blk
  -- ^ CertRBs whose EB closure isn't locally available yet (issue #890).
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

    -- Leios state created up front so the wrapped 'ChainDbView' below can
    -- register fetch work / wake the fetch loop without forward references.
    leiosPeersVars <- MVar.newMVar Map.empty
    leiosOutstanding <- MVar.newMVar Leios.emptyLeiosOutstanding
    leiosReady <- MVar.newEmptyMVar
    leiosCertRbStaging <- newLeiosStagingArea

    let readFetchMode =
          BlockFetchClientInterface.readFetchModeDefault
            (toConsensusMode $ gnkaLoEAndGDDArgs genesisArgs)
            btime
            (ChainDB.getCurrentChain chainDB)
            getUseBootstrapPeers
            (GSM.gsmStateToLedgerJudgement <$> readTVar varGsmState)
        chainDbView =
          wrapChainDbViewForLeiosStaging
            (leiosKernelTracer tracers)
            leiosDB
            leiosCertRbStaging
            varChainSyncHandles
            leiosReady
            (BlockFetchClientInterface.defaultChainDbView chainDB)
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

-- | CertRB staging gate for issue #890.
--
-- Wraps a 'ChainDbView' so that incoming CertRBs whose announced EB closure
-- is not yet in the local LeiosDb are *not* handed to ChainSel — they're
-- parked in 'stagingTVar' and the missing EB is registered as Leios fetch
-- work. Once the EB closure arrives, a separate drain thread (forked in
-- 'initNodeKernel') re-submits the parked block via the unwrapped
-- 'ChainDB.addBlockAsync'.
--
-- 'getIsFetched' is also widened so the BlockFetch decision logic doesn't
-- keep refetching the same block while it's staged.
wrapChainDbViewForLeiosStaging ::
  forall m blk addrNTN.
  ( IOLike m
  , GetPrevHash blk
  , Ord addrNTN
  , ResolveLeiosBlock blk
  ) =>
  Tracer m TraceLeiosKernel ->
  LeiosDbHandle m ->
  LeiosStagingArea m (Leios.PeerId (ConnectionId addrNTN)) blk ->
  ChainSyncClientHandleCollection (ConnectionId addrNTN) m blk ->
  -- | 'getLeiosReady' — pinged after a stage so the fetch loop wakes
  -- promptly and can synthesise the new entry into its next iteration.
  MVar.MVar m () ->
  BlockFetchClientInterface.ChainDbView m blk ->
  BlockFetchClientInterface.ChainDbView m blk
wrapChainDbViewForLeiosStaging
  tracer
  leiosDbHandle
  stagingArea
  varChainSyncHandles
  readyMVar
  defView =
    defView
      { BlockFetchClientInterface.addBlockAsync = stagingAwareAddBlock
      , BlockFetchClientInterface.getIsFetched = do
          baseFetched <- BlockFetchClientInterface.getIsFetched defView
          staged <- isStagedBlock stagingArea
          pure $ \p -> baseFetched p || staged p
      }
   where
    stagingAwareAddBlock punish blk
      | Nothing <- blockLeiosCert blk =
          BlockFetchClientInterface.addBlockAsync defView punish blk
      | otherwise = do
          mAnn <- atomically $ do
            candidates <- candidateFragments
            currentChain <- BlockFetchClientInterface.getCurrentChain defView
            pure $
              findParentAnnouncement
                (Block.blockPrevHash blk)
                currentChain
                candidates
          case mAnn of
            -- Parent isn't visible on the current chain or any
            -- ChainSync candidate, or didn't announce. Can't determine
            -- the EB. Admit and let ChainSel / apply-time error
            -- decide. Fork-time / out-of-order arrivals blind spot:
            -- see issue #890 PR description.
            Nothing ->
              BlockFetchClientInterface.addBlockAsync defView punish blk
            Just (point, size) -> do
              mEb <- withLeiosDb leiosDbHandle $ \conn ->
                leiosDbQueryCompletedEbByHash conn (pointEbHash point)
              case mEb of
                Just _ ->
                  BlockFetchClientInterface.addBlockAsync defView punish blk
                Nothing -> stage point size punish blk

    candidateFragments = do
      handles <- cschcMap varChainSyncHandles
      traverse (fmap csCandidate . readTVar . cschState) handles

    stage point size _punish blk = do
      peers <- atomically $ peersThatKnowBlock varChainSyncHandles blk
      traceWith
        tracer
        TraceLeiosCertRBStaged
          { stagedBlockPoint = show (Block.blockPoint blk)
          , stagedEbPoint = point
          , stagedKnownPeers = Set.size peers
          }
      atomically $ stageCertRB stagingArea point size peers blk
      _ <- MVar.tryPutMVar readyMVar ()
      -- Block this BlockFetch client thread until the staging
      -- drain releases this entry (closure arrived; block was
      -- admitted to ChainDB via the unwrapped 'addBlockAsync' on
      -- the drain side). This stops the BlockFetch decision
      -- module from re-fetching the same CertRB in a tight loop
      -- during the staging window — the previous "lie about
      -- 'blockWrittenToDisk = pure True'" approach plus the
      -- 'getIsFetched' widening together don't suppress refetch
      -- in the steady-state late-join scenario, and the resulting
      -- decision-loop spin is the dominant retainer of iosim
      -- 'SimTrace' state (PR open point #3).
      --
      -- Cost: head-of-line blocking on this peer's BlockFetch
      -- pipeline. The closure fetch runs on a separate
      -- LeiosFetch channel of the same connection, so progress
      -- is not deadlocked. CPU cost is zero — STM 'retry'.
      --
      -- Caveat: if the closure never arrives (all peers offering
      -- it disconnect), this STM blocks forever and the client
      -- thread leaks. Tracked alongside PR open point #4 (no GC
      -- of staged entries); the GC pass will need to also wake
      -- parked threads with a 'FailedToAddBlock' verdict before
      -- evicting an entry.
      atomically $ do
        snapshot <- stagedSnapshot stagingArea
        when (Map.member point snapshot) retry
      -- Drain has admitted the block via the unwrapped
      -- 'addBlockAsync'; surface success so the BlockFetch
      -- client counts it as fetched and moves on.
      pure
        AddBlockPromise
          { blockWrittenToDisk = pure True
          , blockProcessed =
              pure $ SuccesfullyAddedBlock (Block.blockPoint blk)
          }

-- | Find the parent header in the current chain or any ChainSync
-- candidate fragment, and read its 'headerLeiosAnnouncement'.
-- Candidates are scanned because the parent may not yet be on the
-- selected chain (BlockFetch can deliver a child before its parent
-- reaches ChainSel).
findParentAnnouncement ::
  forall blk peer.
  (HasHeader (Header blk), ResolveLeiosBlock blk) =>
  ChainHash blk ->
  AF.AnchoredFragment (Header blk) ->
  Map.Map peer (AF.AnchoredFragment (HeaderWithTime blk)) ->
  Maybe (LeiosPoint, BytesSize)
findParentAnnouncement prev currentChain candidates = case prev of
  GenesisHash -> Nothing
  BlockHash h ->
    let onChain =
          find (\hdr -> Block.blockHash hdr == h) (AF.toNewestFirst currentChain)
        onCandidate =
          find (\hdr -> Block.blockHash hdr == h) $
            concatMap (fmap hwtHeader . AF.toNewestFirst) (Map.elems candidates)
     in (onChain <|> onCandidate) >>= headerLeiosAnnouncement

-- | Re-derive each staged entry's peer set by unioning in any peer
-- whose *current* ChainSync candidate contains the staged block.
-- Handles peers that connect (or extend their candidate through this
-- block) after staging.
-- | Re-derive each staged entry's peer set by unioning in any peer
-- whose *current* ChainSync candidate contains the staged block —
-- handles peers that connect (or extend through this block) after
-- staging. We deliberately UNION (rather than replace) so that
-- original attesters who were known to have the block at stage time
-- stay in the set even if their candidate fragment has since rolled
-- past it (the block is in their immutable DB and they can still
-- serve it via LeiosFetch). The 'Map.restrictKeys livePeers' filter
-- in 'leiosFetchLogic's synth step drops any peers that have
-- disconnected from 'leiosPeersVars' so 'Map.intersectionWith' won't
-- silently drop their requests.
augmentStagedPeers ::
  (HasHeader blk, HasHeader (Header blk), Ord peer) =>
  Map.Map peer (AF.AnchoredFragment (HeaderWithTime blk)) ->
  Map.Map LeiosPoint (StagedCertRB peer blk) ->
  Map.Map LeiosPoint (StagedCertRB peer blk)
augmentStagedPeers candidates staged =
  Map.map go staged
 where
  go entry =
    let extra =
          Set.fromList
            [ peer
            | (peer, frag) <- Map.toList candidates
            , any
                ((== Block.blockHash (stagedBlock entry)) . Block.blockHash)
                (AF.toOldestFirst frag)
            ]
     in entry{stagedPeers = stagedPeers entry `Set.union` extra}

-- | Scan all ChainSync candidates for ones whose fragment contains a
-- header with the same hash as @blk@. Those peers' chains have admitted
-- this block, so they almost certainly hold (or can quickly obtain) the
-- certified EB closure.
peersThatKnowBlock ::
  ( IOLike m
  , HasHeader blk
  , HasHeader (Header blk)
  , Ord peer
  ) =>
  ChainSyncClientHandleCollection peer m blk ->
  blk ->
  STM m (Set.Set (Leios.PeerId peer))
peersThatKnowBlock varChainSyncHandles blk = do
  handles <- cschcMap varChainSyncHandles
  let hsh = Block.blockHash blk
  fmap (Set.fromList . Map.elems) $
    flip Map.traverseMaybeWithKey handles $ \peer h -> do
      st <- readTVar (cschState h)
      let frag = csCandidate st
      pure $
        if any
          ((== hsh) . Block.blockHash)
          (AF.toOldestFirst frag)
          then Just (Leios.MkPeerId peer)
          else Nothing

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
    ( \(bf, leiosConn) -> knownSlotWatcher btime $
        \currentSlot -> withEarlyExit_ $ go bf leiosConn currentSlot
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
    pure (bf, leiosConn)

  finalizeForging (bf, leiosConn) = do
    LeiosDb.close leiosConn
    finalize bf

  go :: BlockForging m blk -> LeiosDbConnection m -> SlotNo -> WithEarlyExit m ()
  go blockForging leiosConn currentSlot = do
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
    (rbTxs, ebTxs, txssz, proof, snapSize, tickedLedgerState, forgingOnTopOf, untickedChainDepState) <-
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
          let tickedLedgerState :: Ticked (LedgerState blk) DiffMK
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

          let readTables = fmap castLedgerTables . roforkerReadTables forker . castLedgerTables

          mempoolSnapshot <-
            lift $
              getSnapshotFor
                mempool
                currentSlot
                tickedLedgerState
                readTables

          let rbCap = blockCapacityTxMeasure (configLedger cfg) tickedLedgerState
              mayEbCap = ebCapacityTxMeasure (configLedger cfg) tickedLedgerState
              (rbTxs, txssz) = snapshotTake mempoolSnapshot rbCap
              ebTxs = case mayEbCap of
                Nothing -> []
                Just ebCap ->
                  let (allTxs, _) = snapshotTake mempoolSnapshot (Data.Measure.plus rbCap ebCap)
                   in drop (length rbTxs) allTxs
          -- NB respect the capacity of the ledger state we're extending,
          -- which is /not/ 'snapshotLedgerState'

          -- force the mempool's computation before the tracer event
          _ <- evaluate (length rbTxs)
          _ <- evaluate (length ebTxs)
          _ <- evaluate mempoolHash

          trace blockForging $ TraceForgingMempoolSnapshot currentSlot bcPrevPoint mempoolHash mempoolSlotNo

          pure
            ( rbTxs
            , ebTxs
            , txssz
            , proof
            , snapshotMempoolSize mempoolSnapshot
            , forgetLedgerTables tickedLedgerState
            , ledgerTipPoint (ledgerState unticked)
            , headerStateChainDep (headerState unticked)
            )

    -- Actually produce the block
    newBlock <-
      lift $
        Block.forgeBlock
          blockForging
          Block.ForgeBlockArgs
            { Block.fbConfig = cfg
            , Block.fbCurrentBlockNo = bcBlockNo
            , Block.fbCurrentSlotNo = currentSlot
            , Block.fbCurrentTickedLedgerState = tickedLedgerState
            , Block.fbRbTxs = rbTxs
            , Block.fbEbTxs = ebTxs
            , Block.fbIsLeader = proof
            , Block.fbChainDepState = Just untickedChainDepState
            , Block.fbLeiosDb = leiosConn
            , Block.fbLeiosTracer = leiosKernelTracer tracers
            , Block.fbLeiosVoteState = leiosVoteState
            }

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
              (NE.nonEmpty (map (txId . txForgetValidated) (rbTxs ++ ebTxs)))
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
      trace blockForging $ TraceAdoptedBlock currentSlot newBlock rbTxs

  trace :: BlockForging m blk -> TraceForgeEvent blk -> WithEarlyExit m ()
  trace blockForging =
    lift
      . traceWith (forgeTracer tracers)
      . TraceLabelCreds (forgeLabel blockForging)

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
