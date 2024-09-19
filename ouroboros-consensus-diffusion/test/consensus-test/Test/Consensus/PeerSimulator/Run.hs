{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Consensus.PeerSimulator.Run (
    SchedulerConfig (..)
  , debugScheduler
  , defaultSchedulerConfig
  , runPointSchedule
  ) where

import           Control.Monad (foldM, forM, void, when)
import           Control.Monad.Base
import           Control.Monad.Class.MonadTime (MonadTime)
import           Control.Monad.Class.MonadTimer.SI (MonadTimer)
import           Control.ResourceRegistry
import           Control.Tracer (Tracer (..), nullTracer, traceWith)
import           Data.Coerce (coerce)
import           Data.Foldable (for_)
import           Data.List (sort)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config (TopLevelConfig (..))
import           Ouroboros.Consensus.Genesis.Governor (gddWatcher)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (CSJConfig (..), CSJEnabledConfig (..), ChainDbView,
                     ChainSyncClientHandle,
                     ChainSyncClientHandleCollection (..),
                     ChainSyncLoPBucketConfig (..),
                     ChainSyncLoPBucketEnabledConfig (..), viewChainSyncState)
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client as CSClient
import qualified Ouroboros.Consensus.Node.GsmState as GSM
import           Ouroboros.Consensus.Storage.ChainDB.API
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.STM (forkLinkedWatcher)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.BlockFetch (FetchClientRegistry,
                     bracketSyncWithFetchClient, newFetchClientRegistry)
import           Ouroboros.Network.Channel (createConnectedChannels)
import           Ouroboros.Network.ControlMessage (ControlMessage (..),
                     ControlMessageSTM)
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Util.ShowProxy (ShowProxy)
import qualified Test.Consensus.PeerSimulator.BlockFetch as BlockFetch
import qualified Test.Consensus.PeerSimulator.ChainSync as ChainSync
import           Test.Consensus.PeerSimulator.Config
import qualified Test.Consensus.PeerSimulator.CSJInvariants as CSJInvariants
import           Test.Consensus.PeerSimulator.NodeLifecycle
import           Test.Consensus.PeerSimulator.Resources
import           Test.Consensus.PeerSimulator.StateDiagram
                     (peerSimStateDiagramSTMTracerDebug)
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PeerSimulator.Trace
import           Test.Consensus.PointSchedule (BlockFetchTimeout,
                     CSJParams (..), GenesisTest (..), GenesisTestFull,
                     LoPBucketParams (..), PointSchedule (..), peersStates,
                     peersStatesRelative)
import           Test.Consensus.PointSchedule.NodeState (NodeState)
import           Test.Consensus.PointSchedule.Peers (Peer (..), PeerId,
                     getPeerIds)
import           Test.Util.ChainDB
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (TestBlock)

-- | Behavior config for the scheduler.
data SchedulerConfig =
  SchedulerConfig {
    -- | Whether to enable timeouts for the ChainSync protocol. The value of
    -- timeouts themselves is defined in 'GenesisTest'.
      scEnableChainSyncTimeouts  :: Bool

    -- | Whether to enable timeouts for the BlockFetch protocol. The value of
    -- timeouts themselves is defined in 'GenesisTest'.
    , scEnableBlockFetchTimeouts :: Bool

    -- | If 'True', 'Test.Consensus.Genesis.Setup.runTest' will print traces
    -- to stderr.
    --
    -- Use 'debugScheduler' to toggle it conveniently.
    , scDebug                    :: Bool

    -- | Whether to trace when running the scheduler.
    , scTrace                    :: Bool

    -- | Whether to trace only the current state of the candidates and selection,
    -- which provides a less verbose view of the test progress.
    , scTraceState               :: Bool

    -- | Enable Limit on Eagerness (LoE) and the Genesis Density Disconnection
    -- governor (GDD).
    , scEnableLoE                :: Bool

    -- | Whether to enable the LoP. The parameters of the LoP come from
    -- 'GenesisTest'.
    , scEnableLoP                :: Bool

    -- | Enable node downtime if this is 'Just', using the value as minimum tick
    -- duration to trigger it.
    , scDowntime                 :: Maybe DiffTime

    -- | Enable the use of ChainSel starvation information in the block fetch
    -- decision logic. It is never actually disabled, but rather the grace
    -- period is made virtually infinite.
    , scEnableChainSelStarvation :: Bool

    -- | Whether to enable ChainSync Jumping. The parameters come from
    -- 'GenesisTest'.
    , scEnableCSJ                :: Bool
  }

-- | Default scheduler config
defaultSchedulerConfig :: SchedulerConfig
defaultSchedulerConfig =
  SchedulerConfig {
    scEnableChainSyncTimeouts = True,
    scEnableBlockFetchTimeouts = True,
    scDebug = False,
    scTrace = True,
    scTraceState = False,
    scEnableLoE = False,
    scEnableLoP = False,
    scDowntime = Nothing,
    scEnableChainSelStarvation = True,
    scEnableCSJ = False
  }

-- | Enable debug tracing during a scheduler test.
debugScheduler :: SchedulerConfig -> SchedulerConfig
debugScheduler conf = conf { scDebug = True }

-- | Run a ChainSync protocol for one peer, consisting of a server and client.
--
-- The connection uses timeouts based on the ASC.
--
-- The client is synchronized with BlockFetch using the supplied 'FetchClientRegistry'.
--
-- Execution is started asynchronously, returning an action that kills the thread,
-- to allow extraction of a potential exception.
startChainSyncConnectionThread ::
  (IOLike m, MonadTimer m, LedgerSupportsProtocol blk, ShowProxy blk, ShowProxy (Header blk)) =>
  ResourceRegistry m ->
  Tracer m (TraceEvent blk) ->
  TopLevelConfig blk ->
  ChainDbView m blk ->
  FetchClientRegistry PeerId (Header blk) blk m ->
  SharedResources m blk ->
  ChainSyncResources m blk ->
  ChainSyncTimeout ->
  ChainSyncLoPBucketConfig ->
  CSJConfig ->
  StateViewTracers blk m ->
  ChainSyncClientHandleCollection PeerId m blk ->
  m (Thread m (), Thread m ())
startChainSyncConnectionThread
  registry
  tracer
  cfg
  chainDbView
  fetchClientRegistry
  SharedResources {srPeerId}
  ChainSyncResources {csrServer}
  chainSyncTimeouts_
  chainSyncLoPBucketConfig
  csjConfig
  tracers
  varHandles
  = do
    (clientChannel, serverChannel) <- createConnectedChannels
    clientThread <-
      forkLinkedThread registry ("ChainSyncClient" <> condense srPeerId) $
        bracketSyncWithFetchClient fetchClientRegistry srPeerId $
          ChainSync.runChainSyncClient tracer cfg chainDbView srPeerId chainSyncTimeouts_ chainSyncLoPBucketConfig csjConfig tracers varHandles clientChannel
    serverThread <-
      forkLinkedThread registry ("ChainSyncServer" <> condense srPeerId) $
        ChainSync.runChainSyncServer tracer srPeerId tracers csrServer serverChannel
    pure (clientThread, serverThread)

-- | Start the BlockFetch client, using the supplied 'FetchClientRegistry' to
-- register it for synchronization with the ChainSync client.
startBlockFetchConnectionThread ::
  (IOLike m, MonadTime m, MonadTimer m, HasHeader blk, HasHeader (Header blk), ShowProxy blk) =>
  ResourceRegistry m ->
  Tracer m (TraceEvent blk) ->
  StateViewTracers blk m ->
  FetchClientRegistry PeerId (Header blk) blk m ->
  ControlMessageSTM m ->
  SharedResources m blk ->
  BlockFetchResources m blk ->
  BlockFetchTimeout ->
  m (Thread m (), Thread m ())
startBlockFetchConnectionThread
  registry
  tracer
  tracers
  fetchClientRegistry
  controlMsgSTM
  SharedResources {srPeerId}
  BlockFetchResources {bfrServer}
  blockFetchTimeouts = do
    (clientChannel, serverChannel) <- createConnectedChannels
    clientThread <-
      forkLinkedThread registry ("BlockFetchClient" <> condense srPeerId) $
        BlockFetch.runBlockFetchClient tracer srPeerId blockFetchTimeouts tracers fetchClientRegistry controlMsgSTM clientChannel
    serverThread <-
      forkLinkedThread registry ("BlockFetchServer" <> condense srPeerId) $
        BlockFetch.runBlockFetchServer tracer srPeerId tracers bfrServer serverChannel
    pure (clientThread, serverThread)

-- | Wait for the given duration, but if the duration is longer than the minimum
-- duration in the live cycle, shutdown the node and restart it after the delay.
smartDelay ::
  (MonadDelay m) =>
  NodeLifecycle blk m ->
  LiveNode blk m ->
  DiffTime ->
  m (LiveNode blk m)
smartDelay lifecycle@NodeLifecycle {nlStart, nlShutdown} node duration
  | itIsTimeToRestartTheNode lifecycle duration = do
    results <- nlShutdown node
    threadDelay duration
    nlStart results
smartDelay _ node duration = do
  threadDelay duration
  pure node

itIsTimeToRestartTheNode :: NodeLifecycle blk m -> DiffTime -> Bool
itIsTimeToRestartTheNode NodeLifecycle {nlMinDuration} duration =
  case nlMinDuration of
    Just minInterval -> duration > minInterval
    Nothing          -> False

-- | The 'Tick' contains a state update for a specific peer.
-- If the peer has not terminated by protocol rules, this will update its TMVar
-- with the new state, thereby unblocking the handler that's currently waiting
-- for new instructions.
--
-- TODO doc is outdated
dispatchTick :: forall m blk.
  IOLike m =>
  Tracer m (TraceSchedulerEvent blk) ->
  STM m (Map PeerId (ChainSyncClientHandle m blk)) ->
  Map PeerId (PeerResources m blk) ->
  NodeLifecycle blk m ->
  LiveNode blk m ->
  (Int, (DiffTime, Peer (NodeState blk))) ->
  m (LiveNode blk m)
dispatchTick tracer varHandles peers lifecycle node (number, (duration, Peer pid state)) =
  case peers Map.!? pid of
    Just PeerResources {prUpdateState} -> do
      traceNewTick
      atomically (prUpdateState state)
      newNode <- smartDelay lifecycle node duration
      traceWith (lnStateTracer newNode) ()
      pure newNode
    Nothing -> error "“The impossible happened,” as GHC would say."
  where
    traceNewTick :: m ()
    traceNewTick = do
      currentChain <- atomically $ ChainDB.getCurrentChain (lnChainDb node)
      (csState, jumpingStates) <- atomically $ do
         m <- varHandles
         csState <- traverse (readTVar . CSClient.cschState) (m Map.!? pid)
         jumpingStates <- forM (Map.toList m) $ \(peer, h) -> do
           st <- readTVar (CSClient.cschJumping h)
           pure (peer, st)
         pure (csState, jumpingStates)
      traceWith tracer $ TraceNewTick
        number
        duration
        (Peer pid state)
        currentChain
        (CSClient.csCandidate <$> csState)
        jumpingStates

-- | Iterate over a 'PointSchedule', sending each tick to the associated peer in turn,
-- giving each peer a chunk of computation time, sequentially, until it satisfies the
-- conditions given by the tick.
-- This usually means for the ChainSync server to have sent the target header to the
-- client.
runScheduler ::
  IOLike m =>
  Tracer m (TraceSchedulerEvent blk) ->
  STM m (Map PeerId (ChainSyncClientHandle m blk)) ->
  PointSchedule blk ->
  Map PeerId (PeerResources m blk) ->
  NodeLifecycle blk m ->
  m (ChainDB m blk, StateViewTracers blk m)
runScheduler tracer varHandles ps@PointSchedule{psMinEndTime} peers lifecycle@NodeLifecycle {nlStart} = do
  node0 <- nlStart LiveIntervalResult {lirActive = Map.keysSet peers, lirPeerResults = []}
  traceWith tracer TraceBeginningOfTime
  nodeEnd <- foldM tick node0 (zip [0..] (peersStatesRelative ps))
  let extraDelay = case take 1 $ reverse $ peersStates ps of
        [(t, _)] -> if t < psMinEndTime
            then Just $ diffTime psMinEndTime t
            else Nothing
        _        -> Just $ coerce psMinEndTime
  LiveNode{lnChainDb, lnStateViewTracers} <-
    case extraDelay of
      Just duration -> do
        nodeEnd' <- smartDelay lifecycle nodeEnd duration
        -- Give an opportunity to the node to finish whatever it was doing at
        -- shutdown
        when (itIsTimeToRestartTheNode lifecycle duration) $
          threadDelay $ coerce psMinEndTime
        pure nodeEnd'
      Nothing ->
        pure nodeEnd
  traceWith tracer TraceEndOfTime
  pure (lnChainDb, lnStateViewTracers)
  where
    tick = dispatchTick tracer varHandles peers lifecycle

-- | Create the shared resource for the LoE if the feature is enabled in the config.
-- This is used by the ChainDB and the GDD governor.
mkLoEVar ::
  IOLike m =>
  SchedulerConfig ->
  m (LoE (StrictTVar m (AnchoredFragment (Header TestBlock))))
mkLoEVar SchedulerConfig {scEnableLoE}
  | scEnableLoE
  = LoEEnabled <$> newTVarIO (AF.Empty AF.AnchorGenesis)
  | otherwise
  = pure LoEDisabled

mkStateTracer ::
  IOLike m =>
  SchedulerConfig ->
  GenesisTest TestBlock s ->
  PeerSimulatorResources m TestBlock ->
  ChainDB m TestBlock ->
  m (Tracer m ())
mkStateTracer schedulerConfig GenesisTest {gtBlockTree} PeerSimulatorResources {psrHandles, psrPeers} chainDb
  | scTraceState schedulerConfig
  , let getCandidates = viewChainSyncState (cschcMap psrHandles) CSClient.csCandidate
        getCurrentChain = ChainDB.getCurrentChain chainDb
        getPoints = traverse readTVar (srCurrentState . prShared <$> psrPeers)
  = peerSimStateDiagramSTMTracerDebug gtBlockTree getCurrentChain getCandidates getPoints
  | otherwise
  = pure nullTracer

-- | Start all threads for ChainSync, BlockFetch and GDD, using the resources
-- for a single live interval.
-- Only start peers that haven't been disconnected in a previous interval,
-- provided by 'LiveIntervalResult'.
startNode ::
  forall m.
  ( IOLike m
  , MonadTime m
  , MonadTimer m
#if __GLASGOW_HASKELL__ >= 900
  , MonadBase m m
#endif
  ) =>
  SchedulerConfig ->
  GenesisTestFull TestBlock ->
  LiveInterval TestBlock m ->
  m ()
startNode schedulerConfig genesisTest interval = do
  let handles = psrHandles lrPeerSim
  fetchClientRegistry <- newFetchClientRegistry
  let chainDbView = CSClient.defaultChainDbView lnChainDb
      activePeers = Map.toList $ Map.restrictKeys (psrPeers lrPeerSim) (lirActive liveResult)
      peersStartOrder = psStartOrder ++ sort [pid | (pid, _) <- activePeers, pid `notElem` psStartOrder]
      activePeersOrdered = [
          peerResources
          | pid <- peersStartOrder
          , (pid', peerResources) <- activePeers
          , pid == pid'
          ]
  for_ activePeersOrdered $ \PeerResources {prShared, prChainSync, prBlockFetch} -> do
    let pid = srPeerId prShared
    forkLinkedThread lrRegistry ("Peer overview " ++ show pid) $
      -- The peerRegistry helps ensuring that if any thread fails, then
      -- the registry is closed and all threads related to the peer are
      -- killed.
      withRegistry $ \peerRegistry -> do
        (csClient, csServer) <-
          startChainSyncConnectionThread
          peerRegistry
          tracer
          lrConfig
          chainDbView
          fetchClientRegistry
          prShared
          prChainSync
          chainSyncTimeouts_
          chainSyncLoPBucketConfig
          csjConfig
          lnStateViewTracers
          handles
        BlockFetch.startKeepAliveThread peerRegistry fetchClientRegistry pid
        (bfClient, bfServer) <-
          startBlockFetchConnectionThread
          peerRegistry
          tracer
          lnStateViewTracers
          fetchClientRegistry
          (pure Continue)
          prShared
          prBlockFetch
          blockFetchTimeouts_
        waitAnyThread [csClient, csServer, bfClient, bfServer]
  -- The block fetch logic needs to be started after the block fetch clients
  -- otherwise, an internal assertion fails because getCandidates yields more
  -- peer fragments than registered clients.
  BlockFetch.startBlockFetchLogic
    (scEnableChainSelStarvation schedulerConfig)
    lrRegistry
    lrTracer
    lnChainDb
    fetchClientRegistry
    handles

  for_ lrLoEVar $ \ var -> do
      forkLinkedWatcher lrRegistry "LoE updater background" $
        gddWatcher
          lrConfig
          (mkGDDTracerTestBlock lrTracer)
          lnChainDb
          0.0 -- The rate limit makes simpler the calculations of how long tests
              -- should run and still should produce interesting interleavings.
              -- It is similar to the setting of bfcDecisionLoopInterval in
              -- Test.Consensus.PeerSimulator.BlockFetch
          (pure GSM.Syncing) -- TODO actually run GSM
          (cschcMap handles)
          var

  void $ forkLinkedWatcher lrRegistry "CSJ invariants watcher" $
    CSJInvariants.watcher (cschcMap handles)
  where
    LiveResources {lrRegistry, lrTracer, lrConfig, lrPeerSim, lrLoEVar} = resources

    LiveInterval {
        liResources = resources
      , liResult = liveResult
      , liNode = LiveNode {lnChainDb, lnStateViewTracers}
      } = interval

    GenesisTest
      { gtChainSyncTimeouts
      , gtBlockFetchTimeouts
      , gtLoPBucketParams = LoPBucketParams { lbpCapacity, lbpRate }
      , gtCSJParams = CSJParams { csjpJumpSize }
      , gtSchedule = PointSchedule {psStartOrder}
      } = genesisTest

    StateViewTracers{svtTraceTracer} = lnStateViewTracers

    -- FIXME: This type of configuration should move to `Trace.mkTracer`.
    tracer = if scTrace schedulerConfig
      then Tracer (\evt -> traceWith lrTracer evt >> traceWith svtTraceTracer evt)
      else svtTraceTracer

    chainSyncTimeouts_ =
      if scEnableChainSyncTimeouts schedulerConfig
        then gtChainSyncTimeouts
        else ChainSync.chainSyncNoTimeouts

    chainSyncLoPBucketConfig =
      if scEnableLoP schedulerConfig
        then ChainSyncLoPBucketEnabled ChainSyncLoPBucketEnabledConfig { csbcCapacity = lbpCapacity, csbcRate = lbpRate }
        else ChainSyncLoPBucketDisabled

    csjConfig =
      if scEnableCSJ schedulerConfig
        then CSJEnabled CSJEnabledConfig { csjcJumpSize = csjpJumpSize }
        else CSJDisabled

    blockFetchTimeouts_ =
      if scEnableBlockFetchTimeouts schedulerConfig
        then gtBlockFetchTimeouts
        else BlockFetch.blockFetchNoTimeouts

-- | Set up all resources related to node start/shutdown.
nodeLifecycle ::
  (IOLike m, MonadTime m, MonadTimer m, MonadBase m m) =>
  SchedulerConfig ->
  GenesisTestFull TestBlock ->
  Tracer m (TraceEvent TestBlock) ->
  ResourceRegistry m ->
  PeerSimulatorResources m TestBlock ->
  m (NodeLifecycle TestBlock m)
nodeLifecycle schedulerConfig genesisTest lrTracer lrRegistry lrPeerSim = do
  lrCdb <- emptyNodeDBs
  lrLoEVar <- mkLoEVar schedulerConfig
  let
    resources =
      LiveResources {
          lrRegistry
        , lrTracer
        , lrSTracer = mkStateTracer schedulerConfig genesisTest lrPeerSim
        , lrConfig
        , lrPeerSim
        , lrCdb
        , lrLoEVar
        }
  pure NodeLifecycle {
      nlMinDuration = scDowntime schedulerConfig
    , nlStart = lifecycleStart (startNode schedulerConfig genesisTest) resources
    , nlShutdown = lifecycleStop resources
    }
  where
    lrConfig = defaultCfg k gtForecastRange gtGenesisWindow

    GenesisTest {
        gtSecurityParam = k
      , gtForecastRange
      , gtGenesisWindow
      } = genesisTest

-- | Construct STM resources, set up ChainSync and BlockFetch threads, and
-- send all ticks in a 'PointSchedule' to all given peers in turn.
runPointSchedule ::
  forall m.
  (IOLike m, MonadTime m, MonadTimer m, MonadBase m m) =>
  SchedulerConfig ->
  GenesisTestFull TestBlock ->
  Tracer m (TraceEvent TestBlock) ->
  m (StateView TestBlock)
runPointSchedule schedulerConfig genesisTest tracer0 =
  withRegistry $ \registry -> do
    peerSim <- makePeerSimulatorResources tracer gtBlockTree (NonEmpty.fromList $ getPeerIds $ psSchedule gtSchedule)
    lifecycle <- nodeLifecycle schedulerConfig genesisTest tracer registry peerSim
    (chainDb, stateViewTracers) <- runScheduler
      (Tracer $ traceWith tracer . TraceSchedulerEvent)
      (cschcMap (psrHandles peerSim))
      gtSchedule
      (psrPeers peerSim)
      lifecycle
    snapshotStateView stateViewTracers chainDb
  where

    GenesisTest {
        gtBlockTree
      , gtSchedule
      } = genesisTest

    -- FIXME: This type of configuration should move to `Trace.mkTracer`.
    tracer = if scTrace schedulerConfig then tracer0 else nullTracer
