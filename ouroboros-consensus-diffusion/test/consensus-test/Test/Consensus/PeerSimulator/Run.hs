{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Test.Consensus.PeerSimulator.Run (
    SchedulerConfig (..)
  , debugScheduler
  , defaultSchedulerConfig
  , noTimeoutsSchedulerConfig
  , runPointSchedule
  ) where

import           Cardano.Slotting.Time (SlotLength, slotLengthFromSec)
import           Control.Exception (AsyncException (ThreadKilled))
import           Control.Monad.Class.MonadTime (MonadTime)
import           Control.Monad.Class.MonadTimer.SI (MonadTimer)
import           Control.Tracer (Tracer, nullTracer, traceWith)
import           Data.Foldable (for_)
import           Data.Functor (void)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))
import           Ouroboros.Consensus.Config (TopLevelConfig (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client (ChainDbView,
                     Consensus, bracketChainSyncClient, chainSyncClient)
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client as CSClient
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck as InFutureCheck
import           Ouroboros.Consensus.Storage.ChainDB.API
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl
                     (ChainDbArgs (cdbTracer))
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDB.Impl
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.IOLike (Exception (fromException),
                     IOLike, MonadCatch (try), MonadDelay (threadDelay),
                     MonadSTM (atomically, retry), StrictTVar, readTVar,
                     tryPutTMVar)
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Network.Block (blockPoint)
import           Ouroboros.Network.BlockFetch (FetchClientRegistry,
                     bracketSyncWithFetchClient, newFetchClientRegistry)
import           Ouroboros.Network.Channel (createConnectedChannels)
import           Ouroboros.Network.ControlMessage (ControlMessage (..),
                     ControlMessageSTM)
import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.NodeToNode.Version (NodeToNodeVersion)
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
                     (ChainSyncClientPipelined, chainSyncClientPeerPipelined)
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
                     (pipelineDecisionLowHighMark)
import           Ouroboros.Network.Protocol.ChainSync.Server
                     (chainSyncServerPeer)
import qualified Test.Consensus.BlockTree as BT
import           Test.Consensus.Genesis.Setup.GenChains (GenesisTest)
import           Test.Consensus.Network.Driver.Limits.Extras
import qualified Test.Consensus.PeerSimulator.BlockFetch as PeerSimulator.BlockFetch
import           Test.Consensus.PeerSimulator.Config
import           Test.Consensus.PeerSimulator.Resources
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PeerSimulator.Trace
import qualified Test.Consensus.PointSchedule as PointSchedule
import           Test.Consensus.PointSchedule (GenesisTest (GenesisTest),
                     Peer (Peer), PeerId, PointSchedule (PointSchedule),
                     PointScheduleConfig, TestFragH, Tick (Tick),
                     pointSchedulePeers, pscTickDuration)
import           Test.Ouroboros.Consensus.ChainGenerator.Params (Asc)
import           Test.Util.ChainDB
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (Header (..), TestBlock, testInitExtLedger)

-- | Behavior config for the scheduler.
data SchedulerConfig =
  SchedulerConfig {
    -- | Timeouts for the ChainSync protocol. These apply when the client sends
    -- a 'MsgRequestNext' or a 'MsgFindIntersect' and the server doesn't reply.
    -- Because the point schedule cannot yet handle the case where a slow peer
    -- has a header point that's behind the latest header that another peer has
    -- sent, we need to be able to control or disable them.
    scChainSyncTimeouts :: ChainSyncTimeout

    -- | The duration of a single slot, used by the peer simulator to wait
    -- between ticks.
    , scSlotLength      :: SlotLength

    -- | Config shared with point schedule generators.
    , scSchedule        :: PointScheduleConfig

    -- | If 'True', 'Test.Consensus.Genesis.Setup.runTest' will enable full
    -- tracing during the test.
    --
    -- Use 'debugScheduler' to toggle it conveniently.
    , scDebug           :: Bool
  }

-- | Determine timeouts based on the 'Asc' and a slot length of 20 seconds.
defaultSchedulerConfig :: PointScheduleConfig -> Asc -> SchedulerConfig
defaultSchedulerConfig scSchedule asc =
  SchedulerConfig {
    scChainSyncTimeouts = chainSyncTimeouts scSlotLength asc,
    scSlotLength,
    scSchedule,
    scDebug = False
  }
  where
    scSlotLength = slotLengthFromSec 20

-- | Config with no timeouts and a slot length of 20 seconds.
noTimeoutsSchedulerConfig :: PointScheduleConfig -> SchedulerConfig
noTimeoutsSchedulerConfig scSchedule =
  SchedulerConfig {
    scChainSyncTimeouts = chainSyncNoTimeouts,
    scSlotLength,
    scSchedule,
    scDebug = False
  }
  where
    scSlotLength = slotLengthFromSec 20

-- | Enable debug tracing during a scheduler test.
debugScheduler :: SchedulerConfig -> SchedulerConfig
debugScheduler conf = conf { scDebug = True }

basicChainSyncClient :: forall m.
  IOLike m =>
  Tracer m String ->
  TopLevelConfig TestBlock ->
  ChainDbView m TestBlock ->
  StrictTVar m TestFragH ->
  Consensus ChainSyncClientPipelined TestBlock m
basicChainSyncClient tracer cfg chainDbView varCandidate =
  chainSyncClient
    CSClient.ConfigEnv {
        CSClient.mkPipelineDecision0     = pipelineDecisionLowHighMark 10 20
      , CSClient.tracer                  = mkChainSyncClientTracer tracer
      , CSClient.cfg
      , CSClient.chainDbView
      , CSClient.someHeaderInFutureCheck = dummyHeaderInFutureCheck
      }
    CSClient.DynamicEnv {
        CSClient.version             = maxBound
      , CSClient.controlMessageSTM   = return Continue
      , CSClient.headerMetricsTracer = nullTracer
      , CSClient.varCandidate
      }
  where
    dummyHeaderInFutureCheck ::
      InFutureCheck.SomeHeaderInFutureCheck m TestBlock
    dummyHeaderInFutureCheck =
      InFutureCheck.SomeHeaderInFutureCheck InFutureCheck.HeaderInFutureCheck
      { InFutureCheck.proxyArrival = Proxy
      , InFutureCheck.recordHeaderArrival = \_ -> pure ()
      , InFutureCheck.judgeHeaderArrival = \_ _ _ -> pure ()
      , InFutureCheck.handleHeaderArrival = \_ -> pure Nothing
      }

-- | Run a ChainSync protocol for one peer, consisting of a server and client.
--
-- The connection uses timeouts based on the ASC.
--
-- The client is synchronized with BlockFetch using the supplied 'FetchClientRegistry'.
--
-- Execution is started asynchronously, returning an action that kills the thread,
-- to allow extraction of a potential exception.
startChainSyncConnectionThread ::
  (IOLike m, MonadTimer m) =>
  ResourceRegistry m ->
  Tracer m String ->
  TopLevelConfig TestBlock ->
  ChainDbView m TestBlock ->
  FetchClientRegistry PeerId (Header TestBlock) TestBlock m ->
  SharedResources m ->
  ChainSyncResources m ->
  SchedulerConfig ->
  StateViewTracers m ->
  StrictTVar m (Map PeerId (StrictTVar m TestFragH)) ->
  m ()
startChainSyncConnectionThread
  registry
  tracer
  cfg
  chainDbView
  fetchClientRegistry
  SharedResources {srPeerId}
  ChainSyncResources {csrServer}
  SchedulerConfig {scChainSyncTimeouts}
  StateViewTracers {svtChainSyncExceptionsTracer}
  varCandidates
  =
  void $ forkLinkedThread registry ("ChainSyncClient" <> condense srPeerId) $
    bracketSyncWithFetchClient fetchClientRegistry srPeerId $ do
      bracketChainSyncClient nullTracer chainDbView varCandidates srPeerId ntnVersion $ \ varCandidate -> do
        res <- try $ runConnectedPeersPipelinedWithLimits
          createConnectedChannels
          nullTracer
          codecChainSyncId
          chainSyncNoSizeLimits
          (timeLimitsChainSync scChainSyncTimeouts)
          (chainSyncClientPeerPipelined (basicChainSyncClient tracer cfg chainDbView varCandidate))
          (chainSyncServerPeer csrServer)
        case res of
          Left exn -> do
            traceWith svtChainSyncExceptionsTracer $ ChainSyncException srPeerId exn
            case fromException exn of
              Just (ExceededSizeLimit _) ->
                traceUnitWith tracer ("ChainSyncClient " ++ condense srPeerId) "Terminating because of size limit exceeded."
              Just (ExceededTimeLimit _) ->
                traceUnitWith tracer ("ChainSyncClient " ++ condense srPeerId) "Terminating because of time limit exceeded."
              Nothing ->
                pure ()
            case fromException exn of
              Just ThreadKilled ->
                traceUnitWith tracer ("ChainSyncClient " ++ condense srPeerId) "Terminated by GDD governor."
              _ ->
                pure ()
          Right _ -> pure ()
  where
    ntnVersion :: NodeToNodeVersion
    ntnVersion = maxBound

-- | Start the BlockFetch client, using the supplied 'FetchClientRegistry' to
-- register it for synchronization with the ChainSync client.
startBlockFetchConnectionThread ::
  (IOLike m, MonadTime m) =>
  ResourceRegistry m ->
  FetchClientRegistry PeerId (Header TestBlock) TestBlock m ->
  ControlMessageSTM m ->
  SharedResources m ->
  m ()
startBlockFetchConnectionThread registry fetchClientRegistry controlMsgSTM SharedResources {srPeerId, srBlockTree, srCurrentState} =
  void $ forkLinkedThread registry ("BlockFetchClient" <> condense srPeerId) $
    PeerSimulator.BlockFetch.runBlockFetchClient srPeerId fetchClientRegistry controlMsgSTM getCurrentChain
  where
    getCurrentChain = atomically $ do
      nodeState <- readTVar srCurrentState
      case nodeState of
        Nothing -> retry
        Just aps -> do
          let PointSchedule.BlockPoint b = PointSchedule.block aps
          case BT.findFragment (blockPoint b) srBlockTree of
            Just f  -> pure f
            Nothing -> error "block tip is not in the block tree"

-- | The 'Tick' contains a state update for a specific peer.
-- If the peer has not terminated by protocol rules, this will update its TMVar
-- with the new state, thereby unblocking the handler that's currently waiting
-- for new instructions.
dispatchTick ::
  IOLike m =>
  SchedulerConfig ->
  Tracer m String ->
  Map PeerId (PeerResources m) ->
  Tick ->
  m ()
dispatchTick config tracer peers Tick {active = Peer pid state} =
  case peers Map.!? pid of
    Just PeerResources {prChainSync = ChainSyncResources {csrNextState}} -> do
      trace $ "Writing state " ++ condense state
      atomically (tryPutTMVar csrNextState state) >>= \case
        True -> trace $ "Waiting for full resolution of " ++ condense pid ++ "'s tick..."
        False -> trace $ "Client for " ++ condense pid ++ " has ceased operation."
      -- REVIEW: It would be worth sanity-checking that our understanding of
      -- `threadDelay` is correct; and maybe getting explicit information from
      -- both ChainSync & BlockFetch servers that they are done.
      threadDelay (pscTickDuration (scSchedule config))
      trace $ condense pid ++ "'s tick is now done."
    Nothing -> error "“The impossible happened,” as GHC would say."
  where
    trace = traceUnitWith tracer "Scheduler"

-- | Iterate over a 'PointSchedule', sending each tick to the associated peer in turn,
-- giving each peer a chunk of computation time, sequentially, until it satisfies the
-- conditions given by the tick.
-- This usually means for the ChainSync server to have sent the target header to the
-- client.
runScheduler ::
  IOLike m =>
  SchedulerConfig ->
  Tracer m String ->
  PointSchedule ->
  Map PeerId (PeerResources m) ->
  m ()
runScheduler config tracer PointSchedule {ticks=ps} peers = do
  traceWith tracer "Schedule is:"
  for_ ps  $ \tick -> traceWith tracer $ "  " ++ condense tick
  traceStartOfTime
  for_ ps (dispatchTick config tracer peers)
  traceEndOfTime
  where
    traceStartOfTime =
      traceLinesWith tracer [
        hline,
        "Running point schedule ...",
        hline
        ]
    traceEndOfTime =
      traceLinesWith tracer [
        hline,
        "Finished running point schedule"
        ]
    hline = "--------------------------------------------------------------------------------"

-- | Construct STM resources, set up ChainSync and BlockFetch threads, and
-- send all ticks in a 'PointSchedule' to all given peers in turn.
runPointSchedule ::
  forall m.
  (IOLike m, MonadTime m, MonadTimer m) =>
  SchedulerConfig ->
  GenesisTest ->
  PointSchedule ->
  Tracer m String ->
  m StateView
runPointSchedule schedulerConfig GenesisTest {gtSecurityParam = k, gtBlockTree} pointSchedule tracer =
  withRegistry $ \registry -> do
    stateViewTracers <- defaultStateViewTracers
    resources <- makePeerSimulatorResources tracer gtBlockTree (pointSchedulePeers pointSchedule)
    chainDb <- mkChainDb tracer config registry
    fetchClientRegistry <- newFetchClientRegistry
    let chainDbView = CSClient.defaultChainDbView chainDb
    for_ (psrPeers resources) $ \PeerResources {prShared, prChainSync} -> do
      startChainSyncConnectionThread registry tracer config chainDbView fetchClientRegistry prShared prChainSync schedulerConfig stateViewTracers (psrCandidates resources)
      PeerSimulator.BlockFetch.startKeepAliveThread registry fetchClientRegistry (srPeerId prShared)
    for_ (psrPeers resources) $ \PeerResources {prShared} ->
      startBlockFetchConnectionThread registry fetchClientRegistry (pure Continue) prShared
    -- The block fetch logic needs to be started after the block fetch clients
    -- otherwise, an internal assertion fails because getCandidates yields more
    -- peer fragments than registered clients.
    let getCandidates = traverse readTVar =<< readTVar (psrCandidates resources)
    PeerSimulator.BlockFetch.startBlockFetchLogic registry chainDb fetchClientRegistry getCandidates
    runScheduler schedulerConfig tracer pointSchedule (psrPeers resources)
    snapshotStateView stateViewTracers chainDb
  where
    config = defaultCfg k

-- | Create a ChainDB and start a BlockRunner that operate on the peers'
-- candidate fragments.
mkChainDb ::
  IOLike m =>
  Tracer m String ->
  TopLevelConfig TestBlock ->
  ResourceRegistry m ->
  m (ChainDB m TestBlock)
mkChainDb tracer nodeCfg registry = do
    chainDbArgs <- do
      mcdbNodeDBs <- emptyNodeDBs
      pure $ (
        fromMinimalChainDbArgs MinimalChainDbArgs {
            mcdbTopLevelConfig = nodeCfg
          , mcdbChunkInfo      = mkTestChunkInfo nodeCfg
          , mcdbInitLedger     = testInitExtLedger
          , mcdbRegistry       = registry
          , mcdbNodeDBs
          }
        ) {
            cdbTracer = mkCdbTracer tracer
        }
    (_, (chainDB, ChainDB.Impl.Internal{intAddBlockRunner})) <-
      allocate
        registry
        (\_ -> ChainDB.Impl.openDBInternal chainDbArgs False)
        (ChainDB.closeDB . fst)
    _ <- forkLinkedThread registry "AddBlockRunner" intAddBlockRunner
    pure chainDB
