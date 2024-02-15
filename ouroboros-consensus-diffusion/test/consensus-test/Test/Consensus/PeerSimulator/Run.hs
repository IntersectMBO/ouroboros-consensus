{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Test.Consensus.PeerSimulator.Run (
    SchedulerConfig (..)
  , debugScheduler
  , defaultSchedulerConfig
  , runPointSchedule
  ) where

import           Control.Monad.Class.MonadTime (MonadTime)
import           Control.Monad.Class.MonadTime.SI (DiffTime)
import           Control.Monad.Class.MonadTimer.SI (MonadTimer)
import           Control.Tracer (Tracer, nullTracer, traceWith)
import           Data.Foldable (for_)
import           Data.Functor (void)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Ouroboros.Consensus.Config (TopLevelConfig (..))
import           Ouroboros.Consensus.Genesis.Governor (updateLoEFragStall)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client (ChainDbView)
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client as CSClient
import           Ouroboros.Consensus.Storage.ChainDB.API
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl
                     (ChainDbArgs (cdbTracer), cdbLoE)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDB.Impl
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.IOLike (IOLike,
                     MonadDelay (threadDelay), MonadSTM (atomically),
                     StrictTVar, readTVar)
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Network.BlockFetch (FetchClientRegistry,
                     bracketSyncWithFetchClient, newFetchClientRegistry)
import           Ouroboros.Network.ControlMessage (ControlMessage (..),
                     ControlMessageSTM)
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Test.Consensus.Genesis.Setup.GenChains (GenesisTest)
import           Test.Consensus.Network.Driver.Limits.Extras
import qualified Test.Consensus.PeerSimulator.BlockFetch as PeerSimulator.BlockFetch
import           Test.Consensus.PeerSimulator.BlockFetch (runBlockFetchClient,
                     startBlockFetchLogic)
import           Test.Consensus.PeerSimulator.ChainSync (runChainSyncClient)
import           Test.Consensus.PeerSimulator.Config
import           Test.Consensus.PeerSimulator.Resources
import           Test.Consensus.PeerSimulator.StateDiagram
                     (peerSimStateDiagramSTMTracerDebug)
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PeerSimulator.Trace
import qualified Test.Consensus.PointSchedule as PointSchedule
import           Test.Consensus.PointSchedule (GenesisTest (GenesisTest),
                     NodeState, PeerSchedule, TestFragH, peersStatesRelative,
                     prettyPeersSchedule)
import           Test.Consensus.PointSchedule.Peers (Peer (..), PeerId, Peers,
                     getPeerIds)
import           Test.Util.ChainDB
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (Header (..), TestBlock, testInitExtLedger)

-- | Behavior config for the scheduler.
data SchedulerConfig =
  SchedulerConfig {
    -- | Whether to enable timeouts for the ChainSync protocol. The value of
    -- timeouts themselves is defined in 'GenesisTest'.
      scEnableChainSyncTimeouts :: Bool

    -- | If 'True', 'Test.Consensus.Genesis.Setup.runTest' will print traces
    -- to stderr.
    --
    -- Use 'debugScheduler' to toggle it conveniently.
    , scDebug                   :: Bool

    -- | Whether to trace when running the scheduler.
    , scTrace                   :: Bool

    -- | Whether to trace only the current state of the candidates and selection,
    -- which provides a less verbose view of the test progress.
    , scTraceState              :: Bool

    -- | The LoE is degenerate at the moment, so when this is enabled, we use
    -- the stalling version of the fragment updater, which sets it to the shared
    -- prefix of all candidates, in anticipation of the GDDG killing peers,
    -- which never happens.
    -- Just for the purpose of testing that the selection indeed doesn't
    -- advance.
    , scEnableLoE               :: Bool
  }

-- | Default scheduler config
defaultSchedulerConfig :: SchedulerConfig
defaultSchedulerConfig =
  SchedulerConfig {
    scEnableChainSyncTimeouts = False,
    scDebug = False,
    scTrace = True,
    scTraceState = False,
    scEnableLoE = False
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
  (IOLike m, MonadTimer m) =>
  ResourceRegistry m ->
  Tracer m String ->
  TopLevelConfig TestBlock ->
  ChainDbView m TestBlock ->
  FetchClientRegistry PeerId (Header TestBlock) TestBlock m ->
  SharedResources m ->
  ChainSyncResources m ->
  ChainSyncTimeout ->
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
  chainSyncTimeouts_
  tracers
  varCandidates =
    void $
    forkLinkedThread registry ("ChainSyncClient" <> condense srPeerId) $
    bracketSyncWithFetchClient fetchClientRegistry srPeerId $
    runChainSyncClient tracer cfg chainDbView srPeerId csrServer chainSyncTimeouts_ tracers varCandidates

-- | Start the BlockFetch client, using the supplied 'FetchClientRegistry' to
-- register it for synchronization with the ChainSync client.
startBlockFetchConnectionThread ::
  (IOLike m, MonadTime m, MonadTimer m) =>
  ResourceRegistry m ->
  FetchClientRegistry PeerId (Header TestBlock) TestBlock m ->
  ControlMessageSTM m ->
  SharedResources m ->
  BlockFetchResources m ->
  m ()
startBlockFetchConnectionThread
  registry
  fetchClientRegistry
  controlMsgSTM
  SharedResources {srPeerId}
  BlockFetchResources {bfrServer} =
    void $
    forkLinkedThread registry ("BlockFetchClient" <> condense srPeerId) $
    runBlockFetchClient srPeerId fetchClientRegistry controlMsgSTM bfrServer

-- | The 'Tick' contains a state update for a specific peer.
-- If the peer has not terminated by protocol rules, this will update its TMVar
-- with the new state, thereby unblocking the handler that's currently waiting
-- for new instructions.
dispatchTick ::
  IOLike m =>
  Tracer m String ->
  Tracer m () ->
  Map PeerId (PeerResources m) ->
  (Int, (DiffTime, Peer NodeState)) ->
  m ()
dispatchTick tracer stateTracer peers (number, (duration, Peer pid state)) =
  case peers Map.!? pid of
    Just PeerResources {prUpdateState} -> do
      time <- prettyTime
      traceLinesWith tracer [
        hline,
        "Time is " ++ time,
        "Tick:",
        "  number: " ++ show number,
        "  duration: " ++ show duration,
        "  peer: " ++ condense pid,
        "  state: " ++ condense state
        ]
      atomically (prUpdateState state)
      threadDelay duration
      traceWith stateTracer ()
    Nothing -> error "“The impossible happened,” as GHC would say."
  where
    hline = "--------------------------------------------------------------------------------"

-- | Iterate over a 'PointSchedule', sending each tick to the associated peer in turn,
-- giving each peer a chunk of computation time, sequentially, until it satisfies the
-- conditions given by the tick.
-- This usually means for the ChainSync server to have sent the target header to the
-- client.
runScheduler ::
  IOLike m =>
  Tracer m String ->
  Tracer m () ->
  Peers PeerSchedule ->
  Map PeerId (PeerResources m) ->
  m ()
runScheduler tracer stateTracer ps peers = do
  traceLinesWith tracer ("Point schedule:" : map ("  " ++) (prettyPeersSchedule ps))
  traceStartOfTime
  mapM_ (dispatchTick tracer stateTracer peers) (zip [0..] (peersStatesRelative ps))
  traceEndOfTime
  where
    traceStartOfTime =
      traceWith tracer "Running point schedule ..."
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
  GenesisTest (Peers PeerSchedule) ->
  Tracer m String ->
  m StateView
runPointSchedule schedulerConfig genesisTest tracer0 =
  withRegistry $ \registry -> do
    stateViewTracers <- defaultStateViewTracers
    resources <- makePeerSimulatorResources tracer gtBlockTree (getPeerIds gtSchedule)
    let getCandidates = traverse readTVar =<< readTVar (psrCandidates resources)
        updateLoEFrag = updateLoEFragStall k getCandidates
    chainDb <- mkChainDb schedulerConfig tracer config registry updateLoEFrag
    fetchClientRegistry <- newFetchClientRegistry
    let chainDbView = CSClient.defaultChainDbView chainDb
    for_ (psrPeers resources) $ \PeerResources {prShared, prChainSync} -> do
      startChainSyncConnectionThread registry tracer config chainDbView fetchClientRegistry prShared prChainSync chainSyncTimeouts_ stateViewTracers (psrCandidates resources)
      PeerSimulator.BlockFetch.startKeepAliveThread registry fetchClientRegistry (srPeerId prShared)
    for_ (psrPeers resources) $ \PeerResources {prShared, prBlockFetch} ->
      startBlockFetchConnectionThread registry fetchClientRegistry (pure Continue) prShared prBlockFetch
    -- The block fetch logic needs to be started after the block fetch clients
    -- otherwise, an internal assertion fails because getCandidates yields more
    -- peer fragments than registered clients.
    let getCurrentChain = ChainDB.getCurrentChain chainDb
        getPoints = traverse readTVar (srCurrentState . prShared <$> (psrPeers resources))
        mkStateTracer
          | scTraceState schedulerConfig
          = peerSimStateDiagramSTMTracerDebug gtBlockTree getCurrentChain getCandidates getPoints
          | otherwise
          = pure nullTracer
    stateTracer <- mkStateTracer
    startBlockFetchLogic registry chainDb fetchClientRegistry getCandidates
    runScheduler tracer stateTracer gtSchedule (psrPeers resources)
    snapshotStateView stateViewTracers chainDb
  where
    GenesisTest {
        gtSecurityParam = k
      , gtBlockTree
      , gtSchedule
      , gtChainSyncTimeouts
      } = genesisTest

    config = defaultCfg k

    tracer = if scTrace schedulerConfig then tracer0 else nullTracer

    chainSyncTimeouts_ =
      if scEnableChainSyncTimeouts schedulerConfig
        then gtChainSyncTimeouts
        else chainSyncNoTimeouts

-- | Create a ChainDB and start a BlockRunner that operate on the peers'
-- candidate fragments.
mkChainDb ::
  IOLike m =>
  SchedulerConfig ->
  Tracer m String ->
  TopLevelConfig TestBlock ->
  ResourceRegistry m ->
  UpdateLoEFrag m TestBlock ->
  m (ChainDB m TestBlock)
mkChainDb schedulerConfig tracer nodeCfg registry updateLoEFrag = do
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
            cdbTracer = mkCdbTracer tracer,
            cdbLoE
        }
    (_, (chainDB, ChainDB.Impl.Internal{intAddBlockRunner})) <-
      allocate
        registry
        (\_ -> ChainDB.Impl.openDBInternal chainDbArgs False)
        (ChainDB.closeDB . fst)
    _ <- forkLinkedThread registry "AddBlockRunner" intAddBlockRunner
    pure chainDB
  where
    cdbLoE
      | scEnableLoE schedulerConfig = LoEEnabled updateLoEFrag
      | otherwise = LoEDisabled
