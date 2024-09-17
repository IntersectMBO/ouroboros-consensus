{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.PeerSimulator.NodeLifecycle (
    LiveInterval (..)
  , LiveIntervalResult (..)
  , LiveNode (..)
  , LiveResources (..)
  , NodeLifecycle (..)
  , lifecycleStart
  , lifecycleStop
  , restoreNode
  ) where

import           Control.Tracer (Tracer (..), traceWith)
import           Data.Functor (void)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config (TopLevelConfig (..))
import           Ouroboros.Consensus.Storage.ChainDB.API
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Args (cdbsLoE,
                     updateTracer)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified System.FS.Sim.MockFS as MockFS
import           System.FS.Sim.MockFS (MockFS)
import           Test.Consensus.PeerSimulator.Resources
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PeerSimulator.Trace
import           Test.Consensus.PointSchedule.Peers (PeerId)
import           Test.Util.ChainDB
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (TestBlock, testInitExtLedger)

-- | Resources used for a single live interval of the node, constructed when the
-- node is started.
-- When the node is shut down, 'lnCopyToImmDb' is used to persist the current
-- chain.
data LiveNode blk m = LiveNode {
    lnChainDb          :: ChainDB m blk
  , lnStateViewTracers :: StateViewTracers blk m
  , lnStateTracer      :: Tracer m ()

    -- | Write persistent ChainDB state (the immutable and volatile DBs, but not
    -- the ledger and GSM state) to the VFS TVars to preserve it for the next
    -- interval.
    -- Returns the immutable tip's slot for tracing.
  , lnCopyToImmDb      :: m (WithOrigin SlotNo)

    -- | The set of peers that should be started.
    -- Based on the simulation results at node shutdown, disconnected peers are
    -- removed for the next live interval.
  , lnPeers            :: Set PeerId
  }

-- | Result of a node shutdown at the end of a live interval.
data LiveIntervalResult blk = LiveIntervalResult {
    -- | Used to initialize the 'StateViewTracers' of the next run to preserve
    -- earlier disconnections for the final result.
    lirPeerResults :: [PeerSimulatorResult blk]

    -- | The remaining peers, computed by removing all peers present in
    -- 'lrPeerResults' from the current state in 'lnPeers'.
  , lirActive      :: Set PeerId
  }

-- | Resources used by the handlers 'lifecycleStart' and 'lifecycleStop' to
-- shut down running components, construct tracers used for single intervals,
-- and reset and persist state.
data LiveResources blk m = LiveResources {
    lrRegistry :: ResourceRegistry m
  , lrPeerSim  :: PeerSimulatorResources m blk
  , lrTracer   :: Tracer m (TraceEvent blk)
  , lrSTracer  :: ChainDB m blk -> m (Tracer m ())
  , lrConfig   :: TopLevelConfig blk

    -- | The chain DB state consists of several transient parts and the
    -- immutable DB's virtual file system.
    -- After 'lnCopyToImmDb' was executed, the latter will contain the final
    -- state of an interval.
    -- The rest is reset when the chain DB is recreated.
  , lrCdb      :: NodeDBs (StrictTMVar m MockFS)

    -- | The LoE fragment must be reset for each live interval.
  , lrLoEVar   :: LoE (StrictTVar m (AnchoredFragment (Header blk)))
  }

data LiveInterval blk m = LiveInterval {
    liResources :: LiveResources blk m
  , liResult    :: LiveIntervalResult blk
  , liNode      :: LiveNode blk m
  }

-- | Handlers for starting the node and shutting it down for each live interval,
-- using the state of the previous run.
data NodeLifecycle blk m = NodeLifecycle {
    -- | The minimum tick duration that triggers a node downtime.
    -- If this is 'Nothing', downtimes are disabled.
    nlMinDuration :: Maybe DiffTime

    -- | Start the node with prior state.
    -- For the first start, this must be called with an empty 'lirPeerResults'
    -- and the initial set of all peers in 'lirActive'.
  , nlStart       :: LiveIntervalResult blk -> m (LiveNode blk m)
  , nlShutdown    :: LiveNode blk m -> m (LiveIntervalResult blk)
  }

-- | Create a ChainDB and start a BlockRunner that operate on the peers'
-- candidate fragments.
mkChainDb ::
  IOLike m =>
  LiveResources TestBlock m ->
  m (ChainDB m TestBlock, m (WithOrigin SlotNo))
mkChainDb resources = do
    atomically $ do
      -- Reset only the non-persisted state of the ChainDB's file system mocks:
      -- - GSM state and Ledger DB are discarded
      -- - Immutable DB and Volatile DB are preserved for the next interval
      void $ swapTMVar (nodeDBsGsm lrCdb) MockFS.empty
      void $ swapTMVar (nodeDBsLgr lrCdb) MockFS.empty
    chainDbArgs <- do
      let args = updateTracer
            (Tracer (traceWith lrTracer . TraceChainDBEvent))
            (fromMinimalChainDbArgs MinimalChainDbArgs {
              mcdbTopLevelConfig = lrConfig
            , mcdbChunkInfo      = mkTestChunkInfo lrConfig
            , mcdbInitLedger     = testInitExtLedger
            , mcdbRegistry       = lrRegistry
            , mcdbNodeDBs        = lrCdb
            })
      pure $ args { ChainDB.cdbsArgs = (ChainDB.cdbsArgs args) {
        cdbsLoE = traverse readTVarIO lrLoEVar
        } }
    (_, (chainDB, internal)) <- allocate
        lrRegistry
        (\_ -> ChainDB.openDBInternal chainDbArgs False)
        (ChainDB.closeDB . fst)
    let ChainDB.Internal {intCopyToImmutableDB, intAddBlockRunner} = internal
    void $ forkLinkedThread lrRegistry "AddBlockRunner" (void intAddBlockRunner)
    pure (chainDB, intCopyToImmutableDB)
  where
    LiveResources {lrRegistry, lrTracer, lrConfig, lrCdb, lrLoEVar} = resources

-- | Allocate all the resources that depend on the results of previous live
-- intervals, the ChainDB and its persisted state.
restoreNode ::
  IOLike m =>
  LiveResources TestBlock m ->
  LiveIntervalResult TestBlock ->
  m (LiveNode TestBlock m)
restoreNode resources LiveIntervalResult {lirPeerResults, lirActive} = do
  lnStateViewTracers <- stateViewTracersWithInitial lirPeerResults
  (lnChainDb, lnCopyToImmDb) <- mkChainDb resources
  lnStateTracer <- lrSTracer resources lnChainDb
  pure LiveNode {
      lnChainDb
    , lnStateViewTracers
    , lnStateTracer
    , lnCopyToImmDb
    , lnPeers = lirActive
    }

-- | Allocate resources with 'restoreNode' and pass them to the callback that
-- starts the node's threads.
lifecycleStart ::
  forall m.
  IOLike m =>
  (LiveInterval TestBlock m -> m ()) ->
  LiveResources TestBlock m ->
  LiveIntervalResult TestBlock ->
  m (LiveNode TestBlock m)
lifecycleStart start liResources liResult = do
  trace (TraceSchedulerEvent TraceNodeStartupStart)
  liNode <- restoreNode liResources liResult
  start LiveInterval {liResources, liResult, liNode}
  chain <- atomically (ChainDB.getCurrentChain (lnChainDb liNode))
  trace (TraceSchedulerEvent (TraceNodeStartupComplete chain))
  pure liNode
  where
    trace = traceWith (lrTracer liResources)

-- | Shut down the node by killing all its threads after extracting the
-- persistent state used to restart the node later.
lifecycleStop ::
  (IOLike m, GetHeader blk) =>
  LiveResources blk m ->
  LiveNode blk m ->
  m (LiveIntervalResult blk)
lifecycleStop resources LiveNode {lnStateViewTracers, lnCopyToImmDb, lnPeers} = do
  -- Trigger writing the immutable tip to the MockFS in our TVar for restoring in 'startNode'
  immutableTip <- lnCopyToImmDb
  trace (TraceSchedulerEvent (TraceNodeShutdownStart immutableTip))
  -- Remember which peers were still running before shutdown
  lirPeerResults <- svtGetPeerSimulatorResults lnStateViewTracers
  let disconnectedPeers = Set.fromList (psePeerId <$> lirPeerResults)
      lirActive = lnPeers Set.\\ disconnectedPeers
  -- Killing the peer overview threads should hopefully clean up all connections promptly
  releaseAll lrRegistry
  -- Reset the resources in TVars that were allocated by the simulator
  atomically $ do
    modifyTVar psrHandles (const mempty)
    case lrLoEVar of
      LoEEnabled var -> modifyTVar var (const (AF.Empty AF.AnchorGenesis))
      LoEDisabled    -> pure ()
  trace (TraceSchedulerEvent TraceNodeShutdownComplete)
  pure LiveIntervalResult {lirActive, lirPeerResults}
  where
    trace = traceWith lrTracer
    LiveResources {
        lrRegistry
      , lrTracer
      , lrPeerSim = PeerSimulatorResources {psrHandles}
      , lrLoEVar
      } = resources
