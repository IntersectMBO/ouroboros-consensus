{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}

module Test.Ouroboros.Consensus.PeerSimulator.Run (
  runPointSchedule,
) where

import Control.Monad.State.Strict (StateT, evalStateT, get, gets, lift, modify')
import Control.Tracer (nullTracer, traceWith, Tracer)
import Data.Foldable (for_, traverse_)
import Data.Functor (void)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Network.TypedProtocol.Channel (createConnectedChannels)
import Network.TypedProtocol.Driver.Simple (runConnectedPeersPipelined)
import Ouroboros.Consensus.Block.Abstract (Point (..))
import Ouroboros.Consensus.Config (SecurityParam, TopLevelConfig (..))
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client (
  ChainDbView,
  ChainSyncClientException,
  Consensus,
  chainSyncClient,
  defaultChainDbView,
  )
import Ouroboros.Consensus.Storage.ChainDB.API
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as InvalidBlockPunishment
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDB.Impl
import Ouroboros.Consensus.Storage.ChainDB.Impl (ChainDbArgs (cdbTracer))
import Ouroboros.Consensus.Util.Condense (Condense (..))
import Ouroboros.Consensus.Util.IOLike (
  IOLike,
  StrictTVar,
  async,
  atomically,
  race,
  readTVar,
  threadDelay,
  try,
  waitCatch,
  writeTQueue,
  )
import Ouroboros.Consensus.Util.ResourceRegistry
import Ouroboros.Consensus.Util.STM (blockUntilChanged)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block (blockPoint)
import Ouroboros.Network.ControlMessage (ControlMessage (..))
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined (ChainSyncClientPipelined, chainSyncClientPeerPipelined)
import Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSyncId)
import Ouroboros.Network.Protocol.ChainSync.PipelineDecision (pipelineDecisionLowHighMark)
import Ouroboros.Network.Protocol.ChainSync.Server (chainSyncServerPeer)
import Test.Util.ChainDB
import Test.Util.Orphans.IOLike ()
import Test.Util.TestBlock (Header (..), TestBlock, testInitExtLedger)

import Test.Ouroboros.Consensus.ChainGenerator.Tests.PointSchedule
import Test.Ouroboros.Consensus.ChainGenerator.Tests.Sync (ConnectionThread (..), TestResources (..), defaultCfg)
import Test.Ouroboros.Consensus.PeerSimulator.Resources
import Test.Ouroboros.Consensus.PeerSimulator.Trace

basicChainSyncClient ::
  IOLike m =>
  Tracer m String ->
  TopLevelConfig TestBlock ->
  ChainDbView m TestBlock ->
  StrictTVar m TestFragH ->
  Consensus ChainSyncClientPipelined TestBlock m
basicChainSyncClient tracer cfg chainDbView varCandidate =
  chainSyncClient
    (pipelineDecisionLowHighMark 10 20)
    (mkChainSyncClientTracer tracer)
    cfg
    chainDbView
    maxBound
    (return Continue)
    nullTracer
    varCandidate

startChainSyncConnectionThread ::
  IOLike m =>
  Tracer m String ->
  ChainDbView m TestBlock ->
  ChainSyncResources m ->
  StateT (TestResources m) m ()
startChainSyncConnectionThread tracer chainDbView ChainSyncResources {csrCandidateFragment, csrServer} = do
  cfg <- gets topConfig
  handle <- lift $ async $ do
    runConnectedPeersPipelined
      createConnectedChannels
      nullTracer
      codecChainSyncId
      (chainSyncClientPeerPipelined (basicChainSyncClient tracer cfg chainDbView csrCandidateFragment))
      (chainSyncServerPeer csrServer)
  let wait = void (waitCatch handle)
  modify' $ \ TestResources {..} -> TestResources {connectionThreads = ConnectionThread {..} : connectionThreads, ..}

awaitAll ::
  IOLike m =>
  TestResources m ->
  m ()
awaitAll TestResources {..} =
  void $
  race (threadDelay 100) $
  for_ connectionThreads wait

dispatchTick ::
  IOLike m =>
  Tracer m String ->
  Map PeerId (ChainSyncResources m) ->
  Tick ->
  m ()
dispatchTick tracer peers Tick {active = Peer pid state} =
  case peers Map.!? pid of
    Just ChainSyncResources {csrQueue} -> do
      trace $ "Writing state " ++ condense state
      atomically $ writeTQueue csrQueue state
      trace $ "Waiting for full resolution of " ++ condense pid ++ "'s tick..."
      threadDelay 0.100
      trace $ condense pid ++ "'s tick is now done."
    Nothing -> error "“The impossible happened,” as GHC would say."
  where
    trace = traceUnitWith tracer "Scheduler"

runScheduler ::
  IOLike m =>
  Tracer m String ->
  Map PeerId (ChainSyncResources m) ->
  StateT (TestResources m) m ()
runScheduler tracer peers = do
  TestResources {pointSchedule = PointSchedule ps _} <- get
  lift $ do
    traceWith tracer "Schedule is:"
    for_ ps  $ \tick -> traceWith tracer $ "  " ++ condense tick
    traceWith tracer "--------------------------------------------------------------------------------"
    traceWith tracer "» Time says “Let there be”"
    traceWith tracer "» every moment and instantly"
    traceWith tracer "» there is space and the radiance"
    traceWith tracer "» of each bright galaxy."
    traceWith tracer "--------------------------------------------------------------------------------"
    for_ ps (dispatchTick tracer peers)
    traceWith tracer "--------------------------------------------------------------------------------"
    traceWith tracer "» A Clock stopped -"
    traceWith tracer "» Not the Mantel's -"
    traceWith tracer "» Geneva's farthest skill"
    traceWith tracer "» Can't put the puppet bowing"
    traceWith tracer "» That just now dangled still -"

runPointSchedule ::
  IOLike m =>
  SecurityParam ->
  PointSchedule ->
  Map PeerId (ChainSyncServerState m) ->
  Tracer m String ->
  m (Either ChainSyncClientException TestFragH)
runPointSchedule k pointSchedule peers tracer =
  withRegistry $ \registry -> do
    stuffs <- Map.traverseWithKey (makeChainSyncServerResources tracer) peers
    flip evalStateT TestResources {topConfig = defaultCfg k, connectionThreads = [], pointSchedule, registry} $ do
      a <- setup stuffs
      s <- get
      runScheduler tracer stuffs
      res <- lift (try (awaitAll s))
      b <- lift $ atomically $ ChainDB.getCurrentChain a
      pure (b <$ res)
  where
    setup stuffs = do
      st <- get
      lift $ traceWith tracer $ "Security param k = " ++ show k
      chainDb <- lift $ mkChainDb tracer (csrCandidateFragment <$> stuffs) (topConfig st) (registry st)
      let chainDbView = defaultChainDbView chainDb
      traverse_ (startChainSyncConnectionThread tracer chainDbView) stuffs
      pure chainDb

mkChainDb ::
  IOLike m =>
  Tracer m String ->
  Map PeerId (StrictTVar m TestFragH) ->
  TopLevelConfig TestBlock ->
  ResourceRegistry m ->
  m (ChainDB m TestBlock)
mkChainDb tracer candidateVars nodeCfg registry = do
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
    void $ flip Map.traverseWithKey candidateVars $ \ pid varCandidate ->
      forkLinkedThread registry (condense pid) $ monitorCandidate chainDB varCandidate
    pure chainDB
  where
    monitorCandidate chainDB varCandidate =
        go GenesisPoint
      where
        go candidateTip = do
          ((frag, candidateTip'), isFetched) <- atomically $
            (,)
              <$> blockUntilChanged AF.headPoint candidateTip (readTVar varCandidate)
              <*> ChainDB.getIsFetched chainDB
          let blks =
                  filter (not . isFetched . blockPoint)
                $ testHeader <$> AF.toOldestFirst frag
          for_ blks $ ChainDB.addBlock_ chainDB InvalidBlockPunishment.noPunishment
          go candidateTip'
