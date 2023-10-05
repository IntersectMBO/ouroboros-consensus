{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Test.Ouroboros.Consensus.PeerSimulator.Run (runPointSchedule) where

import           Control.Monad.Class.MonadAsync
                     (AsyncCancelled (AsyncCancelled))
import           Control.Monad.Class.MonadTimer.SI (MonadTimer)
import           Control.Monad.State.Strict (StateT, evalStateT, get, gets,
                     lift, modify')
import           Control.Tracer (Tracer (Tracer), nullTracer, traceWith)
import           Data.Foldable (for_)
import           Data.Functor (void)
import           Data.List.NonEmpty (NonEmpty, nonEmpty)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Traversable (for)
import           Ouroboros.Consensus.Block.Abstract (Point (..))
import           Ouroboros.Consensus.Config (SecurityParam, TopLevelConfig (..))
import qualified Ouroboros.Consensus.HardFork.History.EraParams as HardFork
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client (ChainDbView,
                     Consensus, chainSyncClient, defaultChainDbView)
import           Ouroboros.Consensus.Storage.ChainDB.API
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as InvalidBlockPunishment
import           Ouroboros.Consensus.Storage.ChainDB.Impl
                     (ChainDbArgs (cdbTracer))
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDB.Impl
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.IOLike
                     (Exception (fromException, toException), IOLike,
                     MonadAsync (Async, async, cancel, poll), MonadCatch (try),
                     MonadDelay (threadDelay),
                     MonadSTM (atomically, writeTQueue), MonadThrow (throwIO),
                     SomeException, StrictTVar, readTVar, putTMVar, tryPutTMVar)
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (blockUntilChanged)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (blockPoint)
import           Ouroboros.Network.Channel (createConnectedChannels)
import           Ouroboros.Network.ControlMessage (ControlMessage (..))
import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.Driver.Limits.Extras
import           Ouroboros.Network.Driver.Simple (Role (Client, Server))
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
                     (ChainSyncClientPipelined, chainSyncClientPeerPipelined)
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
                     (pipelineDecisionLowHighMark)
import           Ouroboros.Network.Protocol.ChainSync.Server
                     (chainSyncServerPeer)
import           Test.Ouroboros.Consensus.ChainGenerator.Params (Asc)
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.PointSchedule
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.Sync
                     (ConnectionThread (..), TestResources (..), defaultCfg)
import           Test.Ouroboros.Consensus.PeerSimulator.Resources
import           Test.Ouroboros.Consensus.PeerSimulator.Trace
import           Test.Util.ChainDB
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (Header (..), TestBlock, testInitExtLedger)

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
  (IOLike m, MonadTimer m) =>
  Tracer m String ->
  Asc ->
  ChainDbView m TestBlock ->
  PeerId ->
  ChainSyncResources m ->
  StateT (TestResources m) m ()
startChainSyncConnectionThread tracer activeSlotCoefficient chainDbView peerId ChainSyncResources {csrCandidateFragment, csrServer} = do
  cfg <- gets topConfig
  let slotLength = HardFork.eraSlotLength . topLevelConfigLedger $ cfg
  let timeouts = chainSyncTimeouts slotLength activeSlotCoefficient
  lift $ do
    traceWith tracer $ "timeouts:"
    traceWith tracer $ "  canAwait = " ++ show (canAwaitTimeout timeouts)
    traceWith tracer $ "  intersect = " ++ show (intersectTimeout timeouts)
    traceWith tracer $ "  mustReply = " ++ show (mustReplyTimeout timeouts)
  handle <- lift $ async $ do
    res <- try $ runConnectedPeersPipelinedWithLimits
      createConnectedChannels
      protocolTracer
      codecChainSyncId
      chainSyncNoSizeLimits
      (timeLimitsChainSync timeouts)
      (chainSyncClientPeerPipelined (basicChainSyncClient tracer cfg chainDbView csrCandidateFragment))
      (chainSyncServerPeer csrServer)
    case res of
      Left exn -> do
        case fromException exn of
          Just (ExceededSizeLimit _) ->
            traceUnitWith tracer ("ChainSyncClient " ++ condense peerId) "Terminating because of size limit exceeded."
          Just (ExceededTimeLimit _) ->
            traceUnitWith tracer ("ChainSyncClient " ++ condense peerId) "Terminating because of time limit exceeded."
          Nothing ->
            pure ()
        throwIO exn
      Right res' -> pure res'

  let kill = cancelPoll handle
  modify' $ \ TestResources {..} -> TestResources {connectionThreads = ConnectionThread {..} : connectionThreads, ..}
  where
    protocolTracer = Tracer $ \case
      (clientOrServer, TraceSendMsg payload) ->
        traceUnitWith
          tracer
          ("Protocol ChainSync " ++ condense peerId)
          (case clientOrServer of
             Client -> "Client -> Server"
             Server -> "Server -> Client"
           ++ ": " ++ show payload)
      _ -> pure ()

-- | NOTE: io-sim provides 'cancel' which does not propagate already existing
-- exceptions from the thread.
cancelPoll :: MonadAsync m => Async m a -> m (Either SomeException a)
cancelPoll a = do
  poll a >>= \case
    Just result ->
      -- Thread is already terminated (with either an exception or a value)
      pure result
    Nothing -> do
      -- Thread hasn't terminated yet.
      cancel a
      pure $ Left $ toException AsyncCancelled

killAll ::
  IOLike m =>
  TestResources m ->
  m (Maybe (NonEmpty SomeException))
killAll TestResources {..} = do
  results <- for connectionThreads kill
  pure $ nonEmpty $ flip mapMaybe results $ \case
    Left exn ->
      case fromException exn of
        Just AsyncCancelled -> Nothing
        Nothing             -> Just exn
    Right _ -> Nothing

dispatchTick ::
  IOLike m =>
  Tracer m String ->
  Map PeerId (ChainSyncResources m) ->
  Tick ->
  m ()
dispatchTick tracer peers Tick {active = Peer pid state} =
  case peers Map.!? pid of
    Just ChainSyncResources {csrNextState} -> do
      trace $ "Writing state " ++ condense state
      atomically (tryPutTMVar csrNextState state) >>= \case
        True -> trace $ "Waiting for full resolution of " ++ condense pid ++ "'s tick..."
        False -> trace $ "Client for " ++ condense pid ++ " has ceased operation."
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
  (IOLike m, MonadTimer m) =>
  SecurityParam ->
  Asc ->
  PointSchedule ->
  Map PeerId (ChainSyncServerState m) ->
  Tracer m String ->
  m (Either (NonEmpty SomeException) TestFragH)
runPointSchedule k asc pointSchedule peers tracer =
  withRegistry $ \registry -> do
    resources <- Map.traverseWithKey (makeChainSyncResources tracer) peers
    flip evalStateT TestResources {topConfig = defaultCfg k, connectionThreads = [], pointSchedule, registry} $ do
      a <- setup resources
      s <- get
      runScheduler tracer resources
      res <- lift (killAll s)
      b <- lift $ atomically $ ChainDB.getCurrentChain a
      pure $ maybe (Right b) Left res
  where
    setup resources = do
      st <- get
      lift $ traceWith tracer $ "Security param k = " ++ show k
      chainDb <- lift $ mkChainDb tracer (csrCandidateFragment <$> resources) (topConfig st) (registry st)
      let chainDbView = defaultChainDbView chainDb
      void $ Map.traverseWithKey (startChainSyncConnectionThread tracer asc chainDbView) resources
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
