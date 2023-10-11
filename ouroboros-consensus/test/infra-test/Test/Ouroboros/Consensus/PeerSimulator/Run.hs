{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Test.Ouroboros.Consensus.PeerSimulator.Run (runPointSchedule) where

import           Control.Monad.Class.MonadAsync
                     (AsyncCancelled (AsyncCancelled))
import           Control.Monad.Class.MonadTime (MonadTime)
import           Control.Monad.Class.MonadTimer.SI (MonadTimer)
import           Control.Tracer (Tracer (Tracer), nullTracer, traceWith)
import           Data.Foldable (for_)
import           Data.Functor (void)
import           Data.List.NonEmpty (NonEmpty, nonEmpty)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Traversable (for)
import           Ouroboros.Consensus.Config (SecurityParam, TopLevelConfig (..))
import qualified Ouroboros.Consensus.HardFork.History.EraParams as HardFork
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client (ChainDbView,
                     ChainSyncClientResult, Consensus, chainSyncClient,
                     defaultChainDbView)
import           Ouroboros.Consensus.Storage.ChainDB.API
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl
                     (ChainDbArgs (cdbTracer))
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDB.Impl
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.IOLike
                     (Exception (fromException, toException), IOLike,
                     MonadAsync (Async, async, cancel, poll), MonadCatch (try),
                     MonadDelay (threadDelay), MonadSTM (atomically, retry),
                     MonadThrow (throwIO), SomeException, StrictTVar, readTVar,
                     tryPutTMVar)
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Network.Block (blockPoint)
import           Ouroboros.Network.BlockFetch (FetchClientRegistry,
                     bracketSyncWithFetchClient, newFetchClientRegistry)
import           Ouroboros.Network.Channel (createConnectedChannels)
import           Ouroboros.Network.ControlMessage (ControlMessage (..),
                     ControlMessageSTM)
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
import qualified Test.Ouroboros.Consensus.BlockTree as BT
import           Test.Ouroboros.Consensus.BlockTree (BlockTree)
import           Test.Ouroboros.Consensus.ChainGenerator.Params (Asc)
import qualified Test.Ouroboros.Consensus.PeerSimulator.BlockFetch as PeerSimulator.BlockFetch
import           Test.Ouroboros.Consensus.PeerSimulator.Config
import           Test.Ouroboros.Consensus.PeerSimulator.Resources
import           Test.Ouroboros.Consensus.PeerSimulator.Trace
import qualified Test.Ouroboros.Consensus.PointSchedule as PointSchedule
import           Test.Ouroboros.Consensus.PointSchedule (Peer (Peer), PeerId,
                     PointSchedule (PointSchedule), TestFragH, Tick (Tick),
                     pointSchedulePeers)
import           Test.Util.ChainDB
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (Header (..), TestBlock, testInitExtLedger)

-- | A handle to a thread running a connection between
-- typed-protocols peers
newtype ConnectionThread m =
  ConnectionThread {
    kill :: m (Either SomeException ChainSyncClientResult)
  }

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

-- | Run a ChainSync protocol for one peer, consisting of a server and client.
--
-- The connection uses timeouts based on the ASC.
--
-- The client is synchronized with BlockFetch using the supplied 'FetchClientRegistry'.
--
-- Execution is started asynchronously, returning an action that kills the thread,
-- to allow extraction of a potential exception.
--
-- TODO this should use 'forkLinkedThread' instead of 'async'.
startChainSyncConnectionThread ::
  (IOLike m, MonadTimer m) =>
  Tracer m String ->
  TopLevelConfig TestBlock ->
  Asc ->
  ChainDbView m TestBlock ->
  FetchClientRegistry PeerId (Header TestBlock) TestBlock m ->
  SharedResources m ->
  ChainSyncResources m ->
  m (ConnectionThread m)
startChainSyncConnectionThread tracer cfg activeSlotCoefficient chainDbView fetchClientRegistry SharedResources {srPeerId, srCandidateFragment} ChainSyncResources {csrServer} = do
  let
    slotLength = HardFork.eraSlotLength . topLevelConfigLedger $ cfg
    timeouts = chainSyncTimeouts slotLength activeSlotCoefficient
  traceWith tracer $ "timeouts:"
  traceWith tracer $ "  canAwait = " ++ show (canAwaitTimeout timeouts)
  traceWith tracer $ "  intersect = " ++ show (intersectTimeout timeouts)
  traceWith tracer $ "  mustReply = " ++ show (mustReplyTimeout timeouts)
  handle <- async $ do
    bracketSyncWithFetchClient fetchClientRegistry srPeerId $ do
      res <- try $ runConnectedPeersPipelinedWithLimits
        createConnectedChannels
        protocolTracer
        codecChainSyncId
        chainSyncNoSizeLimits
        (timeLimitsChainSync timeouts)
        (chainSyncClientPeerPipelined (basicChainSyncClient tracer cfg chainDbView srCandidateFragment))
        (chainSyncServerPeer csrServer)
      case res of
        Left exn -> do
          case fromException exn of
            Just (ExceededSizeLimit _) ->
              traceUnitWith tracer ("ChainSyncClient " ++ condense srPeerId) "Terminating because of size limit exceeded."
            Just (ExceededTimeLimit _) ->
              traceUnitWith tracer ("ChainSyncClient " ++ condense srPeerId) "Terminating because of time limit exceeded."
            Nothing ->
              pure ()
          throwIO exn
        Right res' -> pure res'

  let kill = fmap fst <$> cancelPoll handle
  pure (ConnectionThread kill)
  where
    protocolTracer = Tracer $ \case
      (clientOrServer, TraceSendMsg payload) ->
        traceUnitWith
          tracer
          ("Protocol ChainSync " ++ condense srPeerId)
          (case clientOrServer of
             Client -> "Client -> Server"
             Server -> "Server -> Client"
           ++ ": " ++ show payload)
      _ -> pure ()

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

-- | Check whether the supplied thread has terminated and kill it otherwise.
--
-- NOTE: io-sim provides 'cancel' which does not propagate already existing
-- exceptions from the thread.
cancelPoll :: MonadAsync m => Async m a -> m (Either SomeException a)
cancelPoll a =
  poll a >>= \case
    Just result ->
      -- Thread is already terminated (with either an exception or a value)
      pure result
    Nothing -> do
      -- Thread hasn't terminated yet.
      cancel a
      pure $ Left $ toException AsyncCancelled

-- | Kill all supplied threads and collect all exceptions that have occurred.
killAll ::
  IOLike m =>
  [ConnectionThread m] ->
  m (Maybe (NonEmpty SomeException))
killAll connectionThreads = do
  results <- for connectionThreads kill
  pure $ nonEmpty $ flip mapMaybe results $ \case
    Left exn ->
      case fromException exn of
        Just AsyncCancelled -> Nothing
        Nothing             -> Just exn
    Right _ -> Nothing

-- | The 'Tick' contains a state update for a specific peer.
-- If the peer has not terminated by protocol rules, this will update its TMVar
-- with the new state, thereby unblocking the handler that's currently waiting
-- for new instructions.
dispatchTick ::
  IOLike m =>
  Tracer m String ->
  Map PeerId (PeerResources m) ->
  Tick ->
  m ()
dispatchTick tracer peers Tick {active = Peer pid state} =
  case peers Map.!? pid of
    Just PeerResources {prChainSync = ChainSyncResources {csrNextState}} -> do
      trace $ "Writing state " ++ condense state
      atomically (tryPutTMVar csrNextState state) >>= \case
        True -> trace $ "Waiting for full resolution of " ++ condense pid ++ "'s tick..."
        False -> trace $ "Client for " ++ condense pid ++ " has ceased operation."
      threadDelay 0.100
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
  Tracer m String ->
  PointSchedule ->
  Map PeerId (PeerResources m) ->
  m ()
runScheduler tracer (PointSchedule ps) peers = do
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

-- | Construct STM resources, set up ChainSync and BlockFetch threads, and
-- send all ticks in a 'PointSchedule' to all given peers in turn.
runPointSchedule ::
  (IOLike m, MonadTime m, MonadTimer m) =>
  SecurityParam ->
  Asc ->
  PointSchedule ->
  Tracer m String ->
  BlockTree TestBlock ->
  m (Either (NonEmpty SomeException) TestFragH)
runPointSchedule k asc pointSchedule tracer blockTree =
  withRegistry $ \registry -> do
    resources <- makePeersResources tracer blockTree (pointSchedulePeers pointSchedule)
    let candidates = srCandidateFragment . prShared <$> resources
    traceWith tracer $ "Security param k = " ++ show k
    chainDb <- mkChainDb tracer candidates config registry
    fetchClientRegistry <- newFetchClientRegistry
    let chainDbView = defaultChainDbView chainDb
    chainSyncThreads <- for resources $ \PeerResources {prShared, prChainSync} -> do
      thread <- startChainSyncConnectionThread tracer config asc chainDbView fetchClientRegistry prShared prChainSync
      PeerSimulator.BlockFetch.startKeepAliveThread registry fetchClientRegistry (srPeerId prShared)
      pure thread
    for_ resources $ \PeerResources {prShared} ->
      startBlockFetchConnectionThread registry fetchClientRegistry (pure Continue) prShared
    -- The block fetch logic needs to be started after the block fetch clients
    -- otherwise, an internal assertion fails because getCandidates yields more
    -- peer fragments than registered clients.
    let getCandidates = traverse readTVar candidates
    PeerSimulator.BlockFetch.startBlockFetchLogic registry chainDb fetchClientRegistry getCandidates
    runScheduler tracer pointSchedule resources
    res <- killAll (Map.elems chainSyncThreads)
    b <- atomically $ ChainDB.getCurrentChain chainDb
    pure $ maybe (Right b) Left res
  where
    config = defaultCfg k

-- | Create a ChainDB and start a BlockRunner that operate on the peers'
-- candidate fragments.
mkChainDb ::
  IOLike m =>
  Tracer m String ->
  Map PeerId (StrictTVar m TestFragH) ->
  TopLevelConfig TestBlock ->
  ResourceRegistry m ->
  m (ChainDB m TestBlock)
mkChainDb tracer _candidateVars nodeCfg registry = do
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
