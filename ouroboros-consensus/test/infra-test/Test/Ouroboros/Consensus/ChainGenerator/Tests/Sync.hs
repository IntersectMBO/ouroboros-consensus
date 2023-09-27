{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Ouroboros.Consensus.ChainGenerator.Tests.Sync where

import           Cardano.Crypto.DSIGN (SignKeyDSIGN (..), VerKeyDSIGN (..))
import           Cardano.Slotting.Time (SlotLength, slotLengthFromSec)
import           Control.Monad.State.Strict (StateT, evalStateT, get, gets,
                     lift, modify')
import           Control.Tracer (Tracer (Tracer), nullTracer, traceWith)
import           Data.Coerce (coerce)
import           Data.Foldable (for_, traverse_)
import           Data.Functor (void)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Data.Monoid (First (First, getFirst))
import           Data.Time.Clock (diffTimeToPicoseconds)
import           Network.TypedProtocol.Channel (createConnectedChannels)
import           Network.TypedProtocol.Driver.Simple
                     (runConnectedPeersPipelined)
import           Ouroboros.Consensus.Block (WithOrigin (Origin))
import           Ouroboros.Consensus.Block.Abstract (Header, Point (..),
                     getHeader)
import           Ouroboros.Consensus.Config (SecurityParam, TopLevelConfig (..))
import qualified Ouroboros.Consensus.HardFork.History.EraParams as HardFork
                     (EraParams, defaultEraParams)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client (ChainDbView,
                     ChainSyncClientException, Consensus,
                     TraceChainSyncClientEvent (..), chainSyncClient,
                     defaultChainDbView)
import           Ouroboros.Consensus.Node.ProtocolInfo
                     (NumCoreNodes (NumCoreNodes))
import           Ouroboros.Consensus.NodeId (CoreNodeId (CoreNodeId),
                     NodeId (CoreId))
import           Ouroboros.Consensus.Protocol.BFT
                     (BftParams (BftParams, bftNumNodes, bftSecurityParam),
                     ConsensusConfig (BftConfig, bftParams, bftSignKey, bftVerKeys))
import           Ouroboros.Consensus.Storage.ChainDB.API
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as InvalidBlockPunishment
import           Ouroboros.Consensus.Storage.ChainDB.Impl
                     (ChainDbArgs (cdbTracer))
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDB.Impl
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types
                     (NewTipInfo (..), TraceAddBlockEvent (..))
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.IOLike (IOLike, MonadMonotonicTime,
                     StrictTVar, TQueue, Time (Time), async, atomically, cancel,
                     getMonotonicTime, newTQueueIO, race, readTQueue, readTVar,
                     readTVarIO, threadDelay, try, uncheckedNewTVarM, waitCatch,
                     writeTQueue, writeTVar)
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (blockUntilChanged)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (Tip (..), blockPoint, getTipPoint)
import           Ouroboros.Network.ControlMessage (ControlMessage (..))
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
                     (ChainSyncClientPipelined, chainSyncClientPeerPipelined)
import           Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSyncId)
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
                     (pipelineDecisionLowHighMark)
import           Ouroboros.Network.Protocol.ChainSync.Server
                     (ChainSyncServer (..),
                     ServerStIdle (ServerStIdle, recvMsgDoneClient, recvMsgFindIntersect, recvMsgRequestNext),
                     ServerStIntersect (SendMsgIntersectFound, SendMsgIntersectNotFound),
                     ServerStNext (SendMsgRollForward), chainSyncServerPeer)
import           Prelude hiding (log)
import qualified Test.Ouroboros.Consensus.ChainGenerator.Tests.BlockTree as BT
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.PointSchedule
import           Test.Util.ChainDB
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (BlockConfig (TestBlockConfig),
                     CodecConfig (TestBlockCodecConfig), Header (..),
                     StorageConfig (TestBlockStorageConfig), TestBlock,
                     testInitExtLedger)
import           Text.Printf (printf)

data SyncPeer m =
  SyncPeer {
    wait :: m (),
    kill :: m ()
  }

data SyncTest m =
  SyncTest {
    topConfig     :: TopLevelConfig TestBlock,
    peers         :: [SyncPeer m],
    pointSchedule :: PointSchedule,
    registry      :: ResourceRegistry m
  }

defaultCfg :: SecurityParam -> TopLevelConfig TestBlock
defaultCfg secParam = TopLevelConfig {
    topLevelConfigProtocol = BftConfig {
      bftParams  = BftParams {
        bftSecurityParam = secParam
      , bftNumNodes      = NumCoreNodes 2
      }
    , bftSignKey = SignKeyMockDSIGN 0
    , bftVerKeys = Map.fromList [
        (CoreId (CoreNodeId 0), VerKeyMockDSIGN 0)
      , (CoreId (CoreNodeId 1), VerKeyMockDSIGN 1)
      ]
    }
  , topLevelConfigLedger  = eraParams
  , topLevelConfigBlock   = TestBlockConfig numCoreNodes
  , topLevelConfigCodec   = TestBlockCodecConfig
  , topLevelConfigStorage = TestBlockStorageConfig
  }
  where
    slotLength :: SlotLength
    slotLength = slotLengthFromSec 20

    eraParams :: HardFork.EraParams
    eraParams = HardFork.defaultEraParams secParam slotLength

    numCoreNodes = NumCoreNodes 2

-- | The mutable state of a mocked ChainSync server.
--
-- REVIEW: This type name is misleading and should probably be renamed
-- 'MockedChainSyncServerState'; the other functions 'makeMocked...' and
-- 'runMocked...' should also be renamed.
data MockedChainSyncServer m =
  MockedChainSyncServer {
    mcssPeerId              :: PeerId,
    -- ^ REVIEW: Not sure this is the right place for it.
    mcssStateQueue          :: TQueue m NodeState,
    -- ^ A queue of node states coming from the scheduler.
    mcssCurrentState        :: StrictTVar m NodeState,
    -- ^ The current node state, popped from 'mcssStateQueue'.
    mcssCandidateFragment   :: StrictTVar m TestFragH,
    -- ^ REVIEW: Not sure why we need this.
    mcssCurrentIntersection :: StrictTVar m (AF.Point TestBlock),
    -- ^ The current known intersection with the chain of the client.
    mcssBlockTree           :: BT.BlockTree TestBlock,
    -- ^ The block tree in which the test is taking place. In combination to
    -- 'mcssCurrentState' and 'mcssCurrentIntersection', it allows to define
    -- which blocks to serve to the client.
    mcssTracer              :: Tracer m String
    -- ^ A tracer for this specific instance of the server.
  }

makeMockedChainSyncServer ::
  IOLike m =>
  PeerId ->
  Tracer m String ->
  BT.BlockTree TestBlock ->
  m (MockedChainSyncServer m)
makeMockedChainSyncServer mcssPeerId tracer mcssBlockTree = do
  mcssStateQueue <- newTQueueIO
  mcssCurrentState <- uncheckedNewTVarM NodeOffline
  mcssCandidateFragment <- uncheckedNewTVarM $ AF.Empty AF.AnchorGenesis
  mcssCurrentIntersection <- uncheckedNewTVarM $ AF.Point Origin
  let mcssTracer = Tracer $ traceUnitWith tracer ("MockedChainSyncServer " ++ condense mcssPeerId)
  pure MockedChainSyncServer {..}

waitNodeState ::
  IOLike m =>
  MockedChainSyncServer m ->
  m AdvertisedPoints
waitNodeState server@MockedChainSyncServer{..} = do
  newState <- atomically $ do
    newState <- readTQueue mcssStateQueue
    writeTVar mcssCurrentState newState
    pure newState
  case newState of
    NodeOffline                 -> waitNodeState server
    NodeOnline advertisedPoints -> pure advertisedPoints

checkCurrent ::
  IOLike m =>
  MockedChainSyncServer m ->
  m AdvertisedPoints
checkCurrent server@MockedChainSyncServer{..} =
  readTVarIO mcssCurrentState >>= \case
    NodeOffline -> waitNodeState server
    NodeOnline advertisedPoints -> pure advertisedPoints

serveHeader ::
  IOLike m =>
  MockedChainSyncServer m ->
  AdvertisedPoints ->
  m (Maybe (Tip TestBlock, Header TestBlock))
serveHeader MockedChainSyncServer{..} points = do
  intersection <- readTVarIO mcssCurrentIntersection
  trace $ "  last intersection is " ++ condense intersection
  let HeaderPoint header' = header points
      headerPoint = AF.castPoint $ blockPoint header'
  case BT.findPath intersection headerPoint mcssBlockTree of
    Nothing -> error "There should always be a path"
    Just fragmentAhead
      -- If the anchor is the intersection, then the fragment is purely
      -- descendant of the intersection so we can roll forward.
      | AF.anchorPoint fragmentAhead == intersection ->
        case fragmentAhead of
          AF.Empty _ -> do
            trace "  intersection is exactly our header point"
            pure Nothing
          next AF.:< _ -> do
            trace "  intersection is before our header point"
            trace $ "  fragment ahead: " ++ condense fragmentAhead
            atomically $ writeTVar mcssCurrentIntersection $ blockPoint next
            pure $ Just (coerce (tip points), getHeader next)
      -- If the anchor is not the intersection but the fragment is empty, then
      -- the intersection is further than the tip that we can serve.
      | AF.length fragmentAhead == 0 -> do
          trace "  intersection is further than our header point"
          pure Nothing
      -- If the anchor is not the intersection and the fragment is non-empty,
      -- then we require a rollback
      | otherwise -> do
          trace $ "  we will require a rollback to" ++ condense (AF.anchorPoint fragmentAhead)
          trace $ "  fragment: " ++ condense fragmentAhead
          error "Rollback not supported in MockedChainSyncServer"
  where
    trace = traceWith mcssTracer

intersectWith ::
  AnchoredFragment TestBlock ->
  [Point TestBlock] ->
  Maybe (Point TestBlock)
intersectWith fullFrag pts =
  AF.anchorPoint . snd <$> getFirst (foldMap (First . AF.splitAfterPoint fullFrag) pts)

runMockedChainSyncServer ::
  IOLike m =>
  MockedChainSyncServer m ->
  ChainSyncServer (Header TestBlock) (Point TestBlock) (Tip TestBlock) m ()
runMockedChainSyncServer server@MockedChainSyncServer{..} =
  go
  where
    go =
      ChainSyncServer $ pure ServerStIdle {
          recvMsgRequestNext
        , recvMsgFindIntersect
        , recvMsgDoneClient
      }

    recvMsgRequestNext = do
      advertisedPoints <- checkCurrent server
      trace "handling MsgRequestNext"
      trace $ "  points are " ++ condense advertisedPoints
      serveHeader server advertisedPoints >>= \case
        Just (tip, headerToServe) -> do
          trace $ "  gotta serve " ++ condense headerToServe
          trace $ "  tip is      " ++ condense tip
          trace "done handling MsgRequestNext"
          pure $ Left $ SendMsgRollForward headerToServe tip go
        Nothing -> do
          trace "  cannot serve at this point; waiting for node state and starting again"
          void $ waitNodeState server
          recvMsgRequestNext

    recvMsgFindIntersect pts = do
      points <- checkCurrent server
      trace "handling MsgFindIntersect"
      let TipPoint tip' = tip points
          tipPoint = Ouroboros.Network.Block.getTipPoint tip'
          fragment = fromJust $ BT.findFragment tipPoint mcssBlockTree
      case intersectWith fragment pts of
        Nothing -> do
          trace "  no intersection found"
          trace "done handling MsgFindIntersect"
          pure $ SendMsgIntersectNotFound tip' go
        Just intersection -> do
          trace $ "  intersection found: " ++ condense intersection
          atomically $ writeTVar mcssCurrentIntersection intersection
          trace "done handling MsgFindIntersect"
          pure $ SendMsgIntersectFound intersection tip' go

    recvMsgDoneClient = do
      trace "received MsgDoneClient"
      pure ()

    trace = traceWith mcssTracer

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

syncWith ::
  IOLike m =>
  Tracer m String ->
  ChainDbView m TestBlock ->
  MockedChainSyncServer m ->
  StateT (SyncTest m) m ()
syncWith tracer chainDbView server@MockedChainSyncServer{..} = do
  cfg <- gets topConfig
  handle <- lift $ async $ do
    let s = runMockedChainSyncServer server
    runConnectedPeersPipelined
      createConnectedChannels
      nullTracer
      codecChainSyncId
      (chainSyncClientPeerPipelined (basicChainSyncClient tracer cfg chainDbView mcssCandidateFragment))
      (chainSyncServerPeer s)
  let wait = void (waitCatch handle)
      kill = cancel handle
  modify' $ \ SyncTest {..} -> SyncTest {peers = SyncPeer {..} : peers, ..}

awaitAll ::
  IOLike m =>
  SyncTest m ->
  m ()
awaitAll SyncTest {..} =
  void $
  race (threadDelay 100) $
  for_ peers wait

dispatchTick ::
  IOLike m =>
  Tracer m String ->
  Map PeerId (MockedChainSyncServer m) ->
  Tick ->
  m ()
dispatchTick tracer peers Tick {active = Peer pid state} =
  case peers Map.!? pid of
    Just MockedChainSyncServer {mcssStateQueue} -> do
      traceUnitWith tracer "Scheduler" $ "Writing state " ++ condense state
      atomically $ writeTQueue mcssStateQueue state
      traceUnitWith tracer "Scheduler" $ "Waiting for full resolution of " ++ condense pid ++ "'s tick..."
      threadDelay 0.100
      traceUnitWith tracer "Scheduler" $ condense pid ++ "'s tick is now done."
    Nothing -> error "“The impossible happened,” as GHC would say."

runScheduler ::
  IOLike m =>
  Tracer m String ->
  Map PeerId (MockedChainSyncServer m) ->
  StateT (SyncTest m) m ()
runScheduler tracer peers = do
  SyncTest {pointSchedule = PointSchedule ps _} <- get
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

syncTest ::
  IOLike m =>
  Tracer m String ->
  SecurityParam ->
  PointSchedule ->
  Map PeerId (MockedChainSyncServer m) ->
  StateT (SyncTest m) m b ->
  (b -> m a) ->
  m (Either ChainSyncClientException a)
syncTest tracer k pointSchedule peers setup continuation =
  withRegistry $ \registry -> do
    flip evalStateT SyncTest {topConfig = defaultCfg k, peers = [], pointSchedule, registry} $ do
      a <- setup
      s <- get
      runScheduler tracer peers
      res <- lift (try (awaitAll s))
      b <- lift $ continuation a
      pure (b <$ res)

syncPeers ::
  IOLike m =>
  SecurityParam ->
  PointSchedule ->
  Map PeerId (MockedChainSyncServer m) ->
  Tracer m String ->
  m (Either ChainSyncClientException TestFragH)
syncPeers k pointSchedule peers tracer =
  syncTest tracer k pointSchedule peers
    (do
      st <- get
      lift $ traceWith tracer $ "Security param k = " ++ show k
      chainDb <- lift $ mkRealChainDb tracer (mcssCandidateFragment <$> peers) (topConfig st) (registry st)
      let chainDbView = defaultChainDbView chainDb
      traverse_ (syncWith tracer chainDbView) peers
      pure chainDb)
    (atomically . ChainDB.getCurrentChain)

mkRealChainDb ::
  IOLike m =>
  Tracer m String ->
  Map PeerId (StrictTVar m TestFragH) ->
  TopLevelConfig TestBlock ->
  ResourceRegistry m ->
  m (ChainDB m TestBlock)
mkRealChainDb tracer candidateVars nodeCfg registry = do
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

mkCdbTracer ::
  IOLike m =>
  Tracer m String ->
  Tracer m (ChainDB.Impl.TraceEvent TestBlock)
mkCdbTracer tracer =
  Tracer $ \case
    ChainDB.Impl.TraceAddBlockEvent event ->
      case event of
        AddedToCurrentChain _ NewTipInfo {newTipPoint} _ newFragment -> do
          trace "Added to current chain"
          trace $ "New tip: " ++ condense newTipPoint
          trace $ "New fragment: " ++ condense newFragment
        SwitchedToAFork _ NewTipInfo {newTipPoint} _ newFragment -> do
          trace "Switched to a fork"
          trace $ "New tip: " ++ condense newTipPoint
          trace $ "New fragment: " ++ condense newFragment
        _ -> pure ()
    _ -> pure ()
  where
    trace = traceUnitWith tracer "ChainDB"

mkChainSyncClientTracer ::
  IOLike m =>
  Tracer m String ->
  Tracer m (TraceChainSyncClientEvent TestBlock)
mkChainSyncClientTracer tracer =
  Tracer $ \case
    TraceRolledBack point ->
      trace $ "Rolled back to: " ++ condense point
    TraceFoundIntersection point _ourTip _theirTip ->
      trace $ "Found intersection at: " ++ condense point
    _ -> pure ()
  where
    trace = traceUnitWith tracer "ChainSyncClient"

-- | Trace using the given tracer, printing the current time (typically the time
-- of the simulation) and the unit name.
traceUnitWith :: MonadMonotonicTime m => Tracer m String -> String -> String -> m ()
traceUnitWith tracer unit msg = do
  time <- getMonotonicTime
  traceWith tracer $ printf "%s %s | %s" (showTime time) unit msg
  where
    showTime :: Time -> String
    showTime (Time time) =
      let ps = diffTimeToPicoseconds time
          milliseconds = (ps `div` 1000000000) `mod` 1000
          seconds = (ps `div` 1000000000000) `rem` 60
          minutes = (ps `div` 1000000000000) `quot` 60
       in printf "%02d:%02d.%03d" minutes seconds milliseconds
