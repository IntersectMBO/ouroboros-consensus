{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Ouroboros.Consensus.ChainGenerator.Tests.Sync where

import           Cardano.Crypto.DSIGN (SignKeyDSIGN (..), VerKeyDSIGN (..))
import           Cardano.Slotting.Slot (SlotNo (unSlotNo))
import           Cardano.Slotting.Time (SlotLength, slotLengthFromSec)
import           Control.Monad (forever)
import           Control.Monad.State.Strict (StateT, evalStateT, get, gets,
                     lift, modify')
import           Control.Tracer (Tracer (Tracer), nullTracer, traceWith)
import           Data.Coerce (coerce)
import           Data.Foldable (for_)
import           Data.Function ((&))
import           Data.Functor (void)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Data.Monoid (First (First, getFirst))
import           Data.Time.Clock (diffTimeToPicoseconds)
import qualified Data.Vector as Vector
import           Network.TypedProtocol.Channel (createConnectedChannels)
import           Network.TypedProtocol.Driver.Simple
                     (runConnectedPeersPipelined)
import           Ouroboros.Consensus.Block.Abstract (Header, Point (..))
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
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     toOldestFirst)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (BlockNo (unBlockNo), Tip (..),
                     blockHash, blockNo, blockPoint, blockSlot)
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
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.PointSchedule
import           Test.Util.ChainDB
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (BlockConfig (TestBlockConfig),
                     CodecConfig (TestBlockCodecConfig), Header (..),
                     StorageConfig (TestBlockStorageConfig), TestBlock,
                     testInitExtLedger, unTestHash)
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

data MockedChainSyncServer m =
  MockedChainSyncServer {
    mcssPeerId            :: PeerId,
    mcssStateQueue        :: TQueue m NodeState,
    mcssCurrentState      :: StrictTVar m NodeState,
    mcssCandidateFragment :: StrictTVar m TestFragH,
    mcssUnservedFragment  :: StrictTVar m TestFragH,
    mcssTracer            :: Tracer m String
  }

makeMockedChainSyncServer ::
  IOLike m =>
  PeerId ->
  TestFragH ->
  Tracer m String ->
  m (MockedChainSyncServer m)
makeMockedChainSyncServer mcssPeerId unservedFragment tracer = do
  mcssStateQueue <- newTQueueIO
  mcssCurrentState <- uncheckedNewTVarM NodeOffline
  mcssCandidateFragment <- uncheckedNewTVarM $ AF.Empty AF.AnchorGenesis
  mcssUnservedFragment <- uncheckedNewTVarM unservedFragment
  let mcssTracer = Tracer $ traceUnitWith tracer ("MockedChainSyncServer " ++ condense mcssPeerId)
  pure MockedChainSyncServer {..}

waitNodeState ::
  IOLike m =>
  MockedChainSyncServer m ->
  m (AdvertisedPoints, TestFragH)
waitNodeState server@MockedChainSyncServer{..} = do
  newState <- atomically $ do
    newState <- readTQueue mcssStateQueue
    writeTVar mcssCurrentState newState
    pure newState
  case newState of
    NodeOffline -> waitNodeState server
    NodeOnline advertisedPoints -> do
      unservedFragment <- readTVarIO mcssUnservedFragment
      pure (advertisedPoints, unservedFragment)

checkCurrent ::
  IOLike m =>
  MockedChainSyncServer m ->
  m (AdvertisedPoints, TestFragH)
checkCurrent server@MockedChainSyncServer{..} =
  readTVarIO mcssCurrentState >>= \case
    NodeOffline -> waitNodeState server
    NodeOnline s -> do
      frag <- readTVarIO mcssUnservedFragment
      pure (s, frag)

-- | Whether the advertised points allow serving at least one header in the
-- fragment.
--
-- FIXME: `HeaderPoint` in `PointSchedule` is a header and not a point; what the
-- hell?
canServeHeader ::
  AdvertisedPoints ->
  TestFragH ->
  Bool
canServeHeader AdvertisedPoints{header = HeaderPoint point} fragment =
  AF.pointOnFragment (blockPoint point) fragment

serveHeader ::
  IOLike m =>
  MockedChainSyncServer m ->
  AdvertisedPoints ->
  TestFragH ->
  m (Maybe (Tip TestBlock, Header TestBlock))
serveHeader MockedChainSyncServer{..} points = \case
  AF.Empty _ -> pure Nothing
  next AF.:< rest -> do
    atomically (writeTVar mcssUnservedFragment rest)
    pure $ Just (coerce $ tip points, next)

intersectWith ::
  AnchoredFragment TestBlock ->
  [Point TestBlock] ->
  Maybe (AnchoredFragment TestBlock)
intersectWith fullFrag pts =
  snd <$> getFirst (foldMap (First . AF.splitAfterPoint fullFrag) pts)

runMockedChainSyncServer ::
  IOLike m =>
  MockedChainSyncServer m ->
  TestFrag ->
  ChainSyncServer (Header TestBlock) (Point TestBlock) (Tip TestBlock) m ()
runMockedChainSyncServer server@MockedChainSyncServer{..} fullFrag =
  go
  where
    go =
      ChainSyncServer $ pure ServerStIdle {
          recvMsgRequestNext
        , recvMsgFindIntersect
        , recvMsgDoneClient
      }

    recvMsgRequestNext = do
      (advertisedPoints, fragment) <- checkCurrent server
      () <- if canServeHeader advertisedPoints fragment
        then pure ()
        else do
          trace "waiting for node state..."
          void $ waitNodeState server
      trace "handling MsgRequestNext"
      trace $ "  points are " ++ condense advertisedPoints
      trace $ "  fragment is " ++ condense fragment
      serveHeader server advertisedPoints fragment >>= \case
        Just (tip, h) -> do
          trace $ "  gotta serve " ++ condense h
          trace $ "  tip is      " ++ condense tip
          trace "done handling MsgRequestNext"
          pure $ Left $ SendMsgRollForward h tip go
        Nothing -> do
          trace "  no more blocks to serve; done forever!"
          trace "done handling MsgRequestNext"
          pure $ Right stall

    recvMsgFindIntersect pts = do
      (points, _) <- checkCurrent server
      trace "handling MsgFindIntersect"
      let theTip = coerce $ tip points
      case intersectWith fullFrag pts of
        Nothing -> do
          trace "  no intersection found"
          trace "done handling MsgFindIntersect"
          pure $ SendMsgIntersectNotFound theTip go
        Just frag -> do
          unservedFragment <- readTVarIO mcssUnservedFragment
          trace $ "  unserved fragment is: " ++ condense unservedFragment
          let intersection = AF.anchorPoint frag
          trace $ "  intersection found: " ++ condense intersection
          case AF.splitAfterPoint unservedFragment intersection of
            Nothing -> do
              trace "  intersection is not in the unserved fragment"
              pure ()
            Just (_dropped, unservedFragment') -> do
              trace "  intersection was in the unserved fragment"
              atomically $ writeTVar mcssUnservedFragment unservedFragment'
              trace $ "  unserved fragment is now: " ++ condense unservedFragment'
          trace "done handling MsgFindIntersect"
          pure $ SendMsgIntersectFound (AF.anchorPoint frag) theTip go

    recvMsgDoneClient = do
      trace "received MsgDoneClient"
      pure ()

    stall = forever $ threadDelay 1000

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
  Peers TestFrag ->
  PeerId ->
  MockedChainSyncServer m ->
  StateT (SyncTest m) m ()
syncWith tracer chainDbView frags pid server@MockedChainSyncServer{..} = do
  cfg <- gets topConfig
  handle <- lift $ async $ do
    let s = runMockedChainSyncServer server (fromJust (getPeer pid frags))
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

slotAndBlockNoFromAnchor :: AF.Anchor b -> (SlotNo, BlockNo)
slotAndBlockNoFromAnchor = \case
  AF.AnchorGenesis -> (0, 0)
  AF.Anchor slotNo _ blockNumber -> (slotNo, blockNumber)

prettyPrintFragments ::
  MonadMonotonicTime m =>
  Tracer m String ->
  Map PeerId TestFragH ->
  m ()
prettyPrintFragments tracer fragments = do
  let honestFragment = fromJust $ fragments Map.!? HonestPeer
  let advFragment = reanchorAdversary $ fromJust $ fragments Map.!? PeerId "adversary"

  let (oSlotNo, oBlockNo) = slotAndBlockNoFromAnchor $ AF.anchor honestFragment
  let (hSlotNo, _) = slotAndBlockNoFromAnchor $ AF.headAnchor honestFragment

  let (aoSlotNo, _) = slotAndBlockNoFromAnchor $ AF.anchor advFragment
  let (ahSlotNo, _) = slotAndBlockNoFromAnchor $ AF.headAnchor advFragment

  let firstSlotNo = min oSlotNo aoSlotNo
  let lastSlotNo = max hSlotNo ahSlotNo

  traceWith tracer "Fragments:"

  [firstSlotNo .. lastSlotNo]
    & map (printf "%2d" . unSlotNo)
    & unwords
    & ("  slots:  " ++)
    & traceWith tracer

  honestFragment
    & toOldestFirst
    & map (\block -> (fromIntegral (unSlotNo (blockSlot block) - 1), Just (unBlockNo (blockNo block))))
    & Vector.toList . (Vector.replicate (fromIntegral (unSlotNo hSlotNo - unSlotNo oSlotNo)) Nothing Vector.//)
    & map (maybe "  " (printf "%2d"))
    & unwords
    & map (\c -> if c == ' ' then '─' else c)
    & ("─" ++)
    & (printf "%2d" (unBlockNo oBlockNo) ++)
    & ("  honest: " ++)
    & traceWith tracer

  advFragment
    & toOldestFirst
    & map (\block -> (fromIntegral (unSlotNo (blockSlot block) - unSlotNo aoSlotNo - 1), Just (unBlockNo (blockNo block))))
    & Vector.toList . (Vector.replicate (fromIntegral (unSlotNo ahSlotNo - unSlotNo aoSlotNo)) Nothing Vector.//)
    & map (maybe "  " (printf "%2d"))
    & unwords
    & map (\c -> if c == ' ' then '─' else c)
    & (" ╰─" ++)
    & (replicate (3 * fromIntegral (unSlotNo (aoSlotNo - oSlotNo))) ' ' ++)
    & ("  advers: " ++)
    & traceWith tracer

  pure ()

  where
    reanchorAdversary :: TestFragH -> TestFragH
    reanchorAdversary fragment@(AF.Empty _) = fragment
    reanchorAdversary fragment@(block AF.:< restFragment) =
      if all (0 ==) $ unTestHash $ blockHash block then
        reanchorAdversary restFragment
      else
        fragment

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
      fragments <- lift $ traverse (readTVarIO . mcssUnservedFragment) peers
      lift $ prettyPrintFragments tracer fragments
      lift $ for_ peers $ \ MockedChainSyncServer {mcssPeerId, mcssUnservedFragment} -> do
        unservedFragment <- readTVarIO mcssUnservedFragment
        traceWith tracer $ condense mcssPeerId ++ " fragment: " ++ condense unservedFragment
      chainDb <- lift $ mkRealChainDb tracer (mcssCandidateFragment <$> peers) (topConfig st) (registry st)
      let chainDbView = defaultChainDbView chainDb
      void (Map.traverseWithKey (syncWith tracer chainDbView (frags pointSchedule)) peers)
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
          traceUnitWith tracer "ChainDB" "Added to current chain"
          traceUnitWith tracer "ChainDB" $ "New tip: " ++ condense newTipPoint
          traceUnitWith tracer "ChainDB" $ "New fragment: " ++ condense newFragment
        SwitchedToAFork _ NewTipInfo {newTipPoint} _ newFragment -> do
          traceUnitWith tracer "ChainDB" "Switched to a fork"
          traceUnitWith tracer "ChainDB" $ "New tip: " ++ condense newTipPoint
          traceUnitWith tracer "ChainDB" $ "New fragment: " ++ condense newFragment
        _ -> pure ()
    _ -> pure ()

mkChainSyncClientTracer ::
  IOLike m =>
  Tracer m String ->
  Tracer m (TraceChainSyncClientEvent TestBlock)
mkChainSyncClientTracer tracer =
  Tracer $ \case
    TraceRolledBack point ->
      traceUnitWith tracer "ChainSyncClient" $ "Rolled back to: " ++ condense point
    TraceFoundIntersection point _ourTip _theirTip ->
      traceUnitWith tracer "ChainSyncClient" $ "Found intersection at: " ++ condense point
    _ -> pure ()

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
