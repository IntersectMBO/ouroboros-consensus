{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Small repeatable measurements of the real fetch applications. The test
-- transport delays/serializes bytes; it does not model TCP or network diffusion.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#13-scenarios-the-eventual-implementation-must-cover@.
module Test.Consensus.Leios.LeiosFetch.Pilots (tests) where

import Cardano.Network.NodeToNode.Version (NodeToNodeVersion (..))
import qualified Codec.CBOR.Read as CBOR
import Control.Concurrent.Class.MonadSTM.Strict
import qualified Control.Exception as E
import Control.Monad (forM_, forever, unless, void, when)
import Control.Monad.Class.MonadAsync (withAsync)
import Control.Monad.Class.MonadTime.SI (diffTime, getMonotonicTime)
import Control.Monad.Class.MonadTimer.SI (threadDelay, timeout)
import qualified Control.Monad.IOSim as Sim
import Control.Tracer (nullTracer)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Dynamic (fromDynamic)
import Data.List (intercalate, sort)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Strict as V
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import Network.TypedProtocol.Codec (AnnotatedCodec)
import Ouroboros.Consensus.Block (blockSlot)
import Ouroboros.Consensus.Leios.Acquisitions
import Ouroboros.Consensus.Leios.Protocol (LeiosLedgerView (..))
import Ouroboros.Consensus.Leios.Types
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Acquisition as Q
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Admission as A
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Deadline as D
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Descriptor (PayloadBytes (..))
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Handoff as H
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Node
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Retry as Retry
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Server as S
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Server.State as SS
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Type as Wire
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies (LeiosFetchClientContext (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision (LeiosOutstanding (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Kernel
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Server
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Server
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Type
import qualified Ouroboros.Consensus.MiniProtocol.LeiosNotify.Type as Notify
import Ouroboros.Consensus.Network.LeiosFetch
  ( leiosFetchMiniProtocolNum
  , runLookaheadPeerWithAnnotatedLimits
  )
import Ouroboros.Consensus.Network.LeiosNotify (runLookaheadFixedSenderPeerWithLimits)
import Ouroboros.Consensus.NodeKernel (NodeKernel (..))
import Ouroboros.Consensus.Storage.LeiosDB (newLeiosDBInMemory)
import Ouroboros.Consensus.Storage.LeiosDB.API
import qualified Ouroboros.Consensus.Util.IOLike as IOLike
import Ouroboros.Network.BlockFetch.ClientRegistry
  ( KeepAliveRtt (..)
  , lookupKeepAliveRtt
  , readKeepAliveRtt
  )
import Ouroboros.Network.Channel (Channel (..))
import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.Driver.Simple (runAnnotatedPeer)
import System.Environment (lookupEnv)
import System.Mem (performGC)
import Test.Consensus.Leios.Environment
import Test.Consensus.Leios.NodeUnderTest
import Test.Consensus.Leios.TestBlock
import Test.Tasty
import Test.Tasty.HUnit

-- | Explicit fixture inputs; all rates are payload bytes per second per source.
data Pilot = Pilot
  { pilotName :: !String
  -- ^ Stable row/test name.
  , pilotFloor :: !Int
  -- ^ Receiver service benchmark in bits/s; legacy has no policy floor.
  , pilotPeers :: !Int
  -- ^ Simultaneous upstream connections to one receiving node.
  , pilotBlocks :: !Int
  -- ^ Concurrent EBs sharing the receiving node and connection budgets.
  , pilotCeiling :: !Int
  -- ^ Closure ceiling; whole variable-size transactions are packed below it.
  , pilotService :: !Int
  -- ^ Honest source's serialization rate, independent of receiver policy.
  , pilotBadSources :: !Int
  -- ^ Earliest offerers that either withhold or dribble.
  , pilotDribble :: !Bool
  -- ^ Slow service instead of withholding entirely.
  , pilotWriteDelay :: !Rational
  -- ^ Delay to acknowledge each real storage write, in seconds.
  }

pilots :: [Pilot]
pilots =
  [ Pilot "tiny-honest" 300000 2 1 16384 250000 0 False 0
  , Pilot "small-honest" 300000 8 1 262144 250000 0 False 0
  , Pilot "many-peers" 300000 32 2 262144 250000 0 False 0
  , Pilot "honest-overload" 300000 8 1 262144 62500 0 False 0
  , Pilot "silent-first" 300000 8 1 262144 250000 2 False 0
  , Pilot "dribbling-first" 300000 8 1 262144 250000 2 True 0
  , Pilot "slow-storage" 300000 8 4 262144 250000 0 False 0.1
  , Pilot "floor-1500-overload" 1500000 8 1 262144 62500 0 False 0
  , Pilot "floor-1500-dribble" 1500000 8 1 262144 250000 2 True 0
  ]

-- | Events contain counts only, never descriptors, decoded payloads or input.
data PilotEvent
  = PlannerBegin
  | PlannerEnd
  | Resources !H.StorageSnapshot !Int !Int !Word64 !Int
  deriving Show

-- | Bounded transport counters, including bytes from redundant replies.
data Traffic = Traffic
  { trafficBodies :: !Int
  , trafficBatches :: !Int
  , trafficBytes :: !Word64
  }
  deriving Show

-- | Receiver-visible times relative to publication of this pilot's first offer.
data Outcome = Outcome
  { outcomeBodies :: !Int
  , outcomeClosures :: !Int
  , outcomeBodyTime :: !(Maybe Double)
  , outcomeClosureTime :: !(Maybe Double)
  , outcomeDrainTime :: !(Maybe Double)
  , outcomeTraffic :: !Traffic
  , outcomePeakCopies :: !Int
  , outcomeRttReaders :: !Word64
  , outcomeRttFallbacks :: !Word64
  , outcomePayloadBytes :: !Int
  , outcomeEbBodyBytes :: !Int
  }
  deriving Show

-- | Folded trace measurements. Blocking STM retries are not parallel conflicts.
data Measurements = Measurements
  { measuredPasses :: !Int
  , measuredPassNanos :: ![Word64]
  , measuredStarted :: !(Maybe Word64)
  , measuredCommits :: !Int
  , measuredBlocks :: !Int
  , measuredWakes :: !Int
  , measuredSessions :: !Word64
  , measuredReserved :: !Word64
  , measuredClaims :: !Word64
  , measuredEntries :: !Word64
  , measuredData :: !Int
  , measuredExchanges :: !Int
  , measuredResponse :: !Word64
  , measuredArenas :: !Int
  }

emptyMeasurements :: Measurements
emptyMeasurements = Measurements 0 [] Nothing 0 0 0 0 0 0 0 0 0 0 0

tests :: TestTree
tests =
  testGroup
    "resource pilots"
    [ testCase (pilotName p ++ "/" ++ policy atomic) (runPilot p atomic)
    | p <- pilots
    , atomic <- [False, True]
    ]
 where
  policy False = "legacy"
  policy True = "atomic"

runPilot :: Pilot -> Bool -> IO ()
runPilot p atomic = do
  reporting <- (== Just "1") <$> lookupEnv "LEIOS_FETCH_PILOT_REPORT"
  statsEnabled <- getRTSStatsEnabled
  when reporting performGC
  before <- if statsEnabled then allocated_bytes <$> getRTSStats else pure 0
  started <- getMonotonicTimeNSec
  (outcome, measurements) <-
    consumeTrace emptyMeasurements (Sim.runSimTrace (pilot p atomic))
  finished <- getMonotonicTimeNSec
  -- Force the small result before measuring post-scope live memory.
  void $ E.evaluate (length (show outcome))
  when reporting performGC
  finalStats <- if statsEnabled then Just <$> getRTSStats else pure Nothing
  when (atomic || pilotBadSources p == 0) $ outcomeClosures outcome @?= pilotBlocks p
  when atomic $ do
    outcomeRttReaders outcome @?= fromIntegral (pilotPeers p)
    outcomeRttFallbacks outcome @?= 0
    outcomePeakCopies outcome @?= min 2 (outcomePeakCopies outcome)
    assertBool "data admission exceeded D" (measuredData measurements <= 64)
    assertBool "exchange admission exceeded 2D" (measuredExchanges measurements <= 128)
    assertBool
      "response reservation exceeded the peer byte budget"
      (measuredResponse measurements <= 1048576)
    assertBool "acquisition arena bound exceeded" (measuredArenas measurements <= 8)
    assertBool "session bound exceeded" (measuredSessions measurements <= 64)
    assertBool "total per-owner storage bound exceeded" (measuredReserved measurements <= 64 * 2097152)
    assertBool "completed wire did not drain" (outcomeDrainTime outcome /= Nothing)
  when reporting $ do
    let ns = sort (measuredPassNanos measurements)
        quantile f = if null ns then 0 else ns !! min (length ns - 1) (floor (f * fromIntegral (length ns - 1)))
        optional :: Show a => Maybe a -> String
        optional = maybe "NA" show
        row =
          [ pilotName p
          , if atomic then "atomic" else "legacy"
          , show (pilotPeers p)
          , show (pilotBlocks p)
          , show (outcomeBodies outcome)
          , show (outcomeClosures outcome)
          , optional (outcomeBodyTime outcome)
          , optional (outcomeClosureTime outcome)
          , optional (outcomeDrainTime outcome)
          , show (trafficBodies (outcomeTraffic outcome))
          , show (trafficBatches (outcomeTraffic outcome))
          , show (trafficBytes (outcomeTraffic outcome))
          , show (measuredPasses measurements)
          , show (quantile (0.5 :: Double))
          , show (quantile (0.95 :: Double))
          , show (if null ns then 0 else last ns)
          , show (measuredCommits measurements)
          , show (measuredBlocks measurements)
          , show (measuredWakes measurements)
          , show (measuredSessions measurements)
          , show (measuredReserved measurements)
          , show (measuredEntries measurements)
          , show (measuredClaims measurements)
          , show (measuredData measurements)
          , show (measuredExchanges measurements)
          , show (measuredResponse measurements)
          , show (measuredArenas measurements)
          , show (finished - started)
          , optional ((\s -> allocated_bytes s - before) <$> finalStats)
          , optional (max_live_bytes <$> finalStats)
          , optional (gcdetails_live_bytes . gc <$> finalStats)
          , show (pilotFloor p)
          , show (outcomeRttReaders outcome)
          , show (outcomeRttFallbacks outcome)
          , show (outcomePayloadBytes outcome)
          , show (outcomeEbBodyBytes outcome)
          ]
    putStrLn ("E20_PILOT," ++ intercalate "," row)

-- | Consume the lazy trace once, without retaining a root to its input/payloads.
-- Host span timings include IOSim interpretation and any interleaved threads.
consumeTrace :: Measurements -> Sim.SimTrace Outcome -> IO (Outcome, Measurements)
consumeTrace !m trace = case trace of
  Sim.SimTrace _ _ label event rest -> do
    let coordinator = label == Just "NodeKernel.leiosFetchDecisionLoop"
        counted = case event of
          Sim.EventTxCommitted{} | coordinator -> m{measuredCommits = measuredCommits m + 1}
          Sim.EventTxBlocked{} | coordinator -> m{measuredBlocks = measuredBlocks m + 1}
          Sim.EventTxWakeup{} | coordinator -> m{measuredWakes = measuredWakes m + 1}
          _ -> m
    next <- case event of
      Sim.EventLog dynamic -> case fromDynamic dynamic of
        Just PlannerBegin -> do
          now <- getMonotonicTimeNSec
          pure counted{measuredStarted = Just now}
        Just PlannerEnd -> do
          now <- getMonotonicTimeNSec
          pure
            counted
              { measuredPasses = measuredPasses counted + 1
              , measuredPassNanos =
                  maybe id (\at -> (now - at :)) (measuredStarted counted) (measuredPassNanos counted)
              , measuredStarted = Nothing
              }
        Just (Resources storage dat exchanges response arenas) ->
          pure
            counted
              { measuredSessions = max (measuredSessions counted) (fromIntegral (H.snapshotSessions storage))
              , measuredReserved = max (measuredReserved counted) (H.snapshotReservedBytes storage)
              , measuredEntries = max (measuredEntries counted) (H.snapshotReservedEntries storage)
              , measuredClaims = max (measuredClaims counted) (H.snapshotClaims storage)
              , measuredData = max (measuredData counted) dat
              , measuredExchanges = max (measuredExchanges counted) exchanges
              , measuredResponse = max (measuredResponse counted) response
              , measuredArenas = max (measuredArenas counted) arenas
              }
        Nothing -> pure counted
      _ -> pure counted
    consumeTrace next rest
  Sim.TraceMainReturn _ _ result _ -> pure (result, m)
  Sim.TraceMainException _ _ exception _ -> E.throwIO exception
  _ -> assertFailure "pilot simulation did not terminate normally" >> error "unreachable"

config :: Pilot -> AtomicNodeConfig
config p =
  AtomicNodeConfig
    { nodeStorageLimits = H.StorageLimits 2097152 4096 64 8 20000
    , nodeResponseBytes = 1048576
    , nodeCheckpointLimits = must (Q.mkCheckpointLimits 20000 500000 20000 550000)
    , nodeClosureRetryConfig = Retry.defaultRetryConfig
    , nodeTimingConfig =
        must (D.mkTimingConfig (fromIntegral (pilotFloor p)) 14600 0.5 2 60 (PayloadBytes 600000))
    }
 where
  must :: Show e => Either e a -> a
  must = either (error . show) id

-- | Install only test-local observation around the real extension callback.
observePlanner :: NodeUnderTest (Sim.IOSim s) -> Sim.IOSim s ()
observePlanner node = case nodeAtomicFetch node of
  Nothing -> pure ()
  Just runtime -> IOLike.modifyMVar_ (lfcOutstanding (lfkContext kernel)) $ \state -> do
    IOLike.atomically $ IOLike.modifyTVar (lfkExtension kernel) (fmap wrap)
    pure state
   where
    wrap original =
      original
        { extensionDecide = \now bounds horizon peers offers outstanding -> do
            Sim.traceM PlannerBegin
            result <- extensionDecide original now bounds horizon peers offers outstanding
            sample <- atomically $ do
              storage <- readNodeStorageSnapshot runtime
              usage <- readNodeClientUsage runtime
              bodies <- readNodeBodySnapshots runtime
              let usages = [u | Right u <- Map.elems usage]
                  largest :: (Ord n, Num n) => (A.Usage -> n) -> n
                  largest get = maximum (0 : map get usages)
              unless
                (all (\u -> A.usedHandoffBytes u <= 2097152 && A.usedHandoffEntries u <= 4096) usages)
                (error "pilot exceeded per-session handoff limits")
              pure
                ( Resources
                    storage
                    (largest A.usedData)
                    (largest A.usedExchanges)
                    (fromIntegral (largest A.usedResponseBytes))
                    (Map.size bodies)
                )
            Sim.traceM sample
            Sim.traceM PlannerEnd
            pure result
        }
 where
  kernel = getLeiosFetchKernel (nodeKernel node)

-- | Variable whole-TX payloads, unique within and across the concurrent EBs.
payloads :: Int -> Int -> V.Vector LeiosTx
payloads byteCeiling salt = V.fromList (go byteCeiling 0)
 where
  sizes = cycle [360, 1460, 8000, 16384]
  go remaining i
    | remaining < 360 = []
    | otherwise =
        let n = min remaining (sizes !! (i `mod` 4))
            bytes = BS.cons (fromIntegral salt) (BS.replicate (n - 1) (fromIntegral i))
         in MkLeiosTx bytes : go (remaining - n) (i + 1)

pilot :: forall s. Pilot -> Bool -> Sim.IOSim s Outcome
pilot p atomic = do
  db <- newLeiosDBInMemory
  let db' = delayReceipts (pilotWriteDelay p) db
      runNode = withNodeOverConfig $ \base ->
        let ledger = nodeConfigLedger base
         in base
              { nodeConfigAtomicFetch = if atomic then Just (config p) else Nothing
              , nodeConfigLedger =
                  ledger{ltlcLeios = (ltlcLeios ledger){llvMaxEbBodySize = 600000, llvMaxEbTxsSize = 600000}}
              }
      fixtures =
        [ let txs = payloads (pilotCeiling p) i
              eb =
                mkEndorserBlock [(hashLeiosTx tx, fromIntegral (BS.length (leiosTxBytes tx))) | tx <- V.toList txs]
              block =
                setAnnouncement
                  (endorserBlockHash eb)
                  (encodeEndorserBlockSize eb)
                  (iterate successorLeiosBlock (firstLeiosBlock 0) !! i)
           in (MkLeiosPoint (blockSlot block) (endorserBlockHash eb), eb, txs, block)
        | i <- [0 .. pilotBlocks p - 1]
        ]
  runNode db' $ \node -> do
    observePlanner node
    next <- subscribeLeiosAcquisitions (nodeLeiosAcquisitions node)
    traffic <- newTVarIO (Traffic 0 0 0)
    queues <- mapM (const newTQueueIO) [1 .. pilotPeers p]
    bodies <- newTVarIO Map.empty
    closures <- newTVarIO Map.empty
    let server i queue = EnvPeer (notifyServer node queue) Nothing (upstream i) Nothing
        upstream i channel = timedChannel (if i < pilotBadSources p then 18750 else pilotService p) traffic channel $ \paced ->
          if atomic
            then S.withAtomicServer (SS.defaultServerLimits 64) (atomicHandler i) $ \serverState ->
              void $
                runLookaheadPeerWithAnnotatedLimits
                  nullTracer
                  atomicCodec
                  (Wire.byteLimitsLeiosFetchAtomic (fromIntegral . BSL.length))
                  Wire.timeLimitsLeiosFetchAtomic
                  paced
                  (S.atomicServerPeer serverState)
            else
              void $
                runAnnotatedPeer nullTracer (envLeiosFetchCodec node) paced (leiosFetchServerPeer (legacyHandler i))
        find point = case [(eb, txs) | (at, eb, txs, _) <- fixtures, at == point] of
          [found] -> found
          _ -> error "pilot received a request for an unknown EB"
        record :: Bool -> Int -> Sim.IOSim s ()
        record body i = do
          atomically $
            modifyTVar
              traffic
              ( \t ->
                  if body then t{trafficBodies = trafficBodies t + 1} else t{trafficBatches = trafficBatches t + 1}
              )
          when (i < pilotBadSources p && not (pilotDribble p)) (atomically retry)
        atomicHandler :: Int -> S.AtomicRequestHandler LeiosPoint EndorserBlock LeiosTx (Sim.IOSim s)
        atomicHandler i = S.AtomicRequestHandler $ \case
          S.ServeBody _ point -> do
            record True i
            let (eb, _) = find point
            pure (S.PreparedBody (encodeEndorserBlockSize eb + 2) eb)
          S.ServeTransactions _ point bitmaps -> do
            record False i
            let (_, txs) = find point
                selected = V.fromList [txs V.! off | off <- bitmapOffsets (NE.toList bitmaps)]
                bytes = Wire.txResponseBytes [fromIntegral (BS.length (leiosTxBytes tx)) | tx <- V.toList selected]
            pure (S.PreparedTransactions (fromIntegral bytes) selected)
        legacyHandler :: Int -> LeiosFetchRequestHandler LeiosPoint EndorserBlock LeiosTx (Sim.IOSim s)
        legacyHandler i = MkLeiosFetchRequestHandler $ \case
          MsgLeiosBlockRequest point -> record True i >> pure (MsgLeiosBlock (fst (find point)))
          MsgLeiosBlockTxsRequest point bitmaps -> do
            record False i
            let txs = snd (find point)
            pure (MsgLeiosBlockTxs (V.fromList [txs V.! off | off <- bitmapOffsets bitmaps]))
        connect [] action = action
        connect ((i, queue) : rest) action =
          withNodeVsEnvConnectionDelayed
            0.05
            (if atomic then Nothing else Just NodeToNodeV_16)
            node
            (PeerAddr (i + 1))
            (server i queue)
            $ \connection -> do
              result <- connect rest action
              status <- atomically $ case Map.lookup leiosFetchMiniProtocolNum (connNodeInitiators connection) of
                Nothing -> error "pilot fetch initiator missing"
                Just finished -> (Just <$> finished) `orElse` pure Nothing
              case status of
                Just (Left failure) -> error ("pilot fetch initiator failed: " ++ show failure)
                _ -> pure result
    threadDelay (fromIntegral (pilotBlocks p))
    connect (zip [0 ..] queues) $ do
      -- A real KeepAlive exchange, not an injected RTT or GSV approximation.
      measured <- timeout 1 $ atomically $ do
        reader <-
          lookupKeepAliveRtt (getKeepAliveRegistry (nodeKernel node)) (ConnectionId (PeerAddr 0) (PeerAddr 1))
        observation <- maybe retry readKeepAliveRtt reader
        maybe retry pure observation
      unless
        (maybe False (\r -> rttDuration r == 0.1) measured)
        (error "pilot KeepAlive did not observe the bearer RTT")
      origin <- getMonotonicTime
      let publish = forM_ (zip [0 :: Int ..] queues) $ \(i, queue) -> do
            when (i > 0) (threadDelay 0.01)
            atomically $ forM_ fixtures $ \(point, eb, _, block) -> do
              writeTQueue queue (MsgLeiosBlockAnnouncement (ltbHeader block))
              writeTQueue queue (MsgLeiosBlockOffer point (encodeEndorserBlockSize eb))
              writeTQueue queue (MsgLeiosBlockTxsOffer point)
          collect = do
            note <- atomically next
            now <- getMonotonicTime
            atomically $ case note of
              AcquiredEb point _ -> modifyTVar bodies (Map.insert point (seconds now origin))
              AcquiredEbTxs point -> modifyTVar closures (Map.insert point (seconds now origin))
            count <- Map.size <$> readTVarIO closures
            unless (count == pilotBlocks p) collect
      withAsync publish $ \_ -> void (timeout 15 collect)
      completed <- readTVarIO closures
      drained <-
        if Map.size completed /= pilotBlocks p
          then pure Nothing
          else case nodeAtomicFetch node of
            Nothing ->
              timeout 15 $
                let
                  loop = do
                    state <- IOLike.readMVar (lfcOutstanding (lfkContext (getLeiosFetchKernel (nodeKernel node))))
                    if Map.null (requestedBytesSizePerPeer state)
                      then (`seconds` origin) <$> getMonotonicTime
                      else threadDelay 0.05 >> loop
                 in
                  loop
            Just runtime -> timeout 3 $ do
              atomically $ do
                usages <- readNodeClientUsage runtime
                check
                  ( all
                      (\u -> case u of Right used -> A.usedData used == 0 && A.usedExchanges used == 0; _ -> False)
                      (Map.elems usages)
                  )
              (`seconds` origin) <$> getMonotonicTime
      acquired <- readTVarIO bodies
      observation <- traverse (atomically . readNodeObservation) (nodeAtomicFetch node)
      forM_ fixtures $ \(point, eb, txs, _) -> when (Map.member point completed) $ withReader db $ \reader -> do
        stored <- lookupEbBody reader point
        unless (stored == Just (endorserBlockBytes eb)) (error "pilot body was not durable")
        closure <- lookupEbClosure reader point
        unless
          (closure == Just [(hashLeiosTx tx, leiosTxBytes tx) | tx <- V.toList txs])
          (error "pilot closure was not durable")
      result <- readTVarIO traffic
      pure
        ( Outcome
            (Map.size acquired)
            (Map.size completed)
            (lastTime acquired)
            (lastTime completed)
            drained
            result
            (maybe 0 observedPeakClosureCopies observation)
            (maybe 0 observedScopedRttReaders observation)
            (maybe 0 observedRttFallbacks observation)
            (sum [sum [BS.length (leiosTxBytes tx) | tx <- V.toList txs] | (_, _, txs, _) <- fixtures])
            (sum [fromIntegral (encodeEndorserBlockSize eb) | (_, eb, _, _) <- fixtures])
        )
 where
  seconds now origin = realToFrac (diffTime now origin)
  lastTime m = if Map.null m then Nothing else Just (maximum (Map.elems m))

-- | Acknowledgment delay on real DB writes; it does not fabricate possession.
delayReceipts :: forall s. Rational -> LeiosDbHandle (Sim.IOSim s) -> LeiosDbHandle (Sim.IOSim s)
delayReceipts delay db =
  db
    { openWriter = do
        writer <- openWriter db
        let delayed :: Promise (Sim.IOSim s) a -> Promise (Sim.IOSim s) a
            delayed promise = Promise (threadDelay (fromRational delay) >> await promise)
        pure
          writer
            { writeEbBody = \point body -> delayed <$> writeEbBody writer point body
            , writeTxs = \point txs -> delayed <$> writeTxs writer point txs
            }
    }

-- | Bound serialized fetch output by two queued segments. Propagation applies
-- separately to the whole bearer, so KeepAlive measures the same RTT.
timedChannel ::
  Int ->
  StrictTVar (Sim.IOSim s) Traffic ->
  Channel (Sim.IOSim s) BSL.ByteString ->
  (Channel (Sim.IOSim s) BSL.ByteString -> Sim.IOSim s a) ->
  Sim.IOSim s a
timedChannel rate traffic channel action = do
  outbound <- newTBQueueIO 2
  let serialize = forever $ do
        bytes <- atomically (readTBQueue outbound)
        threadDelay (fromRational (toRational (BSL.length bytes) / toRational rate))
        send channel bytes
        atomically $
          modifyTVar traffic (\t -> t{trafficBytes = trafficBytes t + fromIntegral (BSL.length bytes)})
      enqueue bytes = unless (BSL.null bytes) $ do
        let (headBytes, tailBytes) = BSL.splitAt 1460 bytes
        atomically (writeTBQueue outbound headBytes)
        enqueue tailBytes
  withAsync serialize $ \_ -> action (Channel enqueue (recv channel))

atomicCodec ::
  AnnotatedCodec
    (Wire.LeiosFetchAtomic LeiosPoint EndorserBlock LeiosTx)
    CBOR.DeserialiseFailure
    (Sim.IOSim s)
    BSL.ByteString
atomicCodec =
  Wire.codecLeiosFetchAtomic
    20000
    encodeLeiosPoint
    decodeLeiosPoint
    encodeEndorserBlock
    decodeEndorserBlock
    encodeLeiosTx
    decodeLeiosTx

notifyServer ::
  NodeUnderTest (Sim.IOSim s) ->
  StrictTQueue
    (Sim.IOSim s)
    (Message (LeiosNotify LeiosPoint (Header Blk) LeiosVote) Notify.StBusy Notify.StIdle) ->
  Channel (Sim.IOSim s) BSL.ByteString ->
  Sim.IOSim s ()
notifyServer node queue channel = do
  server <- leiosNotifyServerPeerLookahead (pure NotExcessiveRequests) (readTQueue queue)
  void $
    runLookaheadFixedSenderPeerWithLimits
      nullTracer
      (envLeiosNotifyCodec node)
      (byteLimitsLeiosNotify (fromIntegral . BSL.length))
      timeLimitsLeiosNotify
      channel
      server
