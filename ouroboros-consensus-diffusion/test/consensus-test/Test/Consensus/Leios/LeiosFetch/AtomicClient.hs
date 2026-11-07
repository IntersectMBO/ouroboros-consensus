{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.Leios.LeiosFetch.AtomicClient (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Codec.CBOR.Read as CBOR
import Control.Concurrent.Class.MonadSTM.Strict
  ( STM
  , StrictTMVar
  , StrictTVar
  , atomically
  , check
  , modifyTVar
  , newEmptyTMVarIO
  , newTVarIO
  , orElse
  , putTMVar
  , readTMVar
  , readTVar
  , readTVarIO
  , retry
  , throwSTM
  , writeTVar
  )
import Control.Exception (bracket, fromException, try)
import Control.Monad (forM_, void, when)
import Control.Monad.Class.MonadAsync (Async, cancel, wait, waitCatch, waitCatchSTM, withAsync)
import Control.Monad.Class.MonadTime.SI (Time, addTime, diffTime, getMonotonicTime)
import qualified Control.Monad.Class.MonadTimer.SI as SI
import Control.Tracer (nullTracer)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Vector.Strict as V
import Data.Word (Word32)
import Network.Mux.Timeout (withTimeoutSerial)
import Network.TypedProtocol.Codec (AnnotatedCodec)
import Network.TypedProtocol.Driver (runPipelinedPeerWithDriver)
import Ouroboros.Consensus.Leios.Types
  ( ElectionId (..)
  , EndorserBlock
  , LeiosEbBounds (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , decodeEndorserBlock
  , decodeLeiosPoint
  , decodeLeiosTx
  , encodeEndorserBlock
  , encodeLeiosPoint
  , encodeLeiosTx
  , endorserBlockBytes
  , endorserBlockFromTxBytes
  , endorserBlockHash
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Acquisition as Q
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Admission as A
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Client as C
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Codec as ProgressCodec
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Deadline as D
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Descriptor
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Handoff as H
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Receive
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Retry as Retry
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Server as S
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Server.State as SS
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Timing as T
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Type as W
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
  ( LeiosOutstanding
  , OfferedHalves (..)
  , PeerOffer (..)
  , emptyLeiosOutstanding
  , focusElectionIfUnfocused
  , recordReferencedPoint
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type (bitmapOffsets)
import Ouroboros.Consensus.Network.LeiosFetch (runLookaheadPeerWithAnnotatedLimits)
import Ouroboros.Consensus.Storage.LeiosDB
  ( LeiosDbHandle (..)
  , LeiosDbWriter (..)
  , Promise (..)
  , newLeiosDBInMemory
  , withWriter
  )
import Ouroboros.Network.BlockFetch.ClientRegistry
  ( KeepAliveRtt (..)
  , KeepAliveRttReader
  , bracketKeepAliveClientWithRtt
  , lookupKeepAliveRtt
  , newKeepAliveRegistry
  , publishKeepAliveRtt
  )
import Ouroboros.Network.Channel (Channel (..), createConnectedBufferedChannelsUnbounded)
import Ouroboros.Network.Driver.Progress
  ( MessageReceipt (..)
  , ReceiveScope (..)
  , progressDriverWithLimits
  , progressTrailingBytes
  )
import System.Random (mkStdGen)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "atomic pipelined client"
    [ testCase "three admitted requests pipeline before the first reply is available" testPipeline
    , testCase "a fast reply settles before send returns without losing its identity" testFastReply
    , testCase "a stalled receiver does not prevent sending cancellation at full D" testCancel
    , testCase "original cancellation retains its paired record until the ACK" testPairedRecord
    , testCase "cancel during a normal payload drains it before its separate ACK" testPartialCancel
    , testCase "graceful stop rejects new work and drains every admitted exchange" testStop
    , testCase "a valid understated body survives its peer fault and session cleanup" testBodyFault
    , testCase "malformed TX delivery preserves an authenticated prefix after abort" testBadPrefix
    , testCase "coalesced replies retain separate contexts and original receipt times" testCoalesced
    , testCase "an input timestamp paused before publication cannot incur expiry" testPublicationGap
    , testCase "buffered successor survives a paused FIFO receiver transition" testBufferedTransition
    , testCase "aborting a paused publication releases its exact session" testPublicationAbort
    , testCase "partial input releases observation protection before another read" testPartialInputWait
    , testCase "exact-session cleanup preserves a replacement in the same registration" testSessionCleanup
    , testCase "pre-admission rejection consumes no ordinal or reservation" testRejectedAdmission
    , testCase "concurrent EBs share one peer's byte and data-record budgets" testCrossEbBudget
    , testCase "failed publication rolls back checkpoint admission and ownership" testPublicationRollback
    , testCase "cached RTT updates affect later attempts, not earlier snapshots" testRttSnapshots
    , testCase "a retired RTT reader cannot inherit a replacement registration's sample" testRetiredRtt
    , testCase "the actual fatal guard rejects an incompatible supplied timing config" testGuard
    , testCase
        "a 200-request session drains counters and timing while storage stays owned"
        testLongSession
    , testCase "ACK expiry is independent of data failure and does not refund the pair" testAckExpiry
    , testCase "a client handle cannot start a second interpreter" testSingleUse
    , testCase "source removal wakes a client whose FIFO receiver is stalled" testRemoval
    , testCase "data expiry retains wire debt and blocks new assignments until retirement" testDataExpiry
    , testCase "retry changes for future admission do not renew a live head deadline" testRetrySnapshot
    ]

type Atomic = W.LeiosFetchAtomic LeiosPoint EndorserBlock LeiosTx
type Client = C.AtomicClient IO Int Int

data Events = Events
  { replyCount :: !Int
  , transactionCount :: !Int
  , usefulCount :: !Int
  , abortCount :: !Int
  , replies :: ![(RequestKey Int, ReplyCompletion)]
  , expiries :: ![T.Expiry Int]
  , expiryHandlingTimes :: ![Time]
  }

emptyEvents :: Events
emptyEvents = Events 0 0 0 0 [] [] []

-- Event history is capped even in fixtures; the long-session assertion uses
-- counters, not one retained payload/event per request.
hooks :: StrictTVar IO Events -> C.ClientHooks IO Int
hooks events =
  C.ClientHooks
    { C.clientWakeup = pure ()
    , C.clientTransaction = \_ _ accepted -> modifyTVar events $ \s ->
        s
          { transactionCount = transactionCount s + 1
          , usefulCount = usefulCount s + if accepted == H.Useful then 1 else 0
          }
    , C.clientReply = \key result -> modifyTVar events $ \s ->
        s
          { replyCount = replyCount s + 1
          , replies = take 16 (replies s ++ [(key, result)])
          }
    , C.clientExpired = \handledAt event -> modifyTVar events $ \s ->
        s
          { expiries = take 16 (expiries s ++ [event])
          , expiryHandlingTimes = take 16 (expiryHandlingTimes s ++ [handledAt])
          }
    , C.clientAborted = \_ -> modifyTVar events $ \s -> s{abortCount = abortCount s + 1}
    }

data Fixture = Fixture
  { handoff :: !(H.StorageHandoff IO Int Int)
  , assignments :: !(Q.OrdinaryAssignments IO Int)
  , generation :: !AcquisitionGeneration
  , point :: !LeiosPoint
  , body :: !EndorserBlock
  , txs :: ![LeiosTx]
  , writer :: !(LeiosDbWriter IO)
  , events :: !(StrictTVar IO Events)
  }

right :: Show e => Either e a -> a
right = either (error . show) id

config :: D.TimingConfig
config = right (D.mkTimingConfig 1500000 14600 0.5 2 60 (PayloadBytes 600000))

withFixture :: Bool -> (Fixture -> IO a) -> IO a
withFixture known action = bracket newLeiosDBInMemory closeLeiosDbHandle $ \db -> withWriter db $ \writer -> do
  let txs = [MkLeiosTx (BS.replicate n b) | (n, b) <- [(3, 1), (5, 2), (7, 3)]]
      body = endorserBlockFromTxBytes (fmap leiosTxBytes (fromJust (NE.nonEmpty txs)))
      point = MkLeiosPoint (SlotNo 5) (endorserBlockHash body)
  handoff <- right <$> H.newStorageHandoff (H.StorageLimits 2097152 2048 16 16 20000)
  assignments <- Q.newOrdinaryAssignments
  generation <- right <$> atomically (H.beginAcquisition handoff point)
  events <- newTVarIO emptyEvents
  when known $ do
    _ <- await =<< writeEbBody writer point body
    H.registerDurableBody handoff (const (pure ())) point generation body IntSet.empty
      >>= (@?= Right ())
  action Fixture{handoff, assignments, generation, point, body, txs, writer, events}

withClient :: Fixture -> Int -> Word32 -> Maybe (KeepAliveRttReader IO) -> (Client -> IO a) -> IO a
withClient env peer d reader =
  C.withAtomicClient
    (handoff env)
    (assignments env)
    peer
    (MkLeiosPeerLifetimeId peer 0)
    (fromIntegral d)
    1048576
    config
    reader
    (hooks (events env))

prepared :: Fixture -> NonEmpty Int -> PreparedRequest
prepared env offsets =
  right $
    prepareBatch (point env) (generation env) (body env) offsets $
      EncodedBytes
        ( fromIntegral
            ( W.txResponseBytes
                [fromIntegral (BS.length (leiosTxBytes (txs env !! off))) | off <- NE.toList offsets]
            )
        )

admit :: Fixture -> Client -> NonEmpty Int -> IO (H.StorageRequest Int)
admit env client offsets = do
  now <- getMonotonicTime
  right <$> atomically (C.admitClientRequest client now 1 (prepared env offsets))

snapshotEvents :: Fixture -> IO Events
snapshotEvents = readTVarIO . events

snapshotAcquisition :: Fixture -> IO H.AcquisitionSnapshot
snapshotAcquisition env = fromJust <$> atomically (H.readAcquisition (handoff env) (point env))

codec :: AnnotatedCodec Atomic CBOR.DeserialiseFailure IO BL.ByteString
codec =
  W.codecLeiosFetchAtomic
    20000
    encodeLeiosPoint
    decodeLeiosPoint
    encodeEndorserBlock
    decodeEndorserBlock
    encodeLeiosTx
    decodeLeiosTx

handler :: Fixture -> S.AtomicRequestHandler LeiosPoint EndorserBlock LeiosTx IO
handler env = S.AtomicRequestHandler $ \request -> case request of
  S.ServeBody{} -> pure $ S.PreparedBody (fromIntegral (BS.length (endorserBlockBytes (body env))) + 2) (body env)
  S.ServeTransactions _ _ bitmaps -> do
    let values = [txs env !! off | off <- bitmapOffsets (NE.toList bitmaps)]
        bytes = W.txResponseBytes (map (fromIntegral . BS.length . leiosTxBytes) values)
    pure $ S.PreparedTransactions (fromIntegral bytes) (V.fromList values)

blockedHandler ::
  Fixture ->
  StrictTMVar IO () ->
  StrictTMVar IO () ->
  S.AtomicRequestHandler LeiosPoint EndorserBlock LeiosTx IO
blockedHandler env entered release = S.AtomicRequestHandler $ \request -> case request of
  S.ServeTransactions (WireOrdinal 0) _ _ -> do
    atomically $ putTMVar entered ()
    atomically $ readTMVar release
    S.prepareAtomicReply (handler env) request
  _ -> S.prepareAtomicReply (handler env) request

data Running = Running
  { clientTask :: !(Async IO (Maybe BL.ByteString))
  , serverTask :: !(Async IO ((), Maybe BL.ByteString))
  , serverHandle :: !(S.AtomicServer LeiosPoint EndorserBlock LeiosTx IO)
  }

type AdjustChannels =
  Channel IO BL.ByteString ->
  Channel IO BL.ByteString ->
  IO (Channel IO BL.ByteString, Channel IO BL.ByteString)

unchanged :: AdjustChannels
unchanged c s = pure (c, s)

withConnection ::
  Fixture ->
  Client ->
  S.AtomicRequestHandler LeiosPoint EndorserBlock LeiosTx IO ->
  AdjustChannels ->
  (Running -> IO a) ->
  IO a
withConnection = withConnectionUsing (C.runAtomicClient nullTracer 20000 1048576)

withConnectionUsing ::
  (Channel IO BL.ByteString -> Client -> IO (Maybe BL.ByteString)) ->
  Fixture ->
  Client ->
  S.AtomicRequestHandler LeiosPoint EndorserBlock LeiosTx IO ->
  AdjustChannels ->
  (Running -> IO a) ->
  IO a
withConnectionUsing runner _env client provider adjust action = do
  (c0, s0) <- createConnectedBufferedChannelsUnbounded
  (c, s) <- adjust c0 s0
  window <- C.snapshotClientWindow <$> atomically (C.readClientSnapshot client)
  S.withAtomicServer (SS.defaultServerLimits window) provider $ \serverHandle ->
    withAsync
      ( runLookaheadPeerWithAnnotatedLimits
          nullTracer
          codec
          (W.byteLimitsLeiosFetchAtomic (fromIntegral . BL.length))
          W.timeLimitsLeiosFetchAtomic
          s
          (S.atomicServerPeer serverHandle)
      )
      $ \serverTask ->
        withAsync (runner c client) $ \clientTask ->
          action Running{clientTask, serverTask, serverHandle}

type ScopeHook = IO (ReceiveScope IO LeiosTx) -> IO (ReceiveScope IO LeiosTx)

-- Compose the same public client, codec and driver as the normal runner. Only
-- callback scheduling is instrumented; no application policy or clock is replaced.
observedRunner :: ScopeHook -> Channel IO BL.ByteString -> Client -> IO (Maybe BL.ByteString)
observedRunner adjust channel client = withTimeoutSerial $ \timeoutFn -> do
  let scope ::
        forall (st :: W.LeiosFetchAtomic LeiosPoint EndorserBlock LeiosTx).
        W.SingLeiosFetchAtomic st -> IO (ReceiveScope IO LeiosTx)
      scope tok = adjust (C.atomicClientReceiveScope client tok)
      codec' =
        ProgressCodec.codecLeiosFetchAtomicProgress
          20000
          encodeLeiosPoint
          decodeLeiosPoint
          encodeEndorserBlock
          decodeEndorserBlock
          encodeLeiosTx
          decodeLeiosTx
      driver =
        progressDriverWithLimits
          nullTracer
          timeoutFn
          codec'
          (W.byteLimitsLeiosFetchAtomic (fromIntegral . BL.length))
          W.timeLimitsLeiosFetchAtomic
          1048576
          scope
          (C.observeClientSend client)
          channel
  ((), trailing) <- runPipelinedPeerWithDriver driver (C.atomicClientPeer client)
  pure (progressTrailingBytes trailing)

pollAfterHead :: Running -> Client -> IO Time
pollAfterHead running client = do
  expiry <- awaitEvent running $ do
    snap <- C.readClientSnapshot client
    maybe retry (pure . snd) (T.activeDeadline (C.snapshotClientTiming snap))
  let later = addTime 5 expiry
  atomically $ C.pollAtomicClient client later
  pure later

bounded :: IO a -> IO a
bounded action = SI.timeout 5 action >>= maybe (assertFailure "client fixture timed out") pure

awaitEvent :: Running -> STM IO a -> IO a
awaitEvent running action =
  bounded $
    atomically $
      action `orElse` failed (clientTask running) `orElse` failed (serverTask running)
 where
  failed :: Async IO b -> STM IO a
  failed task =
    waitCatchSTM task
      >>= either throwSTM (const (throwSTM (userError "interpreter ended before the expected state")))

waitReplies :: Fixture -> Running -> Int -> IO ()
waitReplies env running n = awaitEvent running $ readTVar (events env) >>= check . (>= n) . replyCount

success :: Running -> IO ()
success running = do
  bounded (wait (clientTask running)) >>= (@?= Nothing)
  bounded (wait (serverTask running)) >>= (@?= ((), Nothing))

wireEmpty :: Client -> Assertion
wireEmpty client = do
  usage <- right <$> atomically (C.readClientUsage client)
  (A.usedData usage, A.usedExchanges usage, A.usedResponseBytes usage) @?= (0, 0, 0)

focused :: Fixture -> LeiosOutstanding Int
focused env =
  focusElectionIfUnfocused election (pointEbHash (point env)) $
    recordReferencedPoint (point env) SNothing (emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0))
 where
  election = ElectionId (pointSlotNo (point env)) (KeyHash (UnsafeHash (SBS.pack (replicate 28 0))))

withRtt :: Rational -> (KeepAliveRttReader IO -> IO a) -> IO a
withRtt duration action = do
  registry <- newKeepAliveRegistry
  bracketKeepAliveClientWithRtt registry (1 :: Int) $ \context -> do
    now <- getMonotonicTime
    atomically $ publishKeepAliveRtt context (KeepAliveRtt (fromRational duration) now)
    reader <- fromJust <$> atomically (lookupKeepAliveRtt registry 1)
    action reader

testPipeline :: Assertion
testPipeline = withFixture True $ \env -> withClient env 1 3 Nothing $ \client -> do
  forM_ [0, 1, 2] $ \off -> void (admit env client (off :| []))
  atomically $ C.stopAtomicClient client
  entered <- newEmptyTMVarIO
  release <- newEmptyTMVarIO
  withConnection env client (blockedHandler env entered release) unchanged $ \running -> do
    _ <- awaitEvent running $ do
      snapshot <- S.readAtomicServerSnapshot (serverHandle running)
      check (SS.snapshotDataRecords snapshot == 3)
    atomically $ putTMVar release ()
    success running
    wireEmpty client
  stats <- snapshotEvents env
  (replyCount stats, transactionCount stats, usefulCount stats) @?= (3, 3, 3)

testFastReply :: Assertion
testFastReply = withFixture True $ \env -> withClient env 1 1 Nothing $ \client -> do
  first <- admit env client (0 :| [])
  release <- newEmptyTMVarIO
  sent <- newTVarIO (0 :: Int)
  let adjust c s =
        pure
          ( c
              { send = \bytes -> do
                  n <- atomically $ do n <- readTVar sent; writeTVar sent (n + 1); pure n
                  send c bytes
                  when (n == 0) (atomically (readTMVar release))
              }
          , s
          )
  withConnection env client (handler env) adjust $ \running -> do
    waitReplies env running 1
    snap <- atomically (C.readClientSnapshot client)
    assertBool
      "no send-return clock was invented"
      (T.activeDeadline (C.snapshotClientTiming snap) == Nothing)
    second <- admit env client (1 :| [])
    assertBool
      "next admitted request retains another identity"
      (H.storageRequestKey first /= H.storageRequestKey second)
    atomically $ C.stopAtomicClient client >> putTMVar release ()
    success running
    wireEmpty client
  stats <- snapshotEvents env
  map (ordinal . fst) (replies stats) @?= [WireOrdinal 0, WireOrdinal 1]
  fst (head (replies stats)) @?= H.storageRequestKey first

testCancel :: Assertion
testCancel = withFixture True $ \env -> withClient env 1 1 Nothing $ \client -> do
  req <- admit env client (0 :| [])
  now <- getMonotonicTime
  atomically (C.admitClientRequest client now 1 (prepared env (1 :| [])))
    >>= (@?= Left (C.ClientStorageError (H.AdmissionFailure A.DataCapacity)))
  entered <- newEmptyTMVarIO
  release <- newEmptyTMVarIO
  withConnection env client (blockedHandler env entered release) unchanged $ \running -> do
    _ <- awaitEvent running (readTMVar entered)
    now' <- getMonotonicTime
    ack <- right <$> atomically (C.cancelClientRequest client now' req)
    atomically $ C.stopAtomicClient client
    success running
    stats <- snapshotEvents env
    map (\(key, c) -> (key, completedOutcome c)) (replies stats)
      @?= [(H.storageRequestKey req, DataCancelled), (ack, CancelAcknowledged)]
    wireEmpty client

testPairedRecord :: Assertion
testPairedRecord = withFixture True $ \env -> withClient env 1 1 Nothing $ \client -> do
  req <- admit env client (0 :| [])
  entered <- newEmptyTMVarIO
  never <- newEmptyTMVarIO
  releaseAck <- newEmptyTMVarIO
  sends <- newTVarIO (0 :: Int)
  let adjust c s =
        pure
          ( c
          , s
              { send = \bytes -> do
                  n <- atomically $ do n <- readTVar sends; writeTVar sends (n + 1); pure n
                  when (n == 1) (atomically (readTMVar releaseAck))
                  send s bytes
              }
          )
  withConnection env client (blockedHandler env entered never) adjust $ \running -> do
    _ <- awaitEvent running (readTMVar entered)
    now <- getMonotonicTime
    _ <- right <$> atomically (C.cancelClientRequest client now req)
    waitReplies env running 1
    usage <- right <$> atomically (C.readClientUsage client)
    (A.usedData usage, A.usedExchanges usage) @?= (1, 1)
    now' <- getMonotonicTime
    rejected <- atomically (C.admitClientRequest client now' 1 (prepared env (1 :| [])))
    rejected @?= Left (C.ClientStorageError (H.AdmissionFailure A.DataCapacity))
    atomically $ C.stopAtomicClient client >> putTMVar releaseAck ()
    success running
    wireEmpty client

testPartialCancel :: Assertion
testPartialCancel = withFixture True $ \env -> withClient env 1 1 Nothing $ \client -> do
  req <- admit env client (0 :| [1])
  releaseTail <- newEmptyTMVarIO
  buffered <- newTVarIO Nothing
  first <- newTVarIO True
  let splitAtFirst = fromIntegral (W.txResponseHeaderBytes 2 + W.txResponseElementBytes 3)
      adjust c s =
        pure
          ( c
              { recv = do
                  fresh <- atomically $ do x <- readTVar first; writeTVar first False; pure x
                  if fresh
                    then do
                      input <- fromJust <$> recv c
                      atomically $ writeTVar buffered (Just (BL.drop splitAtFirst input))
                      pure (Just (BL.take splitAtFirst input))
                    else do
                      rest <- atomically $ do rest <- readTVar buffered; writeTVar buffered Nothing; pure rest
                      case rest of
                        Nothing -> recv c
                        Just bytes -> atomically (readTMVar releaseTail) >> pure (Just bytes)
              }
          , s
          )
  withConnection env client (handler env) adjust $ \running -> do
    _ <- awaitEvent running $ readTVar (events env) >>= check . (== 1) . transactionCount
    usage <- right <$> atomically (C.readClientUsage client)
    A.usedResponseBytes usage
      @?= let EncodedBytes n = preparedResponseBytes (H.storageRequestPrepared req) in n
    now <- getMonotonicTime
    ack <- right <$> atomically (C.cancelClientRequest client now req)
    atomically $ C.stopAtomicClient client >> putTMVar releaseTail ()
    success running
    stats <- snapshotEvents env
    map (\(key, c) -> (key, completedOutcome c)) (replies stats)
      @?= [(H.storageRequestKey req, TransactionsReceived), (ack, CancelAcknowledged)]
    (transactionCount stats, usefulCount stats) @?= (2, 2)
    wireEmpty client

testStop :: Assertion
testStop = withFixture True $ \env -> withClient env 1 2 Nothing $ \client -> do
  _ <- admit env client (0 :| [])
  _ <- admit env client (1 :| [])
  atomically $ C.stopAtomicClient client
  now <- getMonotonicTime
  atomically (C.admitClientRequest client now 1 (prepared env (2 :| [])))
    >>= (@?= Left C.ClientStopping)
  withConnection env client (handler env) unchanged $ \running -> success running >> wireEmpty client
  snap <- atomically (C.readClientSnapshot client)
  (C.snapshotClientQueued snap, C.snapshotClientUncollected snap, C.snapshotClientReceiver snap)
    @?= (0, 0, Nothing)

testBodyFault :: Assertion
testBodyFault = withFixture False $ \env -> do
  withClient env 1 2 Nothing $ \client -> do
    now <- getMonotonicTime
    let bodyRequest =
          right $
            prepareBody
              (BodyDescriptor (point env) (generation env) 1 (MkLeiosEbBounds 600000 500000))
              (EncodedBytes 600003)
    req <- right <$> atomically (C.admitClientRequest client now 1 bodyRequest)
    before <- right <$> atomically (C.readClientUsage client)
    A.usedResponseBytes before @?= 600003
    withConnection env client (handler env) unchanged $ \running -> do
      outcome <- bounded (waitCatch (clientTask running))
      case outcome of
        Left err -> fromException err @?= Just C.ClientBodySizeMisstated
        Right _ -> assertFailure "understated body should fault after handoff"
    stats <- snapshotEvents env
    map fst (replies stats) @?= [H.storageRequestKey req]
    abortCount stats @?= 1
  pending <- atomically (H.readStorageSnapshot (handoff env))
  H.snapshotClaims pending @?= 1
  H.snapshotDrainingSessions pending @?= 1
  H.withStorageHandoff (handoff env) (writer env) (const (pure ())) $ \_ -> bounded $ atomically $ do
    snap <- H.readAcquisition (handoff env) (point env)
    check (maybe False H.snapshotBodyDurable snap)
  after <- atomically (H.readStorageSnapshot (handoff env))
  H.snapshotSessions after @?= 0

testBadPrefix :: Assertion
testBadPrefix = withFixture True $ \env -> do
  let bad = S.AtomicRequestHandler $ \request -> case request of
        S.ServeTransactions{} ->
          pure $
            S.PreparedTransactions
              (fromIntegral (W.txResponseBytes [3, 5]))
              (V.fromList [head (txs env), MkLeiosTx (BS.replicate 5 99)])
        _ -> S.prepareAtomicReply (handler env) request
  withClient env 1 2 Nothing $ \client -> do
    _ <- admit env client (0 :| [1])
    let resumeAfterAbort c s =
          pure
            ( c
                { send = \bytes -> do
                    send c bytes
                    atomically $ readTVar (events env) >>= check . (> 0) . abortCount
                }
            , s
            )
    withConnection env client bad resumeAfterAbort $ \running -> do
      result <- bounded (waitCatch (clientTask running))
      case result of
        Left err -> fromException err @?= Just (ReceiveStorageError H.InvalidTransaction)
        Right _ -> assertFailure "invalid second TX must abort"
    snap <- atomically (C.readClientSnapshot client)
    assertBool "failure permanently closes the session" (C.snapshotClientClosed snap)
  snap <- snapshotAcquisition env
  H.snapshotPending snap @?= IntSet.singleton 0
  H.snapshotMissing snap @?= IntSet.fromList [1, 2]
  stats <- snapshotEvents env
  (transactionCount stats, usefulCount stats, abortCount stats) @?= (1, 1, 1)
  H.withStorageHandoff (handoff env) (writer env) (const (pure ())) $ \_ -> bounded $ atomically $ do
    snap' <- H.readAcquisition (handoff env) (point env)
    check (maybe False ((== IntSet.singleton 0) . H.snapshotHeld) snap')

testCoalesced :: Assertion
testCoalesced = withFixture True $ \env -> withClient env 1 2 Nothing $ \client -> do
  a <- admit env client (0 :| [])
  b <- admit env client (1 :| [])
  atomically $ C.stopAtomicClient client
  first <- newTVarIO True
  let adjust c s =
        pure
          ( c
              { recv = do
                  fresh <- atomically $ do x <- readTVar first; writeTVar first False; pure x
                  if fresh
                    then do x <- fromJust <$> recv c; y <- fromJust <$> recv c; pure (Just (x <> y))
                    else recv c
              }
          , s
          )
  withConnection env client (handler env) adjust success
  stats <- snapshotEvents env
  map fst (replies stats) @?= [H.storageRequestKey a, H.storageRequestKey b]
  map (receiptObservedAt . completedReceipt . snd) (replies stats)
    @?= replicate 2 (receiptObservedAt (completedReceipt (snd (head (replies stats)))))

testPublicationGap :: Assertion
testPublicationGap = withFixture True $ \env -> withClient env 1 2 Nothing $ \client -> do
  _ <- admit env client (0 :| [])
  atomically $ C.stopAtomicClient client
  entered <- newEmptyTMVarIO
  release <- newEmptyTMVarIO
  let pause acquire = do
        scope <- acquire
        pure
          scope
            { receiveInput = \observation -> do
                atomically $ putTMVar entered ()
                atomically $ readTMVar release
                receiveInput scope observation
            }
  withConnectionUsing (observedRunner pause) env client (handler env) unchanged $ \running -> do
    _ <- awaitEvent running (readTMVar entered)
    _ <- pollAfterHead running client
    stats <- snapshotEvents env
    expiries stats @?= []
    snap <- atomically $ C.readClientSnapshot client
    T.nextDeadline (C.snapshotClientTiming snap) @?= Nothing
    atomically $ putTMVar release ()
    success running
    wireEmpty client
  stats <- snapshotEvents env
  (replyCount stats, usefulCount stats, abortCount stats) @?= (1, 1, 0)
  expiries stats @?= []

testBufferedTransition :: Assertion
testBufferedTransition = withFixture True $ \env -> withClient env 1 2 Nothing $ \client -> do
  a <- admit env client (0 :| [])
  b <- admit env client (1 :| [])
  atomically $ C.stopAtomicClient client
  count <- newTVarIO (0 :: Int)
  framed <- newEmptyTMVarIO
  releaseFrame <- newEmptyTMVarIO
  successor <- newEmptyTMVarIO
  releaseSuccessor <- newEmptyTMVarIO
  first <- newTVarIO True
  let pause acquire = do
        index <- atomically $ do n <- readTVar count; writeTVar count (n + 1); pure n
        when (index == 1) $ do
          atomically $ putTMVar successor ()
          atomically $ readTMVar releaseSuccessor
        scope <- acquire
        pure
          scope
            { receiveComplete = \receipt buffered -> do
                receiveComplete scope receipt buffered
                when (index == 0) $ do
                  assertBool "fixture did not retain the next reply" buffered
                  atomically $ putTMVar framed ()
                  atomically $ readTMVar releaseFrame
            }
      coalesce c s =
        pure
          ( c
              { recv = do
                  fresh <- atomically $ do x <- readTVar first; writeTVar first False; pure x
                  if fresh
                    then do x <- fromJust <$> recv c; y <- fromJust <$> recv c; pure (Just (x <> y))
                    else recv c
              }
          , s
          )
  withConnectionUsing (observedRunner pause) env client (handler env) coalesce $ \running -> do
    _ <- awaitEvent running (readTMVar framed)
    later <- pollAfterHead running client
    atomically $ putTMVar releaseFrame ()
    _ <- awaitEvent running (readTMVar successor)
    atomically $ C.pollAtomicClient client (addTime 1 later)
    stats <- snapshotEvents env
    expiries stats @?= []
    snap <- atomically $ C.readClientSnapshot client
    T.nextDeadline (C.snapshotClientTiming snap) @?= Nothing
    atomically $ putTMVar releaseSuccessor ()
    success running
    wireEmpty client
  stats <- snapshotEvents env
  map fst (replies stats) @?= [H.storageRequestKey a, H.storageRequestKey b]
  let times = map (receiptObservedAt . completedReceipt . snd) (replies stats)
  times @?= replicate 2 (head times)
  expiries stats @?= []

testPublicationAbort :: Assertion
testPublicationAbort = withFixture True $ \env -> withClient env 1 2 Nothing $ \client -> do
  _ <- admit env client (0 :| [])
  entered <- newEmptyTMVarIO
  never <- newEmptyTMVarIO
  let pause acquire = do
        scope <- acquire
        pure scope{receiveInput = \_ -> atomically (putTMVar entered ()) >> atomically (readTMVar never)}
  withConnectionUsing (observedRunner pause) env client (handler env) unchanged $ \running -> do
    _ <- awaitEvent running (readTMVar entered)
    cancel (clientTask running)
  snap <- atomically $ C.readClientSnapshot client
  assertBool "aborted publication left the client open" (C.snapshotClientClosed snap)
  assertBool "aborted publication retained timing work" (T.timingIdle (C.snapshotClientTiming snap))
  usage <- atomically $ H.readStorageSnapshot (handoff env)
  H.snapshotSessions usage @?= 0
  stats <- snapshotEvents env
  abortCount stats @?= 1

testPartialInputWait :: Assertion
testPartialInputWait = withFixture True $ \env -> withClient env 1 2 Nothing $ \client -> do
  _ <- admit env client (0 :| [])
  waiting <- newEmptyTMVarIO
  never <- newEmptyTMVarIO
  first <- newTVarIO True
  let fragment c s =
        pure
          ( c
              { recv = do
                  fresh <- atomically $ do x <- readTVar first; writeTVar first False; pure x
                  if fresh
                    then Just . BL.take 1 . fromJust <$> recv c
                    else atomically (putTMVar waiting ()) >> atomically (readTMVar never)
              }
          , s
          )
  withConnection env client (handler env) fragment $ \running -> do
    _ <- awaitEvent running (readTMVar waiting)
    _ <- pollAfterHead running client
    stats <- snapshotEvents env
    map T.chargeDataFailure (expiries stats) @?= [True]
    cancel (clientTask running)

testSessionCleanup :: Assertion
testSessionCleanup = withFixture True $ \env -> withClient env 1 2 Nothing $ \old -> withClient env 1 2 Nothing $ \fresh -> do
  now <- getMonotonicTime
  let assign client = atomically $ do
        selected <-
          Q.assignCheckpoint
            (assignments env)
            (handoff env)
            Q.defaultCheckpointLimits
            (C.clientStorageSession client)
            (point env)
            (generation env)
            (MkPeerOffer SNothing OfferedClosure)
            (focused env)
        let req = fromJust (right selected)
        C.publishClientRequest client now 1 req
        pure req
  oldReq <- assign old
  atomically $ C.closeAtomicClient old
  replacement <- assign fresh
  assertBool
    "replacement retains a distinct fetch session"
    (H.storageRequestKey oldReq /= H.storageRequestKey replacement)
  atomically $ do
    Q.releaseSessionAssignments
      (assignments env)
      (fst (H.storageSessionIdentity (C.clientStorageSession old)))
      (snd (H.storageSessionIdentity (C.clientStorageSession old)))
    C.closeAtomicClient old
  owned <- atomically $ Q.readAssignedOffsets (assignments env) (point env) (generation env)
  owned @?= IntSet.fromList [0, 1, 2]
  atomically $ C.stopAtomicClient fresh
  withConnection env fresh (handler env) unchanged success

testRejectedAdmission :: Assertion
testRejectedAdmission = withFixture True $ \env -> withClient env 1 1 Nothing $ \client -> do
  now <- getMonotonicTime
  -- Preparing and discarding is deliberately outside irreversible admission.
  let dropped = prepared env (0 :| [1])
  payloadSize dropped @?= 8
  atomically (C.admitClientRequest client now 0 (prepared env (0 :| [])))
    >>= (@?= Left (C.ClientTimingError D.InvalidBackoffFactor))
  req <- right <$> atomically (C.admitClientRequest client now 1 (prepared env (0 :| [])))
  ordinal (H.storageRequestKey req) @?= WireOrdinal 0
  atomically (C.admitClientRequest client now 1 (prepared env (1 :| [])))
    >>= (@?= Left (C.ClientStorageError (H.AdmissionFailure A.DataCapacity)))
  usage <- right <$> atomically (C.readClientUsage client)
  (A.usedData usage, A.usedExchanges usage) @?= (1, 1)
 where
  payloadSize req = sum [n | PayloadBytes n <- NE.toList (preparedElements req)]

ordinal :: RequestKey peer -> WireOrdinal
ordinal (RequestKey _ _ n) = n

testPublicationRollback :: Assertion
testPublicationRollback = withFixture True $ \env -> do
  let tiny = right (D.mkTimingConfig 1500000 14600 0.5 2 60 (PayloadBytes 6))
  C.withAtomicClient
    (handoff env)
    (assignments env)
    1
    (MkLeiosPeerLifetimeId 1 0)
    2
    1048576
    tiny
    Nothing
    (hooks (events env))
    $ \client -> do
      now <- getMonotonicTime
      result <- try $ atomically $ do
        selected <-
          Q.assignCheckpoint
            (assignments env)
            (handoff env)
            Q.defaultCheckpointLimits
            (C.clientStorageSession client)
            (point env)
            (generation env)
            (MkPeerOffer SNothing OfferedClosure)
            (focused env)
        let req = fromJust (right selected)
        C.publishClientRequest client now 1 req
      result @?= Left (C.ClientTimingError D.PayloadBeyondMaximum)
      owned <- atomically $ Q.readAssignedOffsets (assignments env) (point env) (generation env)
      owned @?= IntSet.empty
      usage <- right <$> atomically (C.readClientUsage client)
      (A.usedData usage, A.usedExchanges usage, A.usedResponseBytes usage) @?= (0, 0, 0)
      req <- admit env client (0 :| [])
      ordinal (H.storageRequestKey req) @?= WireOrdinal 0

testRttSnapshots :: Assertion
testRttSnapshots = withFixture True $ \env -> do
  registry <- newKeepAliveRegistry
  bracketKeepAliveClientWithRtt registry (1 :: Int) $ \context -> do
    raw1 <- getMonotonicTime
    atomically $ publishKeepAliveRtt context (KeepAliveRtt 0.1 raw1)
    reader <- fromJust <$> atomically (lookupKeepAliveRtt registry 1)
    withClient env 1 2 (Just reader) $ \client -> do
      now1 <- getMonotonicTime
      a <- right <$> atomically (C.admitClientRequest client now1 1 (prepared env (0 :| [])))
      raw2 <- getMonotonicTime
      atomically $ publishKeepAliveRtt context (KeepAliveRtt 0.2 raw2)
      now2 <- getMonotonicTime
      b <- right <$> atomically (C.admitClientRequest client now2 1 (prepared env (1 :| [])))
      let RequestKey lifetime token _ = H.storageRequestKey a
          s0 = snd (right (T.reconcileTiming now1 (T.newTimingState lifetime token)))
          s1 =
            right
              ( T.registerTiming
                  (H.storageRequestKey a)
                  ( T.TrackData
                      (right (D.snapshotTiming config now1 (Just (D.RttObservation 0.1 raw1)) 1 (prepared env (0 :| []))))
                  )
                  s0
              )
          s2 = snd (right (T.reconcileTiming now2 s1))
          expected =
            right
              ( T.registerTiming
                  (H.storageRequestKey b)
                  ( T.TrackData
                      (right (D.snapshotTiming config now2 (Just (D.RttObservation 0.2 raw2)) 1 (prepared env (1 :| []))))
                  )
                  s2
              )
      snap <- atomically (C.readClientSnapshot client)
      C.snapshotClientTiming snap @?= expected

testRetiredRtt :: Assertion
testRetiredRtt = withFixture True $ \env -> do
  registry <- newKeepAliveRegistry
  reader <- bracketKeepAliveClientWithRtt registry (1 :: Int) $ \context -> do
    now <- getMonotonicTime
    atomically $ publishKeepAliveRtt context (KeepAliveRtt 0.01 now)
    fromJust <$> atomically (lookupKeepAliveRtt registry 1)
  bracketKeepAliveClientWithRtt registry (1 :: Int) $ \context -> do
    now <- getMonotonicTime
    atomically $ publishKeepAliveRtt context (KeepAliveRtt 0.02 now)
    withClient env 1 2 (Just reader) $ \client -> do
      admittedAt <- getMonotonicTime
      req <- right <$> atomically (C.admitClientRequest client admittedAt 1 (prepared env (0 :| [])))
      let RequestKey lifetime token _ = H.storageRequestKey req
          initial = snd (right (T.reconcileTiming admittedAt (T.newTimingState lifetime token)))
          expected =
            right
              ( T.registerTiming
                  (H.storageRequestKey req)
                  (T.TrackData (right (D.snapshotTiming config admittedAt Nothing 1 (prepared env (0 :| [])))))
                  initial
              )
      snap <- atomically (C.readClientSnapshot client)
      C.snapshotClientTiming snap @?= expected

testGuard :: Assertion
testGuard = withFixture True $ \env -> do
  let unsafe = right (D.mkTimingConfig 1000 14600 0.5 2 10000 (PayloadBytes 600000))
  result <-
    try $
      C.withAtomicClient
        (handoff env)
        (assignments env)
        1
        (MkLeiosPeerLifetimeId 1 0)
        2
        1048576
        unsafe
        Nothing
        (hooks (events env))
        (const (pure ()))
  result @?= Left (C.ClientTimingError D.IncompatibleFatalGuard)
  usage <- atomically (H.readStorageSnapshot (handoff env))
  H.snapshotSessions usage @?= 0

testLongSession :: Assertion
testLongSession = withFixture True $ \env -> withClient env 1 2 Nothing $ \client -> do
  withConnection env client (handler env) unchanged $ \running -> do
    forM_ [1 .. 200] $ \n -> do
      _ <- admit env client (0 :| [])
      waitReplies env running n
    atomically $ C.stopAtomicClient client
    success running
    wireEmpty client
    snap <- atomically (C.readClientSnapshot client)
    (C.snapshotClientQueued snap, C.snapshotClientUncollected snap) @?= (0, 0)
    T.activeDeadline (C.snapshotClientTiming snap) @?= Nothing
    usage <- right <$> atomically (C.readClientUsage client)
    A.usedHandoffBytes usage @?= 3
  stats <- snapshotEvents env
  (replyCount stats, transactionCount stats, usefulCount stats) @?= (200, 200, 1)
  length (replies stats) @?= 16

testAckExpiry :: Assertion
testAckExpiry = withFixture True $ \env -> withRtt (1 / 1000) $ \reader -> withClient env 1 1 (Just reader) $ \client -> do
  req <- admit env client (0 :| [])
  entered <- newEmptyTMVarIO
  never <- newEmptyTMVarIO
  releaseAck <- newEmptyTMVarIO
  sends <- newTVarIO (0 :: Int)
  let adjust c s =
        pure
          ( c
          , s
              { send = \bytes -> do
                  n <- atomically $ do n <- readTVar sends; writeTVar sends (n + 1); pure n
                  when (n == 1) (atomically (readTMVar releaseAck))
                  send s bytes
              }
          )
  withConnection env client (blockedHandler env entered never) adjust $ \running -> do
    _ <- awaitEvent running (readTMVar entered)
    now <- getMonotonicTime
    ack <- right <$> atomically (C.cancelClientRequest client now req)
    waitReplies env running 1
    deadline <- awaitEvent running $ do
      snap <- C.readClientSnapshot client
      case T.activeDeadline (C.snapshotClientTiming snap) of
        Just (T.TimerToken key _, at) | key == ack -> pure at
        _ -> retry
    now' <- getMonotonicTime
    SI.threadDelay (max 0 (diffTime deadline now') + 0.01)
    expiredAt <- getMonotonicTime
    atomically $ C.pollAtomicClient client expiredAt >> C.pollAtomicClient client expiredAt
    stats <- snapshotEvents env
    let ackEvents = [ev | ev <- expiries stats, T.TimerToken key _ <- [T.expiryToken ev], key == ack]
    map T.chargeDataFailure ackEvents @?= [False]
    usage <- right <$> atomically (C.readClientUsage client)
    (A.usedData usage, A.usedExchanges usage) @?= (1, 1)
    atomically $ C.stopAtomicClient client >> putTMVar releaseAck ()
    success running
    wireEmpty client

testSingleUse :: Assertion
testSingleUse = withFixture True $ \env -> withClient env 1 2 Nothing $ \client -> do
  atomically $ C.stopAtomicClient client
  withConnection env client (handler env) unchanged success
  withConnection env client (handler env) unchanged $ \running -> do
    result <- bounded (waitCatch (clientTask running))
    case result of
      Left err -> fromException err @?= Just C.ClientAlreadyStarted
      Right _ -> assertFailure "client interpreter cannot be reused"

-- Budgets are per connection/session, not per EB. A rejected second body must
-- neither allocate an ordinal nor prevent a distinct smaller TX request.
testCrossEbBudget :: Assertion
testCrossEbBudget = withFixture True $ \env -> withClient env 1 2 Nothing $ \client -> do
  let point2 = MkLeiosPoint (SlotNo 6) (pointEbHash (point env))
  gen2 <- right <$> atomically (H.beginAcquisition (handoff env) point2)
  _ <- await =<< writeEbBody (writer env) point2 (body env)
  H.registerDurableBody (handoff env) (const (pure ())) point2 gen2 (body env) IntSet.empty
    >>= (@?= Right ())
  now <- getMonotonicTime
  let body1 =
        right $
          prepareBody
            (BodyDescriptor (point env) (generation env) 100 (MkLeiosEbBounds 600000 500000))
            (EncodedBytes 600003)
      body2 =
        right $
          prepareBody (BodyDescriptor point2 gen2 100 (MkLeiosEbBounds 600000 500000)) (EncodedBytes 600003)
      tx2 =
        right $
          prepareBatch point2 gen2 (body env) (0 :| []) (EncodedBytes (fromIntegral (W.txResponseBytes [3])))
  a <- right <$> atomically (C.admitClientRequest client now 1 body1)
  atomically (C.admitClientRequest client now 1 body2)
    >>= (@?= Left (C.ClientStorageError (H.AdmissionFailure A.ResponseCapacity)))
  b <- right <$> atomically (C.admitClientRequest client now 1 tx2)
  (ordinal (H.storageRequestKey a), ordinal (H.storageRequestKey b))
    @?= (WireOrdinal 0, WireOrdinal 1)
  usage <- right <$> atomically (C.readClientUsage client)
  (A.usedData usage, A.usedExchanges usage, A.usedResponseBytes usage)
    @?= (2, 2, 600003 + fromIntegral (W.txResponseBytes [3]))

-- Deliberate removal retires metadata without manufacturing a peer fault. The
-- interpreter's command wait wakes and its structured driver joins reception.
testRemoval :: Assertion
testRemoval = withFixture True $ \env -> withClient env 1 2 Nothing $ \client -> do
  _ <- admit env client (0 :| [])
  entered <- newEmptyTMVarIO
  never <- newEmptyTMVarIO
  withConnection env client (blockedHandler env entered never) unchanged $ \running -> do
    _ <- awaitEvent running (readTMVar entered)
    atomically $ C.closeAtomicClient client
    result <- bounded (waitCatch (clientTask running))
    case result of
      Left err -> fromException err @?= Just C.ClientClosed
      Right _ -> assertFailure "removed client must stop its interpreter"
  snap <- atomically (C.readClientSnapshot client)
  assertBool "removal is permanent" (C.snapshotClientClosed snap)
  stats <- snapshotEvents env
  abortCount stats @?= 0
  usage <- atomically (H.readStorageSnapshot (handoff env))
  H.snapshotSessions usage @?= 0

testDataExpiry :: Assertion
testDataExpiry = withFixture True $ \env -> withRtt (1 / 1000) $ \reader -> withClient env 1 2 (Just reader) $ \client -> do
  req <- admit env client (0 :| [])
  entered <- newEmptyTMVarIO
  never <- newEmptyTMVarIO
  withConnection env client (blockedHandler env entered never) unchanged $ \running -> do
    _ <- awaitEvent running (readTMVar entered)
    SI.threadDelay 0.02
    now <- getMonotonicTime
    atomically $ C.pollAtomicClient client now >> C.pollAtomicClient client now
    stats <- snapshotEvents env
    let dataEvents =
          [ ev | ev <- expiries stats, T.TimerToken key _ <- [T.expiryToken ev], key == H.storageRequestKey req
          ]
    map T.chargeDataFailure dataEvents @?= [True]
    assertBool
      "late reconciliation did not supply its monotone handling time"
      (all (>= now) (expiryHandlingTimes stats) && not (null (expiryHandlingTimes stats)))
    assertBool "deadline was confused with later handling time" (all ((< now) . T.expiryAt) dataEvents)
    usage <- right <$> atomically (C.readClientUsage client)
    (A.usedData usage, A.usedExchanges usage) @?= (1, 1)
    atomically (C.admitClientRequest client now 1 (prepared env (1 :| [])))
      >>= (@?= Left C.ClientUpstreamBlocked)
    _ <- right <$> atomically (C.cancelClientRequest client now req)
    atomically $ C.stopAtomicClient client
    success running
    wireEmpty client

-- A live original retains the timing snapshot admitted before subsequent retry
-- pressure recovery. The next request uses the newly recovered multiplier.
testRetrySnapshot :: Assertion
testRetrySnapshot = withFixture True $ \env -> withClient env 1 2 Nothing $ \client -> do
  admittedAt <- getMonotonicTime
  let grown =
        Retry.recordRetryFailure
          Retry.defaultRetryConfig
          admittedAt
          (Retry.newRetryState Retry.defaultRetryConfig)
      oldFactor = Retry.retryFactor Retry.defaultRetryConfig grown
  _ <-
    right <$> atomically (C.admitClientRequest client admittedAt oldFactor (prepared env (0 :| [])))
  entered <- newEmptyTMVarIO
  release <- newEmptyTMVarIO
  withConnection env client (blockedHandler env entered release) unchanged $ \running -> do
    _ <- awaitEvent running (readTMVar entered)
    before <- T.activeDeadline . C.snapshotClientTiming <$> atomically (C.readClientSnapshot client)
    assertBool "first request never activated" (before /= Nothing)
    let recovered = Retry.recoverRetryService Retry.defaultRetryConfig 20000 20000 grown
        newFactor = Retry.retryFactor Retry.defaultRetryConfig recovered
    assertBool "fixture did not reduce future retry pressure" (newFactor < oldFactor)
    now <- getMonotonicTime
    _ <- right <$> atomically (C.admitClientRequest client now newFactor (prepared env (1 :| [])))
    after <- T.activeDeadline . C.snapshotClientTiming <$> atomically (C.readClientSnapshot client)
    after @?= before
    atomically $ C.stopAtomicClient client >> putTMVar release ()
    success running
    wireEmpty client
