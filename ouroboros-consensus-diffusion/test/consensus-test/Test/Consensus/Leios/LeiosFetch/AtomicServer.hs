{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.Leios.LeiosFetch.AtomicServer (tests) where

import Cardano.Slotting.Slot (SlotNo (..))
import qualified Codec.CBOR.Read as CBOR
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (SomeException, bracket, bracket_, catch, fromException, throwIO, try)
import Control.Monad (forM_, void)
import Control.Monad.Class.MonadAsync (waitCatch, waitCatchSTM, withAsync)
import qualified Control.Monad.Class.MonadTimer.SI as SI
import Control.Tracer (nullTracer)
import Data.Bits (bit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Vector.Strict as V
import Network.TypedProtocol.Codec
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Inbound (InboundError (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Server
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Server.State as S
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Type
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision (offsetsToBitmap)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity (WireOrdinal (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Server (ExnLeiosInvalidRequest (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type (bitmapOffsets)
import Ouroboros.Consensus.Network.LeiosFetch (runLookaheadPeerWithAnnotatedLimits)
import Ouroboros.Consensus.Network.NodeToNode.LeiosFetch (withLeiosFetchAtomicServer)
import Ouroboros.Consensus.Storage.LeiosDB
  ( LeiosDbHandle (..)
  , LeiosDbReader (..)
  , LeiosDbWriter (..)
  , Promise (..)
  , newLeiosDBInMemory
  , withLeiosDBSQLite
  , withReader
  , withWriter
  )
import Ouroboros.Network.Channel (Channel (..))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

type Atomic = LeiosFetchAtomic LeiosPoint EndorserBlock LeiosTx
type Server = AtomicServer LeiosPoint EndorserBlock LeiosTx IO

tests :: TestTree
tests =
  testGroup
    "atomic lookahead server"
    [ testCase "D data records leave room to read D valid cancellations" testWindow
    , testCase "a prepared response can still lose the atomic commit race" testPreparedCancel
    , testCase
        "commit releases slots but retains one output charge until send-return"
        testCommitAccounting
    , testCase "old/control/repeated/future cancel targets and wrong ordinals fail" testInvalidControls
    , testCase "response reservations check exact fits, overflow and invalid limits" testResourceBounds
    , testCase "FIFO reply ordering and paired ACK retirement are enforced" testOrder
    , testCase "ordinal exhaustion cannot wrap and Done can still drain" testExhaustion
    , testCase "mixed body/TX replies use the real annotated lookahead driver" testMixed
    , testCase
        "cancellation interrupts blocked preparation and preserves the next request"
        testCancelPreparing
    , testCase "a canceled preparation's late normal result is discarded" testCanceledLateResult
    , testCase "full-window cancels bypass blocked output and skip unstarted preparation" testCancelQueued
    , testCase "a late cancel cannot replace a committed normal response" testCancelCommitted
    , testCase "a fast client can replace a consumed reply before send-return" testFastReply
    , testCase "Done waits for every accepted response without canceling its preparation" testDoneDrain
    , testCase "disconnect joins preparation before the server scope releases its reader" testDisconnect
    , testCase "preparation and send exceptions retire the bounded server lifetime" testFailures
    , testCase "a server handle cannot run twice" testSingleUse
    , testCase "long sessions drain completed sender bookkeeping continuously" testLongSession
    , testCase "real storage responses match independent CBOR byte counts" testStorage
    , testCase "SQLite readers remain exclusive across preparation tasks" testStorageSQLite
    , testCase "oversized requested payloads are rejected before TX retrieval" testOversizedRequest
    , testCase "the storage provider requires complete protocol-sized response buffers" testProviderLimits
    ]

latch :: IO (StrictTMVar IO ())
latch = newEmptyTMVarIO

right :: Show e => Either e a -> a
right = either (error . show) id

txs :: [LeiosTx]
txs = [MkLeiosTx (BS.replicate n b) | (n, b) <- [(3, 1), (5, 2), (7, 3)]]

body :: EndorserBlock
body = endorserBlockFromTxBytes (leiosTxBytes (head txs) :| map leiosTxBytes (tail txs))

point :: LeiosPoint
point = MkLeiosPoint (SlotNo 5) (endorserBlockHash body)

codec :: AnnotatedCodec Atomic CBOR.DeserialiseFailure IO BL.ByteString
codec =
  codecLeiosFetchAtomic
    maxTxsPerEndorserBlock
    encodeLeiosPoint
    decodeLeiosPoint
    encodeEndorserBlock
    decodeEndorserBlock
    encodeLeiosTx
    decodeLeiosTx

limits :: S.ServerLimits
limits = S.ServerLimits 2 4096 4096 4096 maxBound

bodyRequest :: Word -> Message Atomic StIdle (StBusy StBlock)
bodyRequest n = MsgLeiosBlockRequest (WireOrdinal (fromIntegral n)) point

txRequest :: Word -> Int -> Message Atomic StIdle (StBusy StBlockTxs)
txRequest n off = MsgLeiosBlockTxsRequest (WireOrdinal (fromIntegral n)) point ((0, bit (63 - off)) :| [])

tryCancel :: Word -> Word -> Message Atomic StIdle (StBusy StCancel)
tryCancel n target = MsgTryCancel (WireOrdinal (fromIntegral n)) (WireOrdinal (fromIntegral target))

initial :: S.ServerState
initial = right (S.newServerState limits >>= S.startServer)

apply :: Message Atomic StIdle to -> S.ServerState -> S.ServerState
apply message = right . S.acceptServerMessage message

normalCommit :: WireOrdinal -> S.ServerState -> S.ServerState
normalCommit key state =
  let (_, reading) = right (S.beginPreparation key state)
      ready = right (S.finishPreparation key (Just 42) reading)
      (choice, sending) = right (S.commitDataReply key ready)
   in if choice == S.NormalReply then sending else error "unexpected canceled result"

testWindow :: Assertion
testWindow = do
  let full = apply (bodyRequest 1) (apply (bodyRequest 0) initial)
  S.acceptServerMessage (bodyRequest 2) full @?= Left S.ServerDataCapacity
  let canceled = apply (tryCancel 3 1) (apply (tryCancel 2 0) full)
      fullSnapshot = S.serverSnapshot canceled
  S.snapshotDataRecords fullSnapshot @?= 2
  S.snapshotControls fullSnapshot @?= 2
  S.snapshotExchanges fullSnapshot @?= 4
  assertBool "bounded full window" (S.serverInvariant canceled)

testPreparedCancel :: Assertion
testPreparedCancel = do
  let admitted = apply (bodyRequest 0) initial
      (_, reading) = right (S.beginPreparation (WireOrdinal 0) admitted)
      ready = right (S.finishPreparation (WireOrdinal 0) (Just 42) reading)
      canceled = apply (tryCancel 1 0) ready
      (choice, committed) = right (S.commitDataReply (WireOrdinal 0) canceled)
  choice @?= S.CancelledReply
  S.snapshotPreparationBytes (S.serverSnapshot committed) @?= 0
  S.snapshotSendingBytes (S.serverSnapshot committed) @?= 2
  assertBool "canceled pair remains bounded" (S.serverInvariant committed)

testCommitAccounting :: Assertion
testCommitAccounting = do
  let one = right (S.newServerState limits{S.serverDataLimit = 1} >>= S.startServer)
      committed = normalCommit (WireOrdinal 0) (apply (bodyRequest 0) one)
      extended = apply (bodyRequest 1) committed
  S.snapshotDataRecords (S.serverSnapshot extended) @?= 1
  S.snapshotSendingBytes (S.serverSnapshot extended) @?= 42
  S.snapshotExchanges (S.serverSnapshot extended) @?= 1
  S.beginPreparation (WireOrdinal 1) extended @?= Left S.ServerInvalidPreparation
  assertBool "one separate output charge" (S.serverInvariant extended)

testInvalidControls :: Assertion
testInvalidControls = do
  let a = apply (bodyRequest 0) initial
      c = apply (tryCancel 1 0) a
  S.acceptServerMessage (tryCancel 0 0) initial
    @?= Left (S.ServerIncoming IncomingCancelOutsideWindow)
  S.acceptServerMessage (tryCancel 2 0) c @?= Left (S.ServerIncoming IncomingRepeatedCancel)
  S.acceptServerMessage (tryCancel 2 1) c @?= Left (S.ServerIncoming IncomingControlTarget)
  S.acceptServerMessage (tryCancel 2 3) c @?= Left (S.ServerIncoming IncomingCancelOutsideWindow)
  S.acceptServerMessage (bodyRequest 3) a @?= Left (S.ServerIncoming UnexpectedIncomingOrdinal)
  let completed =
        foldl
          ( \state n ->
              right
                ( S.replySent
                    (WireOrdinal (fromIntegral n))
                    (normalCommit (WireOrdinal (fromIntegral n)) (apply (bodyRequest n) state))
                )
          )
          initial
          [0 .. 3]
  S.acceptServerMessage (tryCancel 4 0) completed
    @?= Left (S.ServerIncoming IncomingCancelOutsideWindow)

testResourceBounds :: Assertion
testResourceBounds = do
  S.newServerState limits{S.serverDataLimit = 0} @?= Left S.InvalidServerLimits
  S.newServerState limits{S.serverPreparedLimit = 4095} @?= Left S.InvalidServerLimits
  let (_, reading) = right (S.beginPreparation (WireOrdinal 0) (apply (bodyRequest 0) initial))
  S.finishPreparation (WireOrdinal 0) (Just 4097) reading @?= Left (S.ServerPreparedLimit 4097 4096)
  S.finishPreparation (WireOrdinal 0) (Just 0) reading @?= Left (S.ServerPreparedLimit 0 4096)
  let ready = right (S.finishPreparation (WireOrdinal 0) (Just 4096) reading)
  S.snapshotPreparationBytes (S.serverSnapshot ready) @?= 4096
  assertBool "exact byte cap" (S.serverInvariant ready)
  let widened = right (S.newServerState (S.defaultServerLimits maxBound) >>= S.startServer)
  assertBool "Word16 count widens safely" (S.serverInvariant widened)

testOrder :: Assertion
testOrder = do
  let both = apply (bodyRequest 1) (apply (bodyRequest 0) initial)
      canceled = apply (tryCancel 2 0) both
  S.commitDataReply (WireOrdinal 1) canceled @?= Left S.ServerReplyOrder
  let (_, first) = right (S.commitDataReply (WireOrdinal 0) canceled)
      firstSent = right (S.replySent (WireOrdinal 0) first)
  S.snapshotDataRecords (S.serverSnapshot firstSent) @?= 2
  S.commitCancelAck (WireOrdinal 2) firstSent @?= Left S.ServerReplyOrder
  let second = normalCommit (WireOrdinal 1) firstSent
      secondSent = right (S.replySent (WireOrdinal 1) second)
      ack = right (S.commitCancelAck (WireOrdinal 2) secondSent)
  S.snapshotDataRecords (S.serverSnapshot ack) @?= 0
  assertBool "paired record retires with its ACK" (S.serverInvariant ack)

testExhaustion :: Assertion
testExhaustion = do
  let start = right (S.newServerState limits{S.serverLastOrdinal = 1} >>= S.startServer)
      both = apply (bodyRequest 1) (apply (bodyRequest 0) start)
  S.acceptServerMessage (tryCancel 2 0) both @?= Left (S.ServerIncoming IncomingOrdinalExhausted)
  let done = apply MsgDone both
  assertBool "Done is not an ordinal" (S.snapshotIntakeDone (S.serverSnapshot done))
  S.finishServer done @?= Left S.ServerNotDrained

prepareFixture ::
  forall busy.
  AtomicDataRequest LeiosPoint EndorserBlock LeiosTx busy ->
  IO (PreparedReply LeiosPoint EndorserBlock LeiosTx busy)
prepareFixture = \case
  ServeBody _ _ -> pure (PreparedBody (fromIntegral (BL.length (encode codec (MsgLeiosBlock body)))) body)
  ServeTransactions _ _ runs -> do
    let values = V.fromList [txs !! off | off <- bitmapOffsets (toList runs)]
    pure
      (PreparedTransactions (fromIntegral (BL.length (encode codec (MsgLeiosBlockTxs values)))) values)
 where
  toList (x :| xs) = x : xs

data Harness = Harness
  { handle :: !Server
  , input :: !(StrictTQueue IO (Maybe BL.ByteString))
  , output :: !(StrictTQueue IO BL.ByteString)
  , outcome :: IO (Either SomeException ())
  , terminated :: STM IO (Either SomeException ())
  }

withHarness ::
  S.ServerLimits ->
  AtomicRequestHandler LeiosPoint EndorserBlock LeiosTx IO ->
  (BL.ByteString -> IO ()) ->
  (Harness -> IO a) ->
  IO a
withHarness config handler = withHarnessServer (withAtomicServer config handler)

withHarnessServer ::
  (forall a. (Server -> IO a) -> IO a) ->
  (BL.ByteString -> IO ()) ->
  (Harness -> IO r) ->
  IO r
withHarnessServer scope sent action = scope $ \handle -> do
  input <- newTQueueIO
  output <- newTQueueIO
  let channel =
        Channel
          { recv = atomically (readTQueue input)
          , send = \bytes -> atomically (writeTQueue output bytes) >> sent bytes
          }
      run =
        runLookaheadPeerWithAnnotatedLimits
          nullTracer
          codec
          (byteLimitsLeiosFetchAtomic (fromIntegral . BL.length))
          timeLimitsLeiosFetchAtomic
          channel
          (atomicServerPeer handle)
  withAsync run $ \task ->
    action
      Harness
        { handle
        , input
        , output
        , outcome = fmap void (waitCatch task)
        , terminated = fmap void (waitCatchSTM task)
        }

bounded :: IO a -> IO a
bounded action = SI.timeout 5 action >>= maybe (assertFailure "server fixture timed out") pure

push :: Harness -> Message Atomic StIdle to -> IO ()
push env message = atomically $ writeTQueue (input env) (Just (encode codec message))

snapshot :: Harness -> (S.ServerSnapshot -> Bool) -> IO S.ServerSnapshot
snapshot env predicate = bounded $ atomically $ do
  value <- readAtomicServerSnapshot (handle env)
  check (predicate value)
  pure value

success :: Harness -> Assertion
success env = bounded (outcome env) >>= either (assertFailure . show) pure

failure :: Harness -> IO SomeException
failure env = bounded (outcome env) >>= either pure (const (assertFailure "expected server failure"))

rawReply :: Harness -> IO BL.ByteString
rawReply env =
  bounded $
    atomically $
      readTQueue (output env) `orElse` do
        terminated env
          >>= either throwSTM (const (throwSTM (userError "server drained without expected reply")))

decodeReply ::
  ActiveState st => SingLeiosFetchAtomic (st :: Atomic) -> BL.ByteString -> IO (SomeMessage st)
decodeReply token bytes = do
  step <- decode codec token
  runDecoder [bytes] step >>= either (assertFailure . show) (pure . (`runAnnotator` bytes))

bodyReply :: Harness -> IO (Either () EndorserBlock)
bodyReply env =
  rawReply env >>= decodeReply SingBlock >>= \case
    SomeMessage (MsgLeiosBlock eb) -> pure (Right eb)
    SomeMessage MsgBlockCancelled -> pure (Left ())

txReply :: Harness -> IO (Either () [LeiosTx])
txReply env =
  rawReply env >>= decodeReply SingBlockTxs >>= \case
    SomeMessage (MsgLeiosBlockTxs values) -> pure (Right (V.toList values))
    SomeMessage MsgBlockTxsCancelled -> pure (Left ())

ackReply :: Harness -> Assertion
ackReply env =
  rawReply env >>= decodeReply SingCancel >>= \case
    SomeMessage MsgAckTryCancel -> pure ()

testMixed :: Assertion
testMixed = withHarness limits (AtomicRequestHandler prepareFixture) (const (pure ())) $ \env -> do
  push env (bodyRequest 0)
  push env (txRequest 1 1)
  push env MsgDone
  bodyReply env
    >>= either
      (const (assertFailure "unexpected cancel"))
      (\eb -> endorserBlockBytes eb @?= endorserBlockBytes body)
  txReply env >>= (@?= Right [txs !! 1])
  success env

testCancelPreparing :: Assertion
testCancelPreparing = do
  entered <- latch
  exited <- latch
  block <- latch
  let prepare ::
        forall busy.
        AtomicDataRequest LeiosPoint EndorserBlock LeiosTx busy ->
        IO (PreparedReply LeiosPoint EndorserBlock LeiosTx busy)
      prepare request = case request of
        ServeBody{} ->
          bracket_ (atomically (putTMVar entered ())) (atomically (putTMVar exited ())) $
            atomically (readTMVar block) >> prepareFixture request
        _ -> do
          atomically (readTMVar exited) -- a canceled reader must have joined first
          prepareFixture request
  withHarness limits (AtomicRequestHandler prepare) (const (pure ())) $ \env -> do
    push env (bodyRequest 0)
    bounded (atomically (readTMVar entered))
    push env (txRequest 1 1)
    push env (tryCancel 2 0)
    push env MsgDone
    bodyReply env >>= (@?= Left ())
    txReply env >>= (@?= Right [txs !! 1])
    ackReply env
    success env
    bounded (atomically (readTMVar exited))

testCancelQueued :: Assertion
testCancelQueued = do
  started <- newTVarIO ([] :: [Word])
  readStarted <- latch
  readBlock <- latch
  sendBlock <- latch
  let prepare ::
        forall busy.
        AtomicDataRequest LeiosPoint EndorserBlock LeiosTx busy ->
        IO (PreparedReply LeiosPoint EndorserBlock LeiosTx busy)
      prepare request = do
        let WireOrdinal n = case request of ServeBody key _ -> key; ServeTransactions key _ _ -> key
        atomically $ modifyTVar started (++ [fromIntegral n])
        case request of
          ServeBody{} ->
            atomically (putTMVar readStarted ()) >> atomically (readTMVar readBlock) >> prepareFixture request
          _ -> prepareFixture request
  withHarness limits (AtomicRequestHandler prepare) (\_ -> atomically (readTMVar sendBlock)) $ \env -> do
    push env (bodyRequest 0)
    bounded (atomically (readTMVar readStarted))
    _ <- snapshot env ((> 0) . S.snapshotPreparationBytes)
    push env (txRequest 1 1)
    push env (tryCancel 2 1)
    push env (tryCancel 3 0)
    push env MsgDone
    state <- snapshot env S.snapshotIntakeDone
    assertBool "D controls were admitted" (S.snapshotControls state == 2)
    atomically $ putTMVar sendBlock ()
    bodyReply env >>= (@?= Left ())
    txReply env >>= (@?= Left ())
    ackReply env >> ackReply env
    success env
  readTVarIO started >>= (@?= [0])

testCancelCommitted :: Assertion
testCancelCommitted = do
  blocked <- latch
  release <- latch
  sends <- newTVarIO (0 :: Int)
  let sent _ = do
        first <- atomically $ do n <- readTVar sends; writeTVar sends (n + 1); pure (n == 0)
        if first then atomically (putTMVar blocked ()) >> atomically (readTMVar release) else pure ()
  withHarness limits (AtomicRequestHandler prepareFixture) sent $ \env -> do
    push env (bodyRequest 0)
    bounded (atomically (readTMVar blocked))
    push env (txRequest 1 1)
    push env (tryCancel 2 0)
    push env MsgDone
    state <- snapshot env S.snapshotIntakeDone
    assertBool "committed output still charged" (S.snapshotSendingBytes state > 0)
    _ <- bodyReply env
    atomically $ putTMVar release ()
    txReply env >>= (@?= Right [txs !! 1])
    ackReply env
    success env

testFastReply :: Assertion
testFastReply = do
  blocked <- latch
  release <- latch
  calls <- newTVarIO (0 :: Int)
  let sent _ = do
        n <- atomically $ do n <- readTVar calls; writeTVar calls (n + 1); pure n
        if n == 0 then atomically (putTMVar blocked ()) >> atomically (readTMVar release) else pure ()
  withHarness limits{S.serverDataLimit = 1} (AtomicRequestHandler prepareFixture) sent $ \env -> do
    push env (bodyRequest 0)
    _ <- bodyReply env
    bounded (atomically (readTMVar blocked))
    push env (txRequest 1 1)
    push env MsgDone
    state <- snapshot env S.snapshotIntakeDone
    S.snapshotDataRecords state @?= 1
    assertBool "old output remains charged" (S.snapshotSending state)
    atomically $ putTMVar release ()
    txReply env >>= (@?= Right [txs !! 1])
    success env

testDoneDrain :: Assertion
testDoneDrain = do
  unblock <- latch
  let prepare ::
        forall busy.
        AtomicDataRequest LeiosPoint EndorserBlock LeiosTx busy ->
        IO (PreparedReply LeiosPoint EndorserBlock LeiosTx busy)
      prepare request = atomically (readTMVar unblock) >> prepareFixture request
  withHarness limits (AtomicRequestHandler prepare) (const (pure ())) $ \env -> do
    push env (bodyRequest 0)
    push env (txRequest 1 0)
    push env MsgDone
    _ <- snapshot env S.snapshotIntakeDone
    atomically (isEmptyTQueue (output env)) >>= (@?= True)
    atomically $ putTMVar unblock ()
    _ <- bodyReply env
    txReply env >>= (@?= Right [head txs])
    success env

testDisconnect :: Assertion
testDisconnect = do
  active <- newTVarIO False
  blocker <- latch
  let prepare ::
        forall busy.
        AtomicDataRequest LeiosPoint EndorserBlock LeiosTx busy ->
        IO (PreparedReply LeiosPoint EndorserBlock LeiosTx busy)
      prepare request =
        bracket_ (atomically (writeTVar active True)) (atomically (writeTVar active False)) $
          atomically (readTMVar blocker) >> prepareFixture request
  saved <- withHarness limits (AtomicRequestHandler prepare) (const (pure ())) $ \env -> do
    push env (bodyRequest 0)
    bounded (atomically (readTVar active >>= check))
    atomically $ writeTQueue (input env) Nothing
    _ <- failure env
    readTVarIO active >>= (@?= False)
    pure (handle env)
  state <- atomically $ readAtomicServerSnapshot saved
  assertBool "retired after preparation joined" (S.snapshotStopped state)
  S.snapshotPreparationBytes state @?= 0
  S.snapshotSendingBytes state @?= 0

testFailures :: Assertion
testFailures = do
  let broken = AtomicRequestHandler (\_ -> throwIO (userError "preparation failed"))
  withHarness limits broken (const (pure ())) $ \env -> push env (bodyRequest 0) >> void (failure env)
  withHarness limits (AtomicRequestHandler prepareFixture) (\_ -> throwIO (userError "send failed")) $ \env -> do
    push env (bodyRequest 0)
    void (failure env)
  withHarness limits (AtomicRequestHandler prepareFixture) (const (pure ())) $ \env -> do
    push env (tryCancel 0 0)
    err <- failure env
    fromException err @?= Just (S.ServerIncoming IncomingCancelOutsideWindow)

testSingleUse :: Assertion
testSingleUse = withAtomicServer limits (AtomicRequestHandler prepareFixture) $ \server -> do
  let run = withHarnessServer (\k -> k server) (const (pure ()))
  run $ \env -> push env MsgDone >> success env
  run $ \env -> do
    err <- failure env
    fromException err @?= Just S.ServerAlreadyStarted

testStorage :: Assertion
testStorage = bracket newLeiosDBInMemory closeLeiosDbHandle testStorageDb

testStorageSQLite :: Assertion
testStorageSQLite = withSystemTempDirectory "leios-atomic-server" $ \dir ->
  withLeiosDBSQLite nullTracer (dir <> "/vol.db") (dir <> "/imm.db") testStorageDb

testStorageDb :: LeiosDbHandle IO -> Assertion
testStorageDb db = withWriter db $ \writer -> do
  let bytes =
        [BS.replicate n (fromIntegral i) | (i, n) <- zip [1 :: Int ..] [1, 23, 24, 255, 256, 65535, 65536]]
          ++ [BS.pack [fromIntegral (i `div` 256), fromIntegral i] | i <- [1 :: Int .. 270]]
      eb = endorserBlockFromTxBytes (head bytes :| tail bytes)
      p = MkLeiosPoint (SlotNo 5) (endorserBlockHash eb)
      request :: Message Atomic StIdle (StBusy StBlockTxs)
      request =
        MsgLeiosBlockTxsRequest
          (WireOrdinal 1)
          p
          ( case offsetsToBitmap (IntSet.fromList [0 .. length bytes - 1]) of
              x : xs -> x :| xs
              [] -> error "empty request"
          )
  _ <- await =<< writeEbBody writer p eb
  _ <- await =<< writeTxs writer p (zip [0 ..] bytes)
  withReader db $ \reader -> do
    unblock <- latch
    withHarnessServer
      (withLeiosFetchAtomicServer nullTracer (S.defaultServerLimits 2) reader)
      (\_ -> atomically (takeTMVar unblock))
      $ \env -> do
        push env (MsgLeiosBlockRequest (WireOrdinal 0) p)
        bodyBytes <- rawReply env
        state <- snapshot env S.snapshotSending
        S.snapshotSendingBytes state @?= fromIntegral (BL.length bodyBytes)
        atomically $ putTMVar unblock ()
        push env request
        push env MsgDone
        txBytes <- rawReply env
        txState <- snapshot env S.snapshotSending
        S.snapshotSendingBytes txState @?= fromIntegral (BL.length txBytes)
        atomically $ putTMVar unblock ()
        _ <- decodeReply SingBlockTxs txBytes
        -- Independent encoding of the original bytes covers CBOR length edges.
        txBytes @?= encode codec (MsgLeiosBlockTxs (V.fromList (map MkLeiosTx bytes)))
        success env

testOversizedRequest :: Assertion
testOversizedRequest = bracket newLeiosDBInMemory closeLeiosDbHandle $ \db -> withWriter db $ \writer -> do
  let eb = mkEndorserBlock [(hashLeiosTx (head txs), 500001)]
      p = MkLeiosPoint (SlotNo 5) (endorserBlockHash eb)
  _ <- await =<< writeEbBody writer p eb
  retrievals <- newTVarIO (0 :: Int)
  withReader db $ \reader -> do
    let counted =
          reader
            { batchRetrieveTxs = \requestedPoint os -> atomically (modifyTVar retrievals (+ 1)) >> batchRetrieveTxs reader requestedPoint os
            }
    withHarnessServer
      (withLeiosFetchAtomicServer nullTracer (S.defaultServerLimits 2) counted)
      (const (pure ()))
      $ \env -> do
        push env (MsgLeiosBlockTxsRequest (WireOrdinal 0) p ((0, bit 63) :| []))
        err <- failure env
        case fromException err of
          Just (ExnLeiosTxsRequestTooLarge received actual cap) -> (received, actual, cap) @?= (p, 500001, 500000)
          _ -> assertFailure (show err)
  readTVarIO retrievals >>= (@?= 0)

testCanceledLateResult :: Assertion
testCanceledLateResult = do
  entered <- latch
  blocked <- latch
  returned <- latch
  let prepare ::
        forall busy.
        AtomicDataRequest LeiosPoint EndorserBlock LeiosTx busy ->
        IO (PreparedReply LeiosPoint EndorserBlock LeiosTx busy)
      prepare request = do
        atomically $ putTMVar entered ()
        atomically (readTMVar blocked) `catch` \(_ :: SomeException) -> pure ()
        value <- prepareFixture request
        atomically $ putTMVar returned ()
        pure value
  withHarness limits (AtomicRequestHandler prepare) (const (pure ())) $ \env -> do
    push env (bodyRequest 0)
    bounded (atomically (readTMVar entered))
    push env (tryCancel 1 0)
    push env MsgDone
    bodyReply env >>= (@?= Left ())
    ackReply env
    success env
    bounded (atomically (readTMVar returned))

testLongSession :: Assertion
testLongSession = withHarness limits{S.serverDataLimit = 1} (AtomicRequestHandler prepareFixture) (const (pure ())) $ \env -> do
  forM_ [0 .. 1999] $ \n -> do
    push env (txRequest n 0)
    txReply env >>= (@?= Right [head txs])
  push env MsgDone
  success env

testProviderLimits :: Assertion
testProviderLimits = bracket newLeiosDBInMemory closeLeiosDbHandle $ \db -> withReader db $ \reader -> do
  result <-
    try (withLeiosFetchAtomicServer nullTracer limits reader (const (pure ()))) ::
      IO (Either S.ServerError ())
  result @?= Left S.InvalidServerLimits
