{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.Leios.LeiosFetch.AtomicReceive (tests) where

import Cardano.Slotting.Slot (SlotNo (..))
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTVar
  , atomically
  , modifyTVar
  , newTVarIO
  , readTVarIO
  , writeTVar
  )
import Control.Exception (SomeException, try)
import Control.Monad (forM_, void)
import Control.Tracer (nullTracer)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft)
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromJust)
import qualified Data.Vector.Strict as V
import Network.Mux.Timeout (TimeoutFn, withTimeoutSerial)
import Network.TypedProtocol.Codec
import Network.TypedProtocol.Core
import Network.TypedProtocol.Driver (Driver (..))
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Codec
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Descriptor
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Handoff
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Receive
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Type
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity
import Ouroboros.Network.Channel (Channel (..))
import Ouroboros.Network.Driver.Progress
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

type Atomic = LeiosFetchAtomic LeiosPoint EndorserBlock LeiosTx
type Handoff = StorageHandoff IO Int Int

tests :: TestTree
tests =
  testGroup
    "atomic incremental receive"
    [ testCase "definite and indefinite replies decode at every byte split" testSplits
    , testCase "staged framing and list bounds reject empty and excess elements" testStagedBounds
    , testCase "a valid prefix is handed off before the tail and keeps its original scope" testStreaming
    , testCase "coalesced FIFO replies keep scopes, spans and original input times" testCoalesced
    , testCase "wrong hash, size and order preserve only authenticated prefixes" testInvalidTransactions
    , testCase "missing and extra transactions cannot settle or earn extra credit" testCounts
    , testCase
        "truncation discards an incomplete TX and preserves its prefix through disconnect"
        testTruncation
    , testCase "malformed remaining framing preserves authenticated transactions" testMalformedTail
    , testCase "obsolete assignments still authenticate against their original descriptor" testObsolete
    , testCase "normal completion does not ingest or credit a TX twice" testOnce
    , testCase "overlapping replies validate but acquire and credit each TX once" testDuplicates
    , testCase "body annotations retain original bytes and emit no TX progress" testBody
    , testCase "canceled data and its cancel ACK settle distinct FIFO exchanges" testCancelled
    , testCase "scope acquisition and typed settlement reject invalid lifecycle calls" testLifecycle
    ]

right :: Show e => Either e a -> a
right = either (error . show) id

txs :: [LeiosTx]
txs = [MkLeiosTx (BS.replicate n b) | (n, b) <- [(3, 1), (5, 2), (7, 3)]]

body :: EndorserBlock
body = endorserBlockFromTxBytes (leiosTxBytes (head txs) :| map leiosTxBytes (tail txs))

point :: LeiosPoint
point = MkLeiosPoint (SlotNo 5) (endorserBlockHash body)

codec ::
  ProgressEmitter IO LeiosTx -> AnnotatedCodec Atomic CBOR.DeserialiseFailure IO BL.ByteString
codec =
  codecLeiosFetchAtomicProgress
    3
    encodeLeiosPoint
    decodeLeiosPoint
    encodeEndorserBlock
    decodeEndorserBlock
    encodeLeiosTx
    decodeLeiosTx

wire :: [LeiosTx] -> BL.ByteString
wire values = encode (codec (const (pure ()))) (MsgLeiosBlockTxs (V.fromList values))

newHandoff :: Bool -> IO (Handoff, AcquisitionGeneration)
newHandoff durable = do
  h <- right <$> newStorageHandoff (StorageLimits 8192 64 8 8 20)
  gen <- right <$> atomically (beginAcquisition h point)
  whenDurable durable $
    registerDurableBody h (const (pure ())) point gen body IntSet.empty >>= (@?= Right ())
  pure (h, gen)
 where
  whenDurable True act = act
  whenDurable False _ = pure ()

session :: Handoff -> Int -> IO (StorageSession Int)
session h n = right <$> atomically (openStorageSession h n (MkLeiosPeerLifetimeId n 0) 8 8192)

admit ::
  Handoff -> AcquisitionGeneration -> StorageSession Int -> NonEmpty Int -> IO (StorageRequest Int)
admit h gen s offsets = do
  let p = right $ prepareBatch point gen body offsets (EncodedBytes 128)
  req <- right <$> atomically (admitStorageRequest h s p)
  atomically (dispatchStorageRequest h (storageRequestKey req)) >>= (@?= Right ())
  pure req

data Events = Events
  { delivered :: StrictTVar IO [(RequestKey Int, ProgressEvent LeiosTx, Acceptance)]
  , aborted :: StrictTVar IO Int
  }

newEvents :: IO Events
newEvents = Events <$> newTVarIO [] <*> newTVarIO 0

hooks :: Events -> RequestKey Int -> ReplyHooks IO
hooks Events{delivered, aborted} key =
  ReplyHooks
    { replyStarted = pure ()
    , replyInput = const (pure ())
    , replyAwaitInput = pure ()
    , replyTransaction = \event accepted -> atomically $ modifyTVar delivered (++ [(key, event, accepted)])
    , replyAborted = \_ -> atomically $ modifyTVar aborted (+ 1)
    }

newReceiver :: Handoff -> Events -> StorageRequest Int -> IO (ReplyReceiver IO Int)
newReceiver h events req = newReplyReceiver h (DataReply req) (hooks events (storageRequestKey req))

driver ::
  TimeoutFn IO ->
  IO (ReplyReceiver IO Int) ->
  Channel IO BL.ByteString ->
  Driver Atomic AsClient ProgressState IO
driver timeoutFn current =
  progressDriverWithLimits
    nullTracer
    timeoutFn
    codec
    (byteLimitsLeiosFetchAtomic (fromIntegral . BL.length))
    timeLimitsLeiosFetchAtomic
    8192
    (\tok -> current >>= \receiver -> receiveReplyScope receiver tok)
    (\_ _ -> pure ())

channel :: [BL.ByteString] -> (Int -> IO ()) -> IO (Channel IO BL.ByteString)
channel chunks beforeRead = do
  remaining <- newTVarIO (0, chunks)
  pure
    Channel
      { send = const (pure ())
      , recv = do
          (n, rest) <- readTVarIO remaining
          beforeRead n
          case rest of
            [] -> pure Nothing
            x : xs -> atomically (writeTVar remaining (n + 1, xs)) >> pure (Just x)
      }

receiveOne ::
  forall busy.
  StateTokenI (StBusy busy :: Atomic) =>
  SingLeiosFetchAtomic (StBusy busy :: Atomic) ->
  Driver Atomic AsClient ProgressState IO ->
  ReplyReceiver IO Int ->
  ProgressState ->
  IO (ReplyCompletion, ProgressState)
receiveOne _tok d receiver state = do
  (message, next) <- recvMessage d ReflServerAgency state
  completion <- case (message :: SomeMessage (StBusy busy :: Atomic)) of
    SomeMessage msg@MsgLeiosBlock{} -> completeReply receiver msg
    SomeMessage msg@MsgLeiosBlockTxs{} -> completeReply receiver msg
    SomeMessage msg@MsgBlockCancelled -> completeReply receiver msg
    SomeMessage msg@MsgBlockTxsCancelled -> completeReply receiver msg
    SomeMessage msg@MsgAckTryCancel -> completeReply receiver msg
  pure (completion, next)

runTx :: ReplyReceiver IO Int -> [BL.ByteString] -> IO ReplyCompletion
runTx receiver chunks = withTimeoutSerial $ \timeoutFn -> do
  input <- channel chunks (const (pure ()))
  let d = driver timeoutFn (pure receiver) input
  fst <$> receiveTx d receiver (initialDState d)

receiveTx ::
  Driver Atomic AsClient ProgressState IO ->
  ReplyReceiver IO Int ->
  ProgressState ->
  IO (ReplyCompletion, ProgressState)
receiveTx = receiveOne SingBlockTxs

pending :: Handoff -> IO IntSet.IntSet
pending h = snapshotPending . fromJust <$> atomically (readAcquisition h point)

failed :: IO a -> Assertion
failed act = do
  result <- try (void act) :: IO (Either SomeException ())
  assertBool "expected receive failure" (isLeft result)

testSplits :: Assertion
testSplits = forM_ variants $ \bytes -> forM_ (fragmentations bytes) $ \chunks -> do
  spans <- newTVarIO []
  let emit element = atomically $ modifyTVar spans (++ [element])
  step <- decode (codec emit) SingBlockTxs
  result <- runDecoder chunks step
  case result of
    Right (Annotator annotate) -> assertTxVector (annotate bytes)
    Left err -> assertFailure (show err)
  observed <- readTVarIO spans
  length observed @?= length txs
  forM_ (zip txs observed) $ \(expected, WithByteSpan (actual, start, end)) -> do
    actual @?= expected
    BL.take (end - start) (BL.drop start bytes) @?= CBOR.toLazyByteString (encodeLeiosTx expected)
 where
  fragmentations bytes =
    [[BL.take cut bytes, BL.empty, BL.drop cut bytes] | cut <- [0 .. BL.length bytes]]
      ++ [map BL.singleton (BL.unpack bytes)]
  payload = foldMap encodeLeiosTx txs
  variants =
    wire txs
      : [ CBOR.toLazyByteString (outer <> CBOR.encodeWord 4 <> inner <> payload <> endInner <> endOuter)
        | (outer, endOuter) <- [(CBOR.encodeListLen 2, mempty), (CBOR.encodeListLenIndef, CBOR.encodeBreak)]
        , (inner, endInner) <- [(CBOR.encodeListLen 3, mempty), (CBOR.encodeListLenIndef, CBOR.encodeBreak)]
        ]

testStagedBounds :: Assertion
testStagedBounds = forM_ invalid $ \(bytes, expectedPrefix) -> do
  events <- newTVarIO (0 :: Int)
  step <- decode (codec (\_ -> atomically $ modifyTVar events (+ 1))) SingBlockTxs
  result <- runDecoder (map BL.singleton (BL.unpack bytes)) step
  assertBool "malformed staged reply was accepted" (isLeft result)
  readTVarIO events >>= (@?= expectedPrefix)
 where
  invalid =
    [ (BL.pack xs, 0)
    | xs <-
        [ [0x82, 4, 0x80]
        , [0x82, 4, 0x84]
        , [0x82, 4, 0x9f, 0xff]
        , [0x83, 4]
        , [0x82, 7]
        , [0x82, 6, 0]
        ]
    ]
      ++ [
           ( CBOR.toLazyByteString
               ( CBOR.encodeListLen 2
                   <> CBOR.encodeWord 4
                   <> CBOR.encodeListLenIndef
                   <> foldMap encodeLeiosTx (txs ++ [head txs])
                   <> CBOR.encodeBreak
               )
           , 3
           )
         ]

testStreaming :: Assertion
testStreaming = do
  (h, gen) <- newHandoff True
  a <- session h 1 >>= \s -> admit h gen s (0 :| [1])
  b <- session h 2 >>= \s -> admit h gen s (2 :| [])
  events <- newEvents
  replacement <- newReceiver h events b
  current <- newTVarIO replacement
  let originalHooks =
        (hooks events (storageRequestKey a))
          { replyTransaction = \event accepted -> do
              replyTransaction (hooks events (storageRequestKey a)) event accepted
              atomically $ writeTVar current replacement
          }
  original <- newReplyReceiver h (DataReply a) originalHooks
  atomically $ writeTVar current original
  let first =
        CBOR.toLazyByteString
          (CBOR.encodeListLen 2 <> CBOR.encodeWord 4 <> CBOR.encodeListLen 2 <> encodeLeiosTx (head txs))
  input <- channel [first, CBOR.toLazyByteString (encodeLeiosTx (txs !! 1))] $ \n ->
    if n == 1 then pending h >>= (@?= IntSet.singleton 0) else pure ()
  withTimeoutSerial $ \timeoutFn -> do
    let d = driver timeoutFn (readTVarIO current) input
    (result, _) <- receiveTx d original (initialDState d)
    completedOutcome result @?= TransactionsReceived
  observed <- readTVarIO (delivered events)
  map (\(key, _, _) -> key) observed @?= replicate 2 (storageRequestKey a)
  pending h >>= (@?= IntSet.fromList [0, 1])

testCoalesced :: Assertion
testCoalesced = do
  (h, gen) <- newHandoff True
  s <- session h 1
  a <- admit h gen s (0 :| [])
  b <- admit h gen s (1 :| [2])
  events <- newEvents
  ra <- newReceiver h events a
  rb <- newReceiver h events b
  current <- newTVarIO ra
  input <- channel [wire [head txs] <> wire (tail txs)] $ \n -> assertBool "no extra channel read" (n == 0)
  withTimeoutSerial $ \timeoutFn -> do
    let d = driver timeoutFn (readTVarIO current) input
    (first, next) <- receiveTx d ra (initialDState d)
    atomically $ writeTVar current rb
    (second, end) <- receiveTx d rb next
    receiptObservedAt (completedReceipt first) @?= receiptObservedAt (completedReceipt second)
    progressTrailingBytes end @?= Nothing
  observed <- readTVarIO (delivered events)
  map (\(key, _, _) -> key) observed
    @?= [storageRequestKey a, storageRequestKey b, storageRequestKey b]
  map (progressIndex . (\(_, e, _) -> e)) observed @?= [0, 0, 1]
  let times = [progressObservedAt e | (_, e, _) <- observed]
  assertBool "input timestamp preserved" (all (== head times) times)

testInvalidTransactions :: Assertion
testInvalidTransactions = forM_ badTails $ \wrong -> do
  (h, gen) <- newHandoff True
  req <- session h 1 >>= \s -> admit h gen s (0 :| [1])
  events <- newEvents
  r <- newReceiver h events req
  failed $ runTx r [wire [head txs, wrong]]
  pending h >>= (@?= IntSet.singleton 0)
  readTVarIO (aborted events) >>= (@?= 1)
 where
  badTails = [MkLeiosTx (BS.replicate 5 9), MkLeiosTx (BS.replicate 6 2), head txs]

testCounts :: Assertion
testCounts = forM_ [take 1 txs, txs] $ \values -> do
  (h, gen) <- newHandoff True
  req <- session h 1 >>= \s -> admit h gen s (0 :| [1])
  events <- newEvents
  r <- newReceiver h events req
  failed $ runTx r [wire values]
  pending h >>= (@?= IntSet.fromList [0 .. min 1 (length values - 1)])
  readTVarIO (aborted events) >>= (@?= 1)

testTruncation :: Assertion
testTruncation = do
  (h, gen) <- newHandoff True
  s <- session h 1
  req <- admit h gen s (0 :| [1])
  events <- newEvents
  r <- newReceiver h events req
  failed $ runTx r [BL.init (wire (take 2 txs))]
  pending h >>= (@?= IntSet.singleton 0)
  atomically (closeStorageSession h s)
  pending h >>= (@?= IntSet.singleton 0)
  readTVarIO (aborted events) >>= (@?= 1)

testMalformedTail :: Assertion
testMalformedTail = do
  (h, gen) <- newHandoff True
  req <- session h 1 >>= \s -> admit h gen s (0 :| [1])
  events <- newEvents
  r <- newReceiver h events req
  let bad =
        CBOR.toLazyByteString
          ( CBOR.encodeListLenIndef
              <> CBOR.encodeWord 4
              <> CBOR.encodeListLen 2
              <> foldMap encodeLeiosTx (take 2 txs)
              <> CBOR.encodeWord 0
          )
  failed $ runTx r [bad]
  pending h >>= (@?= IntSet.fromList [0, 1])
  readTVarIO (aborted events) >>= (@?= 1)

testObsolete :: Assertion
testObsolete = do
  (h, gen) <- newHandoff True
  req <- session h 1 >>= \s -> admit h gen s (0 :| [1])
  events <- newEvents
  r <- newReceiver h events req
  atomically (retireAcquisition h point gen)
  _ <- right <$> atomically (beginAcquisition h point)
  result <- runTx r [wire (take 2 txs)]
  completedOutcome result @?= TransactionsReceived
  observed <- readTVarIO (delivered events)
  map (\(_, _, a) -> a) observed @?= [Irrelevant, Irrelevant]
  pending h >>= (@?= IntSet.empty)

testOnce :: Assertion
testOnce = do
  (h, gen) <- newHandoff True
  req <- session h 1 >>= \s -> admit h gen s (0 :| [1])
  events <- newEvents
  r <- newReceiver h events req
  _ <- runTx r [wire (take 2 txs)]
  before <- atomically $ readStorageSnapshot h
  failed $ completeReply r (MsgLeiosBlockTxs (V.fromList (take 2 txs)))
  atomically (readStorageSnapshot h) >>= (@?= before)
  observed <- readTVarIO (delivered events)
  map (\(_, _, a) -> a) observed @?= [Useful, Useful]
  readTVarIO (aborted events) >>= (@?= 0)

testDuplicates :: Assertion
testDuplicates = do
  (h, gen) <- newHandoff True
  a <- session h 1 >>= \s -> admit h gen s (0 :| [1])
  b <- session h 2 >>= \s -> admit h gen s (0 :| [1])
  events <- newEvents
  forM_ [a, b] $ \req -> do
    r <- newReceiver h events req
    completed <- runTx r [wire (take 2 txs)]
    completedOutcome completed @?= TransactionsReceived
  history <- readTVarIO (delivered events)
  map (\(_, _, acceptance) -> acceptance) history
    @?= [Useful, Useful, PendingDuplicate, PendingDuplicate]
  pending h >>= (@?= IntSet.fromList [0, 1])

testBody :: Assertion
testBody = do
  let rawBody =
        CBOR.encodeMapLenIndef
          <> CBOR.encodeBytes (BS.replicate 32 7)
          <> CBOR.encodeWord32 100
          <> CBOR.encodeBreak
      raw = CBOR.toLazyByteString (CBOR.encodeListLen 2 <> CBOR.encodeWord 2 <> rawBody)
  events <- newTVarIO (0 :: Int)
  step <- decode (codec (\_ -> atomically $ modifyTVar events (+ 1))) SingBlock
  result <- runDecoder [raw] step
  case result of
    Right (Annotator annotate) -> assertBodyBytes (CBOR.toLazyByteString rawBody) (annotate raw)
    Left err -> assertFailure (show err)
  readTVarIO events >>= (@?= 0)
  (h, gen) <- newHandoff False
  s <- session h 1
  let size = encodeEndorserBlockSize body
      descriptor = BodyDescriptor point gen (size - 1) (MkLeiosEbBounds 4096 8192)
  req <-
    right <$> atomically (admitStorageRequest h s (right (prepareBody descriptor (EncodedBytes 4200))))
  atomically (dispatchStorageRequest h (storageRequestKey req)) >>= (@?= Right ())
  history <- newEvents
  r <- newReceiver h history req
  input <- channel [encode (codec (const (pure ()))) (MsgLeiosBlock body)] (const (pure ()))
  withTimeoutSerial $ \timeoutFn -> do
    let d = driver timeoutFn (pure r) input
    (completed, _) <- receiveOne SingBlock d r (initialDState d)
    completedOutcome completed @?= BodyReceived (BodyAcceptance Useful True)
  readTVarIO (delivered history) >>= (@?= [])

testCancelled :: Assertion
testCancelled = do
  (h, gen) <- newHandoff True
  s <- session h 1
  req <- admit h gen s (0 :| [])
  cancel <- right <$> atomically (cancelStorageRequest h req)
  atomically (dispatchStorageRequest h cancel) >>= (@?= Right ())
  events <- newEvents
  r <- newReceiver h events req
  ack <- newReplyReceiver h (CancelReply cancel) (hooks events cancel)
  input <- channel [BL.pack [0x81, 6, 0x81, 7]] (const (pure ()))
  current <- newTVarIO r
  withTimeoutSerial $ \timeoutFn -> do
    let d = driver timeoutFn (readTVarIO current) input
    (original, next) <- receiveTx d r (initialDState d)
    completedOutcome original @?= DataCancelled
    atomically $ writeTVar current ack
    (reply, _) <- receiveOne SingCancel d ack next
    completedOutcome reply @?= CancelAcknowledged
  readTVarIO (delivered events) >>= (@?= [])
  pending h >>= (@?= IntSet.empty)

testLifecycle :: Assertion
testLifecycle = do
  (h, gen) <- newHandoff True
  req <- session h 1 >>= \s -> admit h gen s (0 :| [])
  events <- newEvents
  r <- newReceiver h events req
  failed $ receiveReplyScope r SingBlock
  scope <- receiveReplyScope r SingBlockTxs
  failed $ receiveReplyScope r SingBlockTxs
  receiveStarted scope
  failed $ receiveStarted scope
  other <- newReceiver h events req
  failed $ completeReply other (MsgLeiosBlockTxs V.empty)
  readTVarIO (aborted events) >>= (@?= 1)

assertTxVector :: SomeMessage (StBusy StBlockTxs :: Atomic) -> Assertion
assertTxVector (SomeMessage (MsgLeiosBlockTxs values)) = V.toList values @?= txs
assertTxVector _ = assertFailure "expected TX vector"

assertBodyBytes :: BL.ByteString -> SomeMessage (StBusy StBlock :: Atomic) -> Assertion
assertBodyBytes expected (SomeMessage (MsgLeiosBlock received)) =
  CBOR.toLazyByteString (encodeEndorserBlock received) @?= expected
assertBodyBytes _ _ = assertFailure "expected body"
