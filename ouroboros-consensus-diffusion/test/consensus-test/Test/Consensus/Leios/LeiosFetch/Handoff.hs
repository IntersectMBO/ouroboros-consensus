{-# LANGUAGE NamedFieldPuns #-}

module Test.Consensus.Leios.LeiosFetch.Handoff (tests) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.Class.MonadSTM.Strict
  ( STM
  , StrictTVar
  , atomically
  , check
  , modifyTVar
  , newEmptyTMVarIO
  , newTVarIO
  , putTMVar
  , readTMVar
  , readTVar
  , readTVarIO
  , tryPutTMVar
  )
import Control.Exception (bracket, throwIO)
import Control.Monad (forM_, void)
import Control.Monad.Class.MonadAsync (concurrently, waitCatch, withAsync)
import qualified Control.Monad.Class.MonadTimer.SI as SI
import qualified Data.ByteString as BS
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import Ouroboros.Consensus.Leios.Types
  ( EndorserBlock
  , LeiosEbBounds (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , encodeEndorserBlockSize
  , endorserBlockFromTxBytes
  , endorserBlockHash
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Admission as A
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Descriptor
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Handoff
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity
import Ouroboros.Consensus.Storage.LeiosDB
  ( LeiosDbHandle (..)
  , LeiosDbWriter (..)
  , Promise (..)
  , TxWriteReceipt (..)
  , newLeiosDBInMemory
  , withWriter
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "atomic storage handoff"
    [ testCase "a prefix and legacy vector share one useful claim per TX" testDuplicates
    , testCase "simultaneous valid copies obtain exactly one claim and credit award" testConcurrentClaim
    , testCase
        "a durable legacy copy releases a queued claim without another write or a lost wakeup"
        testLegacyDurableRace
    , testCase
        "validation rejects skipped, oversized and invalid elements without losing a valid prefix"
        testValidation
    , testCase "storage submission backpressure never blocks receive handoff" testBackpressure
    , testCase
        "whole bodies reserve the trusted bound, retain size faults and precede TX writes"
        testBodyDependency
    , testCase "same-owner reconnects share pending byte and entry budgets" testOwnerBudgets
    , testCase
        "node session cap includes draining registrations and stale handles cannot alias"
        testSessionCap
    , testCase "pruning drops queued data but old descriptors still validate and drain" testQueuedPrune
    , testCase "a receipt after prune/refocus cannot complete the new generation" testOldReceipt
    , testCase "unconfirmed writes release claims and leave relevant TXs fetchable" testUnconfirmed
    , testCase
        "coalescing uses ready whole TXs, crosses target once and submits a small remainder immediately"
        testCoalescing
    , testCase "writer failure is fatal, never a positive receipt" testWriterFailure
    , testCase "malformed receipts cannot settle another offset or release a claim" testInvalidReceipt
    , testCase "durable-body bridge and closure notifications are idempotent" testDurableBridge
    , testCase "shutdown drains accepted data and closes admission" testShutdown
    , testCase "cancellation cannot refund an accepted prefix before its receipt" testCancelOwnership
    , testCase "session and acquisition limits reject invalid or excess allocations" testLimits
    ]

type Handoff = StorageHandoff IO Int Int

data Fixture = Fixture
  { handoff :: !Handoff
  , writer :: !(LeiosDbWriter IO)
  , generation :: !AcquisitionGeneration
  , point :: !LeiosPoint
  , body :: !EndorserBlock
  , txs :: ![LeiosTx]
  , events :: !(StrictTVar IO [StorageEvent])
  }

right :: Show e => Either e a -> a
right = either (error . show) id

limits :: StorageLimits
limits = StorageLimits 4096 32 8 8 10

withFixture :: StorageLimits -> [Int] -> (Fixture -> IO a) -> IO a
withFixture config sizes action = withUnseeded config sizes $ \env -> do
  _ <- await =<< writeEbBody (writer env) (point env) (body env)
  registerDurableBody (handoff env) (notify env) (point env) (generation env) (body env) IntSet.empty
    >>= (@?= Right ())
  action env

withUnseeded :: StorageLimits -> [Int] -> (Fixture -> IO a) -> IO a
withUnseeded config sizes action = bracket newLeiosDBInMemory closeLeiosDbHandle $ \db -> withWriter db $ \writer -> do
  let txs = zipWith (\i size -> MkLeiosTx (BS.replicate size i)) [1 ..] sizes
      body = endorserBlockFromTxBytes (neBytes txs)
      point = MkLeiosPoint (SlotNo 5) (endorserBlockHash body)
  handoff <- right <$> newStorageHandoff config
  generation <- right <$> atomically (beginAcquisition handoff point)
  events <- newTVarIO []
  action Fixture{handoff, writer, generation, point, body, txs, events}
 where
  neBytes (tx : rest) = leiosTxBytes tx :| map leiosTxBytes rest
  neBytes [] = error "empty fixture"

notify :: Fixture -> StorageEvent -> STM IO ()
notify env event = modifyTVar (events env) (event :)

newSession :: Fixture -> Int -> Int -> IO (StorageSession Int)
newSession env owner peer =
  right <$> atomically (openStorageSession (handoff env) owner (MkLeiosPeerLifetimeId peer 0) 8 8192)

prepared :: Fixture -> [Int] -> PreparedRequest
prepared env offsets =
  right $
    prepareBatch
      (point env)
      (generation env)
      (body env)
      (fromMaybe (error "empty test request") (nonEmpty offsets))
      (EncodedBytes (64 + sum [fromIntegral (BS.length (leiosTxBytes (txs env !! off))) | off <- offsets]))
 where
  nonEmpty [] = Nothing
  nonEmpty (x : xs) = Just (x :| xs)

admit :: Fixture -> StorageSession Int -> [Int] -> IO (StorageRequest Int)
admit env session offsets = do
  req <- right <$> atomically (admitStorageRequest (handoff env) session (prepared env offsets))
  atomically (dispatchStorageRequest (handoff env) (storageRequestKey req)) >>= (@?= Right ())
  pure req

accept :: Fixture -> StorageRequest Int -> Word32 -> Int -> IO (Either StorageError Acceptance)
accept env req position off = acceptTransaction (handoff env) req position (txs env !! off)

snapshot :: Fixture -> IO AcquisitionSnapshot
snapshot env =
  fromMaybe (error "missing acquisition") <$> atomically (readAcquisition (handoff env) (point env))

awaitSTM :: STM IO a -> IO a
awaitSTM action =
  SI.timeout 5 (atomically action)
    >>= maybe (assertFailure "timed out waiting for storage transition") pure

awaitHeld :: Fixture -> IntSet.IntSet -> IO ()
awaitHeld env expected = awaitSTM $ do
  a <- readAcquisition (handoff env) (point env)
  check (maybe False ((== expected) . snapshotHeld) a)

withWorker :: Fixture -> LeiosDbWriter IO -> IO a -> IO a
withWorker env storage action = withStorageHandoff (handoff env) storage (notify env) (const action)

testDuplicates :: Assertion
testDuplicates = withFixture limits [6, 6] $ \env -> do
  a <- newSession env 1 1
  b <- newSession env 2 2
  ra <- admit env a [0, 1]
  rb <- admit env b [0, 1]
  accept env ra 0 0 >>= (@?= Right Useful)
  acceptLegacyBatch (handoff env) rb [head (txs env), head (txs env)]
    >>= (@?= Left InvalidTransaction)
  snapshotPending <$> snapshot env >>= (@?= IntSet.singleton 0)
  acceptLegacyBatch (handoff env) rb (txs env) >>= (@?= Right [PendingDuplicate, Useful])
  accept env ra 1 1 >>= (@?= Right PendingDuplicate)
  pending <- snapshot env
  snapshotPending pending @?= IntSet.fromList [0, 1]
  snapshotHeld pending @?= IntSet.empty
  calls <- newTVarIO (0 :: Int)
  let storage =
        (writer env)
          { writeTxs = \p txs -> atomically (modifyTVar calls (+ 1)) >> writeTxs (writer env) p txs
          }
  withWorker env storage $ awaitHeld env (IntSet.fromList [0, 1])
  readTVarIO calls >>= (@?= 1)

testConcurrentClaim :: Assertion
testConcurrentClaim = withFixture limits [6] $ \env -> do
  a <- newSession env 1 1
  b <- newSession env 2 2
  ra <- admit env a [0]
  rb <- admit env b [0]
  pair <- concurrently (accept env ra 0 0) (accept env rb 0 0)
  assertBool
    "one useful copy, one pending duplicate"
    (pair == (Right Useful, Right PendingDuplicate) || pair == (Right PendingDuplicate, Right Useful))
  owned <- atomically $ readStorageSnapshot (handoff env)
  snapshotClaims owned @?= 1
  withWorker env (writer env) $ awaitHeld env (IntSet.singleton 0)

testLegacyDurableRace :: Assertion
testLegacyDurableRace = withFixture limits [6] $ \env -> do
  a <- newSession env 1 1
  ra <- admit env a [0]
  accept env ra 0 0 >>= (@?= Right Useful)
  _ <- await =<< writeTxs (writer env) (point env) [(0, leiosTxBytes (head (txs env)))]
  registerDurableBody
    (handoff env)
    (notify env)
    (point env)
    (generation env)
    (body env)
    (IntSet.singleton 0)
    >>= (@?= Right ())
  calls <- newTVarIO (0 :: Int)
  let storage =
        (writer env)
          { writeTxs = \p txs -> atomically (modifyTVar calls (+ 1)) >> writeTxs (writer env) p txs
          }
  withWorker env storage $ do
    _ <- awaitSTM $ readStorageSnapshot (handoff env) >>= \owned -> check (snapshotClaims owned == 0)
    notes <- readTVarIO (events env)
    assertBool
      "capacity wakeup survives worker returning to STM retry"
      (StorageCapacityAvailable `elem` notes)
  readTVarIO calls >>= (@?= 0)

testValidation :: Assertion
testValidation = withFixture limits [6, 6] $ \env -> do
  a <- newSession env 1 1
  ra <- admit env a [0, 1]
  initial <- atomically $ readStorageSnapshot (handoff env)
  accept env ra 1 1 >>= (@?= Left InvalidPosition)
  acceptTransaction (handoff env) ra 0 (MkLeiosTx (BS.replicate 7 1))
    >>= (@?= Left InvalidTransaction)
  atomically (readStorageSnapshot (handoff env)) >>= (@?= initial)
  accept env ra 0 0 >>= (@?= Right Useful)
  accept env ra 0 0 >>= (@?= Left InvalidPosition)
  acceptTransaction (handoff env) ra 1 (txs env !! 0) >>= (@?= Left InvalidTransaction)
  atomically $ closeStorageSession (handoff env) a
  owned <- atomically $ readStorageSnapshot (handoff env)
  snapshotReservedBytes owned @?= 6
  snapshotClaims owned @?= 1
  withWorker env (writer env) $ awaitHeld env (IntSet.singleton 0)

testBackpressure :: Assertion
testBackpressure = withFixture limits [6, 6, 6] $ \env -> do
  a <- newSession env 1 1
  ra <- admit env a [0, 1, 2]
  accept env ra 0 0 >>= (@?= Right Useful)
  entered <- newEmptyTMVarIO
  gate <- newEmptyTMVarIO
  let storage =
        (writer env)
          { writeTxs = \p txs -> do
              atomically $ void $ tryPutTMVar entered ()
              atomically $ readTMVar gate
              writeTxs (writer env) p txs
          }
  withWorker env storage $ do
    _ <- awaitSTM (readTMVar entered)
    SI.timeout 0.1 (accept env ra 1 1) >>= (@?= Just (Right Useful))
    SI.timeout 0.1 (accept env ra 2 2) >>= (@?= Just (Right Useful))
    atomically $ closeStorageSession (handoff env) a
    owned <- atomically $ readStorageSnapshot (handoff env)
    snapshotClaims owned @?= 3
    snapshotReservedBytes owned @?= 18
    atomically $ putTMVar gate ()
    awaitHeld env (IntSet.fromList [0, 1, 2])

testBodyDependency :: Assertion
testBodyDependency = withUnseeded limits [6, 6] $ \env -> do
  a <- newSession env 1 1
  b <- newSession env 2 2
  let ceilingBytes = encodeEndorserBlockSize (body env) + 100
      descriptor = BodyDescriptor (point env) (generation env) 1 (MkLeiosEbBounds ceilingBytes 4096)
      prep = right $ prepareBody descriptor (EncodedBytes (fromIntegral ceilingBytes + 16))
  ra <- right <$> atomically (admitStorageRequest (handoff env) a prep)
  atomically (dispatchStorageRequest (handoff env) (storageRequestKey ra)) >>= (@?= Right ())
  atomically (admitStorageRequest (handoff env) b (prepared env [0])) >>= (@?= Left BodyUnknown)
  acceptBody (handoff env) ra (body env) >>= (@?= Right (BodyAcceptance Useful True))
  atomically $ closeStorageSession (handoff env) a
  rb <- right <$> atomically (admitStorageRequest (handoff env) b prep)
  atomically (dispatchStorageRequest (handoff env) (storageRequestKey rb)) >>= (@?= Right ())
  acceptBody (handoff env) rb (body env) >>= (@?= Right (BodyAcceptance PendingDuplicate True))
  atomically (finishStorageRequest (handoff env) (storageRequestKey rb) A.NormalTerminal)
    >>= (@?= Right ())
  rtx <- admit env b [0, 1]
  accept env rtx 0 0 >>= (@?= Right Useful)
  accept env rtx 1 1 >>= (@?= Right Useful)
  charge <- atomically $ readStorageSnapshot (handoff env)
  snapshotReservedBytes charge @?= fromIntegral (encodeEndorserBlockSize (body env)) + 12
  gate <- newEmptyTMVarIO
  entered <- newEmptyTMVarIO
  calls <- newTVarIO ([] :: [String])
  let storage =
        (writer env)
          { writeEbBody = \p body -> do
              atomically $ modifyTVar calls ("body" :)
              promise <- writeEbBody (writer env) p body
              pure $ Promise $ do
                atomically $ putTMVar entered ()
                atomically $ readTMVar gate
                await promise
          , writeTxs = \p txs -> atomically (modifyTVar calls ("tx" :)) >> writeTxs (writer env) p txs
          }
  withWorker env storage $ do
    _ <- awaitSTM (readTMVar entered)
    readTVarIO calls >>= (@?= ["body"])
    atomically $ putTMVar gate ()
    awaitHeld env (IntSet.fromList [0, 1])
  reverse <$> readTVarIO calls >>= (@?= ["body", "tx"])

testOwnerBudgets :: Assertion
testOwnerBudgets = do
  withFixture limits{storageBytesPerOwner = 10} [6] $ \env -> do
    a <- newSession env 1 1
    ra <- admit env a [0]
    accept env ra 0 0 >>= (@?= Right Useful)
    atomically $ closeStorageSession (handoff env) a
    b <- newSession env 1 2
    atomically (admitStorageRequest (handoff env) b (prepared env [0])) >>= (@?= Left OwnerByteCapacity)
    withWorker env (writer env) $ awaitHeld env (IntSet.singleton 0)
  withFixture limits{storageEntriesPerOwner = 1} [6] $ \env -> do
    a <- newSession env 1 1
    ra <- admit env a [0]
    accept env ra 0 0 >>= (@?= Right Useful)
    atomically $ closeStorageSession (handoff env) a
    b <- newSession env 1 2
    atomically (admitStorageRequest (handoff env) b (prepared env [0]))
      >>= (@?= Left OwnerEntryCapacity)
    withWorker env (writer env) $ awaitHeld env (IntSet.singleton 0)

testSessionCap :: Assertion
testSessionCap = withFixture limits{storageMaxSessions = 2} [6] $ \env -> do
  a <- newSession env 1 1
  ra <- admit env a [0]
  accept env ra 0 0 >>= (@?= Right Useful)
  atomically $ closeStorageSession (handoff env) a
  b <- newSession env 2 2
  atomically (openStorageSession (handoff env) 3 (MkLeiosPeerLifetimeId 3 0) 8 8192)
    >>= (@?= Left SessionCapacity)
  withWorker env (writer env) $ do
    awaitHeld env (IntSet.singleton 0)
    c <- newSession env 1 1
    assertBool "fresh token" (c /= a)
    accept env ra 0 0 >>= (@?= Left UnknownStorageSession)
    atomically $ closeStorageSession (handoff env) b >> closeStorageSession (handoff env) c

testQueuedPrune :: Assertion
testQueuedPrune = withFixture limits [6, 6] $ \env -> do
  a <- newSession env 1 1
  ra <- admit env a [0, 1]
  accept env ra 0 0 >>= (@?= Right Useful)
  atomically $ retireAcquisition (handoff env) (point env) (generation env)
  owned <- atomically $ readStorageSnapshot (handoff env)
  snapshotClaims owned @?= 0
  snapshotReservedBytes owned @?= 6
  accept env ra 1 1 >>= (@?= Right Irrelevant)
  atomically (finishStorageRequest (handoff env) (storageRequestKey ra) A.NormalTerminal)
    >>= (@?= Right ())
  atomically (readAcquisition (handoff env) (point env)) >>= (@?= Nothing)
  withWorker env (writer env) (pure ())

testOldReceipt :: Assertion
testOldReceipt = withFixture limits [6] $ \env -> do
  a <- newSession env 1 1
  ra <- admit env a [0]
  accept env ra 0 0 >>= (@?= Right Useful)
  calls <- newTVarIO (0 :: Int)
  gate1 <- newEmptyTMVarIO
  gate2 <- newEmptyTMVarIO
  let storage =
        (writer env)
          { writeTxs = \p txs -> do
              written <- writeTxs (writer env) p txs
              i <- atomically $ do
                modifyTVar calls (+ 1)
                readTVar calls
              pure $ Promise $ atomically (readTMVar (if i == 1 then gate1 else gate2)) >> await written
          }
  withWorker env storage $ do
    _ <- awaitSTM (readTVar calls >>= \n -> check (n == 1))
    atomically $ retireAcquisition (handoff env) (point env) (generation env)
    gen <- right <$> atomically (beginAcquisition (handoff env) (point env))
    assertBool "new acquisition generation" (gen /= generation env)
    let newer = env{generation = gen}
    registerDurableBody (handoff env) (notify env) (point env) gen (body env) IntSet.empty
      >>= (@?= Right ())
    b <- newSession newer 2 2
    rb <- admit newer b [0]
    accept newer rb 0 0 >>= (@?= Right Useful)
    atomically $ putTMVar gate1 ()
    _ <- awaitSTM (readTVar calls >>= \n -> check (n == 2))
    notes <- readTVarIO (events env)
    assertBool
      "an obsolete receipt still wakes capacity waiters"
      (StorageCapacityAvailable `elem` notes)
    current <- snapshot newer
    snapshotHeld current @?= IntSet.empty
    snapshotPending current @?= IntSet.singleton 0
    atomically $ putTMVar gate2 ()
    awaitHeld newer (IntSet.singleton 0)

testUnconfirmed :: Assertion
testUnconfirmed = withFixture limits [6] $ \env -> do
  a <- newSession env 1 1
  ra <- admit env a [0]
  accept env ra 0 0 >>= (@?= Right Useful)
  calls <- newTVarIO (0 :: Int)
  let storage =
        (writer env)
          { writeTxs = \p txs -> do
              i <- atomically $ modifyTVar calls (+ 1) >> readTVar calls
              if i == 1
                then pure $ Promise $ pure (TxWriteReceipt IntSet.empty (IntSet.singleton 0) [])
                else writeTxs (writer env) p txs
          }
  withWorker env storage $ do
    _ <- awaitSTM $ do
      notes <- readTVar (events env)
      check (TransactionsUnconfirmed (point env) (generation env) (IntSet.singleton 0) `elem` notes)
    failed <- snapshot env
    snapshotPending failed @?= IntSet.empty
    snapshotMissing failed @?= IntSet.singleton 0
    snapshotHeld failed @?= IntSet.empty
    b <- newSession env 2 2
    rb <- admit env b [0]
    accept env rb 0 0 >>= (@?= Right Reclaimed)
    awaitHeld env (IntSet.singleton 0)

testCoalescing :: Assertion
testCoalescing = withFixture limits [6, 6, 6] $ \env -> do
  a <- newSession env 1 1
  ra <- admit env a [0, 1, 2]
  forM_ [0, 1, 2] $ \off -> accept env ra (fromIntegral off) off >>= (@?= Right Useful)
  batches <- newTVarIO ([] :: [[Int]])
  let storage =
        (writer env)
          { writeTxs = \p txs -> atomically (modifyTVar batches (map fst txs :)) >> writeTxs (writer env) p txs
          }
  withWorker env storage $ awaitHeld env (IntSet.fromList [0, 1, 2])
  reverse <$> readTVarIO batches >>= (@?= [[0, 1], [2]])

testWriterFailure :: Assertion
testWriterFailure = withFixture limits [6] $ \env -> do
  a <- newSession env 1 1
  ra <- admit env a [0]
  accept env ra 0 0 >>= (@?= Right Useful)
  let storage = (writer env){writeTxs = \_ _ -> throwIO (userError "injected fatal writer failure")}
  withAsync (runStorageHandoff (handoff env) storage (notify env)) $ \worker -> do
    result <- waitCatch worker
    case result of
      Left _ -> pure ()
      Right () -> assertFailure "writer error became success"
  state <- snapshot env
  snapshotHeld state @?= IntSet.empty
  snapshotPending state @?= IntSet.singleton 0

testInvalidReceipt :: Assertion
testInvalidReceipt = withFixture limits [6] $ \env -> do
  a <- newSession env 1 1
  ra <- admit env a [0]
  accept env ra 0 0 >>= (@?= Right Useful)
  let storage =
        (writer env)
          { writeTxs = \_ _ -> pure $ Promise $ pure (TxWriteReceipt (IntSet.singleton 99) IntSet.empty [])
          }
  withAsync (runStorageHandoff (handoff env) storage (notify env)) $ \worker -> do
    result <- waitCatch worker
    case result of
      Left _ -> pure ()
      Right () -> assertFailure "invalid receipt was accepted"
  owned <- atomically $ readStorageSnapshot (handoff env)
  snapshotClaims owned @?= 1
  snapshotHeld <$> snapshot env >>= (@?= IntSet.empty)

testDurableBridge :: Assertion
testDurableBridge = withFixture limits [6] $ \env -> do
  let register =
        registerDurableBody
          (handoff env)
          (notify env)
          (point env)
          (generation env)
          (body env)
          (IntSet.singleton 0)
  register >>= (@?= Right ())
  register >>= (@?= Right ())
  notes <- readTVarIO (events env)
  length [() | BodyDurable{} <- notes] @?= 1
  length [() | ClosureDurable{} <- notes] @?= 1
  a <- newSession env 1 1
  ra <- admit env a [0]
  accept env ra 0 0 >>= (@?= Right DurableDuplicate)
  withWorker env (writer env) (pure ())

testShutdown :: Assertion
testShutdown = withFixture limits [6] $ \env -> do
  a <- newSession env 1 1
  ra <- admit env a [0]
  accept env ra 0 0 >>= (@?= Right Useful)
  withWorker env (writer env) $ atomically $ closeStorageHandoff (handoff env)
  awaitHeld env (IntSet.singleton 0)
  closed <- atomically $ readStorageSnapshot (handoff env)
  snapshotSessions closed @?= 0
  snapshotReservedBytes closed @?= 0
  atomically (beginAcquisition (handoff env) (point env)) >>= (@?= Left StorageClosed)

testCancelOwnership :: Assertion
testCancelOwnership = withFixture limits [6, 6] $ \env -> do
  a <- newSession env 1 1
  ra <- admit env a [0, 1]
  accept env ra 0 0 >>= (@?= Right Useful)
  cancel <- right <$> atomically (cancelStorageRequest (handoff env) ra)
  atomically (dispatchStorageRequest (handoff env) cancel) >>= (@?= Right ())
  before <- atomically $ readStorageSnapshot (handoff env)
  snapshotReservedBytes before @?= 12
  atomically (finishStorageRequest (handoff env) (storageRequestKey ra) A.CancelledTerminal)
    >>= (@?= Left (AdmissionFailure A.InvalidTerminal))
  atomically $ closeStorageSession (handoff env) a
  after <- atomically $ readStorageSnapshot (handoff env)
  snapshotReservedBytes after @?= 6
  snapshotDrainingSessions after @?= 1
  withWorker env (writer env) $ awaitHeld env (IntSet.singleton 0)

testLimits :: Assertion
testLimits = withFixture limits{storageMaxAcquisitions = 1} [6] $ \env -> do
  invalid <- newStorageHandoff limits{storageCheckpointBytes = 0} :: IO (Either StorageError Handoff)
  case invalid of
    Left err -> err @?= InvalidStorageLimits
    Right _ -> assertFailure "zero checkpoint accepted"
  atomically (openStorageSession (handoff env) 1 (MkLeiosPeerLifetimeId 1 0) 0 8192)
    >>= (@?= Left (AdmissionFailure A.InvalidLimits))
  let other = (point env){pointSlotNo = SlotNo 6}
  atomically (beginAcquisition (handoff env) other) >>= (@?= Left AcquisitionCapacity)
  atomically $ retireAcquisition (handoff env) (point env) (generation env)
  gen <- right <$> atomically (beginAcquisition (handoff env) other)
  assertBool "retirement does not recycle generation tokens" (gen /= generation env)
  withWorker env (writer env) (pure ())
