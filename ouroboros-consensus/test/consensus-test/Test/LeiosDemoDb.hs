{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Tests for the LeiosDemoDb interface.
--
-- These tests verify the semantics of the LeiosDbHandle operations for both
-- InMemory and SQLite implementations.
module Test.LeiosDemoDb (tests) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.Class.MonadSTM.Strict (atomically, readTChan, tryReadTChan)
import Control.Monad (forM_, replicateM)
import qualified Data.ByteString as BS
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Vector as V
import LeiosDemoDb
  ( LeiosDbHandle (..)
  , newInMemoryLeiosDb
  , newLeiosDbConnectionIO
  )
import LeiosDemoTypes
  ( EbHash (..)
  , LeiosEb (..)
  , LeiosNotification (..)
  , LeiosPoint (..)
  , TxHash (..)
  , leiosEbBytesSize
  )
import System.Directory (removeDirectoryRecursive)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Test.QuickCheck
  ( Property
  , chooseInt
  , counterexample
  , forAllShrinkShow
  , ioProperty
  , shrink
  , (===)
  )
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "LeiosDemoDb" $
    forEachImplementation mkTestGroups

-- | Run tests for each database implementation (InMemory and SQLite).
-- The database is created once per implementation and shared across all tests.
forEachImplementation :: (IO (LeiosDbHandle IO) -> [TestTree]) -> [TestTree]
forEachImplementation mkTests =
  [ withResource newInMemoryLeiosDb (const $ pure ()) $ \getDb ->
      testGroup "InMemory" (mkTests getDb)
  , withResource mkTestSqliteDb cleanupSqliteDb $ \getDbAndPath ->
      testGroup "SQLite" (mkTests (fst <$> getDbAndPath))
  ]
 where
  cleanupSqliteDb (_, tmpDir) = removeDirectoryRecursive tmpDir

-- | Create the test groups for a given database handle.
-- Each group uses a different counter offset to avoid slot collisions.
mkTestGroups :: IO (LeiosDbHandle IO) -> [TestTree]
mkTestGroups getDb =
  [ withResource (getDb >>= newDbStateAt 0) (const $ pure ()) $ \getState ->
      testGroup
        "points"
        [ testProperty "insert then scan" $ prop_pointsInsertThenScan getState
        , testProperty "multiple inserts accumulate" $ prop_pointsAccumulate getState
        ]
  , withResource (getDb >>= newDbStateAt 100_000) (const $ pure ()) $ \getState ->
      testGroup
        "ebs"
        [ testProperty "insert then lookup" $ prop_ebsInsertThenLookup getState
        , testProperty "lookup missing returns empty" $ prop_ebsLookupMissing getState
        ]
  , withResource (getDb >>= newDbStateAt 200_000) (const $ pure ()) $ \getState ->
      testGroup
        "transactions"
        [ testProperty "update then retrieve" $ prop_txsUpdateThenRetrieve getState
        , testProperty "retrieve missing returns empty" $ prop_txsRetrieveMissing getState
        ]
  , testGroup
      "notifications"
      [ testCase "single subscriber" $ getDb >>= test_singleSubscriber
      , testCase "multiple subscribers" $ getDb >>= test_multipleSubscribers
      , testCase "correct data" $ getDb >>= test_correctData
      , testCase "late subscriber" $ getDb >>= test_lateSubscriber
      , testCase "multiple notifications" $ getDb >>= test_multipleNotifications
      , testCase "no offerBlockTxs before last update" $ getDb >>= test_noOfferBlockTxsBeforeComplete
      , testCase "offerBlockTxs on last update" $ getDb >>= test_offerBlockTxs
      ]
  ]

-- * Database state for property tests

-- | State wrapper that tracks a unique counter for generating non-colliding test data.
data DbState = DbState
  { dbHandle :: LeiosDbHandle IO
  , dbCounter :: IORef Int
  }

-- | Create a DbState with counter starting at a specific offset.
-- Used to avoid slot collisions between different test groups.
newDbStateAt :: Int -> LeiosDbHandle IO -> IO DbState
newDbStateAt start h = DbState h <$> newIORef start

-- | Get a unique integer for this test run to avoid collisions with previous runs.
nextUnique :: DbState -> IO Int
nextUnique st = atomicModifyIORef' (dbCounter st) $ \n -> (n + 1, n)

-- * Test fixtures / generators

-- | Generate a unique EbHash based on a seed.
genEbHash :: Int -> EbHash
genEbHash seed = MkEbHash $ BS.pack $ take 32 $ cycle [fromIntegral seed, fromIntegral (seed `div` 256)]

-- | Generate a unique TxHash based on seeds.
genTxHash :: Int -> Int -> TxHash
genTxHash ebSeed txSeed = MkTxHash $ BS.pack $ take 32 $ cycle [fromIntegral ebSeed, fromIntegral txSeed]

-- | Generate a LeiosPoint from a unique seed.
genPoint :: Int -> LeiosPoint
genPoint seed = MkLeiosPoint (SlotNo $ fromIntegral seed) (genEbHash seed)

-- | Generate a LeiosEb with the given number of transactions.
genEb :: Int -> Int -> LeiosEb
genEb seed numTxs =
  MkLeiosEb $
    V.fromList
      [ (genTxHash seed i, 100 + fromIntegral i)
      | i <- [0 .. numTxs - 1]
      ]

-- | Generate transaction bytes for testing.
genTxBytes :: Int -> BS.ByteString
genTxBytes seed = BS.pack $ replicate 100 (fromIntegral seed)

-- | Create a simple test EbHash from a seed byte.
mkTestEbHash :: Word -> EbHash
mkTestEbHash seed = MkEbHash $ BS.pack $ replicate 32 (fromIntegral seed)

-- | Create a simple test TxHash from a seed byte.
mkTestTxHash :: Word -> TxHash
mkTestTxHash seed = MkTxHash $ BS.pack $ replicate 32 (fromIntegral seed)

-- | Create a test LeiosPoint.
mkTestPoint :: SlotNo -> Word -> LeiosPoint
mkTestPoint slot seed = MkLeiosPoint slot (mkTestEbHash seed)

-- | Create a test LeiosEb with the given number of transactions.
mkTestEb :: Int -> LeiosEb
mkTestEb numTxs =
  MkLeiosEb $
    V.fromList
      [ (mkTestTxHash (fromIntegral i), 100 + fromIntegral i)
      | i <- [0 .. numTxs - 1]
      ]

-- * Property tests for points

-- | Property: inserting a point and then scanning should return it.
prop_pointsInsertThenScan :: IO DbState -> Property
prop_pointsInsertThenScan getState =
  ioProperty $ do
    st <- getState
    unique <- nextUnique st
    let db = dbHandle st
        point = genPoint unique
    leiosDbInsertEbPoint db point
    points <- leiosDbScanEbPoints db
    pure $ (point.pointSlotNo, point.pointEbHash) `elem` points

-- | Property: multiple inserted points all appear in scan results.
prop_pointsAccumulate :: IO DbState -> Property
prop_pointsAccumulate getState =
  forAllShrinkShow (chooseInt (1, 10)) shrink show $ \count ->
    ioProperty $ do
      st <- getState
      unique <- nextUnique st
      let db = dbHandle st
          points = [genPoint (unique * 1000 + i) | i <- [0 .. count - 1]]
      forM_ points $ leiosDbInsertEbPoint db
      scanned <- leiosDbScanEbPoints db
      let expected = [(p.pointSlotNo, p.pointEbHash) | p <- points]
      pure $ all (`elem` scanned) expected

-- * Property tests for EBs

-- | Property: inserting an EB body and looking it up returns the correct txs.
prop_ebsInsertThenLookup :: IO DbState -> Property
prop_ebsInsertThenLookup getState =
  forAllShrinkShow (chooseInt (1, 50)) shrink show $ \numTxs ->
    ioProperty $ do
      st <- getState
      unique <- nextUnique st
      let db = dbHandle st
          point = genPoint unique
          eb = genEb unique numTxs
          expectedTxs = V.toList (leiosEbTxs eb)
      leiosDbInsertEbBody db point eb
      result <- leiosDbLookupEbBody db point.pointEbHash
      pure $
        counterexample ("Expected: " ++ show expectedTxs ++ "\nGot: " ++ show result) $
          result == expectedTxs

-- | Property: looking up a non-existent EB returns empty list.
prop_ebsLookupMissing :: IO DbState -> Property
prop_ebsLookupMissing getState =
  ioProperty $ do
    st <- getState
    unique <- nextUnique st
    let db = dbHandle st
        missingHash = genEbHash (unique + 1_000_000) -- Use a hash we never inserted
    result <- leiosDbLookupEbBody db missingHash
    pure $ result === []

-- * Property tests for transactions

-- | Property: updating tx bytes and retrieving them returns the correct data.
prop_txsUpdateThenRetrieve :: IO DbState -> Property
prop_txsUpdateThenRetrieve getState =
  forAllShrinkShow (chooseInt (1, 20)) shrink show $ \numTxs ->
    ioProperty $ do
      st <- getState
      unique <- nextUnique st
      let db = dbHandle st
          point = genPoint unique
          eb = genEb unique numTxs
      -- Insert the EB body first
      leiosDbInsertEbBody db point eb
      -- Update some transactions with bytes
      let offsetsToUpdate = filter (< numTxs) [0, numTxs `div` 2, numTxs - 1]
          txBytesMap = [(off, genTxBytes (unique * 100 + off)) | off <- offsetsToUpdate]
      forM_ txBytesMap $ \(off, bytes) ->
        leiosDbUpdateEbTx db point.pointEbHash off bytes
      -- Retrieve all offsets
      let allOffsets = [0 .. numTxs - 1]
      results <- leiosDbBatchRetrieveTxs db point.pointEbHash allOffsets
      -- Check that updated offsets have the correct bytes
      let checkResult (off, _txHash, mBytes) =
            case lookup off txBytesMap of
              Just expectedBytes -> mBytes == Just expectedBytes
              Nothing -> mBytes == Nothing -- Not updated yet
      pure $
        counterexample ("Results: " ++ show results) $
          all checkResult results && length results == numTxs

-- | Property: retrieving from non-existent EB returns empty list.
prop_txsRetrieveMissing :: IO DbState -> Property
prop_txsRetrieveMissing getState =
  ioProperty $ do
    st <- getState
    unique <- nextUnique st
    let db = dbHandle st
        missingHash = genEbHash (unique + 2_000_000)
    result <- leiosDbBatchRetrieveTxs db missingHash [0, 1, 2]
    pure $ result === []

-- * Notification tests

-- | Test that a single subscriber receives a notification when insertEbBody is called.
test_singleSubscriber :: LeiosDbHandle IO -> IO ()
test_singleSubscriber db = do
  chan <- subscribeEbNotifications db
  let point = mkTestPoint (SlotNo 1001) 1001
      eb = mkTestEb 3
  leiosDbInsertEbBody db point eb
  notification <- atomically $ readTChan chan
  case notification of
    LeiosOfferBlock notifPoint _ ->
      notifPoint @?= point
    LeiosOfferBlockTxs _ ->
      assertFailure "expected LeiosOfferBlock, got LeiosOfferBlockTxs"

-- | Test that multiple subscribers each receive the notification.
test_multipleSubscribers :: LeiosDbHandle IO -> IO ()
test_multipleSubscribers db = do
  chan1 <- subscribeEbNotifications db
  chan2 <- subscribeEbNotifications db
  chan3 <- subscribeEbNotifications db
  let point = mkTestPoint (SlotNo 1002) 1002
      eb = mkTestEb 5
  leiosDbInsertEbBody db point eb
  -- All subscribers should receive the notification
  notif1 <- atomically $ readTChan chan1
  notif2 <- atomically $ readTChan chan2
  notif3 <- atomically $ readTChan chan3
  assertOfferBlock point notif1
  assertOfferBlock point notif2
  assertOfferBlock point notif3

-- | Test that the notification contains the correct LeiosOfferBlock data.
test_correctData :: LeiosDbHandle IO -> IO ()
test_correctData db = do
  chan <- subscribeEbNotifications db
  let point = mkTestPoint (SlotNo 1003) 1003
      eb = mkTestEb 10
      expectedSize = leiosEbBytesSize eb
  leiosDbInsertEbBody db point eb
  notification <- atomically $ readTChan chan
  case notification of
    LeiosOfferBlock notifPoint notifSize -> do
      notifPoint.pointSlotNo @?= point.pointSlotNo
      notifPoint.pointEbHash @?= point.pointEbHash
      notifSize @?= expectedSize
    LeiosOfferBlockTxs _ ->
      assertFailure "expected LeiosOfferBlock, got LeiosOfferBlockTxs"

-- | Test that a subscriber who subscribes after an insertion does not receive
-- the past notification.
test_lateSubscriber :: LeiosDbHandle IO -> IO ()
test_lateSubscriber db = do
  -- Insert before subscribing
  let point1 = mkTestPoint (SlotNo 1004) 1004
      eb1 = mkTestEb 2
  leiosDbInsertEbBody db point1 eb1
  -- Now subscribe
  chan <- subscribeEbNotifications db
  -- The channel should be empty (no past notifications)
  maybeNotif <- atomically $ tryReadTChan chan
  case maybeNotif of
    Nothing -> pure () -- Expected: no notification
    Just _ -> assertFailure "late subscriber should not receive past notifications"
  -- But new insertions should be received
  let point2 = mkTestPoint (SlotNo 1005) 1005
      eb2 = mkTestEb 3
  leiosDbInsertEbBody db point2 eb2
  notification <- atomically $ readTChan chan
  assertOfferBlock point2 notification

-- | Test that multiple insertions yield multiple notifications in order.
test_multipleNotifications :: LeiosDbHandle IO -> IO ()
test_multipleNotifications db = do
  chan <- subscribeEbNotifications db
  let points =
        [ mkTestPoint (SlotNo i) (fromIntegral i)
        | i <- [1010 .. 1014]
        ]
      ebs = [mkTestEb i | i <- [1 .. 5]]
  -- Insert all
  mapM_ (uncurry $ leiosDbInsertEbBody db) (zip points ebs)
  -- Read all notifications and verify order
  notifications <- replicateM 5 (atomically $ readTChan chan)
  mapM_
    (uncurry assertOfferBlock)
    (zip points notifications)

-- | Test that no LeiosOfferBlockTxs notification is produced when only some
-- transactions have been updated via leiosDbUpdateEbTx.
test_noOfferBlockTxsBeforeComplete :: LeiosDbHandle IO -> IO ()
test_noOfferBlockTxsBeforeComplete db = do
  chan <- subscribeEbNotifications db
  let point = mkTestPoint (SlotNo 1020) 1020
      eb = mkTestEb 3 -- 3 transactions
  leiosDbInsertEbBody db point eb
  -- Consume the LeiosOfferBlock notification
  _ <- atomically $ readTChan chan
  -- Update only 2 of 3 txs
  leiosDbUpdateEbTx db point.pointEbHash 0 (BS.pack [1, 2, 3])
  leiosDbUpdateEbTx db point.pointEbHash 1 (BS.pack [4, 5, 6])
  -- No LeiosOfferBlockTxs notification should be available
  maybeNotif <- atomically $ tryReadTChan chan
  case maybeNotif of
    Nothing -> pure ()
    Just _ -> assertFailure "should not notify before all txs are updated"

-- | Test that a LeiosOfferBlockTxs notification is produced when the last
-- transaction is inserted via leiosDbUpdateEbTx.
test_offerBlockTxs :: LeiosDbHandle IO -> IO ()
test_offerBlockTxs db = do
  chan <- subscribeEbNotifications db
  let point = mkTestPoint (SlotNo 1030) 1030
      eb = mkTestEb 3 -- 3 transactions
      -- Insert the EB body (creates entries with NULL txBytes)
  leiosDbInsertEbBody db point eb
  -- Consume the LeiosOfferBlock notification
  _ <- atomically $ readTChan chan
  -- Update all txs
  leiosDbUpdateEbTx db point.pointEbHash 0 (BS.pack [1, 2, 3])
  leiosDbUpdateEbTx db point.pointEbHash 1 (BS.pack [4, 5, 6])
  leiosDbUpdateEbTx db point.pointEbHash 2 (BS.pack [7, 8, 9])
  notification <- atomically $ readTChan chan
  assertOfferBlockTxs point notification

-- * Test utilities

-- | Create a temporary SQLite database.
-- Returns the handle and temp directory path for cleanup.
mkTestSqliteDb :: IO (LeiosDbHandle IO, FilePath)
mkTestSqliteDb = do
  sysTmp <- getCanonicalTemporaryDirectory
  tmpDir <- createTempDirectory sysTmp "leios-test"
  db <- newLeiosDbConnectionIO (tmpDir <> "/test.db")
  pure (db, tmpDir)

-- | Assert that a notification is LeiosOfferBlock with the expected point.
assertOfferBlock :: LeiosPoint -> LeiosNotification -> IO ()
assertOfferBlock expectedPoint = \case
  LeiosOfferBlock actualPoint _ ->
    actualPoint @?= expectedPoint
  LeiosOfferBlockTxs _ ->
    assertFailure "expected LeiosOfferBlock, got LeiosOfferBlockTxs"

-- | Assert that a notification is LeiosOfferBlockTxs with the expected point.
assertOfferBlockTxs :: LeiosPoint -> LeiosNotification -> IO ()
assertOfferBlockTxs expectedPoint = \case
  LeiosOfferBlockTxs actualPoint ->
    actualPoint @?= expectedPoint
  LeiosOfferBlock _ _ ->
    assertFailure "expected LeiosOfferBlockTxs, got LeiosOfferBlock"
