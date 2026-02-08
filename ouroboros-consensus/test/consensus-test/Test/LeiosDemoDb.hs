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
import Control.Monad (forM, forM_, replicateM)
import qualified Data.ByteString as BS
import Data.Time.Clock (diffUTCTime, getCurrentTime)
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
  ( Gen
  , Property
  , chooseInt
  , counterexample
  , forAll
  , forAllShrinkShow
  , ioProperty
  , shrink
  , sized
  , tabulate
  , vector
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
mkTestGroups :: IO (LeiosDbHandle IO) -> [TestTree]
mkTestGroups getDb =
  [ testGroup
      "points"
      [ testProperty "insert then scan" $ prop_pointsInsertThenScan getDb
      , testProperty "multiple inserts accumulate" $ prop_pointsAccumulate getDb
      ]
  , testGroup
      "ebs"
      [ testProperty "insert then lookup" $ prop_ebsInsertThenLookup getDb
      , testProperty "lookup missing returns empty" $ prop_ebsLookupMissing getDb
      ]
  , testGroup
      "transactions"
      [ testProperty "update then retrieve" $ prop_txsUpdateThenRetrieve getDb
      , testProperty "retrieve missing returns empty" $ prop_txsRetrieveMissing getDb
      , testProperty "updateEbTx performance" $ prop_updateEbTxPerformance getDb
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

-- * QuickCheck generators

-- | Generate a random EbHash (32 random bytes).
-- With 256 bits of randomness, collisions are practically impossible.
genEbHash :: Gen EbHash
genEbHash = MkEbHash . BS.pack <$> vector 32

-- | Generate a random TxHash (32 random bytes).
genTxHash :: Gen TxHash
genTxHash = MkTxHash . BS.pack <$> vector 32

-- | Generate a random SlotNo.
genSlotNo :: Gen SlotNo
genSlotNo = SlotNo . fromIntegral <$> chooseInt (0, maxBound)

-- | Generate a random LeiosPoint.
genPoint :: Gen LeiosPoint
genPoint = MkLeiosPoint <$> genSlotNo <*> genEbHash

-- | Generate a LeiosEb with the given number of transactions.
genEb :: Int -> Gen LeiosEb
genEb numTxs = do
  txs <- replicateM numTxs $ do
    txHash <- genTxHash
    size <- chooseInt (50, 500)
    pure (txHash, fromIntegral size)
  pure $ MkLeiosEb $ V.fromList txs

-- | Generate a LeiosPoint and LeiosEb together with the given number of transactions.
genPointAndEb :: Int -> Gen (LeiosPoint, LeiosEb)
genPointAndEb numTxs = (,) <$> genPoint <*> genEb numTxs

-- | Generate random transaction bytes.
genTxBytes :: Gen BS.ByteString
genTxBytes = BS.pack <$> vector 100

-- * Test fixtures for unit tests

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
prop_pointsInsertThenScan :: IO (LeiosDbHandle IO) -> Property
prop_pointsInsertThenScan getDb =
  forAll genPoint $ \point ->
    ioProperty $ do
      db <- getDb
      leiosDbInsertEbPoint db point
      points <- leiosDbScanEbPoints db
      pure $ (point.pointSlotNo, point.pointEbHash) `elem` points

-- | Property: multiple inserted points all appear in scan results.
prop_pointsAccumulate :: IO (LeiosDbHandle IO) -> Property
prop_pointsAccumulate getDb =
  forAllShrinkShow (chooseInt (1, 10)) shrink show $ \count ->
    forAll (replicateM count genPoint) $ \points ->
      ioProperty $ do
        db <- getDb
        forM_ points $ leiosDbInsertEbPoint db
        scanned <- leiosDbScanEbPoints db
        let expected = [(p.pointSlotNo, p.pointEbHash) | p <- points]
        pure $ all (`elem` scanned) expected

-- * Property tests for EBs

-- | Property: inserting an EB body and looking it up returns the correct txs.
prop_ebsInsertThenLookup :: IO (LeiosDbHandle IO) -> Property
prop_ebsInsertThenLookup getDb =
  forAllShrinkShow (chooseInt (1, 50)) shrink show $ \numTxs ->
    forAll (genPointAndEb numTxs) $ \(point, eb) ->
      ioProperty $ do
        db <- getDb
        let expectedTxs = V.toList (leiosEbTxs eb)
        leiosDbInsertEbBody db point eb
        result <- leiosDbLookupEbBody db point.pointEbHash
        pure $
          counterexample ("Expected: " ++ show expectedTxs ++ "\nGot: " ++ show result) $
            result == expectedTxs

-- | Property: looking up a non-existent EB returns empty list.
prop_ebsLookupMissing :: IO (LeiosDbHandle IO) -> Property
prop_ebsLookupMissing getDb =
  forAll genEbHash $ \missingHash ->
    ioProperty $ do
      db <- getDb
      result <- leiosDbLookupEbBody db missingHash
      pure $ result === []

-- * Property tests for transactions

-- | Property: updating tx bytes and retrieving them returns the correct data.
prop_txsUpdateThenRetrieve :: IO (LeiosDbHandle IO) -> Property
prop_txsUpdateThenRetrieve getDb =
  forAllShrinkShow (chooseInt (1, 20)) shrink show $ \numTxs ->
    forAll (genPointAndEb numTxs) $ \(point, eb) ->
      ioProperty $ do
        db <- getDb
        -- Insert the EB body first
        leiosDbInsertEbBody db point eb
        -- Update some transactions with bytes
        let offsetsToUpdate = filter (< numTxs) [0, numTxs `div` 2, numTxs - 1]
        txBytesMap <- forM offsetsToUpdate $ \off -> do
          bytes <- BS.pack <$> replicateM 100 (pure $ fromIntegral off)
          pure (off, bytes)
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
prop_txsRetrieveMissing :: IO (LeiosDbHandle IO) -> Property
prop_txsRetrieveMissing getDb =
  forAll genEbHash $ \missingHash ->
    ioProperty $ do
      db <- getDb
      result <- leiosDbBatchRetrieveTxs db missingHash [0, 1, 2]
      pure $ result === []

-- | Property: measure leiosDbUpdateEbTx performance and report timing distribution.
-- Tracks performance across different database sizes and EB configurations.
-- Use --quickcheck-max-size to scale the test parameters.
prop_updateEbTxPerformance :: IO (LeiosDbHandle IO) -> Property
prop_updateEbTxPerformance getDb =
  forAllShrinkShow genPerfParams shrinkPerfParams show $ \(dbSize, numTxs) ->
    forAll (genPerfData dbSize numTxs) $ \(primeData, point, eb, txBytes) ->
      ioProperty $ do
        db <- getDb
        -- Prime the database with additional EBs to simulate load
        forM_ primeData $ \(primePoint, primeEb) ->
          leiosDbInsertEbBody db primePoint primeEb
        -- Create the target EB
        leiosDbInsertEbBody db point eb
        -- Measure time for a single updateEbTx call
        start <- getCurrentTime
        leiosDbUpdateEbTx db point.pointEbHash 0 txBytes
        end <- getCurrentTime
        let elapsedUs = realToFrac (diffUTCTime end start) * 1_000_000 :: Double
            timeBucket
              | elapsedUs < 100 = "<100μs"
              | elapsedUs < 1000 = "100μs-1ms"
              | elapsedUs < 2000 = "1-2ms"
              | elapsedUs < 5000 = "2-5ms"
              | elapsedUs < 10000 = "5-10ms"
              | elapsedUs < 50000 = "10-50ms"
              | otherwise = ">50ms"
            sizeBucket n
              | n == 0 = "0"
              | n <= 10 = "1-10"
              | n <= 100 = "11-100"
              | n <= 1000 = "101-1000"
              | otherwise = ">1000"
            dbBucket = sizeBucket dbSize
            txsBucket = sizeBucket numTxs
        pure $
          tabulate "updateEbTx time" [timeBucket] $
            tabulate "DB size (EBs)" [dbBucket] $
              tabulate "numTxs in EB" [txsBucket] $
                counterexample
                  ("updateEbTx took " ++ show elapsedUs ++ "μs with " ++ show dbSize ++ " EBs in DB")
                  True
 where
  genPerfParams = sized $ \size -> do
    let maxSize = max 1 size
    dbSize <- chooseInt (0, maxSize)
    numTxs <- chooseInt (1, maxSize)
    pure (dbSize, numTxs)

  shrinkPerfParams (dbSize, numTxs) =
    [(d, numTxs) | d <- shrink dbSize]
      ++ [(dbSize, n) | n <- shrink numTxs, n >= 1]

  -- Generate all data needed for a performance test run
  genPerfData dbSize numTxs = do
    primeData <- replicateM dbSize (genPointAndEb 10)
    point <- genPoint
    eb <- genEb numTxs
    txBytes <- genTxBytes
    pure (primeData, point, eb, txBytes)

-- * Notification tests

-- | Test that a single subscriber receives a notification when insertEbBody is called.
test_singleSubscriber :: LeiosDbHandle IO -> IO ()
test_singleSubscriber db = do
  chan <- subscribeEbNotifications db
  let point = mkTestPoint (SlotNo 1001) 1
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
  let point = mkTestPoint (SlotNo 1002) 2
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
  let point = mkTestPoint (SlotNo 1003) 3
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
  let point1 = mkTestPoint (SlotNo 1004) 4
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
  let point2 = mkTestPoint (SlotNo 1005) 5
      eb2 = mkTestEb 3
  leiosDbInsertEbBody db point2 eb2
  notification <- atomically $ readTChan chan
  assertOfferBlock point2 notification

-- | Test that multiple insertions yield multiple notifications in order.
test_multipleNotifications :: LeiosDbHandle IO -> IO ()
test_multipleNotifications db = do
  chan <- subscribeEbNotifications db
  let points =
        [ mkTestPoint (SlotNo (1010 + i)) (fromIntegral i + 10)
        | i <- [0 .. 4]
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
  let point = mkTestPoint (SlotNo 1020) 20
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
  let point = mkTestPoint (SlotNo 1030) 30
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
