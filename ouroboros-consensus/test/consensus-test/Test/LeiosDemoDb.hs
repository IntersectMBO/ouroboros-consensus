{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Tests for the LeiosDemoDb interface.
--
-- These tests verify the semantics of the LeiosDbHandle operations for both
-- InMemory and SQLite implementations. Each test case uses a fresh database
-- to ensure isolation and allow non-priming tests to serve as performance baselines.
module Test.LeiosDemoDb (tests) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.Class.MonadSTM.Strict (atomically, readTChan, tryReadTChan)
import Control.Exception (bracket)
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
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "LeiosDemoDb" $
    forEachImplementation mkTestGroups

-- | Database creation strategy for different implementations.
data DbImpl
  = InMemory
  | SQLite

-- | Create a fresh database and run an action with it.
-- Ensures proper cleanup for SQLite databases.
withFreshDb :: DbImpl -> (LeiosDbHandle IO -> IO a) -> IO a
withFreshDb InMemory action = newInMemoryLeiosDb >>= action
withFreshDb SQLite action = do
  sysTmp <- getCanonicalTemporaryDirectory
  bracket
    (createTempDirectory sysTmp "leios-test")
    removeDirectoryRecursive
    (\tmpDir -> newLeiosDbConnectionIO (tmpDir <> "/test.db") >>= action)

-- | Run tests for each database implementation (InMemory and SQLite).
forEachImplementation :: (DbImpl -> [TestTree]) -> [TestTree]
forEachImplementation mkTests =
  [ testGroup "InMemory" (mkTests InMemory)
  , testGroup "SQLite" (mkTests SQLite)
  ]

-- | Create the test groups for a given database implementation.
mkTestGroups :: DbImpl -> [TestTree]
mkTestGroups impl =
  [ testGroup
      "points"
      [ testProperty "insert then scan" $ prop_pointsInsertThenScan impl
      , testProperty "multiple inserts accumulate" $ prop_pointsAccumulate impl
      ]
  , testGroup
      "ebs"
      [ testProperty "insert then lookup" $ prop_ebsInsertThenLookup impl
      , testProperty "lookup missing returns empty" $ prop_ebsLookupMissing impl
      ]
  , testGroup
      "transactions"
      [ testProperty "update then retrieve" $ prop_txsUpdateThenRetrieve impl
      , testProperty "retrieve missing returns empty" $ prop_txsRetrieveMissing impl
      , testProperty "updateEbTx performance" $ prop_updateEbTxPerformance impl
      ]
  , testGroup
      "notifications"
      [ testCase "single subscriber" $ withFreshDb impl test_singleSubscriber
      , testCase "multiple subscribers" $ withFreshDb impl test_multipleSubscribers
      , testCase "correct data" $ withFreshDb impl test_correctData
      , testCase "late subscriber" $ withFreshDb impl test_lateSubscriber
      , testCase "multiple notifications" $ withFreshDb impl test_multipleNotifications
      , testCase "no offerBlockTxs before last update" $ withFreshDb impl test_noOfferBlockTxsBeforeComplete
      , testCase "offerBlockTxs on last update" $ withFreshDb impl test_offerBlockTxs
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
prop_pointsInsertThenScan :: DbImpl -> Property
prop_pointsInsertThenScan impl =
  forAll genPoint $ \point ->
    ioProperty $ withFreshDb impl $ \db -> do
      leiosDbInsertEbPoint db point
      points <- leiosDbScanEbPoints db
      pure $ (point.pointSlotNo, point.pointEbHash) `elem` points

-- | Property: multiple inserted points all appear in scan results.
prop_pointsAccumulate :: DbImpl -> Property
prop_pointsAccumulate impl =
  forAllShrinkShow (chooseInt (1, 10)) shrink show $ \count ->
    forAll (replicateM count genPoint) $ \points ->
      ioProperty $ withFreshDb impl $ \db -> do
        forM_ points $ leiosDbInsertEbPoint db
        scanned <- leiosDbScanEbPoints db
        let expected = [(p.pointSlotNo, p.pointEbHash) | p <- points]
        pure $ all (`elem` scanned) expected

-- * Property tests for EBs

-- | Property: inserting an EB body and looking it up returns the correct txs.
prop_ebsInsertThenLookup :: DbImpl -> Property
prop_ebsInsertThenLookup impl =
  forAllShrinkShow (chooseInt (1, 50)) shrink show $ \numTxs ->
    forAll (genPointAndEb numTxs) $ \(point, eb) ->
      ioProperty $ withFreshDb impl $ \db -> do
        let expectedTxs = V.toList (leiosEbTxs eb)
        leiosDbInsertEbBody db point eb
        result <- leiosDbLookupEbBody db point.pointEbHash
        pure $
          counterexample ("Expected: " ++ show expectedTxs ++ "\nGot: " ++ show result) $
            result == expectedTxs

-- | Property: looking up a non-existent EB returns empty list.
prop_ebsLookupMissing :: DbImpl -> Property
prop_ebsLookupMissing impl =
  forAll genEbHash $ \missingHash ->
    ioProperty $ withFreshDb impl $ \db -> do
      result <- leiosDbLookupEbBody db missingHash
      pure $ result === []

-- * Property tests for transactions

-- | Property: updating tx bytes and retrieving them returns the correct data.
prop_txsUpdateThenRetrieve :: DbImpl -> Property
prop_txsUpdateThenRetrieve impl =
  forAllShrinkShow (chooseInt (1, 20)) shrink show $ \numTxs ->
    forAll (genPointAndEb numTxs) $ \(point, eb) ->
      ioProperty $ withFreshDb impl $ \db -> do
        -- Insert the EB body first
        leiosDbInsertEbBody db point eb
        -- Update some transactions with bytes
        let offsetsToUpdate = filter (< numTxs) [0, numTxs `div` 2, numTxs - 1]
        txBytesMap <- forM offsetsToUpdate $ \off -> do
          let bytes = BS.pack $ replicate 100 (fromIntegral off)
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
prop_txsRetrieveMissing :: DbImpl -> Property
prop_txsRetrieveMissing impl =
  forAll genEbHash $ \missingHash ->
    ioProperty $ withFreshDb impl $ \db -> do
      result <- leiosDbBatchRetrieveTxs db missingHash [0, 1, 2]
      pure $ result === []

-- | Property: measure leiosDbUpdateEbTx performance and report timing distribution.
-- Tracks performance across different database sizes and EB configurations.
-- Use --quickcheck-max-size to scale the test parameters.
prop_updateEbTxPerformance :: DbImpl -> Property
prop_updateEbTxPerformance impl =
  forAllShrinkShow genPerfParams shrinkPerfParams show $ \(dbSize, numTxs) ->
    forAll (genPerfData dbSize numTxs) $ \(primeData, point, eb, txBytes) ->
      ioProperty $ withFreshDb impl $ \db -> do
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
  let point = mkTestPoint (SlotNo 1) 1
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
  let point = mkTestPoint (SlotNo 1) 1
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
  let point = mkTestPoint (SlotNo 1) 1
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
  let point1 = mkTestPoint (SlotNo 1) 1
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
  let point2 = mkTestPoint (SlotNo 2) 2
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
        | i <- [1 .. 5]
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
  let point = mkTestPoint (SlotNo 1) 1
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
  let point = mkTestPoint (SlotNo 1) 1
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
