{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

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
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Function ((&))
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
  , conjoin
  , counterexample
  , forAll
  , forAllShrinkShow
  , ioProperty
  , property
  , shrink
  , shrinkList
  , sized
  , sublistOf
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

-- * Timing helpers

-- | Measure the time of an IO action and return the result with elapsed microseconds.
timed :: IO a -> IO (a, Double)
timed action = do
  start <- getCurrentTime
  result <- action
  end <- getCurrentTime
  let elapsedUs = realToFrac (diffUTCTime end start) * 1_000_000
  pure (result, elapsedUs)

-- | Convert elapsed microseconds to a bucket label.
timeBucket :: Double -> String
timeBucket elapsedUs
  | elapsedUs < 1 = "<1μs"
  | elapsedUs < 10 = "1μs-10μs"
  | elapsedUs < 100 = "10μs-100μs"
  | elapsedUs < 1_000 = "100μs-1ms"
  | elapsedUs < 2_000 = "1-2ms"
  | elapsedUs < 5_000 = "2-5ms"
  | elapsedUs < 10_000 = "5-10ms"
  | elapsedUs < 50_000 = "10-50ms"
  | otherwise = ">50ms"

magnitudeBucket :: (Num a, Ord a) => a -> String
magnitudeBucket size
  | size < 1 = "0-1"
  | size < 10 = "1-10"
  | size < 100 = "10-100"
  | size < 1_000 = "100-1_000"
  | size < 10_000 = "1_000-10_000"
  | size < 100_000 = "10_000-100_000"
  | otherwise = ">100_000"

-- * Property tests for points

-- | Property: inserting a point and then scanning should return it.
prop_pointsInsertThenScan :: DbImpl -> Property
prop_pointsInsertThenScan impl =
  forAll genPoint $ \point ->
    ioProperty $ withFreshDb impl $ \db -> do
      (_, insertTime) <- timed $ leiosDbInsertEbPoint db point
      (points, scanTime) <- timed $ leiosDbScanEbPoints db
      pure $
        tabulate "insertEbPoint" [timeBucket insertTime] $
          tabulate "scanEbPoints" [timeBucket scanTime] $
            (point.pointSlotNo, point.pointEbHash) `elem` points

-- | Property: multiple inserted points all appear in scan results.
prop_pointsAccumulate :: DbImpl -> Property
prop_pointsAccumulate impl =
  forAllShrinkShow (chooseInt (1, 10)) shrink show $ \count ->
    forAll (replicateM count genPoint) $ \points ->
      ioProperty $ withFreshDb impl $ \db -> do
        (_, insertTime) <- timed $ forM_ points $ leiosDbInsertEbPoint db
        (scanned, scanTime) <- timed $ leiosDbScanEbPoints db
        let expected = [(p.pointSlotNo, p.pointEbHash) | p <- points]
        pure $
          tabulate "insertEbPoint (batch)" [timeBucket insertTime] $
            tabulate "scanEbPoints" [timeBucket scanTime] $
              all (`elem` scanned) expected

-- * Property tests for EBs

-- | Property: inserting an EB body and looking it up returns the correct txs.
prop_ebsInsertThenLookup :: DbImpl -> Property
prop_ebsInsertThenLookup impl =
  forAllShrinkShow (chooseInt (1, 50)) shrink show $ \numTxs ->
    forAll (genPointAndEb numTxs) $ \(point, eb) ->
      ioProperty $ withFreshDb impl $ \db -> do
        let expectedTxs = V.toList (leiosEbTxs eb)
        (_, insertTime) <- timed $ leiosDbInsertEbBody db point eb
        (result, lookupTime) <- timed $ leiosDbLookupEbBody db point.pointEbHash
        pure $
          tabulate "insertEbBody" [timeBucket insertTime] $
            tabulate "lookupEbBody" [timeBucket lookupTime] $
              counterexample ("Expected: " ++ show expectedTxs ++ "\nGot: " ++ show result) $
                result == expectedTxs

-- | Property: looking up a non-existent EB returns empty list.
prop_ebsLookupMissing :: DbImpl -> Property
prop_ebsLookupMissing impl =
  forAll genEbHash $ \missingHash ->
    ioProperty $ withFreshDb impl $ \db -> do
      (result, lookupTime) <- timed $ leiosDbLookupEbBody db missingHash
      pure $
        tabulate "lookupEbBody (missing)" [timeBucket lookupTime] $
          result === []

-- * Property tests for transactions

-- | Property: updating tx bytes and retrieving them returns the correct data.
prop_txsUpdateThenRetrieve :: DbImpl -> Property
prop_txsUpdateThenRetrieve impl =
  forAllShrinkShow genTxs shrinkTxs showTxs $ \(numTxs, txs) ->
    forAll (genPointAndEb numTxs) $ \(point, eb) ->
      ioProperty $ withFreshDb impl $ \db -> do
        -- Insert the EB body first
        leiosDbInsertEbBody db point eb
        -- Update some transactions with bytes
        updateTimes <- forM txs $ \(off, bytes) ->
          snd <$> timed (leiosDbUpdateEbTx db point.pointEbHash off bytes)
        -- Retrieve all offsets
        let allOffsets = [0 .. numTxs - 1]
        (results, retrieveTime) <- timed $ leiosDbBatchRetrieveTxs db point.pointEbHash allOffsets
        -- Check that updated offsets have the correct bytes
        let checkResult (off, _txHash, mBytes) =
              case lookup off txs of
                Just expectedBytes -> mBytes == Just expectedBytes
                Nothing -> mBytes == Nothing -- Not updated yet
        pure $
          conjoin
            [ property $ all checkResult results
            , length results === numTxs
            ]
            & counterexample ("Results: " ++ show results)
            & tabulate "updateEbTx" (timeBucket <$> updateTimes)
            & tabulate "batchRetrieveTxs" [timeBucket retrieveTime]
            & tabulate "txs" [magnitudeBucket $ length txs]
 where
  genTxs :: Gen (Int, [(Int, ByteString)])
  genTxs = sized $ \n -> do
    totalTxCount <- chooseInt (1, n * 10)
    existing <- sublistOf [0 .. totalTxCount]
    existingTxs <- forM existing $ \off -> do
      c <- chooseInt (50, 16_000)
      pure (off, BS.pack $ replicate c 1) -- txs with all 1's
    pure (totalTxCount, existingTxs)

  shrinkTxs (_, txs) =
    [ (m + 1, txs')
    | txs' <- shrinkList pure txs
    , let m = maximum (fst <$> txs')
    ]

  showTxs (total, txs) = "Total: " <> show total <> ", Existing: " <> show (length txs)

-- | Property: retrieving from non-existent EB returns empty list.
prop_txsRetrieveMissing :: DbImpl -> Property
prop_txsRetrieveMissing impl =
  forAll genEbHash $ \missingHash ->
    ioProperty $ withFreshDb impl $ \db -> do
      (result, retrieveTime) <- timed $ leiosDbBatchRetrieveTxs db missingHash [0, 1, 2]
      pure $
        tabulate "batchRetrieveTxs (missing)" [timeBucket retrieveTime] $
          result === []

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
        (_, updateTime) <- timed $ leiosDbUpdateEbTx db point.pointEbHash 0 txBytes
        pure $
          tabulate "updateEbTx (primed)" [timeBucket updateTime] $
            tabulate "DB size (EBs)" [magnitudeBucket dbSize] $
              tabulate "numTxs in EB" [magnitudeBucket numTxs] $
                counterexample
                  ("updateEbTx took " ++ show updateTime ++ "μs with " ++ show dbSize ++ " EBs in DB")
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
