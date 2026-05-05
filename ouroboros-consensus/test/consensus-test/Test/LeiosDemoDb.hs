{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Tests for the LeiosDemoDb interface.
--
-- These tests verify the semantics of the LeiosDbHandle operations for both
-- InMemory and SQLite implementations. Each test case uses a fresh database
-- to ensure isolation and serve as an (too optimistic) performance baseline.
module Test.LeiosDemoDb (tests) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.Class.MonadSTM.Strict (atomically, readTChan, tryReadTChan)
import Control.DeepSeq
import Control.Exception (bracket)
import Control.Monad (forM, forM_, replicateM)
import Control.Monad.Class.MonadTime.SI (diffTime, getMonotonicTime)
import qualified Data.ByteString as BS
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Time.Clock (DiffTime)
import qualified Data.Vector as V
import LeiosDemoDb
  ( LeiosDbHandle (..)
  , LeiosEbNotification (..)
  , LeiosFetchWork (..)
  , leiosDbBatchRetrieveTxs
  , leiosDbFilterMissingEbBodies
  , leiosDbFilterMissingTxs
  , leiosDbInsertEbBody
  , leiosDbInsertEbPoint
  , leiosDbInsertTxs
  , leiosDbLookupEbBody
  , leiosDbLookupEbPoint
  , leiosDbQueryCompletedEbByPoint
  , leiosDbQueryFetchWork
  , leiosDbScanEbPoints
  , newLeiosDBInMemory
  , newLeiosDBSQLite
  , withLeiosDb
  )
import LeiosDemoTypes
  ( EbHash (..)
  , LeiosEb (..)
  , LeiosPoint (..)
  , TxHash (..)
  , leiosEbBytesSize
  , leiosEbTxs
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
  , forAllBlind
  , forAllShrinkShow
  , ioProperty
  , shrink
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
withFreshDb InMemory action =
  newLeiosDBInMemory >>= action
withFreshDb SQLite action = do
  sysTmp <- getCanonicalTemporaryDirectory
  bracket
    ( do
        tmpDir <- createTempDirectory sysTmp "leios-test"
        db <- newLeiosDBSQLite (tmpDir <> "/test.db")
        pure (db, removeDirectoryRecursive tmpDir)
    )
    snd
    (action . fst)

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
      , testProperty "insert then lookup" $ prop_pointsInsertThenLookup impl
      , testProperty "lookup missing returns Nothing" $ prop_pointsLookupMissing impl
      , testProperty "multiple inserts accumulate" $ prop_pointsAccumulate impl
      ]
  , testGroup
      "ebs"
      [ testProperty "insert then lookup" $ prop_ebsInsertThenLookup impl
      , testProperty "lookup missing returns empty" $ prop_ebsLookupMissing impl
      ]
  , testGroup
      "transactions"
      [ testProperty "insert then retrieve" $ prop_txsInsertThenRetrieve impl
      , testProperty "retrieve missing returns empty" $ prop_txsRetrieveMissing impl
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
      , testCase "no re-notification of completed EBs" $ withFreshDb impl test_noReNotifyCompletedEbs
      ]
  , testGroup
      "queryFetchWork"
      [ testProperty "empty db returns empty work" $ prop_fetchWorkEmpty impl
      , testProperty "missing bodies reported" $ prop_fetchWorkMissingBodies impl
      , testProperty "missing txs reported" $ prop_fetchWorkMissingTxs impl
      , testProperty "complete eb has no missing txs" $ prop_fetchWorkCompleteTxs impl
      ]
  , testGroup
      "filterMissingEbBodies"
      [ testProperty "empty input returns empty" $ prop_filterEbBodiesEmpty impl
      , testProperty "returns only missing EBs" $ prop_filterEbBodiesCorrect impl
      ]
  , testGroup
      "filterMissingTxs"
      [ testProperty "empty input returns empty" $ prop_filterTxsEmpty impl
      , testProperty "returns only missing TXs" $ prop_filterTxsCorrect impl
      ]
  , testGroup
      "queryCompletedEbByPoint"
      [ testProperty "complete EB returns Just with tx data" $ prop_completedEbComplete impl
      , testProperty "no txs returns Nothing" $ prop_completedEbMissingTxs impl
      , testProperty "partial txs returns Nothing" $ prop_completedEbPartialTxs impl
      , testProperty "no body returns Nothing" $ prop_completedEbNoBody impl
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

-- | Generate random max sized tx (16k).
genTxBytes :: Gen BS.ByteString
genTxBytes = BS.pack <$> vector 16_384

-- | Max sized tx (16k) with all zeros.
maxTxBytesZero :: BS.ByteString
maxTxBytesZero = BS.replicate 16_384 0

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

-- | Measure the time of an IO action and return the result with 'DiffTime'.
timed :: IO a -> IO (a, DiffTime)
timed action = do
  start <- getMonotonicTime
  result <- action
  end <- getMonotonicTime
  pure (result, diffTime end start)

-- | Convert 'DiffTime' to a bucket label.
timeBucket :: DiffTime -> String
timeBucket d
  | d < micro 1 = "<1μs"
  | d < micro 10 = "1μs-10μs"
  | d < micro 100 = "10μs-100μs"
  | d < milli 1 = "100μs-1ms"
  | d < milli 2 = "1-2ms"
  | d < milli 3 = "2-3ms"
  | d < milli 4 = "3-4ms"
  | d < milli 5 = "4-5ms"
  | d < milli 10 = "5-10ms"
  | d < milli 50 = "10-50ms"
  | otherwise = ">50ms"
 where

milli :: Integer -> DiffTime
milli x = fromIntegral x / 1000

micro :: Integer -> DiffTime
micro x = fromIntegral x / 1_000_000

showTime :: DiffTime -> String
showTime t
  | t < micro 1 = show (s * 1_000_000_000) <> "ns"
  | t < milli 1 = show (s * 1_000_000) <> "μs"
  | t < 1 = show (s * 1_000) <> "ms"
  | otherwise = show t
 where
  s = realToFrac t :: Double

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
    ioProperty $ withFreshDb impl $ \db -> withLeiosDb db $ \con -> do
      (_, insertTime) <- timed $ leiosDbInsertEbPoint con point 1000
      (points, scanTime) <- timed $ leiosDbScanEbPoints con
      pure $
        (point.pointSlotNo, point.pointEbHash) `elem` points
          & tabulate "insertEbPoint" [timeBucket insertTime]
          & tabulate "scanEbPoints" [timeBucket scanTime]

-- | Property: inserting a point and then looking it up returns the slot.
prop_pointsInsertThenLookup :: DbImpl -> Property
prop_pointsInsertThenLookup impl =
  forAll genPoint $ \point ->
    ioProperty $ withFreshDb impl $ \db -> withLeiosDb db $ \con -> do
      (_, insertTime) <- timed $ leiosDbInsertEbPoint con point 1000
      (result, lookupTime) <- timed $ leiosDbLookupEbPoint con point.pointEbHash
      pure $
        result === Just point.pointSlotNo
          & tabulate "insertEbPoint" [timeBucket insertTime]
          & tabulate "lookupEbPoint" [timeBucket lookupTime]

-- | Property: looking up a non-existent point returns Nothing.
prop_pointsLookupMissing :: DbImpl -> Property
prop_pointsLookupMissing impl =
  forAll genEbHash $ \missingHash ->
    ioProperty $ withFreshDb impl $ \db -> withLeiosDb db $ \con -> do
      (result, lookupTime) <- timed $ leiosDbLookupEbPoint con missingHash
      pure $
        result === Nothing
          & tabulate "lookupEbPoint (missing)" [timeBucket lookupTime]

-- | Property: multiple inserted points all appear in scan results.
prop_pointsAccumulate :: DbImpl -> Property
prop_pointsAccumulate impl =
  forAllShrinkShow (chooseInt (1, 10)) shrink show $ \count ->
    forAll (replicateM count genPoint) $ \points ->
      ioProperty $ withFreshDb impl $ \db -> withLeiosDb db $ \con -> do
        insertTimes <- forM points $ \p ->
          snd <$> timed (leiosDbInsertEbPoint con p 1000)
        (scanned, scanTime) <- timed $ leiosDbScanEbPoints con
        let expected = [(p.pointSlotNo, p.pointEbHash) | p <- points]
        pure $
          all (`elem` scanned) expected
            & tabulate "insertEbPoint" [timeBucket $ maximum insertTimes]
            & tabulate "scanEbPoints" [timeBucket scanTime]

-- * Property tests for EBs

-- | Property: inserting an EB body and looking it up returns the correct txs.
prop_ebsInsertThenLookup :: DbImpl -> Property
prop_ebsInsertThenLookup impl =
  forAllShrinkShow (chooseInt (1, 50)) shrink show $ \numTxs ->
    forAll (genPointAndEb numTxs) $ \(point, eb) ->
      ioProperty $ withFreshDb impl $ \db -> withLeiosDb db $ \con -> do
        let expectedTxs = V.toList (leiosEbTxs eb)
        leiosDbInsertEbPoint con point (leiosEbBytesSize eb)
        (_, insertTime) <- timed $ leiosDbInsertEbBody con point eb
        (result, lookupTime) <- timed $ leiosDbLookupEbBody con point.pointEbHash
        pure $
          result == expectedTxs
            & counterexample ("Expected: " ++ show expectedTxs ++ "\nGot: " ++ show result)
            & tabulate "insertEbBody" [timeBucket insertTime]
            & tabulate "lookupEbBody" [timeBucket lookupTime]

-- | Property: looking up a non-existent EB returns empty list.
prop_ebsLookupMissing :: DbImpl -> Property
prop_ebsLookupMissing impl =
  forAll genEbHash $ \missingHash ->
    ioProperty $ withFreshDb impl $ \db -> withLeiosDb db $ \con -> do
      (result, lookupTime) <- timed $ leiosDbLookupEbBody con missingHash
      pure $
        result === []
          & tabulate "lookupEbBody (missing)" [timeBucket lookupTime]

-- * Property tests for transactions

-- | Property: inserting tx bytes and retrieving them returns the correct data.
-- With the normalized schema, txs are inserted into a global `txs` table by TxHash,
-- and retrieval JOINs with that table.
prop_txsInsertThenRetrieve :: DbImpl -> Property
prop_txsInsertThenRetrieve impl =
  forAllShrinkShow (chooseInt (1, 50)) shrink show $ \numTxs ->
    forAllBlind (genPointAndEb numTxs) $ \(point, eb) ->
      forAllBlind (sublistOf [0 .. numTxs - 1]) $ \offsetsToInsert ->
        ioProperty $ withFreshDb impl $ \db -> withLeiosDb db $ \con -> do
          -- Insert the EB first (point then body)
          leiosDbInsertEbPoint con point (leiosEbBytesSize eb)
          leiosDbInsertEbBody con point eb
          -- Get the txHashes from the EB for the offsets we want to insert
          let ebTxList = V.toList (leiosEbTxs eb)
              !txsToInsert =
                force $
                  [ (txHash, txBytes)
                  | off <- offsetsToInsert
                  , let (txHash, _size) = ebTxList !! off
                  , let txBytes = BS.pack [fromIntegral off, 1, 2, 3] -- deterministic test bytes
                  ]
          -- Insert txs into global txs table
          insertTime <- snd <$> timed (leiosDbInsertTxs con txsToInsert)
          -- Retrieve all offsets
          let allOffsets = [0 .. numTxs - 1]
          (results, retrieveTime) <- timed $ leiosDbBatchRetrieveTxs con point.pointEbHash allOffsets
          -- Check that inserted txs have bytes, others don't
          let checkResult (off, _txHash, mBytes) =
                if off `elem` offsetsToInsert
                  then mBytes == Just (BS.pack [fromIntegral off, 1, 2, 3])
                  else mBytes == Nothing
          pure $
            conjoin
              [ all checkResult results
                  & counterexample "Unexpected bytes"
                  & counterexample ("Inserted offsets: " ++ show offsetsToInsert)
                  & counterexample ("Results: " ++ show results)
              , length results === numTxs
                  & counterexample "Length mismatch"
                  & counterexample ("Results: " ++ show (length results))
              , insertTime < milli 10
                  & counterexample ("Insert took too long: " <> showTime insertTime)
              , retrieveTime < milli 10
                  & counterexample ("Retrieve took too long: " <> showTime retrieveTime)
              ]
              & counterexample ("Total txs: " <> show numTxs)
              & counterexample ("Inserted txs: " <> show (length txsToInsert))
              & tabulate "insertTxs" [timeBucket insertTime]
              & tabulate "batchRetrieveTxs" [timeBucket retrieveTime]
              & tabulate "txs inserted" [magnitudeBucket $ length txsToInsert]

-- | Property: retrieving from non-existent EB returns empty list.
prop_txsRetrieveMissing :: DbImpl -> Property
prop_txsRetrieveMissing impl =
  forAll genEbHash $ \missingHash ->
    ioProperty $ withFreshDb impl $ \db -> withLeiosDb db $ \con -> do
      (result, retrieveTime) <- timed $ leiosDbBatchRetrieveTxs con missingHash [0, 1, 2]
      pure $
        result === []
          & tabulate "batchRetrieveTxs (missing)" [timeBucket retrieveTime]

-- * Notification tests

-- | Test that a single subscriber receives a notification when leiosDbInsertEbBody is called.
test_singleSubscriber :: LeiosDbHandle IO -> IO ()
test_singleSubscriber db = do
  chan <- subscribeEbNotifications db
  let point = mkTestPoint (SlotNo 1) 1
      eb = mkTestEb 3
  withLeiosDb db $ \con -> do
    leiosDbInsertEbPoint con point (leiosEbBytesSize eb)
    leiosDbInsertEbBody con point eb
  notification <- atomically $ readTChan chan
  case notification of
    AcquiredEb notifPoint _ ->
      notifPoint @?= point
    AcquiredEbTxs _ ->
      assertFailure "expected AcquiredEb, got AcquiredEbTxs"

-- | Test that multiple subscribers each receive the notification.
test_multipleSubscribers :: LeiosDbHandle IO -> IO ()
test_multipleSubscribers db = do
  chan1 <- subscribeEbNotifications db
  chan2 <- subscribeEbNotifications db
  chan3 <- subscribeEbNotifications db
  let point = mkTestPoint (SlotNo 1) 1
      eb = mkTestEb 5
  withLeiosDb db $ \con -> do
    leiosDbInsertEbPoint con point (leiosEbBytesSize eb)
    leiosDbInsertEbBody con point eb
  -- All subscribers should receive the notification
  notif1 <- atomically $ readTChan chan1
  notif2 <- atomically $ readTChan chan2
  notif3 <- atomically $ readTChan chan3
  assertOfferBlock point notif1
  assertOfferBlock point notif2
  assertOfferBlock point notif3

-- | Test that the notification contains the correct AcquiredEb data.
test_correctData :: LeiosDbHandle IO -> IO ()
test_correctData db = do
  chan <- subscribeEbNotifications db
  let point = mkTestPoint (SlotNo 1) 1
      eb = mkTestEb 10
      expectedSize = leiosEbBytesSize eb
  withLeiosDb db $ \con -> do
    leiosDbInsertEbPoint con point (leiosEbBytesSize eb)
    leiosDbInsertEbBody con point eb
  notification <- atomically $ readTChan chan
  case notification of
    AcquiredEb notifPoint notifSize -> do
      notifPoint.pointSlotNo @?= point.pointSlotNo
      notifPoint.pointEbHash @?= point.pointEbHash
      notifSize @?= expectedSize
    AcquiredEbTxs _ ->
      assertFailure "expected AcquiredEb, got AcquiredEbTxs"

-- | Test that a subscriber who subscribes after an insertion does not receive
-- the past notification.
test_lateSubscriber :: LeiosDbHandle IO -> IO ()
test_lateSubscriber db = do
  -- Insert before subscribing
  let point1 = mkTestPoint (SlotNo 1) 1
      eb1 = mkTestEb 2
  withLeiosDb db $ \con -> do
    leiosDbInsertEbPoint con point1 (leiosEbBytesSize eb1)
    leiosDbInsertEbBody con point1 eb1
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
  withLeiosDb db $ \con -> do
    leiosDbInsertEbPoint con point2 (leiosEbBytesSize eb2)
    leiosDbInsertEbBody con point2 eb2
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
  -- Insert all (point then body for each)
  withLeiosDb db $ \con ->
    forM_ (zip points ebs) $ \(point, eb) -> do
      leiosDbInsertEbPoint con point (leiosEbBytesSize eb)
      leiosDbInsertEbBody con point eb
  -- Read all notifications and verify order
  notifications <- replicateM 5 (atomically $ readTChan chan)
  mapM_
    (uncurry assertOfferBlock)
    (zip points notifications)

-- | Test that no AcquiredEbTxs notification is produced when only some
-- transactions have been inserted via leiosDbInsertTxs.
test_noOfferBlockTxsBeforeComplete :: LeiosDbHandle IO -> IO ()
test_noOfferBlockTxsBeforeComplete db = do
  chan <- subscribeEbNotifications db
  let point = mkTestPoint (SlotNo 1) 1
      eb = mkTestEb 3 -- 3 transactions
      ebTxList = V.toList (leiosEbTxs eb)
  withLeiosDb db $ \con -> do
    leiosDbInsertEbPoint con point (leiosEbBytesSize eb)
    leiosDbInsertEbBody con point eb
    -- Consume the LeiosOfferBlock notification
    _ <- atomically $ readTChan chan
    -- Insert only 2 of 3 txs (by txHash)
    let txsToInsert =
          [ (txHash, BS.pack [fromIntegral i, 1, 2, 3])
          | (i, (txHash, _size)) <- zip [0 :: Int, 1] ebTxList
          ]
    _ <- leiosDbInsertTxs con txsToInsert
    -- No LeiosOfferBlockTxs notification should be available
    maybeNotif <- atomically $ tryReadTChan chan
    case maybeNotif of
      Nothing -> pure ()
      Just _ -> assertFailure "should not notify before all txs are inserted"

-- | Test that a AcquiredEbTxs notification is produced when all
-- transactions are inserted via leiosDbInsertTxs.
test_offerBlockTxs :: LeiosDbHandle IO -> IO ()
test_offerBlockTxs db = do
  chan <- subscribeEbNotifications db
  let point = mkTestPoint (SlotNo 1) 1
      eb = mkTestEb 3 -- 3 transactions
      ebTxList = V.toList (leiosEbTxs eb)
  withLeiosDb db $ \con -> do
    -- Insert the EB (point then body)
    leiosDbInsertEbPoint con point (leiosEbBytesSize eb)
    leiosDbInsertEbBody con point eb
    -- Consume the LeiosOfferBlock notification
    _ <- atomically $ readTChan chan
    -- Insert all txs (by txHash)
    let txsToInsert =
          [ (txHash, BS.pack [fromIntegral i, 1, 2, 3])
          | (i, (txHash, _size)) <- zip [0 :: Int ..] ebTxList
          ]
    _ <- leiosDbInsertTxs con txsToInsert
    -- FIXME: blocks forever if impl not working
    notification <- atomically $ readTChan chan
    assertOfferBlockTxs point notification

-- | Test that completed EBs are not re-notified when subsequent unrelated
-- transactions are inserted.
test_noReNotifyCompletedEbs :: LeiosDbHandle IO -> IO ()
test_noReNotifyCompletedEbs db = do
  chan <- subscribeEbNotifications db
  let point = mkTestPoint (SlotNo 1) 1
      eb = mkTestEb 2
      ebTxList = V.toList (leiosEbTxs eb)
  withLeiosDb db $ \con -> do
    -- Insert and complete the EB
    leiosDbInsertEbPoint con point (leiosEbBytesSize eb)
    leiosDbInsertEbBody con point eb
    -- Consume the AcquiredEb notification
    acquiredEb <- atomically $ tryReadTChan chan
    case acquiredEb of
      Just (AcquiredEb{}) -> pure ()
      _ -> assertFailure "expected AcquiredEb notification"
    let txsToInsert = [(txHash, maxTxBytesZero) | (txHash, _) <- ebTxList]
    _ <- leiosDbInsertTxs con txsToInsert
    -- Consume the AcquiredEbTxs notification
    acquiredTxs <- atomically $ tryReadTChan chan
    case acquiredTxs of
      Just (AcquiredEbTxs p) -> p @?= point
      _ -> assertFailure "expected AcquiredEbTxs notification"
    -- Insert an unrelated tx
    _ <- leiosDbInsertTxs con [(mkTestTxHash 99, maxTxBytesZero)]
    -- No re-notification should occur for the already-completed EB
    maybeNotif <- atomically $ tryReadTChan chan
    case maybeNotif of
      Nothing -> pure ()
      Just _ -> assertFailure "completed EB should not be re-notified"

-- * Property tests for queryFetchWork

-- | Property: empty database returns empty fetch work.
prop_fetchWorkEmpty :: DbImpl -> Property
prop_fetchWorkEmpty impl =
  ioProperty $ withFreshDb impl $ \db -> withLeiosDb db $ \con -> do
    (work, queryTime) <- timed $ leiosDbQueryFetchWork con
    pure $
      conjoin
        [ missingEbBodies work === Map.empty
            & counterexample "Expected no missing EB bodies"
        , missingEbTxs work === Map.empty
            & counterexample "Expected no missing TXs"
        ]
        & tabulate "queryFetchWork (empty)" [timeBucket queryTime]

-- | Property: EBs with points but no bodies are reported as missing.
prop_fetchWorkMissingBodies :: DbImpl -> Property
prop_fetchWorkMissingBodies impl =
  forAllShrinkShow (chooseInt (1, 10)) shrink show $ \count ->
    forAll (replicateM count genPoint) $ \points ->
      ioProperty $
        withFreshDb impl $ \db ->
          withLeiosDb db $ \con -> do
            -- Insert points without bodies
            forM_ points $ \p -> leiosDbInsertEbPoint con p 1000
            (work, queryTime) <- timed $ leiosDbQueryFetchWork con
            let expectedMap = Map.fromList [(p, 1000) | p <- points]
            pure $
              missingEbBodies work === expectedMap
                & counterexample ("Expected: " ++ show expectedMap)
                & counterexample ("Got: " ++ show (missingEbBodies work))
                & tabulate "queryFetchWork (missing bodies)" [timeBucket queryTime]
                & tabulate "numPoints" [magnitudeBucket count]

-- | Property: EBs with bodies but missing TXs are reported.
prop_fetchWorkMissingTxs :: DbImpl -> Property
prop_fetchWorkMissingTxs impl =
  forAllShrinkShow (chooseInt (1, 20)) shrink show $ \numTxs ->
    forAll (genPointAndEb numTxs) $ \(point, eb) ->
      ioProperty $
        withFreshDb impl $ \db ->
          withLeiosDb db $ \con -> do
            -- Insert point and body, but no txs
            leiosDbInsertEbPoint con point (leiosEbBytesSize eb)
            leiosDbInsertEbBody con point eb
            (work, queryTime) <- timed $ leiosDbQueryFetchWork con
            let expectedTxCount = numTxs
            pure $
              conjoin
                [ -- Should have no missing bodies (we inserted the body)
                  missingEbBodies work === Map.empty
                    & counterexample "Should have no missing bodies"
                , -- Should have the EB with missing TXs
                  Map.size (missingEbTxs work) === 1
                    & counterexample ("Expected 1 EB with missing TXs, got: " ++ show (Map.size $ missingEbTxs work))
                , -- The missing TX count should match
                  case Map.toList (missingEbTxs work) of
                    [(_, txs)] ->
                      length txs === expectedTxCount
                        & counterexample ("Expected " ++ show expectedTxCount ++ " missing TXs, got: " ++ show (length txs))
                    _ -> False & counterexample "Unexpected missing TXs structure"
                ]
                & tabulate "queryFetchWork (missing txs)" [timeBucket queryTime]
                & tabulate "numTxs" [magnitudeBucket numTxs]

-- | Property: EBs with all TXs present have no missing TXs.
prop_fetchWorkCompleteTxs :: DbImpl -> Property
prop_fetchWorkCompleteTxs impl =
  forAllShrinkShow (chooseInt (1, 20)) shrink show $ \numTxs ->
    forAll (genPointAndEb numTxs) $ \(point, eb) ->
      forAllBlind genTxBytes $ \baseTxBytes ->
        ioProperty $
          withFreshDb impl $ \db ->
            withLeiosDb db $ \con -> do
              -- Insert point, body, and all txs
              leiosDbInsertEbPoint con point (leiosEbBytesSize eb)
              leiosDbInsertEbBody con point eb
              let ebTxList = V.toList (leiosEbTxs eb)
                  txsToInsert =
                    [ (txHash, baseTxBytes)
                    | (txHash, _size) <- ebTxList
                    ]
              _ <- leiosDbInsertTxs con txsToInsert
              (work, queryTime) <- timed $ leiosDbQueryFetchWork con
              pure $
                conjoin
                  [ -- Should have no missing bodies
                    missingEbBodies work === Map.empty
                      & counterexample "Should have no missing bodies"
                  , -- Should have no missing TXs
                    missingEbTxs work === Map.empty
                      & counterexample ("Should have no missing TXs, got: " ++ show (missingEbTxs work))
                  ]
                  & tabulate "queryFetchWork (complete)" [timeBucket queryTime]
                  & tabulate "numTxs" [magnitudeBucket numTxs]

-- * Test utilities

-- | Assert that a notification is AcquiredEb with the expected point.
assertOfferBlock :: LeiosPoint -> LeiosEbNotification -> IO ()
assertOfferBlock expectedPoint = \case
  AcquiredEb actualPoint _ ->
    actualPoint @?= expectedPoint
  AcquiredEbTxs _ ->
    assertFailure "expected AcquiredEb, got AcquiredEbTxs"

-- | Assert that a notification is AcquiredEbTxs with the expected point.
assertOfferBlockTxs :: LeiosPoint -> LeiosEbNotification -> IO ()
assertOfferBlockTxs expectedPoint = \case
  AcquiredEbTxs actualPoint ->
    actualPoint @?= expectedPoint
  AcquiredEb _ _ ->
    assertFailure "expected AcquiredEbTxs, got AcquiredEb"

-- * Property tests for filterHaveEbBodies

-- | Property: filtering empty list returns empty list.
prop_filterEbBodiesEmpty :: DbImpl -> Property
prop_filterEbBodiesEmpty impl =
  ioProperty $ withFreshDb impl $ \db -> withLeiosDb db $ \con -> do
    result <- leiosDbFilterMissingEbBodies con []
    pure $ result === []

-- | Property: filter returns exactly the EBs whose bodies are missing.
prop_filterEbBodiesCorrect :: DbImpl -> Property
prop_filterEbBodiesCorrect impl =
  forAll (chooseInt (1, 20)) $ \numEbs ->
    forAll (replicateM numEbs (genPointAndEb 5)) $ \pointsAndEbs ->
      forAllBlind (sublistOf pointsAndEbs) $ \toInsert ->
        ioProperty $
          withFreshDb impl $ \db ->
            withLeiosDb db $ \con -> do
              -- Insert some EBs (those in toInsert will have bodies)
              forM_ toInsert $ \(point, eb) -> do
                leiosDbInsertEbPoint con point (leiosEbBytesSize eb)
                leiosDbInsertEbBody con point eb
              -- Also insert points without bodies for the rest
              let withoutBodies = filter (`notElem` toInsert) pointsAndEbs
              forM_ withoutBodies $ \(point, eb) ->
                leiosDbInsertEbPoint con point (leiosEbBytesSize eb)
              -- Filter should return the ones WITHOUT bodies
              let allPoints = [p | (p, _) <- pointsAndEbs]
                  expectedMissing = [p | (p, _) <- withoutBodies]
              (result, filterTime) <- timed $ leiosDbFilterMissingEbBodies con allPoints
              pure $
                conjoin
                  [ length result === length expectedMissing
                  , all (`elem` expectedMissing) result === True
                  , all (`elem` result) expectedMissing === True
                  ]
                  & tabulate "filterMissingEbBodies" [timeBucket filterTime]
                  & tabulate "numEbs" [magnitudeBucket numEbs]

-- * Property tests for filterMissingTxs

-- | Property: filtering empty list returns empty list.
prop_filterTxsEmpty :: DbImpl -> Property
prop_filterTxsEmpty impl =
  ioProperty $ withFreshDb impl $ \db -> withLeiosDb db $ \con -> do
    result <- leiosDbFilterMissingTxs con []
    pure $ result === []

-- | Property: filter returns exactly the TXs we do NOT have.
prop_filterTxsCorrect :: DbImpl -> Property
prop_filterTxsCorrect impl =
  forAll (chooseInt (1, 50)) $ \numTxs ->
    forAllBlind (replicateM numTxs genTxHash) $ \txHashes ->
      forAllBlind (sublistOf txHashes) $ \toInsert ->
        ioProperty $ withFreshDb impl $ \db -> withLeiosDb db $ \con -> do
          -- Insert some TXs
          forM_ toInsert $ \txHash ->
            leiosDbInsertTxs con [(txHash, maxTxBytesZero)]
          -- Filter should return the ones NOT inserted
          let expectedMissing = filter (`notElem` toInsert) txHashes
          (result, filterTime) <- timed $ leiosDbFilterMissingTxs con txHashes
          pure $
            conjoin
              [ length result === length expectedMissing
              , all (`elem` expectedMissing) result === True
              , all (`elem` result) expectedMissing === True
              ]
              & tabulate "filterMissingTxs" [timeBucket filterTime]
              & tabulate "numTxs" [magnitudeBucket numTxs]

-- * Property tests for queryCompletedEbByPoint

-- | Property: complete EB (all txs inserted) returns Just with correct tx data.
prop_completedEbComplete :: DbImpl -> Property
prop_completedEbComplete impl =
  forAllShrinkShow (chooseInt (1, 20)) shrink show $ \numTxs ->
    forAllBlind (genPointAndEb numTxs) $ \(point, eb) ->
      forAllBlind genTxBytes $ \txBytes ->
        ioProperty $ withFreshDb impl $ \db -> withLeiosDb db $ \con -> do
          leiosDbInsertEbPoint con point (leiosEbBytesSize eb)
          leiosDbInsertEbBody con point eb
          let ebTxList = V.toList (leiosEbTxs eb)
              txsToInsert = [(txHash, txBytes) | (txHash, _size) <- ebTxList]
          _ <- leiosDbInsertTxs con txsToInsert
          (result, queryTime) <- timed $ leiosDbQueryCompletedEbByPoint con point
          let expectedHashes = map fst txsToInsert
              check = case result of
                Nothing ->
                  False & counterexample "Expected Just, got Nothing"
                Just txData ->
                  conjoin
                    [ length txData === numTxs
                        & counterexample "Wrong number of txs"
                    , all (`elem` map fst txData) expectedHashes
                        & counterexample "Missing expected tx hashes"
                    , all (== txBytes) (map snd txData)
                        & counterexample "Wrong tx bytes"
                    ]
          pure $
            check
              & tabulate "queryCompletedEbByPoint (complete)" [timeBucket queryTime]
              & tabulate "numTxs" [magnitudeBucket numTxs]

-- | Property: EB with body but no txs returns Nothing.
prop_completedEbMissingTxs :: DbImpl -> Property
prop_completedEbMissingTxs impl =
  forAllShrinkShow (chooseInt (1, 20)) shrink show $ \numTxs ->
    forAllBlind (genPointAndEb numTxs) $ \(point, eb) ->
      ioProperty $ withFreshDb impl $ \db -> withLeiosDb db $ \con -> do
        leiosDbInsertEbPoint con point (leiosEbBytesSize eb)
        leiosDbInsertEbBody con point eb
        (result, queryTime) <- timed $ leiosDbQueryCompletedEbByPoint con point
        pure $
          result === Nothing
            & counterexample "Expected Nothing when no txs are present"
            & tabulate "queryCompletedEbByPoint (no txs)" [timeBucket queryTime]
            & tabulate "numTxs" [magnitudeBucket numTxs]

-- | Property: EB with partial txs (at least one missing) returns Nothing.
prop_completedEbPartialTxs :: DbImpl -> Property
prop_completedEbPartialTxs impl =
  forAllShrinkShow (chooseInt (2, 20)) shrink show $ \numTxs ->
    forAllBlind (genPointAndEb numTxs) $ \(point, eb) ->
      forAllBlind genTxBytes $ \txBytes ->
        ioProperty $ withFreshDb impl $ \db -> withLeiosDb db $ \con -> do
          leiosDbInsertEbPoint con point (leiosEbBytesSize eb)
          leiosDbInsertEbBody con point eb
          -- Insert only the first half of txs, leaving at least one missing
          let ebTxList = V.toList (leiosEbTxs eb)
              partialTxs = take (numTxs `div` 2) ebTxList
              txsToInsert = [(txHash, txBytes) | (txHash, _size) <- partialTxs]
          _ <- leiosDbInsertTxs con txsToInsert
          (result, queryTime) <- timed $ leiosDbQueryCompletedEbByPoint con point
          pure $
            result === Nothing
              & counterexample
                ( "Expected Nothing with "
                    ++ show (length txsToInsert)
                    ++ "/"
                    ++ show numTxs
                    ++ " txs present"
                )
              & tabulate "queryCompletedEbByPoint (partial txs)" [timeBucket queryTime]
              & tabulate "numTxs" [magnitudeBucket numTxs]

-- | Property: EB with only a point announced (no body) returns Nothing.
prop_completedEbNoBody :: DbImpl -> Property
prop_completedEbNoBody impl =
  forAll genPoint $ \point ->
    ioProperty $ withFreshDb impl $ \db -> withLeiosDb db $ \con -> do
      leiosDbInsertEbPoint con point 1000
      (result, queryTime) <- timed $ leiosDbQueryCompletedEbByPoint con point
      pure $
        result === Nothing
          & counterexample "Expected Nothing for EB with no body inserted"
          & tabulate "queryCompletedEbByPoint (no body)" [timeBucket queryTime]
