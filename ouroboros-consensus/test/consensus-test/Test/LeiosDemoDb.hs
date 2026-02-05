{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Tests for the LeiosDemoDb notification mechanism.
--
-- These tests verify that subscribers to 'subscribeEbNotifications' receive
-- 'LeiosNotification' events when 'leiosDbInsertEbBody' or 'leiosDbUpdateEbTx'
-- is called.
module Test.LeiosDemoDb (tests) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.Class.MonadSTM.Strict (atomically, readTChan, tryReadTChan)
import Control.Monad (forM_, replicateM, when)
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
  , leiosEBMaxClosureSize
  , leiosEBMaxSize
  , leiosEbBytesSize
  , maxTxsPerEb
  )
import Ouroboros.Consensus.Ledger.SupportsMempool (ByteSize32 (..))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory, withSystemTempDirectory)
import Test.QuickCheck
  ( Gen
  , Property
  , choose
  , counterexample
  , forAllShrinkBlind
  , ioProperty
  , listOf
  , once
  , vectorOf
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "LeiosDemoDb" . forEachImplementation $ \dbTest dbPropTest ->
    [ dbTest "single subscriber" test_singleSubscriber
    , dbTest "multiple subscribers" test_multipleSubscribers
    , dbTest "correct data" test_correctData
    , dbTest "late subscriber" test_lateSubscriber
    , dbTest "multiple notifications" test_multipleNotifications
    , dbTest "no offerBlockTxs before last update" test_noOfferBlockTxsBeforeComplete
    , dbTest "offerBlockTxs on last update" test_offerBlockTxs
    , dbPropTest "reasonable update performance" prop_reasonableUpdatePerformance
    ]

-- | Run test cases for each database implementation (InMemory and SQLite).
-- Provides two test runner functions: one for unit tests (with a pre-built
-- handle) and one for property tests (with a DB creation action).
forEachImplementation ::
  ( (String -> (LeiosDbHandle IO -> IO ()) -> TestTree) ->
    (String -> (IO (LeiosDbHandle IO) -> Property) -> TestTree) ->
    [TestTree]
  ) ->
  [TestTree]
forEachImplementation mkTests =
  [ testGroup "InMemory" $ mkTests inMemoryTest inMemoryPropTest
  , testGroup "SQLite" $ mkTests sqliteTest sqlitePropTest
  ]
 where
  inMemoryTest name test =
    testCase name $
      newInMemoryLeiosDb >>= test

  inMemoryPropTest name prop =
    testProperty name $ prop newInMemoryLeiosDb

  sqliteTest name test =
    testCase name $
      withTestSqliteDb test

  sqlitePropTest name prop =
    testProperty name $ prop mkTestSqliteDb

-- | Run a test with a temporary SQLite database (bracket-style).
withTestSqliteDb :: (LeiosDbHandle IO -> IO a) -> IO a
withTestSqliteDb action =
  withSystemTempDirectory "leios-test" $ \tmpDir -> do
    let dbPath = tmpDir <> "/test.db"
    db <- newLeiosDbConnectionIO dbPath
    action db

-- | Create a temporary SQLite database (no automatic cleanup).
-- Suitable for property tests where the DB must outlive the creation action.
mkTestSqliteDb :: IO (LeiosDbHandle IO)
mkTestSqliteDb = do
  sysTmp <- getCanonicalTemporaryDirectory
  tmpDir <- createTempDirectory sysTmp "leios-test"
  newLeiosDbConnectionIO (tmpDir <> "/test.db")

-- * Test fixtures

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

-- * Test cases (interface-level, work with any LeiosDbHandle)

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
  let point = mkTestPoint (SlotNo 42) 42
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
  let point = mkTestPoint (SlotNo 100) 55
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

-- * Property tests

-- | Maximum transaction size as on mainnet.
maxTxByteSize :: Int
maxTxByteSize = 16_000_000

-- | Generate a performance test scenario: a list of (numTxs, completionRatio)
-- for arbitrary background EBs, plus a target EB size and the offset to update.
genPerfScenario :: Gen ([(Int, Double)], Int, Int)
genPerfScenario = do
  -- TODO: scale or size
  bgEbs <- listOf $ do
    numTxs <- choose (1, maxTxsPerEb)
    ratio <- choose (0.0 :: Double, 1.0)
    pure (numTxs, ratio)
  targetSize <- choose (1, maxTxsPerEb)
  targetOffset <- choose (0, targetSize - 1)
  pure (bgEbs, targetSize, targetOffset)

-- | Property: leiosDbUpdateEbTx completes within 1ms even with 10k EB bodies
-- in the database with randomly completed/sparse transaction closures.
prop_reasonableUpdatePerformance :: IO (LeiosDbHandle IO) -> Property
prop_reasonableUpdatePerformance mkDb =
  once $
    forAllShrinkBlind genPerfScenario (const []) $ \(bgEbs, targetSize, targetOffset) ->
      ioProperty $ do
        db <- mkDb
        -- Prime the database with background EB bodies
        putStrLn $ "Priming database with " <> show (length bgEbs) <> " ebs"
        forM_ (zip [1 :: Int ..] bgEbs) $ \(i, (numTxs, completionRatio)) -> do
          let point = mkTestPoint (SlotNo $ fromIntegral i) (fromIntegral i)
              eb = mkTestEb numTxs
          leiosDbInsertEbBody db point eb
          -- Deterministically complete the first N txs based on the ratio
          let numCompleted = floor (completionRatio * fromIntegral numTxs)
          forM_ [0 .. numCompleted - 1] $ \j ->
            leiosDbUpdateEbTx db point.pointEbHash j (BS.replicate maxTxByteSize 0)
        -- Create the target EB and fill all but the target offset
        putStrLn $ "Priming target eb"
        let targetPoint = mkTestPoint (SlotNo 99999) 99999
            targetEb = mkTestEb targetSize
        leiosDbInsertEbBody db targetPoint targetEb
        forM_ [0 .. targetSize - 1] $ \j ->
          when (j /= targetOffset) $
            leiosDbUpdateEbTx db targetPoint.pointEbHash j (BS.replicate maxTxByteSize 0)
        -- Time the final update
        -- TODO: use monotonic clock
        start <- getCurrentTime
        leiosDbUpdateEbTx db targetPoint.pointEbHash targetOffset (BS.replicate maxTxByteSize 0)
        end <- getCurrentTime
        let elapsedMs = realToFrac (diffUTCTime end start) * 1000 :: Double
        pure $ counterexample ("leiosDbUpdateEbTx took " ++ show elapsedMs ++ "ms") (elapsedMs < 1.0)

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
