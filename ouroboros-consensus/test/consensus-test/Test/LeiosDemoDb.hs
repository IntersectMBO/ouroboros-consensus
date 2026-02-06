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
import Control.Monad (forM_, replicateM)
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
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory, withSystemTempDirectory)
import Test.QuickCheck
  ( Property
  , chooseInt
  , counterexample
  , forAllShrinkShow
  , ioProperty
  , withMaxSuccess
  , within
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "LeiosDemoDb" . forEachImplementation $ \dbTest dbPropTest ->
    [ testGroup
        "notifications"
        [ dbTest "single subscriber" test_singleSubscriber
        , dbTest "multiple subscribers" test_multipleSubscribers
        , dbTest "correct data" test_correctData
        , dbTest "late subscriber" test_lateSubscriber
        , dbTest "multiple notifications" test_multipleNotifications
        , dbTest "no offerBlockTxs before last update" test_noOfferBlockTxsBeforeComplete
        , dbTest "offerBlockTxs on last update" test_offerBlockTxs
        ]
    , dbPropTest "reasonable write performance" prop_reasonableWritePerformance
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
  print tmpDir
  -- FIXME: this leaks temp files
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

-- | Property: leiosDbUpdateEbTx completes within 1ms at various database sizes.
-- Generates a random number of EBs (0 to 10000) with quadratic shrinking.
--
-- TODO: Also test with varying EB closure states (partial completions).
prop_reasonableWritePerformance :: IO (LeiosDbHandle IO) -> Property
prop_reasonableWritePerformance mkDb =
  withMaxSuccess 10 $
    forAllShrinkShow (chooseInt (0, maxNumEbs)) shrinkQuadratic show $ \numEbs ->
      -- Allow up to 60 seconds for full test including priming
      within 60_000_000 $
        ioProperty $ do
          db <- mkDb
          -- Prime the database (each EB has 100 txs, all complete)
          forM_ [1 .. numEbs] $ \i -> do
            let point = mkTestPoint (SlotNo $ fromIntegral i) (fromIntegral i)
                eb = mkTestEb numTxsPerEb
            leiosDbInsertEbBody db point eb
            forM_ [0 .. numTxsPerEb - 1] $ \j ->
              leiosDbUpdateEbTx db point.pointEbHash j (BS.replicate maxTxByteSize 1)
          -- Create target EB with 1 tx and time the update
          let targetPoint = mkTestPoint (SlotNo 100_000) 100_000
              targetEb = mkTestEb 1
          leiosDbInsertEbBody db targetPoint targetEb
          -- TODO: use monotonic clock
          start <- getCurrentTime
          leiosDbUpdateEbTx db targetPoint.pointEbHash 0 (BS.replicate maxTxByteSize 0)
          end <- getCurrentTime
          let elapsedMs = realToFrac (diffUTCTime end start) * 1000 :: Double
          pure $
            counterexample
              ("leiosDbUpdateEbTx took " ++ show elapsedMs ++ "ms with " ++ show numEbs ++ " EBs")
              (elapsedMs < 1.0)
 where
  maxNumEbs = 10_000
  numTxsPerEb = 100

  -- Shrink by square root steps: 10000 -> 100 -> 10 -> 3 -> 1 -> 0
  shrinkQuadratic :: Int -> [Int]
  shrinkQuadratic 0 = []
  shrinkQuadratic n = [0, isqrt n]
   where
    isqrt = floor . sqrt . (fromIntegral :: Int -> Double)

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
