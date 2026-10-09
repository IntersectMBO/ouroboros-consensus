{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the LeiosDemoDb interface.
--
-- These tests verify the semantics of the LeiosDbHandle operations for both
-- InMemory and SQLite implementations. Each test case uses a fresh database
-- to ensure isolation and serve as an (too optimistic) performance baseline.
module Test.LeiosDemoDb (module Test.LeiosDemoDb) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTChan
  , atomically
  , readTChan
  , tryReadTChan
  )
import Control.Concurrent.MVar (newEmptyMVar, readMVar, takeMVar, tryPutMVar)
import Control.DeepSeq (force)
import Control.Exception
  ( IOException
  , SomeException
  , bracket
  , catch
  , displayException
  , fromException
  , throwIO
  , try
  )
import Control.Monad (forM, forM_, replicateM, void)
import Control.Monad.Class.MonadTime.SI (diffTime, getMonotonicTime)
import Control.Tracer (Tracer (..), emit, nullTracer)
import qualified Data.ByteString as BS
import Data.Function ((&))
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (DiffTime)
import qualified Data.Vector.Strict as V
import LeiosDemoDb
  ( CompletedEbs
  , LeiosDbHandle (..)
  , LeiosDbReader (..)
  , LeiosDbWriter (..)
  , LeiosEbNotification (..)
  , Promise (..)
  , TraceLeiosDb (..)
  , deleteDanglingTxs
  , newLeiosDBInMemory
  , newLeiosDBSQLite
  , truncateLeiosDbAfterSlot
  , withLeiosDBSQLite
  , withReader
  , withWriter
  )
import LeiosDemoException (LeiosDbException (LeiosDbWriteException, writeFailure, writeJob))
import LeiosDemoTypes
  ( BytesSize
  , EbHash (..)
  , LeiosEb (..)
  , LeiosPoint (..)
  , RbHash (..)
  , TxHash (..)
  , TxLocation (..)
  , encodeLeiosEbSize
  , leiosEbTxs
  )
import System.Directory (removeDirectoryRecursive)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Mem (performMajorGC)
import qualified System.Timeout as Timeout
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
import Test.Tasty.HUnit
  ( Assertion
  , assertBool
  , assertEqual
  , assertFailure
  , testCase
  , (@?=)
  )
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.LeiosHash (unsafeEbHashFromBytes, unsafeTxHashFromBytes)

tests :: TestTree
tests =
  testGroup "LeiosDemoDb" $
    forEachImplementation mkTestGroups
      <> [ testGroup
             "truncateLeiosDbAfterSlot"
             [ testCase "drops the EBs announced after the slot, with their bodies" $
                 withFreshSQLiteFile test_truncateDropsEbsAfterSlot
             ]
         , testGroup
             "deleteDanglingTxs"
             [ testCase "keeps the txs an EB references" $
                 withFreshSQLiteFile test_deleteDanglingTxs
             ]
         , testGroup
             "close"
             [ testCase "returns while an EB stays pinned" test_closeWithPinnedEb
             ]
         , testGroup
             "writer"
             [ testCase "tells the awaiter when a job throws" test_awaiterHearsAFailedJob
             ]
         , testGroup
             "orphanhood"
             [ testCase "dropping the handle does not kill its owner" test_droppingTheHandleSpreadsNoException
             ]
         , -- InMemory only: on SQLite a failed ingest write also kills the
           -- writer (see 'startWriter'), which is linked to the test thread.
           testCase "InMemory: inserting a body for an unregistered point fails, as on SQLite" $
             withFreshDb InMemory test_ebBodyWithoutPointFails
         ]

-- | Database creation strategy for different implementations.
data DbImpl
  = InMemory
  | SQLite

-- | A reader and a writer onto the same database -- what every caller outside
-- these tests now holds, since reads and writes no longer share a connection.
data RW = RW
  { rwReader :: LeiosDbReader IO
  , rwWriter :: LeiosDbWriter IO
  }

withRW :: LeiosDbHandle IO -> (RW -> IO a) -> IO a
withRW db k = withReader db $ \r -> withWriter db $ \w -> k (RW r w)

-- Writes are awaited, so a read that follows one sees it.
rwInsertEbPoint :: RW -> LeiosPoint -> BytesSize -> IO ()
rwInsertEbPoint rw point sz = void $ await =<< writeEbPoint (rwWriter rw) point sz

rwInsertEbBody :: RW -> LeiosPoint -> LeiosEb -> IO CompletedEbs
rwInsertEbBody rw point eb = fst <$> (await =<< writeEbBody (rwWriter rw) point eb [])

rwInsertTxs :: RW -> LeiosPoint -> [(Int, BS.ByteString)] -> IO CompletedEbs
rwInsertTxs rw point offBytes = await =<< writeTxs (rwWriter rw) point offBytes

rwScanEbPoints :: RW -> IO [(SlotNo, EbHash)]
rwScanEbPoints = scanEbPoints . rwReader

rwLookupEbBody :: RW -> EbHash -> IO [(TxHash, BytesSize)]
rwLookupEbBody = lookupEbBody . rwReader

rwLookupTrustedEbClosure :: RW -> EbHash -> IO (Maybe [(TxHash, BS.ByteString)])
rwLookupTrustedEbClosure = lookupTrustedEbClosure . rwReader

rwBatchRetrieveTxs :: RW -> EbHash -> [Int] -> IO [(Int, TxHash, Maybe BS.ByteString)]
rwBatchRetrieveTxs = batchRetrieveTxs . rwReader

-- | Create a fresh database and run an action with it.
-- Ensures proper cleanup for SQLite databases.
withFreshDb :: DbImpl -> (LeiosDbHandle IO -> IO a) -> IO a
withFreshDb InMemory action =
  newLeiosDBInMemory >>= action
withFreshDb SQLite action = withFreshSQLiteDb action

withFreshSQLiteDb :: (LeiosDbHandle IO -> IO a) -> IO a
withFreshSQLiteDb action = withFreshSQLiteFile (\_vol _imm -> action)

-- | Create a fresh SQLite database and hand its paths to the action, which
-- 'truncateLeiosDbAfterSlot' needs. The database is torn down -- writes
-- flushed, threads stopped, connections closed -- before the directory is
-- removed; a still-open connection makes the removal flaky (a hard failure
-- on Windows, where deleting an open WAL is a sharing violation).
withFreshSQLiteFile :: (FilePath -> FilePath -> LeiosDbHandle IO -> IO a) -> IO a
withFreshSQLiteFile action = do
  sysTmp <- getCanonicalTemporaryDirectory
  bracket
    (createTempDirectory sysTmp "leios-test")
    removeDirectoryRecursive
    ( \tmpDir -> do
        let volDbPath = tmpDir <> "/test.vol.db"
            immDbPath = tmpDir <> "/test.imm.db"
        withLeiosDBSQLite nullTracer volDbPath immDbPath $
          action volDbPath immDbPath
    )

-- | Run tests for each database implementation.
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
      , testCase "offerBlockTxs when body arrives after all txs" $
          withFreshDb impl test_offerBlockTxsWhenBodyArrivesAfterTxs
      , testCase "cross-EB fill copies local bytes and can complete at body time" $
          withFreshDb impl test_crossEbFill
      , testCase "no re-notification of completed EBs" $ withFreshDb impl test_noReNotifyCompletedEbs
      , testCase "no re-notification when re-inserting an EB's own tx" $
          withFreshDb impl test_noReNotifyOnRelatedTxReinsert
      , testCase "same EB hash at multiple slots notifies each completion" $
          withFreshDb impl test_multipleSlotsSameHash
      ]
  , testGroup
      "misstated transaction sizes"
      [ testCase "a body claiming the wrong size never completes" $
          withFreshDb impl test_misstatedSizeNeverCompletes
      ]
  , testGroup
      "lookupTrustedEbClosure"
      [ testProperty "complete EB returns Just with tx data" $ prop_completedEbComplete impl
      , testProperty "no txs returns Nothing" $ prop_completedEbMissingTxs impl
      , testProperty "partial txs returns Nothing" $ prop_completedEbPartialTxs impl
      , testProperty "no body returns Nothing" $ prop_completedEbNoBody impl
      ]
  ]

-- | An endorser block stating a size that is not its transaction's own never
-- has its closure called complete, so no @AcquiredEbTxs@ is emitted for it.
--
-- Runs against both backends. The node must not behave differently depending
-- on which one it was built with, and the two implement this by different
-- means: the row pre-allocated at the declared size and a @length@ guard on
-- the fill, against the same check in the in-memory accept filter.
--
-- Only the body-first order is covered here. The other order --- holding the
-- transaction before the body lands --- cannot be posed to a single endorser
-- block, because the bytes are owned per endorser block and a write before the
-- body has no row to land on. Posing it needs a second, honest endorser block
-- to hold the transaction, which is what
-- 'Test.Consensus.Leios.RecoveryPath' does against a real node.
test_misstatedSizeNeverCompletes :: LeiosDbHandle IO -> IO ()
test_misstatedSizeNeverCompletes db = do
  chan <- subscribeEbNotifications db
  let point = mkTestPoint (SlotNo 1) 1
      eb = mkTestEb 1
  case V.toList (leiosEbTxs eb) of
    [(_txHash, claimed)] -> withRW db $ \con -> do
      -- One byte longer than the body says. A transaction hash covers its
      -- bytes, so this is a claim no honest endorser block makes.
      let txs = [(0, BS.replicate (fromIntegral claimed + 1) 0)]
      rwInsertEbPoint con point (encodeLeiosEbSize eb)
      void $ rwInsertEbBody con point eb
      void $ rwInsertTxs con point txs
      -- The body's own arrival is still worth announcing; its closure is not.
      notifications <- drainChan chan
      assertBool
        "expected an AcquiredEb for the body"
        (not (null [() | AcquiredEb{} <- notifications]))
      assertEqual
        "the closure of a body that misstates a size must not be announced"
        []
        [p | AcquiredEbTxs p <- notifications]
    other -> assertFailure $ "expected a one-transaction endorser block: " <> show (length other)

-- | Everything on the channel right now.
drainChan :: StrictTChan IO LeiosEbNotification -> IO [LeiosEbNotification]
drainChan chan = go []
 where
  go acc =
    atomically (tryReadTChan chan) >>= \case
      Nothing -> pure (reverse acc)
      Just x -> go (x : acc)

-- * QuickCheck generators

-- | Generate a random EbHash (32 random bytes).
-- With 256 bits of randomness, collisions are practically impossible.
genEbHash :: Gen EbHash
genEbHash = unsafeEbHashFromBytes . BS.pack <$> vector 32

-- | Generate a random RbHash (32 random bytes).
-- With 256 bits of randomness, collisions are practically impossible.
genRbHash :: Gen RbHash
genRbHash = MkRbHash . BS.pack <$> vector 32

-- | Generate a random TxHash (32 random bytes).
genTxHash :: Gen TxHash
genTxHash = unsafeTxHashFromBytes . BS.pack <$> vector 32

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
-- | Bytes of exactly the size the body declares for this offset: the write
-- path rejects anything else ('sql_fill_ebTxBytes' pins the size the row was
-- allocated with).
txBytesFor :: LeiosEb -> Int -> BS.ByteString
txBytesFor eb off = BS.replicate (fromIntegral sz) (fromIntegral off)
 where
  (_h, sz) = leiosEbTxs eb V.! off

-- | Max sized tx (16k) with all zeros.
-- * Test fixtures for unit tests

-- | Create a simple test EbHash from a seed byte.
mkTestEbHash :: Word -> EbHash
mkTestEbHash seed = unsafeEbHashFromBytes $ BS.pack $ replicate 32 (fromIntegral seed)

-- | Create a simple test TxHash from a seed byte.
mkTestTxHash :: Word -> TxHash
mkTestTxHash seed = unsafeTxHashFromBytes $ BS.pack $ replicate 32 (fromIntegral seed)

-- | Create a test LeiosPoint.
mkTestPoint :: SlotNo -> Word -> LeiosPoint
mkTestPoint slot seed = MkLeiosPoint slot (mkTestEbHash seed)

-- | A dummy announcing RB hash for DB tests that don't care about its value.
testRbHash :: RbHash
testRbHash = MkRbHash (BS.replicate 32 7)

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
    ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
      (_, insertTime) <- timed $ rwInsertEbPoint con point 1000
      (points, scanTime) <- timed $ rwScanEbPoints con
      pure $
        (point.pointSlotNo, point.pointEbHash) `elem` points
          & tabulate "insertEbPoint" [timeBucket insertTime]
          & tabulate "scanEbPoints" [timeBucket scanTime]

-- | Property: multiple inserted points all appear in scan results.
prop_pointsAccumulate :: DbImpl -> Property
prop_pointsAccumulate impl =
  forAllShrinkShow (chooseInt (1, 10)) shrink show $ \count ->
    forAll (replicateM count genPoint) $ \points ->
      ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
        insertTimes <- forM points $ \p ->
          snd <$> timed (rwInsertEbPoint con p 1000)
        (scanned, scanTime) <- timed $ rwScanEbPoints con
        let expected = [(p.pointSlotNo, p.pointEbHash) | p <- points]
        pure $
          all (`elem` scanned) expected
            & tabulate "insertEbPoint" [timeBucket $ maximum insertTimes]
            & tabulate "scanEbPoints" [timeBucket scanTime]

-- * Property tests for EBs

-- | Property: inserting an EB body and looking it up returns the correct txs.
prop_ebsInsertThenLookup :: DbImpl -> Property
prop_ebsInsertThenLookup impl =
  forAllShrinkShow (chooseInt (1, 50)) (filter (>= 1) . shrink) show $ \numTxs ->
    forAll (genPointAndEb numTxs) $ \(point, eb) ->
      ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
        let expectedTxs = V.toList (leiosEbTxs eb)
        rwInsertEbPoint con point (encodeLeiosEbSize eb)
        (_, insertTime) <- timed $ rwInsertEbBody con point eb
        (result, lookupTime) <- timed $ rwLookupEbBody con point.pointEbHash
        pure $
          result == expectedTxs
            & counterexample ("Expected: " ++ show expectedTxs ++ "\nGot: " ++ show result)
            & tabulate "insertEbBody" [timeBucket insertTime]
            & tabulate "lookupEbBody" [timeBucket lookupTime]

-- | Property: looking up a non-existent EB returns empty list.
prop_ebsLookupMissing :: DbImpl -> Property
prop_ebsLookupMissing impl =
  forAll genEbHash $ \missingHash ->
    ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
      (result, lookupTime) <- timed $ rwLookupEbBody con missingHash
      pure $
        result === []
          & tabulate "lookupEbBody (missing)" [timeBucket lookupTime]

-- * Property tests for transactions

-- | Property: inserting tx bytes and retrieving them returns the correct data.
-- With the normalized schema, txs are inserted into a global `txs` table by TxHash,
-- and retrieval JOINs with that table.
prop_txsInsertThenRetrieve :: DbImpl -> Property
prop_txsInsertThenRetrieve impl =
  forAllShrinkShow (chooseInt (1, 50)) (filter (>= 1) . shrink) show $ \numTxs ->
    forAllBlind (genPointAndEb numTxs) $ \(point, eb) ->
      forAllBlind (sublistOf [0 .. numTxs - 1]) $ \offsetsToInsert ->
        ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
          -- Insert the EB first (point then body)
          rwInsertEbPoint con point (encodeLeiosEbSize eb)
          void $ rwInsertEbBody con point eb
          -- Get the txHashes from the EB for the offsets we want to insert
          let !txsToInsert =
                force $
                  [(off, txBytesFor eb off) | off <- offsetsToInsert]
          -- Insert the tx bytes for this EB
          insertTime <- snd <$> timed (rwInsertTxs con point txsToInsert)
          -- Retrieve all offsets
          let allOffsets = [0 .. numTxs - 1]
          (results, retrieveTime) <- timed $ rwBatchRetrieveTxs con point.pointEbHash allOffsets
          -- Check that inserted txs have bytes, others don't
          let checkResult (off, _txHash, mBytes) =
                if off `elem` offsetsToInsert
                  then mBytes == Just (txBytesFor eb off)
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
    ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
      (result, retrieveTime) <- timed $ rwBatchRetrieveTxs con missingHash [0, 1, 2]
      pure $
        result === []
          & tabulate "batchRetrieveTxs (missing)" [timeBucket retrieveTime]

-- * Notification tests

-- | Test that a single subscriber receives a notification when rwInsertEbBody is called.
test_singleSubscriber :: LeiosDbHandle IO -> IO ()
test_singleSubscriber db = do
  chan <- subscribeEbNotifications db
  let point = mkTestPoint (SlotNo 1) 1
      eb = mkTestEb 3
  withRW db $ \con -> do
    rwInsertEbPoint con point (encodeLeiosEbSize eb)
    void $ rwInsertEbBody con point eb
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
  withRW db $ \con -> do
    rwInsertEbPoint con point (encodeLeiosEbSize eb)
    void $ rwInsertEbBody con point eb
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
      expectedSize = encodeLeiosEbSize eb
  withRW db $ \con -> do
    rwInsertEbPoint con point (encodeLeiosEbSize eb)
    void $ rwInsertEbBody con point eb
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
  withRW db $ \con -> do
    rwInsertEbPoint con point1 (encodeLeiosEbSize eb1)
    void $ rwInsertEbBody con point1 eb1
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
  withRW db $ \con -> do
    rwInsertEbPoint con point2 (encodeLeiosEbSize eb2)
    void $ rwInsertEbBody con point2 eb2
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
  withRW db $ \con ->
    forM_ (zip points ebs) $ \(point, eb) -> do
      rwInsertEbPoint con point (encodeLeiosEbSize eb)
      void $ rwInsertEbBody con point eb
  -- Read all notifications and verify order
  notifications <- replicateM 5 (atomically $ readTChan chan)
  mapM_
    (uncurry assertOfferBlock)
    (zip points notifications)

-- | Test that no AcquiredEbTxs notification is produced when only some
-- transactions have been inserted via rwInsertTxs.
test_noOfferBlockTxsBeforeComplete :: LeiosDbHandle IO -> IO ()
test_noOfferBlockTxsBeforeComplete db = do
  chan <- subscribeEbNotifications db
  let point = mkTestPoint (SlotNo 1) 1
      eb = mkTestEb 3 -- 3 transactions
      ebTxList = V.toList (leiosEbTxs eb)
  withRW db $ \con -> do
    rwInsertEbPoint con point (encodeLeiosEbSize eb)
    void $ rwInsertEbBody con point eb
    -- Consume the LeiosOfferBlock notification
    _ <- atomically $ readTChan chan
    -- Insert only 2 of 3 txs (by txHash)
    let txsToInsert = [(i, txBytesFor eb i) | (i, _) <- zip [0 :: Int, 1] ebTxList]
    _ <- rwInsertTxs con point txsToInsert
    -- No LeiosOfferBlockTxs notification should be available
    maybeNotif <- atomically $ tryReadTChan chan
    case maybeNotif of
      Nothing -> pure ()
      Just _ -> assertFailure "should not notify before all txs are inserted"

-- | Test that a AcquiredEbTxs notification is produced when all
-- transactions are inserted via rwInsertTxs.
test_offerBlockTxs :: LeiosDbHandle IO -> IO ()
test_offerBlockTxs db = do
  chan <- subscribeEbNotifications db
  let point = mkTestPoint (SlotNo 1) 1
      eb = mkTestEb 3 -- 3 transactions
      ebTxList = V.toList (leiosEbTxs eb)
  withRW db $ \con -> do
    -- Insert the EB (point then body)
    rwInsertEbPoint con point (encodeLeiosEbSize eb)
    void $ rwInsertEbBody con point eb
    -- Consume the LeiosOfferBlock notification
    _ <- atomically $ readTChan chan
    -- Insert all txs (by offset)
    let txsToInsert = [(i, txBytesFor eb i) | (i, _) <- zip [0 :: Int ..] ebTxList]
    _ <- rwInsertTxs con point txsToInsert
    -- FIXME: blocks forever if impl not working
    notification <- atomically $ readTChan chan
    assertOfferBlockTxs point notification

-- | A body write naming fill sources copies their durable bytes in the same
-- transaction: an EB whose closure another EB already holds completes at body
-- time, without fetching anything. A vanished source fills nothing and the
-- offset stays missing.
test_crossEbFill :: LeiosDbHandle IO -> IO ()
test_crossEbFill db = do
  chan <- subscribeEbNotifications db
  let pointA = mkTestPoint (SlotNo 1) 1
      ebA = mkTestEb 3
      hashA = pointEbHash pointA
      -- B references the same first two txs (same declared sizes), so A's rows
      -- are valid fill sources for B's offsets 0 and 1.
      pointB = mkTestPoint (SlotNo 2) 2
      ebB = mkTestEb 2
  withRW db $ \con -> do
    -- A: full closure, the durable source.
    rwInsertEbPoint con pointA (encodeLeiosEbSize ebA)
    void $ rwInsertEbBody con pointA ebA
    _ <- rwInsertTxs con pointA [(i, txBytesFor ebA i) | i <- [0 .. 2]]
    _ <- atomically $ tryReadTChan chan -- AcquiredEb A
    _ <- atomically $ tryReadTChan chan -- AcquiredEbTxs A
    -- B: body write with fills from A, plus one from a source that does not
    -- exist -- the vanished-source case must fill nothing for that offset.
    rwInsertEbPoint con pointB (encodeLeiosEbSize ebB)
    (completed, filled) <-
      await
        =<< writeEbBody
          (rwWriter con)
          pointB
          ebB
          [ (0, MkTxLocation hashA 0)
          , (1, MkTxLocation hashA 1)
          , (1, MkTxLocation (mkTestEbHash 99) 0) -- redundant AND vanished: must be a no-op
          ]
    filled @?= [0, 1]
    completed @?= [pointB]
    -- and the closure reads back with A's bytes
    closure <- rwLookupTrustedEbClosure con (pointEbHash pointB)
    fmap (map snd) closure @?= Just [txBytesFor ebA 0, txBytesFor ebA 1]
    -- C declares a DIFFERENT tx hash of the same size as A's offset 0: a
    -- stale location resolving to the wrong EB must fill nothing even when
    -- the lengths agree.
    let pointC = mkTestPoint (SlotNo 3) 3
        ebC = MkLeiosEb $ V.fromList [(mkTestTxHash 77, 100)]
    rwInsertEbPoint con pointC (encodeLeiosEbSize ebC)
    (completedC, filledC) <-
      await =<< writeEbBody (rwWriter con) pointC ebC [(0, MkTxLocation hashA 0)]
    filledC @?= []
    completedC @?= []

-- | Rows are pre-allocated by the body write, so bytes arriving before the
-- body are dropped: there is no row to fill, and nothing may count towards
-- completion. Once the body has allocated the rows, the same fills land and
-- complete the closure. (Production cannot deliver bytes before the body --
-- the writer queue is FIFO and tx writes follow their body write -- so the
-- dropped fills only assert the guard.)
test_offerBlockTxsWhenBodyArrivesAfterTxs :: LeiosDbHandle IO -> IO ()
test_offerBlockTxsWhenBodyArrivesAfterTxs db = do
  chan <- subscribeEbNotifications db
  let point = mkTestPoint (SlotNo 1) 1
      eb = mkTestEb 3
      ebTxList = V.toList (leiosEbTxs eb)
      txsToInsert = [(i, txBytesFor eb i) | (i, _) <- zip [0 :: Int ..] ebTxList]
  withRW db $ \con -> do
    -- Bytes before the body: dropped, no notification.
    _ <- rwInsertTxs con point txsToInsert
    noEarlyNotif <- atomically $ tryReadTChan chan
    case noEarlyNotif of
      Nothing -> pure ()
      Just _ -> assertFailure "must not notify before any EB body is inserted"
    -- The body allocates the rows; the closure is NOT complete (the early
    -- fills were dropped), so only AcquiredEb fires.
    rwInsertEbPoint con point (encodeLeiosEbSize eb)
    void $ rwInsertEbBody con point eb
    acquiredEb <- readTChanWithin 100_000_000 chan "AcquiredEb"
    assertOfferBlock point acquiredEb
    noTxsYet <- atomically $ tryReadTChan chan
    case noTxsYet of
      Nothing -> pure ()
      Just _ -> assertFailure "pre-body fills must not complete the closure"
    -- The same fills now land on the allocated rows and complete it.
    _ <- rwInsertTxs con point txsToInsert
    acquiredTxs <- readTChanWithin 100_000_000 chan "AcquiredEbTxs"
    assertOfferBlockTxs point acquiredTxs

-- | Test that completed EBs are not re-notified when subsequent unrelated
-- transactions are inserted.
test_noReNotifyCompletedEbs :: LeiosDbHandle IO -> IO ()
test_noReNotifyCompletedEbs db = do
  chan <- subscribeEbNotifications db
  let point = mkTestPoint (SlotNo 1) 1
      eb = mkTestEb 2
      ebTxList = V.toList (leiosEbTxs eb)
  withRW db $ \con -> do
    -- Insert and complete the EB
    rwInsertEbPoint con point (encodeLeiosEbSize eb)
    void $ rwInsertEbBody con point eb
    -- Consume the AcquiredEb notification
    acquiredEb <- atomically $ tryReadTChan chan
    case acquiredEb of
      Just (AcquiredEb{}) -> pure ()
      _ -> assertFailure "expected AcquiredEb notification"
    let txsToInsert = [(i, txBytesFor eb i) | (i, _) <- zip [0 :: Int ..] ebTxList]
    _ <- rwInsertTxs con point txsToInsert
    -- Consume the AcquiredEbTxs notification
    acquiredTxs <- atomically $ tryReadTChan chan
    case acquiredTxs of
      Just (AcquiredEbTxs p) -> p @?= point
      _ -> assertFailure "expected AcquiredEbTxs notification"
    -- Fill an unrelated EB's offset (no body row: the write is dropped, which
    -- is the point -- nothing may be re-notified either way)
    _ <- rwInsertTxs con (mkTestPoint (SlotNo 9) 9) [(0, txBytesFor eb 0)]
    -- No re-notification should occur for the already-completed EB
    maybeNotif <- atomically $ tryReadTChan chan
    case maybeNotif of
      Nothing -> pure ()
      Just _ -> assertFailure "completed EB should not be re-notified"

-- | Re-inserting a tx that is _referenced_ by an already-completed EB
-- must not re-fire 'AcquiredEbTxs'. This is the practical case where
-- the previous 'no re-notification' check (which only used an unrelated
-- tx) lets a bug through: the completion predicate
-- @any (\\e -> eteTxHash e \`elem\` insertedTxHashes)@ matches again on
-- any subsequent batch containing an EB-referenced tx, since the
-- 'all txs present' clause remains true once the EB is complete.
-- The voting layer treats the duplicate 'AcquiredEbTxs' as a fatal
-- 'AlreadyKnown' from 'addVote'.
test_noReNotifyOnRelatedTxReinsert :: LeiosDbHandle IO -> IO ()
test_noReNotifyOnRelatedTxReinsert db = do
  chan <- subscribeEbNotifications db
  let point = mkTestPoint (SlotNo 1) 1
      eb = mkTestEb 2
      ebTxList = V.toList (leiosEbTxs eb)
  withRW db $ \con -> do
    rwInsertEbPoint con point (encodeLeiosEbSize eb)
    void $ rwInsertEbBody con point eb
    acquiredEb <- atomically $ tryReadTChan chan
    case acquiredEb of
      Just (AcquiredEb{}) -> pure ()
      _ -> assertFailure "expected AcquiredEb notification"
    -- Insert all EB-referenced txs → EB completes, one AcquiredEbTxs.
    let txsToInsert = [(i, txBytesFor eb i) | (i, _) <- zip [0 :: Int ..] ebTxList]
    _ <- rwInsertTxs con point txsToInsert
    acquiredTxs <- atomically $ tryReadTChan chan
    case acquiredTxs of
      Just (AcquiredEbTxs p) -> p @?= point
      _ -> assertFailure "expected AcquiredEbTxs notification"
    -- Re-insert one of the EB's own txs (a no-op at the tx storage
    -- level — it's already present). The completed EB must NOT be
    -- re-notified.
    case ebTxList of
      (_ : _) -> do
        _ <- rwInsertTxs con point [(0, txBytesFor eb 0)]
        maybeNotif <- atomically $ tryReadTChan chan
        case maybeNotif of
          Nothing -> pure ()
          Just _ ->
            assertFailure
              "completed EB should not be re-notified on re-insert of its own tx"
      [] -> assertFailure "test EB has no txs"

-- | A body can only be persisted for a point already registered (via
-- 'writeEbPoint', on the announcement path).
test_ebBodyWithoutPointFails :: LeiosDbHandle IO -> IO ()
test_ebBodyWithoutPointFails db = withRW db $ \con -> do
  result <- tryDb $ rwInsertEbBody con (mkTestPoint (SlotNo 1) 1) (mkTestEb 2)
  case result of
    Left _ -> pure ()
    Right _ -> assertFailure "writeEbBody succeeded for a point that was never registered"
 where
  tryDb :: IO a -> IO (Either LeiosDbException a)
  tryDb = try

-- | The same EB content can be forged at multiple slots; the DB must
-- track each 'LeiosPoint' independently and emit one 'AcquiredEbTxs'
-- per (slot, hash) when the closure completes, regardless of how many
-- slots reference the same hash. Conflating the slots loses a
-- notification.
test_multipleSlotsSameHash :: LeiosDbHandle IO -> IO ()
test_multipleSlotsSameHash db = do
  chan <- subscribeEbNotifications db
  let hashSeed = 1
      point1 = mkTestPoint (SlotNo 1) hashSeed
      point2 = mkTestPoint (SlotNo 2) hashSeed
      eb = mkTestEb 2 -- same content at both points (deterministic from numTxs)
      ebTxList = V.toList (leiosEbTxs eb)
      drainNotifications = go []
       where
        go acc =
          atomically (tryReadTChan chan) >>= \case
            Nothing -> pure (reverse acc)
            Just n -> go (n : acc)
  withRW db $ \con -> do
    -- Announce the same EB at two slots and insert the body twice.
    rwInsertEbPoint con point1 (encodeLeiosEbSize eb)
    rwInsertEbPoint con point2 (encodeLeiosEbSize eb)
    void $ rwInsertEbBody con point1 eb
    void $ rwInsertEbBody con point2 eb
    -- Drain the two AcquiredEb notifications (order matches insertion).
    acquiredEbs <- drainNotifications
    let acquiredEbPoints =
          [p | AcquiredEb p _ <- acquiredEbs]
    acquiredEbPoints `setEquals` [point1, point2]
    -- Insert every tx the EB references — closure completes for both rows.
    _ <-
      rwInsertTxs
        con
        point1
        [(i, txBytesFor eb i) | (i, _) <- zip [0 :: Int ..] ebTxList]
    -- Both rows must notify completion, once each.
    completionNotifs <- drainNotifications
    let completionPoints =
          [p | AcquiredEbTxs p <- completionNotifs]
    completionPoints `setEquals` [point1, point2]
    length completionNotifs @?= 2
 where
  setEquals xs ys = Map.fromList [(p, ()) | p <- xs] @?= Map.fromList [(p, ()) | p <- ys]

-- * Test utilities

-- | Read a notification with a microsecond timeout. Fails the enclosing
-- test if no notification arrives within the deadline (so a missing
-- notification surfaces as a normal test failure rather than a hang).
readTChanWithin ::
  Int ->
  StrictTChan IO LeiosEbNotification ->
  String ->
  IO LeiosEbNotification
readTChanWithin micros chan label =
  Timeout.timeout micros (atomically (readTChan chan)) >>= \case
    Just x -> pure x
    Nothing -> assertFailure $ "expected " <> label <> " notification within " <> show micros <> "μs, got none"

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

-- * Property tests for lookupTrustedEbClosure

-- | Property: complete EB (all txs inserted) returns Just with correct tx data.
prop_completedEbComplete :: DbImpl -> Property
prop_completedEbComplete impl =
  forAllShrinkShow (chooseInt (1, 20)) shrink show $ \numTxs ->
    forAllBlind (genPointAndEb numTxs) $ \(point, eb) ->
      ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
        rwInsertEbPoint con point (encodeLeiosEbSize eb)
        void $ rwInsertEbBody con point eb
        let ebTxList = V.toList (leiosEbTxs eb)
            txsToInsert = [(i, txBytesFor eb i) | (i, _) <- zip [0 :: Int ..] ebTxList]
        _ <- rwInsertTxs con point txsToInsert
        (result, queryTime) <- timed $ rwLookupTrustedEbClosure con (pointEbHash point)
        let expectedHashes = map fst ebTxList
            check = case result of
              Nothing ->
                False & counterexample "Expected Just, got Nothing"
              Just txData ->
                conjoin
                  [ length txData === numTxs
                      & counterexample "Wrong number of txs"
                  , all (`elem` map fst txData) expectedHashes
                      & counterexample "Missing expected tx hashes"
                  , and
                      [ bytes == txBytesFor eb off
                      | (off, (_h, bytes)) <- zip [0 ..] txData
                      ]
                      & counterexample "Wrong tx bytes"
                  ]
        pure $
          check
            & tabulate "lookupEbClosure (complete)" [timeBucket queryTime]
            & tabulate "numTxs" [magnitudeBucket numTxs]

-- | Property: EB with body but no txs returns Nothing.
prop_completedEbMissingTxs :: DbImpl -> Property
prop_completedEbMissingTxs impl =
  forAllShrinkShow (chooseInt (1, 20)) shrink show $ \numTxs ->
    forAllBlind (genPointAndEb numTxs) $ \(point, eb) ->
      ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
        rwInsertEbPoint con point (encodeLeiosEbSize eb)
        void $ rwInsertEbBody con point eb
        (result, queryTime) <- timed $ rwLookupTrustedEbClosure con (pointEbHash point)
        pure $
          result === Nothing
            & counterexample "Expected Nothing when no txs are present"
            & tabulate "lookupTrustedEbClosure (no txs)" [timeBucket queryTime]
            & tabulate "numTxs" [magnitudeBucket numTxs]

-- | Property: EB with partial txs (at least one missing) returns Nothing.
prop_completedEbPartialTxs :: DbImpl -> Property
prop_completedEbPartialTxs impl =
  forAllShrinkShow (chooseInt (2, 20)) shrink show $ \numTxs ->
    forAllBlind (genPointAndEb numTxs) $ \(point, eb) ->
      ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
        rwInsertEbPoint con point (encodeLeiosEbSize eb)
        void $ rwInsertEbBody con point eb
        -- Insert only the first half of txs, leaving at least one missing
        let ebTxList = V.toList (leiosEbTxs eb)
            partialTxs = take (numTxs `div` 2) ebTxList
            txsToInsert = [(i, txBytesFor eb i) | (i, _) <- zip [0 :: Int ..] partialTxs]
        _ <- rwInsertTxs con point txsToInsert
        (result, queryTime) <- timed $ rwLookupTrustedEbClosure con (pointEbHash point)
        pure $
          result === Nothing
            & counterexample
              ( "Expected Nothing with "
                  ++ show (length txsToInsert)
                  ++ "/"
                  ++ show numTxs
                  ++ " txs present"
              )
            & tabulate "lookupEbClosure (partial txs)" [timeBucket queryTime]
            & tabulate "numTxs" [magnitudeBucket numTxs]

-- | Property: EB with only a point announced (no body) returns Nothing.
prop_completedEbNoBody :: DbImpl -> Property
prop_completedEbNoBody impl =
  forAll genPoint $ \point ->
    ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
      rwInsertEbPoint con point 1000
      (result, queryTime) <- timed $ rwLookupTrustedEbClosure con (pointEbHash point)
      pure $
        result === Nothing
          & counterexample "Expected Nothing for EB with no body inserted"
          & tabulate "lookupTrustedEbClosure (no body)" [timeBucket queryTime]

-- * truncateLeiosDbAfterSlot

-- | A caller that drops the handle without closing it leaves the background
-- threads with nothing that can reach them, and the runtime raises
-- 'BlockedIndefinitelyOnSTM' at them. That is the handle going away, not a
-- failed write, so it must not reach the thread that opened the database.
test_droppingTheHandleSpreadsNoException :: Assertion
test_droppingTheHandleSpreadsNoException = do
  sysTmp <- getCanonicalTemporaryDirectory
  outcome <-
    bracket (createTempDirectory sysTmp "leios-test") removeQuietly $ \tmpDir ->
      try $ do
        -- Nothing binds the handle, so the collection below finds the threads
        -- it started unreachable. Without it the test allocates too little to
        -- collect, and the exception would land on a later test instead.
        void $
          newLeiosDBSQLite nullTracer (tmpDir <> "/test.vol.db") (tmpDir <> "/test.imm.db")
        performMajorGC
        -- The copier has to finish and its link watcher has to wake before
        -- anything reaches this thread.
        threadDelay settleMicros
  case outcome :: Either SomeException () of
    Right () -> pure ()
    Left e ->
      assertFailure $ "dropping the handle threw at its owner: " <> displayException e
 where
  -- The dropped handle closes its connections on its own schedule, so it can
  -- still be deleting its write-ahead files here.
  removeQuietly dir =
    removeDirectoryRecursive dir `catch` \(_ :: IOException) -> pure ()

  settleMicros = 1_000_000

-- | The writer takes a job off its queue before it runs it, so the drain that
-- fails the still-queued jobs when the writer stops can no longer reach that
-- one. Its awaiter must be told anyway, whatever the job threw.
--
-- Three things can unblock the awaiter, and only one of them counts.
-- 'startWriter' links its worker to the thread that created the handle, so the
-- awaiter runs on a thread of its own and the link exception cannot be what
-- wakes it. A result nobody can write any more is the failure under test, so
-- 'BlockedIndefinitelyOnSTM' is not an answer either. And a submission that a
-- sealed queue refuses throws a 'LeiosDbWriteException' just as a reported
-- failure does, so submitting happens outside the 'try'.
test_awaiterHearsAFailedJob :: Assertion
test_awaiterHearsAFailedJob = do
  sysTmp <- getCanonicalTemporaryDirectory
  bracket (createTempDirectory sysTmp "leios-test") removeDirectoryRecursive $ \tmpDir -> do
    outcomeVar <- newEmptyMVar
    let volDbPath = tmpDir <> "/test.vol.db"
        immDbPath = tmpDir <> "/test.imm.db"
        point = mkTestPoint 5 1
        eb = mkTestEb 2
        -- 'sqlInsertEbBody' traces a collision from inside the job it runs,
        -- and an 'IOException' is not a 'LeiosDbException', so 'publish' lets
        -- it past and the job dies with its result unwritten. That trace is
        -- the only one a job makes, so if it moves out of the job the second
        -- write below succeeds and this test fails, asking for another way to
        -- make a job throw.
        tracer = Tracer . emit $ \case
          TraceLeiosDbInsertCollision{} -> throwIO (userError jobFailureMarker)
          _ -> pure ()
        forkAwaiter w =
          void . forkIO $ do
            -- Submitting is outside the 'try'. If it throws, nothing is
            -- recorded, and the budget below reports that the awaiter was
            -- never told rather than counting a refused submission as a
            -- report.
            promise <- writeEbBody w point eb []
            outcome <- try (await promise) :: IO (Either SomeException (CompletedEbs, [Int]))
            void $ tryPutMVar outcomeVar outcome
        -- This thread creates the handle, so this is where the worker's
        -- parting exception lands. It does nothing else.
        runUntilTheWriterDies =
          withLeiosDBSQLite tracer volDbPath immDbPath $ \db ->
            withWriter db $ \w -> do
              void $ await =<< writeEbPoint w point (encodeLeiosEbSize eb)
              void $ await =<< writeEbBody w point eb []
              -- The same body again collides on the primary key of ebTxs, so
              -- this second write is the job whose action throws.
              forkAwaiter w
              void $ Timeout.timeout awaitBudgetMicros (readMVar outcomeVar)
    _ <- try runUntilTheWriterDies :: IO (Either SomeException ())
    -- Again, because the link exception can end the block above before the
    -- awaiter runs.
    Timeout.timeout awaitBudgetMicros (readMVar outcomeVar) >>= \case
      Nothing ->
        assertFailure "the awaiter was never told the write's fate"
      Just (Right completed) ->
        assertFailure $ "the write should have failed, but reported " <> show completed
      Just (Left e) -> case (fromException e :: Maybe LeiosDbException) of
        Just LeiosDbWriteException{writeJob = job, writeFailure = cause}
          | "WriteEbBody" `isPrefixOf` job
          , jobFailureMarker `isInfixOf` displayException cause ->
              pure ()
        _ ->
          assertFailure $
            "the awaiter should have been told the write failed; it got: "
              <> displayException e
 where
  jobFailureMarker :: String
  jobFailureMarker = "the tracer threw inside the write job"

  awaitBudgetMicros :: Int
  awaitBudgetMicros = 20_000_000

-- | A pinned EB whose tx closure never arrives cannot be copied, so the
-- copier keeps retrying it for as long as the database is open. 'close' must
-- still return.
test_closeWithPinnedEb :: Assertion
test_closeWithPinnedEb = do
  sysTmp <- getCanonicalTemporaryDirectory
  bracket (createTempDirectory sysTmp "leios-test") removeDirectoryRecursive $ \tmpDir -> do
    copyFailed <- newEmptyMVar
    let volDbPath = tmpDir <> "/test.vol.db"
        immDbPath = tmpDir <> "/test.imm.db"
        -- The copier traces one of these per pass that cannot retire the pin.
        -- Waiting for the first one leaves the copier on the retry path, which
        -- is the case a stop check on the idle wait alone does not reach.
        tracer = Tracer . emit $ \case
          TraceLeiosDbCopyError{} -> void $ tryPutMVar copyFailed ()
          _ -> pure ()
        point = mkTestPoint 5 1
    closed <-
      Timeout.timeout closeTimeoutMicros $
        withLeiosDBSQLite tracer volDbPath immDbPath $ \db -> do
          -- The body never lands, so the closure stays incomplete and every
          -- attempt to copy this EB fails.
          withRW db $ \con -> rwInsertEbPoint con point (encodeLeiosEbSize (mkTestEb 2))
          leiosDbPromoteToImmutable db [point]
          takeMVar copyFailed
    case closed of
      Nothing -> assertFailure "close did not return while an EB stayed pinned"
      Just () -> pure ()
 where
  closeTimeoutMicros = 30_000_000

test_truncateDropsEbsAfterSlot :: FilePath -> FilePath -> LeiosDbHandle IO -> IO ()
test_truncateDropsEbsAfterSlot volDbPath _immDbPath db = do
  let eb = mkTestEb 2
      keptHash = mkTestEbHash 1
      droppedHash = mkTestEbHash 2
  withRW db $ \con -> do
    rwInsertEbPoint con (MkLeiosPoint 5 keptHash) (encodeLeiosEbSize eb)
    void $ rwInsertEbBody con (MkLeiosPoint 5 keptHash) eb
    -- The kept EB is announced again at a slot the truncation drops.
    rwInsertEbPoint con (MkLeiosPoint 15 keptHash) (encodeLeiosEbSize eb)
    rwInsertEbPoint con (MkLeiosPoint 15 droppedHash) (encodeLeiosEbSize eb)
    void $ rwInsertEbBody con (MkLeiosPoint 15 droppedHash) eb

  truncateLeiosDbAfterSlot volDbPath 10

  withRW db $ \con -> do
    points <- rwScanEbPoints con
    points @?= [(5, keptHash)]
    keptBody <- rwLookupEbBody con keptHash
    keptBody @?= V.toList (leiosEbTxs eb)
    droppedBody <- rwLookupEbBody con droppedHash
    droppedBody @?= []

-- * deleteDanglingTxs

test_deleteDanglingTxs :: FilePath -> FilePath -> LeiosDbHandle IO -> IO ()
test_deleteDanglingTxs volDbPath _immDbPath db = do
  let eb = mkTestEb 2
      ebHash = mkTestEbHash 1
      danglingTx = mkTestTxHash 9
  withRW db $ \con -> do
    rwInsertEbPoint con (MkLeiosPoint 5 ebHash) (encodeLeiosEbSize eb)
    void $ rwInsertEbBody con (MkLeiosPoint 5 ebHash) eb
    -- The out-of-range offset is dropped by the guarded fill; the in-range
    -- ones land.
    void $
      rwInsertTxs con (MkLeiosPoint 5 ebHash) $
        (V.length (leiosEbTxs eb), txBytesFor eb 0)
          : [(i, txBytesFor eb i) | (i, _) <- zip [0 :: Int ..] (V.toList (leiosEbTxs eb))]

  deleteDanglingTxs volDbPath

  withRW db $ \con -> do
    closure <- rwLookupTrustedEbClosure con ebHash
    fmap (map fst) closure @?= Just (map fst (V.toList (leiosEbTxs eb)))
    -- A closure resolves only when the db holds every tx the body names. So
    -- this probe resolves only if the delete missed the dangling tx.
    let probeEb = MkLeiosEb (V.fromList [(danglingTx, 10)])
        probePoint = MkLeiosPoint 6 (mkTestEbHash 2)
    rwInsertEbPoint con probePoint (encodeLeiosEbSize probeEb)
    void $ rwInsertEbBody con probePoint probeEb
    probeClosure <- rwLookupTrustedEbClosure con probePoint.pointEbHash
    probeClosure @?= Nothing
