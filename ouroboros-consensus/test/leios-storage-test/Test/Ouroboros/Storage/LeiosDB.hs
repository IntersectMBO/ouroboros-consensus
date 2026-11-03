{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Tests for the "Ouroboros.Consensus.Storage.LeiosDB" interface.
--
-- These tests verify the semantics of the LeiosDbHandle operations for both
-- InMemory and SQLite implementations. Each test case uses a fresh database
-- to ensure isolation and serve as an (too optimistic) performance baseline.
module Test.Ouroboros.Storage.LeiosDB (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Control.DeepSeq (force)
import Control.Exception (bracket, try)
import Control.Monad (forM, forM_, replicateM, void)
import Control.Monad.Class.MonadTime.SI (diffTime, getMonotonicTime)
import Control.Tracer (Tracer (..), emit, nullTracer)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Function ((&))
import Data.List (nub, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Time.Clock (DiffTime)
import qualified Database.SQLite3 as SQL
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EndorserBlock
  , EndorserBlockHash
  , LeiosPoint (..)
  , TxHash
  , TxLocation (..)
  , TxOffset
  , encodeEndorserBlockSize
  , endorserBlockReferences
  , mkEndorserBlock
  )
import Ouroboros.Consensus.Storage.LeiosDB
  ( CompletedEbs
  , LeiosDbHandle (..)
  , LeiosDbReader (..)
  , LeiosDbWriter (..)
  , LocalFill
  , Promise (..)
  , TraceLeiosDb (..)
  , newLeiosDBInMemory
  , truncateLeiosDbAfterSlot
  , withLeiosDBSQLite
  , withReader
  , withWriter
  )
import Ouroboros.Consensus.Storage.LeiosDB.Exception (LeiosDbException)
import System.Directory (doesFileExist, removeDirectoryRecursive)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import qualified System.Timeout as Timeout
import Test.QuickCheck
  ( Gen
  , Property
  , chooseInt
  , conjoin
  , counterexample
  , elements
  , forAll
  , forAllBlind
  , forAllShrinkShow
  , frequency
  , ioProperty
  , listOf
  , shrink
  , shrinkList
  , sublistOf
  , tabulate
  , vector
  , (===)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "LeiosDB" $
    forEachImplementation mkTestGroups
      <> [ testProperty "the two implementations agree on what a reader sees" prop_implementationsAgree
         , -- InMemory only: on SQLite a failed write also stops the writer, which
           -- is linked to the thread that opened the database.
           testCase "InMemory: writing a body for an unwritten point fails, as on SQLite" $
             withFreshDb InMemory test_ebBodyWithoutPointFails
         , -- InMemory only: the SQLite implementation sweeps in the background.
           testGroup
             "InMemory: garbage collection"
             [ testCase "evicts a point older than the slot, with its txs" $
                 withFreshDb InMemory test_gcEvictsOldEb
             , testCase "keeps a point at the slot" $
                 withFreshDb InMemory test_gcKeepsEbWithPointAtSlot
             , testCase "keeps a pinned point readable" $
                 withFreshDb InMemory test_gcKeepsPinnedEb
             , testCase "promoting an EB whose closure is incomplete fails" $
                 withFreshDb InMemory test_promoteIncompleteFails
             ]
         , testGroup
             "truncateLeiosDbAfterSlot"
             [ testCase "drops the points after the slot, with their bodies" $
                 withFreshSQLiteFile test_truncateDropsEbsAfterSlot
             , testCase "runs on the immutable partition" $
                 withTempLeiosDbPaths test_truncateImmutablePartition
             ]
         , testGroup
             "close"
             [ testCase "returns while an EB stays pinned" test_closeWithPinnedEb
             ]
         , testGroup
             "file initialisation (SQLite)"
             [ testCase "handle creation applies both schemas" $
                 withTempLeiosDbPaths test_schemaAtHandleCreation
             , testCase "zero-byte files get a schema" $
                 withTempLeiosDbPaths test_zeroByteFilesGetSchema
             , testCase "reopening keeps the data" $
                 withTempLeiosDbPaths test_reopenKeepsData
             ]
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

rwInsertEbBody :: RW -> LeiosPoint -> EndorserBlock -> IO CompletedEbs
rwInsertEbBody rw point eb = fst <$> rwInsertEbBodyFilling rw point eb []

rwInsertEbBodyFilling ::
  RW -> LeiosPoint -> EndorserBlock -> [LocalFill] -> IO (CompletedEbs, [TxOffset])
rwInsertEbBodyFilling rw point eb fills = await =<< writeEbBody (rwWriter rw) point eb fills

rwInsertTxs :: RW -> LeiosPoint -> [(TxOffset, BS.ByteString)] -> IO CompletedEbs
rwInsertTxs rw point txs = await =<< writeTxs (rwWriter rw) point txs

rwScanEbPoints :: RW -> IO [(SlotNo, EndorserBlockHash)]
rwScanEbPoints = testScanEbPoints . rwReader

rwLookupEbBody :: RW -> LeiosPoint -> IO [(TxHash, BytesSize)]
rwLookupEbBody = lookupEbBody . rwReader

rwLookupEbClosure :: RW -> LeiosPoint -> IO (Maybe [(TxHash, BS.ByteString)])
rwLookupEbClosure = lookupEbClosure . rwReader

rwBatchRetrieveTxs ::
  RW -> LeiosPoint -> [Int] -> IO [(Int, TxHash, Maybe BS.ByteString)]
rwBatchRetrieveTxs = batchRetrieveTxs . rwReader

-- | Create a fresh database and run an action with it.
-- Ensures proper cleanup for SQLite databases.
withFreshDb :: DbImpl -> (LeiosDbHandle IO -> IO a) -> IO a
withFreshDb InMemory action =
  newLeiosDBInMemory >>= action
withFreshDb SQLite action = withFreshSQLiteDb action

-- | 'withFreshDb', with a wait for the SQLite implementation's background work
-- to catch up (see 'withSettle'). The in-memory implementation has none.
withSettledDb :: DbImpl -> (LeiosDbHandle IO -> IO () -> IO a) -> IO a
withSettledDb InMemory action = newLeiosDBInMemory >>= \db -> action db (pure ())
withSettledDb SQLite action =
  withFreshSQLiteFile $ \volDbPath _immDbPath db -> withSettle volDbPath (action db)

withFreshSQLiteDb :: (LeiosDbHandle IO -> IO a) -> IO a
withFreshSQLiteDb action = withFreshSQLiteFile (\_vol _imm -> action)

-- | Create a fresh SQLite database and hand its paths to the action, which
-- 'truncateLeiosDbAfterSlot' needs. The database is torn down -- writes
-- flushed, threads stopped, connections closed -- before the directory is
-- removed; a still-open connection makes the removal flaky (a hard failure
-- on Windows, where deleting an open WAL is a sharing violation).
withFreshSQLiteFile :: (FilePath -> FilePath -> LeiosDbHandle IO -> IO a) -> IO a
withFreshSQLiteFile action =
  withTempLeiosDbPaths $ \volDbPath immDbPath ->
    withLeiosDBSQLite nullTracer volDbPath immDbPath $
      action volDbPath immDbPath

-- | Paths for both partition files in a fresh temporary directory, which is
-- removed afterwards. Nothing is created at the paths.
withTempLeiosDbPaths :: (FilePath -> FilePath -> IO a) -> IO a
withTempLeiosDbPaths action = do
  sysTmp <- getCanonicalTemporaryDirectory
  bracket
    (createTempDirectory sysTmp "leios-test")
    removeDirectoryRecursive
    (\tmpDir -> action (tmpDir <> "/test.vol.db") (tmpDir <> "/test.imm.db"))

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
      , testCase "out of bounds, duplicate and wrong-sized writes are dropped" $
          withFreshDb impl test_malformedTxWritesDropped
      , testCase "a point copied to the immutable partition and evicted is read from the immutable copy" $
          test_readsFallBackToImmutable impl
      ]
  , testGroup
      "local fills"
      [ testCase "a body fills a tx from another point's bytes" $
          withFreshDb impl test_fillFromAnotherEb
      , testCase "a fill from a source without the bytes fills nothing" $
          withFreshDb impl test_fillWithoutSourceBytes
      , testCase "a body whose fills complete its closure completes it" $
          withFreshDb impl test_fillCompletesClosure
      ]
  , testGroup
      "completion"
      [ testCase "only the write that completes a closure reports it" $
          withFreshDb impl test_completion
      , testCase "each point of an endorser block completes on its own" $
          withFreshDb impl test_multipleSlotsSameHash
      , testCase "a scan finds a point whose body filled every tx from an older point of its endorser block" $
          withFreshDb impl test_scanFindsLaterPoint
      ]
  , testGroup
      "garbage collection"
      [ testCase "evicts an older point of an endorser block and keeps a newer one" $
          test_gcKeepsNewerPoint impl
      ]
  , testGroup
      "lookupEbClosure"
      [ testProperty "complete EB returns Just with tx data" $ prop_completedEbComplete impl
      , testProperty "no txs returns Nothing" $ prop_completedEbMissingTxs impl
      , testProperty "partial txs returns Nothing" $ prop_completedEbPartialTxs impl
      , testProperty "no body returns Nothing" $ prop_completedEbNoBody impl
      ]
  ]

-- * QuickCheck generators

-- | Generate a random EndorserBlockHash (32 random bytes).
-- With 256 bits of randomness, collisions are practically impossible.
genEbHash :: Gen EndorserBlockHash
genEbHash = UnsafeHash . SBS.pack <$> vector 32

-- | Generate a random TxHash (32 random bytes).
genTxHash :: Gen TxHash
genTxHash = UnsafeHash . SBS.pack <$> vector 32

-- | Generate a random SlotNo.
genSlotNo :: Gen SlotNo
genSlotNo = SlotNo . fromIntegral <$> chooseInt (0, maxBound)

-- | Generate a random LeiosPoint.
genPoint :: Gen LeiosPoint
genPoint = MkLeiosPoint <$> genSlotNo <*> genEbHash

-- | Generate a EndorserBlock with the given number of transactions.
genEb :: Int -> Gen EndorserBlock
genEb numTxs = do
  txs <- replicateM numTxs $ do
    txHash <- genTxHash
    size <- chooseInt (50, 500)
    pure (txHash, fromIntegral size)
  pure $ mkEndorserBlock txs

-- | Generate a LeiosPoint and EndorserBlock together with the given number of transactions.
genPointAndEb :: Int -> Gen (LeiosPoint, EndorserBlock)
genPointAndEb numTxs = (,) <$> genPoint <*> genEb numTxs

-- | Bytes for the tx at the given offset of the EB: of its declared size, as
-- a write must be to be accepted, and distinct per offset.
txBytesFor :: EndorserBlock -> TxOffset -> BS.ByteString
txBytesFor eb off = BS.replicate (fromIntegral size) (fromIntegral off)
 where
  (_, size) = endorserBlockReferences eb !! off

-- | Every tx of the EB, with 'txBytesFor'.
allTxsFor :: EndorserBlock -> [(TxOffset, BS.ByteString)]
allTxsFor eb = [(off, txBytesFor eb off) | off <- [0 .. length (endorserBlockReferences eb) - 1]]

-- * Test fixtures for unit tests

-- | Create a simple test EndorserBlockHash from a seed byte.
mkTestEbHash :: Word -> EndorserBlockHash
mkTestEbHash seed = UnsafeHash $ SBS.pack $ replicate 32 (fromIntegral seed)

-- | Create a simple test TxHash from a seed byte.
mkTestTxHash :: Word -> TxHash
mkTestTxHash seed = UnsafeHash $ SBS.pack $ replicate 32 (fromIntegral seed)

-- | Create a test LeiosPoint.
mkTestPoint :: SlotNo -> Word -> LeiosPoint
mkTestPoint slot seed = MkLeiosPoint slot (mkTestEbHash seed)

-- | Create a test EndorserBlock with the given number of transactions.
mkTestEb :: Int -> EndorserBlock
mkTestEb numTxs =
  mkEndorserBlock $
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
        (pointSlotNo point, pointEbHash point) `elem` points
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
        let expected = [(pointSlotNo p, pointEbHash p) | p <- points]
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
        let expectedTxs = endorserBlockReferences eb
        rwInsertEbPoint con point (encodeEndorserBlockSize eb)
        (_, insertTime) <- timed $ rwInsertEbBody con point eb
        (result, lookupTime) <- timed $ rwLookupEbBody con point
        pure $
          result == expectedTxs
            & counterexample ("Expected: " ++ show expectedTxs ++ "\nGot: " ++ show result)
            & tabulate "insertEbBody" [timeBucket insertTime]
            & tabulate "lookupEbBody" [timeBucket lookupTime]

-- | Property: looking up a non-existent point returns empty list.
prop_ebsLookupMissing :: DbImpl -> Property
prop_ebsLookupMissing impl =
  forAll genPoint $ \missing ->
    ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
      (result, lookupTime) <- timed $ rwLookupEbBody con missing
      pure $
        result === []
          & tabulate "lookupEbBody (missing)" [timeBucket lookupTime]

-- * Property tests for transactions

-- | Property: inserting tx bytes and retrieving them returns the correct data.
prop_txsInsertThenRetrieve :: DbImpl -> Property
prop_txsInsertThenRetrieve impl =
  forAllShrinkShow (chooseInt (1, 50)) (filter (>= 1) . shrink) show $ \numTxs ->
    forAllBlind (genPointAndEb numTxs) $ \(point, eb) ->
      forAllBlind (sublistOf [0 .. numTxs - 1]) $ \offsetsToInsert ->
        ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
          -- Insert the EB first (point then body)
          rwInsertEbPoint con point (encodeEndorserBlockSize eb)
          void $ rwInsertEbBody con point eb
          -- Forced, so that the tabulated insert time excludes building the
          -- bytes.
          let !txsToInsert =
                force [(off, txBytesFor eb off) | off <- offsetsToInsert]
          insertTime <- snd <$> timed (rwInsertTxs con point txsToInsert)
          -- Retrieve all offsets
          let allOffsets = [0 .. numTxs - 1]
          (results, retrieveTime) <- timed $ rwBatchRetrieveTxs con point allOffsets
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

-- | Property: retrieving from a non-existent point returns empty list.
prop_txsRetrieveMissing :: DbImpl -> Property
prop_txsRetrieveMissing impl =
  forAll genPoint $ \missing ->
    ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
      (result, retrieveTime) <- timed $ rwBatchRetrieveTxs con missing [0, 1, 2]
      pure $
        result === []
          & tabulate "batchRetrieveTxs (missing)" [timeBucket retrieveTime]

-- * Completion tests

-- | Only the write that completes an EB's closure reports it: not one that
-- leaves a tx missing, not one that writes a tx of the complete EB again, and
-- not one that completes another EB.
test_completion :: LeiosDbHandle IO -> IO ()
test_completion db = do
  let point = mkTestPoint (SlotNo 1) 1
      eb = mkTestEb 3
      otherPoint = mkTestPoint (SlotNo 2) 2
      otherEb = mkTestEb 2
  withRW db $ \con -> do
    rwInsertEbPoint con point (encodeEndorserBlockSize eb)
    rwInsertEbBody con point eb >>= (@?= [])
    rwInsertTxs con point (take 2 (allTxsFor eb)) >>= (@?= [])
    rwInsertTxs con point (drop 2 (allTxsFor eb)) >>= (@?= [point])
    rwInsertTxs con point (take 1 (allTxsFor eb)) >>= (@?= [])
    rwInsertEbPoint con otherPoint (encodeEndorserBlockSize otherEb)
    rwInsertEbBody con otherPoint otherEb >>= (@?= [])
    rwInsertTxs con otherPoint (allTxsFor otherEb) >>= (@?= [otherPoint])

-- | The same EB content can be announced at several slots; the DB stores each
-- 'LeiosPoint' on its own, so a write completes only the point it names.
test_multipleSlotsSameHash :: LeiosDbHandle IO -> IO ()
test_multipleSlotsSameHash db = do
  let hashSeed = 1
      point1 = mkTestPoint (SlotNo 1) hashSeed
      point2 = mkTestPoint (SlotNo 2) hashSeed
      eb = mkTestEb 2 -- same content at both points (deterministic from numTxs)
  withRW db $ \con -> do
    rwInsertEbPoint con point1 (encodeEndorserBlockSize eb)
    rwInsertEbPoint con point2 (encodeEndorserBlockSize eb)
    void $ rwInsertEbBody con point1 eb
    void $ rwInsertEbBody con point2 eb
    rwInsertTxs con point1 (allTxsFor eb) >>= (@?= [point1])
    holdsClosure con point2 >>= (@?= False)
    rwInsertTxs con point2 (allTxsFor eb) >>= (@?= [point2])

-- | An EB complete at one point and announced again at a later one is not
-- complete there until a body is written there, which can fill every tx from
-- the older point.
test_scanFindsLaterPoint :: LeiosDbHandle IO -> IO ()
test_scanFindsLaterPoint db = do
  let older = mkTestPoint (SlotNo 10) 1
      younger = mkTestPoint (SlotNo 30) 1
      eb = mkTestEb 2
  withRW db $ \con -> do
    writeWholeEb con [older] eb
    rwInsertEbPoint con younger (encodeEndorserBlockSize eb)
    scanCompleteEbClosuresNotOlderThanSlot (rwReader con) 20 >>= (@?= [])
    rwInsertEbBodyFilling con younger eb [(off, MkTxLocation older off) | (off, _) <- allTxsFor eb]
      >>= (@?= ([younger], [0, 1]))
    scanCompleteEbClosuresNotOlderThanSlot (rwReader con) 20 >>= (@?= [younger])

-- | A body can only be written for a point already written, by 'writeEbPoint'
-- on the announcement path.
test_ebBodyWithoutPointFails :: LeiosDbHandle IO -> IO ()
test_ebBodyWithoutPointFails db = withRW db $ \con -> do
  result <- tryDb $ rwInsertEbBody con (mkTestPoint (SlotNo 1) 1) (mkTestEb 2)
  case result of
    Left _ -> pure ()
    Right _ -> assertFailure "writeEbBody succeeded for a point that was never written"
 where
  tryDb :: IO a -> IO (Either LeiosDbException a)
  tryDb = try

-- | A tx write only fills a tx the body declares, once, at its declared size.
-- Anything else changes nothing and completes nothing.
--
-- Neither the LeiosDB nor this test requires the bytes to hash to the body's
-- tx hash: the LeiosDB assumes that its caller checked that.
test_malformedTxWritesDropped :: LeiosDbHandle IO -> IO ()
test_malformedTxWritesDropped db = do
  let point = mkTestPoint (SlotNo 1) 1
      eb = mkTestEb 2
      offsets = [0, 1]
  withRW db $ \con -> do
    rwInsertEbPoint con point (encodeEndorserBlockSize eb)
    void $ rwInsertEbBody con point eb
    -- An offset the body does not declare, and wrong-sized bytes.
    dropped <-
      rwInsertTxs
        con
        point
        [ (2, txBytesFor eb 0)
        , (0, BS.snoc (txBytesFor eb 0) 0)
        , (1, BS.take 1 (txBytesFor eb 1))
        ]
    dropped @?= []
    retrieved <- rwBatchRetrieveTxs con point offsets
    [bytes | (_, _, bytes) <- retrieved] @?= [Nothing, Nothing]
    -- A duplicate delivery keeps the first bytes and does not count twice.
    first <- rwInsertTxs con point [(0, txBytesFor eb 0)]
    first @?= []
    duplicate <- rwInsertTxs con point [(0, BS.map (+ 1) (txBytesFor eb 0))]
    duplicate @?= []
    retrieved' <- rwBatchRetrieveTxs con point [0]
    [bytes | (_, _, bytes) <- retrieved'] @?= [Just (txBytesFor eb 0)]
    completed <- rwInsertTxs con point [(1, txBytesFor eb 1)]
    completed @?= [point]

-- | Two endorser block points that both reference tx 1: 'mkTestEb' 2, and one
-- that references it at offset 0, then tx 7.
fillFixtures :: (LeiosPoint, EndorserBlock, LeiosPoint, EndorserBlock)
fillFixtures =
  ( mkTestPoint (SlotNo 1) 1
  , mkTestEb 2
  , mkTestPoint (SlotNo 2) 2
  , mkEndorserBlock [(mkTestTxHash 1, 101), (mkTestTxHash 7, 107)]
  )

test_fillFromAnotherEb :: LeiosDbHandle IO -> IO ()
test_fillFromAnotherEb db = do
  let (pointA, ebA, pointB, ebB) = fillFixtures
  withRW db $ \con -> do
    rwInsertEbPoint con pointA (encodeEndorserBlockSize ebA)
    void $ rwInsertEbBody con pointA ebA
    void $ rwInsertTxs con pointA (allTxsFor ebA)
    rwInsertEbPoint con pointB (encodeEndorserBlockSize ebB)
    -- The second fill's source lists another tx at its offset.
    result <-
      rwInsertEbBodyFilling
        con
        pointB
        ebB
        [ (0, MkTxLocation pointA 1)
        , (1, MkTxLocation pointA 0)
        ]
    result @?= ([], [0])
    retrieved <- rwBatchRetrieveTxs con pointB [0, 1]
    [bytes | (_, _, bytes) <- retrieved] @?= [Just (txBytesFor ebA 1), Nothing]
    completed <- rwInsertTxs con pointB [(1, txBytesFor ebB 1)]
    completed @?= [pointB]

test_fillWithoutSourceBytes :: LeiosDbHandle IO -> IO ()
test_fillWithoutSourceBytes db = do
  let (pointA, ebA, pointB, ebB) = fillFixtures
  withRW db $ \con -> do
    rwInsertEbPoint con pointA (encodeEndorserBlockSize ebA)
    void $ rwInsertEbBody con pointA ebA
    rwInsertEbPoint con pointB (encodeEndorserBlockSize ebB)
    -- The first source holds the body but not its txs; the second is unknown.
    result <-
      rwInsertEbBodyFilling
        con
        pointB
        ebB
        [ (0, MkTxLocation pointA 1)
        , (0, MkTxLocation (mkTestPoint 9 9) 0)
        ]
    result @?= ([], [])

test_fillCompletesClosure :: LeiosDbHandle IO -> IO ()
test_fillCompletesClosure db = do
  let (pointA, ebA, _, _) = fillFixtures
      pointB = mkTestPoint (SlotNo 2) 2
      ebB = mkEndorserBlock [(mkTestTxHash 1, 101)]
  withRW db $ \con -> do
    rwInsertEbPoint con pointA (encodeEndorserBlockSize ebA)
    void $ rwInsertEbBody con pointA ebA
    void $ rwInsertTxs con pointA (allTxsFor ebA)
    rwInsertEbPoint con pointB (encodeEndorserBlockSize ebB)
    result <- rwInsertEbBodyFilling con pointB ebB [(0, MkTxLocation pointA 1)]
    result @?= ([pointB], [0])

-- * Garbage collection

-- | Write the EB at these points, with its body and all its transactions.
writeWholeEb :: RW -> [LeiosPoint] -> EndorserBlock -> IO ()
writeWholeEb con points eb =
  forM_ points $ \point -> do
    rwInsertEbPoint con point (encodeEndorserBlockSize eb)
    void $ rwInsertEbBody con point eb
    void $ rwInsertTxs con point (allTxsFor eb)

-- | A point copied to the immutable partition and then evicted from the
-- volatile one is still read, from the immutable copy.
test_readsFallBackToImmutable :: DbImpl -> IO ()
test_readsFallBackToImmutable impl = withSettledDb impl $ \db settle -> withRW db $ \con -> do
  let eb = mkTestEb 2
      point = mkTestPoint 5 1
  writeWholeEb con [point] eb
  leiosDbPromoteToImmutable db [point]
  settle
  garbageCollect db 6
  settle
  rwScanEbPoints con >>= (@?= [])
  rwLookupEbBody con point >>= (@?= endorserBlockReferences eb)
  holdsClosure con point >>= (@?= True)
  rwBatchRetrieveTxs con point [0, 1]
    >>= (@?= [(off, mkTestTxHash (fromIntegral off), Just (txBytesFor eb off)) | off <- [0, 1]])

-- | Whether the reader sees the point's whole closure.
holdsClosure :: RW -> LeiosPoint -> IO Bool
holdsClosure con point = isJust <$> rwLookupEbClosure con point

test_gcEvictsOldEb :: LeiosDbHandle IO -> IO ()
test_gcEvictsOldEb db = withRW db $ \con -> do
  -- The old point references transactions 0, 1 and 2, and the other only 0
  -- and 1.
  writeWholeEb con [mkTestPoint 5 1] (mkTestEb 3)
  writeWholeEb con [mkTestPoint 10 2] (mkTestEb 2)
  garbageCollect db 6
  rwLookupEbBody con (mkTestPoint 5 1) >>= (@?= [])
  holdsClosure con (mkTestPoint 5 1) >>= (@?= False)
  rwScanEbPoints con >>= (@?= [(10, mkTestEbHash 2)])
  holdsClosure con (mkTestPoint 10 2) >>= (@?= True)

-- | An EB written whole at slot 5 and again at slot 10, collected at slot 6.
test_gcKeepsNewerPoint :: DbImpl -> IO ()
test_gcKeepsNewerPoint impl = withSettledDb impl $ \db settle -> withRW db $ \con -> do
  let eb = mkTestEb 2
      older = mkTestPoint 5 1
      newer = mkTestPoint 10 1
  writeWholeEb con [older, newer] eb
  garbageCollect db 6
  settle
  rwScanEbPoints con >>= (@?= [(10, mkTestEbHash 1)])
  rwLookupEbBody con older >>= (@?= [])
  holdsClosure con older >>= (@?= False)
  holdsClosure con newer >>= (@?= True)
  scanCompleteEbClosuresNotOlderThanSlot (rwReader con) 6 >>= (@?= [newer])

test_gcKeepsEbWithPointAtSlot :: LeiosDbHandle IO -> IO ()
test_gcKeepsEbWithPointAtSlot db = withRW db $ \con -> do
  writeWholeEb con [mkTestPoint 5 1, mkTestPoint 6 1] (mkTestEb 2)
  garbageCollect db 6
  holdsClosure con (mkTestPoint 5 1) >>= (@?= False)
  holdsClosure con (mkTestPoint 6 1) >>= (@?= True)

test_gcKeepsPinnedEb :: LeiosDbHandle IO -> IO ()
test_gcKeepsPinnedEb db = withRW db $ \con -> do
  writeWholeEb con [mkTestPoint 5 1] (mkTestEb 2)
  leiosDbPromoteToImmutable db [mkTestPoint 5 1]
  garbageCollect db 6
  holdsClosure con (mkTestPoint 5 1) >>= (@?= True)

test_promoteIncompleteFails :: LeiosDbHandle IO -> IO ()
test_promoteIncompleteFails db = withRW db $ \con -> do
  rwInsertEbPoint con (mkTestPoint 5 1) (encodeEndorserBlockSize (mkTestEb 2))
  void $ rwInsertEbBody con (mkTestPoint 5 1) (mkTestEb 2)
  result <- tryDb $ leiosDbPromoteToImmutable db [mkTestPoint 5 1]
  case result of
    Left _ -> pure ()
    Right () -> assertFailure "promoting an EB whose closure is incomplete succeeded"
 where
  tryDb :: IO a -> IO (Either LeiosDbException a)
  tryDb = try

-- * Property tests for lookupEbClosure

-- | Property: complete EB (all txs inserted) returns Just with correct tx data.
prop_completedEbComplete :: DbImpl -> Property
prop_completedEbComplete impl =
  forAllShrinkShow (chooseInt (1, 20)) shrink show $ \numTxs ->
    forAllBlind (genPointAndEb numTxs) $ \(point, eb) ->
      ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
        rwInsertEbPoint con point (encodeEndorserBlockSize eb)
        void $ rwInsertEbBody con point eb
        _ <- rwInsertTxs con point (allTxsFor eb)
        (result, queryTime) <- timed $ rwLookupEbClosure con point
        let expected =
              [ (txHash, txBytesFor eb off)
              | (off, (txHash, _size)) <- zip [0 ..] (endorserBlockReferences eb)
              ]
        pure $
          result === Just expected
            & tabulate "lookupEbClosure (complete)" [timeBucket queryTime]
            & tabulate "numTxs" [magnitudeBucket numTxs]

-- | Property: EB with body but no txs returns Nothing.
prop_completedEbMissingTxs :: DbImpl -> Property
prop_completedEbMissingTxs impl =
  forAllShrinkShow (chooseInt (1, 20)) shrink show $ \numTxs ->
    forAllBlind (genPointAndEb numTxs) $ \(point, eb) ->
      ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
        rwInsertEbPoint con point (encodeEndorserBlockSize eb)
        void $ rwInsertEbBody con point eb
        (result, queryTime) <- timed $ rwLookupEbClosure con point
        pure $
          result === Nothing
            & counterexample "Expected Nothing when no txs are present"
            & tabulate "lookupEbClosure (no txs)" [timeBucket queryTime]
            & tabulate "numTxs" [magnitudeBucket numTxs]

-- | Property: EB with partial txs (at least one missing) returns Nothing.
prop_completedEbPartialTxs :: DbImpl -> Property
prop_completedEbPartialTxs impl =
  forAllShrinkShow (chooseInt (2, 20)) shrink show $ \numTxs ->
    forAllBlind (genPointAndEb numTxs) $ \(point, eb) ->
      ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
        rwInsertEbPoint con point (encodeEndorserBlockSize eb)
        void $ rwInsertEbBody con point eb
        -- Insert only the first half of txs, leaving at least one missing
        let txsToInsert = take (numTxs `div` 2) (allTxsFor eb)
        _ <- rwInsertTxs con point txsToInsert
        (result, queryTime) <- timed $ rwLookupEbClosure con point
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
      (result, queryTime) <- timed $ rwLookupEbClosure con point
      pure $
        result === Nothing
          & counterexample "Expected Nothing for EB with no body inserted"
          & tabulate "lookupEbClosure (no body)" [timeBucket queryTime]

-- * truncateLeiosDbAfterSlot

test_truncateDropsEbsAfterSlot :: FilePath -> FilePath -> LeiosDbHandle IO -> IO ()
test_truncateDropsEbsAfterSlot volDbPath _immDbPath db = do
  let eb = mkTestEb 2
      kept = mkTestPoint 5 1
      -- The kept point's endorser block, at a slot the truncation drops.
      droppedAgain = mkTestPoint 15 1
      dropped = mkTestPoint 15 2
  withRW db $ \con -> writeWholeEb con [kept, droppedAgain, dropped] eb

  truncateLeiosDbAfterSlot volDbPath 10
  ebTxBytesRows volDbPath 15 >>= (@?= 0)

  withRW db $ \con -> do
    points <- rwScanEbPoints con
    points @?= [(5, mkTestEbHash 1)]
    keptBody <- rwLookupEbBody con kept
    keptBody @?= endorserBlockReferences eb
    forM_ [droppedAgain, dropped] $ \point ->
      rwLookupEbBody con point >>= (@?= [])
    keptClosure <- rwLookupEbClosure con kept
    fmap (map snd) keptClosure @?= Just (map snd (allTxsFor eb))

-- | The immutable schema lacks the volatile-only columns and indexes; the
-- tooling that DBTruncater runs on both files must still work on it.
test_truncateImmutablePartition :: FilePath -> FilePath -> IO ()
test_truncateImmutablePartition volDbPath immDbPath = do
  withLeiosDBSQLite nullTracer volDbPath immDbPath $ \_db -> pure ()
  truncateLeiosDbAfterSlot immDbPath 10

-- * File initialisation

-- | Names of the tables and indexes in a database file.
schemaObjects :: FilePath -> IO [String]
schemaObjects path =
  bracket (SQL.open (T.pack path)) SQL.close $ \db -> do
    SQL.exec db (T.pack "pragma busy_timeout = 1000;")
    bracket
      (SQL.prepare db (T.pack "SELECT name FROM sqlite_master WHERE type IN ('table', 'index')"))
      SQL.finalize
      $ \stmt ->
        let loop acc =
              SQL.step stmt >>= \case
                SQL.Row -> SQL.columnText stmt 0 >>= \name -> loop (T.unpack name : acc)
                SQL.Done -> pure acc
         in loop []

test_schemaAtHandleCreation :: FilePath -> FilePath -> IO ()
test_schemaAtHandleCreation volDbPath immDbPath =
  -- No reader or writer is opened: the schema must not depend on one.
  withLeiosDBSQLite nullTracer volDbPath immDbPath $ \_db -> do
    forM_ [volDbPath, immDbPath] $ \path ->
      doesFileExist path >>= assertBool (path <> " exists")
    vol <- schemaObjects volDbPath
    imm <- schemaObjects immDbPath
    forM_ [(vol, "vol"), (imm, "imm")] $ \(objs, file) ->
      forM_ ["ebs", "ebTxs", "ebTxBytes"] $ \name ->
        assertBool (file <> " has " <> name) (name `elem` objs)
    forM_
      [ "idx_ebs_sweepable"
      , "idx_ebs_markedForGc"
      , "idx_ebs_pinned"
      ]
      $ \name -> do
        assertBool ("vol has " <> name) (name `elem` vol)
        assertBool ("imm lacks " <> name) (name `notElem` imm)

-- | A file that exists without a schema, as a crash between creating the file
-- and applying the schema leaves it.
test_zeroByteFilesGetSchema :: FilePath -> FilePath -> IO ()
test_zeroByteFilesGetSchema volDbPath immDbPath = do
  writeFile volDbPath ""
  writeFile immDbPath ""
  withLeiosDBSQLite nullTracer volDbPath immDbPath $ \db ->
    withReader db $ \reader -> do
      points <- scanCompleteEbClosuresNotOlderThanSlot reader 0
      points @?= []

test_reopenKeepsData :: FilePath -> FilePath -> IO ()
test_reopenKeepsData volDbPath immDbPath = do
  let eb = mkTestEb 2
      ebHash = mkTestEbHash 1
  withLeiosDBSQLite nullTracer volDbPath immDbPath $ \db ->
    withRW db $ \con -> rwInsertEbPoint con (MkLeiosPoint 5 ebHash) (encodeEndorserBlockSize eb)
  withLeiosDBSQLite nullTracer volDbPath immDbPath $ \db ->
    withRW db $ \con -> do
      points <- rwScanEbPoints con
      points @?= [(5, ebHash)]

-- | Open a database file on a connection of its own.
withSqlFile :: FilePath -> (SQL.Database -> IO a) -> IO a
withSqlFile path k =
  bracket (SQL.open (T.pack path)) SQL.close $ \sql -> do
    SQL.exec sql (T.pack "pragma busy_timeout = 1000;")
    k sql

-- | How many 'ebTxBytes' rows the file holds at the slot.
ebTxBytesRows :: FilePath -> SlotNo -> IO Int
ebTxBytesRows path (SlotNo slot) =
  withSqlFile path $ \sql ->
    SQL.withStatement
      sql
      (T.pack ("SELECT COUNT(*) FROM ebTxBytes WHERE ebSlot = " <> show slot))
      $ \stmt ->
        SQL.step stmt >> fromIntegral <$> SQL.columnInt64 stmt 0

-- | A pinned EB whose tx closure never arrives cannot be copied, so the
-- copier keeps retrying it for as long as the database is open. 'close' must
-- still return.
test_closeWithPinnedEb :: Assertion
test_closeWithPinnedEb =
  withTempLeiosDbPaths $ \volDbPath immDbPath -> do
    copyFailed <- newEmptyMVar
    let
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
          withRW db $ \con -> rwInsertEbPoint con point (encodeEndorserBlockSize (mkTestEb 2))
          leiosDbPromoteToImmutable db [point]
          takeMVar copyFailed
    case closed of
      Nothing -> assertFailure "close did not return while an EB stayed pinned"
      Just () -> pure ()
 where
  closeTimeoutMicros = 30_000_000

-- * The two implementations agree

-- | An operation on the database: a write, or what the ChainDB has it do as
-- blocks become immutable.
data Op
  = OpPoint LeiosPoint
  | OpBody LeiosPoint [LocalFill]
  | OpTxs LeiosPoint [TxOffset]
  | OpPromote LeiosPoint
  | OpGc SlotNo
  deriving Show

-- | What each endorser block's body is, by its hash, with the index in the
-- pool of each transaction it references: the same hash always names the same
-- body, as it would on the wire.
type Bodies = Map.Map EndorserBlockHash (EndorserBlock, [Int])

-- | Property: running the same operations against both implementations, a
-- reader sees the same in each, and each write completes the same points.
--
-- The node runs on SQLite and its tests run on the in-memory implementation, so
-- those tests say something about the node only as far as the two agree.
--
-- The SQLite implementation copies pinned endorser blocks and sweeps what the
-- garbage collection marked in the background, so how far it has got at any
-- moment depends on its load. The in-memory implementation does both at once.
-- So the SQLite implementation's background work catches up after each
-- operation, before the next.
prop_implementationsAgree :: Property
prop_implementationsAgree =
  forAll genBodies $ \bodies ->
    forAllShrinkShow (genOps bodies) (shrinkList (const [])) show $ \ops ->
      ioProperty $ do
        ((nPinned, nEvicted), inMemory) <-
          withFreshDb InMemory $ \db -> observe db (pure ()) bodies ops
        (_, sqlite) <-
          withFreshSQLiteFile $ \volDbPath _immDbPath db ->
            withSettle volDbPath $ \settle -> observe db settle bodies ops
        pure $
          tabulate "points pinned" [bucket nPinned] $
            tabulate "points evicted" [bucket nEvicted] $
              tabulate "local fills landed" [bucket (sum (map (length . snd) (fst inMemory)))] $
                inMemory === sqlite
 where
  bucket n = if n >= 3 then "3+" else show n

-- | Three endorser blocks over a pool of eight transactions. Sometimes a
-- reference misstates its transaction's size.
genBodies :: Gen Bodies
genBodies =
  fmap Map.fromList $
    forM [1 .. 3] $ \seed -> do
      indices <- chooseInt (1, 4) >>= \n -> replicateM n (chooseInt (1, 8))
      refs <- forM indices $ \i -> do
        let size = fromIntegral (BS.length (poolTxBytes i))
        misstate <- frequency [(9, pure False), (1, pure True)]
        pure (mkTestTxHash (fromIntegral i), if misstate then size + 1 else size)
      pure (mkTestEbHash seed, (mkEndorserBlock refs, indices))

-- | Writes, with tx offsets up to one past the largest body, so that some are
-- not in the body they are written for.
genOps :: Bodies -> Gen [Op]
genOps bodies = concat <$> listOf (frequency [(3, pure <$> genOp), (1, genWhole)])
 where
  genPointOf = elements (allPoints bodies)
  -- Mostly from a source that lists the same tx, which is the only kind that
  -- can land, possibly another point of the same endorser block; otherwise from
  -- anywhere.
  genFill point =
    frequency $
      (1, (,) <$> chooseInt (0, 4) <*> (MkTxLocation <$> genPointOf <*> chooseInt (0, 4)))
        : [ (3, elements matching)
          | let indices = snd (bodies Map.! pointEbHash point)
                matching =
                  [ (dstOff, MkTxLocation src srcOff)
                  | (dstOff, i) <- zip [0 ..] indices
                  , src <- allPoints bodies
                  , src /= point
                  , let srcIndices = snd (bodies Map.! pointEbHash src)
                  , (srcOff, j) <- zip [0 ..] srcIndices
                  , i == j
                  ]
          , not (null matching)
          ]
  genOp =
    frequency
      [ (2, OpPoint <$> genPointOf)
      , (2, genPointOf >>= \point -> OpBody point <$> listOf (genFill point))
      , (3, OpTxs <$> genPointOf <*> sublistOf [0 .. 4])
      , (3, OpPromote <$> genPointOf)
      , (1, OpGc . SlotNo . fromIntegral <$> chooseInt (2, 3))
      ]
  -- A point written whole, as the node writes a point it fetches: its point,
  -- its body, and every tx. So closures complete, and fills and promotions
  -- happen.
  genWhole = do
    point <- genPointOf
    fills <- listOf (genFill point)
    pure [OpPoint point, OpBody point fills, OpTxs point [0 .. 4]]

-- | Every point the operations may name: each endorser block at slot 1 and at
-- slot 2. Few enough that a point's closure often completes, so that fills and
-- promotions happen.
allPoints :: Bodies -> [LeiosPoint]
allPoints bodies = [MkLeiosPoint (SlotNo slot) ebHash | slot <- [1, 2], ebHash <- Map.keys bodies]

-- | A transaction's bytes, by its index in the pool.
poolTxBytes :: Int -> BS.ByteString
poolTxBytes i = BS.replicate (10 + i) (fromIntegral i)

-- | Run the operations, then read back everything a reader can see at each
-- point. Also how many points the operations pinned and evicted.
--
-- The operations run only as far as they meet the node's own preconditions: a
-- body is written once per point, for a point the database holds; and a point
-- is promoted only with a complete closure, as the node pins only what an
-- immutable block certifies, and its ChainSel adopts a block certifying an
-- endorser block only once it holds that point's closure. So this tracks which
-- points the garbage collection evicts: every point older than its slot,
-- pinned or not, since a pinned one has a complete closure, so has been copied.
--
-- A point is also promoted at most once: the node pins only what the immutable
-- chain certifies, and a chain cannot certify the same point twice.
--
-- @settle@ runs after each operation.
observe ::
  LeiosDbHandle IO ->
  IO () ->
  Bodies ->
  [Op] ->
  IO
    ( (Int, Int)
    , ( [(CompletedEbs, [TxOffset])]
      , [ ( [(TxHash, BytesSize)]
          , Maybe [(TxHash, BS.ByteString)]
          , [(Int, TxHash, Maybe BS.ByteString)]
          )
        ]
      )
    )
observe db settle bodies ops = withRW db $ \rw -> do
  (counts, completed) <- go rw [] [] [] 0 ops
  seen <- forM (allPoints bodies) $ \point ->
    (,,)
      <$> rwLookupEbBody rw point
      <*> rwLookupEbClosure rw point
      <*> rwBatchRetrieveTxs
        rw
        point
        [0 .. length (endorserBlockReferences (fst (bodies Map.! pointEbHash point)))]
  pure (counts, (completed, seen))
 where
  go _rw _points _bodied pinned nEvicted [] = pure ((length pinned, nEvicted), [])
  go rw points bodied pinned nEvicted (op : rest) = case op of
    OpPoint point -> do
      rwInsertEbPoint rw point (encodeEndorserBlockSize (fst (bodies Map.! pointEbHash point)))
      settle
      go rw (point : points) bodied pinned nEvicted rest
    OpBody point fills
      | point `elem` points && point `notElem` bodied -> do
          (done, filled) <- rwInsertEbBodyFilling rw point (fst (bodies Map.! pointEbHash point)) fills
          settle
          fmap ((sortOn show done, sort filled) :)
            <$> go rw points (point : bodied) pinned nEvicted rest
      | otherwise -> go rw points bodied pinned nEvicted rest
    OpTxs point offsets -> do
      let indices = snd (bodies Map.! pointEbHash point)
      done <-
        rwInsertTxs
          rw
          point
          [ (offset, poolTxBytes (if offset < length indices then indices !! offset else 1))
          | offset <- offsets
          ]
      settle
      fmap ((sortOn show done, []) :) <$> go rw points bodied pinned nEvicted rest
    OpPromote point
      | point `elem` points && point `notElem` pinned -> do
          complete <- isJust <$> rwLookupEbClosure rw point
          if complete
            then do
              leiosDbPromoteToImmutable db [point]
              settle
              go rw points bodied (point : pinned) nEvicted rest
            else go rw points bodied pinned nEvicted rest
      | otherwise -> go rw points bodied pinned nEvicted rest
    OpGc slot -> do
      garbageCollect db slot
      settle
      let kept p = pointSlotNo p >= slot
      go
        rw
        (filter kept points)
        (filter kept bodied)
        pinned
        (nEvicted + length (filter (not . kept) (nub points)))
        rest

-- | Hand the action a wait for the SQLite implementation's background work to
-- catch up: for its sweep to have evicted every row the garbage collection
-- marked, and the transactions that left orphaned, and for its copier to have
-- copied every pinned endorser block.
withSettle :: FilePath -> (IO () -> IO a) -> IO a
withSettle volDbPath action =
  bracket (SQL.open (T.pack volDbPath)) SQL.close $ \conn -> do
    SQL.exec conn (T.pack "pragma busy_timeout = 1000;")
    bracket (SQL.prepare conn sqlBackgroundWork) SQL.finalize $ \stmt -> do
      let backlog = do
            _ <- SQL.step stmt
            n <- SQL.columnInt64 stmt 0
            SQL.reset stmt
            pure n
          poll =
            backlog >>= \case
              0 -> pure ()
              _ -> threadDelay 1_000 >> poll
          settle =
            Timeout.timeout settleTimeoutMicros poll >>= \case
              Just () -> pure ()
              Nothing -> assertFailure "the SQLite implementation's background work did not catch up"
      action settle
 where
  settleTimeoutMicros = 10_000_000

-- | How many rows the SQLite implementation's background work has yet to deal
-- with: those pinned (status 1) or marked for the sweep (status 3).
sqlBackgroundWork :: T.Text
sqlBackgroundWork =
  T.pack "SELECT COUNT(*) FROM ebs WHERE status IN (1, 3)"
