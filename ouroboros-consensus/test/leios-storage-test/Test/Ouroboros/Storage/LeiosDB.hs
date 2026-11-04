{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Tests for the "Ouroboros.Consensus.Storage.LeiosDB" interface.
--
-- These tests verify the semantics of the LeiosDbHandle operations for both
-- InMemory and SQLite implementations. Each test case uses a fresh database
-- to ensure isolation and serve as an (too optimistic) performance baseline.
module Test.Ouroboros.Storage.LeiosDB (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash), hashToStringAsHex)
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
import Data.Foldable (toList)
import Data.Function ((&))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
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
  , LeiosTx (..)
  , TxHash
  , TxOffset
  , encodeEndorserBlockSize
  , endorserBlockHash
  , endorserBlockReferences
  , forgeLeiosEb
  , forgedEbBody
  , hashLeiosTx
  , mkEndorserBlock
  , serialiseEndorserBlock
  )
import Ouroboros.Consensus.Storage.LeiosDB
  ( LeiosDbHandle (..)
  , LeiosDbReader (..)
  , LeiosDbWriter (..)
  , Promise (..)
  , TraceLeiosDb (..)
  , newLeiosDBInMemory
  , storeForgedEb
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
  , suchThat
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
             [ testCase "evicts an EB all of whose points are older, with its txs" $
                 withFreshDb InMemory test_gcEvictsOldEb
             , testCase "keeps an EB with a point at the slot" $
                 withFreshDb InMemory test_gcKeepsEbWithPointAtSlot
             , testCase "keeps a pinned EB" $
                 withFreshDb InMemory test_gcKeepsPinnedEb
             , testCase "promoting an EB whose closure is incomplete fails" $
                 withFreshDb InMemory test_promoteIncompleteFails
             ]
         , testGroup
             "truncateLeiosDbAfterSlot"
             [ testCase "drops the EBs announced after the slot, with their bodies" $
                 withFreshSQLiteFile test_truncateDropsEbsAfterSlot
             , testCase "runs on the immutable partition" $
                 withTempLeiosDbPaths test_truncateImmutablePartition
             ]
         , testGroup
             "close"
             [ testCase "returns while an EB stays pinned" test_closeWithPinnedEb
             ]
         , testCase "SQLite: the copier does not copy a pinned EB whose txs do not hash to its body" $
             test_copyChecksHashes
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

rwInsertEbBody :: RW -> LeiosPoint -> EndorserBlock -> IO ()
rwInsertEbBody rw point eb = await =<< writeEbBody (rwWriter rw) point eb

rwInsertTxs :: RW -> LeiosPoint -> [(TxOffset, BS.ByteString)] -> IO ()
rwInsertTxs rw point txs = await =<< writeTxs (rwWriter rw) point txs

rwScanEbPoints :: RW -> IO [(SlotNo, EndorserBlockHash)]
rwScanEbPoints = testScanEbPoints . rwReader

rwScanComplete :: RW -> SlotNo -> IO [LeiosPoint]
rwScanComplete = scanCompleteEbClosuresNotOlderThanSlot . rwReader

rwLookupEbBody :: RW -> EndorserBlockHash -> IO (Maybe BS.ByteString)
rwLookupEbBody = lookupEbBody . rwReader

rwLookupEbClosure :: RW -> EndorserBlockHash -> IO (Maybe [BS.ByteString])
rwLookupEbClosure = lookupEbClosure . rwReader

rwBatchRetrieveTxs :: RW -> EndorserBlockHash -> [Int] -> IO [(Int, Maybe BS.ByteString)]
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
  withFreshSQLiteFile $ \volDbPath _immDbPath db -> withSettle volDbPath db action

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
      , testCase "a tx missing from a volatile copy fetched again is read from the immutable copy" $
          test_retrieveFallsBackPerTx impl
      ]
  , testGroup
      "completion"
      [ testCase "a scan finds an EB once each of its txs is written" $
          withFreshDb impl test_completion
      , testCase "a scan does not find an EB whose txs do not hash to what it lists" $
          withFreshDb impl test_completionHashes
      , testCase "a scan never finds an EB with a tx whose bytes are zeroes" $
          withFreshDb impl test_completionZeroTx
      , testCase "a scan finds an EB at its youngest point" $
          withFreshDb impl test_multipleSlotsSameHash
      , testCase "a scan finds a complete EB at a point written after the closure completed" $
          withFreshDb impl test_scanFindsLaterPoint
      ]
  , testGroup
      "garbage collection of an EB a newer point keeps"
      [ testCase "drops its older points and still finds it complete" $
          withSettledDb impl test_gcDropsOlderPoints
      , testCase "finds its closure complete after dropping the point its body was written at" $
          withSettledDb impl test_gcKeepsMissingTxCount
      , testCase "keeps only the newest point of an EB announced again in each round" $
          withSettledDb impl test_gcBoundsPointsOfReannouncedEb
      ]
  , testGroup
      "lookupEbClosure"
      [ testProperty "complete EB returns its txs" $ prop_completedEbComplete impl
      , testProperty "an EB with txs missing returns Nothing" $ prop_completedEbPartialTxs impl
      , testProperty "no body returns Nothing" $ prop_completedEbNoBody impl
      ]
  , testGroup
      "storeForgedEb"
      [ testProperty "stores the point, the body and the txs of the endorser block" $
          prop_storeForgedEbThenLookup impl
      ]
  ]

-- * QuickCheck generators

-- | Generate a random EndorserBlockHash (32 random bytes).
-- With 256 bits of randomness, collisions are practically impossible.
genEbHash :: Gen EndorserBlockHash
genEbHash = UnsafeHash . SBS.pack <$> vector 32

-- | Generate a random SlotNo.
genSlotNo :: Gen SlotNo
genSlotNo = SlotNo . fromIntegral <$> chooseInt (0, maxBound)

-- | Generate a random LeiosPoint.
genPoint :: Gen LeiosPoint
genPoint = MkLeiosPoint <$> genSlotNo <*> genEbHash

-- | Generate a EndorserBlock with the given number of transactions.
genEb :: Int -> Gen EndorserBlock
genEb numTxs = ebOfSizes <$> replicateM numTxs (fromIntegral <$> chooseInt (50, 500))

-- | Generate a LeiosPoint and EndorserBlock together with the given number of transactions.
genPointAndEb :: Int -> Gen (LeiosPoint, EndorserBlock)
genPointAndEb numTxs = (,) <$> genPoint <*> genEb numTxs

-- | Transaction bytes, distinct from each other, and none all zeroes, which no
-- valid tx is.
genDistinctTxBytes :: Gen (NonEmpty BS.ByteString)
genDistinctTxBytes = NE.nub <$> ((:|) <$> genTxBytes <*> listOf genTxBytes)
 where
  genTxBytes = (chooseInt (1, 32) >>= fmap BS.pack . vector) `suchThat` BS.any (/= 0)

-- | The bytes of the test tx at this offset: of this size, and distinct per
-- offset.
testTxBytes :: TxOffset -> BytesSize -> BS.ByteString
testTxBytes off size = BS.replicate (fromIntegral size) (fromIntegral (off `mod` 255) + 1)

-- | The hash of 'testTxBytes'.
testTxHash :: TxOffset -> BytesSize -> TxHash
testTxHash off size = hashLeiosTx (MkLeiosTx (testTxBytes off size))

-- | An EB naming txs of these sizes, each 'testTxBytes' at its offset.
ebOfSizes :: [BytesSize] -> EndorserBlock
ebOfSizes sizes = mkEndorserBlock [(testTxHash off size, size) | (off, size) <- zip [0 ..] sizes]

-- | Bytes for the tx at the given offset of the EB: of its declared size, as
-- a write must be to be accepted, and hashing to its declared hash if the EB is
-- 'ebOfSizes'.
txBytesFor :: EndorserBlock -> TxOffset -> BS.ByteString
txBytesFor eb off = testTxBytes off size
 where
  (_, size) = endorserBlockReferences eb !! off

-- | Every tx of the EB, with 'txBytesFor'.
allTxsFor :: EndorserBlock -> [(TxOffset, BS.ByteString)]
allTxsFor eb = [(off, txBytesFor eb off) | off <- [0 .. length (endorserBlockReferences eb) - 1]]

-- | The bytes the EB arrived in, as 'lookupEbBody' returns them.
bodyBytes :: EndorserBlock -> BS.ByteString
bodyBytes = serialiseEndorserBlock

-- * Test fixtures for unit tests

-- | Create a simple test EndorserBlockHash from a seed byte.
mkTestEbHash :: Word -> EndorserBlockHash
mkTestEbHash seed = UnsafeHash $ SBS.pack $ replicate 32 (fromIntegral seed)

-- | Create a test LeiosPoint.
mkTestPoint :: SlotNo -> Word -> LeiosPoint
mkTestPoint slot seed = MkLeiosPoint slot (mkTestEbHash seed)

-- | Create a test EndorserBlock with the given number of transactions.
mkTestEb :: Int -> EndorserBlock
mkTestEb numTxs = ebOfSizes [100 + fromIntegral i | i <- [0 .. numTxs - 1]]

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
        let expected = Just (bodyBytes eb)
        rwInsertEbPoint con point (encodeEndorserBlockSize eb)
        (_, insertTime) <- timed $ rwInsertEbBody con point eb
        (result, lookupTime) <- timed $ rwLookupEbBody con (pointEbHash point)
        pure $
          result == expected
            & counterexample ("Expected: " ++ show expected ++ "\nGot: " ++ show result)
            & tabulate "insertEbBody" [timeBucket insertTime]
            & tabulate "lookupEbBody" [timeBucket lookupTime]

-- | Property: looking up a non-existent EB returns 'Nothing'.
prop_ebsLookupMissing :: DbImpl -> Property
prop_ebsLookupMissing impl =
  forAll genEbHash $ \missingHash ->
    ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
      (result, lookupTime) <- timed $ rwLookupEbBody con missingHash
      pure $
        result === Nothing
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
          (results, retrieveTime) <- timed $ rwBatchRetrieveTxs con (pointEbHash point) allOffsets
          -- Check that inserted txs have their bytes, others are missing
          let checkResult (off, mbBytes) =
                if off `elem` offsetsToInsert
                  then mbBytes == Just (txBytesFor eb off)
                  else mbBytes == Nothing
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

-- * Completion tests

-- | The scan finds an EB only once each of its txs is written, and only that
-- EB.
test_completion :: LeiosDbHandle IO -> IO ()
test_completion db = do
  let point = mkTestPoint (SlotNo 1) 1
      eb = mkTestEb 3
      otherPoint = mkTestPoint (SlotNo 2) 2
      otherEb = mkTestEb 2
  withRW db $ \con -> do
    rwInsertEbPoint con point (encodeEndorserBlockSize eb)
    rwInsertEbPoint con otherPoint (encodeEndorserBlockSize otherEb)
    void $ rwInsertEbBody con point eb
    void $ rwInsertEbBody con otherPoint otherEb
    rwInsertTxs con point (take 2 (allTxsFor eb))
    rwScanComplete con 0 >>= (@?= [])
    rwInsertTxs con point (drop 2 (allTxsFor eb))
    rwScanComplete con 0 >>= (@?= [point])

-- | Bytes of the declared sizes that do not hash to the declared txs are
-- written, but do not complete the closure.
test_completionHashes :: LeiosDbHandle IO -> IO ()
test_completionHashes db = do
  let point = mkTestPoint (SlotNo 1) 1
      eb = mkTestEb 2
  withRW db $ \con -> do
    rwInsertEbPoint con point (encodeEndorserBlockSize eb)
    void $ rwInsertEbBody con point eb
    rwInsertTxs con point [(off, BS.map (+ 1) bytes) | (off, bytes) <- allTxsFor eb]
    rwScanComplete con 0 >>= (@?= [])

-- | A tx whose bytes are zeroes, which no valid tx is, never counts as
-- written: zeroes are what the closure holds before a tx is written.
test_completionZeroTx :: LeiosDbHandle IO -> IO ()
test_completionZeroTx db = do
  let point = mkTestPoint (SlotNo 1) 1
      zero = BS.singleton 0
      eb = mkEndorserBlock [(hashLeiosTx (MkLeiosTx zero), 1)]
  withRW db $ \con -> do
    rwInsertEbPoint con point (encodeEndorserBlockSize eb)
    void $ rwInsertEbBody con point eb
    rwScanComplete con 0 >>= (@?= [])
    rwLookupEbClosure con (pointEbHash point) >>= (@?= Nothing)
    rwInsertTxs con point [(0, zero)]
    rwScanComplete con 0 >>= (@?= [])
    rwLookupEbClosure con (pointEbHash point) >>= (@?= Nothing)

-- | The same EB content can be announced at several slots; the DB keeps each
-- 'LeiosPoint', and the scan finds the EB at its youngest.
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
    rwInsertTxs con point1 (allTxsFor eb)
    rwScanComplete con 0 >>= (@?= [point2])

-- | An EB announced again after its closure completed gets only the new point
-- written. Once the scan's slot is past the point it completed at, the scan
-- still finds it, at the new point.
test_scanFindsLaterPoint :: LeiosDbHandle IO -> IO ()
test_scanFindsLaterPoint db = do
  let older = mkTestPoint (SlotNo 10) 1
      younger = mkTestPoint (SlotNo 30) 1
      eb = mkTestEb 2
  withRW db $ \con -> do
    rwInsertEbPoint con older (encodeEndorserBlockSize eb)
    void $ rwInsertEbBody con older eb
    rwInsertTxs con older (allTxsFor eb)
    rwInsertEbPoint con younger (encodeEndorserBlockSize eb)
    rwScanComplete con 20 >>= (@?= [younger])

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

-- | A tx write only fills a tx the body declares, at its declared size, the
-- first of a write winning. Anything else changes nothing.
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
    rwInsertTxs
      con
      point
      [ (2, txBytesFor eb 0)
      , (0, BS.snoc (txBytesFor eb 0) 0)
      , (1, BS.take 1 (txBytesFor eb 1))
      ]
    retrieved <- rwBatchRetrieveTxs con (pointEbHash point) offsets
    retrieved @?= [(off, Nothing) | off <- offsets]
    -- Within one write, the first delivery wins.
    rwInsertTxs con point [(0, txBytesFor eb 0), (0, BS.map (+ 1) (txBytesFor eb 0))]
    retrieved' <- rwBatchRetrieveTxs con (pointEbHash point) [0]
    retrieved' @?= [(0, Just (txBytesFor eb 0))]
    rwInsertTxs con point [(1, txBytesFor eb 1)]
    closure <- rwLookupEbClosure con (pointEbHash point)
    closure @?= Just (map snd (allTxsFor eb))

-- * Garbage collection

-- | Write the EB at these points, with its body and all its transactions.
writeWholeEb :: RW -> [LeiosPoint] -> EndorserBlock -> IO ()
writeWholeEb con points eb = do
  forM_ points $ \point -> rwInsertEbPoint con point (encodeEndorserBlockSize eb)
  forM_ points $ \point -> void $ rwInsertEbBody con point eb
  forM_ (take 1 points) $ \point -> rwInsertTxs con point (allTxsFor eb)

-- | An EB copied to the immutable partition, evicted from the volatile one, and
-- then fetched again at a new point: a tx the new volatile copy lacks is still
-- read from the immutable copy.
test_retrieveFallsBackPerTx :: DbImpl -> IO ()
test_retrieveFallsBackPerTx impl = withSettledDb impl $ \db settle -> withRW db $ \con -> do
  let eb = mkTestEb 2
      again = mkTestPoint 10 1
  writeWholeEb con [mkTestPoint 5 1] eb
  leiosDbPromoteToImmutable db [mkTestPoint 5 1]
  settle
  garbageCollect db 6
  settle
  rwInsertEbPoint con again (encodeEndorserBlockSize eb)
  void $ rwInsertEbBody con again eb
  rwInsertTxs con again (take 1 (allTxsFor eb))
  rwBatchRetrieveTxs con (mkTestEbHash 1) [0, 1]
    >>= (@?= [(off, Just (txBytesFor eb off)) | off <- [0, 1]])
  rwLookupEbClosure con (mkTestEbHash 1) >>= (@?= Just (map snd (allTxsFor eb)))

-- | Whether the reader sees the EB's whole closure.
holdsClosure :: RW -> EndorserBlockHash -> IO Bool
holdsClosure con ebHash = isJust <$> rwLookupEbClosure con ebHash

test_gcEvictsOldEb :: LeiosDbHandle IO -> IO ()
test_gcEvictsOldEb db = withRW db $ \con -> do
  -- The old EB references transactions 0, 1 and 2, and the other only 0 and 1.
  writeWholeEb con [mkTestPoint 5 1] (mkTestEb 3)
  writeWholeEb con [mkTestPoint 10 2] (mkTestEb 2)
  garbageCollect db 6
  rwLookupEbBody con (mkTestEbHash 1) >>= (@?= Nothing)
  holdsClosure con (mkTestEbHash 1) >>= (@?= False)
  rwScanEbPoints con >>= (@?= [(10, mkTestEbHash 2)])
  holdsClosure con (mkTestEbHash 2) >>= (@?= True)
  -- The old EB's transactions went with it, so a new EB referencing them
  -- lacks them.
  rwInsertEbPoint con (mkTestPoint 20 3) (encodeEndorserBlockSize (mkTestEb 3))
  void $ rwInsertEbBody con (mkTestPoint 20 3) (mkTestEb 3)
  holdsClosure con (mkTestEbHash 3) >>= (@?= False)

-- | An EB written whole at slot 5 and announced again at slot 10, collected at
-- slot 6.
test_gcDropsOlderPoints :: LeiosDbHandle IO -> IO () -> IO ()
test_gcDropsOlderPoints db settle = withRW db $ \con -> do
  let eb = mkTestEb 2
      again = mkTestPoint 10 1
  writeWholeEb con [mkTestPoint 5 1] eb
  rwInsertEbPoint con again (encodeEndorserBlockSize eb)
  garbageCollect db 6
  settle
  rwScanEbPoints con >>= (@?= [(10, mkTestEbHash 1)])
  holdsClosure con (mkTestEbHash 1) >>= (@?= True)
  rwScanComplete con 6 >>= (@?= [again])

-- | An EB whose body was written at slot 5, and one of whose two transactions
-- arrived, is announced again at slot 10 and collected at slot 6. Then its
-- other transaction arrives.
test_gcKeepsMissingTxCount :: LeiosDbHandle IO -> IO () -> IO ()
test_gcKeepsMissingTxCount db settle = withRW db $ \con -> do
  let eb = mkTestEb 2
      older = mkTestPoint 5 1
      again = mkTestPoint 10 1
  rwInsertEbPoint con older (encodeEndorserBlockSize eb)
  void $ rwInsertEbBody con older eb
  rwInsertTxs con older (take 1 (allTxsFor eb))
  rwInsertEbPoint con again (encodeEndorserBlockSize eb)
  garbageCollect db 6
  settle
  rwInsertTxs con again (drop 1 (allTxsFor eb))
  rwScanComplete con 6 >>= (@?= [again])

-- | An EB written whole at slot 10 is announced again in each of nine more
-- rounds, each followed by a collection at its slot.
test_gcBoundsPointsOfReannouncedEb :: LeiosDbHandle IO -> IO () -> IO ()
test_gcBoundsPointsOfReannouncedEb db settle = withRW db $ \con -> do
  let eb = mkTestEb 2
  writeWholeEb con [mkTestPoint 10 1] eb
  forM_ [2 .. 10] $ \n -> do
    rwInsertEbPoint con (mkTestPoint (10 * n) 1) (encodeEndorserBlockSize eb)
    garbageCollect db (10 * n)
    settle
  rwScanEbPoints con >>= (@?= [(100, mkTestEbHash 1)])
  holdsClosure con (mkTestEbHash 1) >>= (@?= True)

test_gcKeepsEbWithPointAtSlot :: LeiosDbHandle IO -> IO ()
test_gcKeepsEbWithPointAtSlot db = withRW db $ \con -> do
  writeWholeEb con [mkTestPoint 5 1, mkTestPoint 6 1] (mkTestEb 2)
  garbageCollect db 6
  holdsClosure con (mkTestEbHash 1) >>= (@?= True)

test_gcKeepsPinnedEb :: LeiosDbHandle IO -> IO ()
test_gcKeepsPinnedEb db = withRW db $ \con -> do
  writeWholeEb con [mkTestPoint 5 1] (mkTestEb 2)
  leiosDbPromoteToImmutable db [mkTestPoint 5 1]
  garbageCollect db 6
  holdsClosure con (mkTestEbHash 1) >>= (@?= True)

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

-- | Property: a complete EB returns its txs' bytes in body order.
prop_completedEbComplete :: DbImpl -> Property
prop_completedEbComplete impl =
  forAllShrinkShow (chooseInt (1, 20)) shrink show $ \numTxs ->
    forAllBlind (genPointAndEb numTxs) $ \(point, eb) ->
      ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
        rwInsertEbPoint con point (encodeEndorserBlockSize eb)
        void $ rwInsertEbBody con point eb
        rwInsertTxs con point (allTxsFor eb)
        (result, queryTime) <- timed $ rwLookupEbClosure con (pointEbHash point)
        pure $
          result === Just (map snd (allTxsFor eb))
            & tabulate "lookupEbClosure (complete)" [timeBucket queryTime]
            & tabulate "numTxs" [magnitudeBucket numTxs]

-- | Property: an EB with some txs missing has no closure, until they are
-- written.
prop_completedEbPartialTxs :: DbImpl -> Property
prop_completedEbPartialTxs impl =
  forAllShrinkShow (chooseInt (1, 20)) shrink show $ \numTxs ->
    forAllBlind (genPointAndEb numTxs) $ \(point, eb) ->
      forAllBlind (sublistOf [0 .. numTxs - 1]) $ \offsetsToInsert ->
        ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
          rwInsertEbPoint con point (encodeEndorserBlockSize eb)
          void $ rwInsertEbBody con point eb
          rwInsertTxs con point [(off, txBytesFor eb off) | off <- offsetsToInsert]
          (result, queryTime) <- timed $ rwLookupEbClosure con (pointEbHash point)
          let expected
                | length offsetsToInsert == numTxs = Just (map snd (allTxsFor eb))
                | otherwise = Nothing
          pure $
            result === expected
              & counterexample ("Inserted offsets: " ++ show offsetsToInsert)
              & tabulate "lookupEbClosure (partial txs)" [timeBucket queryTime]
              & tabulate "numTxs" [magnitudeBucket numTxs]

-- | Property: EB with only a point announced (no body) returns Nothing.
prop_completedEbNoBody :: DbImpl -> Property
prop_completedEbNoBody impl =
  forAll genPoint $ \point ->
    ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
      rwInsertEbPoint con point 1000
      (result, queryTime) <- timed $ rwLookupEbClosure con (pointEbHash point)
      pure $
        result === Nothing
          & counterexample "Expected Nothing for EB with no body inserted"
          & tabulate "lookupEbClosure (no body)" [timeBucket queryTime]

-- * truncateLeiosDbAfterSlot

test_truncateDropsEbsAfterSlot :: FilePath -> FilePath -> LeiosDbHandle IO -> IO ()
test_truncateDropsEbsAfterSlot volDbPath _immDbPath db = do
  let eb = mkTestEb 2
      keptHash = mkTestEbHash 1
      droppedHash = mkTestEbHash 2
  withRW db $ \con -> do
    rwInsertEbPoint con (MkLeiosPoint 5 keptHash) (encodeEndorserBlockSize eb)
    void $ rwInsertEbBody con (MkLeiosPoint 5 keptHash) eb
    -- The kept EB is announced again at a slot the truncation drops.
    rwInsertEbPoint con (MkLeiosPoint 15 keptHash) (encodeEndorserBlockSize eb)
    rwInsertEbPoint con (MkLeiosPoint 15 droppedHash) (encodeEndorserBlockSize eb)
    void $ rwInsertEbBody con (MkLeiosPoint 15 droppedHash) eb
    rwInsertTxs con (MkLeiosPoint 5 keptHash) (allTxsFor eb)
    rwInsertTxs con (MkLeiosPoint 15 droppedHash) (allTxsFor eb)

  truncateLeiosDbAfterSlot volDbPath 10
  ebBodyRows volDbPath droppedHash >>= (@?= 0)

  withRW db $ \con -> do
    points <- rwScanEbPoints con
    points @?= [(5, keptHash)]
    keptBody <- rwLookupEbBody con keptHash
    keptBody @?= Just (bodyBytes eb)
    droppedBody <- rwLookupEbBody con droppedHash
    droppedBody @?= Nothing
    keptClosure <- rwLookupEbClosure con keptHash
    keptClosure @?= Just (map snd (allTxsFor eb))

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
      forM_ ["ebs", "idx_ebs_ebHashBytes"] $ \name ->
        assertBool (file <> " has " <> name) (name `elem` objs)
    forM_ ["pinned"] $ \name -> do
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

-- | An EB hash as an SQL blob literal.
sqlBlob :: EndorserBlockHash -> String
sqlBlob ebHash = "x'" <> hashToStringAsHex ebHash <> "'"

-- | How many rows of the file hold the EB's body.
ebBodyRows :: FilePath -> EndorserBlockHash -> IO Int
ebBodyRows path ebHash =
  queryCount
    path
    ("SELECT COUNT(*) FROM ebs WHERE ebHashBytes = " <> sqlBlob ebHash <> " AND txCount IS NOT NULL")

queryCount :: FilePath -> String -> IO Int
queryCount path query =
  withSqlFile path $ \sql ->
    SQL.withStatement sql (T.pack query) $ \stmt ->
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

-- | A pinned EB whose txs are written but do not hash to its body is not
-- copied to the immutable partition, where the scan would take it as complete.
test_copyChecksHashes :: Assertion
test_copyChecksHashes =
  withTempLeiosDbPaths $ \volDbPath immDbPath -> do
    copyFailed <- newEmptyMVar
    let tracer = Tracer . emit $ \case
          TraceLeiosDbCopyError{} -> void $ tryPutMVar copyFailed ()
          _ -> pure ()
        point = mkTestPoint 5 1
        eb = mkTestEb 2
    withLeiosDBSQLite tracer volDbPath immDbPath $ \db -> withRW db $ \con -> do
      rwInsertEbPoint con point (encodeEndorserBlockSize eb)
      void $ rwInsertEbBody con point eb
      rwInsertTxs con point [(off, BS.map (+ 1) bytes) | (off, bytes) <- allTxsFor eb]
      leiosDbPromoteToImmutable db [point]
      Timeout.timeout 30_000_000 (takeMVar copyFailed)
        >>= maybe (assertFailure "the copier did not refuse the EB") pure
      rwScanComplete con 0 >>= (@?= [])

-- * The two implementations agree

-- | An operation on the database: a write, or what the ChainDB has it do as
-- blocks become immutable.
data Op
  = OpPoint LeiosPoint
  | OpBody LeiosPoint
  | OpTxs LeiosPoint [TxOffset]
  | OpPromote EndorserBlockHash
  | OpGc SlotNo
  deriving Show

-- | What each endorser block's body is, by its hash, with the index in the
-- pool of each transaction it references: the same hash always names the same
-- body, as it would on the wire.
type Bodies = Map.Map EndorserBlockHash (EndorserBlock, [Int])

-- | 'storeForgedEb' writes the endorser block at the point of the given slot
-- and the hash of the body. After the writer closes, a reader sees that point
-- and the bytes of all its txs.
prop_storeForgedEbThenLookup :: DbImpl -> Property
prop_storeForgedEbThenLookup impl =
  forAll genSlotNo $ \slot ->
    forAll genDistinctTxBytes $ \txs ->
      ioProperty $ withFreshDb impl $ \db -> do
        let forged = forgeLeiosEb txs
            body = forgedEbBody forged
            ebHash = endorserBlockHash body
        withWriter db $ \writer -> storeForgedEb writer slot forged
        (points, closure) <-
          withReader db $ \reader ->
            (,) <$> testScanEbPoints reader <*> lookupEbClosure reader ebHash
        pure $
          conjoin
            [ counterexample "the point is not the slot and the hash of the body" $
                points === [(slot, ebHash)]
            , counterexample "the stored txs are not the forged ones" $
                closure === Just (toList txs)
            ]

-- | Property: running the same operations against both implementations, a
-- reader sees the same in each, and the scan finds the same endorser blocks
-- complete.
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
            withSettle volDbPath db $ \db' settle -> observe db' settle bodies ops
        pure $
          tabulate "endorser blocks pinned" [bucket nPinned] $
            tabulate "endorser blocks evicted" [bucket nEvicted] $
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
        pure (hashLeiosTx (MkLeiosTx (poolTxBytes i)), if misstate then size + 1 else size)
      pure (mkTestEbHash seed, (mkEndorserBlock refs, indices))

-- | Writes, with tx offsets up to one past the largest body, so that some are
-- not in the body they are written for.
genOps :: Bodies -> Gen [Op]
genOps bodies = listOf genOp
 where
  genSlot = SlotNo . fromIntegral <$> chooseInt (1, 4)
  genPointOf = MkLeiosPoint <$> genSlot <*> elements (Map.keys bodies)
  genOp =
    frequency
      [ (2, OpPoint <$> genPointOf)
      , (2, OpBody <$> genPointOf)
      , (3, OpTxs <$> genPointOf <*> sublistOf [0 .. 4])
      , (3, OpPromote <$> elements (Map.keys bodies))
      , (1, OpGc . (+ 1) <$> genSlot)
      ]

-- | A transaction's bytes, by its index in the pool.
poolTxBytes :: Int -> BS.ByteString
poolTxBytes i = BS.replicate (10 + i) (fromIntegral i)

-- | Run the operations, then read back everything a reader can see of each
-- endorser block. Also how many endorser blocks the operations pinned and
-- evicted.
--
-- The operations run only as far as they meet the node's own preconditions: a
-- body is written once per point, for a point the database holds, where a
-- point the garbage collection carried an older point's body write over to
-- counts as written, since the node then holds the endorser block; and an
-- endorser block is promoted only with a complete closure, as the node pins
-- only what an immutable block certifies, and its ChainSel adopts a block
-- certifying an endorser block only once it holds that endorser block's
-- closure. So this tracks which points and
-- bodies the garbage collection evicts: those of every endorser block all of
-- whose points are older than its slot, pinned or not, since a pinned one has
-- a complete closure, so has been copied; and every other point older than its
-- slot.
--
-- An endorser block is also promoted at most once: the node pins only what the
-- immutable chain certifies, and a chain cannot certify the same endorser
-- block twice, since it cannot be valid twice.
--
-- @settle@ runs after each operation.
observe ::
  LeiosDbHandle IO ->
  IO () ->
  Bodies ->
  [Op] ->
  IO
    ( (Int, Int)
    , ( [ ( Maybe BS.ByteString
          , Maybe [BS.ByteString]
          , [(Int, Maybe BS.ByteString)]
          )
        ]
      , [LeiosPoint]
      )
    )
observe db settle bodies ops = withRW db $ \rw -> do
  counts <- go rw [] [] [] 0 ops
  seen <- forM (Map.toList bodies) $ \(ebHash, (eb, _)) ->
    (,,)
      <$> rwLookupEbBody rw ebHash
      <*> rwLookupEbClosure rw ebHash
      <*> rwBatchRetrieveTxs rw ebHash [0 .. length (endorserBlockReferences eb)]
  complete <- rwScanComplete rw 0
  pure (counts, (seen, complete))
 where
  go _rw _points _bodied pinned nEvicted [] = pure (length pinned, nEvicted)
  go rw points bodied pinned nEvicted (op : rest) = case op of
    OpPoint point -> do
      rwInsertEbPoint rw point (encodeEndorserBlockSize (fst (bodies Map.! pointEbHash point)))
      settle
      go rw (point : points) bodied pinned nEvicted rest
    OpBody point
      | point `elem` points && point `notElem` bodied -> do
          rwInsertEbBody rw point (fst (bodies Map.! pointEbHash point))
          settle
          go rw points (point : bodied) pinned nEvicted rest
      | otherwise -> go rw points bodied pinned nEvicted rest
    OpTxs point offsets -> do
      let indices = snd (bodies Map.! pointEbHash point)
      rwInsertTxs
        rw
        point
        [ (offset, poolTxBytes (if offset < length indices then indices !! offset else 1))
        | offset <- offsets
        ]
      settle
      go rw points bodied pinned nEvicted rest
    OpPromote ebHash -> case [p | p <- points, pointEbHash p == ebHash, ebHash `notElem` pinned] of
      point : _ -> do
        complete <- holdsClosure rw ebHash
        if complete
          then do
            leiosDbPromoteToImmutable db [point]
            settle
            go rw points bodied (nub (ebHash : pinned)) nEvicted rest
          else go rw points bodied pinned nEvicted rest
      [] -> go rw points bodied pinned nEvicted rest
    OpGc slot -> do
      garbageCollect db slot
      settle
      let newest ebHash = maximum [pointSlotNo p | p <- points, pointEbHash p == ebHash]
          evicted =
            nub [ebHash | p <- points, let ebHash = pointEbHash p, newest ebHash < slot]
          kept p = pointSlotNo p >= slot
          carried =
            [ q
            | p <- bodied
            , not (kept p)
            , q <- points
            , pointEbHash q == pointEbHash p
            , kept q
            ]
      go
        rw
        (filter kept points)
        (nub (filter kept bodied <> carried))
        pinned
        (nEvicted + length evicted)
        rest

-- | Hand the action a wait for the SQLite implementation's background work to
-- catch up: for its sweep to have deleted every row it collects, and for its
-- copier to have copied every pinned point. The action gets the handle back,
-- recording what each garbage collection collects: the rows before its slot,
-- written before it ran.
withSettle ::
  FilePath -> LeiosDbHandle IO -> (LeiosDbHandle IO -> IO () -> IO a) -> IO a
withSettle volDbPath db action = do
  frontiersVar <- newIORef []
  bracket (SQL.open (T.pack volDbPath)) SQL.close $ \conn -> do
    SQL.exec conn (T.pack "pragma busy_timeout = 1000;")
    let query sql k = SQL.withStatement conn (T.pack sql) $ \stmt ->
          let loop acc =
                SQL.step stmt >>= \case
                  SQL.Row -> k stmt >>= loop . (: acc)
                  SQL.Done -> pure acc
           in loop []
        collect slot = do
          rowId <- sum <$> query "SELECT coalesce(max(id), 0) FROM ebs" (`SQL.columnInt64` 0)
          modifyIORef' frontiersVar ((unSlotNo slot, rowId) :)
          garbageCollect db slot
        -- The pinned points, and the rows some garbage collection collects.
        backlog = do
          frontiers <- readIORef frontiersVar
          pinned <- sum <$> query "SELECT COUNT(*) FROM pinned" (`SQL.columnInt64` 0)
          rows <-
            query
              "SELECT ebSlot, id FROM ebs WHERE ebHashBytes NOT IN (SELECT ebHashBytes FROM pinned)"
              (\stmt -> (,) <$> SQL.columnInt64 stmt 0 <*> SQL.columnInt64 stmt 1)
          pure $
            pinned
              + fromIntegral
                (length [() | (slot, rowId) <- rows, any (\(s, r) -> slot < fromIntegral s && rowId <= r) frontiers])
        poll =
          backlog >>= \case
            0 -> pure ()
            _ -> threadDelay 1_000 >> poll
        settle =
          Timeout.timeout settleTimeoutMicros poll >>= \case
            Just () -> pure ()
            Nothing -> assertFailure "the SQLite implementation's background work did not catch up"
    action db{garbageCollect = collect} settle
 where
  settleTimeoutMicros = 10_000_000
