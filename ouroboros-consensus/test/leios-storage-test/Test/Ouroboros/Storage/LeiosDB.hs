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
import Control.Concurrent.Class.MonadSTM.Strict (atomically, newTVarIO, writeTVar)
import Control.DeepSeq (force)
import Control.Exception (bracket, try)
import Control.Monad (forM, forM_, replicateM, void)
import Control.Monad.Class.MonadTime.SI (diffTime, getMonotonicTime)
import Control.Tracer (nullTracer)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Foldable (toList)
import Data.Function ((&))
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
  , TxLocation (..)
  , TxOffset
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
  , LocalFill
  , Promise (..)
  , newLeiosDBInMemory
  , storeForgedEb
  , truncateLeiosDbAfterSlot
  , withLeiosDBSQLite
  , withReader
  , withWriter
  )
import Ouroboros.Consensus.Storage.LeiosDB.Exception (LeiosDbException)
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Connection
  ( closeChecked
  , mkConn
  , openRawConnection
  )
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Maintenance (gcBatchSize, initialStats)
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Read (lookupBlob, readPartition)
import System.Directory (doesFileExist, removeDirectoryRecursive)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
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
         , testGroup
             "truncateLeiosDbAfterSlot"
             [ testCase "drops the EBs announced after the slot, with their bodies" $
                 withFreshSQLiteFile test_truncateDropsEbsAfterSlot
             , testCase "runs on the immutable partition" $
                 withTempLeiosDbPaths test_truncateImmutablePartition
             ]
         , testGroup
             "reads (SQLite)"
             [ testCase "a read a collection overtook reads the immutable partition" $
                 withFreshSQLiteFile test_readOvertakenByCollection
             ]
         , testGroup
             "promotion (SQLite)"
             [ testCase "promotes an EB again after a crash lost the volatile txs that completed it" $
                 withTempLeiosDbPaths test_promoteAgainAfterLosingVolatileTxs
             , testCase "promotes an EB at the certified point, not at another point of it" $
                 withFreshSQLiteFile test_promoteAtCertifiedSlot
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
rwInsertEbBody :: RW -> LeiosPoint -> EndorserBlock -> IO ()
rwInsertEbBody rw point eb = void $ rwInsertEbBodyFilling rw point eb []

-- | Also fill from local bytes, and return the offsets filled.
rwInsertEbBodyFilling :: RW -> LeiosPoint -> EndorserBlock -> [LocalFill] -> IO [TxOffset]
rwInsertEbBodyFilling rw point eb fills = await =<< writeEbBody (rwWriter rw) point eb fills

rwInsertTxs :: RW -> LeiosPoint -> [(TxOffset, BS.ByteString)] -> IO ()
rwInsertTxs rw point txs = void $ rwInsertTxsDropped rw point txs

-- | Also the offsets dropped for their size.
rwInsertTxsDropped :: RW -> LeiosPoint -> [(TxOffset, BS.ByteString)] -> IO [TxOffset]
rwInsertTxsDropped rw point txs = await =<< writeTxs (rwWriter rw) point txs

rwScanEbPoints :: RW -> IO [(SlotNo, EndorserBlockHash)]
rwScanEbPoints = testScanEbPoints . rwReader

rwScanComplete :: RW -> SlotNo -> IO [LeiosPoint]
rwScanComplete = scanCompleteEbClosuresNotOlderThanSlot . rwReader

rwLookupEbBody :: RW -> LeiosPoint -> IO (Maybe BS.ByteString)
rwLookupEbBody = lookupEbBody . rwReader

rwLookupEbClosure :: RW -> LeiosPoint -> IO (Maybe [BS.ByteString])
rwLookupEbClosure = lookupEbClosure . rwReader

rwBatchRetrieveTxs :: RW -> LeiosPoint -> [Int] -> IO [(Int, Maybe BS.ByteString)]
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
      , testCase "a body write records its point" $
          withFreshDb impl test_ebBodyWritesPoint
      , testCase "a body written again reports no txs, but keeps them" $
          withFreshDb impl test_ebBodyAgainKeepsTxs
      , testCase "a point holds only what is written at it" $
          withFreshDb impl test_readsAtPoint
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
      , testCase "reads a point older than the latest collection from the immutable partition only" $
          test_readsDispatchBySlot impl
      ]
  , testGroup
      "local fills"
      [ testCase "a body fills a tx from another endorser block's bytes" $
          withFreshDb impl test_fillFromAnotherEb
      , testCase "a fill from a source without the bytes fills nothing" $
          withFreshDb impl test_fillWithoutSourceBytes
      , testCase "a body whose fills complete its closure completes it" $
          withFreshDb impl test_fillCompletesClosure
      , testCase "a body fills its txs from another point of the same endorser block" $
          withFreshDb impl test_fillFromAnotherPoint
      ]
  , testGroup
      "completion"
      [ testCase "a scan finds an EB once each of its txs is written" $
          withFreshDb impl test_completion
      , testCase "a scan does not find an EB whose txs do not hash to what it lists" $
          withFreshDb impl test_completionHashes
      , testCase "a scan never finds an EB with a tx whose bytes are zeroes" $
          withFreshDb impl test_completionZeroTx
      , testCase "a scan finds each point of an EB whose closure is complete" $
          withFreshDb impl test_multipleSlotsSameHash
      , testCase "a scan finds a point written after another completed, once complete itself" $
          withFreshDb impl test_scanFindsLaterPoint
      ]
  , testGroup
      "garbage collection"
      [ testCase "evicts an EB all of whose points are older, with its txs" $
          withFreshDb impl test_gcEvictsOldEb
      , testCase "keeps an EB with a point at the slot" $
          withFreshDb impl test_gcKeepsEbWithPointAtSlot
      , testCase "collects more points than one job deletes" $
          withFreshDb impl test_gcCollectsManyPoints
      ]
  , testGroup
      "garbage collection of an EB a newer point keeps"
      [ testCase "drops its older points and still finds it complete" $
          withFreshDb impl test_gcDropsOlderPoints
      , testCase "keeps only the newest point of an EB announced again in each round" $
          withFreshDb impl test_gcBoundsPointsOfReannouncedEb
      , testCase "drops the writes of a point older than the latest collection" $
          withFreshDb impl test_gcDropsWritesOfCollectedPoint
      ]
  , testGroup
      "promotion"
      [ testCase "keeps the closure of a promoted EB its garbage collection evicts" $
          withFreshDb impl test_gcKeepsPromotedEb
      , testCase "fails for an EB whose closure is incomplete, copying nothing" $
          withFreshDb impl test_promoteIncompleteFails
      , testCase "fails for an EB whose txs do not hash to its body, copying nothing" $
          withFreshDb impl test_promoteChecksHashes
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
      (_, insertTime) <- timed $ rwInsertEbBody con point (mkTestEb 1)
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
          snd <$> timed (rwInsertEbBody con p (mkTestEb 1))
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
        (_, insertTime) <- timed $ rwInsertEbBody con point eb
        (result, lookupTime) <- timed $ rwLookupEbBody con point
        pure $
          result == expected
            & counterexample ("Expected: " ++ show expected ++ "\nGot: " ++ show result)
            & tabulate "insertEbBody" [timeBucket insertTime]
            & tabulate "lookupEbBody" [timeBucket lookupTime]

-- | Property: looking up a non-existent EB returns 'Nothing'.
prop_ebsLookupMissing :: DbImpl -> Property
prop_ebsLookupMissing impl =
  forAll genPoint $ \missingPoint ->
    ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
      (result, lookupTime) <- timed $ rwLookupEbBody con missingPoint
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
          rwInsertEbBody con point eb
          -- Forced, so that the tabulated insert time excludes building the
          -- bytes.
          let !txsToInsert =
                force [(off, txBytesFor eb off) | off <- offsetsToInsert]
          insertTime <- snd <$> timed (rwInsertTxs con point txsToInsert)
          -- Retrieve all offsets
          let allOffsets = [0 .. numTxs - 1]
          (results, retrieveTime) <- timed $ rwBatchRetrieveTxs con point allOffsets
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
  forAll genPoint $ \missingPoint ->
    ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
      (result, retrieveTime) <- timed $ rwBatchRetrieveTxs con missingPoint [0, 1, 2]
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
    rwInsertEbBody con point eb
    rwInsertEbBody con otherPoint otherEb
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
    rwInsertEbBody con point eb
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
    rwInsertEbBody con point eb
    rwScanComplete con 0 >>= (@?= [])
    rwLookupEbClosure con point >>= (@?= Nothing)
    rwInsertTxs con point [(0, zero)]
    rwScanComplete con 0 >>= (@?= [])
    rwLookupEbClosure con point >>= (@?= Nothing)

-- | The same EB content can be announced at several slots; the DB keeps each
-- 'LeiosPoint' on its own, and the scan finds each whose closure is complete.
test_multipleSlotsSameHash :: LeiosDbHandle IO -> IO ()
test_multipleSlotsSameHash db = do
  let hashSeed = 1
      point1 = mkTestPoint (SlotNo 1) hashSeed
      point2 = mkTestPoint (SlotNo 2) hashSeed
      eb = mkTestEb 2 -- same content at both points (deterministic from numTxs)
  withRW db $ \con -> do
    rwInsertEbBody con point1 eb
    rwInsertEbBody con point2 eb
    rwInsertTxs con point1 (allTxsFor eb)
    rwScanComplete con 0 >>= (@?= [point1])

-- | An EB announced again after its closure completed at an older point: the
-- new point is complete only once its own txs are written.
test_scanFindsLaterPoint :: LeiosDbHandle IO -> IO ()
test_scanFindsLaterPoint db = do
  let older = mkTestPoint (SlotNo 10) 1
      younger = mkTestPoint (SlotNo 30) 1
      eb = mkTestEb 2
  withRW db $ \con -> do
    rwInsertEbBody con older eb
    rwInsertTxs con older (allTxsFor eb)
    rwInsertEbBody con younger eb
    rwScanComplete con 20 >>= (@?= [])
    rwInsertTxs con younger (allTxsFor eb)
    rwScanComplete con 20 >>= (@?= [younger])

-- | A body write records its point.
test_ebBodyWritesPoint :: LeiosDbHandle IO -> IO ()
test_ebBodyWritesPoint db = withRW db $ \con -> do
  let eb = mkTestEb 2
  rwInsertEbBody con (mkTestPoint 1 1) eb
  rwScanEbPoints con >>= (@?= [(1, mkTestEbHash 1)])
  rwLookupEbBody con (mkTestPoint 1 1) >>= (@?= Just (serialiseEndorserBlock eb))

-- | A body written again at its point reports only what its fills filled, none
-- here, and keeps the txs written since the first.
test_ebBodyAgainKeepsTxs :: LeiosDbHandle IO -> IO ()
test_ebBodyAgainKeepsTxs db = withRW db $ \con -> do
  let eb = mkTestEb 3
      point = mkTestPoint 1 1
      written = take 2 (allTxsFor eb)
  rwInsertEbBodyFilling con point eb [] >>= (@?= [])
  rwInsertTxs con point written
  rwInsertEbBodyFilling con point eb [] >>= (@?= [])
  rwBatchRetrieveTxs con point [0, 1, 2]
    >>= (@?= [(o, Just bytes) | (o, bytes) <- written] <> [(2, Nothing)])

-- | Each point of an EB is read on its own: a point holds only the body and
-- txs written at it, whatever another point of the EB holds.
test_readsAtPoint :: LeiosDbHandle IO -> IO ()
test_readsAtPoint db = withRW db $ \con -> do
  let eb = mkTestEb 2
  writeWholeEb con [mkTestPoint 1 1] eb
  rwLookupEbBody con (mkTestPoint 2 1) >>= (@?= Nothing)
  rwLookupEbClosure con (mkTestPoint 2 1) >>= (@?= Nothing)
  rwBatchRetrieveTxs con (mkTestPoint 2 1) [0, 1] >>= (@?= [])
  rwInsertEbBody con (mkTestPoint 2 1) eb
  rwLookupEbClosure con (mkTestPoint 2 1) >>= (@?= Nothing)
  rwInsertTxs con (mkTestPoint 2 1) (allTxsFor eb)
  rwLookupEbClosure con (mkTestPoint 2 1) >>= (@?= Just (map snd (allTxsFor eb)))

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
    rwInsertEbBody con point eb
    -- An offset the body does not declare, and wrong-sized bytes. Only the
    -- latter are reported.
    dropped <-
      rwInsertTxsDropped
        con
        point
        [ (2, txBytesFor eb 0)
        , (0, BS.snoc (txBytesFor eb 0) 0)
        , (1, BS.take 1 (txBytesFor eb 1))
        ]
    dropped @?= [0, 1]
    retrieved <- rwBatchRetrieveTxs con point offsets
    retrieved @?= [(off, Nothing) | off <- offsets]
    -- Within one write, the first delivery wins.
    rwInsertTxs con point [(0, txBytesFor eb 0), (0, BS.map (+ 1) (txBytesFor eb 0))]
    retrieved' <- rwBatchRetrieveTxs con point [0]
    retrieved' @?= [(0, Just (txBytesFor eb 0))]
    rwInsertTxs con point [(1, txBytesFor eb 1)]
    closure <- rwLookupEbClosure con point
    closure @?= Just (map snd (allTxsFor eb))

-- * Local fills

-- | Two endorser blocks that both reference the tx at offset 1 of 'mkTestEb'
-- 2: that one, and one that references it at offset 0, then a tx of its own.
fillFixtures :: (LeiosPoint, EndorserBlock, LeiosPoint, EndorserBlock)
fillFixtures =
  ( mkTestPoint (SlotNo 1) 1
  , mkTestEb 2
  , mkTestPoint (SlotNo 2) 2
  , mkEndorserBlock [(testTxHash 1 101, 101), (testTxHash 1 107, 107)]
  )

test_fillFromAnotherEb :: LeiosDbHandle IO -> IO ()
test_fillFromAnotherEb db = do
  let (pointA, ebA, pointB, ebB) = fillFixtures
  withRW db $ \con -> do
    writeWholeEb con [pointA] ebA
    -- The second fill's source lists another tx at its offset.
    result <-
      rwInsertEbBodyFilling
        con
        pointB
        ebB
        [ (0, MkTxLocation (pointEbHash pointA) 1)
        , (1, MkTxLocation (pointEbHash pointA) 0)
        ]
    result @?= [0]
    retrieved <- rwBatchRetrieveTxs con pointB [0, 1]
    retrieved @?= [(0, Just (txBytesFor ebA 1)), (1, Nothing)]
    rwInsertTxs con pointB [(1, txBytesFor ebB 1)]
    rwScanComplete con 0 >>= (@?= [pointA, pointB])

test_fillWithoutSourceBytes :: LeiosDbHandle IO -> IO ()
test_fillWithoutSourceBytes db = do
  let (pointA, ebA, pointB, ebB) = fillFixtures
  withRW db $ \con -> do
    rwInsertEbBody con pointA ebA
    -- The first source holds the body but not its txs; the second is unknown.
    -- So the first source's tx is not written.
    result <-
      rwInsertEbBodyFilling
        con
        pointB
        ebB
        [ (0, MkTxLocation (pointEbHash pointA) 1)
        , (0, MkTxLocation (mkTestEbHash 9) 0)
        ]
    result @?= []

test_fillCompletesClosure :: LeiosDbHandle IO -> IO ()
test_fillCompletesClosure db = do
  let (pointA, ebA, _, _) = fillFixtures
      pointB = mkTestPoint (SlotNo 2) 2
      ebB = mkEndorserBlock [(testTxHash 1 101, 101)]
  withRW db $ \con -> do
    writeWholeEb con [pointA] ebA
    result <- rwInsertEbBodyFilling con pointB ebB [(0, MkTxLocation (pointEbHash pointA) 1)]
    result @?= [0]
    rwScanComplete con 0 >>= (@?= [pointA, pointB])

-- | An EB announced again: the new point fills its txs from the older one's,
-- which a point does not share otherwise.
test_fillFromAnotherPoint :: LeiosDbHandle IO -> IO ()
test_fillFromAnotherPoint db = withRW db $ \con -> do
  let eb = mkTestEb 2
      older = mkTestPoint 1 1
      younger = mkTestPoint 2 1
  writeWholeEb con [older] eb
  result <-
    rwInsertEbBodyFilling con younger eb [(off, MkTxLocation (mkTestEbHash 1) off) | off <- [0, 1]]
  result @?= [0, 1]
  rwLookupEbClosure con younger >>= (@?= Just (map snd (allTxsFor eb)))

-- * Garbage collection

-- | Write the EB at these points, with its body and all its transactions.
writeWholeEb :: RW -> [LeiosPoint] -> EndorserBlock -> IO ()
writeWholeEb con points eb = do
  forM_ points $ \point -> rwInsertEbBody con point eb
  forM_ points $ \point -> rwInsertTxs con point (allTxsFor eb)

-- | An EB promoted at slot 5, collected at slot 6, and fetched again at slot
-- 10, so far one of its txs. The reads of slot 5 go to the immutable
-- partition, those of slot 10 to the volatile one, and neither falls back to
-- the other.
test_readsDispatchBySlot :: DbImpl -> IO ()
test_readsDispatchBySlot impl = withFreshDb impl $ \db -> withRW db $ \con -> do
  let eb = mkTestEb 2
      promoted = mkTestPoint 5 1
      again = mkTestPoint 10 1
  writeWholeEb con [promoted] eb
  leiosDbPromoteToImmutable db [promoted]
  garbageCollect db 6
  rwInsertEbBody con again eb
  rwInsertTxs con again (take 1 (allTxsFor eb))
  rwBatchRetrieveTxs con again [0, 1] >>= (@?= [(0, Just (txBytesFor eb 0)), (1, Nothing)])
  rwLookupEbClosure con again >>= (@?= Nothing)
  rwLookupEbClosure con promoted >>= (@?= Just (map snd (allTxsFor eb)))
  rwLookupEbBody con promoted >>= (@?= Just (bodyBytes eb))
  rwScanComplete con 0 >>= (@?= [promoted])

-- | A crash can lose the volatile writes that completed a promoted EB, but not
-- its immutable copy. Promoting it again, as when the crash lost the block
-- certifying it too, succeeds, and the closure is read from the immutable copy.
test_promoteAgainAfterLosingVolatileTxs :: FilePath -> FilePath -> IO ()
test_promoteAgainAfterLosingVolatileTxs volDbPath immDbPath = do
  let eb = mkTestEb 2
      point = mkTestPoint 5 1
  withLeiosDBSQLite nullTracer volDbPath immDbPath $ \db -> withRW db $ \con -> do
    writeWholeEb con [point] eb
    leiosDbPromoteToImmutable db [point]
  -- What the crash leaves of the volatile partition: the body, but no txs.
  withSqlFile volDbPath $ \sql ->
    SQL.exec sql (T.pack "UPDATE ebs SET txs = zeroblob(length(txs))")
  withLeiosDBSQLite nullTracer volDbPath immDbPath $ \db -> withRW db $ \con -> do
    leiosDbPromoteToImmutable db [point]
    rwLookupEbClosure con point >>= (@?= Just (map snd (allTxsFor eb)))

-- | An EB announced again after the announcement that gets certified: the
-- immutable copy is at the certified slot.
test_promoteAtCertifiedSlot :: FilePath -> FilePath -> LeiosDbHandle IO -> IO ()
test_promoteAtCertifiedSlot _volDbPath immDbPath db = do
  let certified = mkTestPoint 10 1
  withRW db $ \con -> writeWholeEb con [certified, mkTestPoint 20 1] (mkTestEb 2)
  leiosDbPromoteToImmutable db [certified]
  ebSlotIn immDbPath (mkTestEbHash 1) >>= (@?= 10)

-- | An EB promoted at slot 5 and collected at slot 6, read by a reader that
-- chose the volatile partition before that collection: as of its choice, the
-- latest collection was at slot 0. It finds no row there, and reads the
-- immutable partition only if the collection has overtaken it.
test_readOvertakenByCollection :: FilePath -> FilePath -> LeiosDbHandle IO -> IO ()
test_readOvertakenByCollection volDbPath immDbPath db = do
  let eb = mkTestEb 2
      point = mkTestPoint 5 1
  withRW db $ \con -> writeWholeEb con [point] eb
  leiosDbPromoteToImmutable db [point]
  garbageCollect db 6
  statsVar <- newTVarIO =<< initialStats volDbPath immDbPath
  gcSlotVar <- newTVarIO 0
  bracket (openRawConnection volDbPath) closeChecked $ \volDb ->
    bracket (openRawConnection immDbPath) closeChecked $ \immDb -> do
      let conn = mkConn nullTracer statsVar gcSlotVar volDb immDb
      readPartition conn point (`lookupBlob` point) >>= (@?= Nothing)
      -- The collection completes while the read runs.
      let overtaken readDb = do
            atomically $ writeTVar gcSlotVar 6
            lookupBlob readDb point
      readPartition conn point overtaken >>= (@?= Just (bodyBytes eb))

-- | Whether the reader sees the EB's whole closure.
holdsClosure :: RW -> LeiosPoint -> IO Bool
holdsClosure con point = isJust <$> rwLookupEbClosure con point

test_gcEvictsOldEb :: LeiosDbHandle IO -> IO ()
test_gcEvictsOldEb db = withRW db $ \con -> do
  -- The old EB references transactions 0, 1 and 2, and the other only 0 and 1.
  writeWholeEb con [mkTestPoint 5 1] (mkTestEb 3)
  writeWholeEb con [mkTestPoint 10 2] (mkTestEb 2)
  garbageCollect db 6
  rwLookupEbBody con (mkTestPoint 5 1) >>= (@?= Nothing)
  holdsClosure con (mkTestPoint 5 1) >>= (@?= False)
  rwScanEbPoints con >>= (@?= [(10, mkTestEbHash 2)])
  holdsClosure con (mkTestPoint 10 2) >>= (@?= True)
  -- The old EB's transactions went with it, so a new EB referencing them
  -- lacks them.
  rwInsertEbBody con (mkTestPoint 20 3) (mkTestEb 3)
  holdsClosure con (mkTestPoint 20 3) >>= (@?= False)

-- | An EB written whole at slot 5 and again at slot 10, collected at slot 6.
test_gcDropsOlderPoints :: LeiosDbHandle IO -> IO ()
test_gcDropsOlderPoints db = withRW db $ \con -> do
  let eb = mkTestEb 2
      again = mkTestPoint 10 1
  writeWholeEb con [mkTestPoint 5 1, again] eb
  garbageCollect db 6
  rwScanEbPoints con >>= (@?= [(10, mkTestEbHash 1)])
  holdsClosure con again >>= (@?= True)
  rwScanComplete con 6 >>= (@?= [again])

-- | After a collection at slot 6, a point at slot 5 and a body for another
-- point at slot 5 are written, and one at slot 4 that an earlier collection at
-- slot 3 would not collect.
test_gcDropsWritesOfCollectedPoint :: LeiosDbHandle IO -> IO ()
test_gcDropsWritesOfCollectedPoint db = withRW db $ \con -> do
  writeWholeEb con [mkTestPoint 10 1] (mkTestEb 2)
  garbageCollect db 6
  garbageCollect db 3
  rwInsertEbBody con (mkTestPoint 5 2) (mkTestEb 2)
  rwInsertEbBody con (mkTestPoint 5 3) (mkTestEb 3)
  rwInsertEbBody con (mkTestPoint 4 1) (mkTestEb 2)
  rwScanEbPoints con >>= (@?= [(10, mkTestEbHash 1)])
  rwLookupEbBody con (mkTestPoint 5 3) >>= (@?= Nothing)

-- | An EB written whole at slot 10 is announced again in each of nine more
-- rounds, each followed by a collection at its slot.
test_gcBoundsPointsOfReannouncedEb :: LeiosDbHandle IO -> IO ()
test_gcBoundsPointsOfReannouncedEb db = withRW db $ \con -> do
  let eb = mkTestEb 2
  writeWholeEb con [mkTestPoint 10 1] eb
  forM_ [2 .. 10] $ \n -> do
    rwInsertEbBody con (mkTestPoint (10 * n) 1) eb
    garbageCollect db (10 * n)
  rwScanEbPoints con >>= (@?= [(100, mkTestEbHash 1)])

test_gcKeepsEbWithPointAtSlot :: LeiosDbHandle IO -> IO ()
test_gcKeepsEbWithPointAtSlot db = withRW db $ \con -> do
  writeWholeEb con [mkTestPoint 5 1, mkTestPoint 6 1] (mkTestEb 2)
  garbageCollect db 6
  holdsClosure con (mkTestPoint 6 1) >>= (@?= True)

-- | More points before the slot than the SQLite implementation deletes in one
-- job, and one at the slot.
test_gcCollectsManyPoints :: LeiosDbHandle IO -> IO ()
test_gcCollectsManyPoints db = withRW db $ \con -> do
  let eb = mkTestEb 2
      n = 2 * gcBatchSize + 1
  forM_ [1 .. n] $ \slot ->
    rwInsertEbBody con (mkTestPoint (fromIntegral slot) 1) eb
  garbageCollect db (fromIntegral n)
  rwScanEbPoints con >>= (@?= [(fromIntegral n, mkTestEbHash 1)])

test_gcKeepsPromotedEb :: LeiosDbHandle IO -> IO ()
test_gcKeepsPromotedEb db = withRW db $ \con -> do
  writeWholeEb con [mkTestPoint 5 1] (mkTestEb 2)
  leiosDbPromoteToImmutable db [mkTestPoint 5 1]
  garbageCollect db 6
  holdsClosure con (mkTestPoint 5 1) >>= (@?= True)

-- | A complete EB promoted along with one whose closure lacks a tx.
test_promoteIncompleteFails :: LeiosDbHandle IO -> IO ()
test_promoteIncompleteFails db = withRW db $ \con -> do
  writeWholeEb con [mkTestPoint 4 2] (mkTestEb 3)
  rwInsertEbBody con (mkTestPoint 5 1) (mkTestEb 2)
  rwInsertTxs con (mkTestPoint 5 1) (take 1 (allTxsFor (mkTestEb 2)))
  assertPromoteFails db [mkTestPoint 4 2, mkTestPoint 5 1]
  garbageCollect db 6
  holdsClosure con (mkTestPoint 4 2) >>= (@?= False)

-- | An EB whose txs are written but do not hash to its body, which the scan
-- would take as complete in the immutable partition.
test_promoteChecksHashes :: LeiosDbHandle IO -> IO ()
test_promoteChecksHashes db = withRW db $ \con -> do
  let point = mkTestPoint 5 1
      eb = mkTestEb 2
  rwInsertEbBody con point eb
  rwInsertTxs con point [(off, BS.map (+ 1) bytes) | (off, bytes) <- allTxsFor eb]
  assertPromoteFails db [point]
  rwScanComplete con 0 >>= (@?= [])

assertPromoteFails :: LeiosDbHandle IO -> [LeiosPoint] -> Assertion
assertPromoteFails db points =
  tryDb (leiosDbPromoteToImmutable db points) >>= \case
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
        rwInsertEbBody con point eb
        rwInsertTxs con point (allTxsFor eb)
        (result, queryTime) <- timed $ rwLookupEbClosure con point
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
          rwInsertEbBody con point eb
          rwInsertTxs con point [(off, txBytesFor eb off) | off <- offsetsToInsert]
          (result, queryTime) <- timed $ rwLookupEbClosure con point
          let expected
                | length offsetsToInsert == numTxs = Just (map snd (allTxsFor eb))
                | otherwise = Nothing
          pure $
            result === expected
              & counterexample ("Inserted offsets: " ++ show offsetsToInsert)
              & tabulate "lookupEbClosure (partial txs)" [timeBucket queryTime]
              & tabulate "numTxs" [magnitudeBucket numTxs]

-- | Property: a point with no body written returns Nothing.
prop_completedEbNoBody :: DbImpl -> Property
prop_completedEbNoBody impl =
  forAll genPoint $ \point ->
    ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
      (result, queryTime) <- timed $ rwLookupEbClosure con point
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
    rwInsertEbBody con (MkLeiosPoint 5 keptHash) eb
    -- The kept EB is announced again at a slot the truncation drops.
    rwInsertEbBody con (MkLeiosPoint 15 keptHash) eb
    rwInsertEbBody con (MkLeiosPoint 15 droppedHash) eb
    rwInsertTxs con (MkLeiosPoint 5 keptHash) (allTxsFor eb)
    rwInsertTxs con (MkLeiosPoint 15 droppedHash) (allTxsFor eb)

  truncateLeiosDbAfterSlot volDbPath 10
  ebBodyRows volDbPath droppedHash >>= (@?= 0)

  withRW db $ \con -> do
    points <- rwScanEbPoints con
    points @?= [(5, keptHash)]
    keptBody <- rwLookupEbBody con (MkLeiosPoint 5 keptHash)
    keptBody @?= Just (bodyBytes eb)
    droppedBody <- rwLookupEbBody con (MkLeiosPoint 15 droppedHash)
    droppedBody @?= Nothing
    keptClosure <- rwLookupEbClosure con (MkLeiosPoint 5 keptHash)
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
    withRW db $ \con -> rwInsertEbBody con (MkLeiosPoint 5 ebHash) eb
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

-- | How many rows of the file hold the EB.
ebBodyRows :: FilePath -> EndorserBlockHash -> IO Int
ebBodyRows path ebHash =
  queryCount
    path
    ("SELECT COUNT(*) FROM ebs WHERE ebHashBytes = " <> sqlBlob ebHash)

-- | The slot of the file's row of the EB.
ebSlotIn :: FilePath -> EndorserBlockHash -> IO Int
ebSlotIn path ebHash =
  queryCount path ("SELECT ebSlot FROM ebs WHERE ebHashBytes = " <> sqlBlob ebHash)

queryCount :: FilePath -> String -> IO Int
queryCount path query =
  withSqlFile path $ \sql ->
    SQL.withStatement sql (T.pack query) $ \stmt ->
      SQL.step stmt >> fromIntegral <$> SQL.columnInt64 stmt 0

-- * The two implementations agree

-- | An operation on the database: a write, or what the ChainDB has it do as
-- blocks become immutable.
data Op
  = OpBody LeiosPoint [LocalFill]
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
            (,) <$> testScanEbPoints reader <*> lookupEbClosure reader (MkLeiosPoint slot ebHash)
        pure $
          conjoin
            [ counterexample "the point is not the slot and the hash of the body" $
                points === [(slot, ebHash)]
            , counterexample "the stored txs are not the forged ones" $
                closure === Just (toList txs)
            ]

-- | Property: running the same operations against both implementations, a
-- reader sees the same in each, each body write holds the same offsets, and
-- the scan finds the same endorser blocks complete.
--
-- The node runs on SQLite and its tests run on the in-memory implementation, so
-- those tests say something about the node only as far as the two agree.
prop_implementationsAgree :: Property
prop_implementationsAgree =
  forAll genBodies $ \bodies ->
    forAllShrinkShow (genOps bodies) (shrinkList (const [])) show $ \ops ->
      ioProperty $ do
        ((nPromoted, nEvicted), inMemory) <-
          withFreshDb InMemory $ \db -> observe db bodies ops
        (_, sqlite) <- withFreshDb SQLite $ \db -> observe db bodies ops
        pure $
          tabulate "endorser blocks promoted" [bucket nPromoted] $
            tabulate "endorser blocks evicted" [bucket nEvicted] $
              tabulate "local fills landed" [bucket (sum (map length (fst inMemory)))] $
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
  -- Mostly from a source that lists the same tx, which is the only kind that
  -- can land, another point of the same endorser block included; otherwise
  -- from anywhere.
  genFill point =
    frequency $
      (1, (,) <$> chooseInt (0, 4) <*> (MkTxLocation <$> elements (Map.keys bodies) <*> chooseInt (0, 4)))
        : [ (3, elements matching)
          | let indices = snd (bodies Map.! pointEbHash point)
                matching =
                  [ (dstOff, MkTxLocation src srcOff)
                  | (dstOff, i) <- zip [0 ..] indices
                  , (src, (_, srcIndices)) <- Map.toList bodies
                  , (srcOff, j) <- zip [0 ..] srcIndices
                  , i == j
                  ]
          , not (null matching)
          ]
  genOp =
    frequency
      [ (3, genPointOf >>= \point -> OpBody point <$> listOf (genFill point))
      , (3, OpTxs <$> genPointOf <*> sublistOf [0 .. 4])
      , (3, OpPromote <$> elements (Map.keys bodies))
      , (1, OpGc . (+ 1) <$> genSlot)
      ]

-- | A transaction's bytes, by its index in the pool.
poolTxBytes :: Int -> BS.ByteString
poolTxBytes i = BS.replicate (10 + i) (fromIntegral i)

-- | Run the operations, then read back everything a reader can see at each
-- point they name. Also how many endorser blocks the operations promoted and
-- evicted.
--
-- The operations run only as far as they meet the node's own preconditions: a
-- body is written once per point; and an
-- endorser block is promoted only with a complete closure, as the node promotes
-- only what an immutable block certifies, and its ChainSel adopts a block
-- certifying an endorser block only once it holds that endorser block's
-- closure. So this tracks which points and
-- bodies the garbage collection evicts: those of every endorser block all of
-- whose points are older than its slot, promoted or not, since a promoted one
-- has been copied; and every other point older than its slot. A point older
-- than the latest garbage collection is not written at all.
--
-- An endorser block is also promoted at most once: the node promotes only what
-- the immutable chain certifies, and a chain cannot certify the same endorser
-- block twice, since it cannot be valid twice.
observe ::
  LeiosDbHandle IO ->
  Bodies ->
  [Op] ->
  IO
    ( (Int, Int)
    , ( [[TxOffset]]
      , ( [ ( Maybe BS.ByteString
            , Maybe [BS.ByteString]
            , [(Int, Maybe BS.ByteString)]
            )
          ]
        , [LeiosPoint]
        )
      )
    )
observe db bodies ops = withRW db $ \rw -> do
  (counts, held) <- go rw 0 [] [] [] 0 ops
  seen <- forM (nub [p | op <- ops, Just p <- [opPoint op]]) $ \point ->
    (,,)
      <$> rwLookupEbBody rw point
      <*> rwLookupEbClosure rw point
      <*> rwBatchRetrieveTxs
        rw
        point
        [0 .. length (endorserBlockReferences (fst (bodies Map.! pointEbHash point)))]
  complete <- rwScanComplete rw 0
  pure (counts, (held, (seen, complete)))
 where
  go _rw _gcSlot _points _bodied promoted nEvicted [] = pure ((length promoted, nEvicted), [])
  go rw gcSlot points bodied promoted nEvicted (op : rest) = case op of
    OpBody point fills
      | point `notElem` bodied -> do
          held <- rwInsertEbBodyFilling rw point (fst (bodies Map.! pointEbHash point)) fills
          let bodied' = if collected point then bodied else point : bodied
          fmap (held :) <$> go rw gcSlot (written point points) bodied' promoted nEvicted rest
      | otherwise -> go rw gcSlot points bodied promoted nEvicted rest
    OpTxs point offsets -> do
      let indices = snd (bodies Map.! pointEbHash point)
      rwInsertTxs
        rw
        point
        [ (offset, poolTxBytes (if offset < length indices then indices !! offset else 1))
        | offset <- offsets
        ]
      go rw gcSlot points bodied promoted nEvicted rest
    OpPromote ebHash -> case [p | p <- points, pointEbHash p == ebHash, ebHash `notElem` promoted] of
      point : _ -> do
        complete <- holdsClosure rw point
        if complete
          then do
            leiosDbPromoteToImmutable db [point]
            go rw gcSlot points bodied (nub (ebHash : promoted)) nEvicted rest
          else go rw gcSlot points bodied promoted nEvicted rest
      [] -> go rw gcSlot points bodied promoted nEvicted rest
    OpGc slot -> do
      garbageCollect db slot
      let newest ebHash = maximum [pointSlotNo p | p <- points, pointEbHash p == ebHash]
          evicted =
            nub [ebHash | p <- points, let ebHash = pointEbHash p, newest ebHash < slot]
          kept p = pointSlotNo p >= slot
      go
        rw
        (max slot gcSlot)
        (filter kept points)
        (filter kept bodied)
        promoted
        (nEvicted + length evicted)
        rest
   where
    collected point = pointSlotNo point < gcSlot
    written point ps
      | collected point || point `elem` ps = ps
      | otherwise = point : ps
  opPoint = \case
    OpBody point _ -> Just point
    OpTxs point _ -> Just point
    _ -> Nothing
