{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Tests for the "Ouroboros.Consensus.Storage.LeiosDB" interface.
--
-- These tests verify the semantics of the LeiosDbHandle operations for both
-- InMemory and SQLite implementations. Each test case uses a fresh database
-- to ensure isolation and serve as an (too optimistic) performance baseline.
module Test.Ouroboros.Storage.LeiosDB (tests) where

import Cardano.Slotting.Slot (SlotNo (..))
import Codec.CBOR.Write (toStrictByteString)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Control.DeepSeq (force)
import Control.Exception (bracket, try)
import Control.Monad (forM, forM_, replicateM, void)
import Control.Monad.Class.MonadTime.SI (diffTime, getMonotonicTime)
import Control.Tracer (Tracer (..), emit, nullTracer)
import qualified Data.ByteString as BS
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Clock (DiffTime)
import qualified Data.Vector.Strict as V
import qualified Database.SQLite3 as SQL
import Numeric (showHex)
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EbHash (..)
  , LeiosEb (..)
  , LeiosPoint (..)
  , TxHash (..)
  , TxOffset
  , encodeLeiosEb
  , encodeLeiosEbSize
  , leiosEbTxs
  )
import Ouroboros.Consensus.Storage.LeiosDB
  ( LeiosDbHandle (..)
  , LeiosDbReader (..)
  , LeiosDbWriter (..)
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

rwInsertEbBody :: RW -> LeiosPoint -> LeiosEb -> IO ()
rwInsertEbBody rw point eb = await =<< writeEbBody (rwWriter rw) point eb

rwInsertTxs :: RW -> LeiosPoint -> [(TxOffset, BS.ByteString)] -> IO ()
rwInsertTxs rw point txs = await =<< writeTxs (rwWriter rw) point txs

rwScanEbPoints :: RW -> IO [(SlotNo, EbHash)]
rwScanEbPoints = testScanEbPoints . rwReader

rwLookupEbBody :: RW -> EbHash -> IO (Maybe BS.ByteString)
rwLookupEbBody = lookupEbBody . rwReader

rwLookupEbClosure :: RW -> EbHash -> IO (Maybe [BS.ByteString])
rwLookupEbClosure = lookupEbClosure . rwReader

rwBatchRetrieveTxs :: RW -> EbHash -> [Int] -> IO [(Int, BS.ByteString)]
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
      ]
  , testGroup
      "lookupEbClosure"
      [ testProperty "complete EB returns its txs" $ prop_completedEbComplete impl
      , testProperty "partial txs are zeroes where missing" $ prop_completedEbPartialTxs impl
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

-- | Bytes for the tx at the given offset of the EB: of its declared size, as
-- a write must be to be accepted, and distinct per offset.
txBytesFor :: LeiosEb -> TxOffset -> BS.ByteString
txBytesFor eb off = BS.replicate (fromIntegral size) (fromIntegral off)
 where
  (_, size) = leiosEbTxs eb V.! off

-- | What a tx not written yet reads as.
zeroesFor :: LeiosEb -> TxOffset -> BS.ByteString
zeroesFor eb off = BS.replicate (BS.length (txBytesFor eb off)) 0

-- | Every tx of the EB, with 'txBytesFor'.
allTxsFor :: LeiosEb -> [(TxOffset, BS.ByteString)]
allTxsFor eb = [(off, txBytesFor eb off) | off <- [0 .. V.length (leiosEbTxs eb) - 1]]

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

-- | Property: inserting an EB body and looking it up returns it in its wire
-- encoding.
prop_ebsInsertThenLookup :: DbImpl -> Property
prop_ebsInsertThenLookup impl =
  forAllShrinkShow (chooseInt (1, 50)) (filter (>= 1) . shrink) show $ \numTxs ->
    forAll (genPointAndEb numTxs) $ \(point, eb) ->
      ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
        let expected = Just (toStrictByteString (encodeLeiosEb eb))
        rwInsertEbPoint con point (encodeLeiosEbSize eb)
        (_, insertTime) <- timed $ rwInsertEbBody con point eb
        (result, lookupTime) <- timed $ rwLookupEbBody con (pointEbHash point)
        pure $
          result === expected
            & tabulate "insertEbBody" [timeBucket insertTime]
            & tabulate "lookupEbBody" [timeBucket lookupTime]

-- | Property: looking up a non-existent EB returns nothing.
prop_ebsLookupMissing :: DbImpl -> Property
prop_ebsLookupMissing impl =
  forAll genEbHash $ \missingHash ->
    ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
      (result, lookupTime) <- timed $ rwLookupEbBody con missingHash
      pure $
        result === Nothing
          & tabulate "lookupEbBody (missing)" [timeBucket lookupTime]

-- * Property tests for transactions

-- | Property: inserting some of an EB's txs and retrieving some returns the
-- bytes of those inserted, and zeroes for the others.
prop_txsInsertThenRetrieve :: DbImpl -> Property
prop_txsInsertThenRetrieve impl =
  forAllShrinkShow (chooseInt (1, 50)) (filter (>= 1) . shrink) show $ \numTxs ->
    forAllBlind (genPointAndEb numTxs) $ \(point, eb) ->
      forAllBlind (sublistOf [0 .. numTxs - 1]) $ \offsetsToInsert ->
        forAllBlind (sublistOf [0 .. numTxs - 1]) $ \offsetsToRetrieve ->
          ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
            -- Insert the EB first (point then body)
            rwInsertEbPoint con point (encodeLeiosEbSize eb)
            void $ rwInsertEbBody con point eb
            -- Forced, so that the tabulated insert time excludes building the
            -- bytes.
            let !txsToInsert = force [(off, txBytesFor eb off) | off <- offsetsToInsert]
            insertTime <- snd <$> timed (rwInsertTxs con point txsToInsert)
            (results, retrieveTime) <-
              timed $ rwBatchRetrieveTxs con (pointEbHash point) offsetsToRetrieve
            pure $
              results
                === [ (off, if off `elem` offsetsToInsert then txBytesFor eb off else zeroesFor eb off)
                    | off <- offsetsToRetrieve
                    ]
                & counterexample ("Inserted offsets: " ++ show offsetsToInsert)
                & counterexample ("Retrieved offsets: " ++ show offsetsToRetrieve)
                & counterexample ("Total txs: " <> show numTxs)
                & tabulate "insertTxs" [timeBucket insertTime]
                & tabulate "batchRetrieveTxs" [timeBucket retrieveTime]
                & tabulate "txs retrieved" [magnitudeBucket $ length offsetsToRetrieve]

-- | Property: retrieving from non-existent EB returns empty list.
prop_txsRetrieveMissing :: DbImpl -> Property
prop_txsRetrieveMissing impl =
  forAll genEbHash $ \missingHash ->
    ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
      (result, retrieveTime) <- timed $ rwBatchRetrieveTxs con missingHash [0, 1, 2]
      pure $
        result === []
          & tabulate "batchRetrieveTxs (missing)" [timeBucket retrieveTime]

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
    rwInsertEbPoint con point (encodeLeiosEbSize eb)
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
    retrieved @?= [(off, zeroesFor eb off) | off <- offsets]
    -- Within one write, the first delivery wins.
    rwInsertTxs con point [(0, txBytesFor eb 0), (0, BS.map (+ 1) (txBytesFor eb 0))]
    retrieved' <- rwBatchRetrieveTxs con (pointEbHash point) [0]
    retrieved' @?= [(0, txBytesFor eb 0)]
    rwInsertTxs con point [(1, txBytesFor eb 1)]
    closure <- rwLookupEbClosure con (pointEbHash point)
    closure @?= Just (map snd (allTxsFor eb))

-- * Property tests for lookupEbClosure

-- | Property: a complete EB returns its txs' bytes in body order.
prop_completedEbComplete :: DbImpl -> Property
prop_completedEbComplete impl =
  forAllShrinkShow (chooseInt (1, 20)) shrink show $ \numTxs ->
    forAllBlind (genPointAndEb numTxs) $ \(point, eb) ->
      ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
        rwInsertEbPoint con point (encodeLeiosEbSize eb)
        void $ rwInsertEbBody con point eb
        _ <- rwInsertTxs con point (allTxsFor eb)
        (result, queryTime) <- timed $ rwLookupEbClosure con (pointEbHash point)
        pure $
          result === Just (map snd (allTxsFor eb))
            & tabulate "lookupEbClosure (complete)" [timeBucket queryTime]
            & tabulate "numTxs" [magnitudeBucket numTxs]

-- | Property: an EB with some txs missing returns zeroes for those.
prop_completedEbPartialTxs :: DbImpl -> Property
prop_completedEbPartialTxs impl =
  forAllShrinkShow (chooseInt (1, 20)) shrink show $ \numTxs ->
    forAllBlind (genPointAndEb numTxs) $ \(point, eb) ->
      forAllBlind (sublistOf [0 .. numTxs - 1]) $ \offsetsToInsert ->
        ioProperty $ withFreshDb impl $ \db -> withRW db $ \con -> do
          rwInsertEbPoint con point (encodeLeiosEbSize eb)
          void $ rwInsertEbBody con point eb
          rwInsertTxs con point [(off, txBytesFor eb off) | off <- offsetsToInsert]
          (result, queryTime) <- timed $ rwLookupEbClosure con (pointEbHash point)
          let expected =
                [ if off `elem` offsetsToInsert then txBytesFor eb off else zeroesFor eb off
                | off <- [0 .. numTxs - 1]
                ]
          pure $
            result === Just expected
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
    rwInsertEbPoint con (MkLeiosPoint 5 keptHash) (encodeLeiosEbSize eb)
    void $ rwInsertEbBody con (MkLeiosPoint 5 keptHash) eb
    -- The kept EB is announced again at a slot the truncation drops.
    rwInsertEbPoint con (MkLeiosPoint 15 keptHash) (encodeLeiosEbSize eb)
    rwInsertEbPoint con (MkLeiosPoint 15 droppedHash) (encodeLeiosEbSize eb)
    void $ rwInsertEbBody con (MkLeiosPoint 15 droppedHash) eb
    void $ rwInsertTxs con (MkLeiosPoint 5 keptHash) (allTxsFor eb)
    void $ rwInsertTxs con (MkLeiosPoint 15 droppedHash) (allTxsFor eb)

  truncateLeiosDbAfterSlot volDbPath 10
  ebBodyRows volDbPath droppedHash >>= (@?= 0)
  orphanedClosures volDbPath >>= (@?= 0)

  withRW db $ \con -> do
    points <- rwScanEbPoints con
    points @?= [(5, keptHash)]
    keptBody <- rwLookupEbBody con keptHash
    keptBody @?= Just (toStrictByteString (encodeLeiosEb eb))
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
      forM_ ["ebs", "idx_ebs_ebHashBytes", "ebBodies", "ebClosures"] $ \name ->
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
      points <- testScanEbPoints reader
      points @?= []

test_reopenKeepsData :: FilePath -> FilePath -> IO ()
test_reopenKeepsData volDbPath immDbPath = do
  let eb = mkTestEb 2
      ebHash = mkTestEbHash 1
  withLeiosDBSQLite nullTracer volDbPath immDbPath $ \db ->
    withRW db $ \con -> rwInsertEbPoint con (MkLeiosPoint 5 ebHash) (encodeLeiosEbSize eb)
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
sqlBlob :: EbHash -> String
sqlBlob (MkEbHash bytes) = "x'" <> concatMap hex (BS.unpack bytes) <> "'"
 where
  hex w = (if w < 16 then ('0' :) else id) (showHex w "")

-- | How many 'ebBodies' rows the file holds for the EB.
ebBodyRows :: FilePath -> EbHash -> IO Int
ebBodyRows path ebHash =
  queryCount path ("SELECT COUNT(*) FROM ebBodies WHERE ebHashBytes = " <> sqlBlob ebHash)

-- | How many tx chunks the file holds without their body.
orphanedClosures :: FilePath -> IO Int
orphanedClosures path =
  queryCount
    path
    "SELECT COUNT(*) FROM ebClosures WHERE ebHashBytes NOT IN (SELECT ebHashBytes FROM ebBodies)"

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
          withRW db $ \con -> rwInsertEbPoint con point (encodeLeiosEbSize (mkTestEb 2))
          leiosDbPromoteToImmutable db [point]
          takeMVar copyFailed
    case closed of
      Nothing -> assertFailure "close did not return while an EB stayed pinned"
      Just () -> pure ()
 where
  closeTimeoutMicros = 30_000_000

-- * The two implementations agree

-- | A write to the database.
data Op
  = OpPoint LeiosPoint
  | OpBody LeiosPoint
  | OpTxs LeiosPoint [TxOffset]
  deriving Show

-- | What each endorser block's body is, by its hash, with the index in the
-- pool of each transaction it references: the same hash always names the same
-- body, as it would on the wire.
type Bodies = Map.Map EbHash (LeiosEb, [Int])

-- | Property: running the same writes against both implementations, a reader
-- sees the same in each.
--
-- The node runs on SQLite and its tests run on the in-memory implementation, so
-- those tests say something about the node only as far as the two agree.
prop_implementationsAgree :: Property
prop_implementationsAgree =
  forAll genBodies $ \bodies ->
    forAllShrinkShow (genOps bodies) (shrinkList (const [])) show $ \ops ->
      ioProperty $ do
        inMemory <- withFreshDb InMemory $ \db -> observe db bodies ops
        sqlite <- withFreshDb SQLite $ \db -> observe db bodies ops
        pure $ inMemory === sqlite

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
      pure (mkTestEbHash seed, (MkLeiosEb (V.fromList refs), indices))

-- | Writes, with tx offsets up to one past the largest body, so that some are
-- not in the body they are written for.
genOps :: Bodies -> Gen [Op]
genOps bodies = listOf genOp
 where
  genPointOf = MkLeiosPoint <$> (SlotNo . fromIntegral <$> chooseInt (1, 4)) <*> elements (Map.keys bodies)
  genOp =
    frequency
      [ (2, OpPoint <$> genPointOf)
      , (2, OpBody <$> genPointOf)
      , (3, OpTxs <$> genPointOf <*> sublistOf [0 .. 4])
      ]

-- | A transaction's bytes, by its index in the pool.
poolTxBytes :: Int -> BS.ByteString
poolTxBytes i = BS.replicate (10 + i) (fromIntegral i)

-- | Run the writes, then read back everything a reader can see of each
-- endorser block. The writes meet the precondition of 'writeEbBody', and a
-- body is written only once per point, since the node never does otherwise.
observe ::
  LeiosDbHandle IO ->
  Bodies ->
  [Op] ->
  IO [(Maybe BS.ByteString, Maybe [BS.ByteString], [(Int, BS.ByteString)])]
observe db bodies ops = withRW db $ \rw -> do
  go rw [] [] ops
  forM (Map.toList bodies) $ \(ebHash, (eb, _)) ->
    (,,)
      <$> rwLookupEbBody rw ebHash
      <*> rwLookupEbClosure rw ebHash
      <*> rwBatchRetrieveTxs rw ebHash [0 .. V.length (leiosEbTxs eb)]
 where
  go _rw _points _bodied [] = pure ()
  go rw points bodied (op : rest) = case op of
    OpPoint point -> do
      rwInsertEbPoint rw point (encodeLeiosEbSize (fst (bodies Map.! pointEbHash point)))
      go rw (point : points) bodied rest
    OpBody point
      | point `elem` points && point `notElem` bodied -> do
          rwInsertEbBody rw point (fst (bodies Map.! pointEbHash point))
          go rw points (point : bodied) rest
      | otherwise -> go rw points bodied rest
    OpTxs point offsets -> do
      let indices = snd (bodies Map.! pointEbHash point)
      rwInsertTxs
        rw
        point
        [ (offset, poolTxBytes (if offset < length indices then indices !! offset else 1))
        | offset <- offsets
        ]
      go rw points bodied rest
