{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Concurrent benchmark for 'LeiosDemoDb' mirroring production access patterns.
--
-- The following roles run concurrently against the same SQLite handle:
--
-- * __Fetch clients__ (configurable, default 3 threads): each inserts 20 fresh
--   EBs via 'leiosDbInsertEbPoint' → 'leiosDbInsertEbBody' → 'leiosDbInsertTxs'.
--
-- * __Fetch servers__ (configurable, default 3 threads): each does 30
--   'leiosDbLookupEbBody' + 10 'leiosDbBatchRetrieveTxs' calls cycling through
--   the pre-populated EBs.
--
-- * __Chain-sel reader__ (1 thread): mimics the block-apply path via
--   'leiosDbLookupEbClosure' — the same read that 'resolveLeiosClosure'
--   issues per Dijkstra-era CertRB.
--
-- * __GC ticker__ (1 thread): periodic 'leiosDbGarbageCollect' calls (a
--   handle-level operation that touches every table). Exercises
--   contention with the concurrent readers/writers.
--
-- All data is deterministic (no QuickCheck generators), so runs are stable and
-- comparable across refactors.
--
-- Usage:
--
-- @
-- cabal bench leios-db-bench --benchmark-options='+RTS -N4 -RTS'
-- @
module Main (main) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.Async (async, mapConcurrently_, wait)
import Control.Monad (forM, forM_, when)
import Control.Monad.Class.MonadTime.SI (diffTime, getMonotonicTime)
import Control.Tracer (debugTracer, (>$<))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Time.Clock (DiffTime)
import qualified Data.Vector.Strict as V
import LeiosDemoDb
  ( LeiosDbConnection
  , LeiosDbHandle (..)
  , LeiosDbWriter (..)
  , Promise (..)
  , leiosDbBatchRetrieveTxs
  , leiosDbGarbageCollect
  , leiosDbInsertEbBody
  , leiosDbInsertEbPoint
  , leiosDbInsertTxs
  , leiosDbLookupEbBody
  , leiosDbLookupEbClosure
  , leiosDbScanEbPoints
  , newLeiosDBSQLite
  , withLeiosDb
  , withLeiosDbWriter
  , withWriterBackedDb
  , writerQueueDepth
  )
import LeiosDemoTypes
  ( BytesSize
  , EbHash (..)
  , LeiosEb (..)
  , LeiosPoint (..)
  , TxHash (..)
  , encodeLeiosEbSize
  )
import System.Directory (copyFile, doesFileExist, getFileSize)
import System.Environment (lookupEnv)
import System.IO (hFlush, stdout)
import System.IO.Temp (withSystemTempDirectory)

main :: IO ()
main = do
  mSeed <- lookupEnv seedEnvVar
  putStr $
    unlines
      [ "LeiosDemoDb concurrent benchmark"
      , ""
      , "Database setup:"
      , case mSeed of
          Nothing ->
            "  synthetic         : "
              <> show numPrePopulatedEbs
              <> " EBs × "
              <> show txsPerEb
              <> " TXs"
          Just p -> "  recorded seed     : " <> p
      , ""
      , "Concurrent workload per iteration:"
      , "  Fetch clients   (×" <> show numFetchClients <> "): 20 insertEbPoint/insertEbBody/insertTxs each"
      , "  Fetch servers   (×" <> show numFetchServers <> "): 30 lookupEbBody + 10 batchRetrieveTxs each"
      , "  Chain-sel reader(×1): " <> show numChainSelReads <> " lookupEbClosure calls"
      , "  GC ticker       (×1): " <> show numGcTicks <> " garbageCollect calls"
      , ""
      , "Runs: 1 warmup + " <> show numRuns <> " timed, per write mode"
      , ""
      ]
  modes <- selectedModes
  forM_ modes $ \mode -> do
    putStrLn $ describeMode mode
    withSystemTempDirectory "leios-db-bench" $ \tmpDir -> do
      db <- openBenchDb mSeed tmpDir
      points <- benchPoints mSeed db
      writerIdx <- newIORef numPrePopulatedEbs
      let env = BenchEnv db points writerIdx
      case mode of
        Direct ->
          runBench $ benchConcurrentAll env (directWrites db)
        SingleWriterSync ->
          withWriterBackedDb db queueDepth $ \wdb ->
            runBench $ benchConcurrentAll env{beDb = wdb} (directWrites wdb)
        SingleWriterAsync ->
          withLeiosDbWriter db queueDepth $ \writer ->
            runBench $ benchConcurrentAll env (asyncWrites writer)
    putStrLn ""

-- | Which write modes to run, as a comma-separated list of constructor names
-- in @LEIOS_DB_BENCH_MODES@ (default: all). Restricting to one mode matters
-- when seeding from a recording, since each mode copies the seed afresh.
selectedModes :: IO [WriteMode]
selectedModes =
  lookupEnv "LEIOS_DB_BENCH_MODES" >>= \case
    Nothing -> pure [minBound .. maxBound]
    Just spec -> case traverse parse (splitOn ',' spec) of
      Just ms -> pure ms
      Nothing -> fail ("LEIOS_DB_BENCH_MODES: cannot parse " <> spec)
 where
  parse name = lookup name [(show m, m) | m <- [minBound .. maxBound]]
  splitOn c str = case break (== c) str of
    (chunk, []) -> [chunk]
    (chunk, _ : rest) -> chunk : splitOn c rest

-- | Point an existing (recorded) @leios.db@ at the benchmark, e.g. one of the
-- multi-GB devnet databases. It is copied into the temp dir first, so the
-- recording is never mutated -- budget the disk and the copy time.
seedEnvVar :: String
seedEnvVar = "LEIOS_DB_BENCH_SEED"

-- | How the fetch clients get their writes to disk.
data WriteMode
  = -- | Today: every client writes on its own connection, concurrently.
    Direct
  | -- | All writes funnelled through the one 'LeiosDbWriter', each awaited --
    -- the 'withWriterBackedDb' facade, so the caller cannot tell the
    -- difference. Isolates the cost of serialising.
    SingleWriterSync
  | -- | Same writer, promises dropped: what a fetch client actually pays
    -- before returning to its mini-protocol. The final promise is awaited so
    -- the timing still covers durability.
    SingleWriterAsync
  deriving (Bounded, Enum, Eq, Show)

describeMode :: WriteMode -> String
describeMode = \case
  Direct -> "Direct (per-client connections, concurrent writers)"
  SingleWriterSync -> "SingleWriterSync (one writer, awaited)"
  SingleWriterAsync -> "SingleWriterAsync (one writer, fire-and-forget)"

-- | Queue depth as a node would size it: one slot per upstream peer (here, per
-- fetch client), plus the forge, plus slack.
queueDepth :: Int
queueDepth = writerQueueDepth numFetchClients

-- | Submit a write, and hand back the action that waits for it to be durable.
type Write = (LeiosDbConnection IO -> IO ()) -> IO (IO ())

-- | Give a client body a 'Write', owning whatever connection that needs for
-- the body's lifetime (one per client, as a fetch client has).
type ClientWrites = (Write -> IO ()) -> IO ()

directWrites :: LeiosDbHandle IO -> ClientWrites
directWrites db body =
  withLeiosDb db $ \conn ->
    body $ \op -> op conn >> pure (pure ())

asyncWrites :: LeiosDbWriter IO -> ClientWrites
asyncWrites writer body =
  body $ \op -> await <$> enqueueWrite writer op

-- * Configuration

-- | Number of EBs pre-inserted into the DB during setup (not timed).
numPrePopulatedEbs :: Int
numPrePopulatedEbs = 500

-- | TXs per EB (mid-range; Leios spec allows up to 2000).
txsPerEb :: Int
txsPerEb = 200

-- | Number of fetch client threads (writers that insert fresh EBs).
numFetchClients :: Int
numFetchClients = 3

-- | Number of fetch server threads (readers serving downstream peers).
numFetchServers :: Int
numFetchServers = 3

-- | Number of chain-sel-shaped reader calls per iteration.
numChainSelReads :: Int
numChainSelReads = 50

-- | Number of GC ticks per iteration.
numGcTicks :: Int
numGcTicks = 3

-- | Timed repetitions (plus one warmup).
numRuns :: Int
numRuns = 5

-- * The benchmark

-- | All production roles running concurrently against one DB handle.
benchConcurrentAll :: BenchEnv -> ClientWrites -> IO ()
benchConcurrentAll BenchEnv{beDb = db, bePoints = points, beWriterIdx = writerIdxRef} clientWrites = do
  startIdx <-
    atomicModifyIORef'
      writerIdxRef
      (\n -> (n + numFetchClients * ebsPerClient, n))
  cs <- async (chainSelReader db points)
  gc <- async (gcTicker db)
  clients <- forM (clientRanges startIdx) $ \range -> async (fetchClient clientWrites range)
  mapConcurrently_ (fetchServer db points) [0 .. numFetchServers - 1]
  wait cs >> wait gc
  forM_ clients wait
 where
  ebsPerClient = 20
  clientRanges startIdx =
    [ [startIdx + i * ebsPerClient .. startIdx + (i + 1) * ebsPerClient - 1]
    | i <- [0 .. numFetchClients - 1]
    ]

-- | Mirrors a fetch client: inserts fresh EBs with full TX payloads.
--
-- Awaits are collected and settled at the end, so an asynchronous write mode
-- is still timed to durability -- what it saves is the client's own blocking,
-- not the work.
fetchClient :: ClientWrites -> [Int] -> IO ()
fetchClient clientWrites range =
  clientWrites $ \write -> do
    awaits <- forM range $ \i -> write (`insertOneEb` i)
    sequence_ awaits

-- | Mirrors chain-selection's block-apply path: repeated
-- 'leiosDbLookupEbClosure' for the tx closure of each certified EB.
chainSelReader :: LeiosDbHandle IO -> [LeiosPoint] -> IO ()
chainSelReader db points =
  withLeiosDb db $ \c ->
    forM_ (take numChainSelReads (cycle points)) $ \p ->
      leiosDbLookupEbClosure c p.pointEbHash

-- | Fires periodic garbage-collect calls. Handle-level operation; touches
-- every table when implemented (currently a no-op backend-side, but the
-- call path is realistic).
gcTicker :: LeiosDbHandle IO -> IO ()
gcTicker db =
  forM_ [1 .. numGcTicks] $ \i ->
    leiosDbGarbageCollect db (SlotNo (fromIntegral (i * 10)))

-- | Mirrors a fetch server: looks up EB bodies and retrieves TX batches.
fetchServer :: LeiosDbHandle IO -> [LeiosPoint] -> Int -> IO ()
fetchServer db points i =
  withLeiosDb db $ \c -> do
    forM_ ebPoints $ \p -> leiosDbLookupEbBody c p.pointEbHash
    forM_ txPoints $ \p -> leiosDbBatchRetrieveTxs c p.pointEbHash sampleOffsets
 where
  sampleOffsets = [0, 10 .. txsPerEb - 1]
  ebPoints = take 30 $ drop (i * 30) (cycle points)
  txPoints = take 10 $ drop (i * 10) (cycle points)

-- * Benchmark environment

data BenchEnv = BenchEnv
  { beDb :: !(LeiosDbHandle IO)
  , bePoints :: ![LeiosPoint]
  -- ^ Pre-computed list of all 'numPrePopulatedEbs' points.
  , beWriterIdx :: !(IORef Int)
  -- ^ Monotonically increasing counter so each benchmark iteration allocates
  -- a fresh range of EB indices for writers (avoids duplicate-key errors).
  }

-- | Open the benchmark database: either a fresh one pre-populated with
-- 'numPrePopulatedEbs' synthetic EBs, or a copy of a recorded one.
--
-- Setup cost is not included in the timed measurements.
openBenchDb :: Maybe FilePath -> FilePath -> IO (LeiosDbHandle IO)
openBenchDb mSeed tmpDir = case mSeed of
  Just seed -> do
    exists <- doesFileExist seed
    when (not exists) $ fail (seedEnvVar <> ": no such file: " <> seed)
    size <- getFileSize seed
    putStr ("Copying recorded DB (" <> show (size `div` 1_000_000) <> " MB): ")
      >> hFlush stdout
    copyFile seed dbPath
    -- The sidecars matter: a recording checkpointed lazily keeps committed
    -- pages in the WAL, so copying only the .db silently loses them.
    forM_ ["-wal", "-shm"] $ \ext -> do
      let from = seed <> ext
      present <- doesFileExist from
      when present $ copyFile from (dbPath <> ext)
    putStrLn "done"
    newLeiosDBSQLite (show >$< debugTracer) dbPath
  Nothing -> do
    db <- newLeiosDBSQLite (show >$< debugTracer) dbPath
    putStr "Inserting EBs: " >> hFlush stdout
    forM_ [0 .. numPrePopulatedEbs - 1] $ \i -> do
      withLeiosDb db (`insertOneEb` i)
      when (i `mod` (numPrePopulatedEbs `div` 10) == numPrePopulatedEbs `div` 10 - 1) $
        putStr (show (i + 1) <> " ") >> hFlush stdout
    putStrLn "done"
    pure db
 where
  dbPath = tmpDir <> "/bench.db"

-- | The EBs the read-side roles cycle through: whatever the recording holds,
-- or the synthetic ones we just inserted.
benchPoints :: Maybe FilePath -> LeiosDbHandle IO -> IO [LeiosPoint]
benchPoints mSeed db = case mSeed of
  Nothing -> pure [genPoint i | i <- [0 .. numPrePopulatedEbs - 1]]
  Just _ -> do
    recorded <- withLeiosDb db leiosDbScanEbPoints
    let points = [MkLeiosPoint slot ebHash | (slot, ebHash) <- take numPrePopulatedEbs recorded]
    when (null points) $ fail "recorded DB has no EB points to read"
    putStrLn $ "  reading " <> show (length points) <> " recorded EBs"
    pure points

-- * Timing

-- | Warm up once, then time 'numRuns' repetitions, printing each result and a
-- final min\/avg\/max summary.
runBench :: IO () -> IO ()
runBench action = do
  action -- warmup (not printed)
  times <- forM [1 .. numRuns] $ \i -> do
    t <- snd <$> timed action
    putStrLn $ "  run " <> show i <> "/" <> show numRuns <> ": " <> showTime t
    pure t
  let avg = sum times / fromIntegral (length times)
      minT = minimum times
      maxT = maximum times
  putStrLn $
    "  => min=" <> showTime minT <> "  avg=" <> showTime avg <> "  max=" <> showTime maxT

timed :: IO a -> IO (a, DiffTime)
timed action = do
  t0 <- getMonotonicTime
  !result <- action
  t1 <- getMonotonicTime
  pure (result, diffTime t1 t0)

showTime :: DiffTime -> String
showTime t
  | t < 1e-6 = show (round (s * 1_000_000_000 :: Double) :: Int) <> " ns"
  | t < 1e-3 = show (round (s * 1_000_000 :: Double) :: Int) <> " μs"
  | t < 1 = show (round (s * 1_000 :: Double) :: Int) <> " ms"
  | otherwise = show s <> " s"
 where
  s = realToFrac t :: Double

-- * DB helpers

-- | Insert one complete EB (point + body + all TXs) by index.
insertOneEb :: Monad m => LeiosDbConnection m -> Int -> m ()
insertOneEb conn ebIdx = do
  let point = genPoint ebIdx
      eb = genEb ebIdx
      txs =
        [ (h, genTx h)
        | txIdx <- [0 .. txsPerEb - 1]
        , let h = genTxHash ebIdx txIdx
        ]
  leiosDbInsertEbPoint conn point (encodeLeiosEbSize eb)
  _ <- leiosDbInsertEbBody conn point eb
  _ <- leiosDbInsertTxs conn txs
  pure ()

-- * Deterministic data generation

-- | 'LeiosPoint' from an index (SlotNo = index).
genPoint :: Int -> LeiosPoint
genPoint i = MkLeiosPoint (SlotNo $ fromIntegral i) (genEbHash i)

-- | 'EbHash' from an index: \"ebHash:<index>\" padded to 32 bytes with zeros.
genEbHash :: Int -> EbHash
genEbHash i = MkEbHash $ BS.take 32 (tag <> BS.replicate 32 0)
 where
  tag = BS8.pack ("ebHash:" <> show i)

-- | 'LeiosEb' with 'txsPerEb' transactions (200 bytes each).
genEb :: Int -> LeiosEb
genEb ebIdx =
  MkLeiosEb $
    V.fromList
      [(genTxHash ebIdx txIdx, 200 :: BytesSize) | txIdx <- [0 .. txsPerEb - 1]]

-- | 'TxHash' from an EB index + TX offset: \"txHash:<ebIdx>:<txIdx>\" padded
-- to 32 bytes with zeros.
--
-- NOTE: This is taking an EB index as it always generates the worst case of
-- fully disjunct transaction closures between EBs.
genTxHash :: Int -> Int -> TxHash
genTxHash ebIdx txIdx = MkTxHash $ BS.take 32 (tag <> BS.replicate 32 0)
 where
  tag = BS8.pack ("txHash:" <> show ebIdx <> ":" <> show txIdx)

-- | Generate a TX payload: the TX hash bytes padded with zeros to 16 KiB.
genTx :: TxHash -> BS.ByteString
genTx (MkTxHash h) = h <> BS.replicate (16_384 - BS.length h) 0
