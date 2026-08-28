{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Control.Monad (forM, forM_, void, when)
import Control.Monad.Class.MonadTime.SI (diffTime, getMonotonicTime)
import Control.Tracer (debugTracer, (>$<))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (sort)
import Data.Time.Clock (DiffTime)
import qualified Data.Vector.Strict as V
import LeiosDemoDb
  ( LeiosDbConnection
  , LeiosDbHandle (..)
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
  )
import LeiosDemoTypes
  ( BytesSize
  , EbHash (..)
  , LeiosEb (..)
  , LeiosPoint (..)
  , TxHash (..)
  , leiosEbBytesSize
  )
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.IO (hFlush, stdout)
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = do
  putStr $
    unlines
      [ "LeiosDemoDb concurrent benchmark"
      , ""
      , "Database setup:"
      , "  EBs pre-populated : " <> show numPrePopulatedEbs
      , "  TXs per EB        : " <> show txsPerEb
      , "  TX hash pool      : "
          <> (if txHashPool <= 0 then "disjoint per EB" else show txHashPool <> " shared")
      , "  Total TXs         : " <> show (numPrePopulatedEbs * txsPerEb)
      , ""
      , "Concurrent workload per iteration:"
      , "  Fetch clients   (×" <> show numFetchClients <> "): 20 insertEbPoint/insertEbBody/insertTxs each"
      , "  Fetch servers   (×" <> show numFetchServers <> "): 30 lookupEbBody + 10 batchRetrieveTxs each"
      , "  Chain-sel reader(×1): " <> show numChainSelReads <> " lookupEbClosure calls"
      , "  GC ticker       (×1): " <> show numGcTicks <> " garbageCollect calls"
      , ""
      , "Runs: 1 warmup + " <> show numRuns <> " timed"
      , ""
      ]
  -- The default temp directory is often tmpfs, i.e. RAM: measurements taken
  -- there miss everything the storage layer does. Point this at the same
  -- filesystem the node uses.
  lookupEnv "LEIOS_DB_BENCH_DIR" >>= \case
    Just dir -> do
      putStrLn $ "  Database directory: " <> dir <> " (from LEIOS_DB_BENCH_DIR)"
      env <- setupBenchEnv dir
      runBench (beInsertTimes env) (benchConcurrentAll env)
    Nothing ->
      withSystemTempDirectory "leios-db-bench" $ \tmpDir -> do
        putStrLn $ "  Database directory: " <> tmpDir <> " (system temp -- may be tmpfs)"
        env <- setupBenchEnv tmpDir
        runBench (beInsertTimes env) (benchConcurrentAll env)

-- * Configuration

-- | Read an @Int@ knob from the environment, defaulting if unset.
--
-- The shape of the database dominates every measurement here, and the
-- production shape is nothing like the defaults: a devnet reached 13.5k txs per
-- EB with closures overlapping almost completely, against 200 unique ones here.
-- Rather than bake one shape in, the three that matter are knobs.
envInt :: String -> Int -> Int
envInt name def = unsafePerformIO $ maybe def read <$> lookupEnv name
{-# NOINLINE envInt #-}

-- | Number of EBs pre-inserted into the DB during setup (not timed).
numPrePopulatedEbs :: Int
numPrePopulatedEbs = envInt "LEIOS_DB_BENCH_EBS" 500
{-# NOINLINE numPrePopulatedEbs #-}

-- | TXs per EB. Production reached 13568, the cap set by 'maxEBClosureSize'.
txsPerEb :: Int
txsPerEb = envInt "LEIOS_DB_BENCH_TXS_PER_EB" 200
{-# NOINLINE txsPerEb #-}

-- | Size of the shared pool tx hashes are drawn from, or 0 for a fresh hash per
-- (EB, offset).
--
-- Zero -- the historical behaviour -- means no two EBs ever share a
-- transaction, so 'txs' grows as fast as 'ebTxs'. In production the mempool
-- does not drain between EBs, so successive closures repeat almost the same
-- transactions: 'ebTxs' grew to 4.7x the distinct hashes it referenced. A pool
-- smaller than @numPrePopulatedEbs * txsPerEb@ reproduces that.
txHashPool :: Int
txHashPool = envInt "LEIOS_DB_BENCH_TX_POOL" 0
{-# NOINLINE txHashPool #-}

-- | Skip 'leiosDbInsertTxs', leaving only what the node traces as its EB body
-- ingest.
skipInsertTxs :: Bool
skipInsertTxs = envInt "LEIOS_DB_BENCH_SKIP_INSERTTXS" 0 /= 0
{-# NOINLINE skipInsertTxs #-}

-- | Number of fetch client threads (writers that insert fresh EBs).
numFetchClients :: Int
numFetchClients = envInt "LEIOS_DB_BENCH_CLIENTS" 3
{-# NOINLINE numFetchClients #-}

-- | Number of fetch server threads (readers serving downstream peers).
numFetchServers :: Int
numFetchServers = envInt "LEIOS_DB_BENCH_SERVERS" 3
{-# NOINLINE numFetchServers #-}

-- | Number of chain-sel-shaped reader calls per iteration.
numChainSelReads :: Int
numChainSelReads = envInt "LEIOS_DB_BENCH_CHAINSEL" 50
{-# NOINLINE numChainSelReads #-}

-- | Number of GC ticks per iteration.
numGcTicks :: Int
numGcTicks = envInt "LEIOS_DB_BENCH_GC" 3
{-# NOINLINE numGcTicks #-}

-- | Timed repetitions (plus one warmup).
numRuns :: Int
numRuns = 5

-- * The benchmark

-- | All production roles running concurrently against one DB handle.
benchConcurrentAll :: BenchEnv -> IO ()
benchConcurrentAll BenchEnv
                     { beDb = db
                     , bePoints = points
                     , bePool = pool
                     , beInsertTimes = times
                     , beWriterIdx = writerIdxRef
                     } = do
  startIdx <-
    atomicModifyIORef'
      writerIdxRef
      (\n -> (n + numFetchClients * ebsPerClient, n))
  cs <- async (chainSelReader db points)
  gc <- async (gcTicker db)
  clients <- forM (clientRanges startIdx) $ \range -> async (fetchClient db pool times range)
  mapConcurrently_ (fetchServer db points) [0 .. numFetchServers - 1 :: Int]
  wait cs >> wait gc
  forM_ clients wait
 where
  ebsPerClient = 20
  clientRanges startIdx =
    [ [startIdx + i * ebsPerClient .. startIdx + (i + 1) * ebsPerClient - 1]
    | i <- [0 .. numFetchClients - 1]
    ]

-- | Mirrors a fetch client: inserts fresh EBs with full TX payloads.
fetchClient :: LeiosDbHandle IO -> V.Vector TxHash -> IORef [Split] -> [Int] -> IO ()
fetchClient db pool times range =
  withLeiosDb db $ \c ->
    forM_ range $ \i -> do
      sp <- insertOneEbTimed c pool i
      atomicModifyIORef' times (\ts -> (sp : ts, ()))

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

-- | Per-insert durations of 'insertEbPoint', 'insertEbBody' and 'insertTxs'.
type Split = (DiffTime, DiffTime, DiffTime)

data BenchEnv = BenchEnv
  { beDb :: !(LeiosDbHandle IO)
  , bePoints :: ![LeiosPoint]
  -- ^ Pre-computed list of all 'numPrePopulatedEbs' points.
  , bePool :: !(V.Vector TxHash)
  -- ^ Tx hashes the writers draw from; empty means synthesise fresh ones.
  , beInsertTimes :: !(IORef [Split])
  -- ^ Per-'insertOneEb' durations, so the writer path can be read separately
  -- from an iteration total that is a max over concurrent roles, not a sum.
  , beWriterIdx :: !(IORef Int)
  -- ^ Monotonically increasing counter so each benchmark iteration allocates
  -- a fresh range of EB indices for writers (avoids duplicate-key errors).
  }

-- | Open the benchmark database, adopting one that is already there.
--
-- Copy a node's @leios.db@ into the directory as @bench.db@ and the benchmark
-- runs against production data at production scale, with no setup: the readers
-- cycle real EBs and the writers draw their closures from real transaction
-- hashes, so the anti-join finds them present, as it does on a node whose
-- mempool is not draining. Building an equivalent database here would take
-- hours and still not have the same shape.
setupBenchEnv :: FilePath -> IO BenchEnv
setupBenchEnv tmpDir = do
  let path = tmpDir <> "/bench.db"
  adopted <- doesFileExist path
  db <- newLeiosDBSQLite (show >$< debugTracer) path
  points <-
    if adopted
      then do
        ps <- withLeiosDb db leiosDbScanEbPoints
        putStrLn $ "Adopted existing database: " <> show (length ps) <> " EBs"
        pure [MkLeiosPoint slot h | (slot, h) <- ps]
      else do
        putStr "Inserting EBs: " >> hFlush stdout
        forM_ [0 .. numPrePopulatedEbs - 1] $ \i -> do
          withLeiosDb db (\c -> insertOneEb c V.empty i)
          when (i `mod` (numPrePopulatedEbs `div` 10) == numPrePopulatedEbs `div` 10 - 1) $
            putStr (show (i + 1) <> " ") >> hFlush stdout
        putStrLn "done"
        pure [genPoint i | i <- [0 .. numPrePopulatedEbs - 1]]
  when (null points) $ error "leios-db-bench: no EBs in the database"
  -- Writers reuse hashes the database already holds, so their closures overlap
  -- with stored transactions the way production closures do.
  pool <-
    if adopted
      then do
        -- Walk stored EBs until there are enough hashes to fill a closure. The
        -- first EBs a scan returns can be nearly empty, and a pool smaller than
        -- the closure would make the anti-join probe a handful of rows over and
        -- over instead of the ~13.5k distinct ones a real body probes.
        hs <- withLeiosDb db $ \c ->
          let go :: [TxHash] -> [LeiosPoint] -> IO [TxHash]
              go acc [] = pure acc
              go acc (pt : rest)
                | length acc >= txsPerEb = pure acc
                | otherwise = do
                    body <- leiosDbLookupEbBody c pt.pointEbHash
                    go (acc <> map fst body) rest
           in go [] (reverse points)
        putStrLn $ "Writer tx pool: " <> show (length hs) <> " hashes sampled from stored EBs"
        pure $ V.fromList hs
      else pure V.empty
  writerIdx <- newIORef numPrePopulatedEbs
  insertTimes <- newIORef []
  pure $ BenchEnv db points pool insertTimes writerIdx

-- * Timing

-- | Warm up once, then time 'numRuns' repetitions, printing each result and a
-- final min\/avg\/max summary.
runBench :: IORef [Split] -> IO () -> IO ()
runBench insertTimes action = do
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
  splits <- readIORef insertTimes
  let stat name sel =
        let xs = sort (map sel splits)
            pick q = xs !! min (length xs - 1) (floor (q * fromIntegral (length xs) :: Double))
         in putStrLn $
              "    "
                <> name
                <> ": median="
                <> showTime (pick 0.5)
                <> "  p90="
                <> showTime (pick 0.9)
                <> "  max="
                <> showTime (last xs)
  putStrLn $ "  insertOneEb (n=" <> show (length splits) <> "), by call:"
  stat "insertEbPoint" (\(a, _, _) -> a)
  stat "insertEbBody " (\(_, b, _) -> b)
  stat "insertTxs    " (\(_, _, c) -> c)
  stat "total        " (\(a, b, c) -> a + b + c)

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
insertOneEb :: Monad m => LeiosDbConnection m -> V.Vector TxHash -> Int -> m ()
insertOneEb conn pool ebIdx = do
  let hashAt = ebTxHash pool ebIdx
      point = genPoint ebIdx
      eb = MkLeiosEb $ V.fromList [(hashAt i, 200 :: BytesSize) | i <- [0 .. txsPerEb - 1]]
      txs = [(h, genTx h) | txIdx <- [0 .. txsPerEb - 1], let h = hashAt txIdx]
  leiosDbInsertEbPoint conn point (leiosEbBytesSize eb)
  _ <- leiosDbInsertEbBody conn point eb (candidatesOf eb)
  _ <- leiosDbInsertTxs conn txs
  pure ()

-- | As 'insertOneEb', but reporting how long each of the three calls took.
--
-- Poor-man's profiling: a profiled build needs a profiling variant of every
-- linked library, which the nix shell does not ship, so the writer path is
-- bisected by hand instead.
insertOneEbTimed ::
  LeiosDbConnection IO -> V.Vector TxHash -> Int -> IO (DiffTime, DiffTime, DiffTime)
insertOneEbTimed conn pool ebIdx = do
  let hashAt = ebTxHash pool ebIdx
      point = genPoint ebIdx
      eb = MkLeiosEb $ V.fromList [(hashAt i, 200 :: BytesSize) | i <- [0 .. txsPerEb - 1]]
      txs = [(h, genTx h) | txIdx <- [0 .. txsPerEb - 1], let h = hashAt txIdx]
  (_, tPoint) <- timed $ leiosDbInsertEbPoint conn point (leiosEbBytesSize eb)
  (_, tBody) <- timed $ leiosDbInsertEbBody conn point eb (candidatesOf eb)
  -- The node's 'dbInsertMs' spans only the two calls above; 'insertTxs' happens
  -- later, outside that trace. Skipping it isolates the stage production
  -- actually measures.
  (_, tTxs) <-
    if skipInsertTxs
      then pure ((), 0)
      else timed $ void (leiosDbInsertTxs conn txs)
  pure (tPoint, tBody, tTxs)

-- * Deterministic data generation

-- | Which hash a given (EB, offset) slot uses: its own, or one from the shared
-- pool when 'txHashPool' is set.
hashIndex :: Int -> Int -> (Int, Int)
hashIndex ebIdx txIdx
  | txHashPool <= 0 = (ebIdx, txIdx)
  | otherwise = (0, (ebIdx * txsPerEb + txIdx) `mod` txHashPool)

-- | What the writer claims the cache could not vouch for.
--
-- 'LEIOS_DB_BENCH_CANDIDATES' as a percentage: 100 is the pre-change behaviour
-- of checking every tx in the body, 0 is a cache that vouches for the whole
-- closure, which is the steady state on a devnet.
candidatesOf :: LeiosEb -> [TxHash]
candidatesOf (MkLeiosEb v) =
  map fst . take n $ V.toList v
 where
  n = (V.length v * candidatePercent) `div` 100

candidatePercent :: Int
candidatePercent = envInt "LEIOS_DB_BENCH_CANDIDATES" 100
{-# NOINLINE candidatePercent #-}

-- | The hash a writer puts at a given offset: one the database already holds
-- when a pool was sampled, otherwise a synthetic one.
ebTxHash :: V.Vector TxHash -> Int -> Int -> TxHash
ebTxHash pool ebIdx txIdx
  | V.null pool = genTxHash ebIdx txIdx
  | otherwise = pool V.! ((ebIdx * txsPerEb + txIdx) `mod` V.length pool)

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
-- NOTE: Takes an EB index because with 'txHashPool' unset it generates the worst
-- case of fully disjunct closures between EBs. With a pool set, the index pair
-- collapses onto the pool and successive closures overlap, as they do under a
-- mempool that is not draining.
genTxHash :: Int -> Int -> TxHash
genTxHash ebIdx0 txIdx0 = MkTxHash $ BS.take 32 (tag <> BS.replicate 32 0)
 where
  (ebIdx, txIdx) = hashIndex ebIdx0 txIdx0
  tag = BS8.pack ("txHash:" <> show ebIdx <> ":" <> show txIdx)

-- | Generate a TX payload: the TX hash bytes padded with zeros to 16 KiB.
genTx :: TxHash -> BS.ByteString
genTx (MkTxHash h) = h <> BS.replicate (16_384 - BS.length h) 0
