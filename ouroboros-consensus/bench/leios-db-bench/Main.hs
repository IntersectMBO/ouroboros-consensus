{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Concurrent benchmark for 'LeiosDemoDb' mirroring production access patterns.
--
-- The following roles run concurrently against the same SQLite handle:
--
-- * __Fetch logic__ (1 thread): configurable rounds of
--   'leiosDbFilterMissingEbBodies' + 'leiosDbFilterMissingTxs'.
--
-- * __Fetch clients__ (configurable, default 3 threads): each inserts 20 fresh
--   EBs via 'leiosDbInsertEbPoint' → 'leiosDbInsertEbBody' → 'leiosDbInsertTxs'.
--   Blind-write client role.
--
-- * __Fetch client RMW__ (configurable, default 2 threads): mirrors
--   @msgLeiosBlock@ — for each EB, 'leiosDbLookupEbPoint' first, then only
--   'leiosDbInsertEbPoint' if the point was not already present, then
--   'leiosDbInsertEbBody' and 'leiosDbInsertTxs'. Exercises the
--   read-then-write pattern that a shared writer/reader split has to
--   handle atomically.
--
-- * __Fetch servers__ (configurable, default 3 threads): each does 30
--   'leiosDbLookupEbBody' + 10 'leiosDbBatchRetrieveTxs' calls cycling through
--   the pre-populated EBs.
--
-- * __Chain-sel reader__ (1 thread): mimics the block-apply path via
--   'leiosDbQueryCompletedEbByHash' — the same read that
--   'resolveLeiosClosure' issues per Dijkstra-era CertRB.
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
import Control.Monad (forM, forM_, replicateM_, when)
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
  , leiosDbBatchRetrieveTxs
  , leiosDbFilterMissingEbBodies
  , leiosDbFilterMissingTxs
  , leiosDbGarbageCollect
  , leiosDbInsertEbBody
  , leiosDbInsertEbPoint
  , leiosDbInsertTxs
  , leiosDbLookupEbBody
  , leiosDbLookupEbPoint
  , leiosDbQueryCompletedEbByHash
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
import System.IO (hFlush, stdout)
import System.IO.Temp (withSystemTempDirectory)

main :: IO ()
main = do
  putStr $
    unlines
      [ "LeiosDemoDb concurrent benchmark"
      , ""
      , "Database setup:"
      , "  EBs pre-populated : " <> show numPrePopulatedEbs
      , "  TXs per EB        : " <> show txsPerEb
      , "  Total TXs         : " <> show (numPrePopulatedEbs * txsPerEb)
      , ""
      , "Concurrent workload per iteration:"
      , "  Fetch logic     (×1): "
          <> show numFetchLogicRounds
          <> " rounds of filterMissingEbBodies (200+200) + filterMissingTxs (500+500)"
      , "  Fetch clients   (×" <> show numFetchClients <> "): 20 insertEbPoint/insertEbBody/insertTxs each"
      , "  Fetch client RMW(×" <> show numRmwClients <> "): 20 lookup+conditional insert each (msgLeiosBlock shape)"
      , "  Fetch servers   (×" <> show numFetchServers <> "): 30 lookupEbBody + 10 batchRetrieveTxs each"
      , "  Chain-sel reader(×1): " <> show numChainSelReads <> " queryCompletedEbByHash calls"
      , "  GC ticker       (×1): " <> show numGcTicks <> " garbageCollect calls"
      , ""
      , "Runs: 1 warmup + " <> show numRuns <> " timed"
      , ""
      ]
  withSystemTempDirectory "leios-db-bench" $ \tmpDir -> do
    env <- setupBenchEnv tmpDir
    runBench (benchConcurrentAll env)

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

-- | Number of "msgLeiosBlock-shaped" clients that read before writing.
numRmwClients :: Int
numRmwClients = 2

-- | Number of fetch server threads (readers serving downstream peers).
numFetchServers :: Int
numFetchServers = 3

-- | Number of rounds the fetch logic thread performs per iteration.
numFetchLogicRounds :: Int
numFetchLogicRounds = 100

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
benchConcurrentAll :: BenchEnv -> IO ()
benchConcurrentAll BenchEnv{beDb = db, bePoints = points, beWriterIdx = writerIdxRef} = do
  -- Blind writers and RMW writers both claim fresh EB-index ranges so we
  -- don't hit UNIQUE-violations across concurrent iterations.
  startBlind <- atomicModifyIORef' writerIdxRef
    (\n -> (n + numFetchClients * ebsPerClient, n))
  startRmw <- atomicModifyIORef' writerIdxRef
    (\n -> (n + numRmwClients * ebsPerClient, n))
  fl      <- async (fetchLogic db points)
  cs      <- async (chainSelReader db points)
  gc      <- async (gcTicker db)
  clients    <- forM (rangeFor startBlind numFetchClients) $ \r -> async (fetchClient db r)
  rmwClients <- forM (rangeFor startRmw numRmwClients)     $ \r -> async (fetchClientRmw db r)
  mapConcurrently_ (fetchServer db points) [0 .. numFetchServers - 1]
  wait fl >> wait cs >> wait gc
  forM_ (clients <> rmwClients) wait
 where
  ebsPerClient = 20
  rangeFor start n =
    [ [start + i * ebsPerClient .. start + (i + 1) * ebsPerClient - 1]
    | i <- [0 .. n - 1]
    ]

-- | Mirrors the fetch logic loop: filters for missing EB bodies and TXs.
fetchLogic :: LeiosDbHandle IO -> [LeiosPoint] -> IO ()
fetchLogic db points =
  withLeiosDb db $ \c ->
    replicateM_ numFetchLogicRounds $ do
      _ <- leiosDbFilterMissingEbBodies c (existingPoints ++ missingPoints)
      _ <- leiosDbFilterMissingTxs c (existingHashes ++ missingHashes)
      pure ()
 where
  existingPoints = take 200 points
  missingPoints = [genPoint i | i <- [10_000 .. 10_199]]
  existingHashes = [genTxHash 0 i | i <- [0 .. 499]]
  missingHashes = [genTxHash 10_000 i | i <- [0 .. 499]]

-- | Mirrors a fetch client: inserts fresh EBs with full TX payloads.
fetchClient :: LeiosDbHandle IO -> [Int] -> IO ()
fetchClient db range =
  withLeiosDb db $ \c ->
    forM_ range (insertOneEb c)

-- | Mirrors 'msgLeiosBlock' on the fetch-client response path: look up the
-- EB point first, insert it only if missing, then insert the body and its
-- tx closure. This is the read-then-write shape that a shared
-- writer\/reader-split API has to keep atomic.
fetchClientRmw :: LeiosDbHandle IO -> [Int] -> IO ()
fetchClientRmw db range =
  withLeiosDb db $ \c ->
    forM_ range (insertOneEbRmw c)

-- | Mirrors chain-selection's block-apply path: repeated
-- 'leiosDbQueryCompletedEbByHash' for the closure of the certified EB.
chainSelReader :: LeiosDbHandle IO -> [LeiosPoint] -> IO ()
chainSelReader db points =
  withLeiosDb db $ \c ->
    forM_ (take numChainSelReads (cycle points)) $ \p ->
      leiosDbQueryCompletedEbByHash c p.pointEbHash

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

-- | Create a fresh SQLite DB and insert 'numPrePopulatedEbs' complete EBs.
-- This setup cost is not included in the timed measurements.
setupBenchEnv :: FilePath -> IO BenchEnv
setupBenchEnv tmpDir = do
  db <- newLeiosDBSQLite (show >$< debugTracer) (tmpDir <> "/bench.db")
  putStr "Inserting EBs: " >> hFlush stdout
  forM_ [0 .. numPrePopulatedEbs - 1] $ \i -> do
    withLeiosDb db (`insertOneEb` i)
    when (i `mod` (numPrePopulatedEbs `div` 10) == numPrePopulatedEbs `div` 10 - 1) $
      putStr (show (i + 1) <> " ") >> hFlush stdout
  putStrLn "done"
  let points = [genPoint i | i <- [0 .. numPrePopulatedEbs - 1]]
  writerIdx <- newIORef numPrePopulatedEbs
  pure $ BenchEnv db points writerIdx

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
  leiosDbInsertEbPoint conn point (leiosEbBytesSize eb)
  leiosDbInsertEbBody conn point eb
  _ <- leiosDbInsertTxs conn txs
  pure ()

-- | Read-modify-write insert: check whether the EB point is already
-- known and only insert it if not, then insert body + txs. Mirrors
-- 'msgLeiosBlock' on the fetch-client response path.
insertOneEbRmw :: Monad m => LeiosDbConnection m -> Int -> m ()
insertOneEbRmw conn ebIdx = do
  let point = genPoint ebIdx
      eb = genEb ebIdx
      txs =
        [ (h, genTx h)
        | txIdx <- [0 .. txsPerEb - 1]
        , let h = genTxHash ebIdx txIdx
        ]
  existing <- leiosDbLookupEbPoint conn point.pointEbHash
  case existing of
    Just _ -> pure ()
    Nothing -> leiosDbInsertEbPoint conn point (leiosEbBytesSize eb)
  leiosDbInsertEbBody conn point eb
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
