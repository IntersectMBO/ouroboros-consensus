{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Concurrent benchmark for 'LeiosDemoDb' mirroring production access patterns.
--
-- The following three roles run concurrently against the same SQLite handle:
--
-- * __Active reader__ (fetch logic loop): 10 rounds of
--   'leiosDbFilterMissingEbBodies' + 'leiosDbFilterMissingTxs'.
--
-- * __Writers__ (2 threads): each inserts 20 fresh EBs via
--   'leiosDbInsertEbPoint' → 'leiosDbInsertEbBody' → 'leiosDbInsertTxs'.
--
-- * __Serving readers__ (8 threads): each does 30 'leiosDbLookupEbBody' +
--   10 'leiosDbBatchRetrieveTxs' calls cycling through the pre-populated EBs.
--
-- All data is deterministic (no QuickCheck generators), so runs are stable and
-- comparable before\/after lock-removal refactors.
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
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Time.Clock (DiffTime)
import qualified Data.Vector as V
import LeiosDemoDb
  ( LeiosDbHandle (..)
  , newLeiosDBSQLite
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
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)

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
      , "  Active reader (×1): 10 rounds of filterMissingEbBodies (200+200) + filterMissingTxs (500+500)"
      , "  Writers       (×2): 20 insertEbPoint/insertEbBody/insertTxs each"
      , "  Readers       (×" <> show numServingThreads <> "): 30 lookupEbBody + 10 batchRetrieveTxs each"
      , ""
      , "Runs: 1 warmup + " <> show numRuns <> " timed"
      , ""
      ]
  env <- setupBenchEnv
  runBench (benchConcurrentAll env)
  leiosDbClose env.beDb

-- * Configuration

-- | Number of EBs pre-inserted into the DB during setup (not timed).
numPrePopulatedEbs :: Int
numPrePopulatedEbs = 500

-- | TXs per EB (mid-range; Leios spec allows up to 2000).
txsPerEb :: Int
txsPerEb = 200

-- | Number of serving-reader threads (represents downstream peers).
numServingThreads :: Int
numServingThreads = 8

-- | Timed repetitions (plus one warmup).
numRuns :: Int
numRuns = 5

-- * The benchmark

-- | All three production roles running concurrently against one DB handle.
--
-- Per iteration:
--
-- * Active reader (1 thread): 10 rounds of @filterMissingEbBodies@ on
--   200 present + 200 absent points, and @filterMissingTxs@ on 500 present +
--   500 absent hashes.
--
-- * Writers (2 threads): each inserts 20 fresh EBs with full TX payloads.
--   Indices are unique per iteration via 'beWriterIdx'.
--
-- * Serving readers (8 threads): each does 30 @lookupEbBody@ + 10
--   @batchRetrieveTxs@ (sampled offsets) cycling through pre-populated EBs.
benchConcurrentAll :: BenchEnv -> IO ()
benchConcurrentAll BenchEnv{beDb = db, bePoints = points, beWriterIdx = writerIdxRef} = do
  -- Allocate 40 fresh EB indices for the writers in this iteration.
  startIdx <- atomicModifyIORef' writerIdxRef (\n -> (n + 40, n))
  let writerRange1 = [startIdx .. startIdx + 19]
      writerRange2 = [startIdx + 20 .. startIdx + 39]
      -- Active reader mirrors filterMissingWork from LeiosDemoLogic
      activeReader =
        replicateM_ 10 $ do
          let existingPoints = take 200 points
              -- Points beyond the pre-populated range — guaranteed missing
              missingPoints = [genPoint i | i <- [10_000 .. 10_199]]
          _ <- leiosDbFilterMissingEbBodies db (existingPoints ++ missingPoints)
          let existingHashes = [genTxHash 0 i | i <- [0 .. 499]]
              missingHashes = [genTxHash 10_000 i | i <- [0 .. 499]]
          _ <- leiosDbFilterMissingTxs db (existingHashes ++ missingHashes)
          pure ()
      -- Writers insert fresh EBs (unique per call)
      writer range = forM_ range (insertOneEb db)
      -- Serving readers mirror msgLeiosBlockRequest / msgLeiosBlockTxsRequest
      sampleOffsets = [0, 10 .. txsPerEb - 1]
      servingOps i = do
        let ebPoints = take 30 $ drop (i * 30) (cycle points)
            txPoints = take 10 $ drop (i * 10) (cycle points)
        forM_ ebPoints $ \p -> leiosDbLookupEbBody db p.pointEbHash
        forM_ txPoints $ \p -> leiosDbBatchRetrieveTxs db p.pointEbHash sampleOffsets
  -- Start active reader and writers; serving readers are the "main" work.
  ar <- async activeReader
  w1 <- async (writer writerRange1)
  w2 <- async (writer writerRange2)
  mapConcurrently_ servingOps [0 .. numServingThreads - 1]
  wait ar
  wait w1
  wait w2

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
setupBenchEnv :: IO BenchEnv
setupBenchEnv = do
  sysTmp <- getCanonicalTemporaryDirectory
  tmpDir <- createTempDirectory sysTmp "leios-db-bench"
  db <- newLeiosDBSQLite (tmpDir <> "/bench.db")
  putStr "Inserting EBs: " >> hFlush stdout
  forM_ [0 .. numPrePopulatedEbs - 1] $ \i -> do
    insertOneEb db i
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
insertOneEb :: LeiosDbHandle IO -> Int -> IO ()
insertOneEb db ebIdx = do
  let point = genPoint ebIdx
      eb = genEb ebIdx
      txs = [(genTxHash ebIdx txIdx, fixedTxBytes) | txIdx <- [0 .. txsPerEb - 1]]
  leiosDbInsertEbPoint db point (leiosEbBytesSize eb)
  leiosDbInsertEbBody db point eb
  _ <- leiosDbInsertTxs db txs
  pure ()

-- * Deterministic data generation

-- | 'EbHash' from a 32-bit index: 4 index bytes followed by 28 zeros.
genEbHash :: Int -> EbHash
genEbHash i =
  MkEbHash $
    BS.pack
      [ fromIntegral (i .&. 0xFF)
      , fromIntegral ((i `shiftR` 8) .&. 0xFF)
      , fromIntegral ((i `shiftR` 16) .&. 0xFF)
      , fromIntegral ((i `shiftR` 24) .&. 0xFF)
      ]
      <> BS.replicate 28 0

-- | 'TxHash' from an EB index + TX offset: 4 bytes followed by 28 ones.
genTxHash :: Int -> Int -> TxHash
genTxHash ebIdx txIdx =
  MkTxHash $
    BS.pack
      [ fromIntegral (ebIdx .&. 0xFF)
      , fromIntegral ((ebIdx `shiftR` 8) .&. 0xFF)
      , fromIntegral (txIdx .&. 0xFF)
      , fromIntegral ((txIdx `shiftR` 8) .&. 0xFF)
      ]
      <> BS.replicate 28 1

-- | 'LeiosPoint' from an index (SlotNo = index).
genPoint :: Int -> LeiosPoint
genPoint i = MkLeiosPoint (SlotNo $ fromIntegral i) (genEbHash i)

-- | 'LeiosEb' with 'txsPerEb' transactions (200 bytes each).
genEb :: Int -> LeiosEb
genEb ebIdx =
  MkLeiosEb $
    V.fromList
      [(genTxHash ebIdx txIdx, 200 :: BytesSize) | txIdx <- [0 .. txsPerEb - 1]]

-- | Fixed TX payload: 16 KiB zeros (realistic worst-case size).
fixedTxBytes :: BS.ByteString
fixedTxBytes = BS.replicate 16_384 0
