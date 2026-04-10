{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Concurrent benchmark for 'LeiosDemoDb' mirroring production access patterns.
--
-- The following three roles run concurrently against the same SQLite handle:
--
-- * __Fetch logic__ (1 thread): 10 rounds of
--   'leiosDbFilterMissingEbBodies' + 'leiosDbFilterMissingTxs'.
--
-- * __Fetch clients__ (configurable, default 2 threads): each inserts 20 fresh
--   EBs via 'leiosDbInsertEbPoint' → 'leiosDbInsertEbBody' → 'leiosDbInsertTxs'.
--
-- * __Fetch servers__ (configurable, default 8 threads): each does 30
--   'leiosDbLookupEbBody' + 10 'leiosDbBatchRetrieveTxs' calls cycling through
--   the pre-populated EBs.
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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Time.Clock (DiffTime)
import qualified Data.Vector as V
import LeiosDemoDb
  ( LeiosDbConnection
  , LeiosDbHandle (..)
  , leiosDbBatchRetrieveTxs
  , leiosDbFilterMissingEbBodies
  , leiosDbFilterMissingTxs
  , leiosDbInsertEbBody
  , leiosDbInsertEbPoint
  , leiosDbInsertTxs
  , leiosDbLookupEbBody
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
      , "  Fetch logic   (×1): 10 rounds of filterMissingEbBodies (200+200) + filterMissingTxs (500+500)"
      , "  Fetch clients (×" <> show numFetchClients <> "): 20 insertEbPoint/insertEbBody/insertTxs each"
      , "  Fetch servers (×" <> show numFetchServers <> "): 30 lookupEbBody + 10 batchRetrieveTxs each"
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
numFetchClients = 2

-- | Number of fetch server threads (readers serving downstream peers).
numFetchServers :: Int
numFetchServers = 8

-- | Timed repetitions (plus one warmup).
numRuns :: Int
numRuns = 5

-- * The benchmark

-- | All three production roles running concurrently against one DB handle.
benchConcurrentAll :: BenchEnv -> IO ()
benchConcurrentAll BenchEnv{beDb = db, bePoints = points, beWriterIdx = writerIdxRef} = do
  startIdx <- atomicModifyIORef' writerIdxRef (\n -> (n + numFetchClients * ebsPerClient, n))
  fl <- async (fetchLogic db points)
  clients <- forM (clientRanges startIdx) $ \range -> async (fetchClient db range)
  mapConcurrently_ (fetchServer db points) [0 .. numFetchServers - 1]
  wait fl
  forM_ clients wait
 where
  ebsPerClient = 20
  clientRanges startIdx =
    [ [startIdx + i * ebsPerClient .. startIdx + (i + 1) * ebsPerClient - 1]
    | i <- [0 .. numFetchClients - 1]
    ]

-- | Mirrors the fetch logic loop: filters for missing EB bodies and TXs.
fetchLogic :: LeiosDbHandle IO -> [LeiosPoint] -> IO ()
fetchLogic db points =
  withLeiosDb db $ \c ->
    replicateM_ 10 $ do
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
  db <- newLeiosDBSQLite (tmpDir <> "/bench.db")
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
