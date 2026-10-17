{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Concurrent benchmark for "Ouroboros.Consensus.Storage.LeiosDB" mirroring production access patterns.
--
-- The following roles run concurrently against the same SQLite handle:
--
-- * __Fetch clients__: each inserts fresh EBs via 'writeEbPoint' →
--   'writeEbBody' → 'writeTxs', the txs in batches as they arrive.
--
-- * __Fetch servers__: each looks up EB bodies and retrieves all their txs,
--   from the EBs that survive this iteration's GC.
--
-- * __Chain-sel reader__ (1 thread): mimics the block-apply path via
--   'lookupEbClosure' — the same read that 'resolveLeiosClosure'
--   issues per Dijkstra-era CertRB.
--
-- * __GC ticker__ (1 thread): 'leiosDbGarbageCollect' calls advancing the
--   frontier by as many EBs as the iteration inserts, so the volatile
--   partition stays at 'numVolatileEbs' EBs. The writer sweeps the marked EBs
--   between the inserts.
--
-- All data is deterministic (no QuickCheck generators), so runs are stable and
-- comparable across refactors. It is generated, and the readers opened, before
-- the timer starts.
--
-- Usage:
--
-- @
-- cabal bench leios-db-bench --benchmark-options='+RTS -N2 -RTS'
-- @
module Main (main) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, mapConcurrently_, wait)
import Control.Exception (evaluate)
import Control.Monad (forM, forM_, join, replicateM, void, when)
import Control.Monad.Class.MonadTime.SI (diffTime, getMonotonicTime)
import Control.ResourceRegistry (ResourceRegistry, withRegistry)
import Control.Tracer (nullTracer)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Time.Clock (DiffTime)
import qualified Data.Vector.Strict as V
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EbHash (..)
  , LeiosEb (..)
  , LeiosPoint (..)
  , TxHash (..)
  , TxOffset
  , encodeLeiosEbSize
  )
import Ouroboros.Consensus.Storage.LeiosDB
  ( LeiosDbHandle (..)
  , LeiosDbReader
  , LeiosDbStats (..)
  , LeiosDbWriter (..)
  , allocateReader
  , awaitAll
  , batchRetrieveTxs
  , leiosDbGarbageCollect
  , lookupEbBody
  , lookupEbClosure
  , newLeiosDBSQLite
  , withWriter
  )
import System.Directory (getFileSize)
import System.IO (hFlush, stdout)
import System.IO.Temp (withSystemTempDirectory)

main :: IO ()
main = do
  putStr $
    unlines
      [ "LeiosDB concurrent benchmark"
      , ""
      , "Database setup:"
      , "  Volatile EBs      : " <> show numVolatileEbs
      , "  TXs per EB        : " <> show txsPerEb
      , "  TX size           : " <> show txBytesSize <> " bytes"
      , "  Total TXs         : " <> show (numVolatileEbs * txsPerEb)
      , ""
      , "Concurrent workload per iteration:"
      , "  Fetch clients   (×"
          <> show numFetchClients
          <> "): "
          <> show ebsPerClient
          <> " full EB insertions each, "
          <> show txsPerBatch
          <> " TXs per writeTxs"
      , "  Fetch servers   (×"
          <> show numFetchServers
          <> "): "
          <> show ebsPerServer
          <> " lookups and full retrievals each"
      , "  Chain-sel reader(×1): " <> show numChainSelReads <> " lookupEbClosure calls"
      , "  GC ticker       (×1): "
          <> show numGcTicks
          <> " garbageCollect calls, evicting "
          <> show ebsPerIteration
          <> " EBs"
      , ""
      , "Runs: 1 warmup + " <> show numRuns <> " timed"
      , ""
      ]
  when (numVolatileEbs <= ebsPerIteration) $
    error "numVolatileEbs must exceed the EBs inserted per iteration"
  withSystemTempDirectory "leios-db-bench" $ \tmpDir -> withRegistry $ \registry -> do
    env <- setupBenchEnv registry tmpDir
    runBench (prepareIteration env) (statsReport (beDb env) (volDbPath tmpDir))

-- * Configuration

-- | How many EBs the volatile partition holds: setup inserts this many (not
-- timed), and GC keeps it at this size while the iterations insert more. Tunes
-- how much of a burst of EBs the database holds at once.
numVolatileEbs :: Int
numVolatileEbs = 100

-- | TXs per EB (mid-range; Leios spec allows up to 16000).
txsPerEb :: Int
txsPerEb = 5000

-- | Size of every generated TX payload (roughly mainnet median).
txBytesSize :: Int
txBytesSize = 1200

-- | TXs per 'writeTxs', as if each batch arrived in its own fetch response.
txsPerBatch :: Int
txsPerBatch = 500

-- | Number of fetch client threads (writers that insert fresh EBs).
numFetchClients :: Int
numFetchClients = 10

-- | How many EBs each fetch client inserts per iteration.
ebsPerClient :: Int
ebsPerClient = 3

-- | Number of fetch server threads (readers serving downstream peers).
numFetchServers :: Int
numFetchServers = 100

-- | How many EBs each fetch server serves per iteration.
ebsPerServer :: Int
ebsPerServer = 3

-- | Number of chain-sel-shaped reader calls per iteration.
numChainSelReads :: Int
numChainSelReads = 50

-- | Number of GC ticks per iteration.
numGcTicks :: Int
numGcTicks = 3

-- | Timed repetitions (plus one warmup).
numRuns :: Int
numRuns = 5

-- | How many EBs one iteration inserts, and so how many its GC evicts.
ebsPerIteration :: Int
ebsPerIteration = numFetchClients * ebsPerClient

-- * The benchmark

-- | Generate one iteration's EBs (not timed) and return the iteration: all
-- production roles running concurrently against one DB handle.
prepareIteration :: BenchEnv -> IO (IO ())
prepareIteration env = do
  awaitVolatileEbs beDb
  startIdx <- atomicModifyIORef' beNextIdx (\n -> (n + ebsPerIteration, n))
  clientEbs <-
    forM [0 .. numFetchClients - 1] $ \c ->
      forM [startIdx + c * ebsPerClient .. startIdx + (c + 1) * ebsPerClient - 1] genFullEb
  -- The volatile EBs are @[startIdx - numVolatileEbs .. startIdx - 1]@; GC
  -- evicts the oldest 'ebsPerIteration' of them, the readers read the rest.
  let oldest = startIdx - numVolatileEbs
      surviving = [genPoint i | i <- [oldest + ebsPerIteration .. startIdx - 1]]
  pure $ do
    cs <- async (chainSelReader beChainSelReader surviving)
    gc <- async (gcTicker beDb oldest)
    clients <- forM clientEbs $ \ebs -> async (fetchClient beDb ebs)
    mapConcurrently_
      (\(i, r) -> fetchServer r surviving i)
      (zip [0 ..] beServerReaders)
    wait cs >> wait gc
    forM_ clients wait
 where
  BenchEnv{beDb, beServerReaders, beChainSelReader, beNextIdx} = env

-- | Wait for the sweep to bring the volatile partition back to
-- 'numVolatileEbs' EBs, so that every iteration starts from the same size.
awaitVolatileEbs :: LeiosDbHandle IO -> IO ()
awaitVolatileEbs db = go (0 :: Int)
 where
  go waited = do
    LeiosDbStats{volatileEbs} <- leiosDbSampleStats db
    if
      | volatileEbs <= numVolatileEbs -> pure ()
      | waited >= 30_000 ->
          error $ "the volatile partition stays at " <> show volatileEbs <> " EBs"
      | otherwise -> threadDelay 10_000 >> go (waited + 10)

-- | The volatile EBs and WAL size, after a run: what the sweep and the
-- checkpoints have not caught up with yet. 'leiosDbSampleStats' leaves the WAL
-- size to its periodic trace, so read the file.
statsReport :: LeiosDbHandle IO -> FilePath -> IO String
statsReport db volPath = do
  LeiosDbStats{volatileEbs} <- leiosDbSampleStats db
  walBytes <- getFileSize (volPath <> "-wal")
  pure $
    "volatile EBs: "
      <> show volatileEbs
      <> ", WAL: "
      <> show (walBytes `div` 1_000_000)
      <> " MB"

-- | Mirrors a fetch client: inserts fresh EBs with full TX payloads.
fetchClient :: LeiosDbHandle IO -> [FullEb] -> IO ()
fetchClient db ebs =
  withWriter db $ \w ->
    forM_ ebs (insertFullEb w)

-- | Mirrors chain-selection's block-apply path: repeated
-- 'lookupEbClosure' for the tx closure of each certified EB.
chainSelReader :: LeiosDbReader IO -> [LeiosPoint] -> IO ()
chainSelReader r points =
  forM_ (take numChainSelReads (cycle points)) $ \p ->
    lookupEbClosure r (pointEbHash p)

-- | Advances the GC frontier in 'numGcTicks' steps from the oldest volatile EB
-- past the 'ebsPerIteration' oldest ones. EB slots are their indices.
gcTicker :: LeiosDbHandle IO -> Int -> IO ()
gcTicker db oldest =
  forM_ [1 .. numGcTicks] $ \i ->
    leiosDbGarbageCollect db $
      SlotNo (fromIntegral (oldest + ebsPerIteration * i `div` numGcTicks))

-- | Mirrors a fetch server: looks up EB bodies and retrieves all their TXs.
fetchServer :: LeiosDbReader IO -> [LeiosPoint] -> Int -> IO ()
fetchServer r points i =
  forM_ (take ebsPerServer $ drop i $ cycle points) $ \p -> do
    void $ lookupEbBody r (pointEbHash p)
    void $ batchRetrieveTxs r (pointEbHash p) allOffsets
 where
  allOffsets = [0 .. txsPerEb - 1]

-- * Benchmark environment

data BenchEnv = BenchEnv
  { beDb :: !(LeiosDbHandle IO)
  , beServerReaders :: ![LeiosDbReader IO]
  -- ^ One per fetch server, open for the whole benchmark.
  , beChainSelReader :: !(LeiosDbReader IO)
  , beNextIdx :: !(IORef Int)
  -- ^ Index of the next EB to insert; EB indices are also their slots.
  }

-- | Create a fresh SQLite DB, insert 'numVolatileEbs' complete EBs and open
-- the readers. This setup cost is not included in the timed measurements.
setupBenchEnv :: ResourceRegistry IO -> FilePath -> IO BenchEnv
setupBenchEnv registry tmpDir = do
  db <-
    newLeiosDBSQLite
      registry
      nullTracer
      (volDbPath tmpDir)
      (tmpDir <> "/bench.imm.db")
  putStr "Inserting EBs: " >> hFlush stdout
  forM_ [0 .. numVolatileEbs - 1] $ \i -> do
    withWriter db . flip insertFullEb =<< genFullEb i
    when (i `mod` progressStep == progressStep - 1) $
      putStr (show (i + 1) <> " ") >> hFlush stdout
  putStrLn "done"
  serverReaders <- replicateM numFetchServers (allocateReader registry db)
  csReader <- allocateReader registry db
  nextIdx <- newIORef numVolatileEbs
  pure $ BenchEnv db serverReaders csReader nextIdx
 where
  progressStep = max 1 (numVolatileEbs `div` 10)

-- | The volatile partition's file in the benchmark's directory.
volDbPath :: FilePath -> FilePath
volDbPath tmpDir = tmpDir <> "/bench.vol.db"

-- * Timing

-- | Warm up once, then time 'numRuns' repetitions, printing each result and a
-- final min\/avg\/max summary. Only the returned action of each preparation is
-- timed; the report is printed after each run.
runBench :: IO (IO ()) -> IO String -> IO ()
runBench prepare report = do
  join prepare -- warmup (not printed)
  times <- forM [1 .. numRuns] $ \i -> do
    action <- prepare
    t <- snd <$> timed action
    r <- report
    putStrLn $ "  run " <> show i <> "/" <> show numRuns <> ": " <> showTime t <> " (" <> r <> ")"
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

-- | One complete EB, generated ahead of its insertion.
data FullEb = FullEb
  { fePoint :: !LeiosPoint
  , feEb :: !LeiosEb
  , feTxBatches :: ![[(TxOffset, BS.ByteString)]]
  -- ^ The EB's TXs in 'writeTxs' batches.
  }

-- | Generate the EB of the given index, fully evaluated.
genFullEb :: Int -> IO FullEb
genFullEb ebIdx = do
  _ <- evaluate $ V.foldl' (\n (MkTxHash h, _) -> n + BS.length h) 0 (leiosEbTxs eb)
  _ <- evaluate $ sum [BS.length tx | batch <- batches, (_, tx) <- batch]
  pure FullEb{fePoint = genPoint ebIdx, feEb = eb, feTxBatches = batches}
 where
  eb = genEb ebIdx
  batches =
    chunksOf
      txsPerBatch
      [(txIdx, genTx (genTxHash ebIdx txIdx)) | txIdx <- [0 .. txsPerEb - 1]]
  chunksOf n xs = case splitAt n xs of
    (chunk, []) -> [chunk]
    (chunk, rest) -> chunk : chunksOf n rest

-- | Insert one complete EB: its point, its body, then its TXs batch by batch.
insertFullEb :: Monad m => LeiosDbWriter m -> FullEb -> m ()
insertFullEb writer FullEb{fePoint, feEb, feTxBatches} = do
  pointWritten <- writeEbPoint writer fePoint (encodeLeiosEbSize feEb)
  bodyWritten <- writeEbBody writer fePoint feEb
  txsWritten <- forM feTxBatches (writeTxs writer fePoint)
  awaitAll (pointWritten : void bodyWritten : map void txsWritten)

-- * Deterministic data generation

-- | 'LeiosPoint' from an index (SlotNo = index).
genPoint :: Int -> LeiosPoint
genPoint i = MkLeiosPoint (SlotNo $ fromIntegral i) (genEbHash i)

-- | 'EbHash' from an index: \"ebHash:<index>\" padded to 32 bytes with zeros.
genEbHash :: Int -> EbHash
genEbHash i = MkEbHash $ BS.take 32 (tag <> BS.replicate 32 0)
 where
  tag = BS8.pack ("ebHash:" <> show i)

-- | 'LeiosEb' with 'txsPerEb' transactions of 'txBytesSize' each.
--
-- The declared size must be the payload size of 'genTx': a tx write of any
-- other size is dropped.
genEb :: Int -> LeiosEb
genEb ebIdx =
  MkLeiosEb $
    V.fromList
      [ (genTxHash ebIdx txIdx, fromIntegral txBytesSize :: BytesSize)
      | txIdx <- [0 .. txsPerEb - 1]
      ]

-- | 'TxHash' from an EB index + TX offset: \"txHash:<ebIdx>:<txIdx>\" padded
-- to 32 bytes with zeros.
--
-- NOTE: This is taking an EB index as it always generates the worst case of
-- fully disjunct transaction closures between EBs.
genTxHash :: Int -> Int -> TxHash
genTxHash ebIdx txIdx = MkTxHash $ BS.take 32 (tag <> BS.replicate 32 0)
 where
  tag = BS8.pack ("txHash:" <> show ebIdx <> ":" <> show txIdx)

-- | Generate a TX payload: the TX hash bytes padded with zeros to
-- 'txBytesSize'.
genTx :: TxHash -> BS.ByteString
genTx (MkTxHash h) = h <> BS.replicate (txBytesSize - BS.length h) 0
