{-# LANGUAGE BangPatterns #-}
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
-- Every DB call is wrapped via 'LeiosDbWithCallTrace' and the resulting
-- 'SomeJsonCallTrace' events (start + end with duration\/allocation) are
-- written as JSON lines to @leios-db-bench-trace.jsonl@ in the working
-- directory.
--
-- The call trace forms a tree rooted at a single \"LeiosDBBench\" context
-- created in 'main'.  Each role (fetch-client-N, fetch-server-M, etc.) is a
-- child span of that root, and individual DB calls are grandchildren.
--
-- Usage:
--
-- @
-- cabal bench leios-db-bench --benchmark-options='+RTS -N4 -RTS'
-- @
module Main (main) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.Async (async, mapConcurrently_, wait)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Monad (forM, forM_, when)
import Control.Monad.Class.MonadTime.SI (diffTime, getMonotonicTime)
import Control.Tracer (debugTracer, (>$<))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Time.Clock (DiffTime)
import qualified Data.Vector.Strict as V
import LeiosDemoDb
  ( LeiosDbHandle (..)
  , newLeiosDBSQLite
  , withLeiosDb
  )
import LeiosDemoDb.WithCallTrace
  ( LeiosDbWithCallTrace (..)
  , newLeiosDbWithCallTrace
  )
import LeiosDemoTypes
  ( BytesSize
  , EbHash (..)
  , LeiosEb (..)
  , LeiosPoint (..)
  , TxHash (..)
  , leiosEbBytesSize
  )
import LeiosUtils.CallTrace
  ( CallCtx
  , CallTrace
  , SomeJsonCallTrace (..)
  , callTrace
  , callTraceSameThreadVia
  , callTraceToObject
  , rootCallCtx
  )
import System.IO (Handle, IOMode (..), hFlush, stdout, withFile)
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
      , "  Fetch clients   (×" <> show numFetchClients <> "): 20 insertEbPoint/insertEbBody/insertTxs each"
      , "  Fetch servers   (×" <> show numFetchServers <> "): 30 lookupEbBody + 10 batchRetrieveTxs each"
      , "  Chain-sel reader(×1): " <> show numChainSelReads <> " lookupEbClosure calls"
      , "  GC ticker       (×1): " <> show numGcTicks <> " garbageCollect calls"
      , ""
      , "Runs: 1 warmup + " <> show numRuns <> " timed"
      , ""
      , "Trace output: leios-db-bench-trace.jsonl"
      , ""
      ]
  withFile "leios-db-bench-trace.jsonl" WriteMode $ \traceFile -> do
    tracer <- newTracer traceFile
    rootCtx <- rootCallCtx "LeiosDBBench"
    withSystemTempDirectory "leios-db-bench" $ \tmpDir -> do
      env <- setupBenchEnv tmpDir tracer rootCtx
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
benchConcurrentAll :: BenchEnv -> IO ()
benchConcurrentAll BenchEnv{beDb = db, bePoints = points, beWriterIdx = writerIdxRef, beTracer = tracer, beRootCtx = rootCtx} = do
  startIdx <-
    atomicModifyIORef'
      writerIdxRef
      (\n -> (n + numFetchClients * ebsPerClient, n))
  cs <- async (chainSelReader db tracer rootCtx points)
  gc <- async (gcTicker db tracer rootCtx)
  clients <- forM (zip [0 ..] (clientRanges startIdx)) $ \(i, range) ->
    async (fetchClient db tracer rootCtx i range)
  mapConcurrently_ (fetchServer db tracer rootCtx points) [0 .. numFetchServers - 1]
  wait cs >> wait gc
  forM_ clients wait
 where
  ebsPerClient = 20
  clientRanges startIdx =
    [ [startIdx + i * ebsPerClient .. startIdx + (i + 1) * ebsPerClient - 1]
    | i <- [0 .. numFetchClients - 1]
    ]

-- | Mirrors a fetch client: inserts fresh EBs with full TX payloads.
fetchClient :: LeiosDbHandle IO -> Tracer -> CallCtx IO -> Int -> [Int] -> IO ()
fetchClient db tracer rootCtx clientIdx range =
  callTrace (jsonTracer tracer) rootCtx ("fetch-client-" <> show clientIdx) "fetch-client" () $ \roleCtx ->
    withLeiosDb db $ \c -> do
      let wct = newLeiosDbWithCallTrace tracer c
      forM_ range (insertOneEb wct roleCtx)

-- | Mirrors chain-selection's block-apply path: repeated
-- 'leiosDbLookupEbClosure' for the tx closure of each certified EB.
chainSelReader :: LeiosDbHandle IO -> Tracer -> CallCtx IO -> [LeiosPoint] -> IO ()
chainSelReader db tracer rootCtx points =
  callTrace (jsonTracer tracer) rootCtx "chain-sel-reader" "chain-sel-reader" () $ \roleCtx ->
    withLeiosDb db $ \c -> do
      let wct = newLeiosDbWithCallTrace tracer c
      forM_ (take numChainSelReads (cycle points)) $ \p ->
        wct.leiosDbLookupEbClosure roleCtx p.pointEbHash

-- | Fires periodic garbage-collect calls. Handle-level operation; touches
-- every table when implemented (currently a no-op backend-side, but the
-- call path is realistic).
gcTicker :: LeiosDbHandle IO -> Tracer -> CallCtx IO -> IO ()
gcTicker db tracer rootCtx =
  callTrace (jsonTracer tracer) rootCtx "gc-ticker" "gc-ticker" () $ \_ ->
    forM_ [1 .. numGcTicks] $ \i ->
      leiosDbGarbageCollect db (SlotNo (fromIntegral (i * 10)))

-- | Mirrors a fetch server: looks up EB bodies and retrieves TX batches.
fetchServer :: LeiosDbHandle IO -> Tracer -> CallCtx IO -> [LeiosPoint] -> Int -> IO ()
fetchServer db tracer rootCtx points i =
  callTrace (jsonTracer tracer) rootCtx ("fetch-server-" <> show i) "fetch-server" () $ \roleCtx ->
    withLeiosDb db $ \c -> do
      let wct = newLeiosDbWithCallTrace tracer c
      forM_ ebPoints $ \p -> wct.leiosDbLookupEbBody roleCtx p.pointEbHash
      forM_ txPoints $ \p -> wct.leiosDbBatchRetrieveTxs roleCtx p.pointEbHash sampleOffsets
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
  , beTracer :: !Tracer
  -- ^ Serialised JSON-line sink for call trace events.
  , beRootCtx :: !(CallCtx IO)
  -- ^ Single root context for the entire benchmark run.
  }

-- | Create a fresh SQLite DB and insert 'numPrePopulatedEbs' complete EBs.
-- This setup cost is not included in the timed measurements.
setupBenchEnv :: FilePath -> Tracer -> CallCtx IO -> IO BenchEnv
setupBenchEnv tmpDir tracer rootCtx = do
  db <- newLeiosDBSQLite (show >$< debugTracer) (tmpDir <> "/bench.db")
  putStr "Inserting EBs: " >> hFlush stdout
  callTraceSameThreadVia (const ()) (jsonTracer tracer) rootCtx "setup" () $ \setupCtx -> do
    forM_ [0 .. numPrePopulatedEbs - 1] $ \i -> do
      withLeiosDb db $ \c -> do
        let wct = newLeiosDbWithCallTrace tracer c
        insertOneEb wct setupCtx i
      when (i `mod` (numPrePopulatedEbs `div` 10) == numPrePopulatedEbs `div` 10 - 1) $
        putStr (show (i + 1) <> " ") >> hFlush stdout
    putStrLn "done"
    let points = [genPoint i | i <- [0 .. numPrePopulatedEbs - 1]]
    writerIdx <- newIORef numPrePopulatedEbs
    pure $ BenchEnv db points writerIdx tracer rootCtx

-- * Tracer

-- | Thread-safe sink that serialises 'SomeJsonCallTrace' events as JSON lines.
type Tracer = SomeJsonCallTrace -> IO ()

newTracer :: Handle -> IO Tracer
newTracer h = do
  lock <- newMVar ()
  pure $ \(SomeJsonCallTrace ct) ->
    withMVar lock $ \() ->
      BSL.hPut h (Aeson.encode (Aeson.Object (callTraceToObject ct)) <> BSL.singleton 0x0a)

-- | Adapt a 'Tracer' into the form expected by 'callTrace' \/ 'callTraceSameThreadVia'.
jsonTracer :: (Aeson.ToJSON a, Aeson.ToJSON r) => Tracer -> CallTrace a r -> IO ()
jsonTracer tracer ct = tracer (SomeJsonCallTrace ct)

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
insertOneEb :: LeiosDbWithCallTrace IO -> CallCtx IO -> Int -> IO ()
insertOneEb wct ctx ebIdx = do
  let point = genPoint ebIdx
      eb = genEb ebIdx
      txs =
        [ (h, genTx h)
        | txIdx <- [0 .. txsPerEb - 1]
        , let h = genTxHash ebIdx txIdx
        ]
  wct.leiosDbInsertEbPoint ctx point (leiosEbBytesSize eb)
  _ <- wct.leiosDbInsertEbBody ctx point eb
  _ <- wct.leiosDbInsertTxs ctx txs
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
