{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Latency benchmark for "Ouroboros.Consensus.Storage.LeiosDB": the storage
-- steps of the Leios workflow, optionally under a background load.
--
-- Every EB goes through:
--
-- * __Fetch__: 'writeEbPoint' and 'writeEbBody', then its Txs in 'writeTxs'
--   chunks of contiguous offsets (at most 'requestBytes', in whole 'jobBytes'
--   jobs), in random order and spread over 'numUpstreamPeers'. The EB is
--   complete once every chunk is written: the fetch logic knows that, not the
--   storage.
--
-- * __Vote read__: once fetched, one 'lookupEbClosure'.
--
-- * __Serve__: once fetched, every downstream peer reads the body with
--   'lookupEbBody', then the Txs in 'batchRetrieveTxs' chunks like the fetched
--   ones, in its own random order.
--
-- * __Apply read__: once served, one 'lookupEbClosure'.
--
-- The 'numSamples' measured EBs go one at a time: each is fetched once the
-- previous one is fetched, served and read. That is the honest schedule (an EB
-- about every 20 seconds) without the idle time.
--
-- The background load, if on, are bursts of 'burstSize' EBs every
-- 'burstInterval', started whether or not earlier ones are done. They are
-- fetched, voted on and served, never certified, and push the measured
-- latencies where they overlap.
--
-- GC keeps the volatile partition at 'numVolatileEbs' EBs. No notifications,
-- mempool fills or immutable partition: those are outside the storage, or off
-- the critical path. All data is deterministic and generated before it is
-- fetched.
--
-- Usage:
--
-- @
-- cabal bench LeiosDB-bench
-- @
--
-- The background load is off by default; @BURST=1@ turns it on.
--
-- The database lives under @$TMPDIR@: on a tmpfs to compare storage changes, on
-- a disk for realistic numbers.
module Main (main) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (async, cancel, wait)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.QSemN (newQSemN, signalQSemN, waitQSemN)
import Control.Exception (evaluate)
import Control.Monad (forM, forM_, forever, replicateM, void, when)
import Control.Monad.Class.MonadTime.SI (Time, addTime, diffTime, getMonotonicTime)
import Control.Monad.Class.MonadTimer.SI (threadDelay)
import Control.ResourceRegistry (ResourceRegistry, withRegistry)
import Control.Tracer (nullTracer)
import Data.Bits (shiftR, xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Functor ((<&>))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (DiffTime)
import qualified Data.Vector.Strict as V
import GHC.RTS.Flags (GCFlags (..), getGCFlags)
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
  , LeiosDbReader (..)
  , LeiosDbWriter (..)
  , Promise (..)
  , allocateReader
  , awaitAll
  , newLeiosDBSQLite
  , withWriter
  )
import System.Environment (lookupEnv)
import System.IO (hFlush, stdout)
import System.IO.Temp (withSystemTempDirectory)

main :: IO ()
main = do
  mLoad <- readLoad
  caps <- getNumCapabilities
  gcFlags <- getGCFlags
  withSystemTempDirectory "leios-db-bench" $ \tmpDir -> withRegistry $ \registry -> do
    putStr $
      unlines
        [ "LeiosDB latency benchmark"
        , ""
        , "  Database          : "
            <> show numVolatileEbs
            <> " volatile EBs"
        , "  EB                : "
            <> show txsPerEb
            <> " Txs of "
            <> showBytes txBytesSize
            <> " (body "
            <> showBytes (fromIntegral (encodeLeiosEbSize (genEb 0)))
            <> ", closure "
            <> showBytes (txsPerEb * txBytesSize)
            <> ")"
        , "  Requests          : "
            <> showBytes requestBytes
            <> " of "
            <> showKiB jobBytes
            <> " jobs"
        , "  Schedule          : " <> show numSamples <> " EBs, one at a time"
        , "  Fetch             : from " <> show numUpstreamPeers <> " upstream peers"
        , "  Serve             : to " <> show numDownstreamPeers <> " downstream peers"
        , case mLoad of
            Just Load{burstSize, burstInterval} ->
              "  Background load   : "
                <> show burstSize
                <> " EBs every "
                <> show burstInterval
                <> " ("
                <> showBytes (burstSize * txsPerEb * txBytesSize)
                <> "; unset BURST to turn off)"
            Nothing -> "  Background load   : None (BURST=1 to turn on)"
        , "  RTS               : -N"
            <> show caps
            <> ", -A"
            <> showKiB (fromIntegral (minAllocAreaSize gcFlags) * 4096)
        , ""
        ]
    env <- setupBenchEnv registry tmpDir
    runBench mLoad env

-- * Configuration

-- | How many EBs the volatile partition holds: setup inserts this many, and GC
-- keeps it at this size while EBs are fetched. Tunes how much of a burst of EBs the
-- database holds at once.
numVolatileEbs :: Int
numVolatileEbs = 1000

-- | Txs per EB: a body of about 100 KB.
txsPerEb :: Int
txsPerEb = 2857

-- | Size of every generated TX payload: a closure of about 1 MB.
txBytesSize :: Int
txBytesSize = 350

-- | The background load: bursts of EBs whose fetches start at once.
data Load = Load
  { burstSize :: !Int
  -- ^ How many EBs start being fetched at once.
  , burstInterval :: !DiffTime
  -- ^ Time between bursts.
  }

-- | With @BURST=1@, bursts of 10 EBs every 5 seconds; none otherwise.
readLoad :: IO (Maybe Load)
readLoad =
  lookupEnv "BURST" <&> \case
    Just "1" -> Just Load{burstSize = 10, burstInterval = 5}
    _ -> Nothing

-- | How many EBs are measured.
numSamples :: Int
numSamples = 100

-- | Every this many fetched EBs, GC marks what lies beyond 'numVolatileEbs'.
gcEvery :: Int
gcEvery = 10

-- | Upper bound on the TX bytes of one fetch request and its response; the
-- prototype's @maxLeiosTxsRequestBytesSize@.
requestBytes :: Int
requestBytes = 500_000

-- | TX bytes per job, the unit a request is made of; the prototype's
-- @maxJobBytesSize@.
jobBytes :: Int
jobBytes = 65_536

-- | Peers the TX chunks of every EB are fetched from.
numUpstreamPeers :: Int
numUpstreamPeers = 20

-- | Peers each fetching every EB once it is complete.
numDownstreamPeers :: Int
numDownstreamPeers = 100

-- * The benchmark

-- | One EB, generated ahead of its fetch. Its slot is the order of its fetch,
-- see 'fetchEb'.
data BenchEb = BenchEb
  { beIdx :: !Int
  , beHash :: !EbHash
  , beEb :: !LeiosEb
  , beBatches :: ![[(TxOffset, BS.ByteString)]]
  -- ^ The Txs, as the fetch responses deliver them.
  , beMeasured :: !Bool
  }

-- | What a step measures.
data Metric
  = FetchTotal
  | FetchBody
  | FetchTxs
  | ServeTotal
  | ServeBody
  | ServeTxs
  | VoteRead
  | ApplyRead
  deriving (Eq, Ord, Show, Enum, Bounded)

type Record = Metric -> DiffTime -> IO ()

runBench :: Maybe Load -> BenchEnv -> IO ()
runBench mLoad BenchEnv{beDb, beVoter, bePeers, beApplier} = do
  putStr "Generating EBs: " >> hFlush stdout
  signal <-
    forM [0 .. numSamples - 1] $ \i -> genBenchEb (signalIdx i) True
  putStrLn "done"
  samplesVar <- newIORef Map.empty
  missingVar <- newIORef (0 :: Int)
  let record metric t = atomicModifyIORef' samplesVar $ \m -> (Map.insertWith (<>) metric [t] m, ())
      missing = atomicModifyIORef' missingVar $ \n -> (n + 1, ())
  slotVar <- newIORef numVolatileEbs
  voteQ <- newChan
  upstreamQs <- replicateM numUpstreamPeers newChan
  peerQs <- replicateM numDownstreamPeers newChan
  upstreams <- forM upstreamQs $ \q ->
    async $ withWriter beDb $ \w -> drainWith q $ \(point, batch, done) -> do
      writeChunk w point batch
      putMVar done ()
  voter <- async $ drainWith voteQ $ \(eb, _, onDone) ->
    closureRead beVoter eb VoteRead record missing >> onDone
  peers <- forM (zip3 [1 ..] bePeers peerQs) $ \(seed, r, q) ->
    async $ drainWith q $ \(eb, completed, onDone) ->
      servePeer seed r eb completed record missing >> onDone
  let
    -- Fetch an EB as the fetch logic does, in the next slot: its point and
    -- body, then its Tx chunks from the upstream peers. Then hand it to the
    -- voter and the downstream peers, each calling the given action when done.
    fetchEb eb onDone = do
      slot <- atomicModifyIORef' slotVar $ \n -> (n + 1, n)
      when (slot `mod` gcEvery == 0) $
        leiosDbGarbageCollect beDb (SlotNo (fromIntegral (slot + 1 - numVolatileEbs)))
      let point = MkLeiosPoint (SlotNo (fromIntegral slot)) (beHash eb)
      started <- getMonotonicTime
      withWriter beDb $ \w -> writeBody w point eb record
      bodyDone <- getMonotonicTime
      dones <- forM (zip [slot ..] (beBatches eb)) $ \(k, batch) -> do
        done <- newEmptyMVar
        writeChan (upstreamQs !! (k `mod` numUpstreamPeers)) (Just (point, batch, done))
        pure done
      mapM_ takeMVar dones
      completed <- getMonotonicTime
      when (beMeasured eb) $ do
        record FetchTotal (diffTime completed started)
        record FetchTxs (diffTime completed bodyDone)
      forM_ (voteQ : peerQs) $ \q -> writeChan q (Just (eb, completed, onDone))
  fetchedVar <- newIORef (0 :: Int)
  stopVar <- newIORef False
  t0 <- getMonotonicTime
  progress <- async $ forever $ do
    now <- getMonotonicTime
    fetched <- readIORef fetchedVar
    stopped <- readIORef stopVar
    putStr $
      "\rRunning: "
        <> showTime (diffTime now t0)
        <> ", "
        <> show fetched
        <> "/"
        <> show (length signal)
        <> " EBs done"
        <> (if stopped then ", draining   " else "   ")
    hFlush stdout
    threadDelay 1
  bursts <- forM mLoad $ \load -> async $ burstLoad load (`fetchEb` pure ()) stopVar t0
  forM_ signal $ \eb -> do
    readers <- newQSemN 0
    fetchEb eb (signalQSemN readers 1)
    waitQSemN readers (1 + numDownstreamPeers)
    closureRead beApplier eb ApplyRead record missing
    atomicModifyIORef' fetchedVar $ \n -> (n + 1, ())
  atomicModifyIORef' stopVar $ \_ -> (True, ())
  mapM_ wait bursts
  forM_ upstreamQs $ \q -> writeChan q Nothing
  forM_ (voteQ : peerQs) $ \q -> writeChan q Nothing
  mapM_ wait upstreams >> wait voter >> mapM_ wait peers
  cancel progress
  putStrLn ""
  report samplesVar missingVar
 where
  signalIdx i = 1_000_000 + i

-- | Start a burst of EBs every 'burstInterval' from the given time, whether or
-- not earlier ones are done, until stopped; then wait for those in flight.
burstLoad :: Load -> (BenchEb -> IO ()) -> IORef Bool -> Time -> IO ()
burstLoad Load{burstSize, burstInterval} fetchEb stopVar t0 = go 0 []
 where
  go b inFlight = do
    ebs <- forM [0 .. burstSize - 1] $ \i -> genBenchEb (2_000_000 + b * burstSize + i) False
    sleepUntil (addTime (fromIntegral b * burstInterval) t0)
    stopped <- readIORef stopVar
    if stopped
      then mapM_ wait inFlight
      else do
        started <- forM ebs (async . fetchEb)
        go (b + 1) (started <> inFlight)

-- | Handle the queue's items one at a time, until 'Nothing'.
drainWith :: Chan (Maybe a) -> (a -> IO ()) -> IO ()
drainWith q k = loop
 where
  loop =
    readChan q >>= \case
      Nothing -> pure ()
      Just x -> k x >> loop

-- | Write an EB's point and body, as the fetch logic does on receiving it.
writeBody :: LeiosDbWriter IO -> LeiosPoint -> BenchEb -> Record -> IO ()
writeBody w point eb@BenchEb{beEb} record =
  timedStep eb FetchBody record $ do
    pointWritten <- writeEbPoint w point (encodeLeiosEbSize beEb)
    bodyWritten <- writeEbBody w point beEb
    awaitAll [pointWritten, void bodyWritten]

-- | Write a chunk of an EB's Txs, as the fetch logic does on receiving it.
writeChunk :: LeiosDbWriter IO -> LeiosPoint -> [(TxOffset, BS.ByteString)] -> IO ()
writeChunk w point batch = void . await =<< writeTxs w point batch

-- | Write an EB in full on one writer: its body, then its Tx chunks.
ingest :: LeiosDbWriter IO -> LeiosPoint -> BenchEb -> Record -> IO ()
ingest w point eb record = do
  writeBody w point eb record
  forM_ (beBatches eb) $ \batch -> writeChunk w point batch

timedStep :: BenchEb -> Metric -> Record -> IO () -> IO ()
timedStep BenchEb{beMeasured} metric record action = do
  t <- getMonotonicTime
  action
  t' <- getMonotonicTime
  when beMeasured $ record metric (diffTime t' t)

-- | Resolve a completed EB's closure, as the voter and block application do.
closureRead :: LeiosDbReader IO -> BenchEb -> Metric -> Record -> IO () -> IO ()
closureRead r BenchEb{beHash, beMeasured} metric record missing = do
  t <- getMonotonicTime
  mClosure <- lookupEbClosure r beHash
  t' <- getMonotonicTime
  case mClosure of
    Nothing -> missing
    Just _ -> when beMeasured $ record metric (diffTime t' t)

-- | A downstream peer fetching a completed EB in full, one request at a time:
-- its body, then its TX chunks in the order of its seed.
servePeer :: Int -> LeiosDbReader IO -> BenchEb -> Time -> Record -> IO () -> IO ()
servePeer seed r BenchEb{beIdx, beHash, beMeasured} completed record missing = do
  t <- getMonotonicTime
  body <- lookupEbBody r beHash
  t' <- getMonotonicTime
  if length body /= txsPerEb
    then missing
    else when beMeasured $ record ServeBody (diffTime t' t)
  forM_ (requests seed beIdx) $ \offsets -> do
    u <- getMonotonicTime
    txs <- batchRetrieveTxs r beHash offsets
    u' <- getMonotonicTime
    if length [() | (_, _, Just _) <- txs] /= length offsets
      then missing
      else when beMeasured $ record ServeTxs (diffTime u' u)
  done <- getMonotonicTime
  when beMeasured $ record ServeTotal (diffTime done completed)

-- * Requests

-- | The EB's TX offsets as requests of contiguous offsets, in the order of the
-- given seed.
requests :: Int -> Int -> [[TxOffset]]
requests seed ebIdx =
  map request $ sortOn (mix seed ebIdx) [0 .. numRequests - 1]
 where
  txsPerJob = max 1 (jobBytes `div` txBytesSize)
  txsPerRequest = txsPerJob * max 1 (requestBytes `div` (txsPerJob * txBytesSize))
  numRequests = (txsPerEb + txsPerRequest - 1) `div` txsPerRequest
  request i = [i * txsPerRequest .. min txsPerEb ((i + 1) * txsPerRequest) - 1]

-- | A deterministic shuffle key.
mix :: Int -> Int -> Int -> Int
mix seed ebIdx j = h `xor` (h `shiftR` 29)
 where
  h = (j * 0x9E3779B1 + ebIdx * 0x85EBCA77 + seed * 0xC2B2AE3D) * 0x27D4EB2F

-- * Benchmark environment

data BenchEnv = BenchEnv
  { beDb :: !(LeiosDbHandle IO)
  , beVoter :: !(LeiosDbReader IO)
  , bePeers :: ![LeiosDbReader IO]
  , beApplier :: !(LeiosDbReader IO)
  }

-- | Create a fresh SQLite DB, insert 'numVolatileEbs' complete EBs and open
-- the readers. Not timed.
setupBenchEnv :: ResourceRegistry IO -> FilePath -> IO BenchEnv
setupBenchEnv registry tmpDir = do
  db <-
    newLeiosDBSQLite
      registry
      nullTracer
      (tmpDir <> "/bench.vol.db")
      (tmpDir <> "/bench.imm.db")
  putStrLn $ "Database in " <> tmpDir <> " (TMPDIR to change)"
  putStr "Inserting EBs: " >> hFlush stdout
  forM_ [0 .. numVolatileEbs - 1] $ \i -> do
    eb <- genBenchEb i False
    withWriter db $ \w ->
      ingest w (MkLeiosPoint (SlotNo (fromIntegral i)) (beHash eb)) eb (\_ _ -> pure ())
    when (i `mod` progressStep == progressStep - 1) $
      putStr (show (i + 1) <> " ") >> hFlush stdout
  putStrLn "done"
  BenchEnv db
    <$> allocateReader registry db
    <*> replicateM numDownstreamPeers (allocateReader registry db)
    <*> allocateReader registry db
 where
  progressStep = max 1 (numVolatileEbs `div` 10)

-- * Reporting

report :: IORef (Map.Map Metric [DiffTime]) -> IORef Int -> IO ()
report samplesVar missingVar = do
  samples <- readIORef samplesVar
  putStrLn ""
  putStrLn $ padRight 18 "Latency" <> concatMap (padLeft 11) ["p50", "p99", "Max", "Count"]
  forM_ [minBound .. maxBound] $ \metric -> do
    let ts = sort (Map.findWithDefault [] metric samples)
    putStrLn $
      padRight 18 (label metric)
        <> concatMap (padLeft 11) [percentile 0.5 ts, percentile 0.99 ts, percentile 1 ts]
        <> padLeft 11 (show (length ts))
  missingReads <- readIORef missingVar
  when (missingReads > 0) $
    putStrLn $
      "WARNING: " <> show missingReads <> " reads found their EB missing (collected too early?)"
 where
  label = \case
    FetchTotal -> "Fetch"
    FetchBody -> "  Body"
    FetchTxs -> "  Txs"
    ServeTotal -> "Serve"
    ServeBody -> "  Body"
    ServeTxs -> "  Txs chunk"
    VoteRead -> "Vote read"
    ApplyRead -> "Apply read"
  percentile :: Double -> [DiffTime] -> String
  percentile _ [] = "-"
  percentile q ts = showTime (ts !! min (n - 1) (max 0 (ceiling (q * fromIntegral n) - 1)))
   where
    n = length ts
  padLeft k s = replicate (k - length s) ' ' <> s
  padRight k s = s <> replicate (k - length s) ' '

showTime :: DiffTime -> String
showTime t
  | t < 1e-3 = show (round (s * 1_000_000 :: Double) :: Int) <> " μs"
  | t < 1 = show (round (s * 1_000 :: Double) :: Int) <> " ms"
  | otherwise = show (fromIntegral (round (s * 100 :: Double) :: Int) / 100 :: Double) <> " s"
 where
  s = realToFrac t :: Double

-- | Bytes in decimal units: B, KB, MB.
showBytes :: Int -> String
showBytes n
  | n >= 1_000_000 = scaled 1e6 <> " MB"
  | n >= 1_000 = scaled 1e3 <> " KB"
  | otherwise = show n <> " B"
 where
  scaled :: Double -> String
  scaled unit = case round (fromIntegral n / unit * 10) :: Int of
    tenths
      | tenths `mod` 10 == 0 -> show (tenths `div` 10)
      | otherwise -> show (fromIntegral tenths / 10 :: Double)

-- | Bytes in KiB.
showKiB :: Int -> String
showKiB n = show (n `div` 1024) <> " KiB"

sleepUntil :: Time -> IO ()
sleepUntil t = do
  now <- getMonotonicTime
  let d = diffTime t now
  when (d > 0) $ threadDelay d

-- * Deterministic data generation

-- | Generate the EB of the given index, fully evaluated.
genBenchEb :: Int -> Bool -> IO BenchEb
genBenchEb ebIdx measured = do
  _ <- evaluate $ V.foldl' (\n (MkTxHash h, _) -> n + BS.length h) 0 (leiosEbTxs eb)
  _ <- evaluate $ sum [BS.length tx | batch <- batches, (_, tx) <- batch]
  pure
    BenchEb
      { beIdx = ebIdx
      , beHash = genEbHash ebIdx
      , beEb = eb
      , beBatches = batches
      , beMeasured = measured
      }
 where
  eb = genEb ebIdx
  -- The upstreams' order is seed 0.
  batches = [[(off, genTx (genTxHash ebIdx off)) | off <- offs] | offs <- requests 0 ebIdx]

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
