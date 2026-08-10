{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Worst-case benchmark for the 'LeiosTxCache' handle: max residency,
-- allocation rate during population, and — chiefly — the latency of an EB-sized
-- batch of lookups via 'withLookupTx'.
--
-- It is written against the impure handle, so the same workload runs against
-- both the pure-wrapped index ('newPureLeiosTxCache') and a future mutable
-- implementation (add another 'runBench' call).
--
-- Worst-case hard bounds: an EB references up to ~512 kB \/ 34 B == 'maxTxsPerEb'
-- txs, and up to 'maxAnnouncementCount' EBs sit in the index at once with fully
-- disjoint tx closures — ~1.9M distinct txs resident.
--
-- Run (the stanza bakes in @-T@; add @-s@ for the RTS summary):
--
-- @cabal bench leios-txcache-bench@
module Main (main) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (foldM, forM, forM_, when)
import qualified Data.Bits as Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import Data.List (isPrefixOf, partition, sort, stripPrefix, transpose)
import qualified Data.Vector.Strict as V
import Data.Word (Word64)
import Foreign.C.Types (CInt (..))
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import LeiosDemoTypes (EbHash (..), RbHash (..), TxHash (..))
import LeiosTxCache
import LeiosTxCache.Bench.SQLite
  ( newSQLiteLeiosTxCacheForPopulation
  , newSQLiteLeiosTxCacheForQueries
  )
import Numeric (showFFloat)
import System.Directory (doesFileExist, removeFile)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (IOMode (ReadMode, ReadWriteMode), hFlush, openFile, stdout)
import System.Mem (performMajorGC)
import System.Posix.IO (closeFd, handleToFd)
import System.Posix.Types (COff (..))

-- * Configuration

numEbs :: Int
numEbs = maxAnnouncementCount

-- | Worst-case tx references in one EB: a ~512 kB body at 34 B/item (32-byte
-- hash + 2-byte size) in a compact, non-CBOR layout — the encoding-independent
-- ceiling. (The current CBOR-precise 'LeiosDemoTypes.maxTxsPerEb' is 13888.)
txsPerEb :: Int
txsPerEb = 15_058

-- | Timed repetitions of the batch lookup (plus one warmup).
numLookupRuns :: Int
numLookupRuns = 20

-- | A body carrying its tx hashes as packed bytes (mirroring the production
-- serialized body). Crucially it does NOT retain a boxed 'TxHash' per tx: were
-- the body a @Vector TxHash@ instead, both handles would hold the ~1.9M boxed
-- hashes and the residency comparison would be meaningless — the pure map's
-- boxed keys are exactly what the flat table replaces with inline bytes. The
-- fold copies each 32-byte chunk out, so the pure map's keys are separate
-- buffers (the realistic wire-derived case).
--
-- If the pure map were preferred for other reasons, then we could reconsider
-- whether the production should ust Vector TxHash instead of a flat
-- ByteString. One non-option: do not stor the body as a bytestring and have the
-- keys be bytestring slices of that big one, because then a key that is
-- preserved by younger bodies would retain the older body's foreign pointer
-- even after it was evicted. That could cause catastrophic space leaks, in
-- pathological cases.
newtype BenchBody = BenchBody ByteString -- concatenated 32-byte tx hashes

instance ReferencesTxsByHash BenchBody where
  foldTxReferences f z0 (BenchBody bs) = go z0 0
   where
    n = BS.length bs `div` 32
    go !acc i
      | i >= n = acc
      | otherwise =
        go
          (f acc (MkTxHash (BS.copy (BS.take 32 (BS.drop (i * 32) bs)))) dummySize)
          (i + 1)
    dummySize = 0

type BenchCache = LeiosTxCache IO () () BenchBody

-- * Main

main :: IO ()
main = do
  enabled <- getRTSStatsEnabled
  when (not enabled) $
    error "GHC RTS stats not enabled; run with +RTS -T (the stanza bakes it in)"
  putStr $
    unlines
      [ "LeiosTxCache worst-case benchmark"
      , "  EBs in index : " <> show numEbs
      , "  txs per EB   : " <> show txsPerEb
      , "  total txs    : " <> show (numEbs * txsPerEb)
      ]
  printPrivilegeReminder
  args <- getArgs
  -- CLI: an optional variant (pure | ht | sqlite; default all three) plus the
  -- SQLite-only knobs --cache=small|big and --cycle-connection=yes|no. The knobs
  -- error if a non-sqlite variant is named.
  let (flags, positionals) = partition ("--" `isPrefixOf`) args
      flagValue name = case [v | f <- flags, Just v <- [stripPrefix (name <> "=") f]] of
        [] -> Nothing
        (v : _) -> Just v
  forM_ flags $ \f ->
    when (not (any (`isPrefixOf` f) ["--cache=", "--cycle-connection="])) $
      die ("unknown flag: " <> f)
  cache <- case flagValue "--cache" of
    Nothing -> pure BigCache
    Just "small" -> pure SmallCache
    Just "big" -> pure BigCache
    Just v -> die ("--cache must be small|big, got: " <> v)
  cycleConn <- case flagValue "--cycle-connection" of
    Nothing -> pure CycleYes
    Just "yes" -> pure CycleYes
    Just "no" -> pure CycleNo
    Just v -> die ("--cycle-connection must be yes|no, got: " <> v)
  variant <- case positionals of
    [] -> pure Nothing
    [v] | v `elem` ["pure", "ht", "sqlite"] -> pure (Just v)
    _ -> die "expected at most one of: pure | ht | sqlite"
  when (not (null flags) && maybe False (/= "sqlite") variant) $
    die "--cache / --cycle-connection apply only to the sqlite variant"
  let salt0, salt1 :: Word64
      salt0 = 0xD1CED00DFEEDFACE
      salt1 = 0x0123456789ABCDEF
      -- In-memory variants use one handle for both phases and need no cooling.
      mkInMem nm mk = do
        h <- mk
        pure (BenchTarget nm h h (pure ()) (pure ()))
      -- SQLite populates through a fast, unsafe connection and reads through a
      -- separate, coolable one; the fsync between makes its pages evictable.
      -- --cache sizes the query connection's private cache; --cycle-connection
      -- reopens it per batch (cold private cache) — together with the OS-cache
      -- eviction that is what makes a batch genuinely cold.
      mkSqlite = do
        exists <- doesFileExist sqliteDbPath
        when exists $ removeFile sqliteDbPath
        popCache <- newSQLiteLeiosTxCacheForPopulation sqliteDbPath
        (queryCache, reopenQuery) <-
          newSQLiteLeiosTxCacheForQueries (cacheSizePragma cache) txsPerEb sqliteDbPath
        let coolBatch = case cycleConn of
              CycleYes -> reopenQuery >> coolFile sqliteDbPath
              CycleNo -> coolFile sqliteDbPath
        pure $
          BenchTarget
            (sqliteName cache cycleConn)
            popCache
            queryCache
            (syncFile sqliteDbPath)
            coolBatch
      mkPure = mkInMem "pure-wrapped index" newPureLeiosTxCache
      mkHt = mkInMem "hash-table (shift 22)" (newHashTableLeiosTxCache 22 salt0 salt1)
      targets :: [IO BenchTarget]
      targets = case variant of
        Just "pure" -> [mkPure]
        Just "ht" -> [mkHt]
        Just "sqlite" -> [mkSqlite]
        _ -> [mkPure, mkHt, mkSqlite]
  mapM_ (>>= runBench) targets

-- | A benchmark subject: the two cache handles (the same one twice for the
-- in-memory variants; distinct connections for SQLite), the post-population sync
-- that makes SQLite's pages evictable, and the per-batch cooling.
-- Positional (see the pattern in 'runBench'): name, population handle, query
-- handle, post-population sync, per-batch cooling.
data BenchTarget
  = BenchTarget String BenchCache BenchCache (IO ()) (IO ())

-- | Where the SQLite variant keeps its database (removed and recreated per run).
sqliteDbPath :: FilePath
sqliteDbPath = "leios-txcache-bench.sqlite"

-- | The query connection's private page-cache size (@--cache@).
data Cache = SmallCache | BigCache

-- | Whether to reopen the query connection before each batch (@--cycle-connection@),
-- giving a cold private cache per batch.
data Cycle = CycleYes | CycleNo

-- | The @PRAGMA cache_size@ value: 16 pages (~512 kB, far too small to hold the
-- index) vs -262144 (~256 MB, holds the whole batch working set).
cacheSizePragma :: Cache -> Int
cacheSizePragma SmallCache = 16
cacheSizePragma BigCache = -262144

sqliteName :: Cache -> Cycle -> String
sqliteName c y =
  "sqlite (cache="
    <> (case c of SmallCache -> "small"; BigCache -> "big")
    <> ", cycle="
    <> (case y of CycleYes -> "yes"; CycleNo -> "no")
    <> ")"

-- | The SQLite variant only reaches its cold-cache worst case if the db file's
-- pages can be evicted from the OS page cache. 'coolFile' does that best-effort
-- via @posix_fadvise@, but a guaranteed cold cache needs privilege to drop the
-- page cache. Always shown, so the caveat is never silently lost.
printPrivilegeReminder :: IO ()
printPrivilegeReminder =
  putStr $
    unlines
      [ ""
      , "NOTE: the SQLite variant's cold-cache worst case relies on evicting its"
      , "db file from the OS page cache before each batch. posix_fadvise(DONTNEED)"
      , "is best-effort; for a guaranteed cold cache, run with elevated privileges"
      , "(so the OS page cache can be dropped). Otherwise the reported SQLite"
      , "latency may understate the true worst case."
      ]

-- * OS page-cache cooling (SQLite variant)

foreign import ccall unsafe "posix_fadvise"
  c_posix_fadvise :: CInt -> COff -> COff -> CInt -> IO CInt

-- | @POSIX_FADV_DONTNEED@ (Linux).
posixFadvDontneed :: CInt
posixFadvDontneed = 4

-- | Evict a file's pages from the OS page cache, so subsequent reads fault from
-- disk. Best-effort: only clean (flushed) pages are dropped, and it does nothing
-- for pages pinned by an active mmap — which is why the SQLite cache opens with
-- @mmap_size = 0@.
coolFile :: FilePath -> IO ()
coolFile path = do
  h <- openFile path ReadMode
  fd <- handleToFd h
  _ <- c_posix_fadvise (fromIntegral fd) 0 0 posixFadvDontneed
  closeFd fd

foreign import ccall unsafe "fsync"
  c_fsync :: CInt -> IO CInt

-- | Flush a file's dirty pages to disk so a subsequent 'coolFile' can evict them
-- (posix_fadvise drops only clean pages). Run once after the fast, unsafe
-- population — which uses @synchronous = OFF@ and so never fsyncs on its own.
syncFile :: FilePath -> IO ()
syncFile path = do
  h <- openFile path ReadWriteMode
  fd <- handleToFd h
  _ <- c_fsync (fromIntegral fd)
  closeFd fd

runBench :: BenchTarget -> IO ()
runBench (BenchTarget name popCache queryCache syncAfterPop coolBatch) = do
  -- Pre-generate all EB data (hashes fully forced) OUTSIDE the timed region, so
  -- the measured population allocation is index-op churn, not hash generation.
  putStr "\ngenerating data... " >> hFlush stdout
  ebData <-
    forM [0 .. numEbs - 1] $ \e -> do
      let !txhs = force $ V.generate txsPerEb (\i -> mkTxHash (e * txsPerEb + i))
          !bs = BS.concat [b | MkTxHash b <- V.toList txhs]
      pure (mkEbHash e, mkRbHash e, SlotNo (fromIntegral e), txhs, bs)
  _ <- evaluate (length ebData)
  putStrLn "done"

  -- Populate the index (timed) via the population handle. Exactly 'numEbs'
  -- announcements, so no eviction.
  putStr "populating index... " >> hFlush stdout
  allocBefore <- bytesAllocated
  (_, popNs) <-
    timedNs $
      forM_ ebData $ \(ebh, rbh, slot, txhs, bs) -> do
        _ <- insertAnnouncement popCache slot rbh ebh
        _ <- insertBody popCache ebh (BenchBody bs) () (\() _ _ _ -> ())
        withLockedInsertUnappliedTx popCache $ \z step ->
          foldM (\ !acc txh -> step acc txh ()) z txhs
  allocAfter <- bytesAllocated
  -- Flush population to disk (a no-op for the in-memory variants) so the query
  -- handle reads durable, coolable pages.
  syncAfterPop
  putStrLn "done"

  -- Residency (post-major-GC live set); ebData is now dead and collectable.
  performMajorGC
  stats <- getRTSStats
  let live = gcdetails_live_bytes (gc stats)
      maxLive = max_live_bytes stats
      peakMem = max_mem_in_use_bytes stats
      popAlloc = allocAfter - allocBefore

  -- One-shot report: population and residency.
  putStr $
    unlines
      [ ""
      , "== " <> name <> " =="
      , "population:"
      , "  time             : " <> showNs popNs
      , "  allocated        : " <> showBytes popAlloc
      , "  alloc rate       : " <> showBytes (perSecond popAlloc popNs) <> "/s"
      , "residency (post major GC):"
      , "  live             : " <> showBytes live
      , "  max live         : " <> showBytes maxLive
      , "  peak mem in use  : " <> showBytes peakMem
      , "lookup — EB batch of " <> show txsPerEb <> " (x" <> show numLookupRuns <> " each):"
      ]

  -- Lookup latency at each hit ratio. Probe hashes are forced up front so timing
  -- excludes their generation; every batch uses the index, so it stays live
  -- through all measurements (including the residency read above). The loop
  -- prints a per-ratio summary as it goes and collects the sorted durations as a
  -- column; the full grid is printed once at the end.
  let ratios = [0, 20, 40, 60, 80, 100 :: Int]
  cols <- forM ratios $ \pct -> do
    probe <- evaluate $ force $ mkProbe pct
    let lookupBatch =
          withLookupTx queryCache $ \look ->
            foldM (\ !hits txh -> (\r -> hits + maybe 0 (const 1) r) <$> look txh) (0 :: Int) probe
    coolBatch
    hits <- lookupBatch -- warmup, and the actual resident count
    laBefore <- bytesAllocated
    -- 'coolBatch' runs OUTSIDE 'timedNs' so cooling is excluded from the latency;
    -- for the SQLite variant it evicts the db file so each batch reads cold. It is
    -- a no-op for the in-memory variants. (Its own tiny allocation does fall inside
    -- the 'lookupAlloc' bracket below.)
    times <- forM [1 .. numLookupRuns] $ \_ -> do
      coolBatch
      snd <$> timedNs lookupBatch
    laAfter <- bytesAllocated
    let avgNs = sum times `div` fromIntegral numLookupRuns
        perTxNs = fromIntegral avgNs / fromIntegral txsPerEb :: Double
        lookupAlloc = (laAfter - laBefore) `div` fromIntegral numLookupRuns
    putStrLn $
      "  "
        <> lpad 3 (show pct)
        <> "% hits ("
        <> show hits
        <> "/"
        <> show txsPerEb
        <> "): avg "
        <> showNs avgNs
        <> ", per-tx "
        <> showFFloat (Just 1) perTxNs " ns"
        <> ", alloc "
        <> showBytes lookupAlloc
    pure (sort times)

  -- The grid: one column per ratio, rows the sorted batch durations.
  let colW = 11
      header = "    " <> concatMap (lpad colW . (<> "%") . show) ratios
      grid = ["    " <> concatMap (lpad colW . showNs) row | row <- transpose cols]
  putStr $
    unlines $
      [ ""
      , "batch durations (ascending per column; "
          <> show numLookupRuns
          <> " rows x "
          <> show (length ratios)
          <> " ratios):"
      , header
      ]
        ++ grid

-- * Deterministic, well-distributed 32-byte hashes

-- | 32 bytes from a counter via a splitmix64 avalanche, so keys are spread like
-- real Blake2b hashes (differ in early bytes) — usable by an ordered map today
-- and a hash table later.
bytes32 :: Word64 -> ByteString
bytes32 k =
  BSL.toStrict $
    BB.toLazyByteString $
      BB.word64BE (mix (4 * k))
        <> BB.word64BE (mix (4 * k + 1))
        <> BB.word64BE (mix (4 * k + 2))
        <> BB.word64BE (mix (4 * k + 3))
 where
  mix z0 =
    let z1 = (z0 `Bits.xor` (z0 `Bits.shiftR` 30)) * 0xbf58476d1ce4e5b9
        z2 = (z1 `Bits.xor` (z1 `Bits.shiftR` 27)) * 0x94d049bb133111eb
     in z2 `Bits.xor` (z2 `Bits.shiftR` 31)

mkTxHash :: Int -> TxHash
mkTxHash = MkTxHash . bytes32 . fromIntegral

mkEbHash :: Int -> EbHash
mkEbHash = MkEbHash . bytes32 . fromIntegral

mkRbHash :: Int -> RbHash
mkRbHash = MkRbHash . bytes32 . fromIntegral

-- | The resident EB whose txs serve as the "hit" probe hashes.
probeEb :: Int
probeEb = numEbs `div` 2

-- | A probe batch of 'txsPerEb' hashes with @pct@% present, interleaved: hits
-- drawn from the resident 'probeEb', misses from an index range never inserted.
mkProbe :: Int -> V.Vector TxHash
mkProbe pct =
  V.generate txsPerEb $ \i ->
    if i `mod` 5 < pct `div` 20
      then mkTxHash (probeEb * txsPerEb + i) -- resident: a hit
      else mkTxHash (numEbs * txsPerEb + i) -- never inserted: a miss

-- * Helpers

timedNs :: IO a -> IO (a, Word64)
timedNs act = do
  t0 <- getMonotonicTimeNSec
  !x <- act
  t1 <- getMonotonicTimeNSec
  pure (x, t1 - t0)

bytesAllocated :: IO Word64
bytesAllocated = allocated_bytes <$> getRTSStats

perSecond :: Word64 -> Word64 -> Word64
perSecond bytes ns
  | ns == 0 = 0
  | otherwise = round (fromIntegral bytes * 1e9 / fromIntegral ns :: Double)

showBytes :: Word64 -> String
showBytes b
  | b < ki = show b <> " B"
  | b < ki * ki = showFFloat (Just 1) (fromIntegral b / fromIntegral ki :: Double) " KiB"
  | b < ki * ki * ki = showFFloat (Just 1) (fromIntegral b / fromIntegral (ki * ki) :: Double) " MiB"
  | otherwise = showFFloat (Just 2) (fromIntegral b / fromIntegral (ki * ki * ki) :: Double) " GiB"
 where
  ki = 1024 :: Word64

showNs :: Word64 -> String
showNs ns
  | ns < 1_000 = show ns <> " ns"
  | ns < 1_000_000 = showFFloat (Just 2) (fromIntegral ns / 1e3 :: Double) " µs"
  | ns < 1_000_000_000 = showFFloat (Just 2) (fromIntegral ns / 1e6 :: Double) " ms"
  | otherwise = showFFloat (Just 2) (fromIntegral ns / 1e9 :: Double) " s"

lpad :: Int -> String -> String
lpad n s = replicate (max 0 (n - length s)) ' ' <> s
