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
import Data.List (sort, transpose)
import qualified Data.Vector.Strict as V
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import LeiosDemoTypes (EbHash (..), RbHash (..), TxHash (..))
import LeiosTxCache
import LeiosTxCache.Mutable (newHashTableLeiosTxCache)
import LeiosTxCacheIndex (ReferencesTxsByHash (..), maxAnnouncementCount)
import Numeric (showFFloat)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.Mem (performMajorGC)

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
      | otherwise = go (f acc (MkTxHash (BS.copy (BS.take 32 (BS.drop (i * 32) bs))))) (i + 1)

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
  args <- getArgs
  let salt0, salt1 :: Word64
      salt0 = 0xD1CED00DFEEDFACE
      salt1 = 0x0123456789ABCDEF
      runs :: [(String, IO BenchCache)]
      runs = case args of
        ["pure"] -> [("pure-wrapped index", newPureLeiosTxCache)]
        ["ht"] -> [("hash-table (shift 22)", newHashTableLeiosTxCache 22 salt0 salt1)]
        _ ->
          [ ("pure-wrapped index", newPureLeiosTxCache)
          , ("hash-table (shift 22)", newHashTableLeiosTxCache 22 salt0 salt1)
          ]
  mapM_ (uncurry runBench) runs

runBench :: String -> IO BenchCache -> IO ()
runBench name mkCache = do
  cache <- mkCache

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

  -- Populate the index (timed). Exactly 'numEbs' announcements, so no eviction.
  putStr "populating index... " >> hFlush stdout
  allocBefore <- bytesAllocated
  (_, popNs) <-
    timedNs $
      forM_ ebData $ \(ebh, rbh, slot, txhs, bs) -> do
        _ <- insertAnnouncement cache slot rbh ebh
        insertBody cache ebh (BenchBody bs)
        withLockedInsertUnappliedTx cache $ \z step ->
          foldM (\ !acc txh -> step acc txh ()) z txhs
  allocAfter <- bytesAllocated
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
          withLookupTx cache $ \look ->
            foldM (\ !hits txh -> (\r -> hits + maybe 0 (const 1) r) <$> look txh) (0 :: Int) probe
    hits <- lookupBatch -- warmup, and the actual resident count
    laBefore <- bytesAllocated
    times <- forM [1 .. numLookupRuns] $ \_ -> snd <$> timedNs lookupBatch
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
