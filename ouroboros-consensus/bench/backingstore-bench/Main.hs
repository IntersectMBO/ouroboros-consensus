{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TupleSections      #-}

module Main (main) where

import           Bench.Commands (BackingStoreInitialiser, Cmd (..), run)
import           Cardano.Slotting.Slot (WithOrigin (..))
import           Control.DeepSeq (NFData (..), rwhnf)
import           Data.Map.Diff.Strict (Diff)
import qualified Data.Map.Diff.Strict as Diff
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           Ouroboros.Consensus.Ledger.Tables (DiffMK (..), KeysMK (..),
                     ValuesMK)
import           Ouroboros.Consensus.Ledger.Tables.Utils (emptyLedgerTables)
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
                     (BackingStorePath (..))
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore.LMDB
                     (LMDBLimits (..))
import           Ouroboros.Consensus.Storage.LedgerDB.Init
                     (BackingStoreSelector (..), newBackingStoreInitialiser)
import qualified System.Directory as Dir
import           System.FS.API (HasFS (..), SomeHasFS (..))
import           System.FS.API.Types (MountPoint (..), mkFsPath)
import           System.FS.IO (ioHasFS)
import           System.IO.Temp (createTempDirectory,
                     getCanonicalTemporaryDirectory)
import           Test.Tasty.Bench (Benchmark, bench, bgroup, defaultMain,
                     envWithCleanup, nfAppIO)
import           Test.Util.LedgerStateOnlyTables (OTLedgerTables,
                     pattern OTLedgerTables)

{-------------------------------------------------------------------------------
  Main benchmarks
-------------------------------------------------------------------------------}

main :: IO ()
main = defaultMain [ benchmarks ]

benchmarks :: Benchmark
benchmarks = bgroup "BackingStore" [
      benchCmds "simpleCopy InMemory" bssInMem simpleCopy
    , benchCmds "simpleCopy LMDB"     bssLMDB  simpleCopy
    , benchCmds "oneWritePer100Reads InMem 10_000" bssInMem $
        oneWritePer100Reads 10_000
    , benchCmds "oneWritePer100Reads LMDB  10_000"  bssLMDB $
        oneWritePer100Reads 10_000
    ]

benchCmds :: String -> BackingStoreSelector IO -> [Cmd K V D] -> Benchmark
benchCmds name bss cmds0 =
    envWithCleanup ((,cmds0) <$> setup bss) (eCleanup . fst) $
      \ ~(e, cmds) -> bench name $ nfAppIO (runner e) cmds
  where
    runner e cmds = do
      (shfs, cleanup) <- eMakeNewSomeHasFS e
      run shfs (eBackingStoreInitialiser e) cmds
      cleanup

bssInMem :: BackingStoreSelector m
bssInMem = InMemoryBackingStore

bssLMDB :: BackingStoreSelector IO
bssLMDB = LMDBBackingStore benchLMDBLimits

benchLMDBLimits :: LMDBLimits
benchLMDBLimits = LMDBLimits
  { lmdbMapSize      = 100 * 1_024 * 1_024
  , lmdbMaxDatabases = 3
  , lmdbMaxReaders   = 32
  }

{-------------------------------------------------------------------------------
  Benchmark scenarios
-------------------------------------------------------------------------------}

type K = OTLedgerTables Word64 Word64 KeysMK
type V = OTLedgerTables Word64 Word64 ValuesMK
type D = OTLedgerTables Word64 Word64 DiffMK

oneWritePer100Reads :: Int -> [Cmd K V D]
oneWritePer100Reads n = concat [
      [ini]
    , workload
    , [close]
    ]
  where
    ini      = BSInitFromValues Origin emptyLedgerTables
    close    = BSClose

    workload = flip concatMap dat $ \block -> mkWrite block : mkReads block

    mkWrite block = BSWrite (fst $ last block) $
        mkDiffs $ Diff.fromListInserts [(x,x) | (_sl, x) <- block]

    mkReads block = [BSRead (mkKey x) | (_sl, x) <- block]

    dat = groupsOfN 100 $ zip [0..] [0 .. fromIntegral n - 1]

simpleCopy :: [Cmd K V D]
simpleCopy = [
    BSInitFromValues Origin emptyLedgerTables
  , BSCopy (BackingStorePath $ mkFsPath ["copies", "somecopy"])
  , BSClose
  ]

{-------------------------------------------------------------------------------
  Benchmark scenarios: helpers
-------------------------------------------------------------------------------}

mkKey :: k -> OTLedgerTables k v KeysMK
mkKey = mkKeys . Set.singleton

mkKeys :: Set k -> OTLedgerTables k v KeysMK
mkKeys = OTLedgerTables . KeysMK

mkDiffs :: Diff k v -> OTLedgerTables k v DiffMK
mkDiffs = OTLedgerTables . DiffMK

groupsOfN :: Int -> [a] -> [[a]]
groupsOfN n
    | n <= 0    = error "groupsOfN: n should be positive"
    | otherwise = go
  where
    go :: [a] -> [[a]]
    go [] = []
    go xs = take n xs : groupsOfN n (drop n xs)

{-------------------------------------------------------------------------------
  Set up benchmark environment
-------------------------------------------------------------------------------}

data Env m ks vs d = Env {
    eBackingStoreInitialiser :: !(BackingStoreInitialiser m ks vs d)
  , eMakeNewSomeHasFS        :: !(m (SomeHasFS m, m ()))
  , eCleanup                 :: !(m ())
  }

instance NFData (Env m ks vs d) where rnf = rwhnf

setup :: BackingStoreSelector IO -> IO (Env IO K V D)
setup bss = do
  sysTmpDir <- getCanonicalTemporaryDirectory
  benchTmpDir <- createTempDirectory sysTmpDir "bench_backingstore"
  let bsi = newBackingStoreInitialiser mempty bss

  let f = do
        tmpDir <- createTempDirectory benchTmpDir "run"
        let hfs = ioHasFS (MountPoint tmpDir)

        createDirectory hfs (mkFsPath ["copies"])

        let cleanup = removeDirectoryRecursive hfs (mkFsPath [])

        pure (SomeHasFS hfs, cleanup)

  pure $ Env {
      eBackingStoreInitialiser = bsi
    , eMakeNewSomeHasFS        = f
    , eCleanup                 = Dir.removeDirectoryRecursive benchTmpDir
    }
