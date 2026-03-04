{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Control.DeepSeq (NFData (..))
import Control.Monad (void)
import Criterion.Main
import qualified Data.ByteString as BS
import LeiosDemoDb (LeiosDbHandle (..), newLeiosDBSQLite)
import LeiosDemoTypes (TxHash (..))
import System.Directory (removeDirectoryRecursive)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)

-- | Wraps a SQLite LeiosDbHandle and its cleanup action.
-- NFData instance is trivial: the handle contains already-evaluated IORefs,
-- so shallow evaluation is sufficient.
data DbEnv = DbEnv
  { benchDb :: !(LeiosDbHandle IO)
  , benchCleanup :: !(IO ())
  }

instance NFData DbEnv where
  rnf (DbEnv _ _) = ()

-- | Create a fresh SQLite LeiosDbHandle in a temporary directory.
freshSQLiteDb :: IO DbEnv
freshSQLiteDb = do
  sysTmp <- getCanonicalTemporaryDirectory
  tmpDir <- createTempDirectory sysTmp "leios-bench"
  db <- newLeiosDBSQLite (tmpDir <> "/bench.db")
  pure $ DbEnv db (leiosDbClose db >> removeDirectoryRecursive tmpDir)

-- | Generate n unique (TxHash, ByteString) pairs for benchmarking.
-- TxHashes are deterministic: the first 4 bytes encode the index, the rest are zero.
-- Tx bytes are 16KB of zeros, matching the 'maxTxBytesZero' test pattern.
genTxs :: Int -> [(TxHash, BS.ByteString)]
genTxs n =
  [ (txHashFromIndex i, txBytes)
  | i <- [0 .. n - 1]
  ]
 where
  txHashFromIndex i =
    MkTxHash $
      BS.pack $
        [ fromIntegral (i `div` 16_777_216 `mod` 256)
        , fromIntegral (i `div` 65_536 `mod` 256)
        , fromIntegral (i `div` 256 `mod` 256)
        , fromIntegral (i `mod` 256)
        ]
          ++ replicate 28 0
  txBytes = BS.replicate 16_384 0

-- | Benchmark inserting n transactions into a fresh SQLite database.
-- A fresh database is created for each measurement via 'perRunEnvWithCleanup',
-- ensuring that INSERT OR IGNORE does not cause later runs to measure no-ops.
benchInsertTxs :: Int -> Benchmark
benchInsertTxs n =
  env (return (genTxs n)) $ \txs ->
    bench (show n) $
      perRunEnvWithCleanup
        freshSQLiteDb
        benchCleanup
        (\dbEnv -> void $ leiosDbInsertTxs (benchDb dbEnv) txs)

main :: IO ()
main =
  defaultMain
    [ bgroup "insertTxs" $
        map benchInsertTxs [1, 10, 100, 1_000, 10_000]
    ]
