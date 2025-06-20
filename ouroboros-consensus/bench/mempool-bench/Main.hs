{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Bench.Consensus.Mempool
import Bench.Consensus.Mempool.TestBlock (TestBlock)
import qualified Bench.Consensus.Mempool.TestBlock as TestBlock
import Control.Arrow (first)
import Control.DeepSeq
import Control.Monad (unless)
import qualified Control.Tracer as Tracer
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text.Read
import Main.Utf8 (withStdTerminalHandles)
import Ouroboros.Consensus.Ledger.SupportsMempool (ByteSize32)
import qualified Ouroboros.Consensus.Mempool.Capacity as Mempool
import System.Exit (die, exitFailure)
import System.IO (hPutStrLn, stderr)
import Test.Consensus.Mempool.Mocked (MockedMempool)
import qualified Test.Consensus.Mempool.Mocked as Mocked
import Test.Tasty (withResource)
import Test.Tasty.Bench
  ( CsvPath (CsvPath)
  , bench
  , benchIngredients
  , bgroup
  , whnfIO
  )
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Options (changeOption)
import Test.Tasty.Runners (parseOptions, tryIngredients)

main :: IO ()
main = withStdTerminalHandles $ do
  let csvFilePath = "mempool-benchmarks.csv"
  runBenchmarks csvFilePath
  rawValues <- parseBenchmarkResults csvFilePath
  convertCsvRowsToJsonObjects rawValues "mempool-benchmarks.json"
 where
  runBenchmarks csvFilePath = do
    opts <- parseOptions benchIngredients benchmarkJustAddingTransactions
    let opts' = changeOption (Just . fromMaybe (CsvPath csvFilePath)) opts
    case tryIngredients benchIngredients opts' benchmarkJustAddingTransactions of
      Nothing -> exitFailure
      Just runIngredient -> do
        success <- runIngredient
        unless success $ do
          hPutStrLn
            stderr
            "This benchmark is flaky in GitHub Actions due to CI runner load, which can it to run significantly slower than expected. It may be useful to try to re-run the job if it fails. See https://github.com/IntersectMBO/ouroboros-consensus/issues/313"
          exitFailure
   where
    benchmarkJustAddingTransactions =
      bgroup "Just adding" $
        fmap benchAddNTxs [10_000, 20_000]
     where
      benchAddNTxs n =
        withResource
          ( pure $!!
              let cmds = mkNTryAddTxs n
                  sz = foldMap TestBlock.txSize $ getCmdsTxs cmds
               in (cmds, sz)
          )
          (\_ -> pure ())
          ( \getCmds -> do
              bgroup
                (show n <> " transactions")
                [ bench "setup mempool" $ whnfIO $ do
                    (_cmds, capacity) <- getCmds
                    openMempoolWithCapacity capacity
                , bench "setup mempool + benchmark" $ whnfIO $ do
                    (cmds, capacity) <- getCmds
                    mempool <- openMempoolWithCapacity capacity
                    run mempool cmds
                , testCase "test" $ do
                    (cmds, capacity) <- getCmds
                    mempool <- openMempoolWithCapacity capacity
                    testAddCmds mempool cmds
                , testCase "cmds length" $ do
                    (cmds, _capacity) <- getCmds
                    length cmds @?= n
                ]
          )
       where
        testAddCmds mempool cmds = do
          run mempool cmds
          mempoolTxs <- Mocked.getTxs mempool
          mempoolTxs @?= getCmdsTxs cmds

  parseBenchmarkResults csvFilePath = do
    csvData <- BL.readFile csvFilePath
    case Csv.decode Csv.HasHeader csvData of
      Left err -> die err
      Right rows -> pure rows

  -- Output the mempool benchmark results as a JSON file, which conforms to
  -- the input expected by
  -- https://github.com/benchmark-action/github-action-benchmark
  convertCsvRowsToJsonObjects rows outFilePath =
    encodeFile outFilePath $ fmap convertRowToJsonObject rows
   where
    convertRowToJsonObject (name : mean : _) =
      object
        [ "name" .= adjustName name
        , "value" .= adjustedMean
        , "unit" .= unit
        ]
     where
      adjustName =
        Text.replace "." " "
          . Text.replace ".benchmark" ""

      adjustedMean :: Integer
      (adjustedMean, unit) =
        first round $
          convertPicosecondsWithUnit $
            fromInteger $
              textToInt mean
       where
        textToInt = either error fst . Text.Read.decimal

      -- Convert a number of picoseconds to the largest time unit that
      -- makes the conversion greater or equal than one.
      convertPicosecondsWithUnit :: Double -> (Double, String)
      convertPicosecondsWithUnit n
        | numberOfDigits <= 4 = (n, "picoseconds")
        | 4 <= numberOfDigits && numberOfDigits < 7 = (n / 1e3, "nanoseconds")
        | 7 <= numberOfDigits && numberOfDigits < 10 = (n / 1e6, "microseconds")
        | 10 <= numberOfDigits && numberOfDigits < 13 = (n / 1e9, "milliseconds")
        | 13 <= numberOfDigits = (n / 1e12, "seconds")
       where
        numberOfDigits :: Int
        numberOfDigits = floor (logBase 10 n) + 1
      convertPicosecondsWithUnit _ = error "All the cases should be covered by the conditions above"
    convertRowToJsonObject _ = error "Wrong format"

{-------------------------------------------------------------------------------
  Adding TestBlock transactions to a mempool
-------------------------------------------------------------------------------}

openMempoolWithCapacity :: ByteSize32 -> IO (MockedMempool IO TestBlock)
openMempoolWithCapacity capacity =
  Mocked.openMockedMempool
    (Mempool.mkCapacityBytesOverride capacity)
    Tracer.nullTracer
    Mocked.MempoolAndModelParams
      { Mocked.immpInitialState = TestBlock.initialLedgerState
      , Mocked.immpLedgerConfig = TestBlock.sampleLedgerConfig
      }

mkNTryAddTxs :: Int -> [MempoolCmd TestBlock.TestBlock]
mkNTryAddTxs 0 = []
mkNTryAddTxs n =
  [AddTx (TestBlock.mkTx [] [TestBlock.Token 0])]
    <> fmap (AddTx . mkSimpleTx) (zip [0 .. n - 2] [1 .. n - 1])
 where
  mkSimpleTx (x, y) =
    TestBlock.mkTx
      [TestBlock.Token (fromIntegral x)]
      [TestBlock.Token (fromIntegral y)]
