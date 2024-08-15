{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Bench.Consensus.Mempool
import           Bench.Consensus.Mempool.TestBlock (TestBlock)
import qualified Bench.Consensus.Mempool.TestBlock as TestBlock
import           Control.Arrow (first)
import           Control.DeepSeq
import           Control.Monad (unless)
import qualified Control.Tracer as Tracer
import           Criterion (bench, perRunEnv)
import           Criterion.Main (defaultMainWith)
import           Criterion.Main.Options (config, defaultConfig, describeWith)
import           Criterion.Types (Config (csvFile))
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import           Data.Maybe (fromMaybe)
import           Data.Set ()
import qualified Data.Text.Read as Text.Read
import           Main.Utf8 (withStdTerminalHandles)
import           Options.Applicative (execParser)
import qualified Ouroboros.Consensus.Mempool.Capacity as Mempool
import           System.Exit (die, exitFailure)
import qualified Test.Consensus.Mempool.Mocked as Mocked
import           Test.Consensus.Mempool.Mocked (MockedMempool)
import           Test.Tasty (defaultIngredients, testGroup, withResource)
import           Test.Tasty.HUnit (testCase, (@?=))
import           Test.Tasty.Runners (parseOptions, tryIngredients)

main :: IO ()
main = withStdTerminalHandles $ do
    let csvFilePath = "mempool-benchmarks.csv"
    runBenchmarks csvFilePath
    rawValues <- parseBenchmarkResults csvFilePath
    convertCsvRowsToJsonObjects rawValues "mempool-benchmarks.json"
    runTests
  where
    runBenchmarks csvFilePath = do
        cfg <- execParser $ describeWith $ config defaultConfig
        let cfg' = cfg { csvFile = Just . fromMaybe csvFilePath $ csvFile cfg }
        defaultMainWith cfg' $ fmap benchAddNTxs [10_000, 20_000]
      where
        benchAddNTxs n = bench ("Just adding " <> show n <> " transactions") $
          perRunEnv
            (do
              let txs = mkNTryAddTxs n
              mempool <- openMempoolWithCapacityFor txs
              pure (mempool,txs))
            (uncurry run)
    runTests = do
        opts <- parseOptions defaultIngredients testJustAddingTransactions
        case tryIngredients defaultIngredients opts testJustAddingTransactions of
          Nothing               -> exitFailure
          Just    runIngredient -> do
            success <- runIngredient
            unless success exitFailure
      where
        testJustAddingTransactions =
          testGroup "Just adding" $
            fmap testAddNTxs [10_000, 20_000]
        testAddNTxs n =
          withResource
            (pure $!! mkNTryAddTxs n)
            (\_ -> pure ())
            (\getTxs -> do
                testGroup (show n <> " transactions") [
                  testCase "test" $ do
                      txs <- getTxs
                      mempool <- openMempoolWithCapacityFor txs
                      testAddTxs mempool txs
                  , testCase "txs length" $ do
                      txs <- getTxs
                      length txs @?= n
                  ]
            )
          where
            testAddTxs mempool txs = do
                run mempool txs
                mempoolTxs <- Mocked.getTxs mempool
                mempoolTxs @?= getCmdsTxs txs

    parseBenchmarkResults csvFilePath = do
        csvData <- BL.readFile csvFilePath
        case Csv.decode Csv.HasHeader csvData of
          Left err   -> die err
          Right rows -> pure rows

    -- Output the mempool benchmark results as a JSON file, which conforms to
    -- the input expected by
    -- https://github.com/benchmark-action/github-action-benchmark
    convertCsvRowsToJsonObjects rows outFilePath =
      encodeFile outFilePath $ fmap convertRowToJsonObject rows
      where
        convertRowToJsonObject (name:mean:_) =
          object [ "name"  .= name
                 , "value" .= adjustedMean
                 , "unit"  .= unit
                 ]
          where
            adjustedMean :: Integer
            (adjustedMean, unit) = first round
                                 $ convertSecondsWithUnit
                                 $ textToDouble mean
              where
                textToDouble = either error fst . Text.Read.double

            -- Convert a number of seconds to the largest time unit that
            -- makes the conversion greater or equal than one.
            convertSecondsWithUnit :: Double -> (Double, String)
            convertSecondsWithUnit n
                | n >= 1     = (n        , "seconds"     )
                | n >= 1e-3  = (n * 1e3  , "milliseconds")
                | n >= 1e-6  = (n * 1e6  , "microseconds")
                | n >= 1e-9  = (n * 1e9  , "nanoseconds" )
                | n >= 1e-12 = (n * 1e12 , "picoseconds" )
            convertSecondsWithUnit _ = error "All the cases should be covered by the conditions above"

        convertRowToJsonObject _             = error "Wrong format"

{-------------------------------------------------------------------------------
  Adding TestBlock transactions to a mempool
-------------------------------------------------------------------------------}

openMempoolWithCapacityFor :: [MempoolCmd TestBlock] ->  IO (MockedMempool IO TestBlock)
openMempoolWithCapacityFor cmds =
    Mocked.openMockedMempool capacityRequiredByCmds
                             Tracer.nullTracer
                             TestBlock.txSize
                             Mocked.MempoolAndModelParams {
                                 Mocked.immpInitialState = TestBlock.initialLedgerState
                               , Mocked.immpLedgerConfig = TestBlock.sampleLedgerConfig
                             }
  where
    capacityRequiredByCmds = Mempool.mkCapacityBytesOverride totalTxsSize
      where totalTxsSize = sum $ fmap TestBlock.txSize $ getCmdsTxs cmds

mkNTryAddTxs :: Int -> [MempoolCmd TestBlock.TestBlock]
mkNTryAddTxs 0 = []
mkNTryAddTxs n =        [AddTx (TestBlock.mkTx [] [TestBlock.Token 0])]
                <> fmap (AddTx . mkSimpleTx) (zip [0 .. n - 2] [1 .. n - 1])
  where
    mkSimpleTx (x, y) = TestBlock.mkTx [TestBlock.Token (fromIntegral x)]
                                       [TestBlock.Token (fromIntegral y)]
