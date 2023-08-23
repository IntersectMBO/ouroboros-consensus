{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}

module Main (main) where

import           Bench.Consensus.Mempool
import           Bench.Consensus.Mempool.Defaults
import           Bench.Consensus.Mempool.Scenario
import           Bench.Consensus.Mempool.TestBlock (TestBlock)
import qualified Bench.Consensus.Mempool.TestBlock as TestBlock
import           Control.Arrow (first)
import           Control.Monad (unless, void)
import qualified Control.Tracer as Tracer
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text.Read
import qualified Ouroboros.Consensus.Mempool.Capacity as Mempool
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore.Init
                     (BackingStoreSelector (..))
import           System.Exit (die, exitFailure)
import qualified Test.Consensus.Mempool.Mocked as Mocked
import           Test.Consensus.Mempool.Mocked (MockedMempool)
import           Test.Tasty (withResource)
import           Test.Tasty.Bench (Benchmark, CsvPath (CsvPath), bench,
                     benchIngredients, bgroup, nfIO)
import           Test.Tasty.HUnit (testCase, (@?=))
import           Test.Tasty.Options (changeOption)
import           Test.Tasty.Runners (parseOptions, tryIngredients)

main :: IO ()
main = do
    let csvFilePath = "mempool-benchmarks.csv"
    runBenchmarks csvFilePath
    rawValues <- parseBenchmarkResults csvFilePath
    convertCsvRowsToJsonObjects rawValues "mempool-benchmarks.json"
  where
    runBenchmarks csvFilePath = do
        opts <- parseOptions benchIngredients benchmarkJustAddingTransactions
        let opts' = changeOption (Just . fromMaybe (CsvPath csvFilePath)) opts
        case tryIngredients benchIngredients opts' benchmarkJustAddingTransactions of
          Nothing               -> exitFailure
          Just    runIngredient -> do
            success <- runIngredient
            unless success exitFailure
      where
        bsss = [defaultInMemoryBSS, defaultLMDB_BSS]
        ns   = [10_000]

        showBackingStoreSelector bss = case bss of
          InMemoryBackingStore -> "inmem"
          LMDBBackingStore _   -> "lmdb"

        benchmarkJustAddingTransactions =
            bgroup "Just adding" [
                bgroup "empty, empty, n linked" [
                    benchAddNTxs bss n $ do
                        theCandidateTransactionsHaveLinkedTxs Nothing n
                  | bss <- bsss
                  , n <- ns
                  ]
              , bgroup "empty, 2_160 linked, n linked" [
                    benchAddNTxs bss n $ do
                        clTxs <- theChangelogHasLinkedTxs 2_160
                        theCandidateTransactionsHaveLinkedTxs (Just $ last clTxs) n
                  | bss <- bsss
                  , n <- ns
                  ]
              , bgroup "empty, empty, n independent" [
                    benchAddNTxs bss n $ do
                        theCandidateTransactionsHave  n
                  | bss <- bsss
                  , n <- ns
                  ]
              , bgroup "n independent, empty, n independent" [
                    benchAddNTxs bss n $ do
                        bTxs <- theBackingStoreHas n
                        theCandidateTransactionsConsume bTxs
                  | bss <- bsss
                  , n <- ns
                  ]
              , bgroup "empty, n independent, n independent" [
                    benchAddNTxs bss n $ do
                        cTxs <- theChangelogHas n
                        theCandidateTransactionsConsume cTxs
                  | bss <- bsss
                  , n <- ns
                  ]
              , bgroup "n independent, n independent, n independent" [
                    benchAddNTxs bss n $ do
                        bTxs <- theBackingStoreHas n
                        cTxs <- theChangelogConsumes bTxs
                        theCandidateTransactionsConsume cTxs
                  | bss <- bsss
                  , n <- ns
                  ]
              ]
          where
            benchAddNTxs :: BackingStoreSelector IO -> Int -> ScBuilder () -> Benchmark
            benchAddNTxs bss n scenario =
                withResource
                  ((, txs0) <$> openMempoolWithCapacityFor params txs0)
                  (\_ -> pure ())
                  (\getAcquiredRes -> do
                      let withAcquiredMempool act = do
                            (mempool, txs) <- getAcquiredRes
                            void $ act mempool txs
                            -- TODO: consider adding a 'reset' command to the mempool to make sure its state is not tainted.
                            maybe (pure ()) (Mocked.removeTxs mempool) $ NE.nonEmpty $ getCmdsTxIds txs
                      bgroup (showBackingStoreSelector (Mocked.immpBackingStoreSelector params)
                               <> ": "
                               <> show n <> " transactions") [
                          bench "benchmark" $ nfIO $ withAcquiredMempool $ \mempool txs -> do
                            run mempool txs
                        , testCase "test" $ withAcquiredMempool $ \mempool txs ->
                            testAddTxs mempool txs
                        , testCase "txs length" $ withAcquiredMempool $ \_mempool txs -> do
                            length txs @?= n
                        ]
                  )
              where
                (params, txs0) = fromScenario defaultLedgerDbCfg bss (build scenario)

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
          object [ "name"  .= adjustName name
                 , "value" .= adjustedMean
                 , "unit"  .= unit
                 ]
          where
            adjustName = Text.replace "."          " "
                       . Text.replace ".benchmark" ""

            adjustedMean :: Integer
            (adjustedMean, unit) = first round
                                 $ convertPicosecondsWithUnit
                                 $ fromInteger
                                 $ textToInt mean
              where
                textToInt = either error fst . Text.Read.decimal

            -- Convert a number of picoseconds to the largest time unit that
            -- makes the conversion greater or equal than one.
            convertPicosecondsWithUnit :: Double -> (Double, String)
            convertPicosecondsWithUnit n
                |                        numberOfDigits  <= 4  = (n       , "picoseconds" )
                | 4 <= numberOfDigits  && numberOfDigits <  7  = (n / 1e3 , "nanoseconds" )
                | 7 <= numberOfDigits  && numberOfDigits <  10 = (n / 1e6 , "microseconds")
                | 10 <= numberOfDigits && numberOfDigits <  13 = (n / 1e9 , "milliseconds")
                | 13 <= numberOfDigits                         = (n / 1e12, "seconds"     )
              where
                numberOfDigits :: Int
                numberOfDigits = floor (logBase 10 n) + 1
            convertPicosecondsWithUnit _ = error "All the cases should be covered by the conditions above"

        convertRowToJsonObject _             = error "Wrong format"

{-------------------------------------------------------------------------------
  Adding TestBlock transactions to a mempool
-------------------------------------------------------------------------------}

openMempoolWithCapacityFor ::
     Mocked.InitialMempoolAndModelParams IO TestBlock
  -> [MempoolCmd TestBlock]
  -> IO (MockedMempool IO TestBlock)
openMempoolWithCapacityFor params cmds =
    Mocked.openMockedMempool capacityRequiredByCmds
                                   Tracer.nullTracer
                                   TestBlock.txSize
                                   params
  where
    capacityRequiredByCmds = Mempool.mkCapacityBytesOverride totalTxsSize
      where totalTxsSize = sum $ fmap TestBlock.txSize $ getCmdsTxs cmds
