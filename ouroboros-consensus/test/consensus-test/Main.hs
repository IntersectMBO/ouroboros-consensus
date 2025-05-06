{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main (main) where

import qualified Test.Consensus.BlockchainTime.Simple (tests)
import qualified Test.Consensus.HardFork.Forecast (tests)
import qualified Test.Consensus.HardFork.History (tests)
import qualified Test.Consensus.HardFork.Summary (tests)
import qualified Test.Consensus.HeaderValidation (tests)
import qualified Test.Consensus.Ledger.Tables.Diff (tests)
import qualified Test.Consensus.Ledger.Tables.DiffSeq (tests)
import qualified Test.Consensus.Mempool (tests)
import qualified Test.Consensus.Mempool.Fairness (tests)
import qualified Test.Consensus.Mempool.StateMachine (tests)
import qualified Test.Consensus.MiniProtocol.BlockFetch.Client (tests)
import qualified Test.Consensus.MiniProtocol.ChainSync.Client (tests)
import qualified Test.Consensus.MiniProtocol.ChainSync.CSJ (tests)
import qualified Test.Consensus.MiniProtocol.LocalStateQuery.Server (tests)
import qualified Test.Consensus.Util.MonadSTM.NormalForm (tests)
import qualified Test.Consensus.Util.Versioned (tests)
import           Test.Tasty
import           Test.Util.TestEnv (defaultMainWithTestEnv,
                     defaultTestEnvConfig)

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests

tests :: TestTree
tests =
  testGroup "ouroboros-consensus"
  [ Test.Consensus.BlockchainTime.Simple.tests
  , Test.Consensus.HeaderValidation.tests
  , Test.Consensus.MiniProtocol.BlockFetch.Client.tests
  , Test.Consensus.MiniProtocol.ChainSync.CSJ.tests
  , Test.Consensus.MiniProtocol.ChainSync.Client.tests
  , Test.Consensus.MiniProtocol.LocalStateQuery.Server.tests
  , testGroup "Mempool"
       [ Test.Consensus.Mempool.tests
       , Test.Consensus.Mempool.Fairness.tests
       , Test.Consensus.Mempool.StateMachine.tests
       ]
  , Test.Consensus.Util.MonadSTM.NormalForm.tests
  , Test.Consensus.Util.Versioned.tests
  , testGroup "HardFork" [
        testGroup "History" [
            Test.Consensus.HardFork.Summary.tests
          , Test.Consensus.HardFork.History.tests
          ]
      , testGroup "Combinator" [
            Test.Consensus.HardFork.Forecast.tests
          ]
      ]
  , Test.Consensus.Ledger.Tables.Diff.tests
  , Test.Consensus.Ledger.Tables.DiffSeq.tests
  ]
