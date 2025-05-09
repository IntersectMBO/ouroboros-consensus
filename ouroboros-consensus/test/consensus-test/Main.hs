{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

import Test.Consensus.BlockchainTime.Simple qualified (tests)
import Test.Consensus.HardFork.Forecast qualified (tests)
import Test.Consensus.HardFork.History qualified (tests)
import Test.Consensus.HardFork.Summary qualified (tests)
import Test.Consensus.HeaderValidation qualified (tests)
import Test.Consensus.Ledger.Tables.Diff qualified (tests)
import Test.Consensus.Ledger.Tables.DiffSeq qualified (tests)
import Test.Consensus.Mempool qualified (tests)
import Test.Consensus.Mempool.Fairness qualified (tests)
import Test.Consensus.Mempool.StateMachine qualified (tests)
import Test.Consensus.MiniProtocol.BlockFetch.Client qualified (tests)
import Test.Consensus.MiniProtocol.ChainSync.CSJ qualified (tests)
import Test.Consensus.MiniProtocol.ChainSync.Client qualified (tests)
import Test.Consensus.MiniProtocol.LocalStateQuery.Server qualified (tests)
import Test.Consensus.Util.MonadSTM.NormalForm qualified (tests)
import Test.Consensus.Util.Versioned qualified (tests)
import Test.Tasty
import Test.Util.TestEnv
  ( defaultMainWithTestEnv
  , defaultTestEnvConfig
  )

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests

tests :: TestTree
tests =
  testGroup
    "ouroboros-consensus"
    [ Test.Consensus.BlockchainTime.Simple.tests
    , Test.Consensus.HeaderValidation.tests
    , Test.Consensus.MiniProtocol.BlockFetch.Client.tests
    , Test.Consensus.MiniProtocol.ChainSync.CSJ.tests
    , Test.Consensus.MiniProtocol.ChainSync.Client.tests
    , Test.Consensus.MiniProtocol.LocalStateQuery.Server.tests
    , testGroup
        "Mempool"
        [ Test.Consensus.Mempool.tests
        , Test.Consensus.Mempool.Fairness.tests
        , Test.Consensus.Mempool.StateMachine.tests
        ]
    , Test.Consensus.Util.MonadSTM.NormalForm.tests
    , Test.Consensus.Util.Versioned.tests
    , testGroup
        "HardFork"
        [ testGroup
            "History"
            [ Test.Consensus.HardFork.Summary.tests
            , Test.Consensus.HardFork.History.tests
            ]
        , testGroup
            "Combinator"
            [ Test.Consensus.HardFork.Forecast.tests
            ]
        ]
    , Test.Consensus.Ledger.Tables.Diff.tests
    , Test.Consensus.Ledger.Tables.DiffSeq.tests
    ]
