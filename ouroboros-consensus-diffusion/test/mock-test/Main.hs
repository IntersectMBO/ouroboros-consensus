module Main (main) where

import Test.Consensus.Ledger.Mock qualified (tests)
import Test.Consensus.Ledger.Mock.LedgerTables qualified (tests)
import Test.Tasty
import Test.ThreadNet.BFT qualified (tests)
import Test.ThreadNet.LeaderSchedule qualified (tests)
import Test.ThreadNet.PBFT qualified (tests)
import Test.ThreadNet.Praos qualified (tests)
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
    [ Test.Consensus.Ledger.Mock.tests
    , Test.Consensus.Ledger.Mock.LedgerTables.tests
    , Test.ThreadNet.BFT.tests
    , Test.ThreadNet.LeaderSchedule.tests
    , Test.ThreadNet.PBFT.tests
    , Test.ThreadNet.Praos.tests
    ]
