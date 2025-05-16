module Main (main) where

import Test.Consensus.Byron.Golden qualified (tests)
import Test.Consensus.Byron.LedgerTables qualified (tests)
import Test.Consensus.Byron.Serialisation qualified (tests)
import Test.Tasty
import Test.ThreadNet.Byron qualified (tests)
import Test.ThreadNet.DualByron qualified (tests)
import Test.Util.TestEnv
  ( defaultMainWithTestEnv
  , defaultTestEnvConfig
  )

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests

tests :: TestTree
tests =
  testGroup
    "byron"
    [ Test.Consensus.Byron.Golden.tests
    , Test.Consensus.Byron.LedgerTables.tests
    , Test.Consensus.Byron.Serialisation.tests
    , Test.ThreadNet.Byron.tests
    , Test.ThreadNet.DualByron.tests
    ]
