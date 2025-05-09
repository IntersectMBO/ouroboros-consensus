module Main (main) where

import Test.Consensus.Shelley.Coherence qualified (tests)
import Test.Consensus.Shelley.Golden qualified (tests)
import Test.Consensus.Shelley.LedgerTables qualified (tests)
import Test.Consensus.Shelley.Serialisation qualified (tests)
import Test.Consensus.Shelley.SupportedNetworkProtocolVersion qualified (tests)
import Test.Tasty
import Test.ThreadNet.Shelley qualified (tests)
import Test.Util.TestEnv
  ( defaultMainWithTestEnv
  , defaultTestEnvConfig
  )

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests

tests :: TestTree
tests =
  testGroup
    "shelley"
    [ Test.Consensus.Shelley.Coherence.tests
    , Test.Consensus.Shelley.Golden.tests
    , Test.Consensus.Shelley.LedgerTables.tests
    , Test.Consensus.Shelley.Serialisation.tests
    , Test.Consensus.Shelley.SupportedNetworkProtocolVersion.tests
    , Test.ThreadNet.Shelley.tests
    ]
