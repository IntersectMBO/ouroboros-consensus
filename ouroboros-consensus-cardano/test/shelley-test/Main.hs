module Main (main) where

import qualified Test.Consensus.Shelley.Coherence (tests)
import qualified Test.Consensus.Shelley.Golden (tests)
import qualified Test.Consensus.Shelley.LedgerTables (tests)
import qualified Test.Consensus.Shelley.Serialisation (tests)
import qualified Test.Consensus.Shelley.SupportedNetworkProtocolVersion (tests)
import Test.Tasty
import qualified Test.ThreadNet.Shelley (tests)
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
