module Main (main) where

import qualified Test.Consensus.Protocol.Ledger.HotKey
import qualified Test.Consensus.Protocol.Praos.SelectView
import Test.Tasty
import Test.Util.TestEnv

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests

tests :: TestTree
tests =
  testGroup
    "protocol"
    [ Test.Consensus.Protocol.Ledger.HotKey.tests
    , Test.Consensus.Protocol.Praos.SelectView.tests
    ]
