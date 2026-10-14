module Main (main) where

import qualified Test.Consensus.Protocol.Praos.SelectView
import qualified Test.Consensus.Protocol.Praos2
import Test.Tasty
import Test.Util.TestEnv

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests

tests :: TestTree
tests =
  testGroup
    "protocol"
    [ Test.Consensus.Protocol.Praos.SelectView.tests
    , Test.Consensus.Protocol.Praos2.tests
    ]
