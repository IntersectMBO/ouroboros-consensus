module Main (main) where

import Test.Consensus.Protocol.Praos.SelectView qualified
import Test.Tasty
import Test.Util.TestEnv

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests

tests :: TestTree
tests =
  testGroup
    "protocol"
    [ Test.Consensus.Protocol.Praos.SelectView.tests
    ]
