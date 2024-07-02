module Main (main) where

import qualified Test.Consensus.Storage
import           Test.Tasty
import           Test.Util.TestEnv

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests

tests :: TestTree
tests = testGroup "ouroboros-storage" [
    Test.Consensus.Storage.tests
  ]
