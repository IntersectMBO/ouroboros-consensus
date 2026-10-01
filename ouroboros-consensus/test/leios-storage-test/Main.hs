module Main (main) where

import qualified Test.Ouroboros.Storage.LeiosDB
import Test.Tasty
import Test.Util.TestEnv

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests

tests :: TestTree
tests =
  testGroup
    "leios-storage"
    [ Test.Ouroboros.Storage.LeiosDB.tests
    ]
