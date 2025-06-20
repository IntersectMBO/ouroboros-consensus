module Main (main) where

import Test.Tasty
import qualified Test.ThreadNet.Util.Tests (tests)
import Test.Util.TestEnv
  ( defaultMainWithTestEnv
  , defaultTestEnvConfig
  )

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests

tests :: TestTree
tests =
  testGroup
    "test-infra"
    [ Test.ThreadNet.Util.Tests.tests
    ]
