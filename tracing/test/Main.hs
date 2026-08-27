module Main (main) where

import qualified Test.Consensus.Tracing.Golden as Golden
import qualified Test.Consensus.Tracing.MetaTrace as MetaTrace
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tracing"
  [ MetaTrace.tests
  , Golden.tests
  ]
