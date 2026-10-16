module Main (main) where

import qualified Test.Consensus.Tracing.Dijkstra as Dijkstra
import qualified Test.Consensus.Tracing.Forge as Forge
import qualified Test.Consensus.Tracing.Golden as Golden
import qualified Test.Consensus.Tracing.MetaTrace as MetaTrace
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "tracing"
    [ MetaTrace.tests
    , Golden.tests
    , Dijkstra.tests
    , Forge.tests
    ]
