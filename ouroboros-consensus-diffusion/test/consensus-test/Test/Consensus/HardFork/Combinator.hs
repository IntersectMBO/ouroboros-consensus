module Test.Consensus.HardFork.Combinator (tests) where

import qualified Test.Consensus.HardFork.Combinator.Convergence as Convergence
import qualified Test.Consensus.HardFork.Combinator.Forge as Forge
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Consensus"
    [ Convergence.tests
    , Forge.tests
    ]
