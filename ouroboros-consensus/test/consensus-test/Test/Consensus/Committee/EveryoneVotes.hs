module Test.Consensus.Committee.EveryoneVotes (tests) where

import qualified Test.Consensus.Committee.EveryoneVotes.Tests as Impl
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "EveryoneVotes"
    [ Impl.tests
    ]
