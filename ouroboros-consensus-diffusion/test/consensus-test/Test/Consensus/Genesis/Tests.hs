module Test.Consensus.Genesis.Tests (tests) where

import qualified Test.Consensus.Genesis.Tests.LongRangeAttack as LongRangeAttack
import           Test.Tasty

tests :: TestTree
tests = testGroup "Genesis tests"
    [ LongRangeAttack.tests
    ]
