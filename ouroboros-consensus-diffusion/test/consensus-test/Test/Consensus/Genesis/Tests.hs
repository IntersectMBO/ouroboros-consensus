module Test.Consensus.Genesis.Tests (tests) where

import qualified Test.Consensus.Genesis.Tests.LongRangeAttack as LongRangeAttack
import qualified Test.Consensus.Genesis.Tests.Uniform as Uniform
import           Test.Tasty

tests :: TestTree
tests = testGroup "Genesis tests"
    [ LongRangeAttack.tests
    , Uniform.tests
    ]
