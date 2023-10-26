module Test.Consensus.Genesis.Tests (tests) where

import qualified Test.Consensus.Genesis.Tests.LongRangeAttack as LongRangeAttack
import qualified Test.Consensus.Genesis.Tests.RollbackSpam as RollbackSpam
import           Test.Tasty

tests :: TestTree
tests = testGroup "Genesis tests"
    [ LongRangeAttack.tests
    , RollbackSpam.tests
    ]
