module Test.Consensus.Storage.VolatileDB (tests) where

import qualified Test.Consensus.Storage.VolatileDB.StateMachine as StateMachine
import           Test.Tasty (TestTree, testGroup)


tests :: TestTree
tests = testGroup "VolatileDB"
    [ StateMachine.tests
    ]
