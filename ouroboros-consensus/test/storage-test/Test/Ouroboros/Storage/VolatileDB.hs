module Test.Ouroboros.Storage.VolatileDB (tests) where

import Test.Ouroboros.Storage.VolatileDB.StateMachine qualified as StateMachine
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "VolatileDB"
    [ StateMachine.tests
    ]
