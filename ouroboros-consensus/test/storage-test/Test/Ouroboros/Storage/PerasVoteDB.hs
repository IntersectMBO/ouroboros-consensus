module Test.Ouroboros.Storage.PerasVoteDB (tests) where

import qualified Test.Ouroboros.Storage.PerasVoteDB.StateMachine as StateMachine
import Test.Tasty (TestTree, testGroup)

--
-- The list of all PerasVoteDB tests
--

tests :: TestTree
tests =
  testGroup
    "PerasVoteDB"
    [ StateMachine.tests
    ]
