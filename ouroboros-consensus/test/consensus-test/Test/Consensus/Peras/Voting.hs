-- | Test that the Peras voting rules can correctly decide when to vote.
module Test.Consensus.Peras.Voting (tests) where

import qualified Test.Consensus.Peras.Voting.Smoke
import Test.Tasty (TestTree, testGroup)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests =
  testGroup
    "Voting"
    [ Test.Consensus.Peras.Voting.Smoke.tests
    ]
