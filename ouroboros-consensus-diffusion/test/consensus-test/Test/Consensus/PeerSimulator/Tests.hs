module Test.Consensus.PeerSimulator.Tests (tests) where

import qualified Test.Consensus.PeerSimulator.Tests.Rollback as Rollback
import qualified Test.Consensus.PeerSimulator.Tests.Timeouts as Timeouts
import           Test.Tasty

tests :: TestTree
tests = testGroup "PeerSimulator" [
    Rollback.tests,
    Timeouts.tests
  ]
