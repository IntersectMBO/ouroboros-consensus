module Test.Consensus.PeerSimulator.Tests (tests) where

import Test.Consensus.PeerSimulator.Tests.LinkedThreads qualified as LinkedThreads
import Test.Consensus.PeerSimulator.Tests.Rollback qualified as Rollback
import Test.Consensus.PeerSimulator.Tests.Timeouts qualified as Timeouts
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "PeerSimulator"
    [ Rollback.tests
    , Timeouts.tests
    , LinkedThreads.tests
    ]
