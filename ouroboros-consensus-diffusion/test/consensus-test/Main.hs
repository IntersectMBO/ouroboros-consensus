module Main (main) where

import Test.Consensus.GSM qualified (tests)
import Test.Consensus.Genesis.Tests qualified (tests)
import Test.Consensus.HardFork.Combinator qualified (tests)
import Test.Consensus.Node qualified (tests)
import Test.Consensus.PeerSimulator.Tests qualified (tests)
import Test.Consensus.PointSchedule.Shrinking.Tests qualified (tests)
import Test.Consensus.PointSchedule.Tests qualified (tests)
import Test.Tasty
import Test.Util.TestEnv
  ( defaultMainWithTestEnv
  , defaultTestEnvConfig
  )

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests

tests :: TestTree
tests =
  testGroup
    "ouroboros-consensus"
    [ Test.Consensus.Node.tests
    , testGroup
        "HardFork"
        [ testGroup
            "Combinator"
            [ Test.Consensus.HardFork.Combinator.tests
            ]
        ]
    , Test.Consensus.Genesis.Tests.tests
    , testGroup "GSM" Test.Consensus.GSM.tests
    , Test.Consensus.PeerSimulator.Tests.tests
    , Test.Consensus.PointSchedule.Shrinking.Tests.tests
    , Test.Consensus.PointSchedule.Tests.tests
    ]
