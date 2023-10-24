module Main (main) where

import qualified Test.Consensus.Genesis.Tests (tests)
import qualified Test.Consensus.GSM (tests)
import qualified Test.Consensus.HardFork.Combinator (tests)
import qualified Test.Consensus.Node (tests)
import qualified Test.Consensus.PeerSimulator.Tests (tests)
import           Test.Tasty
import           Test.Util.TestEnv (defaultMainWithTestEnv,
                     defaultTestEnvConfig)

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests

tests :: TestTree
tests =
  testGroup "ouroboros-consensus"
  [ Test.Consensus.Node.tests
  , testGroup "HardFork" [
        testGroup "Combinator" [
            Test.Consensus.HardFork.Combinator.tests
          ]
      ]
  , Test.Consensus.Genesis.Tests.tests
  , testGroup "GSM" Test.Consensus.GSM.tests
  , Test.Consensus.PeerSimulator.Tests.tests
  ]
