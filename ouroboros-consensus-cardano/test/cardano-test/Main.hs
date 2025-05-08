module Main (main) where

import           System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import qualified Test.Consensus.Cardano.DiffusionPipelining
import qualified Test.Consensus.Cardano.Golden
import qualified Test.Consensus.Cardano.MiniProtocol.LocalTxSubmission.Server
import qualified Test.Consensus.Cardano.Serialisation (tests)
import qualified Test.Consensus.Cardano.Show ()
import qualified Test.Consensus.Cardano.SupportedNetworkProtocolVersion
import qualified Test.Consensus.Cardano.SupportsSanityCheck
import qualified Test.Consensus.Cardano.Translation (tests)
import           Test.Tasty
import qualified Test.ThreadNet.AllegraMary
import qualified Test.ThreadNet.Cardano
import qualified Test.ThreadNet.MaryAlonzo
import qualified Test.ThreadNet.ShelleyAllegra
import           Test.Util.TestEnv (defaultMainWithTestEnv,
                     defaultTestEnvConfig)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  defaultMainWithTestEnv defaultTestEnvConfig tests

tests :: TestTree
tests =
  testGroup "cardano"
  [ Test.Consensus.Cardano.DiffusionPipelining.tests
  , Test.Consensus.Cardano.Golden.tests
  , Test.Consensus.Cardano.Serialisation.tests
  , Test.Consensus.Cardano.SupportedNetworkProtocolVersion.tests
  , Test.Consensus.Cardano.SupportsSanityCheck.tests
  , Test.ThreadNet.AllegraMary.tests
  , Test.ThreadNet.Cardano.tests
  , Test.ThreadNet.MaryAlonzo.tests
  , Test.ThreadNet.ShelleyAllegra.tests
  , Test.Consensus.Cardano.MiniProtocol.LocalTxSubmission.Server.tests
  , Test.Consensus.Cardano.Translation.tests
  ]
