module Main (main) where

import           System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import qualified Test.Consensus.Cardano.ByronCompatibility
import qualified Test.Consensus.Cardano.Golden
import qualified Test.Consensus.Cardano.MiniProtocol.LocalTxSubmission.Server
import qualified Test.Consensus.Cardano.Serialisation
import qualified Test.Consensus.Cardano.SupportedNetworkProtocolVersion
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
  hSetEncoding stdout utf8
  defaultMainWithTestEnv defaultTestEnvConfig tests

tests :: TestTree
tests =
  testGroup "cardano"
  [ Test.Consensus.Cardano.ByronCompatibility.tests
  , Test.Consensus.Cardano.Golden.tests
  , Test.Consensus.Cardano.Serialisation.tests
  , Test.Consensus.Cardano.SupportedNetworkProtocolVersion.tests
  , Test.ThreadNet.AllegraMary.tests
  , Test.ThreadNet.Cardano.tests
  , Test.ThreadNet.MaryAlonzo.tests
  , Test.ThreadNet.ShelleyAllegra.tests
  , Test.Consensus.Cardano.MiniProtocol.LocalTxSubmission.Server.tests
  ]
