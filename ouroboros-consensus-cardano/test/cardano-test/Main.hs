module Main (main) where

import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import Test.Consensus.Cardano.DiffusionPipelining qualified
import Test.Consensus.Cardano.Golden qualified
import Test.Consensus.Cardano.MiniProtocol.LocalTxSubmission.Server qualified
import Test.Consensus.Cardano.Serialisation qualified (tests)
import Test.Consensus.Cardano.Show qualified ()
import Test.Consensus.Cardano.SupportedNetworkProtocolVersion qualified
import Test.Consensus.Cardano.SupportsSanityCheck qualified
import Test.Consensus.Cardano.Translation qualified (tests)
import Test.Tasty
import Test.ThreadNet.AllegraMary qualified
import Test.ThreadNet.Cardano qualified
import Test.ThreadNet.MaryAlonzo qualified
import Test.ThreadNet.ShelleyAllegra qualified
import Test.Util.TestEnv
  ( defaultMainWithTestEnv
  , defaultTestEnvConfig
  )

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  defaultMainWithTestEnv defaultTestEnvConfig tests

tests :: TestTree
tests =
  testGroup
    "cardano"
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
