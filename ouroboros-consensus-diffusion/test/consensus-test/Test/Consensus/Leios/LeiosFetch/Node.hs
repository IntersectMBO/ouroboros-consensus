{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- | Tests of the node's LeiosFetch client and server, over real connections,
-- against peers run by the Leios node-vs-environment tests.
module Test.Consensus.Leios.LeiosFetch.Node (tests) where

import Cardano.Network.NodeToNode (defaultMiniProtocolParameters)
import Cardano.Network.NodeToNode.Version (NodeToNodeVersion (..))
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI (timeout)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Control.Tracer (nullTracer)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map.Strict as Map
import Network.TypedProtocol.Codec (PeerRole (..))
import Network.TypedProtocol.Core (IsPipelined (..), ReflRelativeAgency (..))
import Network.TypedProtocol.Peer (Peer (..))
import Ouroboros.Consensus.Leios.Types (LeiosEb, LeiosPoint (..), LeiosTx)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type
import Ouroboros.Consensus.Network.LeiosFetch (leiosFetchMiniProtocolNum)
import qualified Ouroboros.Consensus.Network.NodeToNode as NTN
import Ouroboros.Network.Channel (Channel)
import Ouroboros.Network.Driver.Simple (runPeer)
import Ouroboros.Network.Mux
  ( MiniProtocol (..)
  , TemperatureBundle (..)
  , WithProtocolTemperature (..)
  )
import Test.Consensus.Leios.Environment
import Test.Consensus.Leios.NodeUnderTest
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "node vs environment"
    [ testCase "the node's server ends when the peer sends MsgDone" $
        runSimOrThrow (timeout 100 (nodeServerAgainst doneClient)) @?= Just ServerEnded
    , testCase "the node runs LeiosFetch from NodeToNodeV_16 on" $
        runSimOrThrow (timeout 100 (nodeRunsLeiosFetch NodeToNodeV_16)) @?= Just (True, True)
    , testCase "the node does not run LeiosFetch before NodeToNodeV_16" $
        runSimOrThrow (timeout 100 (nodeRunsLeiosFetch NodeToNodeV_15)) @?= Just (False, False)
    ]

{-------------------------------------------------------------------------------
  The node's server
-------------------------------------------------------------------------------}

data ServerOutcome = ServerEnded | ServerFailed String
  deriving (Eq, Show)

type EnvClient s =
  Peer (LeiosFetch LeiosPoint LeiosEb LeiosTx) AsClient NonPipelined StIdle (IOSim s) ()

-- | Run the node's server against a peer that runs this client.
nodeServerAgainst :: EnvClient s -> IOSim s ServerOutcome
nodeServerAgainst client = withNode $ \nut -> do
  let envPeer =
        EnvPeer
          { epLeiosNotifyServer = \_chan -> atomically retry
          , epLeiosNotifyClient = Nothing
          , epLeiosFetchServer = refusingLeiosFetchServer nut
          , epLeiosFetchClient = Just $ runEnvClient nut client
          }
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \conn -> do
    server <- nodeLeiosFetchServer conn
    atomically server >>= \case
      Right () -> pure ServerEnded
      Left e -> pure $ ServerFailed (show e)

runEnvClient :: NodeUnderTest (IOSim s) -> EnvClient s -> Channel (IOSim s) ByteString -> IOSim s ()
runEnvClient nut client chan = fst <$> runPeer nullTracer (envLeiosFetchCodec nut) chan client

doneClient :: EnvClient s
doneClient = Yield ReflClientAgency MsgDone $ Done ReflNobodyAgency ()

{-------------------------------------------------------------------------------
  Registration
-------------------------------------------------------------------------------}

-- | Whether the node's initiator bundle, and its initiator-and-responder bundle,
-- include LeiosFetch among their hot mini-protocols for this version.
nodeRunsLeiosFetch :: NodeToNodeVersion -> IOSim s (Bool, Bool)
nodeRunsLeiosFetch ntnVersion = withNode $ \nut ->
  pure
    ( runsLeiosFetch $
        NTN.initiator mempty defaultMiniProtocolParameters ntnVersion versionData (nutApps nut ())
    , runsLeiosFetch $
        NTN.initiatorAndResponder
          mempty
          defaultMiniProtocolParameters
          ntnVersion
          versionData
          (nutApps nut ())
    )
 where
  versionData = nodeVersionData ntnVersion
  runsLeiosFetch bundle = case withHot bundle of
    WithHot protocols -> leiosFetchMiniProtocolNum `elem` map miniProtocolNum protocols

{-------------------------------------------------------------------------------
  The node, connected to peers run by the test
-------------------------------------------------------------------------------}

-- | The node's LeiosFetch server on the connection.
nodeLeiosFetchServer ::
  NodeVsEnvConnection (IOSim s) -> IOSim s (STM (IOSim s) (Either SomeException ()))
nodeLeiosFetchServer conn =
  maybe (throwIO $ userError "the node does not run a LeiosFetch server") pure $
    Map.lookup leiosFetchMiniProtocolNum (connNodeResponders conn)
