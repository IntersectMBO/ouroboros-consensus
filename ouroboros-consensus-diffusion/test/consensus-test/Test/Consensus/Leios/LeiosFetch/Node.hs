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
import Data.Bits ((.|.))
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Strict as V
import Network.TypedProtocol.Codec (PeerRole (..))
import Network.TypedProtocol.Core (IsPipelined (..), ReflRelativeAgency (..))
import Network.TypedProtocol.Peer (Peer (..))
import Ouroboros.Consensus.Leios.Types
  ( EbHash (..)
  , LeiosEb (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , TxHash (..)
  , encodeLeiosEbSize
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type
import Ouroboros.Consensus.Network.LeiosFetch (leiosFetchMiniProtocolNum)
import qualified Ouroboros.Consensus.Network.NodeToNode as NTN
import Ouroboros.Consensus.Network.NodeToNode.LeiosFetch (ExnLeiosInvalidRequest (..))
import Ouroboros.Consensus.NodeKernel (NodeKernel (..))
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbWriter (..)
  , Promise (..)
  , withWriter
  )
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
    , testCase "the node serves an endorser block it holds" $
        runSimOrThrow (timeout 100 (nodeAnswers (requestBlock heldPoint)))
          @?= Just (Just (ReplyBlock heldEb), ServerEnded)
    , testCase "the node serves the transactions it holds" $
        runSimOrThrow (timeout 100 (nodeAnswers (requestTxs heldPoint (bitmapsNaming [0, 63, 64]))))
          @?= Just (Just (ReplyTxs (map heldTx [0, 63, 64])), ServerEnded)
    , testCase "the node disconnects a client that requests an endorser block it does not hold" $
        runSimOrThrow (timeout 100 (nodeAnswers (requestBlock unknownPoint)))
          @?= Just (Nothing, ServerFailed (show (ExnLeiosUnknownBlockRequested unknownPoint)))
    , testCase "the node disconnects a client that requests transactions it does not hold" $
        -- The first is held, the second is not, and the third is past the end
        -- of the endorser block.
        runSimOrThrow (timeout 100 (nodeAnswers (requestTxs heldPoint (bitmapsNaming [0, 69, 100]))))
          @?= Just (Nothing, ServerFailed (show (ExnLeiosUnknownTxsRequested heldPoint [69, 100])))
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

-- | Run the node's server, with 'heldEb' in its LeiosDB, against a peer that
-- runs this client.
nodeServerAgainst :: EnvClient s -> IOSim s ServerOutcome
nodeServerAgainst client = withNode $ \nut -> do
  holdHeldEb nut
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

data Reply = ReplyBlock LeiosEb | ReplyTxs [LeiosTx]
  deriving (Eq, Show)

-- | The node's reply to the client's one request, if it replied, and how its
-- server ended.
nodeAnswers ::
  (StrictTVar (IOSim s) (Maybe Reply) -> EnvClient s) -> IOSim s (Maybe Reply, ServerOutcome)
nodeAnswers client = do
  replyVar <- newTVarIO Nothing
  outcome <- nodeServerAgainst (client replyVar)
  reply <- readTVarIO replyVar
  pure (reply, outcome)

-- | A client that requests an endorser block, keeps the reply, and sends
-- 'MsgDone'.
requestBlock :: LeiosPoint -> StrictTVar (IOSim s) (Maybe Reply) -> EnvClient s
requestBlock point replyVar =
  Yield ReflClientAgency (MsgLeiosBlockRequest point) $
    Await ReflServerAgency $ \case
      MsgLeiosBlock eb -> Effect $ doneClient <$ atomically (writeTVar replyVar (Just (ReplyBlock eb)))

-- | A client that requests transactions, keeps the reply, and sends 'MsgDone'.
requestTxs :: LeiosPoint -> TxBitmaps -> StrictTVar (IOSim s) (Maybe Reply) -> EnvClient s
requestTxs point bitmaps replyVar =
  Yield ReflClientAgency (MsgLeiosBlockTxsRequest point bitmaps) $
    Await ReflServerAgency $ \case
      MsgLeiosBlockTxs txs ->
        Effect $ doneClient <$ atomically (writeTVar replyVar (Just (ReplyTxs (V.toList txs))))

-- | The bitmaps that name these offsets.
bitmapsNaming :: [Int] -> TxBitmaps
bitmapsNaming offsets =
  Map.toList $
    Map.fromListWith
      (.|.)
      [(fromIntegral (offset `div` 64), Bits.bit (63 - offset `mod` 64)) | offset <- offsets]

{-------------------------------------------------------------------------------
  What the node's LeiosDB holds
-------------------------------------------------------------------------------}

-- | An endorser block of 70 transactions, so that a request for them takes two
-- bitmaps.
heldEb :: LeiosEb
heldEb =
  MkLeiosEb $
    V.fromList [(heldTxHash i, fromIntegral (BS.length (heldTxBytes i))) | i <- [0 .. 69]]

heldPoint :: LeiosPoint
heldPoint = MkLeiosPoint 1 (MkEbHash (BS.replicate 32 1))

unknownPoint :: LeiosPoint
unknownPoint = MkLeiosPoint 1 (MkEbHash (BS.replicate 32 2))

heldTxHash :: Int -> TxHash
heldTxHash i = MkTxHash (BS.replicate 31 0 <> BS.singleton (fromIntegral i))

heldTxBytes :: Int -> BS.ByteString
heldTxBytes i = BS.replicate (10 + i) (fromIntegral i)

heldTx :: Int -> LeiosTx
heldTx = MkLeiosTx . heldTxBytes

-- | Write 'heldEb' and all its transactions but the last.
holdHeldEb :: NodeUnderTest (IOSim s) -> IOSim s ()
holdHeldEb nut = withWriter (getLeiosDB (nutKernel nut)) $ \writer -> do
  await =<< writeEbPoint writer heldPoint (encodeLeiosEbSize heldEb)
  _ <- await =<< writeEbBody writer heldPoint heldEb
  _ <- await =<< writeTxs writer [(heldTxHash i, heldTxBytes i) | i <- [0 .. 68]]
  pure ()

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
