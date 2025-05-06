{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Narrow tests for ChainSync Jumping
module Test.Consensus.MiniProtocol.ChainSync.CSJ (tests) where

import qualified Control.Concurrent.Class.MonadSTM.Strict.TVar as TVar
import           Control.Monad (void)
import           Control.Monad.Class.MonadTimer (MonadTimer)
import           Control.Monad.IOSim (runSim)
import           Control.ResourceRegistry
import           Control.Tracer (nullTracer)
import           Data.Typeable
import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Driver.Simple
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HeaderStateHistory as HeaderStateHistory
import           Ouroboros.Consensus.Ledger.Tables.Utils (forgetLedgerTables)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (CSJConfig (..), CSJEnabledConfig (..), ChainDbView (..),
                     ChainSyncLoPBucketConfig (..), ChainSyncStateView (..),
                     ConfigEnv (..), Consensus, DynamicEnv (..),
                     bracketChainSyncClient, chainSyncClient,
                     newChainSyncClientHandleCollection)
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.HistoricityCheck as HistoricityCheck
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck as InFutureCheck
import qualified Ouroboros.Consensus.Node.GsmState as GSM
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (NodeToNodeVersion)
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.STM (Fingerprint (..),
                     WithFingerprint (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.ControlMessage (ControlMessage (..))
import qualified Ouroboros.Network.Mock.Chain as MockChain
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
import           Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSyncId)
import           Ouroboros.Network.Protocol.ChainSync.Examples
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
                     (pipelineDecisionLowHighMark)
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Narrow CSJ"
    [ testProperty "CaughtUp" prop_CaughtUpCsj
    ]

{-------------------------------------------------------------------------------
  Main property
-------------------------------------------------------------------------------}

data TestSetup =
    -- | This test is very simple for now, since it's so far merely a
    -- regression test of a simple property we're very surprised made it past
    -- code review.
    TestSetup
  deriving (Read, Show)

instance Arbitrary TestSetup where
  arbitrary = pure TestSetup

-- | When the node is CaughtUp, CSJ should be disabled.
--
-- The test checks for that by testing whether two upstream peers joining
-- causes them both to promptly receive @MsgRequestNext@. If CSJ were enabled,
-- then instead only the first peer to join would promptly receive
-- @MsgRequestNext@.
prop_CaughtUpCsj :: TestSetup -> Property
prop_CaughtUpCsj testSetup =
    case runSim $ runTest testSetup of

      Left exn ->
          counterexample ("`runTest' threw an exception: " <> show exn)
        $ property False

      Right results ->
          counterexample "At least one peer did not receive MsgRequestNext"
        $ results === (HasReceived, HasReceived)

data WhetherReceivedMsgNextRequest =
      HasNotYetReceived
    |
      HasReceived
  deriving (Eq, Show)

runTest :: forall m.
     (
       IOLike m
     ,
       MonadTimer m
     )
  => TestSetup
  -> m (WhetherReceivedMsgNextRequest, WhetherReceivedMsgNextRequest)
runTest TestSetup = withRegistry $ \registry -> do

    -- The "Ouroboros.Consensus.NodeKernel" does not do anything more than this
    -- in order to "initialize" CSJ.
    varHandles <- atomically newChainSyncClientHandleCollection

    let chainDbView :: ChainDbView m TestBlock
        chainDbView = ChainDbView {
            getCurrentChain       = pure $ AF.Empty AF.AnchorGenesis
          ,
            getHeaderStateHistory =
                pure
              $ HeaderStateHistory.fromChain
                    topLevelCfg
                    testInitExtLedger
                    MockChain.Genesis
          ,
            getPastLedger         = pure . \case
                GenesisPoint -> Just $ forgetLedgerTables testInitExtLedger
                BlockPoint{} -> Nothing
          ,
            getIsInvalidBlock     =
                pure $ WithFingerprint (\_hash -> Nothing) (Fingerprint 0)
          }

        version :: NodeToNodeVersion
        version = maxBound

        lopBucketConfig :: ChainSyncLoPBucketConfig
        lopBucketConfig = ChainSyncLoPBucketDisabled

        csjConfig :: CSJEnabledConfig
        csjConfig = CSJEnabledConfig { csjcJumpSize = SlotNo 10000 }

        diffusionPipelining :: DiffusionPipeliningSupport
        diffusionPipelining = DiffusionPipeliningOn

        headerInFutureCheck ::
            InFutureCheck.SomeHeaderInFutureCheck m TestBlock
        headerInFutureCheck =
            InFutureCheck.SomeHeaderInFutureCheck
          $ InFutureCheck.HeaderInFutureCheck {
                InFutureCheck.proxyArrival        = Proxy :: Proxy ()
              ,
                InFutureCheck.recordHeaderArrival = \_ -> pure ()
              ,
                InFutureCheck.judgeHeaderArrival  =
                    \_lcfg _lstate () -> pure ()
              ,
                InFutureCheck.handleHeaderArrival =
                    \() -> pure $ pure $ RelativeTime 0
              }

        mkClient ::
             ChainSyncStateView m TestBlock
          -> Consensus ChainSyncClientPipelined TestBlock m
        mkClient csv =
            let ChainSyncStateView {
                    csvSetCandidate
                  ,
                    csvSetLatestSlot
                  ,
                    csvIdling
                  ,
                    csvLoPBucket
                  ,
                    csvJumping
                  } = csv
            in
            chainSyncClient
                ConfigEnv {
                    chainDbView
                  ,
                    cfg                           = topLevelCfg
                  ,
                    tracer                        = nullTracer
                  ,
                    someHeaderInFutureCheck       = headerInFutureCheck
                  ,
                    historicityCheck              = HistoricityCheck.noCheck
                  ,
                    mkPipelineDecision0           = pipelineDecisionLowHighMark 10 20
                  ,
                    getDiffusionPipeliningSupport = diffusionPipelining
                  }
                DynamicEnv {
                    version
                  ,
                    controlMessageSTM   = return Continue
                  ,
                    headerMetricsTracer = nullTracer
                  ,
                    setCandidate        = csvSetCandidate
                  ,
                    idling              = csvIdling
                  ,
                    loPBucket           = csvLoPBucket
                  ,
                    setLatestSlot       = csvSetLatestSlot
                  ,
                    jumping             = csvJumping
                  }

        bracketedClient ::
             CoreNodeId
          -> (Consensus ChainSyncClientPipelined TestBlock m -> m a)
          -> m a
        bracketedClient peer k =
            bracketChainSyncClient
                nullTracer
                nullTracer
                chainDbView
                varHandles
                (pure GSM.CaughtUp)
                peer
                version
                lopBucketConfig
                (CSJEnabled csjConfig)
                diffusionPipelining
                (k . mkClient)

        spawnConnection ::
             CoreNodeId
          -> m (TVar.StrictTVar m WhetherReceivedMsgNextRequest)
        spawnConnection peer = do
            var <- TVar.newTVarIO HasNotYetReceived
            (clientChannel, serverChannel) <- createConnectedChannels
            void $ forkLinkedThread registry ("client " <> show peer) $ do
                bracketedClient peer $ \client -> do
                    runPipelinedPeer
                        nullTracer
                        codecChainSyncId
                        clientChannel
                        (chainSyncClientPeerPipelined client)
            void $ forkLinkedThread registry ("server " <> show peer) $ do
                runPeer
                    nullTracer
                    codecChainSyncId
                    serverChannel
              $ chainSyncServerPeer
              $ server
              $ atomically (TVar.writeTVar var HasReceived)
            pure var

    var1 <- spawnConnection $ CoreNodeId 1
    var2 <- spawnConnection $ CoreNodeId 2

    threadDelay testDuration

    atomically $ (,) <$> TVar.readTVar var1 <*> TVar.readTVar var2

-- | How long the test runs for
--
-- The only time-sensitive thing in this test is the 'Exhausted' exception in
-- 'server', so as long as that happens after this duration, time should be
-- irrelevant.
testDuration :: Num a => a
testDuration = 100

server ::
     IOLike m
  => m ()
     -- ^ action to perform on the first @MsgRequestNext@, after which this
     -- peer becomes unresponsive
  -> ChainSyncServer (Header TestBlock) (Point TestBlock) (Tip TestBlock) m ()
server onFirstMsgRequestNext =
    go
  where
    dummyTip = Tip (SlotNo 1000) (testHashFromList [0]) (BlockNo 1000)
        -- inconsequential for this test

    go = ChainSyncServer $ pure $ ServerStIdle {
        recvMsgRequestNext   = do
            onFirstMsgRequestNext
            threadDelay $ testDuration + 1
            throwIO Exhausted
      ,
        recvMsgFindIntersect = \_points ->
            pure $ SendMsgIntersectFound GenesisPoint dummyTip go
      ,
        recvMsgDoneClient    = throwIO UnexpectedTermination
      }

data TestException =
    -- | The test ran for longer than it expected to; see 'testDuration'
    Exhausted
  |
    -- | A peer received @MsgDone@, which shouldn't happen in this test
    UnexpectedTermination
  deriving (Eq, Show)

instance Exception TestException

-- | This data structure contains a lot of values that are inconsequential for
-- this test, especially since this test doesn't actually involve any blocks
topLevelCfg :: TopLevelConfig TestBlock
topLevelCfg = singleNodeTestConfig
