{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.SyncingTest (tests) where

-- Test that the current code can't sync safely (i.e. is vulnerable to naive
-- long-range attacks) when an adversary is sending blocks quickly.
--
--  * Two ChainSync clients
--  * Mocked chain selection logic (longest chain rule)

import           Cardano.Crypto.DSIGN (SignKeyDSIGN (..), VerKeyDSIGN (..))
import           Cardano.Slotting.Time (SlotLength, slotLengthFromSec)
import           Control.Monad
import           Control.Monad.IOSim (runSimOrThrow)
import           Control.Tracer
import           Data.Function ((&))
import           Data.Functor
import           Data.List (scanl')
import qualified Data.Map.Strict as Map
import           Data.Monoid (First (..))
import           Network.TypedProtocol.Channel (createConnectedChannels)
import           Network.TypedProtocol.Driver.Simple
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.STM (Fingerprint (..),
                     WithFingerprint (..))
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (Tip (..))
import           Ouroboros.Network.ControlMessage (ControlMessage (..))
import           Ouroboros.Network.Mock.Chain (Chain)
import qualified Ouroboros.Network.Mock.Chain as Chain
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
                     (chainSyncClientPeerPipelined)
import           Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSyncId)
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
                     (pipelineDecisionLowHighMark)
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Test.Consensus.MiniProtocol.ChainSync.Client
                     (computeHeaderStateHistory, computePastLedger)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.TestBlock

tests :: TestTree
tests = testGroup "SyncingTest"
    [ testProperty "basic" $ runSimOrThrow $ runTest exampleTestSetup
    ]
  where
    exampleTestSetup = TestSetup {
          secParam      = SecurityParam 3
        , genesisWindow = GenesisWindow 5
        , goodChain     =
            mkTestChain $ replicate 6 successorBlock
        , badChain      =
            mkTestChain $
                 [ forkBlock . incSlot . successorBlock
                 , forkBlock . incSlot . successorBlock
                 , forkBlock . incSlot . successorBlock
                 ]
              <> replicate 10 successorBlock
        }
     where
       incSlot :: TestBlock -> TestBlock
       incSlot b = b { tbSlot = tbSlot b + 1 }

       mkTestChain :: [TestBlock -> TestBlock] -> AnchoredFragment TestBlock
       mkTestChain =
         AF.fromOldestFirst AF.AnchorGenesis . scanl' (&) (firstBlock 0)

newtype GenesisWindow = GenesisWindow { getGenesisWindow :: SlotNo }
  deriving stock (Show)

data TestSetup = TestSetup {
    secParam      :: SecurityParam
  , genesisWindow :: GenesisWindow
  , goodChain     :: AnchoredFragment TestBlock
    -- has to be less dense than goodChain in the genesisWindow
  , badChain      :: AnchoredFragment TestBlock
  }
  deriving stock (Show)

runTest ::
     forall m. IOLike m
  => TestSetup
  -> m Property
runTest TestSetup{secParam, goodChain, badChain} = do
    varGoodCandidate <- uncheckedNewTVarM $ AF.Empty AF.AnchorGenesis
    varBadCandidate  <- uncheckedNewTVarM $ AF.Empty AF.AnchorGenesis
    let chainDbView = mkChainDbView varGoodCandidate varBadCandidate
        runChainSync client server = do
          runConnectedPeersPipelined
            createConnectedChannels
            nullTracer
            codecChainSyncId
            (chainSyncClientPeerPipelined client)
            (chainSyncServerPeer server)

        runClients = race (threadDelay 10) $
            runChainSync
              (theChainSyncClient chainDbView varGoodCandidate)
              (boringChainSyncServer (threadDelay 0.1) goodChain)
          `concurrently`
            runChainSync
              (theChainSyncClient chainDbView varBadCandidate)
              (boringChainSyncServer (threadDelay 0.05) badChain)

    res :: Either ChainSyncClientException () <- try (void runClients)

    finalGoodCandidate <- readTVarIO varGoodCandidate
    finalBadCandidate  <- readTVarIO varBadCandidate
    pure
      $ counterexample ("result: " <> show res)
      $ counterexample ("final good candidate: " <> condense finalGoodCandidate)
      $ counterexample ("final bad candidate: " <> condense finalGoodCandidate)
      $ conjoin [
          property $ res == Right ()
        , property $ not $ forksWithinK finalGoodCandidate finalBadCandidate
        ]
  where
    SecurityParam k = secParam

    forksWithinK
      :: AnchoredFragment (Header TestBlock)  -- ^ Our chain
      -> AnchoredFragment (Header TestBlock)  -- ^ Their chain
      -> Bool
    forksWithinK ourChain theirChain = case AF.intersect ourChain theirChain of
      Nothing -> False
      Just (_ourPrefix, _theirPrefix, ourSuffix, _theirSuffix) ->
        fromIntegral (AF.length ourSuffix) <= k

    mkChainDbView ::
         StrictTVar m (AnchoredFragment (Header TestBlock))
      -> StrictTVar m (AnchoredFragment (Header TestBlock))
      -> ChainDbView m TestBlock
    mkChainDbView varGoodCandidate varBadCandidate = ChainDbView {
          getCurrentChain       = AF.anchorNewest k <$> getCurFrag
        , getHeaderStateHistory =
            computeHeaderStateHistory nodeCfg <$> getFullChain
        , getPastLedger         = \pt ->
            computePastLedger nodeCfg pt <$> getFullChain
        , getIsInvalidBlock     =
            pure $ WithFingerprint (pure Nothing) (Fingerprint 0)
        }
      where
        getCurFrag :: STM m (AnchoredFragment (Header TestBlock))
        getCurFrag = do
            goodCand <- readTVar varGoodCandidate
            badCand  <- readTVar varBadCandidate
            pure $
              if AF.headBlockNo goodCand > AF.headBlockNo badCand
              then goodCand
              else badCand

        getFullChain :: STM m (Chain TestBlock)
        getFullChain = do
            curFrag <- getCurFrag
            let goodOrBad =
                  if castPoint (AF.headPoint curFrag) `AF.withinFragmentBounds` goodChain
                  then goodChain
                  else badChain
                curFragTipBlockNo =
                  fromIntegral . unBlockNo . withOrigin 0 id $ AF.headBlockNo curFrag
            pure
              . Chain.fromOldestFirst
              . take curFragTipBlockNo
              . AF.toOldestFirst
              $ goodOrBad

    theChainSyncClient chainDbView varCandidate =
      chainSyncClient
        (pipelineDecisionLowHighMark 10 20)
        nullTracer
        nodeCfg
        chainDbView
        maxBound
        (return Continue)
        nullTracer
        varCandidate

    boringChainSyncServer ::
         m ()
      -> AnchoredFragment TestBlock
      -> ChainSyncServer (Header TestBlock) (Point TestBlock) (Tip TestBlock) m ()
    boringChainSyncServer delay fullFrag = go fullFrag
      where
        go :: AnchoredFragment TestBlock -> ChainSyncServer (Header TestBlock) (Point TestBlock) (Tip TestBlock) m ()
        go frag = ChainSyncServer $ pure ServerStIdle {
              recvMsgRequestNext = case frag of
                  AF.Empty _ -> pure $ Right $ forever $ threadDelay 1000
                  blk AF.:< frag' -> delay $> do
                    Left $ SendMsgRollForward (getHeader blk) tip (go frag')
            , recvMsgFindIntersect = \pts ->
                pure $ case getFirst $ foldMap (First . AF.splitAfterPoint fullFrag) pts of
                  Just (_, frag') -> SendMsgIntersectFound (AF.anchorPoint frag') tip (go frag')
                  Nothing         -> SendMsgIntersectNotFound tip (go frag)
            , recvMsgDoneClient = pure ()
            }

        tip = AF.anchorToTip . AF.headAnchor $ fullFrag

    numCoreNodes = NumCoreNodes 2

    slotLength :: SlotLength
    slotLength = slotLengthFromSec 20

    eraParams :: HardFork.EraParams
    eraParams = HardFork.defaultEraParams secParam slotLength

    nodeCfg :: TopLevelConfig TestBlock
    nodeCfg = TopLevelConfig {
        topLevelConfigProtocol = BftConfig {
            bftParams  = BftParams {
                             bftSecurityParam = secParam
                           , bftNumNodes      = numCoreNodes
                           }
          , bftSignKey = SignKeyMockDSIGN 0
          , bftVerKeys = Map.fromList [
                             (CoreId (CoreNodeId 0), VerKeyMockDSIGN 0)
                           , (CoreId (CoreNodeId 1), VerKeyMockDSIGN 1)
                           ]
          }
      , topLevelConfigLedger  = eraParams
      , topLevelConfigBlock   = TestBlockConfig numCoreNodes
      , topLevelConfigCodec   = TestBlockCodecConfig
      , topLevelConfigStorage = TestBlockStorageConfig
      }
