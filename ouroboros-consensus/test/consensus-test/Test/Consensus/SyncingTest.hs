{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.SyncingTest where

-- Test that the current code can't sync safely (i.e. is vulnerable to naive
-- long-range attacks) when an adversary is sending blocks quickly.
--
--  * Two ChainSync clients
--  * Mocked chain selection logic (longest chain rule)

import           Cardano.Crypto.DSIGN (SignKeyDSIGN (..), VerKeyDSIGN (..))
import           Cardano.Slotting.Time (SlotLength, slotLengthFromSec)
import           Control.Monad
import           Control.Tracer
import qualified Data.Map.Strict as Map
import           Data.Void
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SecurityParam
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.STM (Fingerprint (..),
                     WithFingerprint (..))
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (Tip (..), tipFromHeader)
import           Ouroboros.Network.ControlMessage (ControlMessage (..))
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
                     (pipelineDecisionLowHighMark)
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Test.Util.TestBlock

newtype GenesisWindow = GenesisWindow { getGenesisWindow :: SlotNo }

data TestSetup = TestSetup {
    secParam      :: SecurityParam
  , genesisWindow :: GenesisWindow
  , goodChain     :: AnchoredFragment TestBlock
    -- has to be less dense than goodChain in the genesisWindow
  , badChain      :: AnchoredFragment TestBlock
  }

runTest ::
     forall m. IOLike m
  => TestSetup
  -> m ()
runTest TestSetup{secParam} =
    -- remaining stuff to do:
    --
    --  - run two ChainSync clients against the boringChainSyncServer,
    --    with a large threadDelay for the "good" server.
    --
    --  - key function: runConnectedPeersPipelined
    --
    --  - things to check in the end (signalling "failure", i.e. demonstrating
    --    that the Praos longest chain rule is insufficient):
    --
    --     - the final getCurrentChain forks off by more than k from the good
    --       chain
    --
    --     - optional: the good ChainSync client disconnected (as the fork was
    --       too deep)
    undefined
  where
    SecurityParam k = secParam

    chainDbView ::
         StrictTVar m (AnchoredFragment (Header TestBlock))
      -> StrictTVar m (AnchoredFragment (Header TestBlock))
      -> ChainDbView m TestBlock
    chainDbView varGoodCandidate varBadCandidate = ChainDbView {
          getCurrentChain       = getCurChain
          -- These two can be implemented like in
          -- Test.Consensus.MiniProtocol.ChainSync.Client.
        , getHeaderStateHistory = undefined
        , getPastLedger         = undefined
        , getIsInvalidBlock     =
            pure $ WithFingerprint (pure Nothing) (Fingerprint 0)
        }
      where
        getCurChain = do
            goodCand <- readTVar varGoodCandidate
            badCand  <- readTVar varBadCandidate
            pure $ AF.anchorNewest k $
              if AF.headBlockNo goodCand > AF.headBlockNo badCand
              then goodCand
              else badCand

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
    boringChainSyncServer before fullFrag = ChainSyncServer $ do
        before
        go fullFrag
      where
        go :: AnchoredFragment TestBlock -> m (ServerStIdle (Header TestBlock) (Point TestBlock) (Tip TestBlock) m ())
        go frag = pure ServerStIdle {
              recvMsgRequestNext = case frag of
                  AF.Empty _ -> pure $ Right $ forever $ threadDelay 1000
                  blk AF.:< frag' ->
                    pure $ Left $ SendMsgRollForward (getHeader blk) tip (ChainSyncServer $ go frag')
            , recvMsgFindIntersect = \_ -> pure $
                SendMsgIntersectFound GenesisPoint tip (ChainSyncServer $ go frag)
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
