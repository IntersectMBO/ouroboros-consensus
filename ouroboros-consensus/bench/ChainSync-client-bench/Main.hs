{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import           Bench.Consensus.ChainSyncClient.Driver (mainWith)
import           Cardano.Crypto.DSIGN.Mock
import           Control.Monad (void)
import           Control.Tracer (contramap, debugTracer, nullTracer)
import           Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Driver.Simple
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Fragment.InFuture (clockSkewInSeconds)
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import qualified Ouroboros.Consensus.HeaderStateHistory as HeaderStateHistory
import qualified Ouroboros.Consensus.HeaderValidation as HV
import qualified Ouroboros.Consensus.Ledger.Extended as Extended
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client as CSClient
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck as InFutureCheck
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
                     (chainSyncServerForFollower)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (NodeToNodeVersion)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.BFT
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (Fingerprint (..),
                     WithFingerprint (..))
import           Ouroboros.Consensus.Util.Time (secondsToNominalDiffTime)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Ouroboros.Network.Block (ChainUpdate (AddBlock, RollBack),
                     Tip (TipGenesis), tipFromHeader)
import           Ouroboros.Network.ControlMessage (ControlMessage (Continue))
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
import           Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSyncId)
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
                     (pipelineDecisionLowHighMark)
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Orphans.IOLike ()
import qualified Test.Util.TestBlock as TB

type B = TB.TestBlock
type H = Header B

main :: IO ()
main = mainWith $ \n -> do
    varCandidate <- newTVarIO $ AF.Empty AF.AnchorGenesis

    varServerTip <- newTVarIO TipGenesis
    follower     <- mkFollower varServerTip

    oneBenchRun
        varCandidate
        varServerTip
        follower
        (fromIntegral n)

{-# INLINE oneBenchRun #-}
oneBenchRun ::
     StrictTVar IO (AnchoredFragment H)
  -> StrictTVar IO (Tip B)
  -> ChainDB.Follower IO B (ChainDB.WithPoint B H)
  -> Int
  -> IO ()
oneBenchRun
    varCandidate
    varServerTip
    follower
    n
  =
    withRegistry $ \registry -> do
        (clientChannel, serverChannel) <- createConnectedChannels
        void
          $ forkLinkedThread registry "ChainSyncServer"
          $ runPeer nullTracer codecChainSyncId serverChannel
          $ chainSyncServerPeer server
        void
          $ forkLinkedThread registry "ChainSyncClient"
          $ void
          $ runPipelinedPeer nullTracer codecChainSyncId clientChannel
          $ chainSyncClientPeerPipelined client

        atomically $ do
            candidate <- readTVar varCandidate
            check $ case pointHash $ AF.headPoint candidate of
                BlockHash (TB.TestHash ne) -> fromIntegral n < NE.head ne
                _                          -> False
  where
    -- This test is designed so that the initial ledger state suffices for
    -- everything the ChainSync client needs to do.
    chainDbView :: CSClient.ChainDbView IO B
    chainDbView = CSClient.ChainDbView {
        CSClient.getCurrentChain       = pure $ AF.Empty AF.AnchorGenesis
      , CSClient.getHeaderStateHistory =
            pure
          $ HeaderStateHistory.HeaderStateHistory
          $ AS.Empty
          $ HV.genesisHeaderState ()
      , CSClient.getIsInvalidBlock     = pure invalidBlock
      , CSClient.getPastLedger         = pure . oracularLedgerDB
      }

    headerInFutureCheck ::
        InFutureCheck.SomeHeaderInFutureCheck IO B
    headerInFutureCheck =
        InFutureCheck.realHeaderInFutureCheck
            (clockSkewInSeconds 0)
            inTheYearOneBillion

    client :: CSClient.Consensus ChainSyncClientPipelined B IO
    client =
        CSClient.chainSyncClient
            CSClient.ConfigEnv {
                CSClient.chainDbView
              , CSClient.cfg                     = topConfig
              , CSClient.tracer                  = nullTracer `asTypeOf` contramap show debugTracer
              , CSClient.someHeaderInFutureCheck = headerInFutureCheck
              , CSClient.mkPipelineDecision0     =
                    pipelineDecisionLowHighMark 10 20
              }
            CSClient.DynamicEnv {
                CSClient.version             = maxBound :: NodeToNodeVersion
              , CSClient.controlMessageSTM   = return Continue
              , CSClient.headerMetricsTracer = nullTracer
              , CSClient.varCandidate
              , CSClient.setTheirTip         = \_ -> pure ()
              }

    server :: ChainSyncServer H (Point B) (Tip B) IO ()
    server =
        chainSyncServerForFollower
            nullTracer
            (readTVar varServerTip)
            follower

-----

-- | No invalid blocks in this benchmark
invalidBlock ::
    WithFingerprint
        (HeaderHash blk -> Maybe (ChainDB.InvalidBlockReason blk))
invalidBlock =
    WithFingerprint isInvalidBlock fp
  where
    isInvalidBlock _hash = Nothing
    fp                   = Fingerprint $ fromIntegral (0 :: Int)

-- | Ignore time in this benchmark
--
-- This is clock fixed at billion years after the start of the chain. That
-- should trivialize the in-future check: no header will be from the future.
inTheYearOneBillion :: SystemTime IO
inTheYearOneBillion = SystemTime {
    systemTimeWait    = pure ()
  , systemTimeCurrent = pure $ RelativeTime $
      secondsToNominalDiffTime $
          86400   -- seconds in a day
        * 365   -- days in a year
        * 1e9
  }

oracularLedgerDB :: Point B -> Maybe (Extended.ExtLedgerState B)
oracularLedgerDB p =
    Just Extended.ExtLedgerState {
        Extended.headerState = HV.HeaderState {
            HV.headerStateTip      = case pointToWithOriginRealPoint p of
                Origin       -> Origin
                NotOrigin rp -> NotOrigin $ HV.AnnTip {
                    HV.annTipSlotNo  = realPointSlot rp
                  , HV.annTipInfo    = realPointHash rp
                  , HV.annTipBlockNo =
                        testBlockHashBlockNo (realPointHash rp)
                  }
          , HV.headerStateChainDep = ()
          }
      , Extended.ledgerState = TB.TestLedger {
            TB.lastAppliedPoint      = p
          , TB.payloadDependentState = ()
          }
    }

-- | A convenient fact about 'TB.TestBlock'
testBlockHashBlockNo :: TB.TestHash -> BlockNo
testBlockHashBlockNo (TB.TestHash ne) = BlockNo $ fromIntegral $ length ne

-----

kInt :: Int
kInt = 5

securityParam :: SecurityParam
securityParam = SecurityParam $ fromIntegral kInt

initialChain :: NE.NonEmpty B
initialChain =
    NE.fromList
  $ take kInt
  $ iterate TB.successorBlock
  $ TB.firstBlock 0

-----

slotLengthInSeconds :: Int
slotLengthInSeconds = 1

slotLength :: SlotLength
slotLength = slotLengthFromSec $ toEnum slotLengthInSeconds

numCoreNodes :: NumCoreNodes
numCoreNodes = NumCoreNodes 2

topConfig :: TopLevelConfig B
topConfig = TopLevelConfig {
    topLevelConfigProtocol = BftConfig {
        bftParams  = BftParams {
                         bftSecurityParam = securityParam
                       , bftNumNodes      = numCoreNodes
                       }
      , bftSignKey = SignKeyMockDSIGN 0
      , bftVerKeys = Map.fromList [
                         (CoreId (CoreNodeId 0), VerKeyMockDSIGN 0)
                       , (CoreId (CoreNodeId 1), VerKeyMockDSIGN 1)
                       ]
      }
  , topLevelConfigLedger  = eraParams
  , topLevelConfigBlock   = TB.TestBlockConfig numCoreNodes
  , topLevelConfigCodec   = TB.TestBlockCodecConfig
  , topLevelConfigStorage = TB.TestBlockStorageConfig
  }
  where
    eraParams :: HardFork.EraParams
    eraParams = HardFork.defaultEraParams securityParam slotLength

-----

data FollowerState =
    Resting !(RealPoint B)
  | Switching !(Point B) !(NE.NonEmpty B)
  | Switched !(NE.NonEmpty B)

-- | A 'ChaindB.Follower' that merely switches between each of the
-- `kInt`-length chains, in the order enumerated by 'TB.updateToNextNumeral'.
--
-- INVARIANT: the chains are never longer than 'kInt' and are 'kInt' long
-- infinitely often.
mkFollower ::
     StrictTVar IO (Tip B)
  -> IO (ChainDB.Follower IO B (ChainDB.WithPoint B H))
mkFollower varTip = do
    varState <- newIORef $ Resting $ blockRealPoint $ NE.last initialChain

    let wrap blk = ChainDB.WithPoint (getHeader blk) (blockPoint blk)

    let next = readIORef varState >>= \case
            Switching ipoint blks -> do
                writeIORef varState $ Switched blks
                atomically $ writeTVar varTip $ tipFromHeader $ NE.last blks
                pure $ RollBack ipoint
            Switched blks -> do
                let blk = NE.head blks
                writeIORef varState $ case NE.nonEmpty (NE.tail blks) of
                    Nothing    -> Resting $ blockRealPoint blk
                    Just blks' -> Switched blks'
                atomically $ writeTVar varTip $ tipFromHeader $ NE.last blks
                pure $ AddBlock $ wrap blk
            Resting rp -> do
                let (ipoint, blks) = TB.updateToNextNumeral rp
                writeIORef varState $ Switched blks
                atomically $ writeTVar varTip $ tipFromHeader $ NE.last blks
                pure $ RollBack ipoint

    pure ChainDB.Follower {
        ChainDB.followerClose               = pure ()
      , ChainDB.followerInstruction         = Just <$> next
      , ChainDB.followerInstructionBlocking = next
      , ChainDB.followerForward             = \case
            GenesisPoint : _ -> do
                writeIORef varState $ Switching GenesisPoint initialChain
                pure $ Just GenesisPoint

            ps -> error $ "impossible! " <> unlines (map show ps)
                -- The client begins with an empty local chain, so this is the
                -- only possible input at the handshake.
                --
                -- Moreover, no chain is longer than k, so the there can never
                -- another FindIntersect.
      }
