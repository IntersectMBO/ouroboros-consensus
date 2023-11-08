{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}

module Test.Consensus.PeerSimulator.Config (defaultCfg) where

import           Cardano.Crypto.DSIGN (SignKeyDSIGN (..), VerKeyDSIGN (..))
import           Cardano.Slotting.Time (SlotLength, slotLengthFromSec)
import qualified Data.Map.Strict as Map
import           Ouroboros.Consensus.Config (SecurityParam, TopLevelConfig (..))
import           Ouroboros.Consensus.HardFork.History
                     (EraParams (eraGenesisWin))
import qualified Ouroboros.Consensus.HardFork.History.EraParams as HardFork
import           Ouroboros.Consensus.Ledger.SupportsProtocol (GenesisWindow)
import           Ouroboros.Consensus.Node.ProtocolInfo
                     (NumCoreNodes (NumCoreNodes))
import           Ouroboros.Consensus.NodeId (CoreNodeId (CoreNodeId),
                     NodeId (CoreId))
import           Ouroboros.Consensus.Protocol.BFT
                     (BftParams (BftParams, bftNumNodes, bftSecurityParam),
                     ConsensusConfig (BftConfig, bftParams, bftSignKey, bftVerKeys))
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (BlockConfig (TestBlockConfig),
                     CodecConfig (TestBlockCodecConfig),
                     StorageConfig (TestBlockStorageConfig), TestBlock)

-- REVIEW: this has not been deliberately chosen
defaultCfg :: SecurityParam -> GenesisWindow -> TopLevelConfig TestBlock
defaultCfg secParam eraGenesisWin = TopLevelConfig {
    topLevelConfigProtocol = BftConfig {
      bftParams  = BftParams {
        bftSecurityParam = secParam
      , bftNumNodes      = NumCoreNodes 2
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
  where
    -- REVIEW: Make it 1s or a parameter?
    slotLength :: SlotLength
    slotLength = slotLengthFromSec 20

    eraParams :: HardFork.EraParams
    eraParams = (HardFork.defaultEraParams secParam slotLength) {eraGenesisWin}

    numCoreNodes = NumCoreNodes 2
