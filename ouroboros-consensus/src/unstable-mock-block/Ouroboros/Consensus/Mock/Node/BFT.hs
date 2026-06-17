{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Consensus.Mock.Node.BFT
  ( MockBftBlock
  , blockForgingBft
  , protocolInfoBft
  ) where

import Cardano.Crypto.DSIGN
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Data.Map.Strict as Map
import Ouroboros.Consensus.Block.Forging (BlockForging)
import Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.Mock.Ledger
import Ouroboros.Consensus.Mock.Node
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import Ouroboros.Consensus.Peras.Context
import Ouroboros.Consensus.Protocol.BFT

type MockBftBlock = SimpleBftBlock SimpleMockCrypto BftMockCrypto

protocolInfoBft ::
  NumCoreNodes ->
  CoreNodeId ->
  SecurityParam ->
  HardFork.EraParams ->
  ProtocolInfo MockBftBlock
protocolInfoBft numCoreNodes nid securityParam eraParams =
  ProtocolInfo
    { pInfoConfig =
        TopLevelConfig
          { topLevelConfigProtocol =
              BftConfig
                { bftParams =
                    BftParams
                      { bftNumNodes = numCoreNodes
                      , bftSecurityParam = securityParam
                      }
                , bftSignKey = signKey nid
                , bftVerKeys =
                    Map.fromList
                      [ (CoreId n, verKey n)
                      | n <- enumCoreNodes numCoreNodes
                      ]
                }
          , topLevelConfigLedger = SimpleLedgerConfig () eraParams defaultMockConfig
          , topLevelConfigBlock = SimpleBlockConfig
          , topLevelConfigCodec = SimpleCodecConfig
          , topLevelConfigStorage = SimpleStorageConfig securityParam
          , topLevelConfigCheckpoints = emptyCheckpointsMap
          }
    , pInfoInitLedger =
        let ledgerState = genesisSimpleLedgerState addrDist
            headerState = genesisHeaderState ()
            perasEpochContextResolver = ledgerStateHeaderStateMkPerasEpochContextResolver ledgerState headerState
            latestPerasCertOnChainRound = SNothing
         in ExtLedgerState
              { ledgerState
              , headerState
              , perasEpochContextResolver
              , latestPerasCertOnChainRound
              }
    }
 where
  signKey :: CoreNodeId -> SignKeyDSIGN MockDSIGN
  signKey (CoreNodeId n) = SignKeyMockDSIGN n

  verKey :: CoreNodeId -> VerKeyDSIGN MockDSIGN
  verKey (CoreNodeId n) = VerKeyMockDSIGN n

  addrDist :: AddrDist
  addrDist = mkAddrDist numCoreNodes

blockForgingBft :: Monad m => CoreNodeId -> [BlockForging m MockBftBlock]
blockForgingBft nid = [simpleBlockForging nid forgeBftExt]
