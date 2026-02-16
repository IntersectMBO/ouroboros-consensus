{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Tools.ThreadNet.Run
  ( Opts (..)
  , run
  ) where

import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Tools.DBAnalyser.Block.Cardano as Cardano
import Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import qualified Data.Map as Map
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.Condense ()
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Abstract (ValuesMK)
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.NodeId (CoreNodeId)
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Shelley.Node
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.QuickCheck (Gen)
import Test.ThreadNet.General
import Test.ThreadNet.Network
  ( NodeOutput (nodeOutputAdds)
  , TestNodeInitialization (..)
  , nodeOutputAdds
  )
import Test.ThreadNet.TxGen (TxGen (..))
import Test.ThreadNet.Util.NodeJoinPlan (trivialNodeJoinPlan)
import Test.ThreadNet.Util.NodeRestarts (noRestarts)
import Test.ThreadNet.Util.NodeToNodeVersion (newestVersion)
import Test.ThreadNet.Util.NodeTopology (meshNodeTopology)
import Test.ThreadNet.Util.Seed (Seed (Seed))
import Test.Util.HardFork.Future
import Test.Util.Orphans.Arbitrary ()
import Test.Util.Slots (NumSlots (NumSlots))

data Opts = Opts
  { configFile :: FilePath
  }

run :: Opts -> IO ()
run Opts{configFile} = do
  protocolInfo <- mkProtocolInfo (Cardano.CardanoBlockArgs configFile Nothing)

  let
    numCoreNodes = NumCoreNodes 3
    testConfig =
      TestConfig
        { numSlots = NumSlots 1000
        , numCoreNodes
        , nodeTopology = meshNodeTopology numCoreNodes
        , initSeed = Seed 0
        }
  testOutput <-
    runThreadNet $
      RunThreadNetArgs
        { testConfig
        , protocolInfo
        }

  print $ Map.lookupMax . testOutputTipBlockNos $ testOutput
  print $
    [ ( nodeId
      , Map.lookupMax . nodeOutputAdds $ nodeOutput
      )
    | (nodeId, nodeOutput) <- Map.toList . testOutputNodes $ testOutput
    ]

  return ()

data RunThreadNetArgs = RunThreadNetArgs
  { testConfig :: TestConfig
  , protocolInfo :: ProtocolInfo (CardanoBlock StandardCrypto)
  }

runThreadNet :: RunThreadNetArgs -> IO (TestOutput (CardanoBlock StandardCrypto))
runThreadNet
  RunThreadNetArgs
    { testConfig
    , protocolInfo
    } = do
    let
      shelleyGenesis :: ShelleyGenesis =
        shelleyLedgerGenesis
          . shelleyLedgerConfig
          . ( \(CardanoLedgerConfig _cfgByron cfgShelley _cfgAllegra _cfgMary _cfgAlonzo _cfgBabbage _cfgConway) -> cfgShelley
            )
          . topLevelConfigLedger
          . pInfoConfig
          $ protocolInfo
      slotLength = mkSlotLength $ SL.fromNominalDiffTimeMicro $ SL.sgSlotLength shelleyGenesis
      epochLength = sgEpochLength shelleyGenesis

      testConfigB =
        TestConfigB
          { forgeEbbEnv = Nothing
          , future =
              EraFinal slotLength epochLength
          , messageDelay = noCalcMessageDelay
          , nodeJoinPlan = trivialNodeJoinPlan (numCoreNodes testConfig)
          , nodeRestarts = noRestarts
          , txGenExtra = mkTxGenExtra
          , version = newestVersion (Proxy :: Proxy (CardanoBlock StandardCrypto))
          }

      testOutput =
        runTestNetwork
          testConfig
          testConfigB
          TestConfigMB
            { nodeInfo = \_ ->
                TestNodeInitialization
                  { tniProtocolInfo = protocolInfo
                  , tniCrucialTxs = []
                  , tniBlockForging = pure []
                  }
            , mkRekeyM = Nothing
            }

    return testOutput

mkTxGenExtra :: TxGenExtra (CardanoBlock StandardCrypto)
mkTxGenExtra = ()

instance TxGen (CardanoBlock StandardCrypto) where
  testGenTxs ::
    CoreNodeId ->
    NumCoreNodes ->
    SlotNo ->
    TopLevelConfig (CardanoBlock StandardCrypto) ->
    TxGenExtra (CardanoBlock StandardCrypto) ->
    LedgerState (CardanoBlock StandardCrypto) ValuesMK ->
    Gen [GenTx (CardanoBlock StandardCrypto)]
  testGenTxs _coreNodeId _numCores _slotNo _topCfg _txGenExtra (LedgerStateConway _st) = return []
  testGenTxs _coreNodeId _numCores _slotNo _topCfg _txGenExtra _ = return $ error "not in conway"
