{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.ThreadNet.ShelleyAllegra (tests) where

import qualified Cardano.Ledger.Api.Transition as L
import Cardano.Ledger.BaseTypes (unNonZero)
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Shelley.Core as SL
import qualified Cardano.Protocol.TPraos.OCert as SL
import Cardano.Slotting.Slot (EpochSize (..), SlotNo (..))
import Control.Monad (replicateM)
import Control.Tracer (nullTracer)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Data.Proxy (Proxy (..))
import Data.SOP.Strict (NP (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import Lens.Micro ((^.))
import Ouroboros.Consensus.Cardano.Condense ()
import Ouroboros.Consensus.Cardano.Node (TriggerHardFork (..))
import Ouroboros.Consensus.Config.SecurityParam
import Ouroboros.Consensus.Ledger.SupportsMempool (extractTxs)
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.NodeId
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import qualified Ouroboros.Consensus.Protocol.TPraos as TPraos
import Ouroboros.Consensus.Shelley.Eras
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Shelley.Node
  ( ProtocolParamsShelleyBased (..)
  , ShelleyGenesis (..)
  , protocolInfoTPraosShelleyBased
  )
import Test.Consensus.Shelley.MockCrypto (MockCrypto)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.ThreadNet.General
import qualified Test.ThreadNet.Infra.Shelley as Shelley
import Test.ThreadNet.Infra.ShelleyBasedHardFork
import Test.ThreadNet.Infra.TwoEras
import Test.ThreadNet.Network
  ( NodeOutput (..)
  , TestNodeInitialization (..)
  )
import Test.ThreadNet.TxGen
import Test.ThreadNet.TxGen.Allegra ()
import Test.ThreadNet.TxGen.Shelley
import Test.ThreadNet.Util.Expectations (NumBlocks (..))
import Test.ThreadNet.Util.NodeJoinPlan (trivialNodeJoinPlan)
import Test.ThreadNet.Util.NodeRestarts (noRestarts)
import Test.ThreadNet.Util.Seed (runGen)
import qualified Test.Util.BoolProps as BoolProps
import Test.Util.HardFork.Future (EraSize (..), Future (..))
import Test.Util.Orphans.Arbitrary ()
import Test.Util.Slots (NumSlots (..))
import Test.Util.TestEnv

tests :: TestTree
tests =
  testGroup
    "ShelleyAllegra ThreadNet"
    [ askTestEnv $
        adjustTestEnv $
          testProperty "simple convergence" prop_simple_shelleyAllegra_convergence
    ]
 where
  adjustTestEnv :: TestTree -> TestEnv -> TestTree
  adjustTestEnv tree = \case
    Nightly -> tree
    _ -> adjustQuickCheckTests (`div` 10) tree

prop_simple_shelleyAllegra_convergence :: TestSetup TPraos ShelleyEra AllegraEra -> Property
prop_simple_shelleyAllegra_convergence
  TestSetup
    { setupD
    , setupHardFork
    , setupInitialNonce
    , setupK
    , setupPartition
    , setupSlotLength
    , setupTestConfig
    , setupVersion
    } =
    prop_general_semisync pga testOutput
      .&&. prop_inSync testOutput
      .&&. prop_ReachesEra2 reachesEra2
      .&&. prop_noCPViolation
      .&&. ( tabulate "ReachesEra2 label" [label_ReachesEra2 reachesEra2]
               $ tabulate
                 "Observed forge during a non-overlay slot in the second era"
                 [ label_hadActiveNonOverlaySlots
                     testOutput
                     overlaySlots
                 ]
               $ tabulatePartitionDuration setupK setupPartition
               $ tabulateFinalIntersectionDepth
                 setupK
                 (NumBlocks finalIntersectionDepth)
                 finalBlockEra
               $ tabulatePartitionPosition
                 (NumSlots numFirstEraSlots)
                 setupPartition
                 (ledgerReachesEra2 reachesEra2)
               $ property True
           )
   where
    TestConfig
      { initSeed
      , numCoreNodes
      , numSlots
      } = setupTestConfig

    pga =
      PropGeneralArgs
        { pgaBlockProperty = const $ property True
        , pgaCountTxs = fromIntegral . length . extractTxs
        , pgaExpectedCannotForge = noExpectedCannotForges
        , pgaFirstBlockNo = 0
        , pgaFixedMaxForkLength = Just maxForkLength
        , -- the leader schedule isn't fixed because the Shelley leader
          -- schedule is (at least ideally) unpredictable
          pgaFixedSchedule = Nothing
        , pgaSecurityParam = setupK
        , pgaTestConfig = setupTestConfig
        , pgaTestConfigB = testConfigB
        }

    txGenExtra =
      ShelleyTxGenExtra
        { stgeGenEnv = mkGenEnv DoNotGeneratePPUs coreNodes
        , -- We don't generate any transactions before the transaction
          -- carrying the proposal because they might consume its inputs
          -- before it does, thereby rendering it invalid.
          stgeStartAt = SlotNo 1
        }

    testConfigB =
      TestConfigB
        { forgeEbbEnv = Nothing
        , future =
            if setupHardFork
              then
                -- In this case the PVU will trigger the transition to the second era
                --
                -- By FACT (B), the PVU is always successful if we reach the second
                -- era.
                EraCons setupSlotLength epochSize firstEraSize $
                  EraFinal setupSlotLength epochSize
              else
                EraFinal setupSlotLength epochSize
        , messageDelay = mkMessageDelay setupPartition
        , nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
        , nodeRestarts = noRestarts
        , txGenExtra = WrapTxGenExtra txGenExtra :* WrapTxGenExtra () :* Nil
        , version = setupVersion
        }

    testOutput :: TestOutput (DualBlock TPraos ShelleyEra AllegraEra)
    testOutput =
      runTestNetwork
        setupTestConfig
        testConfigB
        TestConfigMB
          { nodeInfo = \(CoreNodeId nid) ->
              let protocolParamsShelleyBased =
                    ProtocolParamsShelleyBased
                      { shelleyBasedInitialNonce = setupInitialNonce
                      , shelleyBasedLeaderCredentials =
                          [ Shelley.mkLeaderCredentials
                              (coreNodes !! fromIntegral nid)
                          ]
                      }
                  hardForkTrigger =
                    TriggerHardForkAtVersion $ SL.getVersion majorVersion2
                  (protocolInfo, blockForging) =
                    protocolInfoShelleyBasedHardFork
                      protocolInfoTPraosShelleyBased
                      protocolInfoTPraosShelleyBased
                      protocolParamsShelleyBased
                      TPraos.tpraosParams
                      TPraos.tpraosParams
                      (SL.ProtVer majorVersion1 0)
                      (SL.ProtVer majorVersion2 0)
                      ( L.mkTransitionConfig L.NoGenesis $
                          L.mkShelleyTransitionConfig shelleyGenesis
                      )
                      hardForkTrigger
               in TestNodeInitialization
                    { tniCrucialTxs =
                        if not setupHardFork
                          then []
                          else
                            fmap GenTxShelley1 $
                              Shelley.mkSetDecentralizationParamTxs
                                coreNodes
                                (SL.ProtVer majorVersion2 0)
                                (SlotNo $ unNumSlots numSlots) -- never expire
                                setupD -- unchanged
                    , tniProtocolInfo = protocolInfo
                    , tniBlockForging = blockForging nullTracer
                    }
          , mkRekeyM = Nothing
          }

    maxForkLength :: NumBlocks
    maxForkLength = NumBlocks $ unNonZero $ maxRollbacks setupK

    initialKESPeriod :: SL.KESPeriod
    initialKESPeriod = SL.KESPeriod 0

    coreNodes :: [Shelley.CoreNode MockCrypto]
    coreNodes =
      runGen initSeed $
        replicateM (fromIntegral n) $
          Shelley.genCoreNode initialKESPeriod
     where
      NumCoreNodes n = numCoreNodes

    maxLovelaceSupply :: Word64
    maxLovelaceSupply =
      fromIntegral (length coreNodes) * Shelley.initialLovelacePerCoreNode

    shelleyGenesis :: ShelleyGenesis
    shelleyGenesis =
      Shelley.mkGenesisConfig
        (SL.ProtVer majorVersion1 0)
        setupK
        activeSlotCoeff
        setupD
        maxLovelaceSupply
        setupSlotLength
        (Shelley.mkKesConfig (Proxy @MockCrypto) numSlots)
        coreNodes

    -- the Shelley ledger is designed to use a fixed epoch size, so this test
    -- does not randomize it
    epochSize :: EpochSize
    epochSize = sgEpochLength shelleyGenesis

    firstEraSize :: EraSize
    firstEraSize = EraSize numFirstEraEpochs

    -- Classifying test cases

    reachesEra2 :: ReachesEra2
    reachesEra2 =
      ReachesEra2
        { rsEra1Slots =
            BoolProps.enabledIf $ t > numFirstEraSlots
        , rsPV = BoolProps.enabledIf setupHardFork
        , rsEra2Blocks =
            or $
              [ not $ isFirstEraBlock blk
              | (_nid, no) <- Map.toList testOutputNodes
              , let NodeOutput{nodeOutputForges} = no
              , (blk, _m) <- maybeToList $ Map.maxView nodeOutputForges
              -- the last block the node forged
              ]
        , rsEra2Slots =
            --- TODO this comment and code are wrong

            BoolProps.requiredIf $
              -- The active slots in the first two Shelley epochs are all overlay
              -- slots, so the first Shelley block will arise from one of those.
              not $
                Set.null overlaySlots
        }
     where
      NumSlots t = numSlots
      TestOutput{testOutputNodes} = testOutput

    -- All OBFT overlay slots in the second era.
    overlaySlots :: Set SlotNo
    overlaySlots =
      secondEraOverlaySlots
        numSlots
        (NumSlots numFirstEraSlots)
        (sgProtocolParams shelleyGenesis ^. SL.ppDG)
        epochSize

    numFirstEraSlots :: Word64
    numFirstEraSlots =
      numFirstEraEpochs * unEpochSize epochSize

    finalBlockEra :: String
    finalBlockEra =
      if rsEra2Blocks reachesEra2
        then "Allegra"
        else "Shelley"

    finalIntersectionDepth :: Word64
    finalIntersectionDepth = depth
     where
      NumBlocks depth = calcFinalIntersectionDepth pga testOutput

    prop_noCPViolation :: Property
    prop_noCPViolation =
      counterexample
        ( "finalChains: "
            <> show (nodeOutputFinalChain <$> testOutputNodes testOutput)
        )
        $ counterexample "CP violation in final chains!"
        $ property
        $ unNonZero (maxRollbacks setupK) >= finalIntersectionDepth

{-------------------------------------------------------------------------------
  Constants
-------------------------------------------------------------------------------}

-- | The major protocol version of the first era in this test
majorVersion1 :: SL.Version
majorVersion1 = SL.natVersion @1

-- | The major protocol version of the second era in this test
majorVersion2 :: SL.Version
majorVersion2 = SL.natVersion @2
