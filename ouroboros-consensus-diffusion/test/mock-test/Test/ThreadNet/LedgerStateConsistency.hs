{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

-- | Verify that independently replaying a node's final chain from genesis
-- produces the same ledger state as the one computed by the ChainDB during
-- the simulation.
--
-- Existing ThreadNet tests check chain properties (common prefix, growth,
-- no invalid blocks) and occasionally inspect specific ledger state fields,
-- but none verify that the final ledger state is actually the result of
-- applying the blocks in the final chain. This test fills that gap.
module Test.ThreadNet.LedgerStateConsistency (tests) where

import Cardano.Ledger.BaseTypes (nonZero, unNonZero)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Config.SecurityParam
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.Ledger.Abstract
  ( ComputeLedgerEvents (OmitLedgerEvents)
  , tickThenReapply
  )
import Ouroboros.Consensus.Ledger.Extended
  ( ExtLedgerCfg (..)
  , ExtLedgerState (..)
  , ledgerState
  )
import Ouroboros.Consensus.Ledger.Tables.Utils (applyDiffs, forgetLedgerTables)
import Ouroboros.Consensus.Mock.Ledger ()
import Ouroboros.Consensus.Mock.Node ()
import Ouroboros.Consensus.Mock.Node.BFT
import Ouroboros.Consensus.Mock.Node.Serialisation ()
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.NodeId
import qualified Ouroboros.Network.Mock.Chain as Chain
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.ThreadNet.General
import Test.ThreadNet.Network (NodeOutput (..))
import Test.ThreadNet.TxGen.Mock ()
import Test.ThreadNet.Util.NodeJoinPlan
import Test.ThreadNet.Util.NodeRestarts
import Test.ThreadNet.Util.NodeToNodeVersion
import Test.Util.HardFork.Future (singleEraFuture)
import Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests =
  testGroup
    "Ledger state consistency"
    [ testProperty "Chain replay matches node's ChainDB state" prop_replay_matches_final_state
    ]

data TestSetup = TestSetup
  { setupK :: SecurityParam
  , setupTestConfig :: TestConfig
  , setupNodeJoinPlan :: NodeJoinPlan
  }
  deriving (Show)

instance Arbitrary TestSetup where
  arbitrary = do
    k <- SecurityParam <$> choose (2, 10) `suchThatMap` nonZero
    testConfig <- arbitrary
    let TestConfig{numCoreNodes, numSlots} = testConfig
    nodeJoinPlan <- genNodeJoinPlan numCoreNodes numSlots
    pure $ TestSetup k testConfig nodeJoinPlan

-- | Replays the final chain from genesis using 'tickThenReapply' and checks
-- that the resulting ledger state matches the node's 'nodeOutputFinalLedger'.
prop_replay_matches_final_state :: TestSetup -> Property
prop_replay_matches_final_state
  TestSetup
    { setupK = k
    , setupTestConfig = testConfig
    , setupNodeJoinPlan = nodeJoinPlan
    } =
    counterexample ("chain length: " <> show (length chain))
      $ counterexample ("node join plan: " <> show nodeJoinPlan)
      $ not (null chain)
        ==> forgetLedgerTables (ledgerState foldedState) === expectedLedger
   where
    TestConfig{numCoreNodes} = testConfig
    slotLength = slotLengthFromSec 20

    pInfo =
      protocolInfoBft
        numCoreNodes
        (CoreNodeId 0)
        k
        (HardFork.defaultEraParams k slotLength)

    testConfigB =
      TestConfigB
        { forgeEbbEnv = Nothing
        , future =
            singleEraFuture
              slotLength
              (EpochSize $ unNonZero (maxRollbacks k) * 10)
        , messageDelay = noCalcMessageDelay
        , nodeJoinPlan
        , nodeRestarts = noRestarts
        , txGenExtra = ()
        , version = newestVersion (Proxy @MockBftBlock)
        }

    testOutput =
      runTestNetwork
        testConfig
        testConfigB
        TestConfigMB
          { nodeInfo = \nid ->
              plainTestNodeInitialization
                ( protocolInfoBft
                    numCoreNodes
                    nid
                    k
                    (HardFork.defaultEraParams k slotLength)
                )
                (pure $ blockForgingBft nid)
          , mkRekeyM = Nothing
          }

    someNode = snd . Map.findMin $ testOutputNodes testOutput
    chain = Chain.toOldestFirst (nodeOutputFinalChain someNode)
    expectedLedger = nodeOutputFinalLedger someNode

    cfg = ExtLedgerCfg (pInfoConfig pInfo)
    -- We use 'tickThenReapply' because the blocks have already been validated
    -- by the ChainDB. We use 'applyDiffs' instead of 'applyDiffForKeys'
    -- because we need to accumulate the full UTxO -- 'applyDiffForKeys' only
    -- retains entries referenced by the current block, which is designed for
    -- the LedgerDB's backing store architecture.
    foldedState = foldl' step (pInfoInitLedger pInfo) chain
     where
      step state blk =
        applyDiffs state $ tickThenReapply OmitLedgerEvents cfg blk state
