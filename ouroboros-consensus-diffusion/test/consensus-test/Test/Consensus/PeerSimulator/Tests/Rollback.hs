{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.PeerSimulator.Tests.Rollback (tests) where

import           Control.Monad.IOSim (runSimOrThrow)
import           Data.Maybe (fromJust)
import           Ouroboros.Consensus.Block (ChainHash (..))
import           Ouroboros.Consensus.Config.SecurityParam
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Test.Consensus.BlockTree (BlockTree (..), BlockTreeBranch (..))
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.PeerSimulator.Run (noTimeoutsSchedulerConfig)
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PointSchedule
import qualified Test.QuickCheck as QC
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (TestBlock, unTestHash)
import           Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests = testGroup "rollback" [
  adjustQuickCheckTests (`div` 10) $
  localOption (QuickCheckMaxRatio 100) $
  testProperty "can rollback" prop_rollback
  ,
  adjustQuickCheckTests (`div` 10) $
  testProperty "cannot rollback" prop_cannotRollback
  ]

-- | @prop_rollback@ tests that the selection of the node under test
-- changes branches when sent a rollback to a block no older than 'k' blocks
-- before the current selection.
prop_rollback :: QC.Gen QC.Property
prop_rollback = do
  genesisTest <- genChains 1

  let SecurityParam k = gtSecurityParam genesisTest
      schedule = rollbackSchedule (fromIntegral k) (gtBlockTree genesisTest)

  pure $
      runSimOrThrow $ runTest schedulerConfig genesisTest schedule $ \StateView{svSelectedChain} ->
        let headOnAlternativeChain = case AF.headHash svSelectedChain of
              GenesisHash    -> False
              BlockHash hash -> any (0 /=) $ unTestHash hash
        in
        -- The test passes if we end up on the alternative chain
        headOnAlternativeChain

  where
    schedulerConfig = noTimeoutsSchedulerConfig defaultPointScheduleConfig

-- @prop_cannotRollback@ tests that the selection of the node under test *does
-- not* change branches when sent a rollback to a block strictly older than 'k'
-- blocks before the current selection.
prop_cannotRollback :: QC.Gen QC.Property
prop_cannotRollback = do
  genesisTest <- genChains 1

  let SecurityParam k = gtSecurityParam genesisTest
      schedule = rollbackSchedule (fromIntegral (k + 1)) (gtBlockTree genesisTest)

  pure $
      runSimOrThrow $ runTest schedulerConfig genesisTest schedule $ \StateView{svSelectedChain} ->
        let headOnAlternativeChain = case AF.headHash svSelectedChain of
              GenesisHash    -> False
              BlockHash hash -> any (0 /=) $ unTestHash hash
        in
        -- The test passes if we end up on the trunk.
        not headOnAlternativeChain

  where
    schedulerConfig = noTimeoutsSchedulerConfig defaultPointScheduleConfig

-- | A schedule that advertises all the points of the trunk up until the nth
-- block after the intersection, then switches to the first alternative
-- chain of the given block tree.
--
-- PRECONDITION: Block tree with at least one alternative chain.
rollbackSchedule :: Int -> BlockTree TestBlock -> PointSchedule
rollbackSchedule n blockTree =
  let branch = head $ btBranches blockTree
      trunkSuffix = AF.takeOldest n (btbTrunkSuffix branch)
      states = concat
        [ banalStates (btbPrefix branch)
        , banalStates trunkSuffix
        , banalStates (btbSuffix branch)
        ]
      peers = peersOnlyHonest states
      pointSchedule = balanced defaultPointScheduleConfig peers
   in fromJust pointSchedule
