{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.PeerSimulator.Tests.Rollback (tests) where

import           Control.Monad.IOSim (runSimOrThrow)
import           Ouroboros.Consensus.Block (ChainHash (..))
import           Ouroboros.Consensus.Config.SecurityParam
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Test.Consensus.BlockTree (BlockTree (..), BlockTreeBranch (..))
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.PeerSimulator.Run (noTimeoutsSchedulerConfig)
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PointSchedule
import qualified Test.QuickCheck as QC
import           Test.QuickCheck
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
  genesisTest <- genChains (pure 1)

  let SecurityParam k = gtSecurityParam genesisTest
      schedule = rollbackSchedule (fromIntegral k) (gtBlockTree genesisTest)

  -- We consider the test case interesting if we can rollback
  pure $
    alternativeChainIsLongEnough (gtSecurityParam genesisTest) (gtBlockTree genesisTest)
    ==>
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
  genesisTest <- genChains (pure 1)

  let SecurityParam k = gtSecurityParam genesisTest
      schedule = rollbackSchedule (fromIntegral (k + 1)) (gtBlockTree genesisTest)

  -- We consider the test case interesting if it allows to rollback even if
  -- the implementation doesn't
  pure $
    alternativeChainIsLongEnough (gtSecurityParam genesisTest) (gtBlockTree genesisTest)
      &&
    honestChainIsLongEnough (gtSecurityParam genesisTest) (gtBlockTree genesisTest)
    ==>
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
  let branch = case btBranches blockTree of
        [b] -> b
        _   -> error "The block tree must have exactly one alternative branch"
      trunkSuffix = AF.takeOldest n (btbTrunkSuffix branch)
      states = concat
        [ banalStates (btbPrefix branch)
        , banalStates trunkSuffix
        , banalStates (btbSuffix branch)
        ]
      peers = peersOnlyHonest states
      pointSchedule = balanced defaultPointScheduleConfig peers
   in pointSchedule

-- | Whether the honest chain has more than 'k' blocks after the
-- intersection with the alternative chain.
--
-- PRECONDITION: Block tree with exactly one alternative chain, otherwise
-- this property does not make sense. With no alternative chain, this will
-- even crash.
honestChainIsLongEnough :: SecurityParam -> BlockTree TestBlock -> Bool
honestChainIsLongEnough (SecurityParam k) blockTree =
  let BlockTreeBranch{btbTrunkSuffix} = case btBranches blockTree of
        [b] -> b
        _   -> error "The block tree must have exactly one alternative branch"
      lengthTrunkSuffix = AF.length btbTrunkSuffix
   in lengthTrunkSuffix > fromIntegral k

-- | Whether the alternative chain has more than 'k' blocks after the
-- intersection with the honest chain.
--
-- PRECONDITION: Block tree with exactly one alternative chain, otherwise
-- this property does not make sense. With no alternative chain, this will
-- even crash.
alternativeChainIsLongEnough :: SecurityParam -> BlockTree TestBlock -> Bool
alternativeChainIsLongEnough (SecurityParam k) blockTree =
  let BlockTreeBranch{btbSuffix} = case btBranches blockTree of
        [b] -> b
        _   -> error "The block tree must have exactly one alternative branch"
      lengthSuffix = AF.length btbSuffix
   in lengthSuffix > fromIntegral k
