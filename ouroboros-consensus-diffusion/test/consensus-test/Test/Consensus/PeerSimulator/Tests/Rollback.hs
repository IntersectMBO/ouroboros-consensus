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
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (TestBlock, unTestHash)
import           Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests = testGroup "rollback" [
  -- NOTE: The property @prop_rollback True@ discards a lot of inputs, making
  -- it quite flakey. We increase the maximum number of discarded tests per
  -- successful ones so as to make this test more reliable.
  adjustQuickCheckTests (`div` 10) $
  localOption (QuickCheckMaxRatio 100) $
  testProperty "can rollback" (prop_rollback True)
  ,
  adjustQuickCheckTests (`div` 10) $
  testProperty "cannot rollback" (prop_rollback False)
  ]

-- | @prop_rollback True@ tests that the selection of the node under test
-- changes branches when sent a rollback to a block no older than 'k' blocks
-- before the current selection.
--
-- @prop_rollback False@ tests that the selection of the node under test *does
-- not* change branches when sent a rollback to a block strictly older than 'k'
-- blocks before the current selection.
prop_rollback :: Bool -> QC.Gen QC.Property
prop_rollback wantRollback = do
  genesisTest <- genChains 1

  let schedule = rollbackSchedule (gtBlockTree genesisTest)

  -- | We consider the test case interesting if we want a rollback and we can
  -- actually get one, or if we want no rollback and we cannot actually get one.
  pure $
    wantRollback == canRollbackFromTrunkTip (gtSecurityParam genesisTest) (gtBlockTree genesisTest)
    ==>
      runSimOrThrow $ runTest schedulerConfig genesisTest schedule $ \StateView{svSelectedChain} ->
        let headOnAlternativeChain = case AF.headHash svSelectedChain of
              GenesisHash    -> False
              BlockHash hash -> any (0 /=) $ unTestHash hash
        in
        -- The test passes if we want a rollback and we actually end up on the
        -- alternative chain or if we want no rollback and end up on the trunk.
        wantRollback == headOnAlternativeChain

  where
    schedulerConfig = noTimeoutsSchedulerConfig defaultPointScheduleConfig

    -- A schedule that advertises all the points of the trunk, then switches to
    -- the first alternative chain of the given block tree.
    --
    -- PRECONDITION: Block tree with at least one alternative chain.
    rollbackSchedule :: BlockTree TestBlock -> PointSchedule
    rollbackSchedule blockTree =
      let trunk = btTrunk blockTree
          branch = btbSuffix $ head $ btBranches blockTree
          states = banalStates trunk ++ banalStates branch
          peers = peersOnlyHonest states
          pointSchedule = balanced defaultPointScheduleConfig peers
       in fromJust pointSchedule

    -- | Whether it is possible to roll back from the trunk after having served
    -- it fully, that is whether there is an alternative chain that forks of the
    -- trunk no more than 'k' blocks from the tip and that is longer than the
    -- trunk.
    --
    -- PRECONDITION: Block tree with exactly one alternative chain, otherwise
    -- this property does not make sense. With no alternative chain, this will
    -- even crash.
    --
    -- REVIEW: Why does 'existsSelectableAdversary' get to be a classifier and
    -- not this? (or a generalised version of this)
    canRollbackFromTrunkTip :: SecurityParam -> BlockTree TestBlock -> Bool
    canRollbackFromTrunkTip (SecurityParam k) blockTree =
      let BlockTreeBranch{btbSuffix, btbTrunkSuffix} = head $ btBranches blockTree
          lengthSuffix = AF.length btbSuffix
          lengthTrunkSuffix = AF.length btbTrunkSuffix
       in lengthTrunkSuffix <= fromIntegral k && lengthSuffix > lengthTrunkSuffix
