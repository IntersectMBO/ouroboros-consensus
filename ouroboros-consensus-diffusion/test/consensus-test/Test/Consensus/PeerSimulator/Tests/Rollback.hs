{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Test.Consensus.PeerSimulator.Tests.Rollback (tests) where

import           Control.Monad.Class.MonadTime.SI (Time (Time))
import           Ouroboros.Consensus.Block (ChainHash (..), Header)
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     toOldestFirst)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Test.Consensus.BlockTree (BlockTree (..), BlockTreeBranch (..))
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.Genesis.Setup.Classifiers
                     (Classifiers (allAdversariesKPlus1InForecast),
                     allAdversariesForecastable, classifiers)
import           Test.Consensus.PeerSimulator.Run (defaultSchedulerConfig)
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PointSchedule
import           Test.Consensus.PointSchedule.Peers (peersOnlyHonest)
import           Test.Consensus.PointSchedule.SinglePeer (SchedulePoint (..),
                     scheduleBlockPoint, scheduleHeaderPoint, scheduleTipPoint)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (TestBlock, unTestHash)
import           Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests = testGroup "rollback" [
  adjustQuickCheckTests (`div` 10) $
  testProperty "can rollback" prop_rollback
  ,
  adjustQuickCheckTests (`div` 10) $
  testProperty "cannot rollback" prop_cannotRollback
  ]

-- | @prop_rollback@ tests that the selection of the node under test
-- changes branches when sent a rollback to a block no older than 'k' blocks
-- before the current selection.
prop_rollback :: Property
prop_rollback = do
  forAllGenesisTest

    (do gt@GenesisTest{gtSecurityParam, gtBlockTree} <- genChains (pure 1)
        -- TODO: Trim block tree, the rollback schedule does not use all of it
        let cls = classifiers gt
        if allAdversariesForecastable cls && allAdversariesKPlus1InForecast cls
          then pure gt {gtSchedule = rollbackSchedule (fromIntegral (maxRollbacks gtSecurityParam)) gtBlockTree}
          else discard)

    defaultSchedulerConfig

    (\_ _ -> [])

    (\_ -> not . hashOnTrunk . AF.headHash . svSelectedChain)

-- @prop_cannotRollback@ tests that the selection of the node under test *does
-- not* change branches when sent a rollback to a block strictly older than 'k'
-- blocks before the current selection.
prop_cannotRollback :: Property
prop_cannotRollback =
  forAllGenesisTest

    (do gt@GenesisTest{gtSecurityParam, gtBlockTree} <- genChains (pure 1)
        if alternativeChainIsLongEnough gtSecurityParam gtBlockTree
          then pure gt {gtSchedule = rollbackSchedule (fromIntegral (maxRollbacks gtSecurityParam + 1)) gtBlockTree}
          else discard)

    defaultSchedulerConfig

    (\_ _ -> [])

    (\_ -> hashOnTrunk . AF.headHash . svSelectedChain)

-- | A schedule that advertises all the points of the trunk up until the nth
-- block after the intersection, then switches to the first alternative
-- chain of the given block tree.
--
-- PRECONDITION: Block tree with at least one alternative chain.
rollbackSchedule :: AF.HasHeader blk => Int -> BlockTree blk -> PeersSchedule blk
rollbackSchedule n blockTree =
    let branch = firstBranch blockTree
        trunkSuffix = AF.takeOldest n (btbTrunkSuffix branch)
        schedulePoints = concat
          [ banalSchedulePoints (btbPrefix branch)
          , banalSchedulePoints trunkSuffix
          , banalSchedulePoints (btbSuffix branch)
          ]
    in peersOnlyHonest $ zip (map (Time . (/30)) [0..]) schedulePoints
  where
    banalSchedulePoints :: AnchoredFragment blk -> [SchedulePoint blk]
    banalSchedulePoints = concatMap banalSchedulePoints' . toOldestFirst
    banalSchedulePoints' :: blk -> [SchedulePoint blk]
    banalSchedulePoints' block = [scheduleTipPoint block, scheduleHeaderPoint block, scheduleBlockPoint block]

-- | Whether the alternative chain has more than 'k' blocks after the
-- intersection with the honest chain.
--
-- PRECONDITION: Block tree with exactly one alternative chain, otherwise
-- this property does not make sense. With no alternative chain, this will
-- even crash.
alternativeChainIsLongEnough :: SecurityParam -> BlockTree TestBlock -> Bool
alternativeChainIsLongEnough (SecurityParam k) blockTree =
  let BlockTreeBranch{btbSuffix} = firstBranch blockTree
      lengthSuffix = AF.length btbSuffix
   in lengthSuffix > fromIntegral k

firstBranch :: BlockTree TestBlock -> BlockTreeBranch TestBlock
firstBranch tree = case btBranches tree of
  b:_ -> b
  _   -> error "The block tree must have at least one branch"

-- | Given a hash, checks whether it is on the trunk of the block tree, that is
-- if it only contains zeroes.
hashOnTrunk :: ChainHash (Header TestBlock) -> Bool
hashOnTrunk GenesisHash      = True
hashOnTrunk (BlockHash hash) = all (== 0) $ unTestHash hash
