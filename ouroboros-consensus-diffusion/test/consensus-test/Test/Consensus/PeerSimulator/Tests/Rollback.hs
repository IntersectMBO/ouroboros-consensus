{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.PeerSimulator.Tests.Rollback
  ( TestKey
  , testSuite
  ) where

import Cardano.Ledger.BaseTypes (unNonZero)
import Control.Monad.Class.MonadTime.SI (Time (Time))
import qualified Data.Map as M
import Ouroboros.Consensus.Block (ChainHash (..), Header)
import Ouroboros.Consensus.Config.SecurityParam
import Ouroboros.Network.AnchoredFragment
  ( AnchoredFragment
  , toOldestFirst
  )
import qualified Ouroboros.Network.AnchoredFragment as AF
import Test.Consensus.BlockTree
  ( BlockTree (..)
  , BlockTreeBranch (..)
  , deforestBlockTree
  )
import Test.Consensus.Genesis.Setup
import Test.Consensus.Genesis.Setup.Classifiers
  ( Classifiers (allAdversariesKPlus1InForecast)
  , allAdversariesForecastable
  , classifiers
  )
import Test.Consensus.Genesis.TestSuite
import Test.Consensus.PeerSimulator.Run (defaultSchedulerConfig)
import Test.Consensus.PeerSimulator.StateView
import Test.Consensus.PointSchedule
import Test.Consensus.PointSchedule.Peers (peersOnlyHonest)
import Test.Consensus.PointSchedule.SinglePeer
  ( SchedulePoint (..)
  , scheduleBlockPoint
  , scheduleHeaderPoint
  , scheduleTipPoint
  )
import Test.QuickCheck
import Test.Util.Orphans.IOLike ()

-- | Default adjustment of the required number of test runs.
-- Can be set individually on each test definition.
adjustTestCount :: AdjustTestCount
adjustTestCount = AdjustTestCount (`div` 2)

-- | Default adjustment of max test case size.
-- Can be set individually on each test definition.
adjustMaxSize :: AdjustMaxSize
adjustMaxSize = AdjustMaxSize id

data TestKey = CanRollback | CannotRollback
  deriving stock (Eq, Ord, Generic)
  deriving SmallKey via Generically TestKey

testSuite ::
  ( IssueTestBlock blk
  , AF.HasHeader blk
  , AF.HasHeader (Header blk)
  , Eq blk
  ) =>
  TestSuite blk TestKey
testSuite = group "rollback" . newTestSuite $ \case
  CanRollback -> testRollback
  CannotRollback -> testCannotRollback

-- | Tests that the selection of the node under test
-- changes branches when sent a rollback to a block no older than 'k' blocks
-- before the current selection.
testRollback ::
  ( IssueTestBlock blk
  , AF.HasHeader blk
  , AF.HasHeader (Header blk)
  , Eq blk
  ) =>
  ConformanceTest blk
testRollback =
  mkConformanceTest
    "can rollback"
    adjustTestCount
    adjustMaxSize
    ( do
        -- Create a block tree with @1@ alternative chain, such that we can rollback
        -- from the trunk to that chain.
        gt@GenesisTest{gtSecurityParam, gtBlockTree} <- genChains (pure 1)
        -- TODO: Trim block tree, the rollback schedule does not use all of it
        let cls = classifiers gt
        if allAdversariesForecastable cls && allAdversariesKPlus1InForecast cls
          then
            pure
              gt
                { gtSchedule = rollbackSchedule (fromIntegral (unNonZero $ maxRollbacks gtSecurityParam)) gtBlockTree
                }
          else discard
    )
    defaultSchedulerConfig
    -- No shrinking because the schedule is tiny and hand-crafted
    mempty
    (\test -> not . hashOnTrunk (gtBlockTree test) . AF.headHash . svSelectedChain)

-- | Tests that the selection of the node under test *does
-- not* change branches when sent a rollback to a block strictly older than 'k'
-- blocks before the current selection.
testCannotRollback ::
  ( IssueTestBlock blk
  , AF.HasHeader blk
  , AF.HasHeader (Header blk)
  , Eq blk
  ) =>
  ConformanceTest blk
testCannotRollback =
  mkConformanceTest
    "cannot rollback"
    adjustTestCount
    adjustMaxSize
    ( do
        gt@GenesisTest{gtSecurityParam, gtBlockTree} <- genChains (pure 1)
        pure
          gt
            { gtSchedule =
                rollbackSchedule (fromIntegral (unNonZero $ maxRollbacks gtSecurityParam) + 1) gtBlockTree
            }
    )
    defaultSchedulerConfig
    -- No shrinking because the schedule is tiny and hand-crafted
    mempty
    (\test -> hashOnTrunk (gtBlockTree test) . AF.headHash . svSelectedChain)

-- | A schedule that advertises all the points of the trunk up until the nth
-- block after the intersection, then switches to the first alternative
-- chain of the given block tree.
--
-- PRECONDITION: Block tree with at least one alternative chain.
rollbackSchedule :: AF.HasHeader blk => Int -> BlockTree blk -> PointSchedule blk
rollbackSchedule n blockTree =
  let branch = case btBranches blockTree of
        [b] -> b
        _ -> error "The block tree must have exactly one alternative branch"
      trunkSuffix = AF.takeOldest n (btbTrunkSuffix branch)
      schedulePoints =
        concat
          [ banalSchedulePoints (btbPrefix branch)
          , banalSchedulePoints trunkSuffix
          , banalSchedulePoints (btbSuffix branch)
          ]
   in PointSchedule
        { psSchedule = peersOnlyHonest $ zip (map (Time . (/ 30)) [0 ..]) schedulePoints
        , psStartOrder = []
        , psMinEndTime = Time 0
        }
 where
  banalSchedulePoints :: AnchoredFragment blk -> [SchedulePoint blk]
  banalSchedulePoints = concatMap banalSchedulePoints' . toOldestFirst
  banalSchedulePoints' :: blk -> [SchedulePoint blk]
  banalSchedulePoints' block = [scheduleTipPoint block, scheduleHeaderPoint block, scheduleBlockPoint block]

-- | Given a hash, checks whether its corresponding block is on the trunk of
-- the block tree.
hashOnTrunk :: (AF.HasHeader blk, Eq blk) => BlockTree blk -> ChainHash (Header blk) -> Bool
hashOnTrunk _ GenesisHash = True
hashOnTrunk bt (BlockHash hash) = do
  case M.lookup hash (deforestBlockTree bt) of
    Nothing -> False
    Just path -> AF.isPrefixOf path $ btTrunk bt
