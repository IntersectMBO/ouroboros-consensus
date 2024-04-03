{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Consensus.PointSchedule.Shrinking (
    shrinkByRemovingAdversaries
  , shrinkPeerSchedules
  , trimBlockTree'
  ) where

import           Data.Containers.ListUtils (nubOrd)
import           Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     AnchoredSeq (Empty), takeWhileOldest)
import           Test.Consensus.BlockTree (BlockTree (..), BlockTreeBranch (..),
                     addBranch', mkTrunk)
import           Test.Consensus.PeerSimulator.StateView (StateView)
import           Test.Consensus.PointSchedule
                     (GenesisTest (gtBlockTree, gtSchedule), GenesisTestFull,
                     PeerSchedule, PeersSchedule, peerSchedulesBlocks)
import           Test.Consensus.PointSchedule.Peers (Peers (..))
import           Test.Consensus.PointSchedule.SinglePeer (SchedulePoint (..))
import           Test.QuickCheck (shrinkList)
import           Test.Util.TestBlock (TestBlock, isAncestorOf,
                     isStrictAncestorOf)

-- | Shrink a 'Peers PeerSchedule'. This does not affect the honest peer; it
-- does, however, attempt to remove other peers or ticks of other peers. The
-- block tree is trimmed to keep only parts that are necessary for the shrunk
-- schedule.
shrinkPeerSchedules ::
  GenesisTestFull TestBlock ->
  StateView TestBlock ->
  [GenesisTestFull TestBlock]
shrinkPeerSchedules genesisTest _stateView =
  let trimmedBlockTree sch = trimBlockTree' sch (gtBlockTree genesisTest)
      shrunkOthers = shrinkOtherPeers shrinkPeerSchedule (gtSchedule genesisTest) <&>
        \shrunkSchedule -> genesisTest
          { gtSchedule = shrunkSchedule
          , gtBlockTree = trimmedBlockTree shrunkSchedule
          }
      shrunkHonest = shrinkHonestPeer shrinkHonestSchedule (gtSchedule genesisTest) <&>
        -- No need to update the tree here, shrinking the honest peer never discards blocks
        \shrunkSchedule -> genesisTest {gtSchedule = shrunkSchedule}
  in shrunkOthers ++ shrunkHonest

-- | Shrink a 'Peers PeerSchedule' by removing adversaries. This does not affect
-- the honest peer; and it does not remove ticks from the schedules of the
-- remaining adversaries.
shrinkByRemovingAdversaries ::
  GenesisTestFull TestBlock ->
  StateView TestBlock ->
  [GenesisTestFull TestBlock]
shrinkByRemovingAdversaries genesisTest _stateView =
  shrinkOtherPeers (const []) (gtSchedule genesisTest) <&> \shrunkSchedule ->
    let trimmedBlockTree = trimBlockTree' shrunkSchedule (gtBlockTree genesisTest)
     in (genesisTest{gtSchedule = shrunkSchedule, gtBlockTree = trimmedBlockTree})

-- | Shrink a 'PeerSchedule' by removing ticks from it. The other ticks are kept
-- unchanged.
shrinkPeerSchedule :: (PeerSchedule blk) -> [PeerSchedule blk]
shrinkPeerSchedule = shrinkList (const [])

-- | Shrink the 'others' field of a 'Peers' structure by attempting to remove
-- peers or by shrinking their values using the given shrinking function.
shrinkOtherPeers :: (a -> [a]) -> Peers a -> [Peers a]
shrinkOtherPeers shrink Peers{honest, others} =
  map (Peers honest . Map.fromList) $
    shrinkList (traverse (traverse shrink)) $ Map.toList others

-- | Shrink on the `honest` field of a `Peers` structure using the given shrinking function
shrinkHonestPeer :: (a -> [a]) -> Peers a -> [Peers a]
shrinkHonestPeer shrink Peers{honest, others} =
  map (\h -> Peers h others) $ traverse shrink honest

-- | We shrink an honest peer schedule by either:
-- * locally re-ordering it (with TipPoint < HeaderPoint < BlockPoint),
-- * collapsing a point with it predecessor if it has the same type.
-- With the additional restriction that we never collapse the last point,
-- in order to keep the same end time.
shrinkHonestSchedule :: (PeerSchedule blk) -> [PeerSchedule blk]
shrinkHonestSchedule sch =
  mapMaybe
    (\case
      (hd, pre:suc:guard:tl) -> case (snd pre, snd suc) of
          (ScheduleTipPoint    _, ScheduleTipPoint    _) -> collapse
          (ScheduleTipPoint    _, ScheduleHeaderPoint _) -> Nothing
          (ScheduleTipPoint    _, ScheduleBlockPoint  _) -> Nothing
          (ScheduleHeaderPoint _, ScheduleTipPoint    _) -> reorder
          (ScheduleHeaderPoint _, ScheduleHeaderPoint _) -> collapse
          (ScheduleHeaderPoint _, ScheduleBlockPoint  _) -> Nothing
          (ScheduleBlockPoint  _, ScheduleTipPoint    _) -> reorder
          (ScheduleBlockPoint  _, ScheduleHeaderPoint _) -> reorder
          (ScheduleBlockPoint  _, ScheduleBlockPoint  _) -> collapse
        where
          (timePre, pointPre) = pre
          (timeSuc, pointSuc) = suc
          reorder  = Just $ hd ++ (timePre, pointSuc):(timeSuc, pointPre):guard:tl
          collapse = Just $ hd ++ (timePre, pointSuc)                    :guard:tl
      _ -> Nothing
    )
    (map (\i -> splitAt i sch) [0..length sch])

-- | Remove blocks from the given block tree that are not necessary for the
-- given peer schedules. If entire branches are unused, they are removed. If the
-- trunk is unused, then it remains as an empty anchored fragment.
trimBlockTree' :: PeersSchedule TestBlock -> BlockTree TestBlock -> BlockTree TestBlock
trimBlockTree' = keepOnlyAncestorsOf . peerSchedulesBlocks

-- | Given some blocks and a block tree, keep only the prefix of the block tree
-- that contains ancestors of the given blocks.
keepOnlyAncestorsOf :: [TestBlock] -> BlockTree TestBlock -> BlockTree TestBlock
keepOnlyAncestorsOf blocks bt =
    let leaves = blocksWithoutDescendents blocks
        trunk = keepOnlyAncestorsOf' leaves (btTrunk bt)
        branches = mapMaybe (fragmentToMaybe . keepOnlyAncestorsOf' leaves . btbSuffix) (btBranches bt)
     in foldr addBranch' (mkTrunk trunk) branches
  where
    fragmentToMaybe (Empty _) = Nothing
    fragmentToMaybe fragment  = Just fragment

    -- | Given some blocks and a fragment, keep only the prefix of the fragment
    -- that contains ancestors of the given blocks.
    keepOnlyAncestorsOf' :: [TestBlock] -> AnchoredFragment TestBlock -> AnchoredFragment TestBlock
    keepOnlyAncestorsOf' leaves = takeWhileOldest (\block -> (block `isAncestorOf`) `any` leaves)

    -- | Return a subset of the given blocks containing only the ones that do
    -- not have any other descendents in the set.
    blocksWithoutDescendents :: [TestBlock] -> [TestBlock]
    blocksWithoutDescendents bs =
      let bs' = nubOrd bs
       in [ b | b <- bs', not ((b `isStrictAncestorOf`) `any` bs') ]
