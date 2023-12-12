{-# LANGUAGE NamedFieldPuns #-}

module Test.Consensus.PointSchedule.Shrink (
    shrinkAlertPointSchedule
  , smartShrinkAlertPointSchedule
  , smartShrinkPointSchedule
  ) where

import           Data.Function ((&))
import           Data.List (isPrefixOf, isSuffixOf, sortOn)
import           Data.List.NonEmpty (toList)
import           Data.Maybe (mapMaybe)
import           Ouroboros.Consensus.Block (blockHash, blockSlot)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     AnchoredSeq (..), headPoint)
import           Ouroboros.Network.Block (Point)
import           Test.Consensus.BlockTree (BlockTree (..), addBranch',
                     btbSuffix, mkTrunk)
import           Test.Consensus.PeerSimulator.StateView (StateView)
import           Test.Consensus.PointSchedule
import           Test.QuickCheck (shrinkList)
import           Test.Util.TestBlock (TestBlock, TestHash (unTestHash))

--------------------------------------------------------------------------------
-- Public interface
--------------------------------------------------------------------------------

-- | Shrink an alert 'PointSchedule' in a generic way; we call a 'PointSchedule'
-- “alert” when it contains an honest peer that behaves in an alert way. This
-- shrinking function always yield alert 'PointSchedules'.
shrinkAlertPointSchedule :: BlockTree TestBlock -> PointSchedule -> [PointSchedule]
shrinkAlertPointSchedule bt ps =
  filter (isAlertPointSchedule bt) $
    shrinkTicksList bt ps

-- | Shrink a 'PointSchedule' in a smart way: this function assumes nothing on
-- the given 'PointSchedule' but it also gets a 'StateView' which allows it to
-- inspect the run that this 'PointSchedule' triggered. This can help it take
-- smart decisions of shrunk 'PointSchedule's which we know have big chances to
-- trigger the same error.
smartShrinkPointSchedule :: BlockTree TestBlock -> PointSchedule -> StateView -> [PointSchedule]
smartShrinkPointSchedule _bt _ps _sv =
  []

-- | The best of both worlds: shrink a 'PointSchedule' in a smart way but fall
-- back on generic shrinking of alert 'PointSchedule's if it does not work.
smartShrinkAlertPointSchedule :: BlockTree TestBlock -> PointSchedule -> StateView -> [PointSchedule]
smartShrinkAlertPointSchedule bt ps sv =
  smartShrinkPointSchedule bt ps sv ++ shrinkAlertPointSchedule bt ps

--------------------------------------------------------------------------------
-- Alertness check
--------------------------------------------------------------------------------

-- | Given a block tree and an honest node's tick, check whether the honest peer
-- will be alert enough. NOTE: For now, this just means that it serves all of
-- its blocks; when LoP gets implemented, this will have to be revised.
honestNodeIsAlert :: BlockTree TestBlock -> [Tick] -> Bool
honestNodeIsAlert blockTree ticks =
    let point = headPoint (btTrunk blockTree)
     in any (\Tick{active=Peer{value}} -> nodeStateServes point value) ticks
  where
    -- | Whether the given 'NodeState' serves the given 'Point', that is whether
    -- both its 'HeaderPoint' and 'BlockPoint' are the given 'Point'.
    nodeStateServes :: Point TestBlock -> NodeState -> Bool
    nodeStateServes _ NodeOffline = False
    nodeStateServes point (NodeOnline AdvertisedPoints{header, block}) =
      headerPointPoint header == point && blockPointPoint block == point

-- | Whether a point schedule exhibits reasonable behaviour of the peers. For
-- instance, the honest peer cannot be too slow.
isAlertPointSchedule :: BlockTree TestBlock -> PointSchedule -> Bool
isAlertPointSchedule blockTree PointSchedule{ticks} =
  toList ticks
    & filter (\Tick{active=Peer{name}} -> name == HonestPeer)
    & honestNodeIsAlert blockTree

--------------------------------------------------------------------------------
-- Generic shrinkers
--------------------------------------------------------------------------------

-- | Shrinks the ticks list using QuickCheck's 'shrinkList'.
shrinkTicksList :: BlockTree TestBlock -> PointSchedule -> [PointSchedule]
shrinkTicksList _bt PointSchedule{ticks, peerIds} =
  mapMaybe (flip pointSchedule peerIds) (shrinkList (const []) (toList ticks))

--------------------------------------------------------------------------------
-- Smart shrinkers
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Other utilities
--------------------------------------------------------------------------------

-- | Remove blocks from the given block tree that are not necessary for the
-- given point schedule. If entire branches are unused, they are removed. If the
-- trunk is unused, then it remains as an empty anchored fragment.
trimBlockTree :: BlockTree TestBlock -> PointSchedule -> BlockTree TestBlock
trimBlockTree bt ps =
    let youngest = selectYoungest (reverse (pointScheduleBlocks ps))
        trunk = trimFragment youngest (btTrunk bt)
        branches = mapMaybe (fragmentToMaybe . trimFragment youngest . btbSuffix) (btBranches bt)
     in foldr addBranch' (mkTrunk trunk) branches
  where
    fragmentToMaybe (Empty _) = Nothing
    fragmentToMaybe fragment  = Just fragment

    -- | Given a list of blocks and a fragment, cut the fragment such that it
    -- contains only blocks that are ancestors of blocks in the list.
    trimFragment :: [TestBlock] -> AnchoredFragment TestBlock -> AnchoredFragment TestBlock
    trimFragment _ fragment@(Empty _) = fragment
    trimFragment youngest (fragment :> block)
      | any (block `isOlderThan`) youngest = fragment :> block
      | otherwise = trimFragment youngest fragment

    -- | Return a subset of the given block containing youngest elements. It is
    -- not guaranteed that this set is minimal. It is however guaranteed that
    -- any block in the input list is an ancestor of a block in the output list.
    selectYoungest :: [TestBlock] -> [TestBlock]
    selectYoungest = go [] . sortOn blockSlot
      where
        go youngest [] = youngest
        go youngest (block : blocks)
          | any (`isYoungerThan` block) youngest = go youngest blocks
          | otherwise = go (block : youngest) blocks

    -- | Partial comparison of blocks. A block is older than another block if it
    -- is its ancestor. For test blocks, this can be seen in the hash.
    isOlderThan :: TestBlock -> TestBlock -> Bool
    isOlderThan b1 b2 =
      toList (unTestHash (blockHash b1))
        `isPrefixOf`
      toList (unTestHash (blockHash b2))

    isYoungerThan :: TestBlock -> TestBlock -> Bool
    isYoungerThan b1 b2 =
      toList (unTestHash (blockHash b1))
        `isSuffixOf`
      toList (unTestHash (blockHash b2))
