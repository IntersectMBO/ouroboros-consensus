{-# LANGUAGE NamedFieldPuns #-}

module Test.Consensus.PointSchedule.Shrinking (shrinkPeerSchedules) where

import           Data.Functor ((<&>))
import           Data.List (isSuffixOf, sortOn)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     AnchoredSeq (Empty, (:>)))
import           Ouroboros.Network.Block (blockHash, blockSlot)
import           Test.Consensus.BlockTree (BlockTree (..), BlockTreeBranch (..),
                     addBranch', mkTrunk)
import           Test.Consensus.PeerSimulator.StateView (StateView)
import           Test.Consensus.PointSchedule (GenesisTest (gtBlockTree),
                     PeerSchedule, PointSchedule, fromSchedulePoints,
                     pointScheduleBlocks)
import           Test.Consensus.PointSchedule.Peers (Peers (..))
import           Test.QuickCheck (shrinkList)
import           Test.Util.TestBlock (TestBlock, TestHash (unTestHash))

-- | Shrink a 'Peers PeerSchedule'. This does not affect the honest peer; it
-- does, however, attempt to remove other peers or ticks of other peers. The
-- block tree is trimmed to keep only parts that are necessary for the shrunk
-- schedule.
shrinkPeerSchedules ::
  GenesisTest ->
  Peers PeerSchedule ->
  StateView ->
  [(GenesisTest, Peers PeerSchedule)]
shrinkPeerSchedules genesisTest schedule _stateView =
  shrinkOtherPeers shrinkPeerSchedule schedule <&> \shrunkSchedule ->
    let trimmedBlockTree = trimBlockTree (gtBlockTree genesisTest) (fromSchedulePoints shrunkSchedule)
     in (genesisTest{gtBlockTree = trimmedBlockTree}, shrunkSchedule)

-- | Shrink a 'PeerSchedule' by removing ticks from it. The other ticks are kept
-- unchanged.
shrinkPeerSchedule :: PeerSchedule -> [PeerSchedule]
shrinkPeerSchedule = shrinkList (const [])

-- | Shrink the 'others' field of a 'Peers' structure by attempting to remove
-- peers or by shrinking their values using the given shrinking function.
shrinkOtherPeers :: (a -> [a]) -> Peers a -> [Peers a]
shrinkOtherPeers shrink Peers{honest, others} =
  map (Peers honest . Map.fromList) $
    shrinkList (traverse (traverse shrink)) $ Map.toList others

-- | Remove blocks from the given block tree that are not necessary for the
-- given point schedule. If entire branches are unused, they are removed. If the
-- trunk is unused, then it remains as an empty anchored fragment.
trimBlockTree :: BlockTree TestBlock -> PointSchedule -> BlockTree TestBlock
trimBlockTree bt ps =
    let youngest = selectYoungest (pointScheduleBlocks ps)
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
    selectYoungest =
        -- NOTE: We process blocks from the biggest slot to the smallest; this
        -- should make us select only the tip of each chain.
        go [] . reverse . sortOn blockSlot
      where
        go youngest [] = youngest
        go youngest (block : blocks)
          | any (`isYoungerThan` block) youngest = go youngest blocks
          | otherwise = go (block : youngest) blocks

    -- | Partial comparison of blocks. A block is older than another block if it
    -- is its ancestor. For test blocks, this can be seen in the hash.
    isOlderThan :: TestBlock -> TestBlock -> Bool
    isOlderThan b1 b2 =
      -- NOTE: 'unTestHash' returns the list of hash components _in reverse
      -- order_ so we need to test that one hash is the _suffix_ of the other.
      NonEmpty.toList (unTestHash (blockHash b1))
        `isSuffixOf`
      NonEmpty.toList (unTestHash (blockHash b2))

    -- | Partial comparison of blocks. @isYoungerThan = flip isOlderThan@.
    isYoungerThan :: TestBlock -> TestBlock -> Bool
    isYoungerThan = flip isOlderThan
