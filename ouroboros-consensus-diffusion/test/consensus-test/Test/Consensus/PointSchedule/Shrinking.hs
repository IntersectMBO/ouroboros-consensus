{-# LANGUAGE NamedFieldPuns #-}

module Test.Consensus.PointSchedule.Shrinking (shrinkPeerSchedules) where

import           Data.Functor ((<&>))
import           Data.List (sortOn)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     AnchoredSeq (Empty), takeWhileOldest)
import           Ouroboros.Network.Block (blockSlot)
import           Test.Consensus.BlockTree (BlockTree (..), BlockTreeBranch (..),
                     addBranch', mkTrunk)
import           Test.Consensus.PeerSimulator.StateView (StateView)
import           Test.Consensus.PointSchedule (GenesisTest (gtBlockTree),
                     PeerSchedule, peerSchedulesBlocks)
import           Test.Consensus.PointSchedule.Peers (Peers (..))
import           Test.QuickCheck (shrinkList)
import           Test.Util.TestBlock (TestBlock, isAncestorOf)

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
    let trimmedBlockTree = trimBlockTree' shrunkSchedule (gtBlockTree genesisTest)
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
-- given peer schedules. If entire branches are unused, they are removed. If the
-- trunk is unused, then it remains as an empty anchored fragment.
trimBlockTree' :: Peers PeerSchedule -> BlockTree TestBlock -> BlockTree TestBlock
trimBlockTree' = keepOnlyAncestorsOf . peerSchedulesBlocks

-- | Given some blocks and a block tree, keep only the prefix of the block tree
-- that contains ancestors of the given blocks.
keepOnlyAncestorsOf :: [TestBlock] -> BlockTree TestBlock -> BlockTree TestBlock
keepOnlyAncestorsOf blocks bt =
    let leafs = blocksWithoutDescendents blocks
        trunk = keepOnlyAncestorsOf' leafs (btTrunk bt)
        branches = mapMaybe (fragmentToMaybe . keepOnlyAncestorsOf' leafs . btbSuffix) (btBranches bt)
     in foldr addBranch' (mkTrunk trunk) branches
  where
    fragmentToMaybe (Empty _) = Nothing
    fragmentToMaybe fragment  = Just fragment

    -- | Given some blocks and a fragment, keep only the prefix of the fragment
    -- that contains ancestors of the given blocks.
    keepOnlyAncestorsOf' :: [TestBlock] -> AnchoredFragment TestBlock -> AnchoredFragment TestBlock
    keepOnlyAncestorsOf' leafs = takeWhileOldest (\block -> (block `isAncestorOf`) `any` leafs)

    -- | Return a subset of the given blocks containing only the ones that do
    -- not have any other descendents in the set.
    blocksWithoutDescendents :: [TestBlock] -> [TestBlock]
    blocksWithoutDescendents =
        -- NOTE: We process blocks from the biggest slot to the smallest; this
        -- should make us select only the tip of each chain.
        blocksWithoutPreviousDescendents . reverse . sortOn blockSlot
      where
        -- | Blocks that do not have any descendents earlier in the list.
        blocksWithoutPreviousDescendents =
          foldl
            (\leafs block ->
               if (block `isAncestorOf`) `any` leafs
                 then leafs
                 else block : leafs)
            []
