{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

-- REVIEW: There is a `BlockTree` in `Test.Utils.TestBlock`. It relies on
-- different mechanisms but maybe we should rely on that instead to avoid
-- duplication.

module Test.Ouroboros.Consensus.ChainGenerator.Tests.BlockTree (
    BlockTree (..)
  , BlockTreeBranch (..)
  , addBranch
  , addBranch'
  , mkTrunk
  , slotLength
  ) where

import           Cardano.Slotting.Slot (SlotNo (unSlotNo), withOrigin)
import           Data.Maybe (fromJust, fromMaybe)
import qualified Ouroboros.Network.AnchoredFragment as AF

-- | Represent a branch of a block tree. We provide:
--
-- - its prefix, whose anchor is the same as the trunk and whose head is the
--   intersection point for this specific branch.
--
-- - its suffix, whose anchor is the intersection point.
--
-- - the full fragment, that is the prefix and suffix catenated.
data BlockTreeBranch blk = BlockTreeBranch {
    prefix :: AF.AnchoredFragment blk,
    suffix :: AF.AnchoredFragment blk,
    full   :: AF.AnchoredFragment blk
  }

-- | Represent a block tree with a main trunk and branches leaving from the
-- trunk in question. All the branches are represented by their prefix to and
-- suffix from the intersection point.
--
-- INVARIANT: The branches are anchored in a block of the trunk or have the same
-- anchor as the trunk.
--
-- INVARIANT: The branches do not contain any block (beside the anchor) that is
-- present in the trunk.
data BlockTree blk = BlockTree {
    trunk    :: AF.AnchoredFragment blk,
    branches :: [BlockTreeBranch blk]
  }

-- | Make a block tree made of only a trunk.
mkTrunk :: AF.AnchoredFragment blk -> BlockTree blk
mkTrunk trunk = BlockTree { trunk, branches = [] }

-- | Add a branch to a block tree. The given fragment has to be anchored in
--
-- PRECONDITION: The given fragment intersects with the trunk or the anchor of
-- the trunk.
addBranch :: AF.HasHeader blk => AF.AnchoredFragment blk -> BlockTree blk -> Maybe (BlockTree blk)
addBranch branch BlockTree{..} = do
  (_, prefix, _, suffix) <- AF.intersect trunk branch
  -- NOTE: We could use the monadic bind for @Maybe@ here but we would rather
  -- catch bugs quicker.
  let full = fromJust $ AF.join prefix suffix
  pure $ BlockTree { trunk, branches = BlockTreeBranch { .. } : branches }

-- | Same as @addBranch@ but assumes that the precondition holds.
addBranch' :: AF.HasHeader blk => AF.AnchoredFragment blk -> BlockTree blk -> BlockTree blk
addBranch' branch blockTree =
  fromMaybe (error "addBranch': precondition does not hold") $ addBranch branch blockTree

-- | Number of slots on which the fragment span.
--
-- REVIEW: Should be put somewhere else but is here in lack of a better place.
slotLength :: AF.HasHeader blk => AF.AnchoredFragment blk -> Int
slotLength fragment =
  fromIntegral $ unSlotNo $
    withOrigin 0 id (AF.anchorToSlotNo $ AF.headAnchor fragment)
    - withOrigin 0 id (AF.anchorToSlotNo $ AF.anchor fragment)
