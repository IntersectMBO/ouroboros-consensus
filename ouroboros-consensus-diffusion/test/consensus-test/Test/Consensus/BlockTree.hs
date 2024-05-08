{-# LANGUAGE CPP             #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

-- REVIEW: There is a `BlockTree` in `Test.Utils.TestBlock`. It relies on
-- different mechanisms but maybe we should rely on that instead to avoid
-- duplication.

module Test.Consensus.BlockTree (
    BlockTree (..)
  , BlockTreeBranch (..)
  , PathAnchoredAtSource (..)
  , addBranch
  , addBranch'
  , allFragments
  , findFragment
  , findPath
  , mkTrunk
  , prettyBlockTree
  ) where

import           Cardano.Slotting.Slot (SlotNo (unSlotNo))
import           Data.Foldable (asum)
import           Data.Function ((&))
import           Data.Functor ((<&>))
import           Data.Maybe (fromJust, fromMaybe)
import qualified Data.Vector as Vector
import           Ouroboros.Consensus.Block.Abstract (blockNo, blockSlot,
                     fromWithOrigin, unBlockNo)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Text.Printf (printf)

-- | Represent a branch of a block tree by a prefix and a suffix. The full
-- fragment (the prefix and suffix catenated) and the trunk suffix (the rest of
-- the trunk after the branch forks off) are provided for practicality.
--
-- INVARIANT: the head of @btbPrefix@ is the anchor of @btbSuffix@.
--
-- INVARIANT: @btbFull == fromJust $ AF.join btbPrefix btbSuffix@.
data BlockTreeBranch blk = BlockTreeBranch {
    btbPrefix      :: AF.AnchoredFragment blk,
    btbSuffix      :: AF.AnchoredFragment blk,
    btbTrunkSuffix :: AF.AnchoredFragment blk,
    btbFull        :: AF.AnchoredFragment blk
  }
  deriving (Show)

-- | Represent a block tree with a main trunk and branches leaving from the
-- trunk in question. All the branches are represented by their prefix to and
-- suffix from the intersection point.
--
-- INVARIANT: The branches' prefixes share the same anchor as the trunk and are
-- fully contained in the trunk.
--
-- INVARIANT: The branches' suffixes are anchored in the trunk and do not
-- contain any blocks in common with the trunk.
--
-- INVARIANT: The branches' suffixes do not contain any block in common with one
-- another.
--
-- INVARIANT: for all @BlockTreeBranch{..}@ in the tree, @btTrunk == fromJust $
-- AF.join btbPrefix btbTrunkSuffix@.
--
-- REVIEW: Find another name so as not to clash with 'BlockTree' from
-- `unstable-consensus-testlib/Test/Util/TestBlock.hs`.
data BlockTree blk = BlockTree {
    btTrunk    :: AF.AnchoredFragment blk,
    btBranches :: [BlockTreeBranch blk]
  }
  deriving (Show)

-- | Make a block tree made of only a trunk.
mkTrunk :: AF.AnchoredFragment blk -> BlockTree blk
mkTrunk btTrunk = BlockTree { btTrunk, btBranches = [] }

-- | Add a branch to an existing block tree.
--
-- PRECONDITION: The given fragment intersects with the trunk or its anchor.
--
-- FIXME: we should enforce that the branch's prefix shares the same anchor as
-- the trunk.
--
-- FIXME: we should enforce that the new branch' suffix does not contain any
-- block in common with an existing branch.
addBranch :: AF.HasHeader blk => AF.AnchoredFragment blk -> BlockTree blk -> Maybe (BlockTree blk)
addBranch branch bt = do
  (btbPrefix, _, btbTrunkSuffix, btbSuffix) <- AF.intersect (btTrunk bt) branch
  -- NOTE: We could use the monadic bind for @Maybe@ here but we would rather
  -- catch bugs quicker.
  let btbFull = fromJust $ AF.join btbPrefix btbSuffix
  pure $ bt { btBranches = BlockTreeBranch { .. } : btBranches bt }

-- | Same as @addBranch@ but assumes that the precondition holds.
addBranch' :: AF.HasHeader blk => AF.AnchoredFragment blk -> BlockTree blk -> BlockTree blk
addBranch' branch blockTree =
  fromMaybe (error "addBranch': precondition does not hold") $ addBranch branch blockTree

-- | Return all the full fragments from the root of the tree.
allFragments :: BlockTree blk -> [AF.AnchoredFragment blk]
allFragments bt = btTrunk bt : map btbFull (btBranches bt)

-- | Look for a point in the block tree and return a fragment going from the
-- root of the tree to the point in question.
findFragment :: AF.HasHeader blk => AF.Point blk -> BlockTree blk -> Maybe (AF.AnchoredFragment blk)
findFragment point blockTree =
  allFragments blockTree
    & map (\fragment -> AF.splitAfterPoint fragment point)
    & asum
    <&> fst

-- | See 'findPath'.
newtype PathAnchoredAtSource = PathAnchoredAtSource Bool

-- | @findPath source target blockTree@ finds a path from the @source@ point to
-- the @target@ point in the @blockTree@ and returns it as an anchored fragment
-- It returns @Nothing@ when either of @source@ are @target@ are not in the
-- 'BlockTree'. There are two interesting properties on this fragment:
--
--   1. Whether the returned fragment is anchored at the @source@.
--   2. Whether the returned fragment is empty.
--
-- Together, those two properties form four interesting cases:
--
--   a. If the fragment is anchored at the @source@ and is empty, then @source
--      == target@.
--
--   b. If the fragment is anchored at the @source@ and is not empty, then
--      @source@ is an ancestor of @target@ and the fragment contains all the
--      blocks between them, @target@ included.
--
--   c. If the fragment is not anchored at the @source@ and is empty, then
--      @target@ is an ancestor of @source@.
--
--   d. If the fragment is not anchored at the @source@ and is not empty, then
--      it is anchored at the youngest common ancestor of both @source@ and
--      @target@ and contains all the blocks between that ancestor and @target@.
findPath ::
  AF.HasHeader blk =>
  AF.Point blk ->
  AF.Point blk ->
  BlockTree blk ->
  Maybe (PathAnchoredAtSource, AF.AnchoredFragment blk)
findPath source target blockTree = do
  sourceFragment <- findFragment source blockTree
  targetFragment <- findFragment target blockTree
  (_, _, _, targetSuffix) <- AF.intersect sourceFragment targetFragment
  pure (
    PathAnchoredAtSource (AF.anchorPoint targetSuffix == source),
    targetSuffix
    )

-- | Pretty prints a block tree for human readability. For instance:
--
--     slots:  0  1  2  3  4  5  6  7  8  9
--     trunk:  0─────1──2──3──4─────5──6──7
--                      ╰─────3──4─────5
--
-- Returns a list of strings intended to be catenated with a newline.
prettyBlockTree :: AF.HasHeader blk => BlockTree blk -> [String]
prettyBlockTree blockTree =
  ["slots:   " ++ unwords (map (printf "%2d" . unSlotNo) [veryFirstSlot .. veryLastSlot])]
    ++ [printTrunk honestFragment]
    ++ (map printBranch adversarialFragments)

  where
    honestFragment = btTrunk blockTree
    adversarialFragments = map btbSuffix (btBranches blockTree)

    veryFirstSlot = firstNo $ honestFragment

    veryLastSlot =
      foldl max 0 $
        map
          lastNo
          (honestFragment : adversarialFragments)

    printTrunk :: AF.HasHeader blk => AF.AnchoredFragment blk -> String
    printTrunk = printLine (\_ -> "trunk:  ─")

    printBranch :: AF.HasHeader blk => AF.AnchoredFragment blk -> String
    printBranch = printLine $ \firstSlot ->
      "      " ++ replicate (3 * fromIntegral (unSlotNo (firstSlot - veryFirstSlot))) ' ' ++ " ╰─"

    printLine :: AF.HasHeader blk => (SlotNo -> String) -> AF.AnchoredFragment blk -> String
    printLine printHeader fragment =
      let firstSlot = firstNo fragment
          lastSlot = lastNo fragment
      in printHeader firstSlot ++ printFragment firstSlot lastSlot fragment

    printFragment :: AF.HasHeader blk => SlotNo -> SlotNo -> AF.AnchoredFragment blk -> String
    printFragment firstSlot lastSlot fragment =
      fragment
        & AF.toOldestFirst
        -- Turn the fragment into a list of (SlotNo, Just BlockNo)
        & map (\block -> (fromIntegral (unSlotNo (blockSlot block) - unSlotNo firstSlot), Just (unBlockNo (blockNo block))))
        -- Update only the Vector elements that have blocks in them
        & Vector.toList . (slotRange Vector.//)
        & map (maybe "  " (printf "%2d"))
        & unwords
        & map (\c -> if c == ' ' then '─' else c)
      where
        -- Initialize a Vector with the length of the fragment containing only Nothings
        slotRange = Vector.replicate (fromIntegral (unSlotNo lastSlot - unSlotNo firstSlot + 1)) Nothing

    lastNo ::  AF.HasHeader blk => AF.AnchoredFragment blk -> SlotNo
    lastNo = fromWithOrigin 0 . AF.headSlot

    firstNo :: AF.AnchoredFragment blk -> SlotNo
    firstNo frag =
      case AF.anchor frag of
        AF.AnchorGenesis     -> 0
        AF.Anchor slotNo _ _ -> slotNo + 1
