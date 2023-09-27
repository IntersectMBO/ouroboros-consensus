{-# LANGUAGE LambdaCase      #-}
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
  , allFragments
  , findFragment
  , findPath
  , mkTrunk
  , prettyPrint
  ) where

import           Cardano.Slotting.Block (BlockNo)
import           Cardano.Slotting.Slot (SlotNo (unSlotNo))
import           Data.Function ((&))
import           Data.Maybe (fromJust, fromMaybe)
import qualified Data.Vector as Vector
import           Ouroboros.Consensus.Block.Abstract (blockNo, blockSlot,
                     unBlockNo)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Text.Printf (printf)

-- | Represent a branch of a block tree by a prefix and a suffix. The full
-- fragment (the prefix and suffix catenated) is provided for practicality.
--
-- INVARIANT: the head of @btbPrefix@ is the anchor of @btbSuffix@.
--
-- INVARIANT: @btbFull == fromJust $ AF.join btbPrefix btbSuffix@
data BlockTreeBranch blk = BlockTreeBranch {
    btbPrefix :: AF.AnchoredFragment blk,
    btbSuffix :: AF.AnchoredFragment blk,
    btbFull   :: AF.AnchoredFragment blk
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
-- block in common with an existingbranch.
addBranch :: AF.HasHeader blk => AF.AnchoredFragment blk -> BlockTree blk -> Maybe (BlockTree blk)
addBranch branch BlockTree{..} = do
  (_, btbPrefix, _, btbSuffix) <- AF.intersect btTrunk branch
  -- NOTE: We could use the monadic bind for @Maybe@ here but we would rather
  -- catch bugs quicker.
  let btbFull = fromJust $ AF.join btbPrefix btbSuffix
  pure $ BlockTree { btTrunk, btBranches = BlockTreeBranch { .. } : btBranches }

-- | Same as @addBranch@ but assumes that the precondition holds.
addBranch' :: AF.HasHeader blk => AF.AnchoredFragment blk -> BlockTree blk -> BlockTree blk
addBranch' branch blockTree =
  fromMaybe (error "addBranch': precondition does not hold") $ addBranch branch blockTree

-- | Return all the full fragments from the root of the tree.
allFragments :: BlockTree blk -> [AF.AnchoredFragment blk]
allFragments BlockTree{..} = btTrunk : map btbFull btBranches

firstJust :: [Maybe a] -> Maybe a
firstJust []               = Nothing
firstJust (Nothing : rest) = firstJust rest
firstJust (Just x : _)     = Just x

-- | Look for a point in the block tree and return a fragment going from the
-- root of the tree to the point in question.
findFragment :: AF.HasHeader blk => AF.Point blk -> BlockTree blk -> Maybe (AF.AnchoredFragment blk)
findFragment point blockTree =
  fst <$> firstJust (map (\fragment -> AF.splitAfterPoint fragment point) $ allFragments blockTree)

-- | @findPath source target blockTree@ finds a path from the @source@ point to
-- the @target@ point in the @blockTree@, or return @Nothing@. A path is a
-- fragment anchored at the @source@ or an ancestor of the @source@. One can
-- distinguish three cases:
--
-- - the fragment is anchored at the @source@: all the blocks are descendants of
--   the source; serving this fragment would only require rolling forward;
--
-- - the fragment is not anchored at the @source@ and is empty: it is then in
--   fact anchored at the @target@ which is an ancestor of the source;
--
-- - the fragment is not anchored at the @source@ and is not empty: serving this
--   fragment would require rolling backwards to the anchor.
findPath :: AF.HasHeader blk => AF.Point blk -> AF.Point blk -> BlockTree blk -> Maybe (AF.AnchoredFragment blk)
findPath source target blockTree = do
  sourceFragment <- findFragment source blockTree
  targetFragment <- findFragment target blockTree
  (_, _, _, targetSuffix) <- AF.intersect sourceFragment targetFragment
  pure targetSuffix

prettyPrint :: AF.HasHeader blk => BlockTree blk -> [String]
prettyPrint blockTree = do
  let honestFragment = btTrunk blockTree
  let advFragment = btbSuffix $ head $ btBranches blockTree

  let (oSlotNo, oBlockNo) = slotAndBlockNoFromAnchor $ AF.anchor honestFragment
  let (hSlotNo, _) = slotAndBlockNoFromAnchor $ AF.headAnchor honestFragment

  let (aoSlotNo, _) = slotAndBlockNoFromAnchor $ AF.anchor advFragment
  let (ahSlotNo, _) = slotAndBlockNoFromAnchor $ AF.headAnchor advFragment

  let firstSlotNo = min oSlotNo aoSlotNo
  let lastSlotNo = max hSlotNo ahSlotNo

  -- FIXME: only handles two fragments at this point. not very hard to make it
  -- handle all of them. Some work needed to make it handle all of them _in a
  -- clean way_.

  [ "Block tree:"
    ,

    [firstSlotNo .. lastSlotNo]
      & map (printf "%2d" . unSlotNo)
      & unwords
      & ("  slots: " ++)
    ,

    honestFragment
      & AF.toOldestFirst
      & map (\block -> (fromIntegral (unSlotNo (blockSlot block) - 1), Just (unBlockNo (blockNo block))))
      & Vector.toList . (Vector.replicate (fromIntegral (unSlotNo hSlotNo - unSlotNo oSlotNo)) Nothing Vector.//)
      & map (maybe "  " (printf "%2d"))
      & unwords
      & map (\c -> if c == ' ' then '─' else c)
      & ("─" ++)
      & (printf "%2d" (unBlockNo oBlockNo) ++)
      & ("  trunk: " ++)
    ,

    advFragment
      & AF.toOldestFirst
      & map (\block -> (fromIntegral (unSlotNo (blockSlot block) - unSlotNo aoSlotNo - 1), Just (unBlockNo (blockNo block))))
      & Vector.toList . (Vector.replicate (fromIntegral (unSlotNo ahSlotNo - unSlotNo aoSlotNo)) Nothing Vector.//)
      & map (maybe "  " (printf "%2d"))
      & unwords
      & map (\c -> if c == ' ' then '─' else c)
      & (" ╰─" ++)
      & (replicate (3 * fromIntegral (unSlotNo (aoSlotNo - oSlotNo))) ' ' ++)
      & ("         " ++)
    ]

  where
    slotAndBlockNoFromAnchor :: AF.Anchor b -> (SlotNo, BlockNo)
    slotAndBlockNoFromAnchor = \case
      AF.AnchorGenesis -> (0, 0)
      AF.Anchor slotNo _ blockNo' -> (slotNo, blockNo')
