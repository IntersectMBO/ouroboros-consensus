{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

-- REVIEW: There is a `BlockTree` in `Test.Utils.TestBlock`. It relies on
-- different mechanisms but maybe we should rely on that instead to avoid
-- duplication.

module Test.Consensus.BlockTree
  ( BlockTree (BlockTree, btTrunk, btBranches)
  , BlockTreeBranch (..)
  , PathAnchoredAtSource (..)
  , addBranch
  , addBranch'
  , allFragments
  , deforestBlockTree
  , findFragment
  , findPath
  , isAncestorOf
  , isStrictAncestorOf
  , mkTrunk
  , nonemptyPrefixesOf
  , onTrunk
  , prettyBlockTree
  ) where

import Cardano.Slotting.Slot (SlotNo (unSlotNo), WithOrigin (..))
import Data.Foldable (asum, fold)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (inits, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Ord (Down (Down))
import qualified Data.Vector as Vector
import Ouroboros.Consensus.Block.Abstract
  ( GetHeader (..)
  , HasHeader
  , Header
  , HeaderHash
  , Point
  , blockHash
  , blockNo
  , blockSlot
  , fromWithOrigin
  , pointSlot
  , unBlockNo
  )
import qualified Ouroboros.Network.AnchoredFragment as AF
import Text.Printf (printf)

-- | Represent a branch of a block tree by a prefix and a suffix. The full
-- fragment (the prefix and suffix catenated) and the trunk suffix (the rest of
-- the trunk after the branch forks off) are provided for practicality.
--
-- INVARIANT: the head of @btbPrefix@ is the anchor of @btbSuffix@.
--
-- INVARIANT: @btbFull == fromJust $ AF.join btbPrefix btbSuffix@.
data BlockTreeBranch blk = BlockTreeBranch
  { btbPrefix :: AF.AnchoredFragment blk
  , btbSuffix :: AF.AnchoredFragment blk
  , btbTrunkSuffix :: AF.AnchoredFragment blk
  , btbFull :: AF.AnchoredFragment blk
  }
  deriving Show

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
-- INVARIANT: In @RawBlockTree trunk branches deforested@, we must have
-- @deforested == deforestRawBlockTree trunk branches@.
--
-- REVIEW: Find another name so as not to clash with 'BlockTree' from
-- `unstable-consensus-testlib/Test/Util/TestBlock.hs`.
data BlockTree blk = RawBlockTree
  { btTrunk' :: AF.AnchoredFragment blk
  , btBranches' :: [BlockTreeBranch blk]
  , -- Cached deforestation of the block tree. This gets queried
    -- many times and there's no reason to rebuild the tree every time.
    btDeforested :: DeforestedBlockTree blk
  }
  deriving Show

pattern BlockTree :: AF.AnchoredFragment blk -> [BlockTreeBranch blk] -> BlockTree blk
pattern BlockTree{btTrunk, btBranches} <- RawBlockTree btTrunk btBranches _

{-# COMPLETE BlockTree #-}

deforestBlockTree :: BlockTree blk -> DeforestedBlockTree blk
deforestBlockTree = btDeforested

-- Smart constructor to cache the deforested block tree at creation time.
mkBlockTree :: HasHeader blk => AF.AnchoredFragment blk -> [BlockTreeBranch blk] -> BlockTree blk
mkBlockTree trunk branches = RawBlockTree trunk branches (deforestRawBlockTree trunk branches)

-- | Make a block tree made of only a trunk.
mkTrunk :: HasHeader blk => AF.AnchoredFragment blk -> BlockTree blk
mkTrunk btTrunk = mkBlockTree btTrunk []

-- | Add a branch to an existing block tree.
--
-- Yields @Nothing@ if the given fragment does not intersect with the trunk or its anchor.
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
  pure $ mkBlockTree (btTrunk bt) (BlockTreeBranch{..} : btBranches bt)

-- | Same as @addBranch@ but calls to 'error' if the former yields 'Nothing'.
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
  pure
    ( PathAnchoredAtSource (AF.anchorPoint targetSuffix == source)
    , targetSuffix
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
    ++ (map (uncurry printBranch) adversarialFragments)
 where
  honestFragment = btTrunk blockTree
  adversarialFragments =
    sortOn (Down . pointSlot . AF.anchorPoint . snd) $
      zip [1 ..] $
        map btbSuffix (btBranches blockTree)

  veryFirstSlot = firstNo $ honestFragment

  veryLastSlot =
    foldl max 0 $
      map
        lastNo
        (honestFragment : map snd adversarialFragments)

  printTrunk :: AF.HasHeader blk => AF.AnchoredFragment blk -> String
  printTrunk = printLine (\_ -> "trunk:  ─")

  printBranch :: AF.HasHeader blk => Int -> AF.AnchoredFragment blk -> String
  printBranch idx = printLine $ \firstSlot ->
    let sidx = "  (" ++ show idx ++ ")"
        pad = replicate 6 ' '
        prefix = sidx ++ drop (length sidx) pad
     in prefix ++ replicate (3 * fromIntegral (unSlotNo (firstSlot - veryFirstSlot))) ' ' ++ " ╰─"

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
      & map
        ( \block ->
            (fromIntegral (unSlotNo (blockSlot block) - unSlotNo firstSlot), Just (unBlockNo (blockNo block)))
        )
      -- Update only the Vector elements that have blocks in them
      & Vector.toList . (slotRange Vector.//)
      & map (maybe "  " (printf "%2d"))
      & unwords
      & map (\c -> if c == ' ' then '─' else c)
   where
    -- Initialize a Vector with the length of the fragment containing only Nothings
    slotRange = Vector.replicate (fromIntegral (unSlotNo lastSlot - unSlotNo firstSlot + 1)) Nothing

  lastNo :: AF.HasHeader blk => AF.AnchoredFragment blk -> SlotNo
  lastNo = fromWithOrigin 0 . AF.headSlot

  firstNo :: AF.AnchoredFragment blk -> SlotNo
  firstNo frag =
    case AF.anchor frag of
      AF.AnchorGenesis -> 0
      AF.Anchor slotNo _ _ -> slotNo + 1

-- | An 'AF.AnchoredFragment' is a list where the last element (the anchor) is a ghost.
-- Here they represent the partial ancestry of a block, where the anchor is either
-- @Genesis@ (start of the chain, not itself an actual block) or the hash of a block.
-- Say we have blocks B1 through B5 (each succeeded by the next) and anchor A. You
-- can think of the chain as growing __from left to right__ like this:
--
-- >                    A :> B1 :> B2 :> B3 :> B4 :> B5
--
-- 'nonemptyPrefixesOf' builds the list of prefixes of an 'AF.AnchoredFragment' with at
-- least one non-anchor entry. The name is a little confusing because the way we
-- usually think of cons-lists these would be suffixes:
--
-- >            A :> B1     A :> B1 :> B2     A :> B1 :> B2 :> B3
-- >      A :> B1 :> B2 :> B3 :> B4     A :> B1 :> B2 :> B3 :> B4 :> B5
--
-- However this is consistent with 'Ouroboros.Network.AnchoredSeq.isPrefixOf'.
nonemptyPrefixesOf ::
  AF.HasHeader blk => AF.AnchoredFragment blk -> [AF.AnchoredFragment blk]
nonemptyPrefixesOf frag =
  fmap (AF.fromOldestFirst (AF.anchor frag)) . drop 1 . inits . AF.toOldestFirst $ frag

type DeforestedBlockTree blk = M.Map (HeaderHash blk) (AF.AnchoredFragment blk)

deforestRawBlockTree ::
  HasHeader blk =>
  AF.AnchoredFragment blk ->
  [BlockTreeBranch blk] ->
  DeforestedBlockTree blk
deforestRawBlockTree trunk branches =
  let folder = foldMap $ \af -> either (const mempty) (flip M.singleton af . blockHash) $ AF.head af
   in fold $
        folder (prefixes (AF.Empty AF.AnchorGenesis) $ AF.toOldestFirst trunk)
          : fmap (\btb -> folder $ prefixes (btbPrefix btb) $ AF.toOldestFirst $ btbSuffix btb) branches

prefixes :: AF.HasHeader blk => AF.AnchoredFragment blk -> [blk] -> [AF.AnchoredFragment blk]
prefixes = scanl (AF.:>)

-- | A check used in some of the handlers, determining whether the first argument
-- is on the chain that ends in the second argument.
--
-- REVIEW: Using 'AF.withinFragmentBounds' for this might be cheaper.
--
-- TODO: Unify with 'Test.Util.TestBlock.isAncestorOf' which basically does the
-- same thing except not on 'WithOrigin'.
isAncestorOf ::
  (HasHeader blk, Eq blk) =>
  BlockTree blk ->
  WithOrigin blk ->
  WithOrigin blk ->
  Bool
isAncestorOf tree (At ancestor) (At descendant) =
  let deforested = btDeforested tree
   in fromMaybe False $ do
        afD <- M.lookup (blockHash descendant) deforested
        afA <- M.lookup (blockHash ancestor) deforested
        pure $ AF.isPrefixOf afA afD
isAncestorOf _ (At _) Origin = False
isAncestorOf _ Origin _ = True

-- | Variant of 'isAncestorOf' that returns @False@ when the two blocks are
-- equal.
--
-- TODO: Unify with 'Test.Util.TestBlock.isStrictAncestorOf' which basically does the
-- same thing except not on 'WithOrigin'.
isStrictAncestorOf ::
  (HasHeader blk, Eq blk) =>
  BlockTree blk ->
  WithOrigin blk ->
  WithOrigin blk ->
  Bool
isStrictAncestorOf bt b1 b2 = b1 /= b2 && isAncestorOf bt b1 b2

-- | Check if a block (represented by its header 'Point') is on the 'BlockTree' trunk.
onTrunk :: GetHeader blk => BlockTree blk -> Point (Header blk) -> Bool
onTrunk blockTree = flip AF.withinFragmentBounds (AF.mapAnchoredFragment getHeader $ btTrunk blockTree)
