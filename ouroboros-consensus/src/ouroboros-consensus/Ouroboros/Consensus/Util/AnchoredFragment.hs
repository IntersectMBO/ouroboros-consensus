{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Utility functions on anchored fragments
--
-- Intended for qualified import
-- > import qualified Ouroboros.Consensus.Util.AnchoredFragment as AF
module Ouroboros.Consensus.Util.AnchoredFragment
  ( compareAnchoredFragments
  , compareHeadBlockNo
  , cross
  , forksAtMostKBlocks
  , preferAnchoredCandidate
  , stripCommonPrefix
  ) where

import Control.Monad.Except (throwError)
import Data.Foldable (toList)
import qualified Data.Foldable1 as F1
import Data.Function (on)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import Data.Word (Word64)
import GHC.Stack
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Util.Assert
import Ouroboros.Network.AnchoredFragment
  ( AnchoredFragment
  , AnchoredSeq (Empty, (:>))
  )
import qualified Ouroboros.Network.AnchoredFragment as AF

{-------------------------------------------------------------------------------
  Utility functions on anchored fragments
-------------------------------------------------------------------------------}

-- | Compare the 'headBlockNo', which is a measure of the length of the chain,
-- of two anchored fragments.
--
-- A fragment with a head is always \"greater\" than one without. When both
-- fragments have no head (i.e. are empty), they are 'EQ'.
--
-- Note that an EBB can share its @BlockNo@ with another regular block. If
-- such an EBB is the head of one fragment and the regular block with the same
-- @BlockNo@ is the head of the other fragment, then this function will say
-- they are 'EQ', while in fact one fragment should be preferred over the
-- other.
--
-- This is not a big deal as we won't be seeing new EBBs, so they will not be
-- the head of a fragment very often anyway, only when catching up. As soon as
-- a new block/header is added to the fragment, the right decision will be
-- made again ('GT' or 'LT').
compareHeadBlockNo ::
  HasHeader b =>
  AnchoredFragment b ->
  AnchoredFragment b ->
  Ordering
compareHeadBlockNo = compare `on` AF.headBlockNo

forksAtMostKBlocks ::
  HasHeader b =>
  -- | How many blocks can it fork?
  Word64 ->
  -- | Our chain.
  AnchoredFragment b ->
  -- | Their chain
  AnchoredFragment b ->
  -- | Indicates whether their chain forks at most the
  -- specified number of blocks.
  Bool
forksAtMostKBlocks k ours theirs = case ours `AF.intersect` theirs of
  Nothing -> False
  Just (_, _, ourSuffix, _) -> fromIntegral (AF.length ourSuffix) <= k

-- | Compare two (potentially empty!) 'AnchoredFragment's.
--
-- PRECONDITION: Either both fragments are non-empty or they intersect.
--
-- For a detailed discussion of this precondition, and a justification for the
-- definition of this function, please refer to the Consensus Report.
--
-- Usage note: the primary user of this function is the chain database when
-- sorting fragments that are preferred over our selection. It establishes the
-- precondition in the following way: It will only compare candidate fragments
-- that it has previously verified are preferable to our current chain. Since
-- these fragments intersect with our current chain, they must by transitivity
-- also intersect each other.
compareAnchoredFragments ::
  forall blk h.
  ( BlockSupportsProtocol blk
  , HasCallStack
  , GetHeader1 h
  , HasHeader (h blk)
  ) =>
  BlockConfig blk ->
  AnchoredFragment (h blk) ->
  AnchoredFragment (h blk) ->
  Ordering
compareAnchoredFragments cfg frag1 frag2 =
  assertWithMsg (precondition frag1 frag2) $
    case (frag1, frag2) of
      (Empty _, Empty _) ->
        -- The fragments intersect but are equal: their anchors must be equal,
        -- and hence the fragments represent the same chain. They are therefore
        -- equally preferable.
        EQ
      (Empty anchor, _ :> tip') ->
        -- Since the fragments intersect, but the first one is empty, its anchor
        -- must lie somewhere along the the second. If it is the tip, the two
        -- fragments represent the same chain and are equally preferable. If
        -- not, the second chain is a strict extension of the first and is
        -- therefore strictly preferable.
        if blockPoint tip' == AF.castPoint (AF.anchorToPoint anchor)
          then EQ
          else LT
      (_ :> tip, Empty anchor') ->
        -- This case is symmetric to the previous
        if blockPoint tip == AF.castPoint (AF.anchorToPoint anchor')
          then EQ
          else GT
      (_ :> tip, _ :> tip') ->
        -- Case 4
        compare
          (selectView cfg (getHeader1 tip))
          (selectView cfg (getHeader1 tip'))

-- | Lift 'preferCandidate' to 'AnchoredFragment'
--
-- PRECONDITION: Either both fragments are non-empty or they intersect.
--
-- Usage note: the primary user of this function is the chain database. It
-- establishes the precondition when comparing a candidate fragment to our
-- current chain in the following way: The fragment is guaranteed (by the chain
-- sync client) to intersect with our chain (indeed, within at most @k@ blocks
-- from our tip, although the exact distance does not matter for
-- 'compareAnchoredFragments').
preferAnchoredCandidate ::
  forall blk h h'.
  ( BlockSupportsProtocol blk
  , HasCallStack
  , GetHeader1 h
  , GetHeader1 h'
  , HeaderHash (h blk) ~ HeaderHash (h' blk)
  , HasHeader (h blk)
  , HasHeader (h' blk)
  ) =>
  BlockConfig blk ->
  -- | Our chain
  AnchoredFragment (h blk) ->
  -- | Candidate
  AnchoredFragment (h' blk) ->
  Bool
preferAnchoredCandidate cfg ours cand =
  assertWithMsg (precondition ours cand) $
    case (ours, cand) of
      (_, Empty _) -> False
      (Empty ourAnchor, _ :> theirTip) ->
        blockPoint theirTip /= castPoint (AF.anchorToPoint ourAnchor)
      (_ :> ourTip, _ :> theirTip) ->
        preferCandidate
          (projectChainOrderConfig cfg)
          (selectView cfg (getHeader1 ourTip))
          (selectView cfg (getHeader1 theirTip))

-- For 'compareAnchoredFragment' and 'preferAnchoredCandidate'.
precondition ::
  ( HeaderHash (h blk) ~ HeaderHash (h' blk)
  , HasHeader (h blk)
  , HasHeader (h' blk)
  ) =>
  AnchoredFragment (h blk) ->
  AnchoredFragment (h' blk) ->
  Either String ()
precondition frag1 frag2
  | not (AF.null frag1)
  , not (AF.null frag2) =
      return ()
  | isJust (AF.intersectionPoint frag1 frag2) =
      return ()
  | otherwise =
      throwError
        "precondition violated: fragments should both be non-empty or they \
        \should intersect"

-- | If the two fragments `c1` and `c2` intersect, return the intersection
-- point and join the prefix of `c1` before the intersection with the suffix
-- of `c2` after the intersection. The resulting fragment has the same
-- anchor as `c1` and the same head as `c2`.
cross ::
  HasHeader block =>
  AnchoredFragment block ->
  AnchoredFragment block ->
  Maybe (Point block, AnchoredFragment block)
cross c1 c2 = do
  (p1, _p2, _s1, s2) <- AF.intersect c1 c2
  -- Note that the head of `p1` and `_p2` is the intersection point, and
  -- `_s1` and `s2` are anchored in the intersection point.
  let crossed = case AF.join p1 s2 of
        Just c -> c
        Nothing -> error "invariant violation of AF.intersect"
  pure (AF.anchorPoint s2, crossed)

-- | Strip the common prefix of multiple fragments.
--
-- PRECONDITION: all fragments have the given anchor as their anchor.
stripCommonPrefix ::
  forall f blk.
  (Functor f, Foldable f, HasHeader blk) => -- TODO: this uses the lazy 'map' for 'Map'...
  AF.Anchor blk ->
  f (AnchoredFragment blk) ->
  (AnchoredFragment blk, f (AnchoredFragment blk))
stripCommonPrefix sharedAnchor frags
  | all ((sharedAnchor ==) . AF.anchor) frags =
      (commonPrefix, splitAfterCommonPrefix <$> frags)
  | otherwise =
      error "Not all fragments are anchored in the given anchor"
 where
  -- Return the common prefix of two fragments with the same anchor
  -- 'sharedAnchor'.
  computeCommonPrefix ::
    AnchoredFragment blk ->
    AnchoredFragment blk ->
    AnchoredFragment blk
  computeCommonPrefix frag1 frag2 = case AF.intersect frag1 frag2 of
    Just (cp, _, _, _) -> cp
    Nothing -> error "unreachable"

  commonPrefix = case NE.nonEmpty $ toList frags of
    Nothing -> AF.Empty sharedAnchor
    Just fragsNE -> F1.foldl1' computeCommonPrefix fragsNE

  splitAfterCommonPrefix frag =
    case AF.splitAfterPoint frag (AF.headPoint commonPrefix) of
      Just (_, afterCommonPrefix) -> afterCommonPrefix
      Nothing -> error "unreachable"
