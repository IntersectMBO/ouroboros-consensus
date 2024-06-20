{-# LANGUAGE ScopedTypeVariables #-}

-- | Utility functions on anchored fragments
--
-- Intended for qualified import
-- > import qualified Ouroboros.Consensus.Util.AnchoredFragment as AF
module Ouroboros.Consensus.Util.AnchoredFragment (
    compareAnchoredFragments
  , cross
  , forksAtMostKBlocks
  , preferAnchoredCandidate
  , stripCommonPrefix
  ) where

import           Control.Monad.Except (throwError)
import           Data.Foldable (toList)
import qualified Data.List as L
import           Data.Maybe (isJust)
import           Data.Word (Word64)
import           GHC.Stack
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Assert
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     AnchoredSeq (Empty, (:>)))
import qualified Ouroboros.Network.AnchoredFragment as AF

{-------------------------------------------------------------------------------
  Utility functions on anchored fragments
-------------------------------------------------------------------------------}

forksAtMostKBlocks ::
     HasHeader b
  => Word64              -- ^ How many blocks can it fork?
  -> AnchoredFragment b  -- ^ Our chain.
  -> AnchoredFragment b  -- ^ Their chain
  -> Bool                -- ^ Indicates whether their chain forks at most the
                         -- specified number of blocks.
forksAtMostKBlocks k ours theirs = case ours `AF.intersect` theirs of
    Nothing                   -> False
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
     forall blk. (BlockSupportsProtocol blk, HasCallStack)
  => BlockConfig blk
  -> AnchoredFragment (Header blk)
  -> AnchoredFragment (Header blk)
  -> Ordering
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
        if blockPoint tip' == AF.anchorToPoint anchor
          then EQ
          else LT
      (_ :> tip, Empty anchor') ->
        -- This case is symmetric to the previous
        if blockPoint tip == AF.anchorToPoint anchor'
          then EQ
          else GT
      (_ :> tip, _ :> tip') ->
        -- Case 4
        compare
          (selectView cfg tip)
          (selectView cfg tip')

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
     forall blk. (BlockSupportsProtocol blk, HasCallStack)
  => BlockConfig blk
  -> AnchoredFragment (Header blk)      -- ^ Our chain
  -> AnchoredFragment (Header blk)      -- ^ Candidate
  -> Bool
preferAnchoredCandidate cfg ours cand =
    assertWithMsg (precondition ours cand) $
    case (ours, cand) of
      (_, Empty _) -> False
      (Empty ourAnchor, _ :> theirTip) ->
        blockPoint theirTip /= AF.anchorToPoint ourAnchor
      (_ :> ourTip, _ :> theirTip) ->
        preferCandidate
          (projectChainOrderConfig cfg)
          (selectView cfg ourTip)
          (selectView cfg theirTip)

-- For 'compareAnchoredFragment' and 'preferAnchoredCandidate'.
precondition ::
     GetHeader blk
  => AnchoredFragment (Header blk)
  -> AnchoredFragment (Header blk)
  -> Either String ()
precondition frag1 frag2
  | not (AF.null frag1), not (AF.null frag2)
  = return ()
  | isJust (AF.intersectionPoint frag1 frag2)
  = return ()
  | otherwise
  = throwError
      "precondition violated: fragments should both be non-empty or they \
      \should intersect"

-- | If the two fragments `c1` and `c2` intersect, return the intersection
-- point and join the prefix of `c1` before the intersection with the suffix
-- of `c2` after the intersection. The resulting fragment has the same
-- anchor as `c1` and the same head as `c2`.
cross ::
     HasHeader block
  => AnchoredFragment block
  -> AnchoredFragment block
  -> Maybe (Point block, AnchoredFragment block)
cross c1 c2 = do
    (p1, _p2, _s1, s2) <- AF.intersect c1 c2
    -- Note that the head of `p1` and `_p2` is the intersection point, and
    -- `_s1` and `s2` are anchored in the intersection point.
    let crossed = case AF.join p1 s2 of
          Just c  -> c
          Nothing -> error "invariant violation of AF.intersect"
    pure (AF.anchorPoint s2, crossed)

-- | Strip the common prefix of multiple fragments.
--
-- PRECONDITION: all fragments have the given anchor as their anchor.
stripCommonPrefix ::
     forall f blk.
     (Functor f, Foldable f, HasHeader blk) -- TODO: this uses the lazy 'map' for 'Map'...
  => AF.Anchor blk
  -> f (AnchoredFragment blk)
  -> (AnchoredFragment blk, f (AnchoredFragment blk))
stripCommonPrefix sharedAnchor frags
  | all ((sharedAnchor ==) . AF.anchor) frags
  = (commonPrefix, splitAfterCommonPrefix <$> frags)
  | otherwise
  = error "Not all fragments are anchored in the given anchor"
  where
    -- Return the common prefix of two fragments with the same anchor
    -- 'sharedAnchor'.
    computeCommonPrefix ::
         AnchoredFragment blk
      -> AnchoredFragment blk
      -> AnchoredFragment blk
    computeCommonPrefix frag1 frag2 = case AF.intersect frag1 frag2 of
      Just (cp, _, _, _) -> cp
      Nothing            -> error "unreachable"

    commonPrefix
      | null frags = AF.Empty sharedAnchor
      -- TODO use Foldable1 once all our GHCs support it
      | otherwise = L.foldl1' computeCommonPrefix (toList frags)

    splitAfterCommonPrefix frag =
      case AF.splitAfterPoint frag (AF.headPoint commonPrefix) of
        Just (_, afterCommonPrefix) -> afterCommonPrefix
        Nothing                     -> error "unreachable"
