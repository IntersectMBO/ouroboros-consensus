{-# LANGUAGE ScopedTypeVariables #-}

-- | Utilities on 'AnchoredSeq's.
module Ouroboros.Consensus.Util.AnchoredSeq
  ( takeLongestSuffix
  ) where

import Data.Maybe (fromMaybe)
import Ouroboros.Network.AnchoredSeq (AnchoredSeq)
import qualified Ouroboros.Network.AnchoredSeq as AS

-- | Take the longest suffix of an 'AnchoredSeq' @as@ satisfying the given
-- predicate @p@ on the monoidal summary given by @f@.
--
-- TODO: upstream this function
--
-- === PRECONDITIONS:
--
-- For @as0, as1@ such that @AS.join as0 as1 = Just as2@, we must have the
-- following homomorphism property:
--
-- > f as0 <> f as1 ≡ f as2
--
-- For empty @ase@, we must have @f ase ≡ mempty@.
--
-- The predicate must be monotonic, ie when @suf0@ is a suffix of @as@ and
-- @suf1@ is a suffix of @suf0@, then @p (f suf0)@ must imply @p (f suf1)@.
-- Furthermore, we must have @p mempty@.
takeLongestSuffix ::
  forall s v a b.
  (Monoid s, AS.Anchorable v a b) =>
  -- | @f@: Compute a monoidal summary of a fragment.
  (AnchoredSeq v a b -> s) ->
  -- | @p@: Predicate on the summary of a fragment.
  (s -> Bool) ->
  -- | Input sequence @as@.
  AnchoredSeq v a b ->
  -- | A suffix of the input sequence.
  AnchoredSeq v a b
takeLongestSuffix f p as =
  go (AS.Empty $ AS.headAnchor as) mempty as
 where
  go ::
    -- @suf@: the longest suffix of @as@ for which we currently know that @p (f
    -- suf)@.
    AnchoredSeq v a b ->
    -- Equal to @f suf@.
    s ->
    -- @pre@: longest infix of @as@ ending just before @suf@ such that we don't
    -- know whether @p (f (AS.join pre suf))@.
    AnchoredSeq v a b ->
    -- Longest suffix of @as@ satisfying @p . f@.
    AnchoredSeq v a b
  go suf sufS pre
    | AS.null pre = suf
    | p suf'S = go suf' suf'S pre0
    | AS.null pre0 = suf
    | otherwise = go suf sufS pre1
   where
    (pre0, pre1) = AS.splitAt (AS.length pre `div` 2) pre
    suf' =
      fromMaybe (error "takeLongestSuffix: internal invariant violation") $
        AS.join (\_ _ -> True) pre1 suf
    suf'S = f pre1 <> sufS
