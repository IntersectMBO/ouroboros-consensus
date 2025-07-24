{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Ouroboros.Consensus.Peras.Weight
  ( -- * 'PerasWeightSnapshot'
    PerasWeightSnapshot
  , emptyPerasWeightSnapshot
  , mkPerasWeightSnapshot
  , perasWeightSnapshotToList
  , addToPerasWeightSnapshot
  , removeFromPerasWeightSnapshot
  , boostedWeightForPoint
  , boostedWeightForFragment

    -- * 'WeightedSelectView'
  , WeightedSelectView (..)
  , wsvTotalWeight
  , weightedSelectView

    -- * Utility: 'WithEmptyFragment'
  , WithEmptyFragment (..)
  , withEmptyFragmentFromMaybe
  , withEmptyFragmentToMaybe
  ) where

import Data.Foldable as Foldable (foldl')
import Data.Function (on)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF

newtype PerasWeightSnapshot blk = PerasWeightSnapshot
  { getPerasWeightSnapshot :: Map (Point blk) PerasWeight
  }
  deriving stock (Show, Eq)
  deriving Generic
  deriving newtype NoThunks

emptyPerasWeightSnapshot :: PerasWeightSnapshot blk
emptyPerasWeightSnapshot = PerasWeightSnapshot Map.empty

mkPerasWeightSnapshot ::
  StandardHash blk =>
  [(Point blk, PerasWeight)] ->
  PerasWeightSnapshot blk
mkPerasWeightSnapshot =
  Foldable.foldl'
    (\s (pt, weight) -> addToPerasWeightSnapshot pt weight s)
    emptyPerasWeightSnapshot

perasWeightSnapshotToList :: PerasWeightSnapshot blk -> [(Point blk, PerasWeight)]
perasWeightSnapshotToList = Map.toList . getPerasWeightSnapshot

addToPerasWeightSnapshot ::
  StandardHash blk =>
  Point blk ->
  PerasWeight ->
  PerasWeightSnapshot blk ->
  PerasWeightSnapshot blk
addToPerasWeightSnapshot pt weight =
  PerasWeightSnapshot . Map.insertWith (<>) pt weight . getPerasWeightSnapshot

removeFromPerasWeightSnapshot ::
  StandardHash blk =>
  Point blk ->
  PerasWeight ->
  PerasWeightSnapshot blk ->
  PerasWeightSnapshot blk
removeFromPerasWeightSnapshot pt (PerasWeight weight) =
  PerasWeightSnapshot . Map.update subtractWeight pt . getPerasWeightSnapshot
 where
  subtractWeight :: PerasWeight -> Maybe PerasWeight
  subtractWeight (PerasWeight w)
    | w > weight = Just $ PerasWeight (w - weight)
    | otherwise = Nothing

boostedWeightForPoint ::
  forall blk.
  StandardHash blk =>
  PerasWeightSnapshot blk -> Point blk -> PerasWeight
boostedWeightForPoint (PerasWeightSnapshot weightByPoint) pt =
  Map.findWithDefault mempty pt weightByPoint

boostedWeightForFragment ::
  forall blk h.
  (HasHeader blk, HasHeader h, HeaderHash blk ~ HeaderHash h) =>
  PerasWeightSnapshot blk ->
  AnchoredFragment h ->
  PerasWeight
boostedWeightForFragment weightSnap frag =
  -- TODO think about whether this could be done in sublinear complexity
  foldMap
    (boostedWeightForPoint weightSnap)
    (castPoint . blockPoint <$> AF.toOldestFirst frag)

{-------------------------------------------------------------------------------
  Weighted select views
-------------------------------------------------------------------------------}

-- | Information from a non-empty chain fragment for a weighted chain comparison
-- against other fragments with the same anchor.
data WeightedSelectView proto = WeightedSelectView
  { wsvBlockNo :: !BlockNo
  , wsvBoostedWeight :: !PerasWeight
  , wsvTiebreaker :: TiebreakerView proto
  -- ^ Lazy because it is only needed when 'wsvTotalWeight' is inconclusive.
  }

deriving stock instance Show (TiebreakerView proto) => Show (WeightedSelectView proto)
deriving stock instance Eq (TiebreakerView proto) => Eq (WeightedSelectView proto)

-- TODO: More type safety to prevent people from accidentally comparing
-- 'WeightedSelectView's obtained from fragments with different anchors?
-- Something ST-trick like?

-- | The total weight, ie the sum of 'wsvBlockNo' and 'wsvBoostedWeight'.
wsvTotalWeight :: WeightedSelectView proto -> PerasWeight
-- could be cached, but then we need to be careful to maintain the invariant
wsvTotalWeight wsv =
  PerasWeight (unBlockNo (wsvBlockNo wsv)) <> wsvBoostedWeight wsv

instance Ord (TiebreakerView proto) => Ord (WeightedSelectView proto) where
  compare =
    mconcat
      [ compare `on` wsvTotalWeight
      , compare `on` wsvTiebreaker
      ]

instance ChainOrder (TiebreakerView proto) => ChainOrder (WeightedSelectView proto) where
  type ChainOrderConfig (WeightedSelectView proto) = ChainOrderConfig (TiebreakerView proto)

  preferCandidate cfg ours cand =
    case compare (wsvTotalWeight ours) (wsvTotalWeight cand) of
      LT -> True
      EQ -> preferCandidate cfg (wsvTiebreaker ours) (wsvTiebreaker cand)
      GT -> False

-- | Get the 'WeightedSelectView' for a fragment using the given
-- 'PerasWeightSnapshot'. Note that this is only meanigful for comparisons
-- against other fragments /with the same anchor/.
--
-- Returns 'EmptyFragment' iff the input fragment is empty.
weightedSelectView ::
  ( GetHeader1 h
  , HasHeader (h blk)
  , HeaderHash blk ~ HeaderHash (h blk)
  , BlockSupportsProtocol blk
  ) =>
  BlockConfig blk ->
  PerasWeightSnapshot blk ->
  AnchoredFragment (h blk) ->
  WithEmptyFragment (WeightedSelectView (BlockProtocol blk))
weightedSelectView bcfg weights = \case
  AF.Empty{} -> EmptyFragment
  frag@(_ AF.:> (getHeader1 -> hdr)) ->
    NonEmptyFragment
      WeightedSelectView
        { wsvBlockNo = blockNo hdr
        , wsvBoostedWeight = boostedWeightForFragment weights frag
        , wsvTiebreaker = tiebreakerView bcfg hdr
        }

{-------------------------------------------------------------------------------
  WithEmptyFragment
-------------------------------------------------------------------------------}

-- | Attach the possibility of an empty fragment to a type.
data WithEmptyFragment a = EmptyFragment | NonEmptyFragment !a
  deriving stock (Show, Eq)

withEmptyFragmentToMaybe :: WithEmptyFragment a -> Maybe a
withEmptyFragmentToMaybe = \case
  EmptyFragment -> Nothing
  NonEmptyFragment a -> Just a

withEmptyFragmentFromMaybe :: Maybe a -> WithEmptyFragment a
withEmptyFragmentFromMaybe = \case
  Nothing -> EmptyFragment
  Just a -> NonEmptyFragment a

-- | Prefer non-empty fragments to empty ones.
instance Ord a => Ord (WithEmptyFragment a) where
  compare = \cases
    EmptyFragment EmptyFragment -> EQ
    EmptyFragment NonEmptyFragment{} -> LT
    NonEmptyFragment{} EmptyFragment -> GT
    (NonEmptyFragment a) (NonEmptyFragment b) -> compare a b

-- | Prefer non-empty fragments to empty ones. This instance assumes that the
-- underlying fragments all have the same anchor.
instance ChainOrder a => ChainOrder (WithEmptyFragment a) where
  type ChainOrderConfig (WithEmptyFragment a) = ChainOrderConfig a

  preferCandidate cfg = \cases
    -- We prefer any non-empty fragment to the empty fragment.
    EmptyFragment NonEmptyFragment{} -> True
    -- We never prefer the empty fragment to our selection (even if it is also
    -- empty).
    _ EmptyFragment -> False
    -- Otherwise, defer to @'ChainOrder' a@.
    (NonEmptyFragment ours) (NonEmptyFragment cand) ->
      preferCandidate cfg ours cand
