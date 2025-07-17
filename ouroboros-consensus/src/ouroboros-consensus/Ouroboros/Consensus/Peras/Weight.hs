{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Peras.Weight
  ( PerasWeightSnapshot (..)
  , emptyPerasWeightSnapshot
  , boostedWeightForPoint
  , boostedWeightForFragment
  , WeightedSelectView (..)
  , wsvTotalWeight
  , weightedSelectView
  ) where

import Data.Function (on)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
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

-- | Information from a chain fragment for a weighted chain comparison against
-- other fragments with the same anchor.
data WeightedSelectView proto = WeightedSelectView
  { wsvLength :: !Word64
  , wsvBoostedWeight :: !PerasWeight
  , wsvTiebreaker :: Maybe (TiebreakerView proto)
  -- ^ Lazy because it is only needed when 'wsvTotalWeight' is inconclusive.
  }

deriving stock instance Show (TiebreakerView proto) => Show (WeightedSelectView proto)
deriving stock instance Eq (TiebreakerView proto) => Eq (WeightedSelectView proto)

-- TODO: More type safety to prevent people from accidentally comparing
-- 'WeightedSelectView's obtained from fragments with different anchors?
-- Something ST-trick like?

-- | The total weight, ie the sum of 'wsvLength' and 'wsvBoostedWeight'.
wsvTotalWeight :: WeightedSelectView proto -> PerasWeight
-- could be cached, but then we need to be careful to maintain the invariant
wsvTotalWeight wsv = PerasWeight (wsvLength wsv) <> wsvBoostedWeight wsv

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
      EQ -> case (wsvTiebreaker ours, wsvTiebreaker cand) of
        -- Both fragments are empty: candidate is not preferable.
        (Nothing, Nothing) -> False
        -- Both fragments are non-empty: compare the tiebreaker.
        (Just tvOurs, Just tvCand) -> preferCandidate cfg tvOurs tvCand
        -- If one fragment is empty, its 'wsvTotalWeight' must be 'mempty', and
        -- hence the other fragment must also be empty as it has the same weight
        -- in this branch.
        _ -> error "unreachble"
      GT -> False

-- | Get the 'WeightedSelectView' for a fragment using the given
-- 'PerasWeightSnapshot'. Note that this is only useful for comparison against
-- other fragments /with the same anchor/.
weightedSelectView ::
  ( GetHeader1 h
  , HasHeader (h blk)
  , HeaderHash blk ~ HeaderHash (h blk)
  , BlockSupportsProtocol blk
  ) =>
  BlockConfig blk ->
  PerasWeightSnapshot blk ->
  AnchoredFragment (h blk) ->
  WeightedSelectView (BlockProtocol blk)
weightedSelectView bcfg weights frag =
  WeightedSelectView
    { wsvLength = fromIntegral $ AF.length frag
    , wsvBoostedWeight = boostedWeightForFragment weights frag
    , wsvTiebreaker = case AF.head frag of
        Left _ -> Nothing
        Right hdr -> Just $ tiebreakerView bcfg (getHeader1 hdr)
    }
