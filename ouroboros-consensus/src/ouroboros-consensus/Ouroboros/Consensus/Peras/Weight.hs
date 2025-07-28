{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Consensus.Peras.Weight
  ( -- * 'PerasWeightSnapshot'
    PerasWeightSnapshot
  , emptyPerasWeightSnapshot
  , mkPerasWeightSnapshot
  , perasWeightSnapshotToList
  , addToPerasWeightSnapshot
  , removeFromPerasWeightSnapshot
  , weightBoostOfPoint
  , weightBoostOfFragment
  ) where

import Data.Foldable as Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block
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

weightBoostOfPoint ::
  forall blk.
  StandardHash blk =>
  PerasWeightSnapshot blk -> Point blk -> PerasWeight
weightBoostOfPoint (PerasWeightSnapshot weightByPoint) pt =
  Map.findWithDefault mempty pt weightByPoint

weightBoostOfFragment ::
  forall blk h.
  (HasHeader blk, HasHeader h, HeaderHash blk ~ HeaderHash h) =>
  PerasWeightSnapshot blk ->
  AnchoredFragment h ->
  PerasWeight
weightBoostOfFragment weightSnap frag =
  -- TODO think about whether this could be done in sublinear complexity
  foldMap
    (weightBoostOfPoint weightSnap . castPoint . blockPoint)
    (AF.toOldestFirst frag)
