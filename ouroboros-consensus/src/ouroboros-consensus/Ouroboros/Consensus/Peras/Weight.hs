{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Data structure for tracking the weight of blocks due to Peras boosts.
module Ouroboros.Consensus.Peras.Weight
  ( -- * 'PerasWeightSnapshot' type
    PerasWeightSnapshot

    -- * Construction
  , emptyPerasWeightSnapshot
  , mkPerasWeightSnapshot

    -- * Conversion
  , perasWeightSnapshotToList

    -- * Insertion
  , addToPerasWeightSnapshot

    -- * Pruning
  , prunePerasWeightSnapshot

    -- * Query
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

-- | Data structure for tracking the weight of blocks due to Peras boosts.
newtype PerasWeightSnapshot blk = PerasWeightSnapshot
  { getPerasWeightSnapshot :: Map (Point blk) PerasWeight
  }
  deriving stock Eq
  deriving Generic
  deriving newtype NoThunks

instance StandardHash blk => Show (PerasWeightSnapshot blk) where
  show = show . perasWeightSnapshotToList

-- | An empty 'PerasWeightSnapshot' not containing any boosted blocks.
emptyPerasWeightSnapshot :: PerasWeightSnapshot blk
emptyPerasWeightSnapshot = PerasWeightSnapshot Map.empty

-- | Create a weight snapshot from a list of boosted points with an associated
-- weight. In case of duplicate points, their weights are combined.
--
-- >>> :{
-- weights :: [(Point Blk, PerasWeight)]
-- weights =
--   [ (BlockPoint 2 "foo", PerasWeight 2)
--   , (GenesisPoint,       PerasWeight 3)
--   , (BlockPoint 3 "bar", PerasWeight 2)
--   , (BlockPoint 2 "foo", PerasWeight 2)
--   ]
-- :}
--
-- >>> snap = mkPerasWeightSnapshot weights
-- >>> snap
-- [(Origin,PerasWeight 3),(At (Block {blockPointSlot = SlotNo 2, blockPointHash = "foo"}),PerasWeight 4),(At (Block {blockPointSlot = SlotNo 3, blockPointHash = "bar"}),PerasWeight 2)]
mkPerasWeightSnapshot ::
  StandardHash blk =>
  [(Point blk, PerasWeight)] ->
  PerasWeightSnapshot blk
mkPerasWeightSnapshot =
  Foldable.foldl'
    (\s (pt, weight) -> addToPerasWeightSnapshot pt weight s)
    emptyPerasWeightSnapshot

-- | Return the list of boosted points with their associated weight, sorted
-- based on their point. Does not contain duplicate points.
--
-- >>> :{
-- weights :: [(Point Blk, PerasWeight)]
-- weights =
--   [ (BlockPoint 2 "foo", PerasWeight 2)
--   , (GenesisPoint,       PerasWeight 3)
--   , (BlockPoint 3 "bar", PerasWeight 2)
--   , (BlockPoint 2 "foo", PerasWeight 2)
--   ]
-- :}
--
-- >>> snap = mkPerasWeightSnapshot weights
-- >>> perasWeightSnapshotToList snap
-- [(Origin,PerasWeight 3),(At (Block {blockPointSlot = SlotNo 2, blockPointHash = "foo"}),PerasWeight 4),(At (Block {blockPointSlot = SlotNo 3, blockPointHash = "bar"}),PerasWeight 2)]
perasWeightSnapshotToList :: PerasWeightSnapshot blk -> [(Point blk, PerasWeight)]
perasWeightSnapshotToList = Map.toAscList . getPerasWeightSnapshot

-- | Add weight for the given point to the 'PerasWeightSnapshot'. If the point
-- already has some weight, it is added on top.
--
-- >>> :{
-- weights :: [(Point Blk, PerasWeight)]
-- weights =
--   [ (BlockPoint 2 "foo", PerasWeight 2)
--   , (GenesisPoint,       PerasWeight 3)
--   ]
-- :}
--
-- >>> snap0 = mkPerasWeightSnapshot weights
-- >>> snap0
-- [(Origin,PerasWeight 3),(At (Block {blockPointSlot = SlotNo 2, blockPointHash = "foo"}),PerasWeight 2)]
--
-- >>> snap1 = addToPerasWeightSnapshot (BlockPoint 3 "bar") (PerasWeight 2) snap0
-- >>> snap1
-- [(Origin,PerasWeight 3),(At (Block {blockPointSlot = SlotNo 2, blockPointHash = "foo"}),PerasWeight 2),(At (Block {blockPointSlot = SlotNo 3, blockPointHash = "bar"}),PerasWeight 2)]
--
-- >>> snap2 = addToPerasWeightSnapshot (BlockPoint 2 "foo") (PerasWeight 2) snap1
-- >>> snap2
-- [(Origin,PerasWeight 3),(At (Block {blockPointSlot = SlotNo 2, blockPointHash = "foo"}),PerasWeight 4),(At (Block {blockPointSlot = SlotNo 3, blockPointHash = "bar"}),PerasWeight 2)]
addToPerasWeightSnapshot ::
  StandardHash blk =>
  Point blk ->
  PerasWeight ->
  PerasWeightSnapshot blk ->
  PerasWeightSnapshot blk
addToPerasWeightSnapshot pt weight =
  PerasWeightSnapshot . Map.insertWith (<>) pt weight . getPerasWeightSnapshot

-- | Prune the given 'PerasWeightSnapshot' by removing the weight of all blocks
-- strictly older than the given slot.
--
-- This function is used to get garbage-collect boosted blocks blocks which are
-- older than our immutable tip as we will never adopt a chain containing them.
--
-- >>> :{
-- weights :: [(Point Blk, PerasWeight)]
-- weights =
--   [ (BlockPoint 2 "foo", PerasWeight 2)
--   , (GenesisPoint,       PerasWeight 3)
--   , (BlockPoint 3 "bar", PerasWeight 2)
--   , (BlockPoint 2 "foo", PerasWeight 2)
--   ]
-- :}
--
-- >>> snap = mkPerasWeightSnapshot weights
--
-- >>> prunePerasWeightSnapshot (SlotNo 2) snap
-- [(At (Block {blockPointSlot = SlotNo 2, blockPointHash = "foo"}),PerasWeight 4),(At (Block {blockPointSlot = SlotNo 3, blockPointHash = "bar"}),PerasWeight 2)]
--
-- >>> prunePerasWeightSnapshot (SlotNo 3) snap
-- [(At (Block {blockPointSlot = SlotNo 3, blockPointHash = "bar"}),PerasWeight 2)]
prunePerasWeightSnapshot ::
  SlotNo ->
  PerasWeightSnapshot blk ->
  PerasWeightSnapshot blk
prunePerasWeightSnapshot slot =
  PerasWeightSnapshot . Map.dropWhileAntitone isTooOld . getPerasWeightSnapshot
 where
  isTooOld :: Point blk -> Bool
  isTooOld pt = pointSlot pt < NotOrigin slot

-- | Get the weight boost for a point, or @'mempty' :: 'PerasWeight'@ otherwise.
--
-- >>> :{
-- weights :: [(Point Blk, PerasWeight)]
-- weights =
--   [ (BlockPoint 2 "foo", PerasWeight 2)
--   , (GenesisPoint,       PerasWeight 3)
--   , (BlockPoint 3 "bar", PerasWeight 2)
--   , (BlockPoint 2 "foo", PerasWeight 2)
--   ]
-- :}
--
-- >>> snap = mkPerasWeightSnapshot weights
--
-- >>> weightBoostOfPoint snap (BlockPoint 2 "foo")
-- PerasWeight 4
--
-- >>> weightBoostOfPoint snap (BlockPoint 2 "baz")
-- PerasWeight 0
weightBoostOfPoint ::
  forall blk.
  StandardHash blk =>
  PerasWeightSnapshot blk -> Point blk -> PerasWeight
weightBoostOfPoint (PerasWeightSnapshot weightByPoint) pt =
  Map.findWithDefault mempty pt weightByPoint

-- | Get the weight boost for a fragment, ie the sum of all
-- 'weightBoostOfPoint' for all points on the fragment (excluding the anchor).
--
-- Note that this quantity is relative to the anchor of the fragment, so it
-- should only be compared against other fragments with the same anchor.
--
-- >>> :{
-- weights :: [(Point Blk, PerasWeight)]
-- weights =
--   [ (BlockPoint 2 "foo", PerasWeight 2)
--   , (GenesisPoint,       PerasWeight 3)
--   , (BlockPoint 3 "bar", PerasWeight 2)
--   , (BlockPoint 2 "foo", PerasWeight 2)
--   ]
-- :}
--
-- >>> :{
-- snap = mkPerasWeightSnapshot weights
-- foo = HeaderFields (SlotNo 2) (BlockNo 1) "foo"
-- bar = HeaderFields (SlotNo 3) (BlockNo 2) "bar"
-- frag0 :: AnchoredFragment (HeaderFields Blk)
-- frag0 = Empty AnchorGenesis :> foo :> bar
-- :}
--
-- >>> weightBoostOfFragment snap frag0
-- PerasWeight 6
--
-- Only keeping the last block from @frag0@:
--
-- >>> frag1 = AF.anchorNewest 1 frag0
-- >>> weightBoostOfFragment snap frag1
-- PerasWeight 2
--
-- Dropping the head from @frag0@, and instead adding an unboosted point:
--
-- >>> frag2 = AF.dropNewest 1 frag0 :> HeaderFields (SlotNo 4) (BlockNo 2) "baz"
-- >>> weightBoostOfFragment snap frag2
-- PerasWeight 4
weightBoostOfFragment ::
  forall blk h.
  (StandardHash blk, HasHeader h, HeaderHash blk ~ HeaderHash h) =>
  PerasWeightSnapshot blk ->
  AnchoredFragment h ->
  PerasWeight
weightBoostOfFragment weightSnap frag =
  -- TODO think about whether this could be done in sublinear complexity
  foldMap
    (weightBoostOfPoint weightSnap . castPoint . blockPoint)
    (AF.toOldestFirst frag)

-- $setup
-- >>> import Ouroboros.Consensus.Block
-- >>> import Ouroboros.Network.AnchoredFragment (AnchoredFragment, AnchoredSeq(..), Anchor(..))
-- >>> import qualified Ouroboros.Network.AnchoredFragment as AF
-- >>> :set -XTypeFamilies
-- >>> data Blk = Blk
-- >>> type instance HeaderHash Blk = String
-- >>> instance StandardHash Blk
