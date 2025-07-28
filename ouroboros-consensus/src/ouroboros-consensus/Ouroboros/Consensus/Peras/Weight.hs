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
  , totalWeightOfFragment
  , takeVolatileSuffix
  ) where

import Data.Foldable as Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config.SecurityParam
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

-- | Get the total weight for a fragment, ie the length plus the weight boost
-- ('weightBoostOfFragment') of the fragment.
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
-- >>> totalWeightOfFragment snap frag0
-- PerasWeight 8
--
-- Only keeping the last block from @frag0@:
--
-- >>> frag1 = AF.anchorNewest 1 frag0
-- >>> totalWeightOfFragment snap frag1
-- PerasWeight 3
--
-- Dropping the head from @frag0@, and instead adding an unboosted point:
--
-- >>> frag2 = AF.dropNewest 1 frag0 :> HeaderFields (SlotNo 4) (BlockNo 2) "baz"
-- >>> totalWeightOfFragment snap frag2
-- PerasWeight 6
totalWeightOfFragment ::
  forall blk h.
  (StandardHash blk, HasHeader h, HeaderHash blk ~ HeaderHash h) =>
  PerasWeightSnapshot blk ->
  AnchoredFragment h ->
  PerasWeight
totalWeightOfFragment weightSnap frag =
  weightLength <> weightBoost
 where
  weightLength = PerasWeight $ fromIntegral $ AF.length frag
  weightBoost = weightBoostOfFragment weightSnap frag

-- | Take the longest suffix of the given fragment with total weight
-- ('totalWeightOfFragment') at most @k@. This is the volatile suffix of blocks
-- which are subject to rollback.
--
-- If the total weight of the input fragment is at least @k@, then the anchor of
-- the output fragment is the most recent point on the input fragment that is
-- buried under at least weight @k@ (also counting the weight boost of that
-- point).
--
-- >>> :{
-- weights :: [(Point Blk, PerasWeight)]
-- weights =
--   [ (BlockPoint 2 "foo", PerasWeight 2)
--   , (GenesisPoint,       PerasWeight 3)
--   , (BlockPoint 3 "bar", PerasWeight 2)
--   , (BlockPoint 2 "foo", PerasWeight 2)
--   ]
-- snap = mkPerasWeightSnapshot weights
-- foo = HeaderFields (SlotNo 2) (BlockNo 1) "foo"
-- bar = HeaderFields (SlotNo 3) (BlockNo 2) "bar"
-- frag :: AnchoredFragment (HeaderFields Blk)
-- frag = Empty AnchorGenesis :> foo :> bar
-- :}
--
-- >>> k1 = SecurityParam $ knownNonZeroBounded @1
-- >>> k3 = SecurityParam $ knownNonZeroBounded @3
-- >>> k6 = SecurityParam $ knownNonZeroBounded @6
-- >>> k9 = SecurityParam $ knownNonZeroBounded @9
--
-- >>> AF.toOldestFirst $ takeVolatileSuffix snap k1 frag
-- []
--
-- >>> AF.toOldestFirst $ takeVolatileSuffix snap k3 frag
-- [HeaderFields {headerFieldSlot = SlotNo 3, headerFieldBlockNo = BlockNo 2, headerFieldHash = "bar"}]
--
-- >>> AF.toOldestFirst $ takeVolatileSuffix snap k6 frag
-- [HeaderFields {headerFieldSlot = SlotNo 3, headerFieldBlockNo = BlockNo 2, headerFieldHash = "bar"}]
--
-- >>> AF.toOldestFirst $ takeVolatileSuffix snap k9 frag
-- [HeaderFields {headerFieldSlot = SlotNo 2, headerFieldBlockNo = BlockNo 1, headerFieldHash = "foo"},HeaderFields {headerFieldSlot = SlotNo 3, headerFieldBlockNo = BlockNo 2, headerFieldHash = "bar"}]
takeVolatileSuffix ::
  forall blk h.
  (StandardHash blk, HasHeader h, HeaderHash blk ~ HeaderHash h) =>
  PerasWeightSnapshot blk ->
  -- | The security parameter @k@ is interpreted as a weight.
  SecurityParam ->
  AnchoredFragment h ->
  AnchoredFragment h
takeVolatileSuffix snap secParam frag
  | Map.null $ getPerasWeightSnapshot snap =
      -- Optimize the case where Peras is disabled.
      AF.anchorNewest (unPerasWeight k) frag
  | hasAtMostWeightK frag = frag
  | otherwise = go 0 lenFrag (AF.Empty $ AF.headAnchor frag)
 where
  k :: PerasWeight
  k = maxRollbackWeight secParam

  hasAtMostWeightK :: AnchoredFragment h -> Bool
  hasAtMostWeightK f = totalWeightOfFragment snap f <= k

  lenFrag = fromIntegral $ AF.length frag

  -- Binary search for the longest suffix of @frag@ which 'hasAtMostWeightK'.
  go ::
    Word64 -> -- lb. The length lb suffix satisfies 'hasAtMostWeightK'.
    Word64 -> -- ub. The length ub suffix does not satisfy 'hasAtMostWeightK'.
    AnchoredFragment h -> -- The length lb suffix.
    AnchoredFragment h
  go lb ub lbFrag
    | lb + 1 == ub = lbFrag
    | hasAtMostWeightK midFrag = go mid ub midFrag
    | otherwise = go lb mid lbFrag
   where
    mid = (lb + ub) `div` 2
    midFrag = AF.anchorNewest mid frag

-- $setup
-- >>> import Cardano.Ledger.BaseTypes
-- >>> import Ouroboros.Consensus.Block
-- >>> import Ouroboros.Consensus.Config.SecurityParam
-- >>> import Ouroboros.Network.AnchoredFragment (AnchoredFragment, AnchoredSeq(..), Anchor(..))
-- >>> import qualified Ouroboros.Network.AnchoredFragment as AF
-- >>> :set -XDataKinds -XTypeApplications -XTypeFamilies
-- >>> data Blk = Blk
-- >>> type instance HeaderHash Blk = String
-- >>> instance StandardHash Blk
