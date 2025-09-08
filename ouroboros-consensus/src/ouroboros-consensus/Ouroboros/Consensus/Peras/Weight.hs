{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

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

import Data.FingerTree.Strict (Measured (..), StrictFingerTree)
import qualified Data.FingerTree.Strict as SFT
import Data.Foldable as Foldable (foldl', toList)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config.SecurityParam
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF

-- | Data structure for tracking the weight of blocks due to Peras boosts.
--
-- PRECONDITION: All boosted points tracked by this data structure must reside
-- on a single linear chain, and no boosted point may be an EBB. Otherwise,
-- queries on this data structure may return incorrect results.
--
-- TODO: This isn't true across cooldowns.
--
-- For Peras (assuming an honest majority), this is guaranteed by the voting
-- rules, together with the fact that Peras is not to be used with blocks where
-- EBBs (if they can even exist) may receive boosts.
newtype PerasWeightSnapshot blk = PerasWeightSnapshot
  { getPerasWeightSnapshot :: StrictFingerTree PWSMeasure (BoostedPoint blk)
  -- ^ INVARIANT: The slots of the boosted points are strictly monotonically
  -- increasing.
  }
  deriving stock Eq
  deriving Generic
  deriving newtype NoThunks

instance StandardHash blk => Show (PerasWeightSnapshot blk) where
  show = show . perasWeightSnapshotToList

data PWSMeasure = PWSMeasure
  { slot :: !(WithOrigin SlotNo)
  -- ^ The maximum slot of all boosted points.
  , weight :: !PerasWeight
  -- ^ The sum of all weight boosts.
  , size :: !Int
  -- ^ The number of boosted points.
  }
  deriving stock Show

instance Semigroup PWSMeasure where
  m0 <> m1 =
    PWSMeasure
      { slot = m0.slot `max` m1.slot
      , weight = m0.weight <> m1.weight
      , size = m0.size + m1.size
      }

instance Monoid PWSMeasure where
  mempty =
    PWSMeasure
      { slot = Origin
      , weight = mempty
      , size = 0
      }

data BoostedPoint blk = BoostedPoint
  { pt :: !(Point blk)
  , weight :: !PerasWeight
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

instance Measured PWSMeasure (BoostedPoint blk) where
  measure bp =
    PWSMeasure
      { slot = pointSlot bp.pt
      , weight = bp.weight
      , size = 1
      }

-- | An empty 'PerasWeightSnapshot' not containing any boosted blocks.
emptyPerasWeightSnapshot :: PerasWeightSnapshot blk
emptyPerasWeightSnapshot = PerasWeightSnapshot SFT.empty

-- | Create a weight snapshot from a list of boosted points with an associated
-- weight. In case of duplicate points, their weights are combined.
--
-- PRECONDITION: The points lie on a singular linear chain.
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
-- [(Point Origin,PerasWeight 3),(Point (At (Block {blockPointSlot = SlotNo 2, blockPointHash = "foo"})),PerasWeight 4),(Point (At (Block {blockPointSlot = SlotNo 3, blockPointHash = "bar"})),PerasWeight 2)]
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
-- [(Point Origin,PerasWeight 3),(Point (At (Block {blockPointSlot = SlotNo 2, blockPointHash = "foo"})),PerasWeight 4),(Point (At (Block {blockPointSlot = SlotNo 3, blockPointHash = "bar"})),PerasWeight 2)]
perasWeightSnapshotToList :: PerasWeightSnapshot blk -> [(Point blk, PerasWeight)]
perasWeightSnapshotToList (PerasWeightSnapshot ft) =
  (\(BoostedPoint pt w) -> (pt, w)) <$> toList ft

-- | Add weight for the given point to the 'PerasWeightSnapshot'. If the point
-- already has some weight, it is added on top.
--
-- PRECONDITION: The point must lie on the same linear chain as the points
-- already part of the 'PerasWeightSnapshot'.
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
-- [(Point Origin,PerasWeight 3),(Point (At (Block {blockPointSlot = SlotNo 2, blockPointHash = "foo"})),PerasWeight 2)]
--
-- >>> snap1 = addToPerasWeightSnapshot (BlockPoint 3 "bar") (PerasWeight 2) snap0
-- >>> snap1
-- [(Point Origin,PerasWeight 3),(Point (At (Block {blockPointSlot = SlotNo 2, blockPointHash = "foo"})),PerasWeight 2),(Point (At (Block {blockPointSlot = SlotNo 3, blockPointHash = "bar"})),PerasWeight 2)]
--
-- >>> snap2 = addToPerasWeightSnapshot (BlockPoint 2 "foo") (PerasWeight 2) snap1
-- >>> snap2
-- [(Point Origin,PerasWeight 3),(Point (At (Block {blockPointSlot = SlotNo 2, blockPointHash = "foo"})),PerasWeight 4),(Point (At (Block {blockPointSlot = SlotNo 3, blockPointHash = "bar"})),PerasWeight 2)]
addToPerasWeightSnapshot ::
  StandardHash blk =>
  Point blk ->
  PerasWeight ->
  PerasWeightSnapshot blk ->
  PerasWeightSnapshot blk
addToPerasWeightSnapshot pt weight =
  \(PerasWeightSnapshot ft) ->
    let (l, r) = SFT.split (\m -> m.slot > pointSlot pt) ft
     in PerasWeightSnapshot $ insert l <> r
 where
  insert l = case SFT.viewr l of
    SFT.EmptyR -> SFT.singleton $ BoostedPoint pt weight
    l' SFT.:> BoostedPoint pt' weight'
      -- We already track some weight of @pt@.
      | pt == pt' -> l' SFT.|> BoostedPoint pt' (weight <> weight')
      -- Otherwise, insert @pt@ as a new boosted point.
      | otherwise -> l SFT.|> BoostedPoint pt weight

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
-- [(Point (At (Block {blockPointSlot = SlotNo 2, blockPointHash = "foo"})),PerasWeight 4),(Point (At (Block {blockPointSlot = SlotNo 3, blockPointHash = "bar"})),PerasWeight 2)]
--
-- >>> prunePerasWeightSnapshot (SlotNo 3) snap
-- [(Point (At (Block {blockPointSlot = SlotNo 3, blockPointHash = "bar"})),PerasWeight 2)]
prunePerasWeightSnapshot ::
  SlotNo ->
  PerasWeightSnapshot blk ->
  PerasWeightSnapshot blk
prunePerasWeightSnapshot slot (PerasWeightSnapshot ft) =
  PerasWeightSnapshot $ SFT.dropUntil (\m -> m.slot >= NotOrigin slot) ft

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
weightBoostOfPoint (PerasWeightSnapshot ft) pt =
  case SFT.viewr $ SFT.takeUntil (\m -> m.slot > pointSlot pt) ft of
    SFT.EmptyR -> mempty
    _ SFT.:> BoostedPoint pt' weight'
      | pt == pt' -> weight'
      | otherwise -> mempty

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
weightBoostOfFragment (PerasWeightSnapshot ft) = \case
  AF.Empty{} -> mempty
  frag@(oldestHdr AF.:< _) -> (measure boostingInfix).weight
   where
    -- /Not/ @'AF.lastSlot' frag@ as we want to ignore the anchor.
    oldestSlot = NotOrigin $ blockSlot oldestHdr

    -- The infix of @ft@ which only contains boosted points which are also on
    -- @frag@ (via @isOnFrag@).
    boostingInfix :: StrictFingerTree PWSMeasure (BoostedPoint blk)
    boostingInfix = case SFT.viewr ft' of
      SFT.EmptyR -> ft'
      t SFT.:> bp
        | isOnFrag bp.pt -> ft'
        | otherwise -> go 0 (measure ft').size t
     where
      -- The suffix of @ft@ without boosted points which are too old to be on
      -- @frag@.
      ft' = SFT.dropUntil (\m -> m.slot >= oldestSlot) ft

      -- Binary search on @ft'@ to find the longest prefix of @ft'@ where all
      -- boosted points satisfy @isOnFrag@.
      --
      -- PRECONDITION: @0 <= lb < ub@.
      go ::
        -- @lb@: All boosted points of the size @lb@ prefix of @ft'@ satisfy
        -- @isOnFrag@.
        Int ->
        -- @ub@: At least one boosted point of the size @ub@ prefix of @ft'@
        -- does not satisfy @isOnFrag@.
        Int ->
        -- The size @ub - 1@ prefix of @ft'@.
        StrictFingerTree PWSMeasure (BoostedPoint blk) ->
        StrictFingerTree PWSMeasure (BoostedPoint blk)
      go lb ub t
        | lb == ub - 1 = t
        | isOnFrag t'Pt = go mid ub t
        | otherwise = go lb mid t'
       where
        mid = (lb + ub) `div` 2
        (t', t'Pt) = case SFT.viewr $ SFT.takeUntil (\m -> m.size > mid) ft' of
          t'' SFT.:> bp -> (t'', bp.pt)
          -- @ft'@ is non-empty here, and we have @0 <= lb < mid@.
          SFT.EmptyR -> error "unreachable"

    isOnFrag :: Point blk -> Bool
    isOnFrag pt = AF.pointOnFragment (castPoint pt) frag

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
  | SFT.null snap.getPerasWeightSnapshot =
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
