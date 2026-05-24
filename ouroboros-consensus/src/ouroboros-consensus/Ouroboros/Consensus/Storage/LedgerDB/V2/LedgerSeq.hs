{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The data structure that holds the cached ledger states.
--
-- The sequence is specialised to 'ExtLedgerState': the top-level LedgerDB
-- always holds extended states. Dropping the @l@ parameter sidesteps the
-- otherwise-illegal type-family application @'Handle' l m blk@ in the
-- 'Anchorable' instance head.
module Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
  ( -- * The ledger seq
    LedgerSeq (..)
  , LedgerSeq'
  , closeLedgerSeq
  , empty
  , LedgerDbCfg
  , configLedgerDb
  , LedgerDbCfgF (..)
  , LedgerDbPrune (..)

    -- * Apply Blocks
  , extend
  , prune
  , pruneToImmTipOnly
  , reapplyBlock
  , reapplyThenPush

    -- * Queries
  , anchor
  , anchorHandle
  , current
  , currentHandle
  , getPastLedgerAt
  , immutableTipSlot
  , isSaturated
  , maxRollback
  , rollback
  , rollbackN
  , rollbackToAnchor
  , rollbackToPoint
  , snapshots
  , tip
  , volatileStatesBimap
  ) where

import Cardano.Ledger.BaseTypes
import Data.Word
import GHC.Generics
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.AnchoredSeq hiding
  ( anchor
  , last
  , map
  , rollback
  )
import qualified Ouroboros.Network.AnchoredSeq as AS hiding (map)

{-------------------------------------------------------------------------------
  Anchorable instance for 'ExtStateHandle'
-------------------------------------------------------------------------------}

instance
  (BlockSupportsLedgerHD m blk, GetTip LedgerState blk) =>
  Anchorable (WithOrigin SlotNo) (ExtStateHandle m blk) (ExtStateHandle m blk)
  where
  asAnchor = id
  getAnchorMeasure _ = getTipSlot . state . unExtStateHandle

{-------------------------------------------------------------------------------
  The LedgerSeq
-------------------------------------------------------------------------------}

newtype LedgerSeq m blk = LedgerSeq
  { getLedgerSeq ::
      AnchoredSeq (WithOrigin SlotNo) (ExtStateHandle m blk) (ExtStateHandle m blk)
  }
  deriving Generic

deriving newtype instance
  (IOLike m, NoThunks (ExtStateHandle m blk)) => NoThunks (LedgerSeq m blk)

deriving newtype instance Eq (ExtStateHandle m blk) => Eq (LedgerSeq m blk)
deriving newtype instance Show (ExtStateHandle m blk) => Show (LedgerSeq m blk)

type LedgerSeq' m blk = LedgerSeq m blk

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Creates an empty @LedgerSeq@
empty ::
  (BlockSupportsLedgerHD m blk, GetTip LedgerState blk) =>
  ExtStateHandle m blk -> LedgerSeq m blk
empty = LedgerSeq . AS.Empty

-- | Close all 'LedgerTablesHandle' in this 'LedgerSeq', in particular that on
-- the anchor.
closeLedgerSeq ::
  (Monad m, BlockSupportsLedgerHD m blk, GetTip LedgerState blk) =>
  LedgerSeq m blk -> m ()
closeLedgerSeq (LedgerSeq l) =
  mapM_ closeExt $ AS.anchor l : AS.toOldestFirst l

{-------------------------------------------------------------------------------
  Apply blocks
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  Pruning
-------------------------------------------------------------------------------}

-- | Options for prunning the LedgerDB
data LedgerDbPrune
  = -- | Prune all states, keeping only the current tip.
    LedgerDbPruneAll
  | -- | Prune such that all (non-anchor) states are not older than the given
    -- slot.
    LedgerDbPruneBeforeSlot SlotNo
  deriving Show

{-------------------------------------------------------------------------------
  Config
-------------------------------------------------------------------------------}

data LedgerDbCfgF f l blk = LedgerDbCfg
  { ledgerDbCfgSecParam :: !(HKD f SecurityParam)
  , ledgerDbCfg :: !(HKD f (LedgerCfg l blk))
  , ledgerDbCfgComputeLedgerEvents :: !ComputeLedgerEvents
  }
  deriving Generic

type LedgerDbCfg l = Complete LedgerDbCfgF l

deriving instance NoThunks (LedgerCfg l blk) => NoThunks (LedgerDbCfg l blk)

configLedgerDb ::
  ConsensusProtocol (BlockProtocol blk) =>
  TopLevelConfig blk ->
  ComputeLedgerEvents ->
  LedgerDbCfg ExtLedgerState blk
configLedgerDb config evs =
  LedgerDbCfg
    { ledgerDbCfgSecParam = configSecurityParam config
    , ledgerDbCfg = ExtLedgerCfg config
    , ledgerDbCfgComputeLedgerEvents = evs
    }

-- | Apply a block on top of the ledger state and extend the LedgerSeq with
-- the result ledger state.
--
-- The @fst@ component of the result should be run to close the pruned states.
reapplyThenPush ::
  ( IOLike m
  , LedgerSupportsProtocol blk
  , BlockSupportsLedgerHD m blk
  ) =>
  LedgerDbCfg ExtLedgerState blk ->
  blk ->
  LedgerSeq m blk ->
  m (LedgerSeq m blk)
reapplyThenPush cfg ap db = do
  newSt <- reapplyBlock (ledgerDbCfgComputeLedgerEvents cfg) (ledgerDbCfg cfg) ap db
  let (m, db') = pruneToImmTipOnly $ extend newSt db
  m
  pure db'

reapplyBlock ::
  ( IOLike m
  , LedgerSupportsProtocol blk
  , BlockSupportsLedgerHD m blk
  ) =>
  ComputeLedgerEvents ->
  LedgerCfg ExtLedgerState blk ->
  blk ->
  LedgerSeq m blk ->
  m (ExtStateHandle m blk)
reapplyBlock evs cfg b = tickThenReapply evs cfg b . currentHandle

-- | Prune older ledger states according to the given 'LedgerDbPrune' strategy.
--
-- The @fst@ component of the returned value is an action closing the pruned
-- ledger states.
--
-- >>> ldb  = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> ldb' = LedgerSeq $ AS.fromOldestFirst     l1 [l2, l3]
-- >>> snd (prune (LedgerDbPruneBeforeSlot 1) ldb) == ldb'
-- True
--
-- where @lX@ is a ledger state from slot @X-1@ (or 'Origin' for @l0@).
prune ::
  (Monad m, BlockSupportsLedgerHD m blk, GetTip LedgerState blk) =>
  LedgerDbPrune ->
  LedgerSeq m blk ->
  (m (), LedgerSeq m blk)
prune howToPrune (LedgerSeq ldb) = case howToPrune of
  LedgerDbPruneAll ->
    (closeButHead before, LedgerSeq after)
   where
    (before, after) = (ldb, AS.Empty (AS.headAnchor ldb))
  LedgerDbPruneBeforeSlot slot ->
    (closeButHead before, LedgerSeq after)
   where
    -- The anchor of @vol'@ might still have a tip slot older than @slot@, which
    -- is fine to ignore (we will prune it later).
    (before, after) = AS.splitAtMeasure (NotOrigin slot) ldb
 where
  -- Above, we split @ldb@ into two sequences @before@ and @after@ such that
  -- @AS.headAnchor before == AS.anchor after@. We want to close all handles of
  -- @ldb@ not present in @after@, which are none if @before@ is empty, and all
  -- (in particular the anchor) of @before@ apart from the the head of @before@
  -- if @before@ is non-empty.
  closeButHead = \case
    AS.Empty _ -> pure ()
    toPrune AS.:> _ -> closeLedgerSeq (LedgerSeq toPrune)

-- NOTE: we must inline 'prune' otherwise we get unexplained thunks in
-- 'LedgerSeq' and thus a space leak. Alternatively, we could disable the
-- @-fstrictness@ optimisation (enabled by default for -O1). See
-- https://github.com/IntersectMBO/ouroboros-network/issues/2532.
--
-- NOTE (@js): this INLINE was inherited from before UTxO-HD, so maybe it is not
-- needed anymore.
{-# INLINE prune #-}

-- | Extending the LedgerDB with a valid ledger state.
--
-- >>> ldb            = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> LedgerSeq ldb' = extend l4 ldb
-- >>> AS.toOldestFirst ldb' == [l1, l2, l3, l4]
-- True
extend ::
  (BlockSupportsLedgerHD m blk, GetTip LedgerState blk) =>
  ExtStateHandle m blk ->
  LedgerSeq m blk ->
  LedgerSeq m blk
extend newState =
  LedgerSeq . (:> newState) . getLedgerSeq

{-------------------------------------------------------------------------------
  Reset
-------------------------------------------------------------------------------}

-- | Set the volatile tip as the immutable tip and prune all older states.
--
-- >>> ldb  = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> LedgerSeq ldb' = snd $ pruneToImmTipOnly ldb
-- >>> AS.anchor ldb' == l3 && AS.toOldestFirst ldb' == []
-- True
pruneToImmTipOnly ::
  (Monad m, BlockSupportsLedgerHD m blk, GetTip LedgerState blk) =>
  LedgerSeq m blk ->
  (m (), LedgerSeq m blk)
pruneToImmTipOnly = prune LedgerDbPruneAll

{-------------------------------------------------------------------------------
  Internal: rolling back
-------------------------------------------------------------------------------}

-- | Rollback @n@ ledger states.
--
-- Returns 'Nothing' if maximum rollback (usually @k@, but can be less on
-- startup or under corruption) is exceeded.
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> fmap (([l1] ==) . AS.toOldestFirst . getLedgerSeq) (rollbackN 2 ldb)
-- Just True
rollbackN ::
  (BlockSupportsLedgerHD m blk, GetTip LedgerState blk) =>
  Word64 ->
  LedgerSeq m blk ->
  Maybe (LedgerSeq m blk)
rollbackN n ldb
  | n <= maxRollback ldb =
      Just $ LedgerSeq (AS.dropNewest (fromIntegral n) $ getLedgerSeq ldb)
  | otherwise =
      Nothing

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | The ledger state at the tip of the chain
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> l3s == ledgerState (current ldb)
-- True
current ::
  (BlockSupportsLedgerHD m blk, GetTip LedgerState blk) =>
  LedgerSeq m blk -> ExtLedgerState blk
current = extLedgerState . currentHandle

currentHandle ::
  (BlockSupportsLedgerHD m blk, GetTip LedgerState blk) =>
  LedgerSeq m blk -> ExtStateHandle m blk
currentHandle = headAnchor . getLedgerSeq

-- | The ledger state at the anchor of the Volatile chain (i.e. the immutable
-- tip).
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> l0s == ledgerState (anchor ldb)
-- True
anchor ::
  (BlockSupportsLedgerHD m blk, GetTip LedgerState blk) =>
  LedgerSeq m blk -> ExtLedgerState blk
anchor = extLedgerState . anchorHandle

anchorHandle ::
  (BlockSupportsLedgerHD m blk, GetTip LedgerState blk) =>
  LedgerSeq m blk -> ExtStateHandle m blk
anchorHandle = AS.anchor . getLedgerSeq

-- | All snapshots currently stored by the ledger DB (new to old)
--
-- This also includes the snapshot at the anchor. For each snapshot we also
-- return the distance from the tip.
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> [(0, l3s), (1, l2s), (2, l1s)] == map (fmap ledgerState) (snapshots ldb)
-- True
snapshots ::
  (BlockSupportsLedgerHD m blk, GetTip LedgerState blk) =>
  LedgerSeq m blk -> [(Word64, ExtLedgerState blk)]
snapshots =
  zip [0 ..]
    . map extLedgerState
    . AS.toNewestFirst
    . getLedgerSeq

-- | How many blocks can we currently roll back?
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> maxRollback ldb
-- 3
maxRollback ::
  (BlockSupportsLedgerHD m blk, GetTip LedgerState blk) =>
  LedgerSeq m blk -> Word64
maxRollback =
  fromIntegral
    . AS.length
    . getLedgerSeq

-- | Reference to the block at the tip of the chain
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> tip ldb == getTip l3s
-- True
tip ::
  (GetTip LedgerState blk, BlockSupportsLedgerHD m blk) =>
  LedgerSeq m blk -> Point blk
tip = getTip . state . unExtStateHandle . currentHandle

-- | Have we seen at least @k@ blocks?
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> isSaturated (SecurityParam (unsafeNonZero 3)) ldb
-- True
-- >>> isSaturated (SecurityParam (unsafeNonZero 4)) ldb
-- False
isSaturated ::
  (BlockSupportsLedgerHD m blk, GetTip LedgerState blk) =>
  SecurityParam -> LedgerSeq m blk -> Bool
isSaturated (SecurityParam k) db =
  maxRollback db >= unNonZero k

-- | Get a past ledger state
--
--  \( O(\log(\min(i,n-i)) \)
--
-- When no ledger state (or anchor) has the given 'Point', 'Nothing' is
-- returned.
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> fmap ledgerState (getPastLedgerAt (Point (At (Block 4 4)) :: Point B) ldb) == Nothing
-- True
-- >>> fmap ledgerState (getPastLedgerAt (Point (At (Block 1 1)) :: Point B) ldb) == Just l2s
-- True
getPastLedgerAt ::
  ( HasHeader blk
  , GetTip LedgerState blk
  , StandardHash blk
  , BlockSupportsLedgerHD m blk
  ) =>
  Point blk ->
  LedgerSeq m blk ->
  Maybe (ExtLedgerState blk)
getPastLedgerAt pt db = current <$> rollback pt db

-- | Roll back the volatile states up to the specified point.
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> Just (LedgerSeq ldb') = rollbackToPoint (Point Origin) ldb
-- >>> AS.anchor ldb' == l0 && AS.toOldestFirst ldb' == []
-- True
-- >>> rollbackToPoint (Point (At (Block 1 2))) ldb == Nothing
-- True
-- >>> Just (LedgerSeq ldb') = rollbackToPoint (Point (At (Block 1 1))) ldb
-- >>> AS.anchor ldb' == l0 && AS.toOldestFirst ldb' == [l1, l2]
-- True
rollbackToPoint ::
  ( StandardHash blk
  , GetTip LedgerState blk
  , BlockSupportsLedgerHD m blk
  ) =>
  Point blk -> LedgerSeq m blk -> Maybe (LedgerSeq m blk)
rollbackToPoint pt (LedgerSeq ldb) = do
  LedgerSeq
    <$> AS.rollback
      (pointSlot pt)
      ((== pt) . getTip . state . unExtStateHandle . either id id)
      ldb

-- | Rollback the volatile states up to the volatile anchor.
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> LedgerSeq ldb' = rollbackToAnchor ldb
-- >>> AS.anchor ldb' == l0 && AS.toOldestFirst ldb' == []
-- True
rollbackToAnchor ::
  (BlockSupportsLedgerHD m blk, GetTip LedgerState blk) =>
  LedgerSeq m blk -> LedgerSeq m blk
rollbackToAnchor (LedgerSeq vol) =
  LedgerSeq (AS.Empty (AS.anchor vol))

-- | Get a prefix of the LedgerDB that ends at the given point
--
--  \( O(\log(\min(i,n-i)) \)
--
-- When no ledger state (or anchor) has the given 'Point', 'Nothing' is
-- returned.
rollback ::
  ( HasHeader blk
  , GetTip LedgerState blk
  , StandardHash blk
  , BlockSupportsLedgerHD m blk
  ) =>
  Point blk ->
  LedgerSeq m blk ->
  Maybe (LedgerSeq m blk)
rollback pt db
  | pt == getTip (ledgerState (anchor db)) =
      Just $ rollbackToAnchor db
  | otherwise =
      rollbackToPoint pt db

immutableTipSlot ::
  (GetTip LedgerState blk, BlockSupportsLedgerHD m blk) =>
  LedgerSeq m blk -> WithOrigin SlotNo
immutableTipSlot =
  getTipSlot
    . state
    . unExtStateHandle
    . AS.anchor
    . getLedgerSeq

-- | Transform the underlying volatile 'AnchoredSeq' using the given functions.
volatileStatesBimap ::
  ( BlockSupportsLedgerHD m blk
  , GetTip LedgerState blk
  , AS.Anchorable (WithOrigin SlotNo) a b
  ) =>
  (ExtLedgerState blk -> a) ->
  (ExtLedgerState blk -> b) ->
  LedgerSeq m blk ->
  AS.AnchoredSeq (WithOrigin SlotNo) a b
volatileStatesBimap f g =
  AS.bimap (f . extLedgerState) (g . extLedgerState)
    . getLedgerSeq

{-------------------------------------------------------------------------------
  docspec setup
-------------------------------------------------------------------------------}

-- $setup
-- >>> :set -XTypeFamilies -XFlexibleInstances -XMultiParamTypeClasses -XEmptyDataDecls
-- >>> import qualified Ouroboros.Network.AnchoredSeq as AS
-- >>> import Ouroboros.Network.Block
-- >>> import Ouroboros.Network.Point
-- >>> import Ouroboros.Consensus.Block.Abstract
-- >>> import Ouroboros.Consensus.Config.SecurityParam
-- >>> import Ouroboros.Consensus.Ledger.Basics
-- >>> import Ouroboros.Consensus.Ledger.Extended
-- >>> import Ouroboros.Consensus.HeaderValidation (genesisHeaderState)
-- >>> import Ouroboros.Consensus.Protocol.PBFT (PBft)
-- >>> import Ouroboros.Consensus.Protocol.PBFT.Crypto (PBftMockCrypto)
-- >>> import qualified Ouroboros.Consensus.Protocol.PBFT.State as PBftState
-- >>> import Cardano.Ledger.BaseTypes.NonZero
-- >>> import Cardano.Slotting.Slot
--
-- The doctests below exercise only the pure structural manipulation of
-- 'LedgerSeq' (anchored-sequence ops, pruning, rollback, etc.). To satisfy
-- the @BlockSupportsLedgerHD IO B@ obligation we declare a stub block @B@
-- with a one-constructor 'LedgerState' and stubbed handle methods — the
-- non-pure ones ('tickedState', 'duplicate', …) are never invoked. Routing
-- @BlockProtocol B@ through 'PBft' 'PBftMockCrypto' gives us a real
-- 'HeaderState' value (via 'genesisHeaderState' on the existing
-- 'PBftState.empty'), which is needed because 'ExtStateHandle' has a strict
-- 'extHeaderState' field.
--
-- >>> data B
-- >>> type instance HeaderHash B = Int
-- >>> instance StandardHash B
-- >>> instance HasHeader B where getHeaderFields = undefined
-- >>> type instance BlockProtocol B = PBft PBftMockCrypto
--
-- >>> data instance LedgerState B = LS (Point B) deriving Eq
-- >>> instance GetTip LedgerState B where getTip (LS p) = castPoint p
-- >>> data instance Ticked LedgerState B
-- >>> type instance LedgerTablesHandle IO B = ()
--
-- >>> :{
--  instance BlockSupportsLedgerHD IO B where
--    data StateHandle IO B = SH (LedgerState B)
--    data TickedStateHandle IO B
--    newStateHandle s _ = SH s
--    state (SH s) = s
--    tickedState _ = error "doctest: tickedState unused"
--    close _ = pure ()
--    closeTicked _ = pure ()
--    duplicate = pure
--    duplicateTicked = pure
--    getStats _ = Statistics 0
-- :}
--
-- 'LedgerSeq' equality is derived via @Eq (ExtStateHandle m blk)@; we
-- provide a structural one that compares only the underlying ledger state
-- (the strict 'HeaderState' field is identical across all handles in the
-- doctest fixtures).
--
-- >>> :{
--  instance Eq (ExtStateHandle IO B) where
--    ExtStateHandle (SH a) _ == ExtStateHandle (SH b) _ = a == b
-- :}
--
-- >>> hdrSt = genesisHeaderState PBftState.empty
--
-- >>> :{
--  s = [ LS (Point Origin)
--      , LS (Point (At (Block 0 0)))
--      , LS (Point (At (Block 1 1)))
--      , LS (Point (At (Block 2 2)))
--      , LS (Point (At (Block 3 3)))
--      ]
-- :}
--
-- >>> [l0s, l1s, l2s, l3s, l4s] = s
-- >>> [l0, l1, l2, l3, l4] = map (\ls -> ExtStateHandle (SH ls) hdrSt) s
