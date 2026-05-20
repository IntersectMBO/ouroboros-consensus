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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The data structure that holds the cached ledger states.
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
  Anchorable instance for 'Handle'
-------------------------------------------------------------------------------}

-- | 'Handle l m blk' lives in the 'AnchoredSeq' as both the anchor and the
-- elements; this orphan-ish 'Anchorable' instance is what lets that work.
--
-- TODO @js: depends on a polymorphic state projection from
-- @'Handle' l m blk@ to @l blk@ (deferred — see Layer 4 review point 3).
-- 'state' here only resolves for @l ~ LedgerState@ via 'MonadLedger'.
instance
  (MonadLedger m blk, GetTip l blk) =>
  Anchorable (WithOrigin SlotNo) (Handle l m blk) (Handle l m blk)
  where
  asAnchor = id
  getAnchorMeasure _ = getTipSlot . state

{-------------------------------------------------------------------------------
  The LedgerSeq
-------------------------------------------------------------------------------}

newtype LedgerSeq m l blk = LedgerSeq
  { getLedgerSeq :: AnchoredSeq (WithOrigin SlotNo) (Handle l m blk) (Handle l m blk)
  }
  deriving Generic

deriving newtype instance (IOLike m, NoThunks (Handle l m blk)) => NoThunks (LedgerSeq m l blk)

deriving newtype instance Eq (Handle l m blk) => Eq (LedgerSeq m l blk)
deriving newtype instance Show (Handle l m blk) => Show (LedgerSeq m l blk)

type LedgerSeq' m blk = LedgerSeq m ExtLedgerState blk

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Creates an empty @LedgerSeq@
empty ::
  ( GetTip l blk
  , IOLike m
  , MonadLedger m blk
  ) =>
  Handle l m blk ->
  LedgerSeq m l blk
empty = LedgerSeq . AS.Empty

-- | Close all 'LedgerTablesHandle' in this 'LedgerSeq', in particular that on
-- the anchor.
closeLedgerSeq :: (Monad m, MonadLedger m blk) => LedgerSeq m l blk -> m ()
closeLedgerSeq (LedgerSeq l) =
  mapM_ close $ AS.anchor l : AS.toOldestFirst l

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
  (IOLike m, ApplyBlock l blk, MonadLedger m blk) =>
  LedgerDbCfg l blk ->
  blk ->
  LedgerSeq m l blk ->
  m (LedgerSeq m l blk)
reapplyThenPush cfg ap db = do
  newSt <- reapplyBlock (ledgerDbCfgComputeLedgerEvents cfg) (ledgerDbCfg cfg) ap db
  let (m, db') = pruneToImmTipOnly $ extend newSt db
  m
  pure db'

reapplyBlock ::
  (ApplyBlock l blk, IOLike m, MonadLedger m blk) =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  blk ->
  LedgerSeq m l blk ->
  m (Handle l m blk)
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
  (Monad m, GetTip l blk, MonadLedger m blk) =>
  LedgerDbPrune ->
  LedgerSeq m l blk ->
  (m (), LedgerSeq m l blk)
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
  (GetTip l blk, MonadLedger m blk) =>
  Handle l m blk ->
  LedgerSeq m l blk ->
  LedgerSeq m l blk
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
  (Monad m, MonadLedger m blk, GetTip l blk) =>
  LedgerSeq m l blk ->
  (m (), LedgerSeq m l blk)
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
  (MonadLedger m blk, GetTip l blk) =>
  Word64 ->
  LedgerSeq m l blk ->
  Maybe (LedgerSeq m l blk)
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
-- >>> l3s == current ldb
-- True
current :: (MonadLedger m blk, GetTip l blk) => LedgerSeq m l blk -> l blk
current = state . currentHandle

currentHandle :: (MonadLedger m blk, GetTip l blk) => LedgerSeq m l blk -> Handle l m blk
currentHandle = headAnchor . getLedgerSeq

-- | The ledger state at the anchor of the Volatile chain (i.e. the immutable
-- tip).
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> l0s == anchor ldb
-- True
anchor :: MonadLedger m blk => LedgerSeq m l blk -> l blk
anchor = state . anchorHandle

anchorHandle :: LedgerSeq m l blk -> Handle l m blk
anchorHandle = AS.anchor . getLedgerSeq

-- | All snapshots currently stored by the ledger DB (new to old)
--
-- This also includes the snapshot at the anchor. For each snapshot we also
-- return the distance from the tip.
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> [(0, l3s), (1, l2s), (2, l1s)] == snapshots ldb
-- True
snapshots :: MonadLedger m blk => LedgerSeq m l blk -> [(Word64, l blk)]
snapshots =
  zip [0 ..]
    . map state
    . AS.toNewestFirst
    . getLedgerSeq

-- | How many blocks can we currently roll back?
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> maxRollback ldb
-- 3
maxRollback :: (MonadLedger m blk, GetTip l blk) => LedgerSeq m l blk -> Word64
maxRollback =
  fromIntegral
    . AS.length
    . getLedgerSeq

-- | Reference to the block at the tip of the chain
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> tip ldb == getTip l3s
-- True
tip :: (MonadLedger m blk, GetTip l blk) => LedgerSeq m l blk -> Point blk
tip = getTip . current

-- | Have we seen at least @k@ blocks?
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> isSaturated (SecurityParam (unsafeNonZero 3)) ldb
-- True
-- >>> isSaturated (SecurityParam (unsafeNonZero 4)) ldb
-- False
isSaturated ::
  (MonadLedger m blk, GetTip l blk) => SecurityParam -> LedgerSeq m l blk -> Bool
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
-- >>> getPastLedgerAt (Point (At (Block 4 4)) :: Point B) ldb == Nothing
-- True
-- >>> getPastLedgerAt (Point (At (Block 1 1)) :: Point B) ldb == Just l2s
-- True
getPastLedgerAt ::
  ( HasHeader blk
  , GetTip l blk
  , HeaderHash (l blk) ~ HeaderHash blk
  , StandardHash blk
  , MonadLedger m blk
  ) =>
  Point blk ->
  LedgerSeq m l blk ->
  Maybe (l blk)
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
  , GetTip l blk
  , MonadLedger m blk
  ) =>
  Point blk -> LedgerSeq m l blk -> Maybe (LedgerSeq m l blk)
rollbackToPoint pt (LedgerSeq ldb) = do
  LedgerSeq
    <$> AS.rollback
      (pointSlot pt)
      ((== pt) . getTip . either state state)
      ldb

-- | Rollback the volatile states up to the volatile anchor.
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> LedgerSeq ldb' = rollbackToAnchor ldb
-- >>> AS.anchor ldb' == l0 && AS.toOldestFirst ldb' == []
-- True
rollbackToAnchor ::
  (GetTip l blk, MonadLedger m blk) =>
  LedgerSeq m l blk -> LedgerSeq m l blk
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
  , GetTip l blk
  , HeaderHash (l blk) ~ HeaderHash blk
  , StandardHash blk
  , MonadLedger m blk
  ) =>
  Point blk ->
  LedgerSeq m l blk ->
  Maybe (LedgerSeq m l blk)
rollback pt db
  | pt == castPoint (getTip (anchor db)) =
      Just $ rollbackToAnchor db
  | otherwise =
      rollbackToPoint (castPoint pt) db

immutableTipSlot ::
  (GetTip l blk, MonadLedger m blk) =>
  LedgerSeq m l blk -> WithOrigin SlotNo
immutableTipSlot =
  getTipSlot
    . state
    . AS.anchor
    . getLedgerSeq

-- | Transform the underlying volatile 'AnchoredSeq' using the given functions.
volatileStatesBimap ::
  (MonadLedger m blk, AS.Anchorable (WithOrigin SlotNo) a b) =>
  (l blk -> a) ->
  (l blk -> b) ->
  LedgerSeq m l blk ->
  AS.AnchoredSeq (WithOrigin SlotNo) a b
volatileStatesBimap f g =
  AS.bimap (f . state) (g . state)
    . getLedgerSeq

{-------------------------------------------------------------------------------
  docspec setup
-------------------------------------------------------------------------------}

-- $setup
-- >>> :set -XTypeFamilies -XUndecidableInstances -XFlexibleInstances -XTypeApplications -XMultiParamTypeClasses
-- >>> import qualified Ouroboros.Network.AnchoredSeq as AS
-- >>> import Ouroboros.Network.Block
-- >>> import Ouroboros.Network.Point
-- >>> import Ouroboros.Consensus.Ledger.Tables
-- >>> import Ouroboros.Consensus.Ledger.Tables.Utils
-- >>> import Ouroboros.Consensus.Ledger.Basics
-- >>> import Ouroboros.Consensus.Config
-- >>> import Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory
-- >>> import Ouroboros.Consensus.Util.IndexedMemPack
-- >>> import Ouroboros.Consensus.Storage.LedgerDB.API
-- >>> import Cardano.Ledger.BaseTypes.NonZero
-- >>> import Data.Void
-- >>> import Cardano.Slotting.Slot
-- >>> import Data.Proxy
--
-- >>> data B
-- >>> type instance HeaderHash B = Int
-- >>> instance StandardHash B
-- >>> instance HasHeader B where getHeaderFields = undefined
--
-- >>> type instance TxIn B = Void
-- >>> type instance TxOut B = Void
--
-- >>> data instance LedgerState B (mk :: MapKind) = LS (Point B)
-- >>> instance Eq (LedgerState B EmptyMK) where LS p1 == LS p2 = p1 == p2
-- >>> :{
--  instance GetTip (LedgerState B) where
--    getTip (LS p) = castPoint p
-- :}
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
--
-- >>> :{
--  instance LedgerTablesAreTrivial LedgerState B where
--    convertMapKind (LS p) = LS p
--  instance HasLedgerTables LedgerState B where
--    projectLedgerTables _ = trivialLedgerTables (Proxy @LedgerState)
--    withLedgerTables st _ = convertMapKind st
--  instance IndexedMemPack LedgerState B Void where
--    indexedTypeName _ _ = typeName @Void
--    indexedPackedByteCount _ = packedByteCount
--    indexedPackM _ = packM
--    indexedUnpackM _ = unpackM
-- :}
--
-- >>> :{
--  emptyHandle =
--     LedgerTablesHandle
--        (pure ())
--        (\_ _ -> pure emptyHandle)
--        (pure emptyHandle)
--        (\_ _ -> pure (trivialLedgerTables (Proxy @LedgerState)))
--        (\_ _ -> pure (trivialLedgerTables (Proxy @LedgerState), Nothing))
--        (\_ -> pure (trivialLedgerTables (Proxy @LedgerState)))
--        (\_ _ -> undefined)
--        0 :: LedgerTablesHandle IO LedgerState B
-- :}
--
-- >>> [l0, l1, l2, l3, l4] = map (flip StateHandle emptyHandle) s
