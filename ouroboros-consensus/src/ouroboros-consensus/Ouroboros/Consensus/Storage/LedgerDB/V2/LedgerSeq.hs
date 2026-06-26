{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- | The data structure that holds the cached ledger states.
module Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
  ( -- * LedgerHandles
    LedgerTablesHandle (..)
  , EraRangeReader (..)
  , EraRangeReaderProvider (..)
  , withEraRangeReader
  , mkEraRangeReaderProvider

    -- * The ledger seq
  , LedgerSeq (..)
  , LedgerSeq'
  , StateRef (..)
  , closeLedgerSeq
  , empty
  , empty'

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
import Data.Function (on)
import Data.Word
import GHC.Generics
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config.SecurityParam
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Forker
  ( EraRangeReader (..)
  , EraRangeReaderProvider (..)
  , RangeReadTables
  )
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.AnchoredSeq hiding
  ( anchor
  , last
  , map
  , rollback
  )
import qualified Ouroboros.Network.AnchoredSeq as AS hiding (map)
import System.FS.CRC (CRC)
import Prelude hiding (read)

{-------------------------------------------------------------------------------
  LedgerTablesHandles
-------------------------------------------------------------------------------}

-- | The interface fulfilled by handles on both the InMemory and LSM handles.
--
-- The most relevant concept is handle duplication:
--
-- A duplicated handle must provide access to all the data that was there in
-- the original handle while being able to mutate in ways different than the
-- original handle.
--
-- When applying diffs to a table, we will first duplicate the handle, then
-- apply the diffs in the copy. It is expected that duplicating the handle
-- takes constant time.
data LedgerTablesHandle m l blk = LedgerTablesHandle
  { close :: !(m ())
  -- ^ Close the handle
  , duplicateWithDiffs :: !(Diff blk -> m (LedgerTablesHandle m l blk))
  -- ^ Create a new handle by duplicating this one and pushing a block's diff to
  -- it (the diff is applied to the held values via 'forward').
  --
  -- This is expected to be used when applying new blocks onto a forker, which
  -- happens only in chain selection (see 'forkerPush') and initial chain
  -- selection (see 'reapplyBlock').
  , duplicate :: !(m (LedgerTablesHandle m l blk))
  -- ^ Create an duplicate of a handle. This will be used when opening read-only
  -- forkers and also to open the first handle for a forker used in chain
  -- selection.
  , read :: !(l blk EmptyMK -> Keys blk -> m (Values blk))
  -- ^ Read values for the given keys from the tables, and deserialize them as
  -- if they were from the same era as the given ledger state.
  , readRange :: !(RangeReadTables m blk)
  -- ^ Read one bounded page of the /current era/'s tables, for LSQ
  -- @QFTraverseTables@ queries (see 'EraRangeReader' / 'withEraRangeReader').
  -- The caller supplies the projection onto the current era @x@; see
  -- 'RangeReadTables' for the per-backend semantics and cursor contract.
  , takeHandleSnapshot :: !(l blk EmptyMK -> String -> m (Maybe CRC))
  -- ^ Take a snapshot of a handle. The given ledger state is used to decide the
  -- encoding of the values based on the current era.
  --
  -- It returns a CRC only on backends that support it, as the InMemory backend.
  , tablesSize :: !Int
  -- ^ Consult the size of the ledger tables in the database.
  }
  deriving NoThunks via OnlyCheckWhnfNamed "LedgerTablesHandle" (LedgerTablesHandle m l blk)

-- | Build an 'EraRangeReader' for the current era @x@ from a tables handle.
--
-- The caller supplies the projection mapping the handle's stored values onto era
-- @x@, and the batch size bounds each page (see 'EraRangeReader').
--
-- The era @x@ is assumed to be the /current/ era (the only era for which the
-- backing store holds tables). 'QFTraverseTables' queries are always dispatched
-- to the current era by the upstream 'QueryIfCurrent' handling, so a mismatch
-- cannot reach here; this is relied upon rather than re-checked. TODO @js:
-- revisit whether to encode this invariant explicitly.
withEraRangeReader ::
  SingleEraBlockSupportsUTxOHD x =>
  LedgerTablesHandle m l blk ->
  -- | Project the handle's stored values onto the current era @x@.
  (Values blk -> Values x) ->
  -- | Batch size (maximum entries per page).
  Int ->
  EraRangeReader m x
withEraRangeReader h proj batchSize =
  EraRangeReader $ \prev -> readRange h proj prev batchSize

-- | Build an 'EraRangeReaderProvider' from a tables handle and a batch size.
--
-- The provider is valid only while the underlying handle is open (it is built
-- from the read-only forker's handle and shares its lifetime).
mkEraRangeReaderProvider ::
  LedgerTablesHandle m l blk ->
  -- | Batch size (maximum entries per page).
  Int ->
  EraRangeReaderProvider m blk
mkEraRangeReaderProvider h batchSize =
  EraRangeReaderProvider $ \proj -> withEraRangeReader h proj batchSize

{-------------------------------------------------------------------------------
  StateRef, represents a full ledger state, i.e. with a handle for its tables
-------------------------------------------------------------------------------}

-- | A full ledger state: the pure state @l blk@ paired with a sibling handle
-- holding its on-disk tables.
--
-- The table data lives entirely behind the 'LedgerTablesHandle', which the rest
-- of the LedgerDB threads around backend-agnostically.
data StateRef m l blk = StateRef
  { state :: !(l blk EmptyMK)
  , tables :: !(LedgerTablesHandle m l blk)
  }
  deriving Generic

deriving instance (IOLike m, NoThunks (l blk EmptyMK)) => NoThunks (StateRef m l blk)

instance Eq (l blk EmptyMK) => Eq (StateRef m l blk) where
  (==) = (==) `on` state

instance Show (l blk EmptyMK) => Show (StateRef m l blk) where
  show = show . state

instance GetTip (l blk) => Anchorable (WithOrigin SlotNo) (StateRef m l blk) (StateRef m l blk) where
  asAnchor = id
  getAnchorMeasure _ = getTipSlot . state

{-------------------------------------------------------------------------------
  The LedgerSeq
-------------------------------------------------------------------------------}

newtype LedgerSeq m l blk = LedgerSeq
  { getLedgerSeq :: AnchoredSeq (WithOrigin SlotNo) (StateRef m l blk) (StateRef m l blk)
  }
  deriving Generic

deriving newtype instance (IOLike m, NoThunks (l blk EmptyMK)) => NoThunks (LedgerSeq m l blk)

deriving newtype instance Eq (l blk EmptyMK) => Eq (LedgerSeq m l blk)
deriving newtype instance Show (l blk EmptyMK) => Show (LedgerSeq m l blk)

type LedgerSeq' m blk = LedgerSeq m ExtLedgerState blk

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Creates an empty @LedgerSeq@
empty ::
  ( GetTip (l blk)
  , IOLike m
  ) =>
  l blk EmptyMK ->
  init ->
  (init -> m (LedgerTablesHandle m l blk)) ->
  m (LedgerSeq m l blk)
empty st tbs new = LedgerSeq . AS.Empty . StateRef st <$> new tbs

-- | Creates an empty @LedgerSeq@ from a state and its full table values.
empty' ::
  ( GetTip (l blk)
  , IOLike m
  ) =>
  l blk EmptyMK ->
  Values blk ->
  (Values blk -> m (LedgerTablesHandle m l blk)) ->
  m (LedgerSeq m l blk)
empty' = empty

-- | Close all 'LedgerTablesHandle' in this 'LedgerSeq', in particular that on
-- the anchor.
closeLedgerSeq :: Monad m => LedgerSeq m l blk -> m ()
closeLedgerSeq (LedgerSeq l) =
  mapM_ (close . tables) $ AS.anchor l : AS.toOldestFirst l

{-------------------------------------------------------------------------------
  Apply blocks
-------------------------------------------------------------------------------}

-- | Apply a block on top of the ledger state and extend the LedgerSeq with
-- the result ledger state.
--
-- The @fst@ component of the result should be run to close the pruned states.
reapplyThenPush ::
  (IOLike m, ApplyBlock l blk) =>
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
  forall l blk m.
  (ApplyBlock l blk, IOLike m) =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  blk ->
  LedgerSeq m l blk ->
  m (StateRef m l blk)
reapplyBlock evs cfg b db = do
  let ks = blockKeys b
      StateRef st tbs = currentHandle db
  vals <- read tbs st ks
  let (st', diff) = tickThenReapply evs cfg b vals st
  newtbs <- duplicateWithDiffs tbs diff
  pure (StateRef st' newtbs)

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
  (Monad m, GetTip (l blk)) =>
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
  GetTip (l blk) =>
  StateRef m l blk ->
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
  (Monad m, GetTip (l blk)) =>
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
  GetTip (l blk) =>
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
current :: GetTip (l blk) => LedgerSeq m l blk -> l blk EmptyMK
current = state . currentHandle

currentHandle :: GetTip (l blk) => LedgerSeq m l blk -> StateRef m l blk
currentHandle = headAnchor . getLedgerSeq

-- | The ledger state at the anchor of the Volatile chain (i.e. the immutable
-- tip).
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> l0s == anchor ldb
-- True
anchor :: LedgerSeq m l blk -> l blk EmptyMK
anchor = state . anchorHandle

anchorHandle :: LedgerSeq m l blk -> StateRef m l blk
anchorHandle = AS.anchor . getLedgerSeq

-- | All snapshots currently stored by the ledger DB (new to old)
--
-- This also includes the snapshot at the anchor. For each snapshot we also
-- return the distance from the tip.
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> [(0, l3s), (1, l2s), (2, l1s)] == snapshots ldb
-- True
snapshots :: LedgerSeq m l blk -> [(Word64, l blk EmptyMK)]
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
maxRollback :: GetTip (l blk) => LedgerSeq m l blk -> Word64
maxRollback =
  fromIntegral
    . AS.length
    . getLedgerSeq

-- | Reference to the block at the tip of the chain
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> tip ldb == getTip l3s
-- True
tip :: GetTip (l blk) => LedgerSeq m l blk -> Point (l blk)
tip = castPoint . getTip . current

-- | Have we seen at least @k@ blocks?
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> isSaturated (SecurityParam (unsafeNonZero 3)) ldb
-- True
-- >>> isSaturated (SecurityParam (unsafeNonZero 4)) ldb
-- False
isSaturated :: GetTip (l blk) => SecurityParam -> LedgerSeq m l blk -> Bool
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
  , GetTip (l blk)
  , HeaderHash (l blk) ~ HeaderHash blk
  , StandardHash (l blk)
  ) =>
  Point blk ->
  LedgerSeq m l blk ->
  Maybe (l blk EmptyMK)
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
  ( StandardHash (l blk)
  , GetTip (l blk)
  ) =>
  Point (l blk) -> LedgerSeq m l blk -> Maybe (LedgerSeq m l blk)
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
  GetTip (l blk) =>
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
  , GetTip (l blk)
  , HeaderHash (l blk) ~ HeaderHash blk
  , StandardHash (l blk)
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
  GetTip (l blk) =>
  LedgerSeq m l blk -> WithOrigin SlotNo
immutableTipSlot =
  getTipSlot
    . state
    . AS.anchor
    . getLedgerSeq

-- | Transform the underlying volatile 'AnchoredSeq' using the given functions.
volatileStatesBimap ::
  AS.Anchorable (WithOrigin SlotNo) a b =>
  (l blk EmptyMK -> a) ->
  (l blk EmptyMK -> b) ->
  LedgerSeq m l blk ->
  AS.AnchoredSeq (WithOrigin SlotNo) a b
volatileStatesBimap f g =
  AS.bimap (f . state) (g . state)
    . getLedgerSeq

{-------------------------------------------------------------------------------
  docspec setup
-------------------------------------------------------------------------------}

-- $setup
-- >>> :set -XTypeFamilies -XUndecidableInstances -XFlexibleInstances -XTypeApplications -XMultiParamTypeClasses -XRankNTypes
-- >>> import qualified Ouroboros.Network.AnchoredSeq as AS
-- >>> import Ouroboros.Network.Block
-- >>> import Ouroboros.Network.Point
-- >>> import Ouroboros.Consensus.Ledger.Basics
-- >>> import Ouroboros.Consensus.Config
-- >>> import Ouroboros.Consensus.Storage.LedgerDB.API
-- >>> import Cardano.Ledger.BaseTypes.NonZero
-- >>> import Data.Void
-- >>> import Cardano.Slotting.Slot
-- >>> import Prelude hiding (read)
--
-- >>> data B
-- >>> type instance HeaderHash B = Int
-- >>> instance StandardHash B
-- >>> instance HasHeader B where getHeaderFields = undefined
--
-- >>> type instance TxIn B = Void
-- >>> type instance TxOut B = Void
--
-- >>> data instance LedgerState B = LS (Point B)
-- >>> instance Eq (LedgerState B) where LS p1 == LS p2 = p1 == p2
-- >>> instance GetTip (LedgerState B) where getTip (LS p) = castPoint p
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
-- The on-disk tables of @B@ are trivial (@()@), so 'BlockSupportsUTxOHD' is
-- discharged with no-op operations; the examples below only manipulate the
-- sequence structure and never touch the handle's table payloads.
--
-- >>> :{
--  instance BlockSupportsUTxOHD B where
--    type Keys B = ()
--    type Values B = ()
--    type Diff B = ()
--    blockKeys _ = ()
--    forward _ = id
--    restrictValues _ = id
--    valuesSize _ = 0
--    encodeValues _ = mempty
--    decodeValues _ = pure ()
-- :}
--
-- >>> :{
--  emptyHandle :: LedgerTablesHandle IO LedgerState B
--  emptyHandle =
--     LedgerTablesHandle
--        { close = pure ()
--        , duplicateWithDiffs = \_ -> pure emptyHandle
--        , duplicate = pure emptyHandle
--        , read = \_ _ -> pure ()
--        , readRange = \proj _ _ -> pure (proj ())
--        , takeHandleSnapshot = \_ _ -> undefined
--        , tablesSize = 0
--        }
-- :}
--
-- >>> [l0, l1, l2, l3, l4] = map (flip StateRef emptyHandle) s
