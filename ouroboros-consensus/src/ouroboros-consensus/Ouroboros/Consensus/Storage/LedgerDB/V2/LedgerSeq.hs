{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | The data structure that holds the cached ledger states.
module Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq (
    -- * LedgerHandles
    LedgerTablesHandle (..)
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

import qualified Data.Bifunctor as B
import           Data.Function (on)
import           Data.Word
import           GHC.Generics
import           NoThunks.Class
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Storage.LedgerDB.API.Config
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Network.AnchoredSeq hiding (anchor, last, map,
                     rollback)
import qualified Ouroboros.Network.AnchoredSeq as AS hiding (map)
import           Prelude hiding (read)

{-------------------------------------------------------------------------------
  LedgerTablesHandles
-------------------------------------------------------------------------------}

data LedgerTablesHandle m l = LedgerTablesHandle {
    close       :: !(m ())
  , duplicate   :: !(m (LedgerTablesHandle m l))
  , read        :: !(LedgerTables l KeysMK -> m (LedgerTables l ValuesMK))
  , readRange   :: !((Key l, Key l) -> m (LedgerTables l ValuesMK))
  , write       :: !(LedgerTables l DiffMK -> m ())
  , writeToDisk :: !(String -> m ())
  , tablesSize  :: !(m (Maybe Int))
  , isOpen      :: !(m Bool)
  }
  deriving NoThunks via OnlyCheckWhnfNamed "LedgerTablesHandle" (LedgerTablesHandle m l)

{-------------------------------------------------------------------------------
  StateRef, represents a full virtual ledger state
-------------------------------------------------------------------------------}

-- | For unary blocks, it would be the same to hold a stowed ledger state, an
-- unstowed one or a tuple with the state and the tables, however, for a n-ary
-- block, these are not equivalent.
--
-- If we were to hold a sequence of type @LedgerState blk EmptyMK@ with stowed
-- values, we would have to translate the entirety of the tables on epoch
-- boundaries.
--
-- If we were to hold a sequence of type @LedgerState blk ValuesMK@ we would
-- have the same problem as the @mk@ in the state actually refers to the @mk@ in
-- the @HardForkState@'ed state.
--
-- Therefore it sounds reasonable to hold a @LedgerState blk EmptyMK@ with no
-- values, and a @LedgerTables blk ValuesMK@ next to it, that will live its
-- entire lifetime as @LedgerTables@ of the @HardForkBlock@.
data StateRef m l = StateRef {
    state  :: !(l EmptyMK)
  , tables :: !(LedgerTablesHandle m l)
  } deriving (Generic)

deriving instance (IOLike m, NoThunks (l EmptyMK)) => NoThunks (StateRef m l)

instance Eq (l EmptyMK) => Eq (StateRef m l) where
  (==) = (==) `on` state

instance Show (l EmptyMK) => Show (StateRef m l) where
  show = show . state

instance GetTip l => Anchorable (WithOrigin SlotNo) (StateRef m l) (StateRef m l) where
  asAnchor = id
  getAnchorMeasure _ = getTipSlot . state

{-------------------------------------------------------------------------------
  The LedgerSeq
-------------------------------------------------------------------------------}

newtype LedgerSeq m l = LedgerSeq {
    getLedgerSeq :: AnchoredSeq (WithOrigin SlotNo) (StateRef m l) (StateRef m l)
  } deriving (Generic)

deriving newtype instance (IOLike m, NoThunks (l EmptyMK)) => NoThunks (LedgerSeq m l)

deriving newtype instance Eq   (l EmptyMK) => Eq   (LedgerSeq m l)
deriving newtype instance Show (l EmptyMK) => Show (LedgerSeq m l)

type LedgerSeq' m blk = LedgerSeq m (ExtLedgerState blk)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Creates an empty @LedgerSeq@
empty ::
     ( GetTip l
     , IOLike m
     )
  => l EmptyMK
  -> LedgerTables l ValuesMK
  -> (LedgerTables l ValuesMK -> m ( LedgerTablesHandle m l))
  -> m (LedgerSeq m l)
empty st tbs new = LedgerSeq . AS.Empty . StateRef st <$> new tbs

-- | Creates an empty @LedgerSeq@
empty' ::
     ( GetTip l
     , IOLike m
     , HasLedgerTables l
     )
  => l ValuesMK
  -> (LedgerTables l ValuesMK -> m (LedgerTablesHandle m l))
  -> m (LedgerSeq m l)
empty' st = empty (forgetLedgerTables st) (ltprj st)

closeLedgerSeq :: Monad m => LedgerSeq m l -> m ()
closeLedgerSeq = mapM_ (close . tables) . toOldestFirst . getLedgerSeq

{-------------------------------------------------------------------------------
  Apply blocks
-------------------------------------------------------------------------------}

-- | If applying a block on top of the ledger state at the tip is succesful,
-- extend the DbChangelog with the resulting ledger state.
--
-- Note that we require @c@ (from the particular choice of @Ap m l blk c@) so
-- this sometimes can throw ledger errors.
reapplyThenPush :: (IOLike m, ApplyBlock l blk)
              => ResourceRegistry m
              -> LedgerDbCfg l
              -> blk
              ->    LedgerSeq m l
              -> m (LedgerSeq m l, LedgerSeq m l)
reapplyThenPush rr cfg ap db =
    (\current' -> prune (ledgerDbCfgSecParam cfg) $ extend current' db) <$>
      reapplyBlock (ledgerDbCfg cfg) ap rr db

reapplyBlock :: forall m l blk. (ApplyBlock l blk, IOLike m)
           => LedgerCfg l
           -> blk
           -> ResourceRegistry m
           -> LedgerSeq m l
           -> m (StateRef m l)
reapplyBlock cfg b _rr db = do
  let ks = getBlockKeySets b
  case currentHandle db of
    StateRef st tbs -> do
      newtbs <- duplicate tbs
      vals <- read newtbs ks
      let st' = tickThenReapply cfg b (st `withLedgerTables` vals)
      let (newst, diffs) = (forgetLedgerTables st', ltprj st')
      write newtbs diffs
      pure (StateRef newst newtbs)

-- | Prune ledger states from the front until at we have at most @k@ in the
-- LedgerDB, excluding the one stored at the anchor.
--
-- >>> ldb  = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> ldb' = LedgerSeq $ AS.fromOldestFirst     l1 [l2, l3]
-- >>> prune (SecurityParam 2) ldb == ldb'
-- True
prune :: GetTip l
      => SecurityParam
      -> LedgerSeq m l
      -> (LedgerSeq m l, LedgerSeq m l)
prune (SecurityParam k) (LedgerSeq ldb) =
    if toEnum nvol <= k
    then (LedgerSeq $ Empty (AS.anchor ldb), LedgerSeq ldb)
    else B.bimap (LedgerSeq . dropNewest 1) LedgerSeq $ AS.splitAt (nvol - fromEnum k) ldb
  where
    nvol = AS.length ldb

-- NOTE: we must inline 'prune' otherwise we get unexplained thunks in
-- 'LedgerSeq' and thus a space leak. Alternatively, we could disable the
-- @-fstrictness@ optimisation (enabled by default for -O1). See #2532.
-- NOTE (@js): this INLINE was inherited from before UTxO-HD, so maybe it is not
-- needed anymore.
{-# INLINE prune #-}

-- | Extending the LedgerDB with a valid ledger state.
--
-- >>> ldb            = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> LedgerSeq ldb' = extend (ValidLedgerState l4) ldb
-- >>> AS.toOldestFirst ldb' == [l1, l2, l3, l4]
-- True
extend :: GetTip l
       => StateRef m l
       -> LedgerSeq m l
       -> LedgerSeq m l
extend newState =
  LedgerSeq . (:> newState) . getLedgerSeq

{-------------------------------------------------------------------------------
  Reset
-------------------------------------------------------------------------------}

-- | When creating a new @LedgerDB@, we should load whichever snapshot we find
-- and then replay the chain up to the immutable tip. When we get there, the
-- @LedgerDB@ will have a @k@-long sequence of states, which all come from
-- immutable blocks, so we just prune all of them and only keep the last one as
-- an anchor, as it is the immutable tip. Then we can proceed with opening the
-- VolatileDB.
--
-- If we didn't do this step, the @LedgerDB@ would accept rollbacks into the
-- immutable part of the chain, which must never be possible.
--
-- >>> ldb  = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> LedgerSeq ldb' = pruneToImmTipOnly ldb
-- >>> AS.anchor ldb' == l3 && AS.toOldestFirst ldb' == []
-- True
pruneToImmTipOnly :: GetTip l
                  => LedgerSeq m l
                  -> (LedgerSeq m l, LedgerSeq m l)
pruneToImmTipOnly = prune (SecurityParam 0)

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
     GetTip l
  => Word64
  -> LedgerSeq m l
  -> Maybe (LedgerSeq m l)
rollbackN n ldb
    | n <= maxRollback ldb
    = Just $ LedgerSeq (AS.dropNewest (fromIntegral n) $ getLedgerSeq ldb)
    | otherwise
    = Nothing

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | The ledger state at the tip of the chain
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> l3s == current ldb
-- True
current :: GetTip l => LedgerSeq m l -> l EmptyMK
current = state . currentHandle

currentHandle :: GetTip l => LedgerSeq m l -> StateRef m l
currentHandle = headAnchor . getLedgerSeq

-- | The ledger state at the anchor of the Volatile chain (i.e. the immutable
-- tip).
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> l0s == anchor ldb
-- True
anchor :: LedgerSeq m l -> l EmptyMK
anchor = state . anchorHandle

anchorHandle :: LedgerSeq m l -> StateRef m l
anchorHandle = AS.anchor . getLedgerSeq

-- | All snapshots currently stored by the ledger DB (new to old)
--
-- This also includes the snapshot at the anchor. For each snapshot we also
-- return the distance from the tip.
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> [(0, l3s), (1, l2s), (2, l1s)] == snapshots ldb
-- True
snapshots :: LedgerSeq m l -> [(Word64, l EmptyMK)]
snapshots =
      zip [0..]
    . map state
    . AS.toNewestFirst
    . getLedgerSeq

-- | How many blocks can we currently roll back?
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> maxRollback ldb
-- 3
maxRollback :: GetTip l => LedgerSeq m l -> Word64
maxRollback =
    fromIntegral
  . AS.length
  . getLedgerSeq

-- | Reference to the block at the tip of the chain
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> tip ldb == getTip l3s
-- True
tip :: GetTip l => LedgerSeq m l -> Point l
tip = castPoint . getTip . current

-- | Have we seen at least @k@ blocks?
--
-- >>> ldb = LedgerSeq $ AS.fromOldestFirst l0 [l1, l2, l3]
-- >>> isSaturated (SecurityParam 3) ldb
-- True
-- >>> isSaturated (SecurityParam 4) ldb
-- False
isSaturated :: GetTip l => SecurityParam -> LedgerSeq m l -> Bool
isSaturated (SecurityParam k) db =
    maxRollback db >= k

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
     ( HasHeader blk, GetTip l, HeaderHash l ~ HeaderHash blk
     , StandardHash l
     )
  => Point blk
  -> LedgerSeq m l
  -> Maybe (l EmptyMK)
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
     ( StandardHash l
     , GetTip l
     )
  => Point l -> LedgerSeq m l -> Maybe (LedgerSeq m l)
rollbackToPoint pt (LedgerSeq ldb) = do
    LedgerSeq <$>
      AS.rollback
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
     GetTip l
  => LedgerSeq m l -> LedgerSeq m l
rollbackToAnchor (LedgerSeq vol) =
    LedgerSeq (AS.Empty (AS.anchor vol))

-- | Get a prefix of the LedgerDB that ends at the given point
--
--  \( O(\log(\min(i,n-i)) \)
--
-- When no ledger state (or anchor) has the given 'Point', 'Nothing' is
-- returned.
rollback ::
     ( HasHeader blk, GetTip l, HeaderHash l ~ HeaderHash blk
     , StandardHash l
     )
  => Point blk
  -> LedgerSeq m l
  -> Maybe (LedgerSeq m l)
rollback pt db
    | pt == castPoint (getTip (anchor db))
    = Just $ rollbackToAnchor db
    | otherwise
    = rollbackToPoint (castPoint pt) db

immutableTipSlot ::
     GetTip l
  => LedgerSeq m l -> WithOrigin SlotNo
immutableTipSlot =
      getTipSlot
    . state
    . AS.anchor
    . getLedgerSeq

-- | Transform the underlying volatile 'AnchoredSeq' using the given functions.
volatileStatesBimap ::
     AS.Anchorable (WithOrigin SlotNo) a b
  => (l EmptyMK -> a)
  -> (l EmptyMK -> b)
  -> LedgerSeq m l
  -> AS.AnchoredSeq (WithOrigin SlotNo) a b
volatileStatesBimap f g =
      AS.bimap (f . state) (g . state)
    . getLedgerSeq

{-------------------------------------------------------------------------------
  docspec setup
-------------------------------------------------------------------------------}

-- $setup
-- >>> :set -XTypeFamilies -XUndecidableInstances
-- >>> import qualified Ouroboros.Network.AnchoredSeq as AS
-- >>> import Ouroboros.Network.Block
-- >>> import Ouroboros.Network.Point
-- >>> import Ouroboros.Consensus.Ledger.Tables
-- >>> import Ouroboros.Consensus.Ledger.Basics
-- >>> import Ouroboros.Consensus.Config
-- >>> import Ouroboros.Consensus.Storage.LedgerDB.Common
-- >>> import Data.Void
-- >>> import Cardano.Slotting.Slot
-- >>> data B
-- >>> data LS (mk :: MapKind) = LS (Point LS)
-- >>> type instance HeaderHash LS = Int
-- >>> type instance HeaderHash B = HeaderHash LS
-- >>> instance StandardHash LS
-- >>> type instance Key LS = Void
-- >>> type instance Value LS = Void
-- >>> instance LedgerTablesAreTrivial LS where convertMapKind (LS p) = LS p
-- >>> instance HasLedgerTables LS
-- >>> s = [LS (Point Origin), LS (Point (At (Block 0 0))), LS (Point (At (Block 1 1))), LS (Point (At (Block 2 2))), LS (Point (At (Block 3 3)))]
-- >>> [l0s, l1s, l2s, l3s, l4s] = s
-- >>> [l0, l1, l2, l3, l4] <- mapM newLedgerHandles
-- >>> instance GetTip LS where getTip (LS p) = p
-- >>> instance Eq (LS EmptyMK) where LS p1 == LS p2 = p1 == p2
-- >>> instance StandardHash B
-- >>> instance HasHeader B where getHeaderFields = undefined
