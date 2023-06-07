{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}

-- | A store for key-value maps that can be extended with deltas.
--
-- When we talk about deltas we mean differences on key-value entries, for
-- example a deletion of a key-value entry or an insertion.
--
-- Its intended use is for storing data structures from the 'LedgerState' and
-- update them with differences produced by executing the Ledger rules.
module Ouroboros.Consensus.Storage.LedgerDB.BackingStore (
    -- * Backing store interface
    BackingStore (..)
  , BackingStorePath (..)
  , BackingStoreValueHandle (..)
  , InitFrom (..)
  , RangeQuery (..)
  , bsRead
  , withBsValueHandle
    -- * Ledger DB wrappers
  , LedgerBackingStore (..)
  , LedgerBackingStore'
  , LedgerBackingStoreValueHandle (..)
  , LedgerBackingStoreValueHandle'
  , castBackingStoreValueHandle
  , castLedgerBackingStoreValueHandle
  , lbsValueHandle
  , lbsvhClose
  , lbsvhRead
  ) where

import           Cardano.Slotting.Slot (SlotNo, WithOrigin (..))
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (OnlyCheckWhnfNamed (..))
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Util.IOLike
import qualified System.FS.API as FS
import qualified System.FS.API.Types as FS

{-------------------------------------------------------------------------------
  Backing store interface
-------------------------------------------------------------------------------}

-- | A backing store for a map
data BackingStore m keys values diff = BackingStore {
    -- | Close the backing store
    --
    -- Other methods throw exceptions if called on a closed store.
    bsClose       :: !(m ())
    -- | Create a persistent copy
    --
    -- Each backing store implementation will offer a way to initialize itself
    -- from such a path.
    --
    -- The destination path must not already exist. After this operation, it
    -- will be a directory.
  , bsCopy        :: !(FS.SomeHasFS m -> BackingStorePath -> m ())
    -- | Open a 'BackingStoreValueHandle' capturing the current value of the
    -- entire database
  , bsValueHandle :: !(HasCallStack => m (WithOrigin SlotNo, BackingStoreValueHandle m keys values))
    -- | Apply a valid diff to the contents of the backing store
  , bsWrite       :: !(SlotNo -> diff -> m ())
  }

deriving via OnlyCheckWhnfNamed "BackingStore" (BackingStore m keys values diff)
  instance NoThunks (BackingStore m keys values diff)

data InitFrom values =
    InitFromValues !(WithOrigin SlotNo) !values
  | InitFromCopy !BackingStorePath

newtype BackingStorePath = BackingStorePath FS.FsPath
  deriving stock (Show, Eq, Ord)
  deriving newtype NoThunks

-- | An ephemeral handle to an immutable value of the entire database
--
-- The performance cost is usually minimal unless this handle is held open too
-- long. We expect clients of the BackingStore to not retain handles for a long
-- time.
data BackingStoreValueHandle m keys values = BackingStoreValueHandle {
    -- | Close the handle
    --
    -- Other methods throw exceptions if called on a closed handle.
    bsvhClose     :: !(m ())
    -- | See 'RangeQuery'
  , bsvhRangeRead :: !(RangeQuery keys -> m values)
    -- | Read the given keys from the handle
    --
    -- Absent keys will merely not be present in the result instead of causing a
    -- failure or an exception.
  , bsvhRead      :: !(keys -> m values)
  }

castBackingStoreValueHandle ::
     Functor m
  => (values -> values')
  -> (keys' -> keys)
  -> BackingStoreValueHandle m keys values
  -> BackingStoreValueHandle m keys' values'
castBackingStoreValueHandle f g bsvh =
  BackingStoreValueHandle {
    bsvhClose
    , bsvhRangeRead = \(RangeQuery prev count) ->
        fmap f . bsvhRangeRead $  RangeQuery (fmap g prev) count
    , bsvhRead = fmap f . bsvhRead . g
    }
  where
    BackingStoreValueHandle {
        bsvhClose
      , bsvhRangeRead
      , bsvhRead
      } = bsvh

data RangeQuery keys = RangeQuery {
      -- | The result of this range query begin at first key that is strictly
      -- greater than the greatest key in 'rqPrev'.
      --
      -- If the given set of keys is 'Just' but contains no keys, then the query
      -- will return no results. (This is the steady-state once a looping range
      -- query reaches the end of the table.)
      rqPrev  :: Maybe keys
      -- | Roughly how many values to read.
      --
      -- The query may return a different number of values than this even if it
      -- has not reached the last key. The only crucial invariant is that the
      -- query only returns an empty map if there are no more keys to read on
      -- disk.
      --
      -- FIXME: #4398 can we satisfy this invariant if we read keys from disk
      -- but all of them were deleted in the changelog?
    , rqCount :: !Int
    }
    deriving stock (Show, Eq)

deriving via OnlyCheckWhnfNamed "BackingStoreValueHandle" (BackingStoreValueHandle m keys values)
  instance NoThunks (BackingStoreValueHandle m keys values)

-- | A combination of 'bsValueHandle' and 'bsvhRead'
bsRead ::
     MonadThrow m
  => BackingStore m keys values diff
  -> keys
  -> m (WithOrigin SlotNo, values)
bsRead store keys = withBsValueHandle store $ \slot vh -> do
    values <- bsvhRead vh keys
    pure (slot, values)

-- | A 'IOLike.bracket'ed 'bsValueHandle'
withBsValueHandle ::
     MonadThrow m
  => BackingStore m keys values diff
  -> (WithOrigin SlotNo -> BackingStoreValueHandle m keys values -> m a)
  -> m a
withBsValueHandle store kont =
    bracket
      (bsValueHandle store)
      (bsvhClose . snd)
      (uncurry kont)

{-------------------------------------------------------------------------------
  Ledger DB wrappers
-------------------------------------------------------------------------------}

-- | A handle to the backing store for the ledger tables
newtype LedgerBackingStore m l = LedgerBackingStore
    (BackingStore m
      (LedgerTables l KeysMK)
      (LedgerTables l ValuesMK)
      (LedgerTables l DiffMK)
    )
  deriving newtype (NoThunks)

lbsValueHandle ::
     (HasCallStack, IOLike m)
  => LedgerBackingStore m l
  -> m (LedgerBackingStoreValueHandle m l)
lbsValueHandle (LedgerBackingStore bstore) =
  uncurry LedgerBackingStoreValueHandle <$> bsValueHandle bstore

-- | A handle to the backing store for the ledger tables
data LedgerBackingStoreValueHandle m l = LedgerBackingStoreValueHandle
    !(WithOrigin SlotNo)
    !(BackingStoreValueHandle m
      (LedgerTables l KeysMK)
      (LedgerTables l ValuesMK)
    )
  deriving stock    (Generic)
  deriving anyclass (NoThunks)

type LedgerBackingStoreValueHandle' m blk =
     LedgerBackingStoreValueHandle  m (ExtLedgerState blk)

lbsvhClose :: LedgerBackingStoreValueHandle m l -> m ()
lbsvhClose (LedgerBackingStoreValueHandle _ vh) = bsvhClose vh

lbsvhRead :: Functor m
          => LedgerBackingStoreValueHandle m l
          -> LedgerTables l KeysMK
          -> m (WithOrigin SlotNo, LedgerTables l ValuesMK)
lbsvhRead (LedgerBackingStoreValueHandle s vh) = fmap (s,) . bsvhRead vh

castLedgerBackingStoreValueHandle ::
     Functor m
  => (LedgerTables l ValuesMK -> LedgerTables l' ValuesMK)
  -> (LedgerTables l' KeysMK -> LedgerTables l KeysMK)
  -> LedgerBackingStoreValueHandle m l
  -> LedgerBackingStoreValueHandle m l'
castLedgerBackingStoreValueHandle f g lbsvh =
    LedgerBackingStoreValueHandle s $ castBackingStoreValueHandle f g bsvh
  where
    LedgerBackingStoreValueHandle s bsvh = lbsvh

type LedgerBackingStore' m blk = LedgerBackingStore m (ExtLedgerState blk)
