{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The 'BackingStore' is the component of the
-- 'Ouroboros.Consensus.Storage.LedgerDB.LedgerDB' implementation that stores a
-- key-value map with the 'LedgerTable's at a specific slot on the chain.
--
-- It is used for storing 'Ouroboros.Consensus.Ledger.Basics.LedgerState' data
-- structures, and updating them with t'Data.Map.Diff.Strict.Diff's produced by
-- executing the Ledger rules.
--
-- See "Ouroboros.Consensus.Storage.LedgerDB.BackingStore" for the
-- implementations provided.
module Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.API (
    -- * FileSystem newtypes
    LiveLMDBFS (..)
  , SnapshotsFS (..)
    -- * Backing store
  , BackingStore (..)
  , BackingStore'
  , DiffsToFlush (..)
  , InitFrom (..)
  , LedgerBackingStore
    -- * Value handle
  , BackingStoreValueHandle (..)
  , BackingStoreValueHandle'
  , LedgerBackingStoreValueHandle
  , castBackingStoreValueHandle
  , withBsValueHandle
    -- * Query
  , RangeQuery (..)
    -- * Statistics
  , Statistics (..)
    -- * Tracing
  , BackingStoreTrace (..)
  , BackingStoreValueHandleTrace (..)
    -- * 🧪 Testing
  , bsRead
  ) where

import           Cardano.Slotting.Slot (SlotNo, WithOrigin (..))
import           GHC.Generics
import           NoThunks.Class (OnlyCheckWhnfNamed (..))
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Util.IOLike
import           System.FS.API
import qualified System.FS.API.Types as FS

-- | The LedgerDB file system. Typically pointing to @<db-path\/vol-db-path>/ledger@.
newtype SnapshotsFS m = SnapshotsFS { snapshotsFs :: SomeHasFS m }
  deriving (Generic, NoThunks)

-- | The LMDB file system. Typically pointing to @<db-path\/vol-db-path>/lmdb@.
newtype LiveLMDBFS m = LiveLMDBFS { liveLMDBFs :: SomeHasFS m }
  deriving (Generic, NoThunks)

{-------------------------------------------------------------------------------
  Backing store interface
-------------------------------------------------------------------------------}

-- | A container for differences that are inteded to be flushed to a
-- 'BackingStore'
data DiffsToFlush l = DiffsToFlush {
    -- | The set of differences that should be flushed into the 'BackingStore'
    toFlushDiffs :: !(LedgerTables l DiffMK)
    -- | At which slot the diffs were split. This must be the slot of the state
    -- considered as "last flushed" in the kept 'DbChangelog'
  , toFlushSlot  :: !SlotNo
  }

data BackingStore m keys values diff = BackingStore {
    -- | Close the backing store
    --
    -- Other methods throw exceptions if called on a closed store. 'bsClose'
    -- itself is idempotent.
    bsClose       :: !(m ())
    -- | Create a persistent copy
    --
    -- Each backing store implementation will offer a way to initialize itself
    -- from such a path.
    --
    -- The destination path must not already exist. After this operation, it
    -- will be a directory.
  , bsCopy        :: !(FS.FsPath -> m ())
    -- | Open a 'BackingStoreValueHandle' capturing the current value of the
    -- entire database
  , bsValueHandle :: !(m (BackingStoreValueHandle m keys values))
    -- | Apply a valid diff to the contents of the backing store
  , bsWrite       :: !(SlotNo -> diff -> m ())
  }

deriving via OnlyCheckWhnfNamed "BackingStore" (BackingStore m keys values diff)
  instance NoThunks (BackingStore m keys values diff)

type LedgerBackingStore m l =
  BackingStore m
    (LedgerTables l KeysMK)
    (LedgerTables l ValuesMK)
    (LedgerTables l DiffMK)

type BackingStore' m blk = LedgerBackingStore m (ExtLedgerState blk)

-- | Choose how to initialize the backing store
data InitFrom values =
    -- | Initialize from a set of values, at the given slot.
    InitFromValues !(WithOrigin SlotNo) !values
    -- | Use a snapshot at the given path to overwrite the set of values in the
    -- opened database.
  | InitFromCopy !FS.FsPath

{-------------------------------------------------------------------------------
  Value handles
-------------------------------------------------------------------------------}

-- | An ephemeral handle to an immutable value of the entire database
--
-- The performance cost is usually minimal unless this handle is held open too
-- long. We expect clients of the 'BackingStore' to not retain handles for a
-- long time.
data BackingStoreValueHandle m keys values = BackingStoreValueHandle {
    -- | At which slot this handle was created
    bsvhAtSlot    :: !(WithOrigin SlotNo)
    -- | Close the handle
    --
    -- Other methods throw exceptions if called on a closed handle. 'bsvhClose'
    -- itself is idempotent.
  , bsvhClose     :: !(m ())
    -- | See 'RangeQuery'
  , bsvhRangeRead :: !(RangeQuery keys -> m values)
    -- | Read the given keys from the handle
    --
    -- Absent keys will merely not be present in the result instead of causing a
    -- failure or an exception.
  , bsvhRead      :: !(keys -> m values)
    -- | Retrieve statistics
  , bsvhStat      :: !(m Statistics)
  }

deriving via OnlyCheckWhnfNamed "BackingStoreValueHandle" (BackingStoreValueHandle m keys values)
  instance NoThunks (BackingStoreValueHandle m keys values)

type LedgerBackingStoreValueHandle m l =
  BackingStoreValueHandle m
    (LedgerTables l KeysMK)
    (LedgerTables l ValuesMK)

type BackingStoreValueHandle' m blk = LedgerBackingStoreValueHandle m (ExtLedgerState blk)

castBackingStoreValueHandle ::
     Functor m
  => (values -> values')
  -> (keys' -> keys)
  -> BackingStoreValueHandle m keys values
  -> BackingStoreValueHandle m keys' values'
castBackingStoreValueHandle f g bsvh =
  BackingStoreValueHandle {
      bsvhAtSlot
    , bsvhClose
    , bsvhRangeRead = \(RangeQuery prev count) ->
        fmap f . bsvhRangeRead $  RangeQuery (fmap g prev) count
    , bsvhRead = fmap f . bsvhRead . g
    , bsvhStat
    }
  where
    BackingStoreValueHandle {
        bsvhClose
      , bsvhAtSlot
      , bsvhRangeRead
      , bsvhRead
      , bsvhStat
      } = bsvh

-- | A combination of 'bsValueHandle' and 'bsvhRead'
bsRead ::
     MonadThrow m
  => BackingStore m keys values diff
  -> keys
  -> m (WithOrigin SlotNo, values)
bsRead store keys = withBsValueHandle store $ \vh -> do
    values <- bsvhRead vh keys
    pure (bsvhAtSlot vh, values)

-- | A 'IOLike.bracket'ed 'bsValueHandle'
withBsValueHandle ::
     MonadThrow m
  => BackingStore m keys values diff
  -> (BackingStoreValueHandle m keys values -> m a)
  -> m a
withBsValueHandle store =
    bracket
      (bsValueHandle store)
      bsvhClose

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | The arguments for a query to the backing store, it is up to the particular
-- function that is performing the query to construct a value of this type, run
-- the query and, if appropriate, repeat this process to do a subsequent query.
data RangeQuery keys = RangeQuery {
      -- | The result of this range query begin at first key that is strictly
      -- greater than the greatest key in 'rqPrev'.
      --
      -- If the given set of keys is 'Just' but contains no keys, then the query
      -- will return no results. (This is the steady-state once a looping range
      -- query reaches the end of the table.)
      rqPrev  :: !(Maybe keys)
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

{-------------------------------------------------------------------------------
  Statistics
-------------------------------------------------------------------------------}

-- | Statistics for a key-value store.
--
-- Using 'bsvhStat' on a value handle only provides statistics for the on-disk
-- state of a key-value store. Combine this with information from a
-- 'DbChangelog' to obtain statistics about a "logical" state of the key-value
-- store. See 'getStatistics'.
data Statistics = Statistics {
    -- | The last slot number for which key-value pairs were stored.
    --
    -- INVARIANT: the 'sequenceNumber' returned by using 'bsvhStat' on a value
    -- handle should match 'bsvhAtSlot' for that same value handle.
    sequenceNumber :: !(WithOrigin SlotNo)
    -- | The total number of key-value pair entries that are stored.
  , numEntries     :: !Int
  }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

data BackingStoreTrace =
    BSOpening
  | BSOpened                 !(Maybe FS.FsPath)
  | BSInitialisingFromCopy   !FS.FsPath
  | BSInitialisedFromCopy    !FS.FsPath
  | BSInitialisingFromValues !(WithOrigin SlotNo)
  | BSInitialisedFromValues  !(WithOrigin SlotNo)
  | BSClosing
  | BSAlreadyClosed
  | BSClosed
  | BSCopying                !FS.FsPath
  | BSCopied                 !FS.FsPath
  | BSCreatingValueHandle
  | BSValueHandleTrace       !(Maybe Int) !BackingStoreValueHandleTrace
  | BSCreatedValueHandle
  | BSWriting                !SlotNo
  | BSWritten                !(WithOrigin SlotNo) !SlotNo
  deriving (Eq, Show)

data BackingStoreValueHandleTrace =
    BSVHClosing
  | BSVHAlreadyClosed
  | BSVHClosed
  | BSVHRangeReading
  | BSVHRangeRead
  | BSVHReading
  | BSVHRead
  | BSVHStatting
  | BSVHStatted
  deriving (Eq, Show)
