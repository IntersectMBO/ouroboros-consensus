{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes  #-}

module Ouroboros.Consensus.Storage.LedgerDB.API (LedgerDB (..)) where

import           Control.Concurrent.Class.MonadSTM.Strict
import           Data.Set (Set)
import           Data.Word
import           NoThunks.Class
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.Config
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog.Update
import           Ouroboros.Consensus.Storage.LedgerDB.ReadsKeySets
import           Ouroboros.Consensus.Storage.LedgerDB.Update
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.IOLike (Time)

type LedgerDBView m b blk =
  StaticEither b
   (LedgerBackingStoreValueHandle' m blk, DbChangelog' blk)
   (Either
     (Point blk)
     (LedgerBackingStoreValueHandle' m blk, DbChangelog' blk))

data LedgerDB m blk = LedgerDB {
    -- | Set the current DbChangelog in the LedgerDB.
    setCurrent            :: DbChangelog' blk -> LedgerDBUpdate -> STM m ()
    -- | Get the current DbChangelog in the LedgerDB.
  , getCurrent            :: STM m (DbChangelog' blk)
    -- | Get the set of previously succesfully applied blocks.
  , getPrevApplied        :: STM m (Set (RealPoint blk))
    -- | Ask the backing store and DbChangelog to provide a table of values at
    -- the requested point.
  , getLedgerTablesAtFor  ::
         Point blk
      -> LedgerTables (ExtLedgerState blk) KeysMK
      -> m (Either
             (PointNotFound blk)
             (LedgerTables (ExtLedgerState blk) ValuesMK))
    -- | Acquire a ledger db read view at the requested point or at the tip. If
    -- the requested point doesn't exist it will return a @StaticRight (Left
    -- pt)@.
  , acquireLDBReadView   ::
         forall a b.
         StaticEither b () (Point blk)
      -> STM m a
         -- ^ STM operation that we want to run in the same atomic block as the
         -- acquisition of the LedgerDB
      -> m (a, LedgerDBView m b blk)
    -- | Apply a list of blocks on top of the given DbChangelog.
  , validate              ::
         LedgerBackingStoreValueHandle' m blk
      -> DbChangelog' blk
         -- ^ This is used as the starting point for validation, not the one
         -- in the 'LgrDB'.
      -> BlockCache blk
      -> Word64  -- ^ How many blocks to roll back
      -> (UpdateLedgerDbTraceEvent blk -> m ())
      -> [Header blk]
      -> m (ValidateResult blk)
    -- | Garbage collect references to old blocks that have been previously
    -- applied.
  , garbageCollect        :: SlotNo -> STM m ()
    -- | If the DbChangelog in the LedgerDB can flush (based on the DiskPolicy
    -- with which this LedgerDB was opened), flush differences to the backing
    -- store. Note this acquires a write lock on the backing store.
  , tryFlush              :: m ()

    -- | If the provided arguments indicate so (based on the DiskPolicy with
    -- which this LedgerDB was opened), take a snapshot and delete stale ones.
  , tryTakeSnapshot       ::
         Maybe (Time, Time)
         -- ^ If a snapshot has been taken already, the time at which it was
         -- taken and the current time.
      -> Word64
         -- ^ How many blocks have been processed since the last snapshot.
      -> m SnapCounters
  } deriving NoThunks via OnlyCheckWhnfNamed "LedgerDB" (LedgerDB m blk)
