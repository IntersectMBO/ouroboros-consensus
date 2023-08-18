{-# LANGUAGE CPP            #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

module Ouroboros.Consensus.Storage.LedgerDB.API (
    LedgerDB (..)
  , LedgerDBView (..)
  , LedgerDBView'
  , closeLedgerDBView
  ) where

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

data LedgerDBView m l = LedgerDBView {
    viewHandle         :: !(LedgerBackingStoreValueHandle m l)
  , viewChangelog      :: !(AnchorlessDbChangelog l)
    -- | See 'onDiskQueryBatchSize'.
  , viewQueryBatchSize :: !Word64
  }

closeLedgerDBView :: LedgerDBView m l -> m ()
closeLedgerDBView LedgerDBView {viewHandle} = bsvhClose viewHandle

type LedgerDBView' m blk = LedgerDBView m (ExtLedgerState blk)

type View m b blk =
  StaticEither b
   (LedgerDBView' m blk)
   (Either
     (Point blk)
     (LedgerDBView' m blk))

{-------------------------------------------------------------------------------
  The LedgerDB API
-------------------------------------------------------------------------------}

data LedgerDB m blk = LedgerDB {
    -- | Set the current DbChangelog in the LedgerDB.
    setCurrent            :: AnchorlessDbChangelog' blk -> STM m ()
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
#if __GLASGOW_HASKELL__ >= 902
         -- ^ STM operation that we want to run in the same atomic block as the
         -- acquisition of the LedgerDB
#endif
      -> m (a, View m b blk)
    -- | Apply a list of blocks on top of the given DbChangelog.
  , validate              ::
         LedgerBackingStoreValueHandle' m blk
      -> AnchorlessDbChangelog' blk
#if __GLASGOW_HASKELL__ >= 902
         -- ^ This is used as the starting point for validation, not the one
         -- in the 'LgrDB'.
#endif
      -> BlockCache blk
      -> Word64
#if __GLASGOW_HASKELL__ >= 902
         -- ^ How many blocks to roll back
#endif
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
#if __GLASGOW_HASKELL__ >= 902
         -- ^ If a snapshot has been taken already, the time at which it was
         -- taken and the current time.
#endif
      -> Word64
#if __GLASGOW_HASKELL__ >= 902
         -- ^ How many blocks have been processed since the last snapshot.
#endif
      -> m SnapCounters
  } deriving NoThunks via OnlyCheckWhnfNamed "LedgerDB" (LedgerDB m blk)
