{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

-- This module exists to avoid a cyclic dependency, but it is morally related to
-- 'Ouroboros.Consensus.Storage.LedgerDB.Impl'.
module Ouroboros.Consensus.Storage.LedgerDB.Types (
    LedgerDBHandle (..)
  , LedgerDBState (..)
  , LedgerDBStateEnv (..)
    -- * Accessors
  , getState
  , getState1
  , getState2
  , getStateSTM
  , getStateSTM1
    -- * Trace
  , BackingStoreTrace (..)
  , TraceBackingStoreInitEvent (..)
  , TraceLedgerDBEvent (..)
  ) where

import           Data.Set (Set)
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore hiding
                     (BackingStoreTrace)
import qualified Ouroboros.Consensus.Storage.LedgerDB.BackingStore as BS
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore.LMDB
                     (LMDBLimits)
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog
import           Ouroboros.Consensus.Storage.LedgerDB.Lock
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import           Ouroboros.Consensus.Util.IOLike
import           Util.CallStack

data LedgerDBState m blk = LedgerDBState {
   ldbChangelog    :: !(StrictTVar m (DbChangelog' blk))
   -- ^ INVARIANT: the tip of the 'LedgerDB' is always in sync with the tip of
   -- the current chain of the ChainDB.
 , ldbBackingStore :: !(LedgerBackingStore' m blk)
   -- ^ Handle to the ledger's backing store, containing the parts that grow too
   -- big for in-memory residency
 , ldbLock         :: !(LedgerDBLock m)
   -- ^ The flush lock to the 'BackingStore'. This lock is crucial when it
   -- comes to keeping the data in memory consistent with the data on-disk.
   --
   -- This lock should be held whenever we want to keep a consistent view of
   -- the backing store for some time. In particular we use this:
   --
   -- - when performing a query on the ledger state, we need to hold a
   --   'DiskLedgerView' which, while live, must maintain a consistent view
   --   of the DB, and therefore we acquire a Read lock.
   --
   -- - when taking a snapshot of the ledger db, we need to prevent others
   --   from altering the backing store at the same time, thus we acquire a
   --   Write lock.
 , varPrevApplied  :: !(StrictTVar m (Set (RealPoint blk)))
   -- ^ INVARIANT: this set contains only points that are in the
   -- VolatileDB.
   --
   -- INVARIANT: all points on the current chain fragment are in this set.
   --
   -- The VolatileDB might contain invalid blocks, these will not be in
   -- this set.
   --
   -- When a garbage-collection is performed on the VolatileDB, the points
   -- of the blocks eligible for garbage-collection should be removed from
   -- this set.
 } deriving (Generic)

deriving instance (IOLike m, LedgerSupportsProtocol blk)
               => NoThunks (LedgerDBState m blk)

{-------------------------------------------------------------------------------
  Handle
-------------------------------------------------------------------------------}

-- | Database error
--
-- Thrown upon incorrect use: invalid input.
newtype LedgerDbError blk =
    -- | The ChainDB is closed.
    --
    -- This will be thrown when performing any operation on the ChainDB except
    -- for 'isOpen' and 'closeDB'. The 'CallStack' of the operation on the
    -- ChainDB is included in the error.
    ClosedDBError PrettyCallStack
    deriving (Show, Exception)

newtype LedgerDBHandle m blk = LDBHandle (StrictTVar m (LedgerDBStateEnv m blk))

getState :: forall m blk r. (IOLike m, HasCallStack, HasHeader blk)
       => LedgerDBHandle m blk
       -> (LedgerDBState m blk -> m r)
       -> m r
getState (LDBHandle varState) f = atomically (readTVar varState) >>= \case
    LedgerDBOpen env -> f env
    LedgerDBClosed   -> throwIO $ ClosedDBError @blk prettyCallStack

-- | Variant 'of 'getState' for functions taking one argument.
getState1 :: (IOLike m, HasCallStack, HasHeader blk)
        => LedgerDBHandle m blk
        -> (LedgerDBState m blk -> a -> m r)
        -> a -> m r
getState1 h f a = getState h (`f` a)

-- | Variant 'of 'getState' for functions taking two arguments.
getState2 :: (IOLike m, HasCallStack, HasHeader blk)
        => LedgerDBHandle m blk
        -> (LedgerDBState m blk -> a -> b -> m r)
        -> a -> b -> m r
getState2 h f a b = getState h (\env -> f env a b)

-- | Variant of 'getState' that works in 'STM'.
getStateSTM :: forall m blk r. (IOLike m, HasCallStack, HasHeader blk)
          => LedgerDBHandle m blk
          -> (LedgerDBState m blk -> STM m r)
          -> STM m r
getStateSTM (LDBHandle varState) f = readTVar varState >>= \case
    LedgerDBOpen env -> f env
    LedgerDBClosed   -> throwSTM $ ClosedDBError @blk prettyCallStack

-- | Variant of 'getState1' that works in 'STM'.
getStateSTM1 ::
     forall m blk a r. (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m blk
  -> (LedgerDBState m blk -> a -> STM m r)
  -> a -> STM m r
getStateSTM1 (LDBHandle varState) f a = readTVar varState >>= \case
    LedgerDBOpen env -> f env a
    LedgerDBClosed   -> throwSTM $ ClosedDBError @blk prettyCallStack

data LedgerDBStateEnv m blk
  = LedgerDBOpen   !(LedgerDBState m blk)
  | LedgerDBClosed
  deriving (Generic, NoThunks)

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

data TraceLedgerDBEvent blk =
    LedgerDBSnapshotEvent (TraceSnapshotEvent blk)
  | BackingStoreEvent BackingStoreTrace
  | BackingStoreInitEvent TraceBackingStoreInitEvent
  deriving (Show, Eq, Generic)

-- | A trace event for the backing store that we have initialised.
data TraceBackingStoreInitEvent =
    BackingStoreInitialisedLMDB LMDBLimits
  | BackingStoreInitialisedInMemory
  deriving (Show, Eq)

-- | A tracing datatype that is the sum of the traces of the backing store
-- implementations
data BackingStoreTrace = LMDBTrace BS.BackingStoreTrace
                       | InMemoryTrace BS.BackingStoreTrace
                       deriving (Eq, Show)
