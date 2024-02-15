{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Storage.LedgerDB.V1.Common (
    -- * LedgerDB internal state
    LedgerDBEnv (..)
  , LedgerDBHandle (..)
  , LedgerDBState (..)
  , getEnv
  , getEnv1
  , getEnv2
  , getEnv5
  , getEnvSTM
  , getEnvSTM1
    -- * Forkers
  , ForkerEnv (..)
  , ForkerKey (..)
  , getForkerEnv
  , getForkerEnv1
  , getForkerEnvSTM
    -- * LedgerDB lock
  , LedgerDBLock
  , mkLedgerDBLock
  , withReadLock
  , withWriteLock
    -- * Constraints
  , LedgerDbSerialiseConstraints
    -- * Exposed internals for testing purposes
  , Internals (..)
  , Internals'
  ) where

import           Codec.Serialise.Class
import           Control.Arrow
import           Control.Tracer
import           Data.Kind
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import           Data.Word
import           GHC.Generics
import           NoThunks.Class
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.LedgerDB.API as API
import           Ouroboros.Consensus.Storage.LedgerDB.API.DiskPolicy
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Validate
import           Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.V1.DbChangelog hiding
                     (ResolveBlock)
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Consensus.Util.MonadSTM.RAWLock as Lock
import           System.FS.API

{-------------------------------------------------------------------------------
  LedgerDB internal state
-------------------------------------------------------------------------------}

newtype LedgerDBHandle m l blk = LDBHandle (StrictTVar m (LedgerDBState m l blk))
  deriving Generic

data LedgerDBState m l blk =
    LedgerDBOpen !(LedgerDBEnv m l blk)
  | LedgerDBClosed
  deriving Generic

deriving instance ( IOLike m
                  , LedgerSupportsProtocol blk
                  , NoThunks (l EmptyMK)
                  , NoThunks (Key l)
                  , NoThunks (Value l)
                  ) => NoThunks (LedgerDBState m l blk)

data LedgerDBEnv m l blk = LedgerDBEnv {
    -- | INVARIANT: the tip of the 'LedgerDB' is always in sync with the tip of
    -- the current chain of the ChainDB.
    ldbChangelog      :: !(StrictTVar m (DbChangelog l))
    -- | Handle to the ledger's backing store, containing the parts that grow too
    -- big for in-memory residency
  , ldbBackingStore   :: !(LedgerBackingStore m l)
    -- | The flush lock to the 'BackingStore'. This lock is crucial when it
    -- comes to keeping the data in memory consistent with the data on-disk.
    --
    -- This lock should be held whenever we want to keep a consistent view of
    -- the backing store for some time. In particular we use this:
    --
    -- - when performing a query on the ledger state, we need to hold a
    --   'LocalStateQueryView' which, while live, must maintain a consistent view
    --   of the DB, and therefore we acquire a Read lock.
    --
    -- - when taking a snapshot of the ledger db, we need to prevent others
    --   from altering the backing store at the same time, thus we acquire a
    --   Write lock.
  , ldbLock           :: !(LedgerDBLock m)
    -- | INVARIANT: this set contains only points that are in the
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
  , ldbPrevApplied    :: !(StrictTVar m (Set (RealPoint blk)))
    -- | Open forkers.
    --
    -- INVARIANT: a forker is open iff its 'ForkerKey' is in this 'Map.
  , ldbForkers        :: !(StrictTVar m (Map ForkerKey (ForkerEnv m l blk)))
  , ldbNextForkerKey  :: !(StrictTVar m ForkerKey)

  , ldbDiskPolicy     :: !DiskPolicy
  , ldbTracer         :: !(Tracer m (TraceLedgerDBEvent blk))
  , ldbCfg            :: !(TopLevelConfig blk)
  , ldbHasFS          :: !(SomeHasFS m)
  , ldbShouldFlush    :: !(Word64 -> Bool)
  , ldbQueryBatchSize :: !QueryBatchSize
  , ldbResolveBlock   :: !(ResolveBlock m blk)
  , ldbSecParam       :: !SecurityParam
  , ldbBsTracer       :: !(Tracer m BackingStoreTraceByBackend)
  } deriving (Generic)

deriving instance ( IOLike m
                  , LedgerSupportsProtocol blk
                  , NoThunks (l EmptyMK)
                  , NoThunks (Key l)
                  , NoThunks (Value l)
                  ) => NoThunks (LedgerDBEnv m l blk)

-- | Check if the LedgerDB is open, if so, executing the given function on the
-- 'LedgerDBEnv', otherwise, throw a 'CloseDBError'.
getEnv ::
     forall m l blk r. (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> (LedgerDBEnv m l blk -> m r)
  -> m r
getEnv (LDBHandle varState) f = readTVarIO varState >>= \case
    LedgerDBOpen env -> f env
    LedgerDBClosed   -> throwIO $ ClosedDBError @blk prettyCallStack

-- | Variant 'of 'getEnv' for functions taking one argument.
getEnv1 ::
     (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> (LedgerDBEnv m l blk -> a -> m r)
  -> a -> m r
getEnv1 h f a = getEnv h (\env -> f env a)

-- | Variant 'of 'getEnv' for functions taking two arguments.
getEnv2 ::
     (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> (LedgerDBEnv m l blk -> a -> b -> m r)
  -> a -> b -> m r
getEnv2 h f a b = getEnv h (\env -> f env a b)

-- | Variant 'of 'getEnv' for functions taking five arguments.
getEnv5 ::
     (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> (LedgerDBEnv m l blk -> a -> b -> c -> d -> e -> m r)
  -> a -> b -> c -> d -> e -> m r
getEnv5 h f a b c d e = getEnv h (\env -> f env a b c d e)

-- | Variant of 'getEnv' that works in 'STM'.
getEnvSTM ::
     forall m l blk r. (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> (LedgerDBEnv m l blk -> STM m r)
  -> STM m r
getEnvSTM (LDBHandle varState) f = readTVar varState >>= \case
    LedgerDBOpen env -> f env
    LedgerDBClosed   -> throwSTM $ ClosedDBError @blk prettyCallStack

-- | Variant of 'getEnv1' that works in 'STM'.
getEnvSTM1 ::
     forall m l blk a r. (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> (LedgerDBEnv m l blk -> a -> STM m r)
  -> a -> STM m r
getEnvSTM1 (LDBHandle varState) f a = readTVar varState >>= \case
    LedgerDBOpen env -> f env a
    LedgerDBClosed   -> throwSTM $ ClosedDBError @blk prettyCallStack

{-------------------------------------------------------------------------------
  Forkers
-------------------------------------------------------------------------------}

-- | An identifier for a 'Forker'. See 'ldbForkers'.
newtype ForkerKey = ForkerKey Word16
  deriving stock (Show, Eq, Ord)
  deriving newtype (Enum, NoThunks)

data ForkerEnv m l blk = ForkerEnv {
    -- * Local, consistent view of ledger state
    foeBackingStoreValueHandle :: !(LedgerBackingStoreValueHandle m l)
  , foeChangelog               :: !(StrictTVar m (AnchorlessDbChangelog l))
    -- * Communication with the LedgerDB
    -- | Points to 'ldbChangelog'.
  , foeSwitchVar               :: !(StrictTVar m (DbChangelog l))
    -- * Config
  , foeSecurityParam           :: !SecurityParam
  , foeQueryBatchSize          :: !QueryBatchSize
  }
  deriving Generic

deriving instance ( IOLike m
                  , LedgerSupportsProtocol blk
                  , NoThunks (l EmptyMK)
                  , NoThunks (Key l)
                  , NoThunks (Value l)
                  ) => NoThunks (ForkerEnv m l blk)

getForkerEnv ::
     forall m l blk r. (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> ForkerKey
  -> (ForkerEnv m l blk -> m r)
  -> m r
getForkerEnv (LDBHandle varState) forkerKey f = do
    forkerEnv <- atomically $ readTVar varState >>= \case
      LedgerDBClosed   -> throwIO $ ClosedDBError @blk prettyCallStack
      LedgerDBOpen env -> readTVar (ldbForkers env) >>= (Map.lookup forkerKey >>> \case
        Nothing        -> throwSTM $ ClosedForkerError @blk prettyCallStack
        Just forkerEnv -> pure forkerEnv)

    f forkerEnv

getForkerEnv1 ::
     (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> ForkerKey
  -> (ForkerEnv m l blk -> a -> m r)
  -> a -> m r
getForkerEnv1 h forkerKey f a = getForkerEnv h forkerKey (`f` a)

getForkerEnvSTM ::
     forall m l blk r. (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> ForkerKey
  -> (ForkerEnv m l blk -> STM m r)
  -> STM m r
getForkerEnvSTM (LDBHandle varState) forkerKey f = readTVar varState >>= \case
    LedgerDBClosed   -> throwIO $ ClosedDBError @blk prettyCallStack
    LedgerDBOpen env -> readTVar (ldbForkers env) >>= (Map.lookup forkerKey >>> \case
      Nothing        -> throwSTM $ ClosedForkerError @blk prettyCallStack
      Just forkerEnv -> f forkerEnv)

{-------------------------------------------------------------------------------
  LedgerDB lock
-------------------------------------------------------------------------------}

-- | A lock to prevent the LedgerDB (i.e. a 'DbChangelog') from getting out of
-- sync with the 'BackingStore'.
--
-- We rely on the capability of the @BackingStore@s of providing
-- 'BackingStoreValueHandles' that can be used to hold a persistent view of the
-- database as long as the handle is open. Assuming this functionality, the lock
-- is used in three ways:
--
-- - Read lock to acquire a value handle: we do this when acquiring a view of the
--   'LedgerDB' (which lives in a 'StrictTVar' at the 'ChainDB' level) and of
--   the 'BackingStore'. We momentarily acquire a read lock, consult the
--   transactional variable and also open a 'BackingStoreValueHandle'. This is
--   the case for ledger state queries and for the forging loop.
--
-- - Read lock to ensure two operations are in sync: in the above situation, we
--   relied on the 'BackingStoreValueHandle' functionality, but sometimes we
--   won't access the values through a value handle, and instead we might use
--   the LMDB environment (as it is the case for 'lmdbCopy'). In these cases, we
--   acquire a read lock until we ended the copy, so that writers are blocked
--   until this process is completed. This is the case when taking a snapshot.
--
-- - Write lock when flushing differences.
newtype LedgerDBLock m = LedgerDBLock (Lock.RAWLock m ())
  deriving newtype NoThunks

mkLedgerDBLock :: IOLike m => m (LedgerDBLock m)
mkLedgerDBLock = LedgerDBLock <$> Lock.new ()

-- | Acquire the ledger DB read lock and hold it while performing an action
withReadLock :: IOLike m => LedgerDBLock m -> m a -> m a
withReadLock (LedgerDBLock lock) m =
    Lock.withReadAccess lock (\() -> m)

-- | Acquire the ledger DB write lock and hold it while performing an action
withWriteLock :: IOLike m => LedgerDBLock m -> m a -> m a
withWriteLock (LedgerDBLock lock) m =
    Lock.withWriteAccess lock (\() -> (,) () <$> m)

{-------------------------------------------------------------------------------
  Constraints
-------------------------------------------------------------------------------}

-- | Serialization constraints required by the 'LedgerDB' to be properly
-- instantiated with a @blk@.
type LedgerDbSerialiseConstraints blk =
  ( Serialise      (HeaderHash  blk)
  , EncodeDisk blk (LedgerState blk EmptyMK)
  , DecodeDisk blk (LedgerState blk EmptyMK)
  , EncodeDisk blk (AnnTip      blk)
  , DecodeDisk blk (AnnTip      blk)
  , EncodeDisk blk (ChainDepState (BlockProtocol blk))
  , DecodeDisk blk (ChainDepState (BlockProtocol blk))
  , CanSerializeLedgerTables (LedgerState blk)
  )

{-------------------------------------------------------------------------------
  Exposed internals for testing purposes
-------------------------------------------------------------------------------}

type Internals' m blk = Internals m (ExtLedgerState blk) blk

-- TODO: fill in as required
type Internals :: (Type -> Type) -> LedgerStateKind -> Type -> Type
data Internals m l blk = Internals {
    intTakeSnapshot         :: (l ~ ExtLedgerState blk) => DiskSnapshot -> m ()
    -- | Reapplies a block to the tip of the LedgerDB, and adds the result as
    -- the new tip of the LedgerDB.
  , intReapplyThenPushBlock :: (l ~ ExtLedgerState blk) => blk -> m ()
  }
