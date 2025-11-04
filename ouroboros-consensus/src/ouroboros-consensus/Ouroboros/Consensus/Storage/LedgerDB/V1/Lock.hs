{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Storage.LedgerDB.V1.Lock
  ( -- * LedgerDB lock
    LedgerDBLock
  , ReadLocked (..)
  , WriteLocked
  , mkLedgerDBLock
  , readLocked
  , unsafeIgnoreWriteLock
  , withReadLock
  , withWriteLock
  , writeLocked
  , unsafeAcquireReadAccess
  , unsafeReleaseReadAccess
  ) where

import Control.Monad.Class.MonadTime
import qualified Control.RAWLock as Lock
import NoThunks.Class
import Ouroboros.Consensus.Util.IOLike

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

deriving newtype instance NoThunks (Lock.RAWLock m ()) => NoThunks (LedgerDBLock m)

mkLedgerDBLock :: IOLike m => m (LedgerDBLock m)
mkLedgerDBLock = LedgerDBLock <$> Lock.new ()

-- | An action in @m@ that has to hold the read lock. See @withReadLock@.
newtype ReadLocked m a = ReadLocked {unsafeRunReadLocked :: m a}
  deriving newtype (Functor, Applicative, Monad)

-- | Enforce that the action has to be run while holding the read lock.
readLocked :: m a -> ReadLocked m a
readLocked = ReadLocked

-- | Acquire the ledger DB read lock and hold it while performing an action
withReadLock :: IOLike m => LedgerDBLock m -> ReadLocked m a -> m a
withReadLock (LedgerDBLock lock) m =
  Lock.withReadAccess lock (\() -> unsafeRunReadLocked m)

-- | An action in @m@ that has to hold the write lock. See @withWriteLock@.
newtype WriteLocked m a = WriteLocked {runWriteLocked :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadMonotonicTime, MonadMonotonicTimeNSec)

-- | Used safely, for example, during initialization.
unsafeIgnoreWriteLock :: WriteLocked m a -> m a
unsafeIgnoreWriteLock = runWriteLocked

-- | Enforce that the action has to be run while holding the write lock.
writeLocked :: m a -> WriteLocked m a
writeLocked = WriteLocked

-- | Acquire the ledger DB write lock and hold it while performing an action
withWriteLock :: IOLike m => LedgerDBLock m -> WriteLocked m a -> m a
withWriteLock (LedgerDBLock lock) m =
  Lock.withWriteAccess lock (\() -> (,()) <$> runWriteLocked m)

unsafeAcquireReadAccess :: IOLike m => LedgerDBLock m -> STM m ()
unsafeAcquireReadAccess (LedgerDBLock lock) = Lock.unsafeAcquireReadAccess lock

unsafeReleaseReadAccess :: IOLike m => LedgerDBLock m -> STM m ()
unsafeReleaseReadAccess (LedgerDBLock lock) = Lock.unsafeReleaseReadAccess lock
