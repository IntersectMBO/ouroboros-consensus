{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A Read-Append-Write (RAW) lock
--
-- A RAW lock allows __multiple concurrent readers__, at most __one appender__,
-- which is allowed to run concurrently with the readers, and at most
-- __one writer__, which has exclusive access to the lock.
--
-- The following table summarises which roles are allowed to concurrently
-- access the RAW lock:
--
-- +----------+--------+----------+--------+
-- |          | Reader | Appender | Writer |
-- +==========+========+==========+========+
-- | Reader   |   V    |     V    |    X   |
-- +----------+--------+----------+--------+
-- | Appender |░░░░░░░░|     X    |    X   |
-- +----------+--------+----------+--------+
-- | Writer   |░░░░░░░░|░░░░░░░░░░|    X   |
-- +----------+--------+----------+--------+
--
-- It is important to realise that a RAW lock is intended to control access to
-- a piece of in-memory state that should remain in sync with some other state
-- that can only be modified using side-effects, e.g., the file system. If,
-- for example, you're only maintaining a counter shared by threads, then
-- simply use a 'TVar' or an 'MVar'.
--
-- = Example use case: log files
--
-- A RAW lock is useful, for example, to maintain an in-memory index of log
-- files stored on disk.
--
-- * To read data from a log file, you need \"read\" access to the index to
--   find out the file and offset where the requested piece of data is stored.
--   While holding the RAW lock as a reader, you can perform the IO operation
--   to read the data from the right log file. This can safely happen
--   concurrently with other read operations.
--
-- * To append data to the current log file, you need \"append\" access to the
--   index so you can append an entry to the index and even to add a new log
--   file to the index when necessary. While holding the RAW lock as an
--   appender, you can perform the IO operation to append the piece of data to
--   the current log file and, if necessary start a new log file. Only one
--   append can happen concurrently. However, reads can safely happen
--   concurrently with appends. Note that the in-memory index is only updated
--   /after/ writing to disk.
--
-- * To remove the oldest log files, you need \"write\" access to the index,
--   so you can remove files from the index. While holding the RAW lock as a
--   writer, you can perform the IO operations to delete the oldest log files.
--   No other operations can run concurrently with this operation: concurrent
--   reads might try to read from deleted files and a concurrent append could
--   try to append to a deleted file.
--
-- = Analogy: Chicken coop
--
-- Think of readers as chickens, the appender as the rooster, and the writer
-- as the fox. All of them want access to the chicken coop, i.e., the state
-- protected by the RAW lock.
--
-- We can allow multiple chickens (readers) together in the chicken coop, they
-- get along (reasonably) fine. We can also let one rooster (appender) in, but
-- not more than one, otherwise he would start fighting with the other rooster
-- (conflict with the other appender). We can only let the fox in when all
-- chickens and the rooster (if present) have left the chicken coop, otherwise
-- the fox would eat them (conflict with the appender and invalidate the
-- results of readers, e.g, closing resources readers try to access).
--
-- = Usage
--
-- To use the lock, use any of the three following operations:
--
-- * 'withReadAccess'
-- * 'withAppendAccess'
-- * 'withWriteAccess'
--
-- If the standard bracketing the above three operations use doesn't suffice,
-- use the following three acquire-release pairs:
--
-- * 'unsafeAcquireReadAccess'   & 'unsafeReleaseReadAccess'
-- * 'unsafeAcquireAppendAccess' & 'unsafeReleaseAppendAccess'
-- * 'unsafeAcquireWriteAccess'  & 'unsafeReleaseWriteAccess'
--
-- NOTE: an acquire __must__ be followed by the corresponding release,
-- otherwise the correctness of the lock is not guaranteed and a dead-lock can
-- happen.
--
-- NOTE: nested locking of the same lock is not allowed, as you might be
-- blocked on yourself.
--
-- = Notes
--
--  * Only use a RAW lock when it is safe to concurrently read and append.
--
--  * We do not guarantee fairness. Once the lock is released, all waiting
--    actors will race for the access.
--
--  * The state @st@ is always evaluated to WHNF and is subject to the
--    'NoThunks' check when enabled.
--
--  * All public functions are exception-safe.
module Control.RAWLock (
    -- * API
    RAWLock
  , new
  , poison
  , read
  , withAppendAccess
  , withReadAccess
  , withWriteAccess
    -- * Unsafe API
    -- $unsafe-api
  , unsafeAcquireAppendAccess
  , unsafeAcquireReadAccess
  , unsafeAcquireWriteAccess
  , unsafeReleaseAppendAccess
  , unsafeReleaseReadAccess
  , unsafeReleaseWriteAccess
  ) where

import           Control.Concurrent.Class.MonadMVar.Strict
import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           GHC.Generics
import           GHC.Stack (CallStack, HasCallStack, callStack)
import           NoThunks.Class
import           Prelude hiding (read)

-- | Any non-negative number of readers
newtype Readers = Readers Word
  deriving newtype (Eq, Ord, Enum, Num, NoThunks)
  deriving stock (Show)

-- | Any non-negative number of writers
newtype Writers = Writers Word
  deriving newtype (Eq, Ord, Enum, Num, NoThunks)
  deriving stock (Show)

-- | Any non-negative number of appenders
newtype Appenders = Appenders Word
  deriving newtype (Eq, Ord, Enum, Num, NoThunks)
  deriving stock (Show)

data RAWState = RAWState {
    waitingReaders   :: !Readers
  , waitingAppenders :: !Appenders
  , waitingWriters   :: !Writers
  } deriving Show

noWriters :: Poisonable RAWState -> Bool
noWriters (Healthy (RAWState _ _ w)) = w == 0
noWriters _                          = True

onlyWriters :: Poisonable RAWState -> Bool
onlyWriters (Healthy (RAWState r a _)) = r == 0 && a == 0
onlyWriters _                          = True

pushReader :: Poisonable RAWState -> Poisonable RAWState
pushReader = fmap (\(RAWState r a w) -> RAWState (r + 1) a w)

pushAppender :: Poisonable RAWState -> Poisonable RAWState
pushAppender = fmap (\(RAWState r a w) -> RAWState r (a + 1) w)

pushWriter :: Poisonable RAWState -> Poisonable RAWState
pushWriter = fmap (\(RAWState r a w) -> RAWState r a (w + 1))

popReader :: Poisonable RAWState -> Poisonable RAWState
popReader = fmap (\(RAWState r a w) -> RAWState (r - 1) a w)

popAppender :: Poisonable RAWState -> Poisonable RAWState
popAppender = fmap (\(RAWState r a w) -> RAWState r (a - 1) w)

popWriter :: Poisonable RAWState -> Poisonable RAWState
popWriter = fmap (\(RAWState r a w) -> RAWState r a (w - 1))

-- | Data that can be replaced with an exception that should be thrown when
-- found.
data Poisonable st =
    Healthy !st
  | Poisoned !(AllowThunk SomeException)
  deriving (Generic, NoThunks, Functor)

data RAWLock m st = RAWLock {
    resource :: !(StrictTMVar m (Poisonable st))
  , appender :: !(StrictMVar m ())
  , queues   :: !(StrictTVar m (Poisonable RAWState))
  } deriving (Generic)

deriving instance ( NoThunks (StrictTMVar m (Poisonable st))
                  , NoThunks (StrictMVar m ())
                  , NoThunks (StrictTVar m (Poisonable RAWState))
                  ) => NoThunks (RAWLock m st)

new ::
     ( MonadMVar m
     , MonadLabelledSTM m
     )
  => st
  -> m (RAWLock m st)
new !st = do
  s <- newTMVarIO (Healthy st)
  atomically $ labelTMVar s "state"
  a <- newMVar ()
  q <- newTVarIO (Healthy emptyRAWState)
  atomically $ labelTVar q "queues"
  pure $ RAWLock s a q

read :: (MonadSTM m, MonadThrow (STM m)) => RAWLock m st -> STM m st
read (RAWLock var _ _) = readTMVar var >>= throwPoisoned

-- | When a lock is poisoned all subsequent access to it is overridden by the
-- poison. This means that the current actor that holds the lock will free it,
-- and any other concurrent actors will be able to release their access,
-- possibly rising the poison exception in the process.
--
-- There is no need (although it is harmless) to release again the current
-- actor once it has poisoned the lock.
poison ::
     (Exception e, MonadMVar m, MonadSTM m, MonadThrow (STM m), HasCallStack)
  => RAWLock m st
  -> (CallStack -> e)
  -> m (Maybe st)
poison (RAWLock var apm q) mkExc = do
  st <- atomically $
    tryReadTMVar var >>= \case
      -- Keep original exception
      Just (Poisoned (AllowThunk exc)) -> throwIO exc
      Just (Healthy st) -> do
        writeTMVar var (Poisoned (AllowThunk (toException (mkExc callStack))))
        writeTVar q (Poisoned (AllowThunk (toException (mkExc callStack))))
        pure (Just st)
      Nothing -> do
        writeTMVar var (Poisoned (AllowThunk (toException (mkExc callStack))))
        writeTVar q (Poisoned (AllowThunk (toException (mkExc callStack))))
        pure Nothing
  _ <- tryPutMVar apm ()
  pure st

-- | Create an initial, empty, unlocked 'RAWState': no readers, no appender,
-- no writer (waiting).
emptyRAWState :: RAWState
emptyRAWState = RAWState 0 0 0

-- | Acquire the 'RAWLock' as a reader.
--
-- Will block when there is a writer or when a writer is waiting to take the
-- lock.
withReadAccess ::
     (MonadSTM m, MonadCatch m, MonadThrow (STM m))
  => RAWLock m st
  -> (st -> m a)
  -> m a
withReadAccess lock =
  bracket
    (atomically (unsafeAcquireReadAccess lock))
    (const (atomically (unsafeReleaseReadAccess lock)))

-- | Acquire the 'RAWLock' as a writer.
--
-- Will block when there is another writer, readers or appenders.
withWriteAccess ::
     (MonadSTM m, MonadCatch m, MonadThrow (STM m))
  => RAWLock m st
  -> (st -> m (a, st))
  -> m a
withWriteAccess lock f =
  fst . fst <$> generalBracket
   (unsafeAcquireWriteAccess lock)
   (\orig -> \case
       ExitCaseSuccess (_, st) -> unsafeReleaseWriteAccess lock st
       _ -> unsafeReleaseWriteAccess lock orig
   )
   f

-- | Acquire the 'RAWLock' as an appender.
--
-- Will block when there is a writer or when there is another appender.
withAppendAccess ::
     (MonadThrow (STM m), MonadSTM m, MonadCatch m, MonadMVar m)
  => RAWLock m st
  -> (st -> m (a, st))
  -> m a
withAppendAccess lock f = do
  fst . fst <$> generalBracket
   (unsafeAcquireAppendAccess lock)
   (\orig -> \case
       ExitCaseSuccess (_, st) -> unsafeReleaseAppendAccess lock st
       _ -> unsafeReleaseAppendAccess lock orig
   )
   f

{-------------------------------------------------------------------------------
  Unsafe API
-------------------------------------------------------------------------------}

throwPoisoned :: MonadThrow m => Poisonable st -> m st
throwPoisoned (Healthy st)                = pure st
throwPoisoned (Poisoned (AllowThunk exc)) = throwIO exc

-- $unsafe-api
--
-- These functions are unsafe in the sense that they do not guard against
-- exceptions, meaning that if you don't take care and ensure exception safety,
-- you might make the RAWLock unusable.
--
-- To be safe, you should ensure that every @unsafeAcquireXAccess@ is paired with
-- @unsafeReleaseXAccess@, __even in the presence of exceptions__.
--
-- Note that for writing and appending, you should restore the original value in
-- presence of an exception!

unsafeAcquireReadAccess ::
     (MonadThrow (STM m), MonadSTM m)
  => RAWLock m st
  -> STM m st
unsafeAcquireReadAccess (RAWLock var _ qs) = do
  -- wait until there are no writers
  readTVar qs >>= check . noWriters
  -- queue myself
  modifyTVar qs pushReader
  -- read the state
  throwPoisoned =<< readTMVar var

unsafeReleaseReadAccess :: MonadSTM m => RAWLock m st -> STM m ()
unsafeReleaseReadAccess (RAWLock _ _ qs) =
  -- unqueue myself
  modifyTVar qs popReader

unsafeAcquireWriteAccess ::
     (MonadThrow (STM m), MonadCatch m, MonadSTM m)
  => RAWLock m st
  -> m st
unsafeAcquireWriteAccess (RAWLock var _ qs) = do
  -- queue myself if there are no other writers
  atomically $ do
    -- wait until there are no writers
    readTVar qs >>= check . noWriters
    -- queue myself
    modifyTVar qs pushWriter
  atomically (do
    -- wait until there are no readers (and as I queued myself above, I'm the
    -- only waiting writer)
    readTVar qs >>= check . onlyWriters
    -- acquire the state
    throwPoisoned =<< takeTMVar var) `onException` atomically (modifyTVar qs popWriter)

unsafeReleaseWriteAccess ::
     (MonadThrow (STM m), MonadSTM m)
  => RAWLock m st
  -> st
  -> m ()
unsafeReleaseWriteAccess (RAWLock var _ qs) !st =
  atomically $ do
    -- write the new state
    tryReadTMVar var >>= \case
      Nothing -> putTMVar var (Healthy st)
      Just (Poisoned (AllowThunk exc)) -> throwIO exc
      Just Healthy{} -> error "Double put"
    -- unqueue myself
    modifyTVar qs popWriter

unsafeAcquireAppendAccess ::
     (MonadThrow (STM m), MonadCatch m, MonadMVar m, MonadSTM m)
  => RAWLock m st
  -> m st
unsafeAcquireAppendAccess (RAWLock var apm qs) = do
  atomically $ do
    -- wait until there are no writers
    readTVar qs >>= check . noWriters
    -- queue myself
    modifyTVar qs pushAppender
  (do
      -- lock the append access
      takeMVar apm
      -- acquire the state
      atomically (readTMVar var >>= throwPoisoned) `onException` putMVar apm ()
    ) `onException` atomically (modifyTVar qs popAppender)

unsafeReleaseAppendAccess ::
     (MonadThrow (STM m), MonadMVar m, MonadSTM m)
  => RAWLock m st
  -> st
  -> m ()
unsafeReleaseAppendAccess (RAWLock var apm qs) !st = do
  atomically $ do
    -- write the new state
    tryReadTMVar var >>= \case
      Just (Poisoned (AllowThunk exc)) -> throwIO exc
      _ -> writeTMVar var (Healthy st)
    -- unqueue myself
    modifyTVar qs popAppender
  -- release the append access
  putMVar apm ()
