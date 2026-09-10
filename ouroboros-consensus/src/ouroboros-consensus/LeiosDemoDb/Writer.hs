{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A worker thread that runs submitted actions one at a time.
--
-- This is the machinery behind 'LeiosDemoDb.Common.LeiosDbWriter' for backends
-- that need their writes serialised -- which is to say SQLite, whose
-- connection and prepared statements must never be driven by two threads at
-- once. (The in-memory backend needs none of this: its state is a @TVar@.)
--
-- The worker is deliberately ignorant of connections. A backend binds its own
-- connection into the actions it submits, so the action is /built/ on the
-- caller's thread and /run/ on the worker's, and the connection still has
-- exactly one toucher.
module LeiosDemoDb.Writer
  ( Submit (..)
  , newWorker
  ) where

import Control.Monad (void)
import GHC.Stack (HasCallStack)
import LeiosDemoDb.Common (Promise (..))
import Ouroboros.Consensus.Util.IOLike

-- | Hand an action to the worker; get back a 'Promise' for its result.
--
-- Blocks only while the queue is full, which is the backpressure: a producer
-- that outruns the disk waits rather than piling up EB bodies in memory.
newtype Submit m = Submit (forall b. HasCallStack => m b -> m (Promise m b))

-- | Start a worker draining a bounded queue. Returns the submission function
-- and a stop action that lets the queue drain before the thread exits.
--
-- The worker is linked: a write that fails is not survivable (no caller
-- catches 'LeiosDemoException.LeiosDbException'), so it takes the node down as
-- it did when writes were inline.
newWorker ::
  forall m.
  (IOLike m, HasCallStack) =>
  -- | Queue depth. One slot per producer that can be mid-write -- each
  -- upstream peer's fetch client, plus the forge -- and a little slack.
  -- Deliberately shallow: depth beyond that adds no throughput (there is one
  -- worker) and only delays the commit-time notifications that gate vote
  -- scheduling and chain selection.
  Int ->
  m (Submit m, m ())
newWorker depth = do
  queue <- atomically $ newTBQueue (fromIntegral depth)
  worker <- async (loop queue)
  link worker
  pure (Submit (submit queue), stop queue worker)
 where
  submit :: forall b. TBQueue m (Maybe (Job m)) -> m b -> m (Promise m b)
  submit queue action = do
    resultVar <- newEmptyTMVarIO
    atomically $ writeTBQueue queue (Just (Job action resultVar))
    pure $ Promise (either throwIO pure =<< atomically (readTMVar resultVar))

  -- A sentinel rather than a cancel, so a write in flight is never torn down
  -- mid-transaction and anything already queued still lands.
  stop queue worker = do
    atomically $ writeTBQueue queue Nothing
    void $ waitCatch worker

  loop queue =
    atomically (readTBQueue queue) >>= \case
      Nothing -> pure ()
      Just (Job action resultVar) -> do
        result <- tryAnyException action
        -- Publish before rethrowing, so an awaiting producer sees the failure
        -- rather than blocking on a promise the dying worker would never fill.
        atomically $ putTMVar resultVar result
        either throwIO (\_ -> loop queue) result

data Job m
  = forall b.
    Job
      !(m b)
      !(StrictTMVar m (Either SomeException b))

-- | 'try' pinned to 'SomeException'; 'Ouroboros.Consensus.Util.IOLike.tryAll'
-- is the same thing but is not exported.
tryAnyException :: MonadCatch m => m b -> m (Either SomeException b)
tryAnyException = try
