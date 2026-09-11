{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The single writer in front of the LeiosDb.
--
-- Every write goes through one worker thread that owns one dedicated
-- connection. Producers (each peer's fetch client, and the forge) hand it an
-- action to run on that connection and get a 'Promise' back, so synchrony is
-- opt-in: the forge awaits (its EB must be on disk before the referencing RB
-- propagates), while the fetch path voids the promise and returns to its
-- mini-protocol immediately.
--
-- What this buys, in the order it matters:
--
-- * __A connection is never used by two threads.__ 'LeiosDbConnection' is not
--   thread-safe (its prepared statements least of all), and driving one from
--   two threads corrupts SQLite and takes the node down with a SIGSEGV. Writes
--   used to be forked onto the /calling/ peer's connection, so two EB bodies
--   arriving close together on one peer raced each other. Here the only thread
--   that ever touches the writer connection is the worker.
--
-- * __Ordering is free.__ A body write initialises the @missingTxCount@ that
--   the tx writes decrement, so the two must not be reordered. One FIFO
--   consumer gives that by construction rather than by convention.
--
-- * __No writer-vs-writer @SQLITE_BUSY@.__ SQLite admits one writer at a time
--   anyway; serialising here costs no throughput and removes the retry-backoff
--   waste that concurrent connections spend discovering the same fact.
--
-- * __Backpressure instead of unbounded work.__ The queue is bounded, so a
--   producer that outruns the disk waits rather than piling up EB bodies
--   (hundreds of kB each) in memory.
--
-- This mirrors the ChainDB's own @varChainSelQueue@ + @varBlockProcessed@
-- pattern: a bounded queue of work with a per-item result variable the
-- submitter may or may not wait on.
module LeiosDemoDb.Writer
  ( -- * Handle
    LeiosDbWriter (..)
  , Promise (..)

    -- * Construction
  , withLeiosDbWriter
  , newLeiosDbWriter
  , writerQueueDepth

    -- * Facade
  , withWriterBackedDb
  ) where

import Control.Monad (forever)
import Control.ResourceRegistry (ResourceRegistry, allocate, forkLinkedThread)
import Data.Void (Void)
import GHC.Stack (HasCallStack)
import LeiosDemoDb.Common
  ( LeiosDbConnection (..)
  , LeiosDbHandle (..)
  )
import Ouroboros.Consensus.Util.IOLike

-- | The result of an enqueued write, to be awaited or ignored.
--
-- 'await' rethrows whatever the write threw, in the awaiting thread. Awaiting
-- twice is fine; not awaiting at all is fine too.
newtype Promise m a = Promise {await :: m a}

newtype LeiosDbWriter m = LeiosDbWriter
  { enqueueWrite :: forall b. HasCallStack => (LeiosDbConnection m -> m b) -> m (Promise m b)
  -- ^ Run the action on the writer's connection, in submission order. Blocks
  -- only while the queue is full; 'void' the promise to fire and forget. The
  -- action runs on the worker thread, so keep it to the write and its tracing.
  }

-- | How deep to make the queue, given the number of upstream peers.
--
-- One slot per producer that can be mid-write -- each upstream peer's fetch
-- client, plus the forge -- and a little slack so a producer rarely finds the
-- queue full while the worker is busy with the job it just took.
--
-- Deliberately shallow. Depth beyond that adds no throughput (there is one
-- worker) and directly delays the commit-time notifications that gate vote
-- scheduling and chain selection, so a deep queue would buy memory and lose
-- certifications.
writerQueueDepth :: Int -> Int
writerQueueDepth numUpstreamPeers = numUpstreamPeers + 1 + slack
 where
  slack = 2

-- | Run an action with a writer backed by a worker thread and its own
-- connection, both torn down on exit.
--
-- The worker is linked: a write that fails is not survivable (no caller
-- catches 'LeiosDbException'), so it takes the node down as it did when the
-- writes were inline.
withLeiosDbWriter ::
  forall m a.
  (IOLike m, HasCallStack) =>
  LeiosDbHandle m ->
  -- | Queue depth; see 'writerQueueDepth'.
  Int ->
  (LeiosDbWriter m -> m a) ->
  m a
withLeiosDbWriter db depth action =
  bracket (openWriter db) close $ \conn -> do
    queue <- newWriterQueue depth
    withAsync (writerLoop conn queue) $ \workerThread -> do
      link workerThread
      action (mkWriter queue)

-- | 'withLeiosDbWriter' for a caller that owns a 'ResourceRegistry' rather
-- than a bracket: the connection is released, and the worker killed, when the
-- registry closes.
newLeiosDbWriter ::
  forall m.
  (IOLike m, HasCallStack) =>
  ResourceRegistry m ->
  LeiosDbHandle m ->
  -- | Queue depth; see 'writerQueueDepth'.
  Int ->
  m (LeiosDbWriter m)
newLeiosDbWriter registry db depth = do
  (_, conn) <- allocate registry (\_ -> openWriter db) close
  queue <- newWriterQueue depth
  _ <- forkLinkedThread registry "LeiosDbWriter" (writerLoop conn queue)
  pure (mkWriter queue)

newWriterQueue :: IOLike m => Int -> m (TBQueue m (Job m))
newWriterQueue depth = atomically $ newTBQueue (fromIntegral depth)

mkWriter :: IOLike m => TBQueue m (Job m) -> LeiosDbWriter m
mkWriter queue =
  LeiosDbWriter
    { enqueueWrite = \op -> do
        resultVar <- newEmptyTMVarIO
        atomically $ writeTBQueue queue (Job op resultVar)
        pure $ Promise (either throwIO pure =<< atomically (readTMVar resultVar))
    }

writerLoop :: IOLike m => LeiosDbConnection m -> TBQueue m (Job m) -> m Void
writerLoop conn queue = forever $ do
  Job op resultVar <- atomically $ readTBQueue queue
  result <- tryAnyException (op conn)
  -- Publish before rethrowing, so an awaiting producer sees the failure
  -- rather than blocking on a promise the dying worker would never fill.
  atomically $ putTMVar resultVar result
  either throwIO (\_ -> pure ()) result

data Job m
  = forall b.
    Job
      !(LeiosDbConnection m -> m b)
      !(StrictTMVar m (Either SomeException b))

-- | 'try' pinned to 'SomeException'; 'Ouroboros.Consensus.Util.IOLike.tryAll'
-- is the same thing but is not exported.
tryAnyException :: MonadCatch m => m b -> m (Either SomeException b)
tryAnyException = try

-- | Present a writer-backed database behind the ordinary 'LeiosDbHandle'.
--
-- Every connection handed out reads on its own connection, as before, but
-- routes its writes through the one shared writer and awaits them. So the
-- interface, and read-your-writes, are exactly as an unwrapped handle -- which
-- is the point: the existing 'LeiosDbHandle' test suite runs against this
-- unchanged, and what it exercises is the writer.
--
-- Callers that want the latency win rather than just the safety take the
-- 'LeiosDbWriter' directly and skip the await.
withWriterBackedDb ::
  forall m a.
  (IOLike m, HasCallStack) =>
  LeiosDbHandle m ->
  -- | Queue depth; see 'writerQueueDepth'.
  Int ->
  (LeiosDbHandle m -> m a) ->
  m a
withWriterBackedDb db depth action =
  withLeiosDbWriter db depth $ \writer ->
    action db{open = wrap writer <$> open db}
 where
  wrap :: LeiosDbWriter m -> LeiosDbConnection m -> LeiosDbConnection m
  wrap writer readConn =
    readConn
      { leiosDbInsertEbPoint = \point sz ->
          through writer $ \c -> leiosDbInsertEbPoint c point sz
      , leiosDbInsertEbBody = \point eb ->
          through writer $ \c -> leiosDbInsertEbBody c point eb
      , leiosDbInsertTxs = \txs ->
          through writer $ \c -> leiosDbInsertTxs c txs
      }

  through :: LeiosDbWriter m -> (LeiosDbConnection m -> m b) -> m b
  through writer op = await =<< enqueueWrite writer op
