{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The single writer in front of the LeiosDb.
--
-- Every write goes through one worker thread that owns one dedicated
-- connection. A caller submits one of the three writes below and gets a
-- 'Promise' for its result.
--
-- What this buys, in the order it matters:
--
-- * __A connection is never used by two threads.__ 'LeiosDbConnection' is not
--   thread-safe (its prepared statements least of all), and driving one from
--   two threads corrupts SQLite and takes the node down with a SIGSEGV. Writes
--   used to be forked onto the /calling/ peer's connection, so two EB bodies
--   arriving close together on one peer raced each other. Here the only thread
--   that ever touches the writer connection is the worker. This is the point of
--   the exercise, and it holds whether or not the submitter waits.
--
-- * __Ordering is free.__ 'writeEbBody' initialises the @missingTxCount@ that
--   'writeTxs' decrements, so the two must not be reordered. One FIFO consumer
--   gives that by construction rather than by convention.
--
-- * __No writer-vs-writer @SQLITE_BUSY@.__ SQLite admits one writer at a time
--   anyway; serialising here costs no throughput and removes the retry-backoff
--   waste that concurrent connections spend discovering the same fact.
--
-- * __Backpressure instead of unbounded work.__ The queue is bounded, so a
--   producer that outruns the disk waits rather than piling up EB bodies
--   (hundreds of kB each) in memory.
--
-- The 'Promise' is what would let a submitter /not/ wait. Today every caller
-- does wait, because the closure-acquired trace needs the write's
-- 'CompletedEbs'; see the TODO at the call sites in "LeiosDemoLogic".
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
import Data.ByteString (ByteString)
import Data.Void (Void)
import GHC.Stack (HasCallStack)
import LeiosDemoDb.Common
  ( CompletedEbs
  , LeiosDbConnection (..)
  , LeiosDbHandle (..)
  )
import LeiosDemoTypes (BytesSize, LeiosEb, LeiosPoint, TxHash)
import Ouroboros.Consensus.Util.IOLike

-- | The result of a submitted write.
--
-- 'await' rethrows whatever the write threw, in the awaiting thread. Awaiting
-- twice is fine; not awaiting at all is fine too -- a write that fails kills
-- the worker, and with it the node, whether or not anyone was waiting.
newtype Promise m a = Promise {await :: m a}

-- | The whole write surface of the LeiosDb.
--
-- Deliberately three fixed operations rather than "run this action on a
-- connection": these are all the writes there are, and naming them keeps the
-- queue's contents inspectable. They stay separate because they have separate
-- futures -- 'writeEbPoint' exists only until EB announcements are wired in,
-- at which point the point is already present and the call goes away.
data LeiosDbWriter m = LeiosDbWriter
  { writeEbPoint :: HasCallStack => LeiosPoint -> BytesSize -> m (Promise m ())
  -- ^ Record an announced EB's point and expected size. Idempotent.
  , writeEbBody :: HasCallStack => LeiosPoint -> LeiosEb -> m (Promise m CompletedEbs)
  -- ^ Persist an EB body. Its point must already be present, so submit
  -- 'writeEbPoint' first; the queue preserves that order.
  , writeTxs :: HasCallStack => [(TxHash, ByteString)] -> m (Promise m CompletedEbs)
  -- ^ Persist tx bodies.
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

-- * Internals

-- | A queued write and the variable its result goes into. Existential because
-- the three operations differ in what they return; the queue does not care.
data Job m
  = forall b.
    Job
      !(LeiosDbConnection m -> m b)
      !(StrictTMVar m (Either SomeException b))

newWriterQueue :: IOLike m => Int -> m (TBQueue m (Job m))
newWriterQueue depth = atomically $ newTBQueue (fromIntegral depth)

mkWriter :: forall m. IOLike m => TBQueue m (Job m) -> LeiosDbWriter m
mkWriter queue =
  LeiosDbWriter
    { writeEbPoint = \point ebBytesSize ->
        submit $ \conn -> leiosDbInsertEbPoint conn point ebBytesSize
    , writeEbBody = \point eb ->
        submit $ \conn -> leiosDbInsertEbBody conn point eb
    , writeTxs = \txs ->
        submit $ \conn -> leiosDbInsertTxs conn txs
    }
 where
  submit :: forall b. (LeiosDbConnection m -> m b) -> m (Promise m b)
  submit op = do
    resultVar <- newEmptyTMVarIO
    atomically $ writeTBQueue queue (Job op resultVar)
    pure $ Promise (either throwIO pure =<< atomically (readTMVar resultVar))

writerLoop :: IOLike m => LeiosDbConnection m -> TBQueue m (Job m) -> m Void
writerLoop conn queue = forever $ do
  Job op resultVar <- atomically $ readTBQueue queue
  result <- tryAnyException (op conn)
  -- Publish before rethrowing, so an awaiting producer sees the failure
  -- rather than blocking on a promise the dying worker would never fill.
  atomically $ putTMVar resultVar result
  either throwIO (\_ -> pure ()) result

-- | 'try' pinned to 'SomeException'; 'Ouroboros.Consensus.Util.IOLike.tryAll'
-- is the same thing but is not exported.
tryAnyException :: MonadCatch m => m b -> m (Either SomeException b)
tryAnyException = try

-- | Present a writer-backed database behind the ordinary 'LeiosDbHandle'.
--
-- Every connection handed out reads on its own connection, as before, but
-- routes its writes through the one shared writer. So the interface, and
-- read-your-writes, are exactly as an unwrapped handle -- which is the point:
-- the existing 'LeiosDbHandle' test suite runs against this unchanged, and
-- what it exercises is the writer.
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
      { leiosDbInsertEbPoint = \point sz -> await =<< writeEbPoint writer point sz
      , leiosDbInsertEbBody = \point eb -> await =<< writeEbBody writer point eb
      , leiosDbInsertTxs = \txs -> await =<< writeTxs writer txs
      }
