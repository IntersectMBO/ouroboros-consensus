module LeiosDemoDb.Trace (TraceLeiosDb (..), LeiosDbStats (..)) where

import LeiosUtils.CallTrace (SomeJsonCallTrace)

-- | In-memory LeiosDb counters: seeded from the database files once per
-- handle, bumped by the write, copy and GC paths, never written back.
--
-- An EB that has been copied to the immutable partition but not yet evicted
-- from the volatile one counts in both partitions -- that is the truth on
-- disk during the window between copy and GC.
data LeiosDbStats = LeiosDbStats
  { volatileEbs :: !Int
  , immutableEbs :: !Int
  , walBytes :: !Integer
  }
  deriving Show

data TraceLeiosDb
  = -- | A UNIQUE/PRIMARY KEY constraint was violated by an INSERT, the
    -- offending row was silently ignored. Fields: table name, then a
    -- human-readable description of the colliding key.
    TraceLeiosDbInsertCollision String String
  | -- | A write transaction could not take the write lock within SQLite's own
    -- 'busy_timeout' and is being re-attempted. Fields: attempt number, and the
    -- total milliseconds waited so far.
    --
    -- Should be rare: the C-level handler already waited a full timeout, so this
    -- means the lock was held for longer than that. A steady stream of these is
    -- the signal that the write path is saturated.
    TraceLeiosDbBusyRetry Int Double
  | -- | A write transaction has been waiting far longer than contention
    -- explains. Fields as for 'TraceLeiosDbBusyRetry'.
    --
    -- Repeated, not terminal: the wait is unbounded by design, since giving up
    -- means throwing, and throwing here kills the Leios threads. A node that is
    -- merely slow should stay a node that is merely slow.
    TraceLeiosDbBusyStuck Int Double
  | -- | Size of the volatile LeiosDB partition and its on-disk footprint.
    TraceLeiosDbStats LeiosDbStats
  | -- | The background copier committed this many EBs' closures to the
    -- immutable partition.
    TraceLeiosDbCopiedToImmutable !Int
  | -- | A sweep pass evicted this many EB announcement rows from the
    -- volatile partition.
    TraceLeiosDbEvicted !Int
  | -- | The background sweeper failed a sweep pass; the connection is
    -- dropped and the pass retried.
    TraceLeiosDbGCError String
  | -- | The copy queue was full and the hash was dropped. Not a data-loss
    -- signal (GC self-heal re-delivers), but a steady stream of these means
    -- the copier cannot keep up with certification.
    TraceLeiosDbCopyQueueFull String
  | -- | The background copier failed on an EB (which stays pinned and will be
    -- retried). Fields: the EB hash, then the reason.
    TraceLeiosDbCopyError String String
  | -- | A trace event for LeiosUtils.CallTrace spans
    TraceLeiosDbCall !SomeJsonCallTrace
  deriving Show
