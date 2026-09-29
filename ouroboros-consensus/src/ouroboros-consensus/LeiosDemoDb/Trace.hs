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
  | -- | A write had to wait for room in the writer's queue: the writer is
    -- the bottleneck and every producer is now behind it. Field: the job
    -- that waited.
    TraceLeiosDbWriterQueueFull String
  | -- | The submitting thread died waiting for room in the queue, so this
    -- write is lost and no exception says so. Field: the job.
    TraceLeiosDbWriteAbandoned String
  | -- | The only evidence that the writer ran a job at all. Field: the job.
    TraceLeiosDbWriteJobDone String
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
  | -- | Copying an EB failed. It stays pinned, so it is still the next one
    -- to copy. Fields: the EB hash, then the reason.
    TraceLeiosDbCopyError String String
  | -- | A trace event for LeiosUtils.CallTrace spans
    TraceLeiosDbCall !SomeJsonCallTrace
  deriving Show
