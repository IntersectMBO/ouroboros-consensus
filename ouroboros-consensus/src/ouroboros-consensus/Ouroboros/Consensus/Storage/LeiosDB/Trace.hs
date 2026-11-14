module Ouroboros.Consensus.Storage.LeiosDB.Trace (TraceLeiosDb (..), LeiosDbStats (..)) where

import Cardano.Slotting.Slot (SlotNo)
import Control.Monad.Class.MonadTime.SI (DiffTime)

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
    -- offending row was silently ignored.
    TraceLeiosDbInsertCollision
      -- | table name
      String
      -- | a human-readable description of the colliding key
      String
  | -- | A write had to wait for room in the writer's queue: the writer is
    -- the bottleneck and every producer is now behind it.
    TraceLeiosDbWriterQueueFull
      -- | the job that waited
      String
  | -- | The submitting thread died waiting for room in the queue, so this
    -- write is lost and no exception says so.
    TraceLeiosDbWriteAbandoned
      -- | the job
      String
  | -- | The only evidence that the writer ran a job at all.
    TraceLeiosDbWriteJobDone
      -- | the job
      String
      -- | how long the writer spent on it
      DiffTime
  | -- | Size of the volatile LeiosDB partition and its on-disk footprint.
    TraceLeiosDbStats LeiosDbStats
  | -- | A promotion committed EBs' closures to the immutable partition.
    TraceLeiosDbCopiedToImmutable
      -- | number of EBs copied
      Int
  | -- | A garbage collection deleted rows from the volatile partition.
    TraceLeiosDbEvicted
      -- | number of rows evicted
      Int
  | -- | The immutable partition dropped EBs at or after the ImmutableDB tip.
    TraceLeiosDbTruncatedImmutable
      -- | the slot the EBs at or after which were deleted, and the new
      -- promoted slot
      SlotNo
      -- | number of EBs deleted
      Int
  deriving Show
