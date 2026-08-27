module LeiosDemoDb.Trace (TraceLeiosDb (..)) where

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
  deriving Show
