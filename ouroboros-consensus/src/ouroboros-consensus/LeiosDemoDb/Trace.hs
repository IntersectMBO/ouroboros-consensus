-- TODO(geo2a): Claude really likes partial fields. Consider getting rid of them in the final production version.
{-# OPTIONS_GHC -Wno-partial-fields #-}

module LeiosDemoDb.Trace (TraceLeiosDb (..)) where

import LeiosUtils.CallTrace (SomeJsonCallTrace)

data TraceLeiosDb
  = -- | A UNIQUE/PRIMARY KEY constraint was violated by an INSERT, the
    -- offending row was silently ignored. Fields: table name, then a
    -- human-readable description of the colliding key.
    TraceLeiosDbInsertCollision String String
  | -- | Size of the volatile LeiosDB partition and its on-disk footprint, sampled on a
    -- timer independent of any chain activity.
    TraceLeiosDbVolatileStats
      { volatileEbs :: !Int
      , volatileEbTxs :: !Int
      , volatileTxs :: !Int
      , dbFileBytes :: !Integer
      , walBytes :: !Integer
      }
  | -- | Size of the immutable LeiosDB partition, sampled on the same timer as
    -- 'TraceLeiosDbVolatileStats'. The counts are maintained incrementally by
    -- 'LeiosDbHandle.leiosDbMarkAsImmutable'.
    TraceLeiosDbImmutableStats
      { immutableEbs :: !Int
      , immutableEbTxs :: !Int
      , immutableTxs :: !Int
      }
  | -- | Rows moved into the immutable partition by 'LeiosDbHandle.leiosDbMarkAsImmutable'.
    TraceLeiosDbCopiedToImmutable
      { copiedEbs :: !Int
      , copiedEbTxs :: !Int
      , copiedTxs :: !Int
      }
  | -- | A trace event for LeiosUtils.CallTrace spans
    TraceLeiosDbCall !SomeJsonCallTrace
  deriving Show
