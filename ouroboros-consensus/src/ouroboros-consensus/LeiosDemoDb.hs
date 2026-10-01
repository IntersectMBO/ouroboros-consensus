{-# LANGUAGE DuplicateRecordFields #-}

module LeiosDemoDb
  ( -- * Lifecycle arguments
    LeiosDbArgs (..)
  , leiosDbInMemory
  , leiosDbSQLite

    -- * API
  , LeiosDbHandle (..)
  , LeiosDbStats (..)
  , LeiosEbNotification (..)
  , LeiosDbReader (..)
  , LeiosDbWriter (..)
  , Promise (..)
  , withReader
  , withWriter
  , allocateHandle
  , withReaderAndWriter
  , allocateReader
  , allocateWriter
  , awaitAll
  , CompletedEbs
  , TraceLeiosDb (..)

    -- * In-memory implementation
  , emptyInMemoryLeiosDb
  , newLeiosDBInMemory
  , newLeiosDBInMemoryWith
  , InMemoryLeiosDb (..)

    -- * SQLite implementation
  , newLeiosDBSQLiteFromEnv
  , newLeiosDBSQLiteWithGcBatchSize
  , newLeiosDBSQLite
  , withLeiosDBSQLite

    -- * Re-exported for internal tooling
  , truncateLeiosDbAfterSlot
  , deleteDanglingTxs
  , vacuumLeiosDb

    -- * SQL (re-exported for leios-schedule-gen)
  , sql_schema
  , sql_insert_eb
  , sql_insert_ebBody
  , sql_insert_tx
  ) where

import Control.Tracer (Tracer)
import LeiosDemoDb.Common
  ( CompletedEbs
  , LeiosDbHandle (..)
  , LeiosDbReader (..)
  , LeiosDbStats (..)
  , LeiosDbWriter (..)
  , LeiosEbNotification (..)
  , Promise (..)
  , allocateHandle
  , allocateReader
  , allocateWriter
  , awaitAll
  , withReader
  , withReaderAndWriter
  , withWriter
  )
import LeiosDemoDb.InMemory
  ( InMemoryLeiosDb (..)
  , emptyInMemoryLeiosDb
  , newLeiosDBInMemory
  , newLeiosDBInMemoryWith
  )
import LeiosDemoDb.SQLite
  ( deleteDanglingTxs
  , newLeiosDBSQLite
  , newLeiosDBSQLiteFromEnv
  , newLeiosDBSQLiteWithGcBatchSize
  , sql_insert_eb
  , sql_insert_ebBody
  , sql_insert_tx
  , sql_schema
  , truncateLeiosDbAfterSlot
  , vacuumLeiosDb
  , withLeiosDBSQLite
  )
import LeiosDemoDb.Trace (TraceLeiosDb (..))
import Ouroboros.Consensus.Util.IOLike (IOLike)

-- | Configuration for opening a Leios database. Passed to 'ChainDB', which
-- calls 'ldbOpen' exactly once during 'openDB'. The handle it returns is
-- shared between the ChainDB and the LedgerDB; do not call 'ldbOpen' yourself
-- unless you need a separate handle (e.g. for the NodeKernel).
data LeiosDbArgs m = LeiosDbArgs
  { ldbOpen :: m (LeiosDbHandle m)
  -- ^ Open the database and return a handle. Called once by ChainDB.
  }

-- | In-memory Leios database, generic in the monad (for tests and IOSim).
leiosDbInMemory :: IOLike m => LeiosDbArgs m
leiosDbInMemory = LeiosDbArgs{ldbOpen = newLeiosDBInMemory}

-- | SQLite-backed Leios database (IO only, for production and tools).
leiosDbSQLite :: Tracer IO TraceLeiosDb -> FilePath -> FilePath -> LeiosDbArgs IO
leiosDbSQLite tracer vol imm = LeiosDbArgs{ldbOpen = newLeiosDBSQLite tracer vol imm}
