module LeiosDemoDb
  ( -- * API
    LeiosDbHandle (..)
  , LeiosDbStats (..)
  , LeiosEbNotification (..)
  , LeiosDbReader (..)
  , LeiosDbWriter (..)
  , Promise (..)
  , withReader
  , withWriter
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
  , newLeiosDBSQLiteReadOnly
  , openLeiosDBSQLite
  , openLeiosDBSQLiteWithGcBatchSize
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

import LeiosDemoDb.Common
  ( CompletedEbs
  , LeiosDbHandle (..)
  , LeiosDbReader (..)
  , LeiosDbStats (..)
  , LeiosDbWriter (..)
  , LeiosEbNotification (..)
  , Promise (..)
  , allocateReader
  , allocateWriter
  , awaitAll
  , withReader
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
  , newLeiosDBSQLiteReadOnly
  , newLeiosDBSQLiteWithGcBatchSize
  , openLeiosDBSQLite
  , openLeiosDBSQLiteWithGcBatchSize
  , sql_insert_eb
  , sql_insert_ebBody
  , sql_insert_tx
  , sql_schema
  , truncateLeiosDbAfterSlot
  , vacuumLeiosDb
  , withLeiosDBSQLite
  )
import LeiosDemoDb.Trace (TraceLeiosDb (..))
