module LeiosDemoDb
  ( -- * API
    LeiosDbHandle (..)
  , LeiosEbNotification (..)
  , LeiosDbReader (..)
  , LeiosDbWriter (..)
  , Promise (..)
  , withReader
  , withWriter
  , allocateReader
  , allocateWriter
  , CompletedEbs
  , TraceLeiosDb (..)

    -- * In-memory implementation
  , emptyInMemoryLeiosDb
  , newLeiosDBInMemory
  , newLeiosDBInMemoryWith
  , InMemoryLeiosDb (..)

    -- * SQLite implementation
  , withLeiosDBSQLiteFromEnv
  , withLeiosDBSQLite
  , newLeiosDBSQLite

    -- * Re-exported for internal tooling
  , truncateLeiosDbAfterSlot
  , deleteDanglingTxs
  , vacuumLeiosDb

    -- * SQL (re-exported for leiosdemo app)
  , sql_schema
  , sql_insert_eb
  , sql_insert_ebBody
  , sql_insert_tx
  ) where

import LeiosDemoDb.Common
  ( CompletedEbs
  , LeiosDbHandle (..)
  , LeiosDbReader (..)
  , LeiosDbWriter (..)
  , LeiosEbNotification (..)
  , Promise (..)
  , allocateReader
  , allocateWriter
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
  , sql_insert_eb
  , sql_insert_ebBody
  , sql_insert_tx
  , sql_schema
  , truncateLeiosDbAfterSlot
  , vacuumLeiosDb
  , withLeiosDBSQLite
  , withLeiosDBSQLiteFromEnv
  )
import LeiosDemoDb.Trace (TraceLeiosDb (..))
