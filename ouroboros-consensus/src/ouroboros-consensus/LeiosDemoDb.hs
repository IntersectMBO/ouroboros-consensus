module LeiosDemoDb
  ( -- * API
    withLeiosDb
  , LeiosDbHandle (..)
  , LeiosEbNotification (..)
  , LeiosDbConnection (..)
  , CompletedEbs
  , TraceLeiosDb (..)

    -- * In-memory implementation
  , emptyInMemoryLeiosDb
  , newLeiosDBInMemory
  , newLeiosDBInMemoryWith
  , InMemoryLeiosDb (..)

    -- * SQLite implementation
  , newLeiosDBSQLiteFromEnv
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
  , LeiosDbConnection (..)
  , LeiosDbHandle (..)
  , LeiosEbNotification (..)
  , withLeiosDb
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
  , sql_insert_eb
  , sql_insert_ebBody
  , sql_insert_tx
  , sql_schema
  , truncateLeiosDbAfterSlot
  , vacuumLeiosDb
  )
import LeiosDemoDb.Trace (TraceLeiosDb (..))
