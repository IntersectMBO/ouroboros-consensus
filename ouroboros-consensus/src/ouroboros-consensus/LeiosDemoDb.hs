module LeiosDemoDb
  ( -- * API
    withLeiosDb
  , LeiosDbHandle (..)
  , LeiosDbStats (..)
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
  , newLeiosDBSQLiteWithGcPacing
  , newLeiosDBSQLite
  , newLeiosDBSQLiteReadOnly

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
  , LeiosDbStats (..)
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
  , newLeiosDBSQLiteReadOnly
  , newLeiosDBSQLiteWithGcPacing
  , sql_insert_eb
  , sql_insert_ebBody
  , sql_insert_tx
  , sql_schema
  , truncateLeiosDbAfterSlot
  , vacuumLeiosDb
  )
import LeiosDemoDb.Trace (TraceLeiosDb (..))
