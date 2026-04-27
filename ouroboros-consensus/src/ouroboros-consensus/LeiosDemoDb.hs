module LeiosDemoDb
  ( -- * API
    withLeiosDb
  , LeiosDbHandle (..)
  , LeiosDbConnection (..)
  , LeiosFetchWork (..)
  , CompletedEbs

    -- * In-memory implementation
  , emptyInMemoryLeiosDb
  , newLeiosDBInMemory
  , newLeiosDBInMemoryWith
  , InMemoryLeiosDb (..)

    -- * SQLite implementation
  , newLeiosDBSQLiteFromEnv
  , newLeiosDBSQLite

    -- * SQL (re-exported for leiosdemo app)
  , sql_schema
  , sql_insert_eb
  , sql_insert_ebBody
  , sql_insert_tx
  ) where

import LeiosDemoDb.Common
import LeiosDemoDb.InMemory
import LeiosDemoDb.SQLite
