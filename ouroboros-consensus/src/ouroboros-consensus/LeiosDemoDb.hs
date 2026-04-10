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
  , sql_insert_ebPoint
  , sql_insert_ebBody
  , sql_insert_tx
  ) where

import Control.Monad.Class.MonadThrow (MonadThrow, bracket)
import LeiosDemoDb.InMemory
import LeiosDemoDb.SQLite
import LeiosDemoDb.Types

withLeiosDb :: MonadThrow m => LeiosDbHandle m -> (LeiosDbConnection m -> m a) -> m a
withLeiosDb db action =
  bracket (open db) close $ \conn ->
    action conn
