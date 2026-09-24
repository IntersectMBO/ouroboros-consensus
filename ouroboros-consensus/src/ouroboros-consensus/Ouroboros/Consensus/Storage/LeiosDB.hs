{-# LANGUAGE DuplicateRecordFields #-}

module Ouroboros.Consensus.Storage.LeiosDB
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
  , withLeiosDBSQLite

    -- * Re-exported for internal tooling
  , truncateLeiosDbAfterSlot
  , deleteDanglingTxs
  , vacuumLeiosDb
  ) where

import Ouroboros.Consensus.Storage.LeiosDB.API
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
import Ouroboros.Consensus.Storage.LeiosDB.Impl.InMemory
  ( InMemoryLeiosDb (..)
  , emptyInMemoryLeiosDb
  , newLeiosDBInMemory
  , newLeiosDBInMemoryWith
  )
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite
  ( deleteDanglingTxs
  , newLeiosDBSQLite
  , newLeiosDBSQLiteFromEnv
  , newLeiosDBSQLiteWithGcBatchSize
  , truncateLeiosDbAfterSlot
  , vacuumLeiosDb
  , withLeiosDBSQLite
  )
import Ouroboros.Consensus.Storage.LeiosDB.Trace (TraceLeiosDb (..))
