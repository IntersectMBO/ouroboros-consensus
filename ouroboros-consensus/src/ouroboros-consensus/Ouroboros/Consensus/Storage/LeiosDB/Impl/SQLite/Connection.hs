{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Opening, configuring and closing connections to the LeiosDB partition
-- files, and 'Conn': a reader's (or the writer's) pair of connections with
-- their prepared statements.
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Connection
  ( -- * Partition files
    initialiseLeiosDbFiles
  , openRawConnection
  , withReadOnlyConn
  , closeChecked

    -- * Connections with prepared statements
  , Conn (..)
  , mkConn
  , closeConn
  , dbWithWriteTransaction
  ) where

import Control.Concurrent.Class.MonadSTM.Strict (StrictTVar)
import Control.Exception (throwIO)
import Control.Monad (void)
import Control.Monad.Class.MonadThrow (bracket)
import Control.Tracer (Tracer)
import Data.Foldable (traverse_)
import Data.String (fromString)
import Database.SQLite3
  ( SQLOpenFlag (..)
  , SQLVFS (..)
  , open2
  )
import qualified Database.SQLite3.Direct as DB
import GHC.Stack (HasCallStack)
import qualified GHC.Stack
import Ouroboros.Consensus.Storage.LeiosDB.Exception
  ( LeiosDbException (..)
  , LeiosDbFailure (..)
  )
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Primitives
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Schema
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Statements
import Ouroboros.Consensus.Storage.LeiosDB.Trace (LeiosDbStats (..), TraceLeiosDb (..))

-- | Open a strictly read-only connection, for seeding DB statistics.
withReadOnlyConn :: HasCallStack => FilePath -> (DB.Database -> IO a) -> IO a
withReadOnlyConn dbPath =
  bracket (openReadOnlyRawConnection dbPath) (void . DB.close)

-- | Open an existing file read-only: no create, no DDL.
openReadOnlyRawConnection :: HasCallStack => FilePath -> IO DB.Database
openReadOnlyRawConnection dbPath = do
  db <- open2 (fromString dbPath) [SQLOpenReadOnly] SQLVFSDefault
  -- Only mmap_size: journal_mode and page_size need write access.
  dbExec db "pragma mmap_size = 268435500;"
  pure db

-- | Create the volatile and immutable LeiosDB partition files
--   if missing and apply their schemas.
initialiseLeiosDbFiles :: HasCallStack => FilePath -> FilePath -> IO ()
initialiseLeiosDbFiles volPath immPath = do
  initialiseFile volPath sql_schema_vol
  initialiseFile immPath sql_schema_imm
 where
  initialiseFile path ddl =
    bracket
      (open2 (fromString path) [SQLOpenReadWrite, SQLOpenCreate] SQLVFSDefault)
      (void . DB.close)
      $ \db -> do
        traverse_ (dbExec db) (connectionPragmas <> creationPragmas)
        dbWithWriteTransactionRaw db $ dbExec db (fromString ddl)

-- | Open a read-write connection to an existing partition file, whose schema
-- 'initialiseLeiosDbFiles' has already applied.
openRawConnection :: HasCallStack => FilePath -> IO DB.Database
openRawConnection path = do
  db <- open2 (fromString path) [SQLOpenReadWrite] SQLVFSDefault
  orCloseOnError db $ traverse_ (dbExec db) connectionPragmas
  pure db

-- | Pragmas every connection sets.
connectionPragmas :: [DB.Utf8]
connectionPragmas =
  [ -- First, before any pragma that takes a lock -- 'journal_mode' does. Until
    -- this runs the timeout is zero, so a contended lock is refused outright
    -- rather than waited for, and opening a second connection to a busy
    -- database fails where it should merely be slow.
    --
    -- Let SQLite do that waiting in C, retrying tightly rather than sleeping
    -- through the window it is waiting for. Safe because writers take the lock
    -- at BEGIN, so nothing waits here holding a snapshot; see
    -- 'dbWithWriteTransaction'.
    "pragma busy_timeout = 1000;"
  , "pragma synchronous = normal;"
  , "pragma mmap_size = 268435500;"
  , -- SQLite's own default, spelled out because it is what keeps the log
    -- bounded: passive checkpoints reset the WAL every 1000 frames, provided
    -- no connection is sitting on a stale read snapshot. One that is will
    -- freeze back-fill indefinitely; see 'dbWithWriteTransaction'.
    "pragma wal_autocheckpoint = 1000;"
  ]

-- | Persistent pragmas, set once by 'initialiseLeiosDbFiles' after
-- 'connectionPragmas'.
creationPragmas :: [DB.Utf8]
creationPragmas =
  [ -- Must precede 'journal_mode': SQLite cannot change the page size of a
    -- database already in WAL mode, so the order this list used to have left
    -- the setting a silent no-op and every run so far on the 4096 default.
    -- Which is where it belongs anyway. Measured: a devnet run with 32768
    -- actually in effect reached 35x WAL amplification (34 GiB of log for 0.97
    -- GiB of data) against ~18x for the same workload at 4096. The WAL is a
    -- page-level redo log, so a commit rewrites each dirtied page whole, and
    -- both hot indexes are keyed by hash, so writes scatter -- the page count
    -- barely falls as the page grows, the bytes just multiply.
    "pragma page_size = 4096;"
  , "pragma journal_mode = WAL;"
  ]

data Conn = Conn
  { conVolDb :: !DB.Database
  -- ^ The connection to the volatile partition.
  , connVolStmts :: !VolStmts
  -- ^ Precompiled statements used with the volatile partition.
  , connTracer :: !(Tracer IO TraceLeiosDb)
  -- ^ So the write path can report exhausting SQLite's own busy timeout.
  , connStats :: !(StrictTVar IO LeiosDbStats)
  -- ^ Usage stats for this connection.
  , conImmDb :: !DB.Database
  -- ^ The connection to the immutable partition.
  , connImmStmts :: !ImmStmts
  -- ^ Precompiled statements used with the immutable partition.
  }

-- | Build a 'Conn' over two open partition connections, preparing every
-- statement; 'closeConn' undoes it.
mkConn ::
  Tracer IO TraceLeiosDb ->
  StrictTVar IO LeiosDbStats ->
  DB.Database ->
  DB.Database ->
  IO Conn
mkConn tracer statsVar volDb immDb = do
  stmts <- prepareVolStmts volDb
  immStmts <- prepareImmStmts immDb
  pure
    Conn
      { conVolDb = volDb
      , connVolStmts = stmts
      , connTracer = tracer
      , connStats = statsVar
      , conImmDb = immDb
      , connImmStmts = immStmts
      }

closeConn :: Conn -> IO ()
closeConn conn = do
  finalizeImmStmts (connImmStmts conn)
  closeChecked (conImmDb conn)
  finalizeVolStmts (connVolStmts conn)
  closeChecked (conVolDb conn)

-- | @sqlite3_close@ refuses -- and would silently leak the connection -- if
-- statements are still open; a caller that failed to finalize must hear it.
closeChecked :: HasCallStack => DB.Database -> IO ()
closeChecked db =
  DB.close db >>= \case
    Left err ->
      throwIO $
        LeiosDbException
          LeiosDbFailure
            { ldfErrorMessage = "failed to close the connection: " <> show err
            , ldfCallStack = GHC.Stack.prettyCallStack GHC.Stack.callStack
            }
    Right () -> pure ()

-- | A writing transaction: @BEGIN IMMEDIATE@, taking the write lock up front.
--
-- A deferred transaction that reads before it writes has to upgrade its lock,
-- and in WAL mode that upgrade fails with @SQLITE_BUSY_SNAPSHOT@ whenever
-- another connection committed in between. That status is not serviced by the
-- busy handler and cannot be retried at the statement level, because the
-- transaction's snapshot is stale for good: the only remedy is to roll back and
-- start over. Taking the lock at BEGIN removes the upgrade, so contention
-- surfaces here instead, where waiting actually resolves it.
dbWithWriteTransaction :: HasCallStack => Conn -> IO a -> IO a
dbWithWriteTransaction Conn{conVolDb} = dbWithWriteTransactionRaw conVolDb
