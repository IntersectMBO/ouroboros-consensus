{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LeiosDemoDb.SQLite
  ( newLeiosDBSQLiteFromEnv
  , newLeiosDBSQLite
  , sqlSampleLeiosDBStats

    -- * SQL strings (re-exported for leiosdemo app)
  , sql_schema
  , sql_insert_eb
  , sql_insert_ebBody
  , sql_insert_tx
  ) where

import Cardano.Prelude (forM, forM_, traverse_, when)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTChan
  , dupTChan
  , newBroadcastTChan
  , writeTChan
  )
import Control.Exception (SomeException, throwIO)
import Control.Monad (forever, unless, void)
import Control.Monad.Class.MonadThrow (generalBracket)
import qualified Control.Monad.Class.MonadThrow as MonadThrow
import Control.Tracer (Tracer, traceWith)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.String (fromString)
import Database.SQLite3
  ( SQLOpenFlag (..)
  , SQLVFS (..)
  , open2
  )
import qualified Database.SQLite3.Direct as DB
import GHC.Stack (HasCallStack)
import qualified GHC.Stack
import LeiosDemoDb.Common
  ( CompletedEbs
  , LeiosDbConnection (..)
  , LeiosDbHandle (..)
  , LeiosEbNotification (..)
  )
import LeiosDemoDb.Trace (TraceLeiosDb (..))
import LeiosDemoException (LeiosDbException (..))
import LeiosDemoTypes
  ( BytesSize
  , EbHash (..)
  , LeiosEb
  , LeiosPoint (..)
  , TxHash (..)
  , leiosEbBodyItems
  , leiosEbBytesSize
  )
import LeiosUtils.CallTrace
  ( CallCtx
  , CallName
  , SomeJsonCallTrace (..)
  , callTraceSameThread
  , rootCallCtx
  )
import Ouroboros.Consensus.Util.IOLike (atomically)
import System.Directory (doesFileExist, getFileSize)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.Random (randomIO)

-- * Public API

-- | Create a new Leios database connection from environment variable.
-- This looks up the LEIOS_DB_PATH environment variable and opens the database.
newLeiosDBSQLiteFromEnv :: Tracer IO TraceLeiosDb -> IO (LeiosDbHandle IO)
newLeiosDBSQLiteFromEnv tracer = do
  dbPath <-
    lookupEnv "LEIOS_DB_PATH" >>= \case
      Nothing -> die "You must define the LEIOS_DB_PATH variable for this demo."
      Just x -> pure x
  newLeiosDBSQLite tracer dbPath

-- | Create a new Leios database using the SQLite implementation at given file
-- path.
--
-- Each call to 'open' on the returned handle creates a new SQLite connection.
-- Connections are not thread-safe and should not be shared across threads.
newLeiosDBSQLite :: Tracer IO TraceLeiosDb -> FilePath -> IO (LeiosDbHandle IO)
newLeiosDBSQLite tracer dbPath = do
  notificationChan <- atomically newBroadcastTChan
  -- start a thread to sample the sizes of the volatile LeiosDB partition
  startVolatileStatsSampler tracer dbPath
  gcRootCtx <- rootCallCtx "leiosdb-gc"
  copyRootCtx <- rootCallCtx "leiosdb-copy"
  noGCYetDoneRef <- newIORef True -- True until the first GC of this handle
  pure $
    LeiosDbHandle
      { subscribeEbNotifications =
          atomically (dupTChan notificationChan)
      , leiosDbGarbageCollect = sqlGarbageCollect tracer gcRootCtx dbPath noGCYetDoneRef
      , leiosDbMarkAsImmutable = sqlMarkAsImmutable tracer copyRootCtx dbPath
      , open = openSQLiteConnection tracer dbPath notificationChan
      }

-- | Implements 'leiosDbMarkAsImmutable': mark the EB's announcement row(s) as
-- immutable, in one database transaction.
sqlMarkAsImmutable ::
  HasCallStack =>
  Tracer IO TraceLeiosDb -> CallCtx IO -> FilePath -> LeiosPoint -> IO ()
sqlMarkAsImmutable tracer rootCtx dbPath point = do
  let leiosDbCallTrace = traceWith tracer . TraceLeiosDbCall . SomeJsonCallTrace
  (copiedEbs, copiedEbTxs, copiedTxs) <-
    callTraceSameThread leiosDbCallTrace rootCtx "sqlMarkAsImmutable" (show point.pointEbHash) $ \_ctx ->
      withMaintenanceConn dbPath $ \db ->
        dbWithTransaction db $ do
          -- TODO(geo2a): this is needed for statistics. Allow to turn it off to check if it affects performance
          alreadyImmutable <- withStmt db sql_copy_is_immutable $ \stmt -> do
            dbBindBlob stmt 1 ebHash
            (/= 0) <$> readSingleInt64 stmt
          -- here we actually mark the EB as immutable
          execWithBlob db sql_copy_flag_immutable ebHash
          -- get the number of EBs affected by the above
          nEbs <- DB.changes db
          -- TODO(geo2a): this is needed for statistics. Allow to turn it off to check if it affects performance
          (nEbTxs, nTxs) <-
            if alreadyImmutable || nEbs == 0
              then pure (0, 0)
              else do
                nEbTxs <- countWithBlob db sql_copy_count_ebTxs
                nTxs <- countWithBlob db sql_copy_count_new_immutable_txs
                pure (nEbTxs, nTxs)
          -- Same transaction as the flag, so the counters commit atomically
          -- with the row they count (see 'sql_copy_move_stats').
          -- TODO(geo2a): this is needed for statistics. Allow to turn it off to check if it affects performance
          when (nEbs > 0 || nEbTxs > 0 || nTxs > 0) $
            execWithInt64x3
              db
              sql_copy_move_stats
              (fromIntegral nEbs, fromIntegral nEbTxs, fromIntegral nTxs)
          pure (nEbs, nEbTxs, nTxs)
  traceWith tracer TraceLeiosDbCopiedToImmutable{copiedEbs, copiedEbTxs, copiedTxs}
 where
  ebHash = point.pointEbHash.ebHashBytes

  countWithBlob :: DB.Database -> String -> IO Int
  countWithBlob db sql =
    withStmt db sql $ \stmt -> do
      dbBindBlob stmt 1 ebHash
      fromIntegral <$> readSingleInt64 stmt

  execWithBlob :: HasCallStack => DB.Database -> String -> ByteString -> IO ()
  execWithBlob db sql blob =
    withStmt db sql $ \stmt -> do
      dbBindBlob stmt 1 blob
      dbStep1Safe stmt

-- | Implements 'leiosDbGarbageCollect': evict every volatile EB all of whose
-- announcements are older than the given slot, then flush the WAL.
sqlGarbageCollect ::
  HasCallStack =>
  Tracer IO TraceLeiosDb -> CallCtx IO -> FilePath -> IORef Bool -> SlotNo -> IO ()
sqlGarbageCollect tracer rootCtx dbPath noGCYetDoneRef gcSlot =
  gcSpan rootCtx "sqlGarbageCollect" (unSlotNo gcSlot) $ \gcCtx ->
    withMaintenanceConn dbPath $ \db -> do
      -- check if we're doing the first ever GC during this node's run
      -- and flip the flag if so
      -- TODO(geo2a): move this configuration out of the IORef.
      firstGc <- atomicModifyIORef' noGCYetDoneRef (\b -> (False, b))
      hasWork <- gcSpan gcCtx "noopGuard" () $ \_ ->
        withStmt db sql_gc_has_work $ \stmt -> do
          dbBindInt64 stmt 1 slot
          (/= 0) <$> readSingleInt64 stmt
      when (hasWork || firstGc) $ do
        (evictedEbs, evictedEbTxs, evictedTxs) <-
          gcSpan gcCtx "evictionTransaction" () $ \txnCtx ->
            dbWithTransaction db $ do
              nTxsFromFirstGC <-
                if firstGc
                  -- if the the first GC (for example, after a node restart),
                  -- run the expensive traversal
                  then gcSpan txnCtx "orphanTxsFullScan" () $ \_ ->
                    withStmt db sql_gc_orphan_txs_full_scan dbStep1Safe >> DB.changes db
                  else pure 0
              -- find txHashBytes that are only referenced by volatile EBs that are older
              -- than the garbage collection slot and stage them for GC
              gcSpan txnCtx "stageOrphanCandidates" () $ \_ ->
                withStmt db sql_gc_stage_orphan_candidates $ \stmt -> do
                  dbBindInt64 stmt 1 slot
                  dbStep1Safe stmt
              -- garbage collect rows of EbTxs
              nEbTxs <- gcSpan txnCtx "evictEbTxs" () $ \_ ->
                execWithInt64 db sql_gc_ebTxs slot >> DB.changes db
              -- garbage collect EBs
              nEbs <- gcSpan txnCtx "evictEbs" () $ \_ ->
                execWithInt64 db sql_gc_ebs slot >> DB.changes db
              -- finally, garbage collect txs that were staged before
              --
              -- TODO(geo2a): can we GC txs based on the data we get from GCing EBs?
              -- why is sql_gc_stage_orphan_candidates + sql_gc_orphan_txs is
              -- faster than sql_gc_orphan_txs_full_scan?
              nTxs <- gcSpan txnCtx "orphanTxs" () $ \_ ->
                withStmt db sql_gc_orphan_txs dbStep1Safe >> DB.changes db
              gcSpan txnCtx "clearCandidates" () $ \_ ->
                withStmt db sql_gc_clear_candidates dbStep1Safe
              let nTxsTotal = nTxs + nTxsFromFirstGC
              when (nEbs > 0 || nEbTxs > 0 || nTxsTotal > 0) $
                execWithInt64x3
                  db
                  sql_update_volatile_stats
                  ( negate (fromIntegral nEbs)
                  , negate (fromIntegral nEbTxs)
                  , negate (fromIntegral nTxsTotal)
                  )
              pure (nEbs, nEbTxs, nTxsTotal)
        traceWith tracer TraceLeiosDbEvicted{evictedEbs, evictedEbTxs, evictedTxs}
      gcSpan gcCtx "walCheckpoint" () $ \_ ->
        dbExec db "PRAGMA wal_checkpoint(TRUNCATE);"
 where
  slot = fromIntegral (unSlotNo gcSlot)

  gcSpan ::
    (Aeson.ToJSON arg, Aeson.ToJSON res) =>
    CallCtx IO -> CallName -> arg -> (CallCtx IO -> IO res) -> IO res
  gcSpan = callTraceSameThread (traceWith tracer . TraceLeiosDbCall . SomeJsonCallTrace)

  execWithInt64 :: HasCallStack => DB.Database -> String -> Int64 -> IO ()
  execWithInt64 db sql n =
    withStmt db sql $ \stmt -> do
      dbBindInt64 stmt 1 n
      dbStep1Safe stmt

-- | Open a strictly read-only connection, for sampling DB statistics.
--
-- Opening fails harmlessly if the database does not exist yet; the caller
-- swallows it and tries again on the next tick.
withReadOnlyConn :: HasCallStack => FilePath -> (DB.Database -> IO a) -> IO a
withReadOnlyConn dbPath =
  MonadThrow.bracket open (void . DB.close)
 where
  open = do
    db <- open2 (fromString dbPath) [SQLOpenReadOnly] SQLVFSDefault
    -- Only mmap_size: journal_mode and page_size need write access.
    dbExec db "pragma mmap_size = 268435500;"
    pure db

-- | LeiosDB volatile and immutable row counts plus the on-disk footprint.
--
-- The counts come from the incrementally-maintained 'leiosDbStats' table.
sqlSampleLeiosDBStats ::
  HasCallStack => FilePath -> IO (TraceLeiosDb, TraceLeiosDb)
sqlSampleLeiosDBStats dbPath =
  withReadOnlyConn dbPath $ \db -> do
    (volatileEbs, volatileEbTxs, volatileTxs) <-
      withStmt db sql_read_volatile_stats $ \stmt ->
        dbStepSafe stmt >>= \case
          DB.Row ->
            (,,)
              <$> (fromIntegral <$> DB.columnInt64 stmt 0)
              <*> (fromIntegral <$> DB.columnInt64 stmt 1)
              <*> (fromIntegral <$> DB.columnInt64 stmt 2)
          DB.Done -> error "sqlSampleLeiosDBStats: leiosDbStats row missing"
    (immutableEbs, immutableEbTxs, immutableTxs) <- readImmutableStats db
    pageCount <- pragmaInt64 db "page_count"
    pageSize <- pragmaInt64 db "page_size"
    walBytes <- fileSizeOr0 (dbPath <> "-wal")
    pure
      ( TraceLeiosDbVolatileStats
          { volatileEbs
          , volatileEbTxs
          , volatileTxs
          , dbFileBytes = fromIntegral pageCount * fromIntegral pageSize
          , walBytes
          }
      , TraceLeiosDbImmutableStats
          { immutableEbs
          , immutableEbTxs
          , immutableTxs
          }
      )
 where
  pragmaInt64 :: HasCallStack => DB.Database -> String -> IO Int64
  pragmaInt64 db name = withStmt db ("PRAGMA " <> name) readSingleInt64

  fileSizeOr0 :: FilePath -> IO Integer
  fileSizeOr0 path = do
    exists <- doesFileExist path
    if exists then getFileSize path else pure 0

-- | Fork a thread that runs 'sqlSampleLeiosDBStats' and traces
--   'TraceLeiosDbVolatileStats' and 'TraceLeiosDbImmutableStats' every 10
--   seconds.
startVolatileStatsSampler ::
  HasCallStack => Tracer IO TraceLeiosDb -> FilePath -> IO ()
startVolatileStatsSampler tracer dbPath =
  void $ forkIO $ forever $ do
    -- wait one sample window to side-step contention with starting the LeiosDB
    threadDelay tenSeconds
    -- swallow the exception here, it's OK to miss one stats sample, and the next
    -- one will open a fresh connection.
    MonadThrow.handle (\(_ :: SomeException) -> pure ()) $ do
      (volatileStats, immutableStats) <- sqlSampleLeiosDBStats dbPath
      traceWith tracer volatileStats
      traceWith tracer immutableStats
 where
  tenSeconds = 10_000_000

-- | Immutable-partition row counts, as @(ebs, ebTxs, txs)@.
--
-- This reads the one-row 'leiosDbStats' running totals that the certification
-- transaction maintains ('sql_copy_move_stats').
readImmutableStats :: HasCallStack => DB.Database -> IO (Int, Int, Int)
readImmutableStats db =
  withStmt db sql_read_immutable_stats $ \stmt ->
    dbStep stmt >>= \case
      DB.Row ->
        (,,)
          <$> (fromIntegral <$> DB.columnInt64 stmt 0)
          <*> (fromIntegral <$> DB.columnInt64 stmt 1)
          <*> (fromIntegral <$> DB.columnInt64 stmt 2)
      DB.Done -> error "readImmutableStats: immutableStats row missing"

-- | Step a statement that yields exactly one integer column.
readSingleInt64 :: HasCallStack => DB.Statement -> IO Int64
readSingleInt64 stmt =
  dbStep stmt >>= \case
    DB.Row -> DB.columnInt64 stmt 0
    DB.Done -> error "readSingleInt64: expected a row, got Done"

-- * Connection management

-- | Every prepared statement the connection needs, prepared once at open
-- time and finalised deterministically at 'close' time (before
-- 'sqlite3_close_v2'). Reused across all calls on this connection via
-- 'useStmt' (bind → step → reset).
--
-- Rationale: the previous per-call @dbWithPrepare@ pattern was safe under
-- bracket unwind for its OWN scope, but under the load of the proto-devnet
-- we hit a use-after-free of the @sqlite3@ conn struct (see coredump
-- analysis in @analysis-runs/bench-baseline.txt@). Preparing once and
-- finalising synchronously with close removes every code path that could
-- call 'sqlite3_finalize' on a statement whose connection has been
-- destroyed.
data Stmts = Stmts
  { stScanEbPoints :: !DB.Statement
  , stInsertEbPoint :: !DB.Statement
  , stLookupEbBody :: !DB.Statement
  , stInsertEbTxsRow :: !DB.Statement
  , stInitMissingCount :: !DB.Statement
  , stInsertTx :: !DB.Statement
  , stDecrMissingCount :: !DB.Statement
  , stFindCompleteEbs :: !DB.Statement
  , stMarkNotifiedEbs :: !DB.Statement
  , stMarkPointNotified :: !DB.Statement
  , stBatchRetrieveTxs :: !DB.Statement
  , stFilterMissingEbBodies :: !DB.Statement
  , stFilterMissingTxs :: !DB.Statement
  , stLookupEbClosure :: !DB.Statement
  , stScanCompleteEbsSince :: !DB.Statement
  , stUpdateVolatileStats :: !DB.Statement
  }

data Conn = Conn
  { connDb :: !DB.Database
  , connStmts :: !Stmts
  }

-- | Prepare every statement 'Stmts' names. Order is not observable.
prepareStmts :: DB.Database -> IO Stmts
prepareStmts db = do
  stScanEbPoints <- dbPrepare db (fromString sql_scan_ebs)
  stInsertEbPoint <- dbPrepare db (fromString sql_insert_eb)
  stLookupEbBody <- dbPrepare db (fromString sql_lookup_ebBodies)
  stInsertEbTxsRow <- dbPrepare db (fromString sql_insert_ebBody)
  stInitMissingCount <- dbPrepare db (fromString sql_init_missing_tx_count)
  stInsertTx <- dbPrepare db (fromString sql_insert_tx)
  stDecrMissingCount <- dbPrepare db (fromString sql_decrement_missing_tx_count)
  stFindCompleteEbs <- dbPrepare db (fromString sql_find_complete_ebs)
  stMarkNotifiedEbs <- dbPrepare db (fromString sql_mark_notified_ebs)
  stMarkPointNotified <- dbPrepare db (fromString sql_mark_point_notified)
  stBatchRetrieveTxs <- dbPrepare db (fromString sql_retrieve_from_ebTxs_json)
  stFilterMissingEbBodies <- dbPrepare db (fromString sql_filter_missing_eb_bodies_json)
  stFilterMissingTxs <- dbPrepare db (fromString sql_filter_missing_txs_json)
  stLookupEbClosure <- dbPrepare db (fromString sql_lookup_eb_closure)
  stScanCompleteEbsSince <- dbPrepare db (fromString sql_scan_complete_ebs_since)
  stUpdateVolatileStats <- dbPrepare db (fromString sql_update_volatile_stats)
  pure Stmts{..}

-- | Finalise every statement in 'Stmts'. Called from 'close' immediately
-- before 'sqlite3_close_v2', on the connection's owner thread.
finalizeStmts :: Stmts -> IO ()
finalizeStmts Stmts{..} = do
  dbFinalize stScanEbPoints
  dbFinalize stInsertEbPoint
  dbFinalize stLookupEbBody
  dbFinalize stInsertEbTxsRow
  dbFinalize stInitMissingCount
  dbFinalize stInsertTx
  dbFinalize stDecrMissingCount
  dbFinalize stFindCompleteEbs
  dbFinalize stMarkNotifiedEbs
  dbFinalize stMarkPointNotified
  dbFinalize stBatchRetrieveTxs
  dbFinalize stFilterMissingEbBodies
  dbFinalize stFilterMissingTxs
  dbFinalize stLookupEbClosure
  dbFinalize stScanCompleteEbsSince
  dbFinalize stUpdateVolatileStats

-- | Run an action on a pre-prepared statement and always @sqlite3_reset@
-- it afterwards, regardless of outcome. Reset uses raw 'DB.reset' (no
-- error re-throw) because SQLite reports the /previous/ step's error via
-- reset; we let the original exception propagate instead.
useStmt :: DB.Statement -> IO a -> IO a
useStmt stmt action =
  action `MonadThrow.finally` (void $ DB.reset stmt)

-- | Fold deltas into the volatile counters of 'leiosDbStats' via the
-- connection's prepared 'sql_update_volatile_stats'.
--
-- TODO(geo2a): refactor the tuple into a dedicated data type
bumpVolatileStats :: Conn -> (Int64, Int64, Int64) -> IO ()
bumpVolatileStats conn (dEbs, dEbTxs, dTxs) =
  unless (dEbs == 0 && dEbTxs == 0 && dTxs == 0) $ useStmt stmt $ do
    dbBindInt64 stmt 1 dEbs
    dbBindInt64 stmt 2 dEbTxs
    dbBindInt64 stmt 3 dTxs
    dbStep1 stmt
 where
  Conn{connStmts = Stmts{stUpdateVolatileStats = stmt}} = conn

openSQLiteConnection ::
  Tracer IO TraceLeiosDb ->
  FilePath ->
  StrictTChan IO LeiosEbNotification ->
  IO (LeiosDbConnection IO)
openSQLiteConnection tracer dbPath notificationChan = do
  db <- openRawConnection dbPath
  stmts <- prepareStmts db
  let conn = Conn{connDb = db, connStmts = stmts}
      notify = atomically . writeTChan notificationChan
  pure $
    LeiosDbConnection
      { close = finalizeStmts stmts >> void (DB.close db)
      , leiosDbScanEbPoints = sqlScanEbPoints conn
      , leiosDbScanCompleteEbClosuresNotOlderThanSlot = sqlScanCompleteEbPointsSince conn
      , leiosDbInsertEbPoint = sqlInsertEbPoint conn
      , leiosDbLookupEbBody = sqlLookupEbBody conn
      , leiosDbInsertEbBody = sqlInsertEbBody tracer conn notify
      , leiosDbInsertTxs = sqlInsertTxs tracer conn notify
      , leiosDbBatchRetrieveTxs = sqlBatchRetrieveTxs conn
      , leiosDbFilterMissingEbBodies = sqlFilterMissingEbBodies conn
      , leiosDbFilterMissingTxs = sqlFilterMissingTxs conn
      , leiosDbLookupEbClosure = sqlLookupEbClosure conn
      }

-- | Open an SQLite connection, setting the shared PRAGMAs plus the idempotent 'sql_schema'.
openRawConnection :: HasCallStack => FilePath -> IO DB.Database
openRawConnection dbPath = do
  db <- open2 (fromString dbPath) [SQLOpenReadWrite, SQLOpenCreate] SQLVFSDefault
  traverse_ (dbExec db) $
    [ "pragma journal_mode = WAL;"
    , "pragma synchronous = normal;"
    , "pragma page_size = 32768;"
    , "pragma mmap_size = 268435500;"
    ]
  -- we run the DDL unconditionally as it is idempotent
  dbExec db (fromString sql_schema)
  pure db

-- | Open a short-lived connection for maintenance operations
-- ('leiosDbMarkAsImmutable', 'leiosDbGarbageCollect') and close it on unwind
withMaintenanceConn :: HasCallStack => FilePath -> (DB.Database -> IO a) -> IO a
withMaintenanceConn dbPath =
  MonadThrow.bracket (openRawConnection dbPath) (void . DB.close)

-- | Prepare a statement, run an action on it, and finalise it on unwind
withStmt :: HasCallStack => DB.Database -> String -> (DB.Statement -> IO a) -> IO a
withStmt db sql =
  MonadThrow.bracket (dbPrepare db (fromString sql)) dbFinalize

-- | Run a maintenance statement that takes three INTEGER parameters and
-- returns no rows.
execWithInt64x3 ::
  HasCallStack => DB.Database -> String -> (Int64, Int64, Int64) -> IO ()
execWithInt64x3 db sql (n1, n2, n3) =
  withStmt db sql $ \stmt -> do
    dbBindInt64 stmt 1 n1
    dbBindInt64 stmt 2 n2
    dbBindInt64 stmt 3 n3
    dbStep1Safe stmt

-- * Top-level implementations

sqlScanEbPoints :: Conn -> IO [(SlotNo, EbHash)]
sqlScanEbPoints conn =
  dbWithTransaction db $ useStmt stmt $ loop []
 where
  Conn{connDb = db, connStmts = Stmts{stScanEbPoints = stmt}} = conn
  loop acc =
    dbStep stmt >>= \case
      DB.Done -> pure (reverse acc)
      DB.Row -> do
        slot <- SlotNo . fromIntegral <$> DB.columnInt64 stmt 0
        hash <- MkEbHash <$> DB.columnBlob stmt 1
        loop ((slot, hash) : acc)

sqlScanCompleteEbPointsSince :: Conn -> SlotNo -> IO [LeiosPoint]
sqlScanCompleteEbPointsSince conn sinceSlot =
  dbWithTransaction db $ useStmt stmt $ do
    dbBindInt64 stmt 1 (fromIntegral $ unSlotNo sinceSlot)
    loop []
 where
  Conn{connDb = db, connStmts = Stmts{stScanCompleteEbsSince = stmt}} = conn
  loop acc =
    dbStep stmt >>= \case
      DB.Done -> pure (reverse acc)
      DB.Row -> do
        slot <- SlotNo . fromIntegral <$> DB.columnInt64 stmt 0
        hash <- MkEbHash <$> DB.columnBlob stmt 1
        loop (MkLeiosPoint slot hash : acc)

sqlLookupEbBody :: Conn -> EbHash -> IO [(TxHash, BytesSize)]
sqlLookupEbBody conn ebHash =
  dbWithTransaction db $ useStmt stmt $ do
    dbBindBlob stmt 1 (let MkEbHash bytes = ebHash in bytes)
    loop []
 where
  Conn{connDb = db, connStmts = Stmts{stLookupEbBody = stmt}} = conn
  loop acc =
    dbStep stmt >>= \case
      DB.Done -> pure (reverse acc)
      DB.Row -> do
        txHash <- MkTxHash <$> DB.columnBlob stmt 0
        size <- fromIntegral <$> DB.columnInt64 stmt 1
        loop ((txHash, size) : acc)

sqlInsertEbPoint :: Conn -> LeiosPoint -> BytesSize -> IO ()
sqlInsertEbPoint conn point ebBytesSize =
  dbWithTransaction db $ useStmt stmt $ do
    dbBindInt64 stmt 1 (fromIntegral $ unSlotNo point.pointSlotNo)
    dbBindBlob stmt 2 point.pointEbHash.ebHashBytes
    dbBindInt64 stmt 3 (fromIntegral ebBytesSize)
    dbStep1 stmt
    inserted <- DB.changes db
    bumpVolatileStats conn (fromIntegral inserted, 0, 0)
 where
  Conn{connDb = db, connStmts = Stmts{stInsertEbPoint = stmt}} = conn

-- | Persist an EB body. The point MUST already be present (inserted
-- via 'sqlInsertEbPoint' on the announcement path).
sqlInsertEbBody ::
  Tracer IO TraceLeiosDb ->
  Conn ->
  (LeiosEbNotification -> IO ()) ->
  LeiosPoint ->
  LeiosEb ->
  IO CompletedEbs
sqlInsertEbBody tracer conn notify point eb = do
  when (null items) $
    error "leiosDbInsertEbBody: empty EB body (programmer error)"
  completedNow <- dbWithTransaction db $ do
    insertedNewRows <- forM items $ \(txOffset, txHash, txBytesSize) -> useStmt stInsertEbTxsRow $ do
      dbBindBlob stInsertEbTxsRow 1 point.pointEbHash.ebHashBytes
      dbBindInt64 stInsertEbTxsRow 2 (fromIntegral txOffset)
      dbBindBlob stInsertEbTxsRow 3 (let MkTxHash bytes = txHash in bytes)
      dbBindInt64 stInsertEbTxsRow 4 (fromIntegral txBytesSize)
      dbStepInsertOrTrace
        tracer
        "ebTxs"
        (show point.pointEbHash <> "@" <> show txOffset)
        stInsertEbTxsRow
    let nOfNewRows = length (filter id insertedNewRows)
    bumpVolatileStats conn (0, fromIntegral nOfNewRows, 0)
    -- Initialize missingTxCount and read the resulting value via
    -- @RETURNING missingTxCount@. Only /this/ point's row can have
    -- transitioned to 0 as a consequence of the insert above.
    missingCount <- useStmt stInitMissingCount $ do
      dbBindBlob stInitMissingCount 1 point.pointEbHash.ebHashBytes
      dbBindBlob stInitMissingCount 2 point.pointEbHash.ebHashBytes
      dbBindInt64 stInitMissingCount 3 (fromIntegral $ unSlotNo point.pointSlotNo)
      readReturningInt64 stInitMissingCount
    if missingCount == 0
      then do
        useStmt stMarkPointNotified $ do
          dbBindInt64 stMarkPointNotified 1 (fromIntegral $ unSlotNo point.pointSlotNo)
          dbBindBlob stMarkPointNotified 2 point.pointEbHash.ebHashBytes
          dbStep1 stMarkPointNotified
        pure [point]
      else pure []
  notify $ AcquiredEb point ebBytesSize
  forM_ completedNow $ \p -> notify (AcquiredEbTxs p)
  pure completedNow
 where
  items = leiosEbBodyItems eb
  ebBytesSize = leiosEbBytesSize eb
  Conn{connDb = db, connStmts} = conn
  Stmts
    { stInsertEbTxsRow
    , stInitMissingCount
    , stMarkPointNotified
    } = connStmts

  readReturningInt64 :: DB.Statement -> IO Int64
  readReturningInt64 stmt =
    dbStep stmt >>= \case
      DB.Done ->
        error "readReturningInt64: expected one row from RETURNING, got Done"
      DB.Row -> do
        n <- DB.columnInt64 stmt 0
        dbStep stmt >>= \case
          DB.Done -> pure n
          DB.Row -> error "readReturningInt64: expected exactly one row from RETURNING"

sqlInsertTxs ::
  Tracer IO TraceLeiosDb ->
  Conn ->
  (LeiosEbNotification -> IO ()) ->
  [(TxHash, ByteString)] ->
  IO CompletedEbs
sqlInsertTxs _tracer conn notify txs = do
  -- Skip txs already persisted in 'txs'. Under mempool backlog,
  -- successive forges (or overlapping peer EBs) re-present the same tx
  -- hashes; attempting the INSERT and catching a constraint violation
  -- still pays the bind + PK-lookup + reset cost per row.
  missing <- Set.fromList <$> sqlFilterMissingTxs conn (map fst txs)
  completed <- dbWithTransaction db $ do
    -- 'dbStepInsert' still handles the rare race where a concurrent
    -- writer inserted the same hash between the filter above and the
    -- INSERT below.
    insertedRows <- forM (novel missing) $ \(txHash, txBytes) -> do
      let txBytesSize = fromIntegral $ BS.length txBytes
          txHashBytes = let MkTxHash bytes = txHash in bytes
      inserted <- useStmt stInsertTx $ do
        dbBindBlob stInsertTx 1 txHashBytes
        dbBindBlob stInsertTx 2 txBytes
        dbBindInt64 stInsertTx 3 txBytesSize
        dbStepInsert stInsertTx
      when inserted $ useStmt stDecrMissingCount $ do
        dbBindBlob stDecrMissingCount 1 txHashBytes
        dbStep1 stDecrMissingCount
      pure inserted
    let nOfNewRows = length (filter id insertedRows)
    bumpVolatileStats conn (0, 0, fromIntegral nOfNewRows)
    -- Find newly-complete EBs (missingTxCount reached 0)
    completed <- useStmt stFindCompleteEbs $ do
      let loop acc =
            dbStep stFindCompleteEbs >>= \case
              DB.Done -> pure (reverse acc)
              DB.Row -> do
                ebHash <- MkEbHash <$> DB.columnBlob stFindCompleteEbs 0
                slot <- SlotNo . fromIntegral <$> DB.columnInt64 stFindCompleteEbs 1
                loop (MkLeiosPoint slot ebHash : acc)
      loop []
    -- Mark them as notified so they are not found again
    useStmt stMarkNotifiedEbs $ dbStep1 stMarkNotifiedEbs
    pure completed
  -- Emit a closure-completion notification for each completed EB
  forM_ completed $ \point -> notify (AcquiredEbTxs point)
  pure completed
 where
  Conn{connDb = db, connStmts} = conn
  Stmts{stInsertTx, stDecrMissingCount, stFindCompleteEbs, stMarkNotifiedEbs} = connStmts
  novel missing = filter (\(h, _) -> h `Set.member` missing) txs

-- | Retrieve tx bytes for a batch of @(ebHash, txOffset)@ points. Passes
-- the offsets list as a JSON int array bound to a single parameter;
-- SQLite's 'json_each' virtual table joins it against 'ebTxs' + 'txs'.
--
-- No temp tables, no attached databases, no per-item INSERT round-trips.
-- Works on strictly read-only connections.
sqlBatchRetrieveTxs ::
  Conn ->
  EbHash ->
  [Int] ->
  IO [(Int, TxHash, Maybe ByteString)]
sqlBatchRetrieveTxs conn ebHash offsets =
  dbWithTransaction db $ useStmt stmt $ do
    dbBindBlob stmt 1 (let MkEbHash bytes = ebHash in bytes)
    dbBindUtf8 stmt 2 (jsonIntArray offsets)
    loop []
 where
  Conn{connDb = db, connStmts = Stmts{stBatchRetrieveTxs = stmt}} = conn
  loop acc =
    dbStep stmt >>= \case
      DB.Done -> pure (reverse acc)
      DB.Row -> do
        offset <- fromIntegral <$> DB.columnInt64 stmt 0
        txHash <- MkTxHash <$> DB.columnBlob stmt 1
        -- Column 2 is from LEFT JOIN, NULL if tx not in txs table
        txBytes <- DB.columnBlob stmt 2
        let mbTxBytes = if txBytes == mempty then Nothing else Just txBytes
        loop ((offset, txHash, mbTxBytes) : acc)

-- | Batch-filter EB points against @ebTxs@. Passes ebHashes as a JSON
-- array of hex strings; SQL decodes with @unhex()@ so index lookups on
-- @ebTxs.ebHashBytes@ still fire.
sqlFilterMissingEbBodies :: Conn -> [LeiosPoint] -> IO [LeiosPoint]
sqlFilterMissingEbBodies conn points =
  dbWithTransaction db $ useStmt stmt $ do
    dbBindUtf8 stmt 1 (jsonHexArray (map ebHashBytes (Map.keys pointsByHash)))
    loop []
 where
  Conn{connDb = db, connStmts = Stmts{stFilterMissingEbBodies = stmt}} = conn
  pointsByHash = Map.fromList [(p.pointEbHash, p) | p <- points]
  loop acc =
    dbStep stmt >>= \case
      DB.Done -> pure (reverse acc)
      DB.Row -> do
        ebHash <- MkEbHash <$> DB.columnBlob stmt 0
        case Map.lookup ebHash pointsByHash of
          Just p -> loop (p : acc)
          Nothing -> loop acc

-- | Batch-filter tx hashes against @txs@. Same idiom as
-- 'sqlFilterMissingEbBodies'.
sqlFilterMissingTxs :: Conn -> [TxHash] -> IO [TxHash]
sqlFilterMissingTxs conn txHashes =
  dbWithTransaction db $ useStmt stmt $ do
    dbBindUtf8 stmt 1 (jsonHexArray [b | MkTxHash b <- txHashes])
    loop []
 where
  Conn{connDb = db, connStmts = Stmts{stFilterMissingTxs = stmt}} = conn
  loop acc =
    dbStep stmt >>= \case
      DB.Done -> pure (reverse acc)
      DB.Row -> do
        txHash <- MkTxHash <$> DB.columnBlob stmt 0
        loop (txHash : acc)

-- | Build a JSON array of hex-encoded blobs: @["aabb...","1234...",...]@.
-- Consumed on the SQL side via @json_each(?)@ + @unhex(je.value)@.
jsonHexArray :: [ByteString] -> ByteString
jsonHexArray xs =
  BSL.toStrict . BB.toLazyByteString $
    BB.char7 '[' <> commaSep (map hexElem xs) <> BB.char7 ']'
 where
  hexElem b = BB.char7 '"' <> BB.byteStringHex b <> BB.char7 '"'
  commaSep = mconcat . intersperseB (BB.char7 ',')
  intersperseB _ [] = []
  intersperseB _ [x] = [x]
  intersperseB s (x : rest) = x : s : intersperseB s rest

-- | Build a JSON array of integers: @[1,2,3,...]@. Same consumer pattern
-- as 'jsonHexArray' (values are already ints, so no decoding step).
jsonIntArray :: [Int] -> ByteString
jsonIntArray xs =
  BSL.toStrict . BB.toLazyByteString $
    BB.char7 '[' <> commaSep (map BB.intDec xs) <> BB.char7 ']'
 where
  commaSep = mconcat . intersperseB (BB.char7 ',')
  intersperseB _ [] = []
  intersperseB _ [x] = [x]
  intersperseB s (x : rest) = x : s : intersperseB s rest

sqlLookupEbClosure :: Conn -> EbHash -> IO (Maybe [(TxHash, ByteString)])
sqlLookupEbClosure conn ebHash =
  dbWithTransaction db $ useStmt stmt $ do
    dbBindBlob stmt 1 (ebHashBytes ebHash)
    -- FIXME(bladyjoker): This should have a SlotNo as the second part of the key
    loop []
 where
  Conn{connDb = db, connStmts = Stmts{stLookupEbClosure = stmt}} = conn
  loop acc =
    dbStep stmt >>= \case
      DB.Done ->
        -- No rows means the EB body hasn't been downloaded yet
        if null acc then pure Nothing else pure $ Just (reverse acc)
      DB.Row -> do
        txHash <- MkTxHash <$> DB.columnBlob stmt 0
        txBytes :: ByteString <- DB.columnBlob stmt 1
        if txBytes == mempty
          then return Nothing
          else loop ((txHash, txBytes) : acc)

-- * SQL strings

-- | Schema for the Leios database
--
--   - 'ebs' holds one row per announced @(slot, hash)@. @immutable = 1@ marks
--     every announcement of an EB the immutable chain references;
--   - 'ebTxs' and 'txs' hold the bodies and tx bytes, keyed by EB hash.
--   - 'leiosDbStats' holds the statistics on volatile and immutable EBs and transiting.
sql_schema :: String
sql_schema =
  unlines
    [ "CREATE TABLE IF NOT EXISTS ebs ("
    , "  ebSlot INTEGER NOT NULL,"
    , "  ebHashBytes BLOB NOT NULL,"
    , "  ebBytesSize INTEGER NOT NULL,"
    , -- NULL = body not downloaded, >0 = txs missing, 0 = just completed, <0 = notified
      "  missingTxCount INTEGER,"
    , -- 1 = the immutable chain references this EB
      "  immutable INTEGER NOT NULL DEFAULT 0,"
    , "  PRIMARY KEY (ebSlot, ebHashBytes)"
    , ");"
    , "CREATE INDEX IF NOT EXISTS idx_ebs_ebHashBytes ON ebs(ebHashBytes);"
    , -- Index on the volatile EBs only
      "CREATE INDEX IF NOT EXISTS idx_ebs_volatile_slot ON ebs(ebSlot) WHERE immutable = 0;"
    , "CREATE TABLE IF NOT EXISTS ebTxs ("
    , "  ebHashBytes BLOB NOT NULL,"
    , "  txOffset INTEGER NOT NULL,"
    , "  txHashBytes BLOB NOT NULL,"
    , "  txBytesSize INTEGER NOT NULL,"
    , "  PRIMARY KEY (ebHashBytes, txOffset)"
    , ");"
    , "CREATE INDEX IF NOT EXISTS idx_ebTxs_txHashBytes ON ebTxs(txHashBytes);"
    , "CREATE TABLE IF NOT EXISTS txs ("
    , "  txHashBytes BLOB NOT NULL PRIMARY KEY,"
    , "  txBytes BLOB NOT NULL,"
    , "  txBytesSize INTEGER NOT NULL"
    , ");"
    , -- Running per-partition row counts, maintained for observability.
      "CREATE TABLE IF NOT EXISTS leiosDbStats ("
    , "  id INTEGER PRIMARY KEY CHECK (id = 0),"
    , "  volatileEbs INTEGER NOT NULL,"
    , "  volatileEbTxs INTEGER NOT NULL,"
    , "  volatileTxs INTEGER NOT NULL,"
    , "  immutableEbs INTEGER NOT NULL,"
    , "  immutableEbTxs INTEGER NOT NULL,"
    , "  immutableTxs INTEGER NOT NULL"
    , ");"
    , -- TODO(geo2a): why exactly is this needed? Initialize the stats with the existing data on node restart?
      "INSERT INTO leiosDbStats (id, volatileEbs, volatileEbTxs, volatileTxs, immutableEbs, immutableEbTxs, immutableTxs)"
    , "SELECT 0,"
    , "  (SELECT COUNT(*) FROM ebs WHERE immutable = 0),"
    , "  (SELECT COUNT(*) FROM ebTxs e WHERE NOT EXISTS"
    , "    (SELECT 1 FROM ebs b WHERE b.ebHashBytes = e.ebHashBytes AND b.immutable = 1)),"
    , "  (SELECT COUNT(*) FROM txs t WHERE NOT EXISTS"
    , "    (SELECT 1 FROM ebTxs e JOIN ebs b ON b.ebHashBytes = e.ebHashBytes AND b.immutable = 1"
    , "     WHERE e.txHashBytes = t.txHashBytes)),"
    , "  (SELECT COUNT(*) FROM ebs WHERE immutable = 1),"
    , "  (SELECT COUNT(*) FROM ebTxs e WHERE EXISTS"
    , "    (SELECT 1 FROM ebs b WHERE b.ebHashBytes = e.ebHashBytes AND b.immutable = 1)),"
    , "  (SELECT COUNT(*) FROM txs t WHERE EXISTS"
    , "    (SELECT 1 FROM ebTxs e JOIN ebs b ON b.ebHashBytes = e.ebHashBytes AND b.immutable = 1"
    , "     WHERE e.txHashBytes = t.txHashBytes))"
    , "WHERE NOT EXISTS (SELECT 1 FROM leiosDbStats WHERE id = 0);"
    , -- Garbage collection candidates.
      "CREATE TABLE IF NOT EXISTS gcTxCandidates ("
    , "  txHashBytes BLOB NOT NULL PRIMARY KEY"
    , ");"
    ]

-- | The 'ebTxs' rows of one EB hash. @?1@ is the ebHash blob.
sql_eb_txs :: String
sql_eb_txs =
  "SELECT txOffset, txHashBytes, txBytesSize FROM ebTxs\n\
  \WHERE ebHashBytes = ?1\n"

-- | A tx's bytes. Yields NULL when the tx is absent.
sql_tx_bytes :: String -> String
sql_tx_bytes txHashExpr =
  "(SELECT txBytes FROM txs WHERE txHashBytes = " <> txHashExpr <> ")"

-- | All EB announcements
-- TODO(geo2a): do we really need all of them, or only volatile ones?
sql_scan_ebs :: String
sql_scan_ebs =
  "SELECT ebSlot, ebHashBytes FROM ebs ORDER BY ebSlot ASC\n"

-- | For 'sqlScanCompleteEbPointsSince'
--
-- The two conditions are decoupled across rows: the same EB hash can have
-- several @(ebSlot, ebHashBytes)@ rows (one per announcer slot), and
-- 'missingTxCount' is maintained per row on body insert but per hash on tx
-- arrival, so the /complete/ row and the /recent/ row can differ. Requiring
-- both on a single row would wrongly drop a complete EB re-announced recently
-- (its recent row never got a body insert, so its @missingTxCount@ is still
-- NULL), leaving its cert-RB parked forever. Hence: keep a hash that has
-- /any/ complete row and /any/ row at @ebSlot >= ?@.
--
-- TODO(geo2a): do we really need all of them, or only volatile ones?
sql_scan_complete_ebs_since :: String
sql_scan_complete_ebs_since =
  "SELECT MAX(ebSlot), ebHashBytes FROM ebs\n\
  \WHERE ebSlot >= ?\n\
  \  AND ebHashBytes IN\n\
  \      (SELECT ebHashBytes FROM ebs WHERE missingTxCount IS NOT NULL AND missingTxCount <= 0)\n\
  \GROUP BY ebHashBytes\n\
  \"

sql_insert_eb :: String
sql_insert_eb =
  "INSERT OR IGNORE INTO ebs (ebSlot, ebHashBytes, ebBytesSize) VALUES (?, ?, ?)"

sql_lookup_ebBodies :: String
sql_lookup_ebBodies =
  "SELECT txHashBytes, txBytesSize FROM (\n"
    <> sql_eb_txs
    <> ")\n\
       \ORDER BY txOffset ASC\n"

sql_insert_ebBody :: String
sql_insert_ebBody =
  "INSERT INTO ebTxs (ebHashBytes, txOffset, txHashBytes, txBytesSize) VALUES (?, ?, ?, ?)\n\
  \"

sql_insert_tx :: String
sql_insert_tx =
  "INSERT INTO txs (txHashBytes, txBytes, txBytesSize) VALUES (?, ?, ?)\n\
  \"

-- | Batch-filter ebHashes via JSON1. Parameter is a JSON array of hex
-- strings; 'unhex(je.value)' decodes back into a BLOB comparable against
-- the indexed @ebTxs.ebHashBytes@ column.
sql_filter_missing_eb_bodies_json :: String
sql_filter_missing_eb_bodies_json =
  "SELECT unhex(je.value) FROM json_each(?) je\n\
  \WHERE NOT EXISTS (SELECT 1 FROM ebTxs e WHERE e.ebHashBytes = unhex(je.value))\n\
  \"

-- | Batch-filter txHashes via JSON1. Same shape as
-- 'sql_filter_missing_eb_bodies_json'.
sql_filter_missing_txs_json :: String
sql_filter_missing_txs_json =
  "SELECT unhex(je.value) FROM json_each(?) je\n\
  \WHERE NOT EXISTS (SELECT 1 FROM txs t WHERE t.txHashBytes = unhex(je.value))\n\
  \"

-- | Find all volatile EBs that are now complete (missingTxCount reached 0).
sql_find_complete_ebs :: String
sql_find_complete_ebs =
  "SELECT ebHashBytes, ebSlot FROM ebs WHERE immutable = 0 AND missingTxCount = 0"

-- | Mark complete volatile EBs as notified so they are not found again by
-- 'sql_find_complete_ebs'. Uses -1 as a sentinel for "already notified".
sql_mark_notified_ebs :: String
sql_mark_notified_ebs =
  "UPDATE ebs SET missingTxCount = -1 WHERE immutable = 0 AND missingTxCount = 0"

-- | Decrement missingTxCount for all volatile EBs referencing the given txHash.
-- Parameter 1: txHashBytes
sql_decrement_missing_tx_count :: String
sql_decrement_missing_tx_count =
  "UPDATE ebs SET missingTxCount = missingTxCount - 1\n\
  \WHERE immutable = 0\n\
  \  AND ebHashBytes IN (SELECT ebHashBytes FROM ebTxs WHERE txHashBytes = ?)\n\
  \"

-- | Initialize missingTxCount after EB body is inserted, returning the
-- resulting count. Counts ebTxs entries that don't yet have a corresponding
-- tx in the txs table. The RETURNING clause lets the caller detect the
-- special case @missingTxCount = 0@ (all referenced txs already present) with
-- a PK lookup on the row that was just touched, instead of a full-table
-- scan via 'sql_find_complete_ebs'.
--
-- Parameters: 1 = ebHashBytes, 2 = ebHashBytes, 3 = ebSlot
sql_init_missing_tx_count :: String
sql_init_missing_tx_count =
  "UPDATE ebs SET missingTxCount = (\n\
  \    SELECT COUNT(*) FROM ebTxs e\n\
  \    WHERE e.ebHashBytes = ?\n\
  \      AND NOT EXISTS (SELECT 1 FROM txs t WHERE t.txHashBytes = e.txHashBytes)\n\
  \) WHERE ebHashBytes = ? AND ebSlot = ?\n\
  \RETURNING missingTxCount\n\
  \"

-- | Mark a specific EB as notified (@missingTxCount = -1@). PK-scoped
-- variant of 'sql_mark_notified_ebs'; used by 'sqlInsertEbBody' when the
-- body's arrival is what completed the closure.
--
-- Parameters: 1 = ebSlot, 2 = ebHashBytes
sql_mark_point_notified :: String
sql_mark_point_notified =
  "UPDATE ebs SET missingTxCount = -1 WHERE ebSlot = ? AND ebHashBytes = ?"

-- | Batch retrieve of tx bytes for a batch of @(ebHash, offset)@ points.
-- @?1@ is the ebHash blob (all offsets belong to the same EB); @?2@ is a
-- JSON int array of offsets.
--
-- The per-hash filter lives inside 'sql_eb_txs', so the join reads as if it
-- were on @txOffset@ alone. It still costs one index seek per offset via the
-- full @(ebHashBytes, txOffset)@ PK.
sql_retrieve_from_ebTxs_json :: String
sql_retrieve_from_ebTxs_json =
  "SELECT je.value, e.txHashBytes,\n       "
    <> sql_tx_bytes "e.txHashBytes"
    <> "\nFROM json_each(?2) je\n\
       \JOIN (\n"
    <> sql_eb_txs
    <> ") e ON e.txOffset = je.value\n\
       \ORDER BY je.value ASC\n\
       \"

sql_lookup_eb_closure :: String
sql_lookup_eb_closure =
  "SELECT ebTx.txHashBytes,\n       "
    <> sql_tx_bytes "ebTx.txHashBytes"
    <> "\nFROM (\n"
    <> sql_eb_txs
    <> ") AS ebTx\n\
       \ORDER BY ebTx.txOffset ASC\n\
       \"

-- ** Marking an EB as immutable

-- | Whether the EB hash (parameter 1) already has an immutable announcement.
sql_copy_is_immutable :: String
sql_copy_is_immutable =
  "SELECT EXISTS (SELECT 1 FROM ebs WHERE ebHashBytes = ?1 AND immutable = 1)\n"

-- | Mark an EB as immutable
-- TODO(geo2a): do we need to check 'immutable = 0'? Probably we could skip that.
sql_copy_flag_immutable :: String
sql_copy_flag_immutable =
  "UPDATE ebs SET immutable = 1 WHERE ebHashBytes = ? AND immutable = 0\n"

sql_copy_count_ebTxs :: String
sql_copy_count_ebTxs =
  "SELECT COUNT(*) FROM ebTxs WHERE ebHashBytes = ?1\n"

-- | How many of the certified EB's txs become immutable /now/: txs referenced
-- by this EB and by no other already-immutable EB.
sql_copy_count_new_immutable_txs :: String
sql_copy_count_new_immutable_txs =
  "SELECT COUNT(DISTINCT e.txHashBytes) FROM ebTxs e\n\
  \WHERE e.ebHashBytes = ?1\n\
  \  AND NOT EXISTS\n\
  \    (SELECT 1 FROM ebTxs e2\n\
  \     JOIN ebs b ON b.ebHashBytes = e2.ebHashBytes AND b.immutable = 1\n\
  \     WHERE e2.txHashBytes = e.txHashBytes AND e2.ebHashBytes <> ?1)\n"

-- | Move this EBs row counts from the volatile side of
-- 'leiosDbStats' to the immutable side.
-- TODO(geo2a): this is getting a little too complicated. Consider simplifying the stats.
sql_copy_move_stats :: String
sql_copy_move_stats =
  "UPDATE leiosDbStats SET\n\
  \  volatileEbs = volatileEbs - ?1, immutableEbs = immutableEbs + ?1,\n\
  \  volatileEbTxs = volatileEbTxs - ?2, immutableEbTxs = immutableEbTxs + ?2,\n\
  \  volatileTxs = volatileTxs - ?3, immutableTxs = immutableTxs + ?3\n\
  \WHERE id = 0\n"

-- | Fold deltas into the volatile counters of
-- 'leiosDbStats'.
sql_update_volatile_stats :: String
sql_update_volatile_stats =
  "UPDATE leiosDbStats SET\n\
  \  volatileEbs = volatileEbs + ?1,\n\
  \  volatileEbTxs = volatileEbTxs + ?2,\n\
  \  volatileTxs = volatileTxs + ?3\n\
  \WHERE id = 0\n"

-- | Read the running immutable-partition counts.
sql_read_immutable_stats :: String
sql_read_immutable_stats =
  "SELECT immutableEbs, immutableEbTxs, immutableTxs FROM leiosDbStats WHERE id = 0\n"

-- | Read the running volatile-partition counts.
sql_read_volatile_stats :: String
sql_read_volatile_stats =
  "SELECT volatileEbs, volatileEbTxs, volatileTxs FROM leiosDbStats WHERE id = 0\n"

-- ** Garbage collection of the volatile partition

-- | Whether a GC at slot @?1@ would evict anything.
sql_gc_has_work :: String
sql_gc_has_work =
  "SELECT EXISTS (SELECT 1 FROM ebs WHERE immutable = 0 AND ebSlot < ?1)\n\
  \"

-- | The evictable EB hashes: every announcement is volatile and older than
-- the GC slot @?1@.
sql_gc_stale_hashes :: String
sql_gc_stale_hashes =
  "SELECT DISTINCT cand.ebHashBytes FROM ebs cand\n\
  \     WHERE cand.immutable = 0 AND cand.ebSlot < ?1\n\
  \       AND NOT EXISTS\n\
  \         (SELECT 1 FROM ebs\n\
  \          WHERE ebs.ebHashBytes = cand.ebHashBytes\n\
  \            AND (ebs.immutable = 1 OR ebs.ebSlot >= ?1))\n"

-- | Stage the txs of the stale EB hashes as orphan candidates. @?1@ is the GC
-- slot.
sql_gc_stage_orphan_candidates :: String
sql_gc_stage_orphan_candidates =
  "INSERT OR IGNORE INTO gcTxCandidates (txHashBytes)\n\
  \  SELECT DISTINCT txHashBytes FROM ebTxs WHERE ebHashBytes IN\n\
  \    ("
    <> sql_gc_stale_hashes
    <> ")\n"

-- | Evict the body rows of the stale EB hashes. @?1@ is the GC slot.
sql_gc_ebTxs :: String
sql_gc_ebTxs =
  "DELETE FROM ebTxs WHERE ebHashBytes IN\n\
  \  ("
    <> sql_gc_stale_hashes
    <> ")\n"

-- | Evict volatile announcements older than the GC slot @?1@.
sql_gc_ebs :: String
sql_gc_ebs = "DELETE FROM ebs WHERE immutable = 0 AND ebSlot < ?\n"

-- | Reap staged candidate txs that no EB references any more.
sql_gc_orphan_txs :: String
sql_gc_orphan_txs =
  "DELETE FROM txs WHERE txHashBytes IN\n\
  \    (SELECT txHashBytes FROM gcTxCandidates)\n\
  \  AND NOT EXISTS\n\
  \    (SELECT 1 FROM ebTxs WHERE ebTxs.txHashBytes = txs.txHashBytes)\n\
  \"

-- | Full-scan variant of 'sql_gc_orphan_txs', run once per handle on the
-- first GC: reaps orphans the candidate scheme cannot see (e.g. from before
-- 'gcTxCandidates' existed).
sql_gc_orphan_txs_full_scan :: String
sql_gc_orphan_txs_full_scan =
  "DELETE FROM txs WHERE NOT EXISTS\n\
  \  (SELECT 1 FROM ebTxs WHERE ebTxs.txHashBytes = txs.txHashBytes)\n\
  \"

-- | Drop all staged candidates.
sql_gc_clear_candidates :: String
sql_gc_clear_candidates = "DELETE FROM gcTxCandidates\n"

-- * Low-level terminating SQLite functions

dbBindBlob :: HasCallStack => DB.Statement -> DB.ParamIndex -> ByteString -> IO ()
dbBindBlob q p v = withDieStmt q $ DB.bindBlob q p v

-- | Bind as TEXT. Needed for JSON1 payloads: 'json_each' interprets BLOB
-- arguments as JSONB (SQLite ≥ 3.45), our payload is ASCII JSON.
dbBindUtf8 :: HasCallStack => DB.Statement -> DB.ParamIndex -> ByteString -> IO ()
dbBindUtf8 q p v = withDieStmt q $ DB.bindText q p (DB.Utf8 v)

dbBindInt64 :: HasCallStack => DB.Statement -> DB.ParamIndex -> Int64 -> IO ()
dbBindInt64 q p v = withDieStmt q $ DB.bindInt64 q p v

dbExec :: HasCallStack => DB.Database -> DB.Utf8 -> IO ()
dbExec db q = withDie db $ fmap (first fst) $ DB.exec db q

dbFinalize :: HasCallStack => DB.Statement -> IO ()
dbFinalize q = withDieStmt q $ DB.finalize q

dbPrepare :: HasCallStack => DB.Database -> DB.Utf8 -> IO DB.Statement
dbPrepare db q = withDieJust db $ DB.prepare db q

-- TODO: alternative: bind and use https://www.sqlite.org/c3ref/busy_handler.html
dbWithTransaction :: HasCallStack => DB.Database -> IO a -> IO a
dbWithTransaction db k =
  do
    fmap fst
    $ generalBracket
      (dbExec db (fromString "BEGIN"))
      ( \() -> \case
          MonadThrow.ExitCaseSuccess _ -> dbExec db (fromString "COMMIT")
          MonadThrow.ExitCaseException _ -> dbExec db (fromString "ROLLBACK")
          MonadThrow.ExitCaseAbort -> dbExec db (fromString "ROLLBACK")
      )
      (\() -> k)

dbStep :: HasCallStack => DB.Statement -> IO DB.StepResult
dbStep stmt = withDieStmt stmt $ DB.stepNoCB stmt

dbStep1 :: HasCallStack => DB.Statement -> IO ()
dbStep1 stmt = withDieDoneStmt stmt $ DB.stepNoCB stmt

-- | 'dbStep' through the safe FFI binding of @sqlite3_step@ ('DB.step'
-- rather than 'DB.stepNoCB').
--
-- Safe FFI calls carry significantly more overhead, but run in a separate
-- GHC RTS capability, meaning they do not block the capability that started them.
--
-- Use for long-running FFI calls where the faster unsafe FFI does not win much.
dbStepSafe :: HasCallStack => DB.Statement -> IO DB.StepResult
dbStepSafe stmt = withDieStmt stmt $ DB.step stmt

-- | 'dbStep1' through the safe FFI binding; see 'dbStepSafe'.
dbStep1Safe :: HasCallStack => DB.Statement -> IO ()
dbStep1Safe stmt = withDieDoneStmt stmt $ DB.step stmt

-- | Like 'dbStep1' but returns 'True' on success and 'False' on constraint
-- violation (duplicate key). Other errors are thrown as usual.
dbStepInsert :: HasCallStack => DB.Statement -> IO Bool
dbStepInsert stmt =
  go maxBusyRetries (DB.stepNoCB stmt)
 where
  go 0 io =
    io >>= \case
      Left e -> DB.getStatementDatabase stmt >>= \db -> throwDbException db e
      Right DB.Done -> pure True
      Right DB.Row -> error "dbStepInsert: unexpected Row result"
  go n io =
    io >>= \case
      Left DB.ErrorBusy -> do
        let retryNum = maxBusyRetries - n
            baseDelay = 100
        jitter <- (`mod` baseDelay) <$> randomIO
        threadDelay (baseDelay * retryNum + jitter)
        go (n - 1) io
      Left DB.ErrorConstraint -> pure False
      Left e -> DB.getStatementDatabase stmt >>= \db -> throwDbException db e
      Right DB.Done -> pure True
      Right DB.Row -> error "dbStepInsert: unexpected Row result"

-- | Step an INSERT statement, absorbing UNIQUE/PRIMARY KEY violations and
-- emitting a 'TraceLeiosDbInsertCollision' for each one. The caller supplies a
-- table label and a key description for the trace. Returns whether the row
-- was actually inserted.
--
-- After a constraint error, sqlite3_reset reports the same error code; the
-- normal 'dbReset' would re-throw it, so we use raw 'DB.reset' and discard the
-- return value. This also leaves the statement in a clean state for the
-- subsequent bracket-time 'dbFinalize' to succeed.
dbStepInsertOrTrace ::
  HasCallStack =>
  Tracer IO TraceLeiosDb ->
  String ->
  String ->
  DB.Statement ->
  IO Bool
dbStepInsertOrTrace tracer table key stmt = do
  isNew <- dbStepInsert stmt
  _ <- DB.reset stmt
  unless isNew $
    traceWith tracer (TraceLeiosDbInsertCollision table key)
  pure isNew

-- ** Error "handling"

maxBusyRetries :: Int
maxBusyRetries = 10000

-- | Execute a database action that may return an error. If the error is
-- 'DB.ErrorBusy', retry up to 'maxBusyRetries' times with linear backoff and
-- jitter. Otherwise and after exhausting retries, throws a 'LeiosDbException'
-- with the error message from the database.
withDie :: HasCallStack => DB.Database -> IO (Either DB.Error a) -> IO a
withDie db = go maxBusyRetries
 where
  go 0 io =
    io >>= \case
      Left e -> throwDbException db e
      Right x -> pure x
  go n io =
    io >>= \case
      -- TODO: Expose and use sqlite3_busy_timeout instead
      Left DB.ErrorBusy -> do
        -- Linear backoff with jitter: base delay increases each retry, plus
        -- random jitter up to the base delay, with a 0.1ms floor.
        let retryNum = maxBusyRetries - n
            baseDelay = 100
        jitter <- (`mod` baseDelay) <$> randomIO
        threadDelay (baseDelay * retryNum + jitter)
        go (n - 1) io
      Left e -> throwDbException db e
      Right x -> pure x

withDieStmt :: HasCallStack => DB.Statement -> IO (Either DB.Error a) -> IO a
withDieStmt stmt io = do
  db <- DB.getStatementDatabase stmt
  withDie db io

withDieJust :: HasCallStack => DB.Database -> IO (Either DB.Error (Maybe a)) -> IO a
withDieJust db io =
  withDie db io >>= \case
    Nothing ->
      throwIO $
        LeiosDbException
          { errorMessage = "unexpected Nothing"
          , callStack = GHC.Stack.prettyCallStack GHC.Stack.callStack
          }
    Just x -> pure x

withDieDoneStmt :: HasCallStack => DB.Statement -> IO (Either DB.Error DB.StepResult) -> IO ()
withDieDoneStmt stmt io = do
  db <- DB.getStatementDatabase stmt
  withDie db io >>= \case
    DB.Row ->
      throwIO $
        LeiosDbException
          { errorMessage = "unexpected Row"
          , callStack = GHC.Stack.prettyCallStack GHC.Stack.callStack
          }
    DB.Done -> pure ()

throwDbException :: HasCallStack => DB.Database -> DB.Error -> IO a
throwDbException db e = do
  reason <- DB.errmsg db
  throwIO $
    LeiosDbException
      { errorMessage = show e <> ": " <> show reason
      , callStack = GHC.Stack.prettyCallStack GHC.Stack.callStack
      }
