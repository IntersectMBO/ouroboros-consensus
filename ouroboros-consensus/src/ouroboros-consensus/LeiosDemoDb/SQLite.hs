{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LeiosDemoDb.SQLite
  ( newLeiosDBSQLiteFromEnv
  , newLeiosDBSQLite

    -- * SQL strings (re-exported for leiosdemo app)
  , sql_schema
  , sql_insert_eb
  , sql_insert_ebBody
  , sql_insert_tx
  ) where

import Cardano.Prelude (forM_, traverse_, when)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTChan
  , dupTChan
  , newBroadcastTChan
  , writeTChan
  )
import Control.Exception (throwIO)
import Control.Monad (unless, void)
import Control.Monad.Class.MonadThrow (generalBracket)
import qualified Control.Monad.Class.MonadThrow as MonadThrow
import Control.Tracer (Tracer, traceWith)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import qualified Data.Set as Set
import Data.String (fromString)
import Database.SQLite3
  ( SQLOpenFlag (..)
  , SQLVFS (..)
  , open2
  )
import qualified Database.SQLite3.Direct as DB
import GHC.Clock (getMonotonicTime)
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
import Ouroboros.Consensus.Util.IOLike (atomically)
import System.Directory (doesFileExist)
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
  pure $
    LeiosDbHandle
      { subscribeEbNotifications =
          atomically (dupTChan notificationChan)
      , -- No-op for now; see 'leiosDbGarbageCollect'. A real implementation
        -- would open a transient connection and evict the no-longer-needed rows.
        leiosDbGarbageCollect = \_slotNo -> pure ()
      , -- No-op for now; see 'leiosDbPromoteToImmutable'. A real implementation
        -- would copy the EB's body and closure rows into immutable storage.
        leiosDbPromoteToImmutable = \_point -> pure ()
      , open = openSQLiteConnection tracer dbPath notificationChan
      }

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
  , stFilterMissingTxs :: !DB.Statement
  , stLookupEbClosure :: !DB.Statement
  , stScanCompleteEbsSince :: !DB.Statement
  }

data Conn = Conn
  { connDb :: !DB.Database
  , connStmts :: !Stmts
  , connTracer :: !(Tracer IO TraceLeiosDb)
  -- ^ So the write path can report exhausting SQLite's own busy timeout.
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
  stFilterMissingTxs <- dbPrepare db (fromString sql_filter_missing_txs_json)
  stLookupEbClosure <- dbPrepare db (fromString sql_lookup_eb_closure)
  stScanCompleteEbsSince <- dbPrepare db (fromString sql_scan_complete_ebs_since)
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
  dbFinalize stFilterMissingTxs
  dbFinalize stLookupEbClosure
  dbFinalize stScanCompleteEbsSince

-- | Run an action on a pre-prepared statement and always @sqlite3_reset@
-- it afterwards, regardless of outcome. Reset uses raw 'DB.reset' (no
-- error re-throw) because SQLite reports the /previous/ step's error via
-- reset; we let the original exception propagate instead.
useStmt :: DB.Statement -> IO a -> IO a
useStmt stmt action =
  action `MonadThrow.finally` (void $ DB.reset stmt)

openSQLiteConnection ::
  Tracer IO TraceLeiosDb ->
  FilePath ->
  StrictTChan IO LeiosEbNotification ->
  IO (LeiosDbConnection IO)
openSQLiteConnection tracer dbPath notificationChan = do
  shouldInitSchema <- not <$> doesFileExist dbPath
  db <- open2 (fromString dbPath) [SQLOpenReadWrite, SQLOpenCreate] SQLVFSDefault
  traverse_ (dbExec db) $
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
    , -- Must precede 'journal_mode': SQLite cannot change the page size of a
      -- database already in WAL mode, so the order this list used to have left
      -- the setting a silent no-op and every run so far on the 4096 default.
      -- Which is where it belongs anyway. Measured: a devnet run with 32768
      -- actually in effect reached 35x WAL amplification (34 GiB of log for 0.97
      -- GiB of data) against ~18x for the same workload at 4096. The WAL is a
      -- page-level redo log, so a commit rewrites each dirtied page whole, and
      -- both hot indexes are keyed by hash, so writes scatter -- the page count
      -- barely falls as the page grows, the bytes just multiply.
      "pragma page_size = 4096;"
    , "pragma mmap_size = 268435500;"
    , "pragma journal_mode = WAL;"
    , -- SQLite's own default, spelled out because it is what keeps the log
      -- bounded: passive checkpoints reset the WAL every 1000 frames, provided
      -- no connection is sitting on a stale read snapshot. One that is will
      -- freeze back-fill indefinitely; see 'dbWithWriteTransaction'.
      "pragma wal_autocheckpoint = 1000;"
    ]
  when shouldInitSchema $
    dbExec db (fromString sql_schema)
  stmts <- prepareStmts db
  let conn = Conn{connDb = db, connStmts = stmts, connTracer = tracer}
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
      , leiosDbLookupEbClosure = sqlLookupEbClosure conn
      }

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
  dbWithWriteTransaction conn $ useStmt stmt $ do
    dbBindInt64 stmt 1 (fromIntegral $ unSlotNo point.pointSlotNo)
    dbBindBlob stmt 2 point.pointEbHash.ebHashBytes
    dbBindInt64 stmt 3 (fromIntegral ebBytesSize)
    dbStep1 stmt
 where
  Conn{connStmts = Stmts{stInsertEbPoint = stmt}} = conn

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
  completedNow <- dbWithWriteTransaction conn $ do
    forM_ items $ \(txOffset, txHash, txBytesSize) -> useStmt stInsertEbTxsRow $ do
      dbBindBlob stInsertEbTxsRow 1 point.pointEbHash.ebHashBytes
      dbBindInt64 stInsertEbTxsRow 2 (fromIntegral txOffset)
      dbBindBlob stInsertEbTxsRow 3 (let MkTxHash bytes = txHash in bytes)
      dbBindInt64 stInsertEbTxsRow 4 (fromIntegral txBytesSize)
      dbStepInsertOrTrace
        tracer
        "ebTxs"
        (show point.pointEbHash <> "@" <> show txOffset)
        stInsertEbTxsRow
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
  Conn{connStmts} = conn
  Stmts
    { stInsertEbTxsRow
    , stInitMissingCount
    , stMarkPointNotified
    } = connStmts

-- | Read a single-column @Int64@ from a statement that uses a
-- @RETURNING@ clause on a PK-scoped @UPDATE@ (i.e. produces exactly one
-- row followed by 'DB.Done'). Any other shape is a programmer error.
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
  completed <- dbWithWriteTransaction conn $ do
    -- 'dbStepInsert' still handles the rare race where a concurrent
    -- writer inserted the same hash between the filter above and the
    -- INSERT below.
    forM_ (novel missing) $ \(txHash, txBytes) -> do
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
  Conn{connStmts} = conn
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

-- | Batch-filter tx hashes against @txs@: passes txHashes as a JSON array
-- of hex strings; SQL decodes with @unhex()@ so index lookups on
-- @txs.txHashBytes@ still fire. Used internally by 'sqlInsertTxs' to skip
-- already-persisted txs.
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

-- | Schema for the Leios database.
sql_schema :: String
sql_schema =
  unlines
    [ "CREATE TABLE ebs ("
    , "  ebSlot INTEGER NOT NULL,"
    , "  ebHashBytes BLOB NOT NULL,"
    , "  ebBytesSize INTEGER NOT NULL,"
    , -- NULL = body not downloaded, >0 = txs missing, 0 = just completed, <0 = notified
      "  missingTxCount INTEGER,"
    , "  PRIMARY KEY (ebSlot, ebHashBytes)"
    , ");"
    , "CREATE INDEX idx_ebs_ebHashBytes ON ebs(ebHashBytes);"
    , "CREATE TABLE ebTxs ("
    , "  ebHashBytes BLOB NOT NULL,"
    , "  txOffset INTEGER NOT NULL,"
    , "  txHashBytes BLOB NOT NULL,"
    , "  txBytesSize INTEGER NOT NULL,"
    , "  PRIMARY KEY (ebHashBytes, txOffset)"
    , ");"
    , "CREATE INDEX idx_ebTxs_txHashBytes ON ebTxs(txHashBytes);"
    , "CREATE TABLE txs ("
    , "  txHashBytes BLOB NOT NULL PRIMARY KEY,"
    , "  txBytes BLOB NOT NULL,"
    , "  txBytesSize INTEGER NOT NULL"
    , ");"
    ]

sql_scan_ebs :: String
sql_scan_ebs =
  "SELECT ebSlot, ebHashBytes\n\
  \FROM ebs\n\
  \ORDER BY ebSlot ASC\n\
  \"

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
  "SELECT txHashBytes, txBytesSize FROM ebTxs\n\
  \WHERE ebHashBytes = ?\n\
  \ORDER BY txOffset ASC\n\
  \"

sql_insert_ebBody :: String
sql_insert_ebBody =
  "INSERT INTO ebTxs (ebHashBytes, txOffset, txHashBytes, txBytesSize) VALUES (?, ?, ?, ?)\n\
  \"

sql_insert_tx :: String
sql_insert_tx =
  "INSERT INTO txs (txHashBytes, txBytes, txBytesSize) VALUES (?, ?, ?)\n\
  \"

-- | Batch-filter txHashes via JSON1. Parameter is a JSON array of hex
-- strings; 'unhex(je.value)' decodes back into a BLOB comparable against
-- the indexed @txs.txHashBytes@ column.
sql_filter_missing_txs_json :: String
sql_filter_missing_txs_json =
  "SELECT unhex(je.value) FROM json_each(?) je\n\
  \WHERE NOT EXISTS (SELECT 1 FROM txs t WHERE t.txHashBytes = unhex(je.value))\n\
  \"

-- | Find EBs that are now complete (missingTxCount reached 0).
sql_find_complete_ebs :: String
sql_find_complete_ebs =
  "SELECT ebHashBytes, ebSlot FROM ebs WHERE missingTxCount = 0"

-- | Mark complete EBs as notified so they are not found again by
-- 'sql_find_complete_ebs'. Uses -1 as a sentinel for "already notified".
sql_mark_notified_ebs :: String
sql_mark_notified_ebs =
  "UPDATE ebs SET missingTxCount = -1 WHERE missingTxCount = 0"

-- | Decrement missingTxCount for all EBs referencing the given txHash.
-- Parameter 1: txHashBytes
sql_decrement_missing_tx_count :: String
sql_decrement_missing_tx_count =
  "UPDATE ebs SET missingTxCount = missingTxCount - 1\n\
  \WHERE ebHashBytes IN (SELECT ebHashBytes FROM ebTxs WHERE txHashBytes = ?)\n\
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
  \    LEFT JOIN txs t ON e.txHashBytes = t.txHashBytes\n\
  \    WHERE e.ebHashBytes = ? AND t.txHashBytes IS NULL\n\
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
-- JSON int array of offsets. The join uses ebTxs' PK
-- @(ebHashBytes, txOffset)@, so index lookups still fire.
sql_retrieve_from_ebTxs_json :: String
sql_retrieve_from_ebTxs_json =
  "SELECT je.value, e.txHashBytes, t.txBytes\n\
  \FROM json_each(?2) je\n\
  \JOIN ebTxs e ON e.ebHashBytes = ?1 AND e.txOffset = je.value\n\
  \LEFT JOIN txs t ON e.txHashBytes = t.txHashBytes\n\
  \ORDER BY je.value ASC\n\
  \"

sql_lookup_eb_closure :: String
sql_lookup_eb_closure =
  unlines
    [ "SELECT ebTx.txHashBytes, tx.txBytes"
    , "FROM ebTxs as ebTx"
    , "LEFT JOIN txs as tx ON ebTx.txHashBytes = tx.txHashBytes"
    , "WHERE ebTx.ebHashBytes = ?"
    , "ORDER BY ebTx.txOffset ASC"
    ]

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

-- | A read-only transaction: @BEGIN DEFERRED@, so readers do not exclude each
-- other. Any transaction that writes must use 'dbWithWriteTransaction'.
dbWithTransaction :: HasCallStack => DB.Database -> IO a -> IO a
dbWithTransaction = dbWithTransactionAs "BEGIN"

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
dbWithWriteTransaction conn k = getMonotonicTime >>= go 0
 where
  Conn{connDb = db, connTracer = tracer} = conn

  -- After this many refusals, a write transaction is no longer merely
  -- contended.
  --
  -- This picks which constructor gets traced and nothing else. Crossing it does
  -- not change how long we wait, does not throw, and does not abandon anything
  -- -- 'dbWithWriteTransaction' retries forever either way. It exists only so
  -- that the severity in the log matches the severity of the situation.
  --
  -- Each attempt is a full 'busy_timeout', so this is about half a minute of one
  -- writer making no progress, re-traced every half minute it stays that way.
  busyStuckAfter = 30

  go !attempt t0 =
    fmap (first fst) (DB.exec db (fromString "BEGIN IMMEDIATE")) >>= \case
      Left DB.ErrorBusy -> do
        -- Unbounded, deliberately. Nothing is held while waiting here -- that is
        -- the whole point of taking the lock at BEGIN -- so waiting costs
        -- latency and nothing else, whereas giving up throws, and a throw on
        -- this path kills the Leios threads outright. Past
        -- 'busyStuckAfter' attempts that is no longer ordinary contention, so
        -- say so at a severity someone will notice, and keep waiting.
        --
        -- The wait is measured, not accumulated: most of it happens inside
        -- SQLite's own busy handler, so summing the sleeps below would report a
        -- fraction of the truth and disagree with the log timestamps.
        now <- getMonotonicTime
        let n = attempt + 1
            waitedMs = 1000 * (now - t0)
        traceWith tracer $
          if n >= busyStuckAfter && n `mod` busyStuckAfter == 0
            then TraceLeiosDbBusyStuck n waitedMs
            else TraceLeiosDbBusyRetry n waitedMs
        busyBackoff
        go n t0
      Left e -> throwDbException db e
      Right () ->
        fmap fst $
          generalBracket
            (pure ())
            ( \() -> \case
                MonadThrow.ExitCaseSuccess _ -> dbExec db (fromString "COMMIT")
                MonadThrow.ExitCaseException _ -> dbExec db (fromString "ROLLBACK")
                MonadThrow.ExitCaseAbort -> dbExec db (fromString "ROLLBACK")
            )
            (\() -> k)

dbWithTransactionAs :: HasCallStack => String -> DB.Database -> IO a -> IO a
dbWithTransactionAs begin db k =
  do
    fmap fst
    $ generalBracket
      (dbExec db (fromString begin))
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
        busyBackoff
        go (n - 1) io
      Left DB.ErrorConstraint -> pure False
      Left e -> DB.getStatementDatabase stmt >>= \db -> throwDbException db e
      Right DB.Done -> pure True
      Right DB.Row -> error "dbStepInsert: unexpected Row result"

-- | Step an INSERT statement, absorbing UNIQUE/PRIMARY KEY violations and
-- emitting a 'TraceLeiosDbInsertCollision' for each one. The caller supplies a
-- table label and a key description for the trace.
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
  IO ()
dbStepInsertOrTrace tracer table key stmt = do
  novel <- dbStepInsert stmt
  _ <- DB.reset stmt
  unless novel $
    traceWith tracer (TraceLeiosDbInsertCollision table key)

-- ** Error "handling"

-- | How many times a busy statement is re-attempted /after/ SQLite's own
-- 'busy_timeout' has already expired on that attempt, before it throws.
--
-- Exhausting these throws 'LeiosDbException', which no caller catches: it leaves
-- the Leios thread it was raised in, and the node dies. That is the intent.
-- Unlike 'dbWithWriteTransaction', these retries happen /inside/ an open
-- transaction, so waiting is not free -- the transaction holds its snapshot
-- throughout, and a connection that sits on a stale snapshot indefinitely is
-- exactly what pins the WAL and stops back-fill. Half a minute of a statement
-- refusing inside a transaction is not contention, it is a deadlock, and dying
-- is better than silently wedging the log.
--
-- With 'busy_timeout' doing the real waiting, each attempt costs about a
-- timeout, so the ceiling is linear -- roughly 30 s -- rather than the quadratic
-- 83 minutes the escalating sleep used to reach at the old value of 10000.
maxBusyRetries :: Int
maxBusyRetries = 30

-- | A short fixed pause between attempts.
--
-- Deliberately not escalating.
busyBackoff :: IO ()
busyBackoff = do
  jitter <- (`mod` 5000) <$> randomIO
  threadDelay (20000 + jitter)

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
      Left DB.ErrorBusy -> do
        busyBackoff
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
