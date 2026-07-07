{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Control.Concurrent (myThreadId, threadDelay)
import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTChan
  , dupTChan
  , newBroadcastTChan
  , writeTChan
  )
import Control.Exception (throwIO)
import Control.Monad (unless, void)
import Control.Monad.Class.MonadThrow
  ( bracket
  , generalBracket
  )
import qualified Control.Monad.Class.MonadThrow as MonadThrow
import Control.Tracer (Tracer, traceWith)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
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
import Foreign.Ptr (Ptr, WordPtr, castPtr)
import Foreign.Storable (peekByteOff)
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
      , leiosDbScanCompleteEbClosuresNotOlderThanSlot = sqlScanCompleteEbClosuresSince dbPath
      , -- No-op for now; see 'leiosDbGarbageCollect'. A real implementation
        -- would open a transient connection (like 'sqlScanCompleteEbClosuresSince')
        -- and evict the no-longer-needed rows.
        leiosDbGarbageCollect = \_slotNo -> pure ()
      , -- No-op for now; see 'leiosDbPromoteToImmutable'. A real implementation
        -- would copy the EB's body and closure rows into immutable storage.
        leiosDbPromoteToImmutable = \_point -> pure ()
      , open = openSQLiteConnection tracer dbPath notificationChan
      }

-- | Implements 'leiosDbScanCompleteEbClosuresNotOlderThanSlot': the complete-closure EBs
-- announced no older than the given slot. Opens its own transient read-only
-- connection. Only an existing DB can hold complete closures: a
-- fresh DB file is created lazily by the first 'open', so when the file does
-- not exist there is nothing to scan.
sqlScanCompleteEbClosuresSince :: FilePath -> SlotNo -> IO [EbHash]
sqlScanCompleteEbClosuresSince dbPath sinceSlot = do
  dbExists <- doesFileExist dbPath
  if not dbExists
    then pure []
    else do
      db <- open2 (fromString dbPath) [SQLOpenReadWrite] SQLVFSDefault
      hashes <- sqlScanCompleteEbHashesSince db sinceSlot
      void $ DB.close db
      pure hashes

-- * Connection management

openSQLiteConnection ::
  Tracer IO TraceLeiosDb ->
  FilePath ->
  StrictTChan IO LeiosEbNotification ->
  IO (LeiosDbConnection IO)
openSQLiteConnection tracer dbPath notificationChan = do
  shouldInitSchema <- not <$> doesFileExist dbPath
  db <- open2 (fromString dbPath) [SQLOpenReadWrite, SQLOpenCreate] SQLVFSDefault
  traverse_ (dbExec db) $
    [ "pragma journal_mode = WAL;"
    , "pragma synchronous = normal;"
    , "pragma page_size = 32768;"
    , "pragma mmap_size = 268435500;"
    ]
  when shouldInitSchema $
    dbExec db (fromString sql_schema)
  let notify = atomically . writeTChan notificationChan
  -- TEMP: safety net. 'LeiosDbConnection' is a direct-sqlite handle that is
  -- not thread-safe (SQLite protects concurrent ops but not close-during-op).
  -- Fail loudly with a call stack if any op runs on a different thread than
  -- the opener. Remove once we have proven all callsites are single-threaded.
  owner <- myThreadId
  let check :: HasCallStack => IO ()
      check = do
        me <- myThreadId
        when (me /= owner) $
          error $
            "LeiosDbConnection used from thread "
              <> show me
              <> " but opened on "
              <> show owner
        checkSqliteIntegrity db
  pure $
    LeiosDbConnection
      { close = check >> void (DB.close db)
      , leiosDbScanEbPoints = check >> sqlScanEbPoints db
      , leiosDbInsertEbPoint = \p sz -> check >> sqlInsertEbPoint db p sz
      , leiosDbLookupEbBody = \h -> check >> sqlLookupEbBody db h
      , leiosDbInsertEbBody = \p eb -> check >> sqlInsertEbBody tracer db notify p eb
      , leiosDbInsertTxs = \txs -> check >> sqlInsertTxs tracer db notify txs
      , leiosDbBatchRetrieveTxs = \h offs -> check >> sqlBatchRetrieveTxs tracer db h offs
      , leiosDbFilterMissingEbBodies = \ebs -> check >> sqlFilterMissingEbBodies tracer db ebs
      , leiosDbFilterMissingTxs = \hs -> check >> sqlFilterMissingTxs tracer db hs
      , leiosDbLookupEbClosure = \h -> check >> sqlLookupEbClosure db h
      }

-- | TEMP: sanity-check that the @sqlite3@ conn struct's @.mutex@ field
-- still looks like a valid pointer.
--
-- Motivation: the proto-devnet crashes with a SIGSEGV inside
-- @sqlite3_finalize@ where the sqlite3 struct's memory has been
-- overwritten with small integer values (in one dump, @pVdbe = 0x11@
-- and @mutex = 0x4b@). The safety net on 'LeiosDbConnection' only
-- catches cross-thread facade entry, not corruption from further
-- inside. This peek runs on every facade entry BEFORE we hand
-- anything to SQLite, so we throw a Haskell exception with a stack
-- trace instead of segfaulting.
--
-- Layout assumption (SQLite 3.45.0, our compile flags):
--
--   +0   sqlite3_vfs *pVfs
--   +8   struct Vdbe *pVdbe
--   +16  CollSeq *pDfltColl
--   +24  sqlite3_mutex *mutex     ← we peek this
--
-- This offset is architecture- and compile-flag-dependent; we know
-- it's correct on x86_64 Linux with our build flags because we
-- pinned it from the coredump.
--
-- Under SERIALIZED threading mode (which our SQLite is compiled
-- with, @THREADSAFE=1@) the @.mutex@ field is always a non-NULL
-- pointer while the conn is alive. So @0@ or any small integer
-- means the struct has been freed and reused.
checkSqliteIntegrity :: HasCallStack => DB.Database -> IO ()
checkSqliteIntegrity (DB.Database dbp) = do
  mutex :: WordPtr <- peekByteOff (castPtr dbp :: Ptr ()) 24
  when (fromIntegral mutex < (0x1000 :: Integer)) $
    error $
      "LeiosDbConnection: sqlite3 conn struct appears corrupted; "
        <> "db->mutex = "
        <> show mutex
        <> " (expected a valid pointer). Refusing to proceed to avoid SIGSEGV."

-- * Top-level implementations

sqlScanEbPoints :: DB.Database -> IO [(SlotNo, EbHash)]
sqlScanEbPoints db =
  dbWithBEGIN db $ dbWithPrepare db (fromString sql_scan_ebs) $ \stmt -> do
    let loop acc =
          dbStep stmt >>= \case
            DB.Done -> pure (reverse acc)
            DB.Row -> do
              slot <- SlotNo . fromIntegral <$> DB.columnInt64 stmt 0
              hash <- MkEbHash <$> DB.columnBlob stmt 1
              loop ((slot, hash) : acc)
    loop []

sqlScanCompleteEbHashesSince :: DB.Database -> SlotNo -> IO [EbHash]
sqlScanCompleteEbHashesSince db sinceSlot =
  dbWithBEGIN db $ dbWithPrepare db (fromString sql_scan_complete_ebs_since) $ \stmt -> do
    dbBindInt64 stmt 1 (fromIntegral $ unSlotNo sinceSlot)
    let loop acc =
          dbStep stmt >>= \case
            DB.Done -> pure (reverse acc)
            DB.Row -> do
              hash <- MkEbHash <$> DB.columnBlob stmt 0
              loop (hash : acc)
    loop []

sqlLookupEbBody :: DB.Database -> EbHash -> IO [(TxHash, BytesSize)]
sqlLookupEbBody db ebHash =
  dbWithBEGIN db $ dbWithPrepare db (fromString sql_lookup_ebBodies) $ \stmt -> do
    dbBindBlob stmt 1 (let MkEbHash bytes = ebHash in bytes)
    let loop acc =
          dbStep stmt >>= \case
            DB.Done -> pure (reverse acc)
            DB.Row -> do
              txHash <- MkTxHash <$> DB.columnBlob stmt 0
              size <- fromIntegral <$> DB.columnInt64 stmt 1
              loop ((txHash, size) : acc)
    loop []

sqlInsertEbPoint :: DB.Database -> LeiosPoint -> BytesSize -> IO ()
sqlInsertEbPoint db point ebBytesSize =
  dbWithBEGIN db $ dbWithPrepare db (fromString sql_insert_eb) $ \stmt -> do
    dbBindInt64 stmt 1 (fromIntegral $ unSlotNo point.pointSlotNo)
    dbBindBlob stmt 2 point.pointEbHash.ebHashBytes
    dbBindInt64 stmt 3 (fromIntegral ebBytesSize)
    dbStep1 stmt

-- | Persist an EB body. The point MUST already be present (inserted
-- via 'sqlInsertEbPoint' on the announcement path).
sqlInsertEbBody ::
  Tracer IO TraceLeiosDb ->
  DB.Database ->
  (LeiosEbNotification -> IO ()) ->
  LeiosPoint ->
  LeiosEb ->
  IO ()
sqlInsertEbBody tracer db notify point eb = do
  let items = leiosEbBodyItems eb
      ebBytesSize = leiosEbBytesSize eb
  when (null items) $
    error "leiosDbInsertEbBody: empty EB body (programmer error)"
  dbWithBEGIN db $ do
    dbWithPrepare db (fromString sql_insert_ebBody) $ \stmt ->
      mapM_
        ( \(txOffset, txHash, txBytesSize) -> do
            dbBindBlob stmt 1 point.pointEbHash.ebHashBytes
            dbBindInt64 stmt 2 (fromIntegral txOffset)
            dbBindBlob stmt 3 (let MkTxHash bytes = txHash in bytes)
            dbBindInt64 stmt 4 (fromIntegral txBytesSize)
            dbStepInsertOrTrace
              tracer
              "ebTxs"
              (show point.pointEbHash <> "@" <> show txOffset)
              stmt
        )
        items
    -- Initialize missingTxCount (accounts for txs already in the DB)
    dbWithPrepare db (fromString sql_init_missing_tx_count) $ \stmt -> do
      dbBindBlob stmt 1 point.pointEbHash.ebHashBytes
      dbBindBlob stmt 2 point.pointEbHash.ebHashBytes
      dbBindInt64 stmt 3 (fromIntegral $ unSlotNo point.pointSlotNo)
      dbStep1 stmt
  notify $ AcquiredEb point ebBytesSize

sqlInsertTxs ::
  Tracer IO TraceLeiosDb ->
  DB.Database ->
  (LeiosEbNotification -> IO ()) ->
  [(TxHash, ByteString)] ->
  IO CompletedEbs
sqlInsertTxs tracer db notify txs = do
  -- Skip txs already persisted in 'txs'. Under mempool backlog,
  -- successive forges (or overlapping peer EBs) re-present the same tx
  -- hashes; attempting the INSERT and catching a constraint violation
  -- still pays the bind + PK-lookup + reset cost per row. Reuses the
  -- existing 'mem.txHashes'-based filter (own read transaction, safe to
  -- call before the write BEGIN below).
  missing <- Set.fromList <$> sqlFilterMissingTxs tracer db (map fst txs)
  let novel = filter (\(h, _) -> h `Set.member` missing) txs
  completed <- dbWithBEGIN db $
    -- 'dbStepInsert' still handles the rare race where a concurrent
    -- writer inserted the same hash between the filter above and the
    -- INSERT below.
    dbWithPrepare db (fromString sql_insert_tx) $ \stmtInsert ->
      dbWithPrepare db (fromString sql_decrement_missing_tx_count) $ \stmtDecr -> do
        forM_ novel $ \(txHash, txBytes) -> do
          let txBytesSize = fromIntegral $ BS.length txBytes
              txHashBytes = let MkTxHash bytes = txHash in bytes
          dbBindBlob stmtInsert 1 txHashBytes
          dbBindBlob stmtInsert 2 txBytes
          dbBindInt64 stmtInsert 3 txBytesSize
          inserted <- dbStepInsert stmtInsert
          -- Use raw reset: after ErrorConstraint, dbReset would re-throw the error
          void $ DB.reset stmtInsert
          when inserted $ do
            dbBindBlob stmtDecr 1 txHashBytes
            dbStep1 stmtDecr
            dbReset stmtDecr
        -- Find newly-complete EBs (missingTxCount reached 0)
        completed <- dbWithPrepare db (fromString sql_find_complete_ebs) $ \stmt -> do
          let loop acc =
                dbStep stmt >>= \case
                  DB.Done -> pure (reverse acc)
                  DB.Row -> do
                    ebHash <- MkEbHash <$> DB.columnBlob stmt 0
                    slot <- SlotNo . fromIntegral <$> DB.columnInt64 stmt 1
                    loop (MkLeiosPoint slot ebHash : acc)
          loop []
        -- Mark them as notified so they are not found again
        dbWithPrepare db (fromString sql_mark_notified_ebs) $ \stmt ->
          dbStep1 stmt
        pure completed
  -- Emit a closure-completion notification for each completed EB
  forM_ completed $ \point -> notify (AcquiredEbTxs point)
  pure completed

-- | Retrieve tx bytes for a batch of @(ebHash, txOffset)@ points. Passes
-- the offsets list as a JSON int array bound to a single parameter;
-- SQLite's 'json_each' virtual table joins it against 'ebTxs' + 'txs'.
--
-- No temp tables, no attached databases, no per-item INSERT round-trips.
-- Works on strictly read-only connections.
sqlBatchRetrieveTxs ::
  Tracer IO TraceLeiosDb ->
  DB.Database ->
  EbHash ->
  [Int] ->
  IO [(Int, TxHash, Maybe ByteString)]
sqlBatchRetrieveTxs _tracer db ebHash offsets =
  dbWithBEGIN db $
    dbWithPrepare db (fromString sql_retrieve_from_ebTxs_json) $ \stmt -> do
      dbBindBlob stmt 1 (let MkEbHash bytes = ebHash in bytes)
      dbBindUtf8 stmt 2 (jsonIntArray offsets)
      let loop acc =
            dbStep stmt >>= \case
              DB.Done -> pure (reverse acc)
              DB.Row -> do
                offset <- fromIntegral <$> DB.columnInt64 stmt 0
                txHash <- MkTxHash <$> DB.columnBlob stmt 1
                -- Column 2 is from LEFT JOIN, NULL if tx not in txs table
                txBytes <- DB.columnBlob stmt 2
                let mbTxBytes = if txBytes == mempty then Nothing else Just txBytes
                loop ((offset, txHash, mbTxBytes) : acc)
      loop []

-- | Batch-filter EB points against @ebTxs@. Passes ebHashes as a JSON
-- array of hex strings; SQL decodes with @unhex()@ so index lookups on
-- @ebTxs.ebHashBytes@ still fire.
sqlFilterMissingEbBodies ::
  Tracer IO TraceLeiosDb -> DB.Database -> [LeiosPoint] -> IO [LeiosPoint]
sqlFilterMissingEbBodies _tracer db points =
  dbWithBEGIN db $
    dbWithPrepare db (fromString sql_filter_missing_eb_bodies_json) $ \stmt -> do
      let pointsByHash = Map.fromList [(p.pointEbHash, p) | p <- points]
      dbBindUtf8 stmt 1 (jsonHexArray (map ebHashBytes (Map.keys pointsByHash)))
      let loop acc =
            dbStep stmt >>= \case
              DB.Done -> pure (reverse acc)
              DB.Row -> do
                ebHash <- MkEbHash <$> DB.columnBlob stmt 0
                case Map.lookup ebHash pointsByHash of
                  Just p -> loop (p : acc)
                  Nothing -> loop acc
      loop []

-- | Batch-filter tx hashes against @txs@. Same idiom as
-- 'sqlFilterMissingEbBodies'.
sqlFilterMissingTxs ::
  Tracer IO TraceLeiosDb -> DB.Database -> [TxHash] -> IO [TxHash]
sqlFilterMissingTxs _tracer db txHashes =
  dbWithBEGIN db $
    dbWithPrepare db (fromString sql_filter_missing_txs_json) $ \stmt -> do
      dbBindUtf8 stmt 1 (jsonHexArray [b | MkTxHash b <- txHashes])
      let loop acc =
            dbStep stmt >>= \case
              DB.Done -> pure (reverse acc)
              DB.Row -> do
                txHash <- MkTxHash <$> DB.columnBlob stmt 0
                loop (txHash : acc)
      loop []

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

sqlLookupEbClosure :: DB.Database -> EbHash -> IO (Maybe [(TxHash, ByteString)])
sqlLookupEbClosure db ebHash =
  dbWithBEGIN db $ dbWithPrepare db (fromString sql_lookup_eb_closure) $ \stmt -> do
    dbBindBlob stmt 1 (ebHashBytes ebHash)
    -- FIXME(bladyjoker): This should have a SlotNo as the second part of the key
    let loop acc =
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
    loop []

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

-- | For 'sqlScanCompleteEbClosuresSince'
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
  "SELECT DISTINCT ebHashBytes FROM ebs\n\
  \WHERE ebSlot >= ?\n\
  \  AND ebHashBytes IN\n\
  \      (SELECT ebHashBytes FROM ebs WHERE missingTxCount IS NOT NULL AND missingTxCount <= 0)\n\
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

-- | Initialize missingTxCount after EB body is inserted.
-- Counts ebTxs entries that don't yet have a corresponding tx in the txs table.
-- Parameters: 1 = ebHashBytes, 2 = ebHashBytes, 3 = ebSlot
sql_init_missing_tx_count :: String
sql_init_missing_tx_count =
  "UPDATE ebs SET missingTxCount = (\n\
  \    SELECT COUNT(*) FROM ebTxs e\n\
  \    LEFT JOIN txs t ON e.txHashBytes = t.txHashBytes\n\
  \    WHERE e.ebHashBytes = ? AND t.txHashBytes IS NULL\n\
  \) WHERE ebHashBytes = ? AND ebSlot = ?\n\
  \"

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

dbWithPrepare :: HasCallStack => DB.Database -> DB.Utf8 -> (DB.Statement -> IO a) -> IO a
dbWithPrepare db q k = bracket (dbPrepare db q) dbFinalize k

-- TODO: alternative: bind and use https://www.sqlite.org/c3ref/busy_handler.html
dbWithBEGIN :: HasCallStack => DB.Database -> IO a -> IO a
dbWithBEGIN db k =
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

dbReset :: HasCallStack => DB.Statement -> IO ()
dbReset stmt = withDieStmt stmt $ DB.reset stmt

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
