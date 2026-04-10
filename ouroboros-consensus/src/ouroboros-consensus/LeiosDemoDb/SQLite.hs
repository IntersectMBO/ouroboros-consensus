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
  , sql_insert_ebPoint
  , sql_insert_ebBody
  , sql_insert_tx
  ) where

import Cardano.Prelude (foldM, forM_, when)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTChan
  , dupTChan
  , newBroadcastTChan
  , writeTChan
  )
import Control.Exception (throwIO)
import Control.Monad (void)
import Control.Monad.Class.MonadThrow
  ( bracket
  , generalBracket
  )
import qualified Control.Monad.Class.MonadThrow as MonadThrow
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
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
  , LeiosFetchWork (..)
  )
import LeiosDemoException (LeiosDbException (..))
import LeiosDemoTypes
  ( BytesSize
  , EbHash (..)
  , LeiosEb
  , LeiosNotification (..)
  , LeiosPoint (..)
  , TxHash (..)
  , leiosEbBodyItems
  , leiosEbBytesSize
  , trustNoVerifyLeiosCertificate
  )
import Ouroboros.Consensus.Util.IOLike (atomically)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (die)

-- * Public API

-- | Create a new Leios database connection from environment variable.
-- This looks up the LEIOS_DB_PATH environment variable and opens the database.
newLeiosDBSQLiteFromEnv :: IO (LeiosDbHandle IO)
newLeiosDBSQLiteFromEnv = do
  dbPath <-
    lookupEnv "LEIOS_DB_PATH" >>= \case
      Nothing -> die "You must define the LEIOS_DB_PATH variable for this demo."
      Just x -> pure x
  newLeiosDBSQLite dbPath

-- | Create a new Leios database using the SQLite implementation at given file
-- path.
--
-- Each call to 'open' on the returned handle creates a new SQLite connection.
-- Connections are not thread-safe and should not be shared across threads.
newLeiosDBSQLite :: FilePath -> IO (LeiosDbHandle IO)
newLeiosDBSQLite dbPath = do
  notificationChan <- atomically newBroadcastTChan
  pure $
    LeiosDbHandle
      { subscribeEbNotifications =
          atomically (dupTChan notificationChan)
      , open = openSQLiteConnection dbPath notificationChan
      }

-- * Connection management

openSQLiteConnection :: FilePath -> StrictTChan IO LeiosNotification -> IO (LeiosDbConnection IO)
openSQLiteConnection dbPath notificationChan = do
  shouldInitSchema <- not <$> doesFileExist dbPath
  db <- open2 (fromString dbPath) [SQLOpenReadWrite, SQLOpenCreate, SQLOpenFullMutex] SQLVFSDefault
  -- traverse_ (dbExec db) $
  --   [ "pragma journal_mode = WAL;"
  --   , -- This would probably be fine to set unless we care very strongly about consistency in the
  --     -- event of sudden power / disk failure:
  --     -- , "pragma synchronous = normal;"
  --     "pragma page_size = 32768;"
  --   , "pragma mmap_size = 268435500;"
  --   ]
  when shouldInitSchema $
    dbExec db (fromString sql_schema)
  dbExec db (fromString sql_attach_memTxPoints)
  let notify = atomically . writeTChan notificationChan
  pure $
    LeiosDbConnection
      { close = void $ DB.close db
      , leiosDbScanEbPoints = sqlScanEbPoints db
      , leiosDbLookupEbPoint = sqlLookupEbPoint db
      , leiosDbInsertEbPoint = sqlInsertEbPoint db
      , leiosDbLookupEbBody = sqlLookupEbBody db
      , leiosDbInsertEbBody = sqlInsertEbBody db notify
      , leiosDbInsertTxs = sqlInsertTxs db notify
      , leiosDbBatchRetrieveTxs = sqlBatchRetrieveTxs db
      , leiosDbFilterMissingEbBodies = sqlFilterMissingEbBodies db
      , leiosDbFilterMissingTxs = sqlFilterMissingTxs db
      , leiosDbQueryFetchWork = sqlQueryFetchWork db
      , leiosDbQueryCompletedEbByPoint = sqlQueryCompletedEbByPoint db
      , leiosDbQueryCertificateByPoint = \_ebPoint -> return $ Just trustNoVerifyLeiosCertificate
      }

-- * Top-level implementations

sqlScanEbPoints :: DB.Database -> IO [(SlotNo, EbHash)]
sqlScanEbPoints db =
  dbWithBEGIN db $ dbWithPrepare db (fromString sql_scan_ebPoints) $ \stmt -> do
    let loop acc =
          dbStep stmt >>= \case
            DB.Done -> pure (reverse acc)
            DB.Row -> do
              slot <- SlotNo . fromIntegral <$> DB.columnInt64 stmt 0
              hash <- MkEbHash <$> DB.columnBlob stmt 1
              loop ((slot, hash) : acc)
    loop []

sqlLookupEbPoint :: DB.Database -> EbHash -> IO (Maybe SlotNo)
sqlLookupEbPoint db ebHash =
  dbWithBEGIN db $ dbWithPrepare db (fromString sql_lookup_ebPoint) $ \stmt -> do
    dbBindBlob stmt 1 (let MkEbHash bytes = ebHash in bytes)
    dbStep stmt >>= \case
      DB.Done -> pure Nothing
      DB.Row -> do
        slot <- SlotNo . fromIntegral <$> DB.columnInt64 stmt 0
        pure (Just slot)

sqlInsertEbPoint :: DB.Database -> LeiosPoint -> BytesSize -> IO ()
sqlInsertEbPoint db point ebBytesSize =
  dbWithBEGIN db $ dbWithPrepare db (fromString sql_insert_ebPoint) $ \stmt -> do
    dbBindInt64 stmt 1 (fromIntegral $ unSlotNo point.pointSlotNo)
    dbBindBlob stmt 2 point.pointEbHash.ebHashBytes
    dbBindInt64 stmt 3 (fromIntegral ebBytesSize)
    dbStep1 stmt

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

sqlInsertEbBody :: DB.Database -> (LeiosNotification -> IO ()) -> LeiosPoint -> LeiosEb -> IO ()
sqlInsertEbBody db notify point eb = do
  let items = leiosEbBodyItems eb
  when (null items) $
    error "leiosDbInsertEbBody: empty EB body (programmer error)"
  dbWithBEGIN db $ dbWithPrepare db (fromString sql_insert_ebBody) $ \stmt ->
    mapM_
      ( \(txOffset, txHash, txBytesSize) -> do
          dbBindBlob stmt 1 point.pointEbHash.ebHashBytes
          dbBindInt64 stmt 2 (fromIntegral txOffset)
          dbBindBlob stmt 3 (let MkTxHash bytes = txHash in bytes)
          dbBindInt64 stmt 4 (fromIntegral txBytesSize)
          dbStep1 stmt
          dbReset stmt
      )
      items
  notify $ LeiosOfferBlock point (leiosEbBytesSize eb)

sqlInsertTxs ::
  DB.Database -> (LeiosNotification -> IO ()) -> [(TxHash, ByteString)] -> IO CompletedEbs
sqlInsertTxs db notify txs = do
  -- Insert all txs into global txs table, then check for newly-complete EBs
  completed <- dbWithBEGIN db $ do
    -- INSERT OR IGNORE all txs
    dbWithPrepare db (fromString sql_insert_tx) $ \stmtTx ->
      forM_ txs $ \(txHash, txBytes) -> do
        let txBytesSize = fromIntegral $ BS.length txBytes
            txHashBytes = let MkTxHash bytes = txHash in bytes
        dbBindBlob stmtTx 1 txHashBytes
        dbBindBlob stmtTx 2 txBytes
        dbBindInt64 stmtTx 3 txBytesSize
        dbStep1 stmtTx
        dbReset stmtTx
    -- For each inserted txHash, find EBs that are now complete
    -- Collect results in a Map to deduplicate (same EB may be found via multiple txHashes)
    dbWithPrepare db (fromString sql_find_complete_ebs) $ \stmt -> do
      let checkTxHash acc (txHash, _) = do
            dbBindBlob stmt 1 (let MkTxHash bytes = txHash in bytes)
            let loop acc' =
                  dbStep stmt >>= \case
                    DB.Done -> pure acc'
                    DB.Row -> do
                      ebHash <- MkEbHash <$> DB.columnBlob stmt 0
                      slot <- SlotNo . fromIntegral <$> DB.columnInt64 stmt 1
                      loop (Map.insert ebHash slot acc')
            result <- loop acc
            dbReset stmt
            pure result
      ebMap <- foldM checkTxHash Map.empty txs
      pure [MkLeiosPoint slot ebHash | (ebHash, slot) <- Map.toList ebMap]
  -- Emit notifications for each completed EB
  forM_ completed $ notify . LeiosOfferBlockTxs
  pure completed

sqlBatchRetrieveTxs :: DB.Database -> EbHash -> [Int] -> IO [(Int, TxHash, Maybe ByteString)]
sqlBatchRetrieveTxs db ebHash offsets =
  dbWithBEGIN db $ do
    -- First, insert offsets into temp table
    dbWithPrepare db (fromString sql_insert_memTxPoints) $ \stmtInsert ->
      mapM_
        ( \offset -> do
            dbBindBlob stmtInsert 1 (let MkEbHash bytes = ebHash in bytes)
            dbBindInt64 stmtInsert 2 (fromIntegral offset)
            dbStep1 stmtInsert
            dbReset stmtInsert
        )
        offsets

    -- Then retrieve from ebTxs LEFT JOIN txs
    results <- dbWithPrepare db (fromString sql_retrieve_from_ebTxs) $ \stmtRetrieve -> do
      let loop acc =
            dbStep stmtRetrieve >>= \case
              DB.Done -> pure (reverse acc)
              DB.Row -> do
                offset <- fromIntegral <$> DB.columnInt64 stmtRetrieve 0
                txHash <- MkTxHash <$> DB.columnBlob stmtRetrieve 1
                -- Column 2 is from LEFT JOIN, NULL if tx not in txs table
                txBytes <- DB.columnBlob stmtRetrieve 2
                let mbTxBytes = if txBytes == mempty then Nothing else Just txBytes
                loop ((offset, txHash, mbTxBytes) : acc)
      loop []

    -- Flush temp table
    dbWithPrepare db (fromString sql_flush_memTxPoints) $ \stmtFlush ->
      dbStep1 stmtFlush
    pure results

sqlFilterMissingEbBodies :: DB.Database -> [LeiosPoint] -> IO [LeiosPoint]
sqlFilterMissingEbBodies db points =
  -- TODO: Replace temp table approach with JSON1 extension for cleaner batch queries.
  dbWithBEGIN db $ do
    let pointsByHash = Map.fromList [(p.pointEbHash, p) | p <- points]
    dbWithPrepare db (fromString sql_insert_memEbHashes) $ \stmtInsert ->
      forM_ points $ \p -> do
        dbBindBlob stmtInsert 1 p.pointEbHash.ebHashBytes
        dbStep1 stmtInsert
        dbReset stmtInsert
    result <- dbWithPrepare db (fromString sql_filter_missing_eb_bodies) $ \stmt -> do
      let loop acc =
            dbStep stmt >>= \case
              DB.Done -> pure (reverse acc)
              DB.Row -> do
                ebHash <- MkEbHash <$> DB.columnBlob stmt 0
                case Map.lookup ebHash pointsByHash of
                  Just p -> loop (p : acc)
                  Nothing -> loop acc
      loop []
    dbWithPrepare db (fromString sql_flush_memEbHashes) $ \stmtFlush ->
      dbStep1 stmtFlush
    pure result

sqlFilterMissingTxs :: DB.Database -> [TxHash] -> IO [TxHash]
sqlFilterMissingTxs db txHashes =
  -- TODO: Replace temp table approach with JSON1 extension for cleaner batch queries:
  --   WHERE t.txHashBytes IN (SELECT unhex(value) FROM json_each(?))
  -- This would eliminate the need for mem.txHashes table and insert/flush overhead.
  dbWithBEGIN db $ do
    dbWithPrepare db (fromString sql_insert_memTxHashes) $ \stmtInsert ->
      forM_ txHashes $ \(MkTxHash bytes) -> do
        dbBindBlob stmtInsert 1 bytes
        dbStep1 stmtInsert
        dbReset stmtInsert
    result <- dbWithPrepare db (fromString sql_filter_missing_txs) $ \stmt -> do
      let loop acc =
            dbStep stmt >>= \case
              DB.Done -> pure (reverse acc)
              DB.Row -> do
                txHash <- MkTxHash <$> DB.columnBlob stmt 0
                loop (txHash : acc)
      loop []
    dbWithPrepare db (fromString sql_flush_memTxHashes) $ \stmtFlush ->
      dbStep1 stmtFlush
    pure result

sqlQueryFetchWork :: DB.Database -> IO LeiosFetchWork
sqlQueryFetchWork db =
  dbWithBEGIN db $ do
    -- Query missing EB bodies
    missingEbBodies <- dbWithPrepare db (fromString sql_query_missing_eb_bodies) $ \stmt -> do
      let loop acc =
            dbStep stmt >>= \case
              DB.Done -> pure (Map.fromList (reverse acc))
              DB.Row -> do
                slot <- SlotNo . fromIntegral <$> DB.columnInt64 stmt 0
                ebHash <- MkEbHash <$> DB.columnBlob stmt 1
                ebBytesSize <- fromIntegral <$> DB.columnInt64 stmt 2
                loop ((MkLeiosPoint slot ebHash, ebBytesSize) : acc)
      loop []
    -- Query missing TXs, grouped by EB
    missingEbTxs <- dbWithPrepare db (fromString sql_query_missing_eb_txs) $ \stmt -> do
      let loop acc =
            dbStep stmt >>= \case
              DB.Done -> pure (Map.fromListWith (++) (reverse acc))
              DB.Row -> do
                slot <- SlotNo . fromIntegral <$> DB.columnInt64 stmt 0
                ebHash <- MkEbHash <$> DB.columnBlob stmt 1
                txOffset <- fromIntegral <$> DB.columnInt64 stmt 2
                txHash <- MkTxHash <$> DB.columnBlob stmt 3
                txBytesSize <- fromIntegral <$> DB.columnInt64 stmt 4
                loop ((MkLeiosPoint slot ebHash, [(txOffset, txHash, txBytesSize)]) : acc)
      loop []
    pure LeiosFetchWork{missingEbBodies, missingEbTxs}

sqlQueryCompletedEbByPoint :: DB.Database -> LeiosPoint -> IO (Maybe [(TxHash, ByteString)])
sqlQueryCompletedEbByPoint db ebPoint =
  dbWithBEGIN db $ dbWithPrepare db (fromString sqlQueryCompletedEbByPoint') $ \stmt -> do
    dbBindBlob stmt 1 (ebHashBytes . pointEbHash $ ebPoint)
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

sql_schema :: String
sql_schema =
  "CREATE TABLE ebPoints (\n\
  \    ebSlot INTEGER NOT NULL\n\
  \  ,\n\
  \    ebHashBytes BLOB NOT NULL\n\
  \  ,\n\
  \    ebBytesSize INTEGER NOT NULL\n\
  \  ,\n\
  \    PRIMARY KEY (ebSlot, ebHashBytes)\n\
  \  );\n\
  \CREATE INDEX idx_ebPoints_ebHashBytes ON ebPoints(ebHashBytes);\n\
  \CREATE TABLE ebTxs (\n\
  \    ebHashBytes BLOB NOT NULL   -- foreign key ebPoints.ebHashBytes\n\
  \  ,\n\
  \    txOffset INTEGER NOT NULL\n\
  \  ,\n\
  \    txHashBytes BLOB NOT NULL   -- raw bytes\n\
  \  ,\n\
  \    txBytesSize INTEGER NOT NULL\n\
  \  ,\n\
  \    PRIMARY KEY (ebHashBytes, txOffset)\n\
  \  );\n\
  \CREATE INDEX idx_ebTxs_txHashBytes ON ebTxs(txHashBytes);\n\
  \CREATE TABLE txs (\n\
  \    txHashBytes BLOB NOT NULL PRIMARY KEY\n\
  \  ,\n\
  \    txBytes BLOB NOT NULL\n\
  \  ,\n\
  \    txBytesSize INTEGER NOT NULL\n\
  \  );\n\
  \"

sql_scan_ebPoints :: String
sql_scan_ebPoints =
  "SELECT ebSlot, ebHashBytes\n\
  \FROM ebPoints\n\
  \ORDER BY ebSlot ASC\n\
  \"

sql_lookup_ebPoint :: String
sql_lookup_ebPoint =
  "SELECT ebSlot FROM ebPoints WHERE ebHashBytes = ?"

sql_insert_ebPoint :: String
sql_insert_ebPoint =
  "INSERT OR IGNORE INTO ebPoints (ebSlot, ebHashBytes, ebBytesSize) VALUES (?, ?, ?)"

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
  "INSERT OR IGNORE INTO txs (txHashBytes, txBytes, txBytesSize) VALUES (?, ?, ?)\n\
  \"

-- | Query for missing EB bodies: EBs announced but not yet downloaded.
-- Returns (ebSlot, ebHashBytes, ebBytesSize) for each missing body.
sql_query_missing_eb_bodies :: String
sql_query_missing_eb_bodies =
  "SELECT ep.ebSlot, ep.ebHashBytes, ep.ebBytesSize\n\
  \FROM ebPoints ep\n\
  \WHERE NOT EXISTS (\n\
  \    SELECT 1 FROM ebTxs e WHERE e.ebHashBytes = ep.ebHashBytes\n\
  \)\n\
  \ORDER BY ep.ebSlot DESC\n\
  \"

-- | Query for missing TXs: TXs in ebTxs that don't have corresponding entries in txs.
-- Returns (ebSlot, ebHashBytes, txOffset, txHashBytes, txBytesSize) for each missing TX.
sql_query_missing_eb_txs :: String
sql_query_missing_eb_txs =
  "SELECT ep.ebSlot, e.ebHashBytes, e.txOffset, e.txHashBytes, e.txBytesSize\n\
  \FROM ebTxs e\n\
  \JOIN ebPoints ep ON e.ebHashBytes = ep.ebHashBytes\n\
  \LEFT JOIN txs t ON e.txHashBytes = t.txHashBytes\n\
  \WHERE t.txHashBytes IS NULL\n\
  \ORDER BY ep.ebSlot DESC, e.txOffset ASC\n\
  \"

-- | Insert ebHashes into temp table for batch filtering
sql_insert_memEbHashes :: String
sql_insert_memEbHashes =
  "INSERT INTO mem.ebHashes (ebHashBytes) VALUES (?)"

-- | Filter: return ebHashes from temp table that do NOT have bodies in ebTxs
sql_filter_missing_eb_bodies :: String
sql_filter_missing_eb_bodies =
  "SELECT m.ebHashBytes FROM mem.ebHashes m\n\
  \WHERE NOT EXISTS (SELECT 1 FROM ebTxs e WHERE e.ebHashBytes = m.ebHashBytes)\n\
  \"

-- | Flush the temp ebHashes table
sql_flush_memEbHashes :: String
sql_flush_memEbHashes =
  "DELETE FROM mem.ebHashes"

-- | Insert txHashes into temp table for batch filtering
sql_insert_memTxHashes :: String
sql_insert_memTxHashes =
  "INSERT INTO mem.txHashes (txHashBytes) VALUES (?)"

-- | Filter: return txHashes from temp table that do NOT exist in txs
sql_filter_missing_txs :: String
sql_filter_missing_txs =
  "SELECT m.txHashBytes FROM mem.txHashes m\n\
  \WHERE NOT EXISTS (SELECT 1 FROM txs t WHERE t.txHashBytes = m.txHashBytes)\n\
  \"

-- | Flush the temp txHashes table
sql_flush_memTxHashes :: String
sql_flush_memTxHashes =
  "DELETE FROM mem.txHashes"

-- | Find EBs that are now complete (all txs present in txs table).
-- This query finds EBs referencing the given txHash where all txs are present.
-- Uses idx_ebTxs_txHashBytes to find candidate EBs efficiently.
-- Parameter 1: txHashBytes to check
--
-- TODO: If JSON1 extension is available, we could pass all txHashes in a single
-- query using json_each() with unhex() for more efficient batch checking:
--   WHERE e.txHashBytes IN (SELECT unhex(j.value) FROM json_each(?) AS j)
sql_find_complete_ebs :: String
sql_find_complete_ebs =
  "SELECT DISTINCT e.ebHashBytes, ep.ebSlot\n\
  \FROM ebTxs e\n\
  \JOIN ebPoints ep ON e.ebHashBytes = ep.ebHashBytes\n\
  \WHERE e.txHashBytes = ?\n\
  \  AND NOT EXISTS (\n\
  \    SELECT 1 FROM ebTxs e2\n\
  \    LEFT JOIN txs t ON e2.txHashBytes = t.txHashBytes\n\
  \    WHERE e2.ebHashBytes = e.ebHashBytes\n\
  \      AND t.txHashBytes IS NULL\n\
  \  )\n\
  \"

sql_insert_memTxPoints :: String
sql_insert_memTxPoints =
  "INSERT INTO mem.txPoints (ebHashBytes, txOffset) VALUES (?, ?)\n\
  \"

sql_retrieve_from_ebTxs :: String
sql_retrieve_from_ebTxs =
  "SELECT e.txOffset, e.txHashBytes, t.txBytes\n\
  \FROM ebTxs e\n\
  \LEFT JOIN txs t ON e.txHashBytes = t.txHashBytes\n\
  \WHERE (e.ebHashBytes, e.txOffset) IN (SELECT ebHashBytes, txOffset FROM mem.txPoints)\n\
  \ORDER BY e.txOffset ASC\n\
  \"

sql_flush_memTxPoints :: String
sql_flush_memTxPoints =
  "DELETE FROM mem.txPoints\n\
  \"

sql_attach_memTxPoints :: String
sql_attach_memTxPoints =
  "ATTACH DATABASE ':memory:' AS mem;\n\
  \\n\
  \CREATE TABLE mem.txPoints (\n\
  \    ebHashBytes INTEGER NOT NULL\n\
  \  ,\n\
  \    txOffset INTEGER NOT NULL\n\
  \  ,\n\
  \    PRIMARY KEY (ebHashBytes ASC, txOffset ASC)\n\
  \  ) WITHOUT ROWID;\n\
  \\n\
  \CREATE TABLE mem.txHashes (\n\
  \    txHashBytes BLOB NOT NULL PRIMARY KEY\n\
  \  ) WITHOUT ROWID;\n\
  \\n\
  \CREATE TABLE mem.ebHashes (\n\
  \    ebHashBytes BLOB NOT NULL PRIMARY KEY\n\
  \  ) WITHOUT ROWID;\n\
  \"

sqlQueryCompletedEbByPoint' :: String
sqlQueryCompletedEbByPoint' =
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
      (dbExec db (fromString "BEGIN IMMEDIATE"))
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

-- ** Error "handling"

maxBusyRetries :: Int
maxBusyRetries = 1000

-- | Execute a database action that may return an error. If the error is
-- 'DB.ErrorBusy', retry up to 'maxBusyRetries' times with a 1ms delay.
-- Otherwise and after exhausting retries, throws a 'LeiosDbException' with the
-- error message from the database.
withDie :: HasCallStack => DB.Database -> IO (Either DB.Error a) -> IO a
withDie db = go maxBusyRetries
 where
  go 0 io =
    io >>= \case
      Left e -> throwDbException db e
      Right x -> pure x
  go n io =
    io >>= \case
      Left DB.ErrorBusy -> threadDelay 1000 >> go (n - 1) io
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
