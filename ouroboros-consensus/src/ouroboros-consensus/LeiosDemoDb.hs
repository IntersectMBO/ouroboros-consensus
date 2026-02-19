{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LeiosDemoDb (module LeiosDemoDb) where

import Cardano.Prelude (foldM, forM_, maybeToList, when)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTChan
  , dupTChan
  , modifyTVar
  , newBroadcastTChan
  , newTVarIO
  , readTVar
  , writeTChan
  )
import Control.Exception (throwIO)
import Control.Monad.Class.MonadThrow
  ( bracket
  , generalBracket
  )
import qualified Control.Monad.Class.MonadThrow as MonadThrow
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import qualified Database.SQLite3.Direct as DB
import GHC.Stack (HasCallStack)
import qualified GHC.Stack
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
  )
import Ouroboros.Consensus.Util.IOLike (IOLike, atomically)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (die)

-- | High-level domain operations handle for Leios database.
-- This interface abstracts over both SQLite (production) and in-memory (testing) implementations.
--
-- NOTE: Each operation in the SQLite implementation is wrapped in its own
-- transaction (dbWithBEGIN). When multiple operations are called in sequence,
-- they are NOT in a single atomic transaction. This is a change from the
-- original low-level SQL code where multiple operations could be grouped in a
-- single BEGIN/COMMIT block. For use cases requiring multi-operation atomicity,
-- consider adding batch operations to this interface.
--
-- TODO: use LeiosPoint and LeiosEb types in the interface instead of raw tuples.
data LeiosDbHandle m = LeiosDbHandle
  { subscribeEbNotifications :: m (StrictTChan m LeiosNotification)
  -- ^ Subscribe to new EBs and EBTxs being stored by the LeiosDB. This will
  -- only inform about new additions, starting from when this function was
  -- called.
  , leiosDbScanEbPoints :: m [(SlotNo, EbHash)]
  , leiosDbLookupEbPoint :: EbHash -> m (Maybe SlotNo)
  -- ^ Check if an EB point exists in the database. Returns the slot if found.
  , leiosDbInsertEbPoint :: LeiosPoint -> m ()
  , leiosDbLookupEbBody :: EbHash -> m [(TxHash, BytesSize)]
  , -- NOTE: yields a LeiosOfferBlock notification
    leiosDbInsertEbBody :: LeiosPoint -> LeiosEb -> m ()
  , -- TODO: Take [LeiosTx] and hash on insert?
    leiosDbInsertTxs :: [(TxHash, ByteString)] -> m CompletedEbs
  -- ^ Insert transactions into the global txs table (INSERT OR IGNORE).
  -- After inserting, checks which EBs referencing these txs are now complete
  -- and emits LeiosOfferBlockTxs notifications for each.
  --
  -- NOTE: Duplicate notifications may be emitted if the same EB becomes
  -- complete via multiple insert batches (e.g., if txs are inserted twice).
  -- Consumers should handle notifications idempotently.
  --
  -- REVIEW: return type only used for tracing, necessary?
  , -- TODO: Return LeiosTx?
    leiosDbBatchRetrieveTxs :: EbHash -> [Int] -> m [(Int, TxHash, Maybe ByteString)]
  }

type CompletedEbs = [LeiosPoint]

-- * In-memory implementation of LeiosDbHandle

-- | In-memory database state
data InMemoryLeiosDb = InMemoryLeiosDb
  { imTxs :: !(Map TxHash (ByteString, BytesSize))
  -- ^ Global transaction storage (normalized)
  , imEbPoints :: !(IntMap {- SlotNo -} EbHash)
  , imEbBodies :: !(Map EbHash (IntMap {- txOffset -} EbTxEntry))
  , imEbSlots :: !(Map EbHash SlotNo)
  }

-- | EB transaction entry (references txs by hash, no bytes stored here)
data EbTxEntry = EbTxEntry
  { eteTxHash :: !TxHash
  , eteTxBytesSize :: !BytesSize
  }

-- | Create a new in-memory Leios database handle.
-- This is suitable for testing in IOSim.
newLeiosDBInMemory :: IOLike m => m (LeiosDbHandle m)
newLeiosDBInMemory = do
  notificationChan <- atomically newBroadcastTChan
  let notify = writeTChan notificationChan
  stateVar <-
    newTVarIO
      InMemoryLeiosDb
        { imTxs = Map.empty
        , imEbPoints = IntMap.empty
        , imEbBodies = Map.empty
        , imEbSlots = Map.empty
        }
  pure $
    LeiosDbHandle
      { subscribeEbNotifications =
          atomically (dupTChan notificationChan)
      , leiosDbScanEbPoints = atomically $ do
          state <- readTVar stateVar
          pure
            [ (SlotNo (fromIntegral slot), hash)
            | (slot, hash) <- IntMap.toAscList (imEbPoints state)
            ]
      , leiosDbLookupEbPoint = \ebHash -> atomically $ do
          state <- readTVar stateVar
          -- Find slot by looking for the hash in imEbPoints
          pure $
            foldr
              (\(slot, h) acc -> if h == ebHash then Just (SlotNo (fromIntegral slot)) else acc)
              Nothing
              (IntMap.toList (imEbPoints state))
      , leiosDbInsertEbPoint = \point -> atomically $ do
          modifyTVar stateVar $ \s ->
            s
              { imEbPoints =
                  IntMap.insert
                    (fromIntegral $ unSlotNo point.pointSlotNo)
                    point.pointEbHash
                    (imEbPoints s)
              }
      , leiosDbLookupEbBody = \ebHash -> atomically $ do
          state <- readTVar stateVar
          case Map.lookup ebHash (imEbBodies state) of
            Nothing -> pure []
            Just offsetMap ->
              pure
                [ (eteTxHash e, eteTxBytesSize e)
                | e <- IntMap.elems offsetMap
                ]
      , leiosDbInsertEbBody = \point eb -> do
          let items = leiosEbBodyItems eb
          when (null items) $
            error "leiosDbInsertEbBody: empty EB body (programmer error)"
          atomically $ do
            let entries =
                  IntMap.fromList
                    [ ( offset
                      , EbTxEntry
                          { eteTxHash = txHash
                          , eteTxBytesSize = size
                          }
                      )
                    | (offset, txHash, size) <- items
                    ]
            modifyTVar stateVar $ \s ->
              s
                { imEbBodies = Map.insert point.pointEbHash entries (imEbBodies s)
                , imEbSlots = Map.insert point.pointEbHash point.pointSlotNo (imEbSlots s)
                }
            notify $ LeiosOfferBlock point (leiosEbBytesSize eb)
      , leiosDbInsertTxs = \txs -> atomically $ do
          -- Insert all txs into global txs table
          let insertedTxHashes = [txHash | (txHash, _) <- txs]
          forM_ txs $ \(txHash, txBytes) -> do
            let txBytesSize = fromIntegral $ BS.length txBytes
            modifyTVar stateVar $ \s ->
              -- INSERT OR IGNORE semantics: only insert if not present
              if Map.member txHash (imTxs s)
                then s
                else s{imTxs = Map.insert txHash (txBytes, txBytesSize) (imTxs s)}
          -- Check which EBs are now complete and emit notifications
          state <- readTVar stateVar
          -- Find all EBs that were completed by the inserted txHashes
          let completed =
                [ MkLeiosPoint slot ebHash
                | (ebHash, entries) <- Map.toList (imEbBodies state)
                , any (\e -> eteTxHash e `elem` insertedTxHashes) (IntMap.elems entries)
                , all (\e -> Map.member (eteTxHash e) (imTxs state)) (IntMap.elems entries)
                , slot <- maybeToList $ Map.lookup ebHash (imEbSlots state)
                ]
          forM_ completed $ notify . LeiosOfferBlockTxs
          pure completed
      , leiosDbBatchRetrieveTxs = \ebHash offsets -> atomically $ do
          state <- readTVar stateVar
          case Map.lookup ebHash (imEbBodies state) of
            Nothing -> pure []
            Just offsetMap ->
              pure
                [ (offset, eteTxHash entry, fst <$> Map.lookup (eteTxHash entry) (imTxs state))
                | offset <- offsets
                , Just entry <- [IntMap.lookup offset offsetMap]
                ]
      }

-- * SQLite implementation of LeiosDbHandle

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
-- NOTE: All call sites of the handle will share a single database connection.
-- This might be undesired if we switch to WAL mode where readers would not be
-- blocked by writers.
newLeiosDBSQLite :: FilePath -> IO (LeiosDbHandle IO)
newLeiosDBSQLite dbPath = do
  -- TODO: not leak resources (the db connection)
  shouldInitSchema <- not <$> doesFileExist dbPath
  db <- withDieMsg $ DB.open (fromString dbPath)
  when shouldInitSchema $
    dbExec db (fromString sql_schema)
  dbExec db (fromString sql_attach_memTxPoints)

  notificationChan <- atomically newBroadcastTChan
  let notify = atomically . writeTChan notificationChan

  pure
    LeiosDbHandle
      { subscribeEbNotifications =
          atomically (dupTChan notificationChan)
      , leiosDbScanEbPoints =
          dbWithBEGIN db $ dbWithPrepare db (fromString sql_scan_ebPoints) $ \stmt -> do
            let loop acc =
                  dbStep stmt >>= \case
                    DB.Done -> pure (reverse acc)
                    DB.Row -> do
                      slot <- SlotNo . fromIntegral <$> DB.columnInt64 stmt 0
                      hash <- MkEbHash <$> DB.columnBlob stmt 1
                      loop ((slot, hash) : acc)
            loop []
      , leiosDbLookupEbPoint = \ebHash ->
          dbWithBEGIN db $ dbWithPrepare db (fromString sql_lookup_ebPoint) $ \stmt -> do
            dbBindBlob stmt 1 (let MkEbHash bytes = ebHash in bytes)
            dbStep stmt >>= \case
              DB.Done -> pure Nothing
              DB.Row -> do
                slot <- SlotNo . fromIntegral <$> DB.columnInt64 stmt 0
                pure (Just slot)
      , leiosDbInsertEbPoint = \point ->
          dbWithBEGIN db $ dbWithPrepare db (fromString sql_insert_ebPoint) $ \stmt -> do
            dbBindInt64 stmt 1 (fromIntegral $ unSlotNo point.pointSlotNo)
            dbBindBlob stmt 2 point.pointEbHash.ebHashBytes
            dbStep1 stmt
      , leiosDbLookupEbBody = \ebHash ->
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
      , leiosDbInsertEbBody = \point eb -> do
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
      , leiosDbInsertTxs = \txs -> do
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
      , leiosDbBatchRetrieveTxs = \ebHash offsets -> do
          -- First, insert offsets into temp table
          dbWithPrepare db (fromString sql_insert_memTxPoints) $ \stmtInsert -> do
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
          dbWithPrepare db (fromString sql_flush_memTxPoints) $ \stmtFlush -> do
            dbStep1 stmtFlush
          pure results
      }

sql_schema :: String
sql_schema =
  "CREATE TABLE ebPoints (\n\
  \    ebSlot INTEGER NOT NULL\n\
  \  ,\n\
  \    ebHashBytes BLOB NOT NULL\n\
  \  ,\n\
  \    PRIMARY KEY (ebSlot, ebHashBytes)\n\
  \  ) WITHOUT ROWID;\n\
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
  \  ) WITHOUT ROWID;\n\
  \CREATE INDEX idx_ebTxs_txHashBytes ON ebTxs(txHashBytes);\n\
  \CREATE TABLE txs (\n\
  \    txHashBytes BLOB NOT NULL PRIMARY KEY\n\
  \  ,\n\
  \    txBytes BLOB NOT NULL\n\
  \  ,\n\
  \    txBytesSize INTEGER NOT NULL\n\
  \  ) WITHOUT ROWID;\n\
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
  "INSERT OR IGNORE INTO ebPoints (ebSlot, ebHashBytes) VALUES (?, ?)"

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
  \"

-- * Low-level terminating SQLite functions

dbBindBlob :: HasCallStack => DB.Statement -> DB.ParamIndex -> ByteString -> IO ()
dbBindBlob q p v = withDie $ DB.bindBlob q p v

dbBindInt64 :: HasCallStack => DB.Statement -> DB.ParamIndex -> Int64 -> IO ()
dbBindInt64 q p v = withDie $ DB.bindInt64 q p v

dbExec :: HasCallStack => DB.Database -> DB.Utf8 -> IO ()
dbExec db q = withDieMsg $ DB.exec db q

dbFinalize :: HasCallStack => DB.Statement -> IO ()
dbFinalize q = withDie $ DB.finalize q

dbPrepare :: HasCallStack => DB.Database -> DB.Utf8 -> IO DB.Statement
dbPrepare db q = withDieJust $ DB.prepare db q

dbWithPrepare :: HasCallStack => DB.Database -> DB.Utf8 -> (DB.Statement -> IO a) -> IO a
dbWithPrepare db q k = bracket (dbPrepare db q) dbFinalize k

dbWithBEGIN :: DB.Database -> IO a -> IO a
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
dbReset stmt = withDie $ DB.reset stmt

dbStep :: HasCallStack => DB.Statement -> IO DB.StepResult
dbStep stmt = withDie $ DB.stepNoCB stmt

dbStep1 :: HasCallStack => DB.Statement -> IO ()
dbStep1 stmt = withDieDone $ DB.stepNoCB stmt

withDiePoly :: (HasCallStack, Show b) => (e -> b) -> IO (Either e a) -> IO a
withDiePoly f io =
  io >>= \case
    Left e -> dieStack $ "LeiosDb: " ++ show (f e)
    Right x -> pure x

withDieMsg :: HasCallStack => IO (Either (DB.Error, DB.Utf8) a) -> IO a
withDieMsg = withDiePoly snd

withDie :: HasCallStack => IO (Either DB.Error a) -> IO a
withDie = withDiePoly id

withDieJust :: HasCallStack => IO (Either DB.Error (Maybe a)) -> IO a
withDieJust io =
  withDie io >>= \case
    Nothing -> dieStack $ "LeiosDb: [Just] " ++ "impossible!"
    Just x -> pure x

withDieDone :: HasCallStack => IO (Either DB.Error DB.StepResult) -> IO ()
withDieDone io =
  withDie io >>= \case
    DB.Row -> dieStack $ "LeiosDb: [Done] " ++ "impossible!"
    DB.Done -> pure ()

dieStack :: HasCallStack => String -> IO a
dieStack s = throwIO $ LeiosDbException s (GHC.Stack.prettyCallStack GHC.Stack.callStack)
