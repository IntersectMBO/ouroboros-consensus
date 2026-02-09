{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LeiosDemoDb (module LeiosDemoDb) where

import Cardano.Prelude (forM_, unless, when)
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
import Control.Monad.Class.MonadThrow
  ( bracket
  , generalBracket
  )
import qualified Control.Monad.Class.MonadThrow as MonadThrow
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.String (IsString, fromString)
import Database.SQLite3 (execPrint)
import qualified Database.SQLite3.Direct as DB
import GHC.Stack (HasCallStack)
import qualified GHC.Stack
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
import Ouroboros.Consensus.Util.IOLike
  ( DiffTime
  , IOLike
  , MonadMonotonicTime (getMonotonicTime)
  , atomically
  , diffTime
  , getMonotonicTime
  )
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
  , leiosDbInsertEbPoint :: LeiosPoint -> m ()
  , leiosDbLookupEbBody :: EbHash -> m [(TxHash, BytesSize)]
  , -- NOTE: yields a LeiosOfferBlock notification
    leiosDbInsertEbBody :: LeiosPoint -> LeiosEb -> m ()
  , -- NOTE: yields a LeiosOfferBlockTxs notification once complete
    leiosDbUpdateEbTx :: EbHash -> [(Int, ByteString)] -> m ()
  , leiosDbInsertTxCache :: TxHash -> ByteString -> BytesSize -> Int64 -> m ()
  , leiosDbBatchRetrieveTxs :: EbHash -> [Int] -> m [(Int, TxHash, Maybe ByteString)]
  , -- TODO: Avoid needing this function
    leiosDbCopyFromTxCacheBatch ::
      Map LeiosPoint (IntMap BytesSize) ->
      BytesSize ->
      m (Map LeiosPoint IntSet)
  }

-- * In-memory implementation of LeiosDbHandle

-- | In-memory database state
data InMemoryLeiosDb = InMemoryLeiosDb
  { imTxCache :: !(Map TxHash TxCacheEntry)
  , imEbPoints :: !(IntMap {- SlotNo -} EbHash)
  , imEbBodies :: !(Map EbHash (IntMap {- txOffset -} EbTxEntry))
  , imEbSlots :: !(Map EbHash SlotNo)
  }

-- | Transaction cache entry
data TxCacheEntry = TxCacheEntry
  { tceBytes :: !ByteString
  , tceBytesSize :: !BytesSize
  , tceExpiry :: !Int64
  }

-- | EB transaction entry
data EbTxEntry = EbTxEntry
  { eteTxHash :: !TxHash
  , eteTxBytesSize :: !BytesSize
  , eteTxBytes :: !(Maybe ByteString) -- NULL initially, filled later
  }

-- | Create a new in-memory Leios database handle.
-- This is suitable for testing in IOSim.
newInMemoryLeiosDb :: IOLike m => m (LeiosDbHandle m)
newInMemoryLeiosDb = do
  notificationChan <- atomically newBroadcastTChan
  let notify = writeTChan notificationChan
  stateVar <-
    newTVarIO
      InMemoryLeiosDb
        { imTxCache = Map.empty
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
      , leiosDbInsertEbBody = \point eb -> atomically $ do
          let entries =
                IntMap.fromList
                  [ ( offset
                    , EbTxEntry
                        { eteTxHash = txHash
                        , eteTxBytesSize = size
                        , eteTxBytes = Nothing
                        }
                    )
                  | (offset, txHash, size) <- leiosEbBodyItems eb
                  ]
          modifyTVar stateVar $ \s ->
            s
              { imEbBodies = Map.insert point.pointEbHash entries (imEbBodies s)
              , imEbSlots = Map.insert point.pointEbHash point.pointSlotNo (imEbSlots s)
              }
          notify $ LeiosOfferBlock point (leiosEbBytesSize eb)
      , leiosDbUpdateEbTx = \ebHash items -> atomically $ do
          forM_ items $ \(txOffset, txBytes) ->
            modifyTVar stateVar $ \s ->
              s
                { imEbBodies =
                    Map.adjust
                      (IntMap.adjust (\e -> e{eteTxBytes = Just txBytes}) txOffset)
                      ebHash
                      (imEbBodies s)
                }
          state <- readTVar stateVar
          case Map.lookup ebHash (imEbBodies state) of
            Just entries
              | not (IntMap.null entries)
              , all (isJust . eteTxBytes) entries ->
                  case Map.lookup ebHash (imEbSlots state) of
                    Just slotNo ->
                      notify $ LeiosOfferBlockTxs (MkLeiosPoint slotNo ebHash)
                    Nothing -> pure ()
            _ -> pure ()
      , leiosDbInsertTxCache = \txHash txBytes txBytesSize expiry -> atomically $ do
          let entry = TxCacheEntry txBytes txBytesSize expiry
          modifyTVar stateVar $ \s ->
            s
              { imTxCache = Map.insert txHash entry (imTxCache s)
              }
      , leiosDbBatchRetrieveTxs = \ebHash offsets -> atomically $ do
          state <- readTVar stateVar
          case Map.lookup ebHash (imEbBodies state) of
            Nothing -> pure []
            Just offsetMap ->
              pure
                [ (offset, eteTxHash entry, eteTxBytes entry)
                | offset <- offsets
                , Just entry <- [IntMap.lookup offset offsetMap]
                ]
      , leiosDbCopyFromTxCacheBatch = \_toCopy _bytesLimit ->
          error "TODO: implement leiosDbCopyFromTxCacheBatch"
      }

-- * SQLite implementation of LeiosDbHandle

-- | Create a new Leios database connection from environment variable.
-- This looks up the LEIOS_DB_PATH environment variable and opens the database.
demoNewLeiosDbConnectionIO :: IO (LeiosDbHandle IO)
demoNewLeiosDbConnectionIO = do
  dbPath <-
    lookupEnv "LEIOS_DB_PATH" >>= \case
      Nothing -> die "You must define the LEIOS_DB_PATH variable for this demo."
      Just x -> pure x
  newLeiosDbConnectionIO dbPath

-- | Create a new Leios database connection from a file path.
-- Opens the SQLite database and attaches the in-memory temp table.
newLeiosDbConnectionIO :: FilePath -> IO (LeiosDbHandle IO)
newLeiosDbConnectionIO dbPath = do
  shouldInitDb <- not <$> doesFileExist dbPath
  db <- withDieMsg $ DB.open (fromString dbPath)
  when shouldInitDb $ do
    dbExec db (fromString sql_schema)
  dbExec db sql_pragmas
  dbExec db (fromString sql_attach_memTxPoints)
  newLeiosDbFromSqlite db

-- | Create a LeiosDbHandle from a low-level SQLite LeiosDb.
-- This allows production code to continue using SQLite while tests use in-memory.
newLeiosDbFromSqlite :: DB.Database -> IO (LeiosDbHandle IO)
newLeiosDbFromSqlite db = do
  notificationChan <- atomically newBroadcastTChan
  let notify = atomically . writeTChan notificationChan
  slotsVar <- newTVarIO Map.empty
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
              (leiosEbBodyItems eb)
          atomically $ modifyTVar slotsVar $ Map.insert point.pointEbHash point.pointSlotNo
          notify $ LeiosOfferBlock point (leiosEbBytesSize eb)
      , leiosDbUpdateEbTx = \ebHash items -> do
          start <- getMonotonicTime
          let printSinceStart str = do
                t <- getMonotonicTime
                putStrLn $ str <> ": " <> showTime (diffTime t start)
          putStrLn ""
          anyMissing <- dbWithBEGIN db $ do
            printSinceStart "In transaction"
            dbWithPrepare db (fromString sql_update_ebTx) $ \stmt -> do
              printSinceStart "Before loop"
              printSinceStart $ "Loop of" <> show (length items)
              forM_ items $ \(txOffset, txBytes) -> do
                dbReset stmt
                dbBindBlob stmt 1 txBytes
                dbBindBlob stmt 2 (let MkEbHash bytes = ebHash in bytes)
                dbBindInt64 stmt 3 (fromIntegral txOffset)
                dbStep1 stmt
              printSinceStart "After loop"
            -- Check whether any txs missing
            printSinceStart "Before completness check"
            execPrint db ("EXPLAIN QUERY PLAN " <> sql_select_ebTxs_missing)
            res <- dbWithPrepare db sql_select_ebTxs_missing $ \stmt -> do
              dbBindBlob stmt 1 ebHash.ebHashBytes
              dbStep stmt >>= \case
                DB.Done -> putStrLn "done" >> pure False
                DB.Row -> putStrLn "not done" >> pure True
            printSinceStart "After completness check"
            pure res
          printSinceStart "End"
          -- XXX: avoid needing this slotsVar, can we pass in the point? otherwise
          -- use the db to look-up
          unless anyMissing $ do
            mSlotNo <- atomically $ Map.lookup ebHash <$> readTVar slotsVar
            case mSlotNo of
              Just slotNo ->
                notify $ LeiosOfferBlockTxs (MkLeiosPoint slotNo ebHash)
              Nothing -> pure ()
      , leiosDbInsertTxCache = \txHash txBytes txBytesSize expiry ->
          dbWithBEGIN db $ dbWithPrepare db (fromString sql_insert_txCache) $ \stmt -> do
            dbBindBlob stmt 1 (let MkTxHash bytes = txHash in bytes)
            dbBindBlob stmt 2 txBytes
            dbBindInt64 stmt 3 (fromIntegral txBytesSize)
            dbBindInt64 stmt 4 expiry
            dbStep1 stmt
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

          -- Then retrieve from ebTxs
          results <- dbWithPrepare db (fromString sql_retrieve_from_ebTxs) $ \stmtRetrieve -> do
            let loop acc =
                  dbStep stmtRetrieve >>= \case
                    DB.Done -> pure (reverse acc)
                    DB.Row -> do
                      offset <- fromIntegral <$> DB.columnInt64 stmtRetrieve 0
                      txHash <- MkTxHash <$> DB.columnBlob stmtRetrieve 1
                      -- Column 2 might be NULL
                      -- For now, we'll just get the blob (SQLite returns empty ByteString for NULL)
                      -- TODO: handle NULL properly
                      txBytes <- DB.columnBlob stmtRetrieve 2
                      let mbTxBytes = if txBytes == mempty then Nothing else Just txBytes
                      loop ((offset, txHash, mbTxBytes) : acc)
            loop []

          -- Flush temp table
          dbWithPrepare db (fromString sql_flush_memTxPoints) $ \stmtFlush -> do
            dbStep1 stmtFlush
          pure results
      , leiosDbCopyFromTxCacheBatch = \toCopy bytesLimit -> do
          dbWithPrepare db (fromString sql_copy_from_txCache) $ \stmt -> do
            let
              go1 !accCopied !accBytes !remaining
                | accBytes < bytesLimit
                , Just ((point :: LeiosPoint, txs), remaining') <- Map.maxViewWithKey remaining =
                    go2 accCopied accBytes remaining' point IntSet.empty txs
                | otherwise = pure accCopied

              go2 !accCopied !accBytes !remaining point !copiedPoint txs
                | Just ((offset, txBytesSize), txs') <- IntMap.minViewWithKey txs =
                    if accBytes + txBytesSize > bytesLimit
                      then pure accCopied'
                      else do
                        dbBindBlob stmt 1 point.pointEbHash.ebHashBytes
                        dbBindInt64 stmt 2 (fromIntegral offset)
                        dbStep1 stmt
                        dbReset stmt
                        go2 accCopied (accBytes + txBytesSize) remaining point (IntSet.insert offset copiedPoint) txs'
                | otherwise = go1 accCopied' accBytes remaining
               where
                accCopied' = if IntSet.null copiedPoint then accCopied else Map.insert point copiedPoint accCopied

            go1 Map.empty 0 toCopy
      }

sql_pragmas :: IsString s => s
sql_pragmas =
  "PRAGMA journal_mode = WAL;\n\
  \PRAGMA synchronous = normal;\n\
  \PRAGMA journal_size_limit = 6144000;"

sql_schema :: String
sql_schema =
  "CREATE TABLE txCache (\n\
  \    txHashBytes BLOB NOT NULL PRIMARY KEY   -- raw bytes\n\
  \  ,\n\
  \    txBytes BLOB NOT NULL   -- valid CBOR\n\
  \  ,\n\
  \    txBytesSize INTEGER NOT NULL\n\
  \  ,\n\
  \    expiryUnixEpoch INTEGER NOT NULL\n\
  \  ) WITHOUT ROWID;\n\
  \CREATE TABLE ebPoints (\n\
  \    ebSlot INTEGER NOT NULL\n\
  \  ,\n\
  \    ebHashBytes BLOB NOT NULL\n\
  \  ,\n\
  \    PRIMARY KEY (ebSlot, ebHashBytes)\n\
  \  ) WITHOUT ROWID;\n\
  \CREATE TABLE ebTxs (\n\
  \    ebHashBytes BLOB NOT NULL   -- foreign key ebPoints.ebHashBytes\n\
  \  ,\n\
  \    txOffset INTEGER NOT NULL\n\
  \  ,\n\
  \    txHashBytes BLOB NOT NULL   -- raw bytes\n\
  \  ,\n\
  \    txBytesSize INTEGER NOT NULL\n\
  \  ,\n\
  \    txBytes BLOB   -- valid CBOR\n\
  \  ,\n\
  \    PRIMARY KEY (ebHashBytes, txOffset)\n\
  \  ) WITHOUT ROWID;\n\
  \"

sql_scan_ebPoints :: String
sql_scan_ebPoints =
  "SELECT ebSlot, ebHashBytes\n\
  \FROM ebPoints\n\
  \ORDER BY ebSlot ASC\n\
  \"

sql_insert_ebPoint :: String
sql_insert_ebPoint =
  "INSERT INTO ebPoints (ebSlot, ebHashBytes) VALUES (?, ?)"

sql_lookup_ebBodies :: String
sql_lookup_ebBodies =
  "SELECT txHashBytes, txBytesSize FROM ebTxs\n\
  \WHERE ebHashBytes = ?\n\
  \ORDER BY txOffset ASC\n\
  \"

sql_insert_ebBody :: String
sql_insert_ebBody =
  "INSERT INTO ebTxs (ebHashBytes, txOffset, txHashBytes, txBytesSize, txBytes) VALUES (?, ?, ?, ?, NULL)\n\
  \"

sql_update_ebTx :: String
sql_update_ebTx =
  "UPDATE ebTxs\n\
  \SET txBytes = ?\n\
  \WHERE ebHashBytes = ? AND txOffset = ?\n\
  \"

sql_select_ebTxs_missing :: IsString s => s
sql_select_ebTxs_missing =
  "SELECT 1 FROM ebTxs\n\
  \WHERE ebHashBytes = ? AND txBytes IS NULL\n\
  \LIMIT 1\n\
  \"

sql_insert_txCache :: String
sql_insert_txCache =
  "INSERT INTO txCache (txHashBytes, txBytes, txBytesSize, expiryUnixEpoch) VALUES (?, ?, ?, ?)\n\
  \"

sql_insert_memTxPoints :: String
sql_insert_memTxPoints =
  "INSERT INTO mem.txPoints (ebHashBytes, txOffset) VALUES (?, ?)\n\
  \"

sql_retrieve_from_ebTxs :: String
sql_retrieve_from_ebTxs =
  "SELECT txOffset, txHashBytes, txBytes FROM ebTxs\n\
  \WHERE (ebHashBytes, txOffset) IN (SELECT ebHashBytes, txOffset FROM mem.txPoints)\n\
  \ORDER BY txOffset ASC\n\
  \"

sql_flush_memTxPoints :: String
sql_flush_memTxPoints =
  "DELETE FROM mem.txPoints\n\
  \"

sql_copy_from_txCache :: String
sql_copy_from_txCache =
  "UPDATE ebTxs\n\
  \SET txBytes = (SELECT txBytes FROM txCache WHERE txCache.txHashBytes = ebTxs.txHashBytes)\n\
  \WHERE ebHashBytes = ? AND txOffset = ? AND txBytes IS NULL\n\
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
dieStack s = die $ s ++ "\n\n" ++ GHC.Stack.prettyCallStack GHC.Stack.callStack

showTime :: DiffTime -> String
showTime t
  | t < micro 1 = show (s * 1_000_000_000) <> "ns"
  | t < milli 1 = show (s * 1_000_000) <> "μs"
  | t < 1 = show (s * 1_000) <> "ms"
  | otherwise = show t
 where
  s = realToFrac t :: Double

milli :: Integer -> DiffTime
milli x = fromIntegral x / 1000

micro :: Integer -> DiffTime
micro x = fromIntegral x / 1_000_000
