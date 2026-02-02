{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LeiosDemoDb
  ( -- * High-level database interface
    LeiosDbHandle (..)
  , newInMemoryLeiosDb
  , leiosDbHandleFromSqlite

    -- * SQLite connection helpers
  , demoNewLeiosDbConnectionIO
  , newLeiosDbConnectionIO

    -- * In-memory state types
  , InMemoryLeiosDb (..)
  , TxCacheEntry (..)
  , EbTxEntry (..)
  ) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.Class.MonadSTM.Strict
  ( modifyTVar
  , newTVarIO
  , readTVar
  , writeTVar
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
import Data.String (fromString)
import qualified Database.SQLite3.Direct as DB
import GHC.Stack (HasCallStack)
import qualified GHC.Stack
import LeiosDemoTypes
  ( BytesSize
  , EbHash (..)
  , EbId (..)
  , TxHash (..)
  , fromIntegralEbId
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
  { -- EB Points operations
    leiosDbScanEbPoints :: m [(SlotNo, EbHash, EbId)]
  , leiosDbInsertEbPoint :: SlotNo -> EbHash -> EbId -> m ()
  , -- EB Bodies operations
    leiosDbLookupEbBody :: EbId -> m [(TxHash, BytesSize)]
  , leiosDbInsertEbBody :: EbId -> [(Int, TxHash, BytesSize)] -> m ()
  , -- Transaction operations
    leiosDbUpdateEbTx :: EbId -> Int -> ByteString -> m ()
  , leiosDbInsertTxCache :: TxHash -> ByteString -> BytesSize -> Int64 -> m ()
  , -- Batch operations (using temp table in SQL, simple lookup in memory)
    leiosDbBatchRetrieveTxs :: EbId -> [Int] -> m [(Int, TxHash, Maybe ByteString)]
  , -- Copy from txCache to ebTxs in batch
    -- TODO: Avoid needing this function
    leiosDbCopyFromTxCacheBatch :: Map EbId (IntMap BytesSize) -> BytesSize -> m (Map EbId IntSet)
  }

-- * In-memory implementation of LeiosDbHandle

-- | In-memory database state
data InMemoryLeiosDb = InMemoryLeiosDb
  { imTxCache :: !(Map TxHash TxCacheEntry)
  , imEbPoints :: !(IntMap (Map EbHash EbId)) -- slot -> (hash -> ebId)
  , imEbPointsInverse :: !(IntMap EbHash) -- ebId -> hash
  , imEbBodies :: !(IntMap (IntMap EbTxEntry)) -- ebId -> (txOffset -> entry)
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
  stateVar <-
    newTVarIO
      InMemoryLeiosDb
        { imTxCache = Map.empty
        , imEbPoints = IntMap.empty
        , imEbPointsInverse = IntMap.empty
        , imEbBodies = IntMap.empty
        }

  pure $
    LeiosDbHandle
      { leiosDbScanEbPoints = atomically $ do
          state <- readTVar stateVar
          pure
            [ (SlotNo (fromIntegral slot), hash, ebId)
            | (slot, hashMap) <- IntMap.toAscList (imEbPoints state)
            , (hash, ebId) <- Map.toList hashMap
            ]
      , leiosDbInsertEbPoint = \slot hash ebId -> atomically $ do
          modifyTVar stateVar $ \s ->
            s
              { imEbPoints =
                  IntMap.insertWith
                    Map.union
                    (fromIntegral $ unSlotNo slot)
                    (Map.singleton hash ebId)
                    (imEbPoints s)
              , imEbPointsInverse = IntMap.insert (fromIntegralEbId ebId) hash (imEbPointsInverse s)
              }
      , leiosDbLookupEbBody = \ebId -> atomically $ do
          state <- readTVar stateVar
          case IntMap.lookup (fromIntegralEbId ebId) (imEbBodies state) of
            Nothing -> pure []
            Just offsetMap ->
              pure
                [ (eteTxHash e, eteTxBytesSize e)
                | e <- IntMap.elems offsetMap
                ]
      , leiosDbInsertEbBody = \ebId items -> atomically $ do
          let entries =
                IntMap.fromList
                  [ ( offset
                    , EbTxEntry
                        { eteTxHash = txHash
                        , eteTxBytesSize = size
                        , eteTxBytes = Nothing
                        }
                    )
                  | (offset, txHash, size) <- items
                  ]
          modifyTVar stateVar $ \s ->
            s
              { imEbBodies = IntMap.insert (fromIntegralEbId ebId) entries (imEbBodies s)
              }
      , leiosDbUpdateEbTx = \ebId txOffset txBytes -> atomically $ do
          modifyTVar stateVar $ \s ->
            s
              { imEbBodies =
                  IntMap.adjust
                    (IntMap.adjust (\e -> e{eteTxBytes = Just txBytes}) txOffset)
                    (fromIntegralEbId ebId)
                    (imEbBodies s)
              }
      , leiosDbInsertTxCache = \txHash txBytes txBytesSize expiry -> atomically $ do
          let entry = TxCacheEntry txBytes txBytesSize expiry
          modifyTVar stateVar $ \s ->
            s
              { imTxCache = Map.insert txHash entry (imTxCache s)
              }
      , leiosDbBatchRetrieveTxs = \ebId offsets -> atomically $ do
          state <- readTVar stateVar
          case IntMap.lookup (fromIntegralEbId ebId) (imEbBodies state) of
            Nothing -> pure []
            Just offsetMap ->
              pure
                [ (offset, eteTxHash entry, eteTxBytes entry)
                | offset <- offsets
                , Just entry <- [IntMap.lookup offset offsetMap]
                ]
      , leiosDbCopyFromTxCacheBatch = \toCopy bytesLimit -> atomically $ do
          -- REVIEW: I'm doubtful this is correct
          state <- readTVar stateVar
          -- Process toCopy map up to bytesLimit
          let go !accCopied !accBytes !remaining
                | accBytes >= bytesLimit = pure (accCopied, state)
                | Just ((ebId, txs), remaining') <- Map.maxViewWithKey remaining =
                    let processOffsets !copiedEbId' !bytes !offsetMap
                          | bytes >= bytesLimit = (copiedEbId', bytes, state)
                          | Just ((offset, txBytesSize), offsetMap') <- IntMap.minViewWithKey offsetMap =
                              case IntMap.lookup (fromIntegralEbId ebId) (imEbBodies state) of
                                Nothing -> processOffsets copiedEbId' bytes offsetMap'
                                Just ebEntries -> case IntMap.lookup offset ebEntries of
                                  Nothing -> processOffsets copiedEbId' bytes offsetMap'
                                  Just entry -> case Map.lookup (eteTxHash entry) (imTxCache state) of
                                    Nothing -> processOffsets copiedEbId' bytes offsetMap'
                                    Just _cacheEntry ->
                                      if bytes + txBytesSize > bytesLimit
                                        then (copiedEbId', bytes, state)
                                        else
                                          -- let updatedEntry = entry{eteTxBytes = Just (tceBytes cacheEntry)}
                                          --     updatedEbEntries = IntMap.insert offset updatedEntry ebEntries
                                          --     updatedState = state{imEbBodies = IntMap.insert (fromIntegralEbId ebId) updatedEbEntries (imEbBodies state)}
                                          processOffsets (IntSet.insert offset copiedEbId') (bytes + txBytesSize) offsetMap'
                          | otherwise = (copiedEbId', bytes, state)
                        (copiedEbId, newBytes, _newState) = processOffsets IntSet.empty accBytes txs
                        accCopied' = if IntSet.null copiedEbId then accCopied else Map.insert ebId copiedEbId accCopied
                     in go accCopied' newBytes remaining'
                | otherwise = pure (accCopied, state)
          (copied, finalState) <- go Map.empty 0 toCopy
          writeTVar stateVar finalState
          pure copied
      }

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

-- * SQLite implementation of LeiosDbHandle

sql_scan_ebId :: String
sql_scan_ebId =
  "SELECT ebSlot, ebHashBytes, ebId\n\
  \FROM ebPoints\n\
  \ORDER BY ebId ASC\n\
  \"

sql_lookup_ebBodies :: String
sql_lookup_ebBodies =
  "SELECT txHashBytes, txBytesSize FROM ebTxs\n\
  \WHERE ebId = ?\n\
  \ORDER BY txOffset ASC\n\
  \"

sql_insert_ebBody :: String
sql_insert_ebBody =
  "INSERT INTO ebTxs (ebId, txOffset, txHashBytes, txBytesSize, txBytes) VALUES (?, ?, ?, ?, NULL)\n\
  \"

sql_update_ebTx :: String
sql_update_ebTx =
  "UPDATE ebTxs\n\
  \SET txBytes = ?\n\
  \WHERE ebId = ? AND txOffset = ?\n\
  \"

sql_insert_txCache :: String
sql_insert_txCache =
  "INSERT INTO txCache (txHashBytes, txBytes, txBytesSize, expiryUnixEpoch) VALUES (?, ?, ?, ?)\n\
  \"

sql_insert_memTxPoints :: String
sql_insert_memTxPoints =
  "INSERT INTO mem.txPoints (ebId, txOffset) VALUES (?, ?)\n\
  \"

sql_retrieve_from_ebTxs :: String
sql_retrieve_from_ebTxs =
  "SELECT txOffset, txHashBytes, txBytes FROM ebTxs\n\
  \WHERE (ebId, txOffset) IN (SELECT ebId, txOffset FROM mem.txPoints)\n\
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
  \WHERE ebId = ? AND txOffset = ? AND txBytes IS NULL\n\
  \"

-- | Create a LeiosDbHandle from a low-level SQLite LeiosDb.
-- This allows production code to continue using SQLite while tests use in-memory.
leiosDbHandleFromSqlite :: DB.Database -> LeiosDbHandle IO
leiosDbHandleFromSqlite db =
  LeiosDbHandle
    { leiosDbScanEbPoints =
        dbWithBEGIN db $ dbWithPrepare db (fromString sql_scan_ebId) $ \stmt -> do
          let loop acc =
                dbStep stmt >>= \case
                  DB.Done -> pure (reverse acc)
                  DB.Row -> do
                    slot <- SlotNo . fromIntegral <$> DB.columnInt64 stmt 0
                    hash <- MkEbHash <$> DB.columnBlob stmt 1
                    ebId <- MkEbId . fromIntegral <$> DB.columnInt64 stmt 2
                    loop ((slot, hash, ebId) : acc)
          loop []
    , leiosDbInsertEbPoint = \slot hash ebId ->
        dbWithBEGIN db $ dbWithPrepare db (fromString "INSERT INTO ebPoints (ebSlot, ebHashBytes, ebId) VALUES (?, ?, ?)") $ \stmt -> do
          dbBindInt64 stmt 1 (fromIntegral $ unSlotNo slot)
          dbBindBlob stmt 2 (let MkEbHash bytes = hash in bytes)
          dbBindInt64 stmt 3 (fromIntegralEbId ebId)
          dbStep1 stmt
    , leiosDbLookupEbBody = \ebId ->
        dbWithBEGIN db $ dbWithPrepare db (fromString sql_lookup_ebBodies) $ \stmt -> do
          dbBindInt64 stmt 1 (fromIntegralEbId ebId)
          let loop acc =
                dbStep stmt >>= \case
                  DB.Done -> pure (reverse acc)
                  DB.Row -> do
                    txHash <- MkTxHash <$> DB.columnBlob stmt 0
                    size <- fromIntegral <$> DB.columnInt64 stmt 1
                    loop ((txHash, size) : acc)
          loop []
    , leiosDbInsertEbBody = \ebId items ->
        dbWithBEGIN db $ dbWithPrepare db (fromString sql_insert_ebBody) $ \stmt -> do
          mapM_
            ( \(txOffset, txHash, txBytesSize) -> do
                dbBindInt64 stmt 1 (fromIntegralEbId ebId)
                dbBindInt64 stmt 2 (fromIntegral txOffset)
                dbBindBlob stmt 3 (let MkTxHash bytes = txHash in bytes)
                dbBindInt64 stmt 4 (fromIntegral txBytesSize)
                dbStep1 stmt
                dbReset stmt
            )
            items
    , leiosDbUpdateEbTx = \ebId txOffset txBytes ->
        dbWithBEGIN db $ dbWithPrepare db (fromString sql_update_ebTx) $ \stmt -> do
          dbBindBlob stmt 1 txBytes
          dbBindInt64 stmt 2 (fromIntegralEbId ebId)
          dbBindInt64 stmt 3 (fromIntegral txOffset)
          dbStep1 stmt
    , leiosDbInsertTxCache = \txHash txBytes txBytesSize expiry ->
        dbWithBEGIN db $ dbWithPrepare db (fromString sql_insert_txCache) $ \stmt -> do
          dbBindBlob stmt 1 (let MkTxHash bytes = txHash in bytes)
          dbBindBlob stmt 2 txBytes
          dbBindInt64 stmt 3 (fromIntegral txBytesSize)
          dbBindInt64 stmt 4 expiry
          dbStep1 stmt
    , leiosDbBatchRetrieveTxs = \ebId offsets -> do
        -- First, insert offsets into temp table
        dbWithPrepare db (fromString sql_insert_memTxPoints) $ \stmtInsert -> do
          mapM_
            ( \offset -> do
                dbBindInt64 stmtInsert 1 (fromIntegralEbId ebId)
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
          let go1 !accCopied !accBytes !remaining
                | accBytes < bytesLimit
                , Just ((ebId, txs), remaining') <- Map.maxViewWithKey remaining =
                    go2 accCopied accBytes remaining' ebId IntSet.empty txs
                | otherwise = pure accCopied

              go2 !accCopied !accBytes !remaining ebId !copiedEbId txs
                | Just ((offset, txBytesSize), txs') <- IntMap.minViewWithKey txs =
                    if accBytes + txBytesSize > bytesLimit
                      then pure accCopied'
                      else do
                        dbBindInt64 stmt 1 (fromIntegralEbId ebId)
                        dbBindInt64 stmt 2 (fromIntegral offset)
                        dbStep1 stmt
                        dbReset stmt
                        go2 accCopied (accBytes + txBytesSize) remaining ebId (IntSet.insert offset copiedEbId) txs'
                | otherwise = go1 accCopied' accBytes remaining
               where
                accCopied' = if IntSet.null copiedEbId then accCopied else Map.insert ebId copiedEbId accCopied

          go1 Map.empty 0 toCopy
    }

sql_attach_memTxPoints :: String
sql_attach_memTxPoints =
  "ATTACH DATABASE ':memory:' AS mem;\n\
  \\n\
  \CREATE TABLE mem.txPoints (\n\
  \    ebId INTEGER NOT NULL\n\
  \  ,\n\
  \    txOffset INTEGER NOT NULL\n\
  \  ,\n\
  \    PRIMARY KEY (ebId ASC, txOffset ASC)\n\
  \  ) WITHOUT ROWID;\n\
  \"

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
  doesFileExist dbPath >>= \case
    False -> die $ "No such LeiosDb file: " ++ dbPath
    True -> do
      db <- withDieMsg $ DB.open (fromString dbPath)
      withDieMsg $ DB.exec db (fromString sql_attach_memTxPoints)
      pure $ leiosDbHandleFromSqlite db
