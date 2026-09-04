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
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTBQueue
  , StrictTChan
  , dupTChan
  , isFullTBQueue
  , newBroadcastTChan
  , newTBQueueIO
  , readTBQueue
  , writeTBQueue
  , writeTChan
  )
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, tryPutMVar)
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
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
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
import LeiosDemoDb.Trace (LeiosDbStats (..), TraceLeiosDb (..))
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
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Util.IOLike (atomically, labelThread)
import System.Directory (doesFileExist, getFileSize)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.Random (randomIO)

-- * Public API

-- | Create a new Leios database connection from environment variable.
-- This looks up the LEIOS_DB_PATH environment variable and opens the database.
newLeiosDBSQLiteFromEnv :: Tracer IO TraceLeiosDb -> IO (LeiosDbHandle IO)
newLeiosDBSQLiteFromEnv tracer = do
  -- TODO(geo2a): for testnet deployment, we want to have two separate env vars for the vol and imm partitions
  dbPath <-
    lookupEnv "LEIOS_DB_PATH" >>= \case
      Nothing -> die "You must define the LEIOS_DB_PATH variable for this demo."
      Just x -> pure x
  newLeiosDBSQLite tracer (volPath dbPath) (immPath dbPath)
 where
  volPath :: FilePath -> FilePath
  volPath = (<> ".vol")

  immPath :: FilePath -> FilePath
  immPath = (<> ".imm")

-- | Create a new Leios database using the SQLite implementation.
--
-- Each call to 'open' on the returned handle creates a new SQLite connection.
-- Connections are not thread-safe and should not be shared across threads.
newLeiosDBSQLite :: Tracer IO TraceLeiosDb -> FilePath -> FilePath -> IO (LeiosDbHandle IO)
newLeiosDBSQLite tracer volLeiosDbPath immLeiosDbPath = do
  notificationChan <- atomically newBroadcastTChan
  -- seed the in-memory stats by counting the EB rows once per handle
  statsVar <- newIORef =<< initialStats volLeiosDbPath immLeiosDbPath
  -- start a thread to sample the sizes of the LeiosDB
  startVolatileStatsSampler tracer statsVar volLeiosDbPath
  copyQueue <- newTBQueueIO copyQueueCapacity

  -- LeiosDB vol-to-imm copier thread
  startCopier tracer statsVar copyQueue volLeiosDbPath immLeiosDbPath
  gcRootCtx <- rootCallCtx "leiosdb-gc"

  -- LeiosDB GC sweeper thread
  sweepDoorbell <- newMVar ()
  startSweeper tracer statsVar volLeiosDbPath sweepDoorbell
  pure $
    LeiosDbHandle
      { subscribeEbNotifications =
          atomically (dupTChan notificationChan)
      , leiosDbGarbageCollect =
          sqlGarbageCollect tracer gcRootCtx volLeiosDbPath copyQueue sweepDoorbell
      , leiosDbPromoteToImmutable = sqlPromoteToImmutable tracer volLeiosDbPath copyQueue
      , leiosDbSampleStats = readIORef statsVar
      , open = openSQLiteConnection tracer volLeiosDbPath immLeiosDbPath notificationChan statsVar
      }

-- | Initialise 'LeiosDbStats' by counting the EB rows of both partitions.
-- Read-only: a missing file counts as zero and is NOT created (creating any
-- file here would trip the ChainDB's @NoDbMarkerAndNotEmpty@ check).
initialStats :: HasCallStack => FilePath -> FilePath -> IO LeiosDbStats
initialStats volPath immPath = do
  vol <- countEbsIfExists volPath
  imm <- countEbsIfExists immPath
  pure
    LeiosDbStats
      { volatileEbs = vol
      , immutableEbs = imm
      , walBytes = 0
      }
 where
  countEbsIfExists path = do
    exists <- doesFileExist path
    if exists
      then fromIntegral <$> withReadOnlyConn path (\db -> queryInt64 db "SELECT COUNT(*) FROM ebs")
      else pure 0

-- * Stats sampling

-- | Fork a thread that traces 'TraceLeiosDbStats' every 10 seconds. Samples
-- the volatile partition's file only.
startVolatileStatsSampler ::
  Tracer IO TraceLeiosDb -> IORef LeiosDbStats -> FilePath -> IO ()
startVolatileStatsSampler tracer statsVar volPath =
  void $ forkIO $ forever $ do
    -- wait one sample window to side-step contention with starting the LeiosDB
    threadDelay tenSeconds
    stats <- readIORef statsVar
    -- read the WAL size
    walBytes <- fileSizeOr0 (volPath <> "-wal")
    traceWith tracer $
      TraceLeiosDbStats
        stats
          { walBytes
          }
 where
  tenSeconds = 10000000

  fileSizeOr0 :: FilePath -> IO Integer
  fileSizeOr0 path = do
    exists <- doesFileExist path
    if exists then getFileSize path else pure 0

-- | Fold a delta into the volatile EB count of the in-memory 'LeiosDbStats'.
bumpVolatileStats :: Conn -> Int -> IO ()
bumpVolatileStats Conn{connStats} = bumpVolatileStatsRef connStats

-- | 'bumpVolatileStats' for the maintenance paths, which have no 'Conn'.
bumpVolatileStatsRef :: IORef LeiosDbStats -> Int -> IO ()
bumpVolatileStatsRef statsVar dEbs =
  unless (dEbs == 0) $
    atomicModifyIORef' statsVar $ \s ->
      (s{volatileEbs = s.volatileEbs + dEbs}, ())

-- | Fold a delta into the immutable EB count of the in-memory 'LeiosDbStats'.
bumpImmutableStats :: IORef LeiosDbStats -> Int -> IO ()
bumpImmutableStats statsVar dEbs =
  unless (dEbs == 0) $
    atomicModifyIORef' statsVar $ \s ->
      (s{immutableEbs = s.immutableEbs + dEbs}, ())

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

-- | Run a query that yields exactly one integer column.
queryInt64 :: HasCallStack => DB.Database -> String -> IO Int64
queryInt64 db sql =
  MonadThrow.bracket (dbPrepare db (fromString sql)) dbFinalize $ \stmt ->
    dbStep stmt >>= \case
      DB.Row -> DB.columnInt64 stmt 0
      DB.Done -> error ("queryInt64: expected a row: " <> sql)

-- | Open a read-write connection to the given file, creating it and running
-- the schema DDL if it does not exist yet. Both partitions share 'sql_schema'.
openRawConnection :: HasCallStack => FilePath -> IO DB.Database
openRawConnection path = do
  shouldInitSchema <- not <$> doesFileExist path
  db <- open2 (fromString path) [SQLOpenReadWrite, SQLOpenCreate] SQLVFSDefault
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
  pure db

-- | 'openRawConnection' for the volatile partition: additionally applies the
-- GC-only DDL ('sql_schema_gc').
openVolRawConnection :: HasCallStack => FilePath -> IO DB.Database
openVolRawConnection path = do
  db <- openRawConnection path
  orCloseOnError db $ dbExec db (fromString sql_schema_gc)
  pure db

-- | Open a connection to the volatile partition for a maintenance operation and
--   close it when done.
--
-- Used by 'sqlPromoteToImmutable' and 'sqlGarbageCollect'.
withMaintenanceConn :: HasCallStack => FilePath -> (DB.Database -> IO a) -> IO a
withMaintenanceConn volPath =
  MonadThrow.bracket (openVolRawConnection volPath) (void . DB.close)

-- | Prepare a statement, run the action, finalize.
withStmt :: HasCallStack => DB.Database -> String -> (DB.Statement -> IO a) -> IO a
withStmt db sql = MonadThrow.bracket (dbPrepare db (fromString sql)) dbFinalize

-- * Copying EBs to the immutable partition

copyQueueCapacity :: Natural
copyQueueCapacity = 1024

-- | Implements 'leiosDbPromoteToImmutable':
--   - pin the EB rows in the volatile partition for promotion (@status@ 0 -> 1)
--     when they are old enough;
--   - put the hash into the queue for the copier to pick up.
sqlPromoteToImmutable ::
  HasCallStack =>
  Tracer IO TraceLeiosDb -> FilePath -> StrictTBQueue IO EbHash -> LeiosPoint -> IO ()
sqlPromoteToImmutable tracer volPath copyQueue point = do
  withMaintenanceConn volPath $ \db ->
    dbWithWriteTransactionRaw tracer db $
      withStmt db sql_pin_eb $ \stmt -> do
        dbBindBlob stmt 1 point.pointEbHash.ebHashBytes
        dbStep1Safe stmt
  enqueueCopy tracer copyQueue point.pointEbHash

-- | Enqueue an EB to be copied into the immutable DB.
enqueueCopy :: Tracer IO TraceLeiosDb -> StrictTBQueue IO EbHash -> EbHash -> IO ()
enqueueCopy tracer copyQueue ebHash = do
  accepted <-
    atomically $
      isFullTBQueue copyQueue >>= \case
        True -> pure False
        False -> True <$ writeTBQueue copyQueue ebHash
  unless accepted $
    traceWith tracer $
      TraceLeiosDbCopyQueueFull (show ebHash)

-- | The copier's long-lived connection and its prepared statements. One
-- connection: main is the immutable file, the volatile file is ATTACHed as @vol@.
data CopierConn = CopierConn
  { ccDb :: !DB.Database
  -- ^ main = immutable partition, @vol@ = attached volatile partition
  , ccHasEb :: !DB.Statement
  -- ^ 'sql_imm_has_eb'
  , ccCompleteness :: !DB.Statement
  -- ^ 'sql_copy_completeness'
  , ccInsertEb :: !DB.Statement
  -- ^ 'sql_copy_insert_eb'
  , ccInsertEbTxs :: !DB.Statement
  -- ^ 'sql_copy_insert_ebTxs'
  , ccInsertTxs :: !DB.Statement
  -- ^ 'sql_copy_insert_txs'
  , ccMarkAsCopied :: !DB.Statement
  -- ^ 'sql_mark_as_copied', against the attached volatile partition
  }

-- | The copier consumes the queue, appending each pinned EB's closure to the
-- immutable partition and then marking the volatile rows as copied
-- (@status = 2@, evictable).
startCopier ::
  Tracer IO TraceLeiosDb ->
  IORef LeiosDbStats ->
  StrictTBQueue IO EbHash ->
  FilePath ->
  FilePath ->
  IO ()
startCopier tracer statsVar copyQueue volPath immPath = do
  let taskName = "leiosdb-copier"
  rootCtx <- rootCallCtx "leiosdb-copier"
  connRef <- newIORef Nothing
  threadId <- forkIO $ forever $ do
    ebHash <- atomically $ readTBQueue copyQueue
    copyOne rootCtx connRef ebHash
      `MonadThrow.catch` \(e :: SomeException) -> do
        traceWith tracer $ TraceLeiosDbCopyError (show ebHash) (show e)
        -- The connection may be poisoned (e.g. an open transaction);
        -- drop it and reopen on the next item.
        dropCopierConn connRef ebHash
        threadDelay 1000000
  labelThread threadId taskName
 where
  copySpan ::
    (Aeson.ToJSON arg, Aeson.ToJSON res) =>
    CallCtx IO -> CallName -> arg -> (CallCtx IO -> IO res) -> IO res
  copySpan = callTraceSameThread (traceWith tracer . TraceLeiosDbCall . SomeJsonCallTrace)

  -- try to copy one EB and its closure to the immutable partition
  copyOne rootCtx connRef ebHash =
    copySpan rootCtx "copyToImmutable" (show ebHash) $ \_ctx -> do
      conn <- getConn connRef
      let CopierConn{ccHasEb, ccMarkAsCopied} = conn
      -- check if the EB is already in the immutable partition
      present <- useStmt ccHasEb $ do
        dbBindBlob ccHasEb 1 ebHash.ebHashBytes
        (/= 0) <$> readSingleInt64 ccHasEb
      copied <-
        if present
          then pure True
          else
            appendToImmutable conn ebHash >>= \case
              Nothing -> do
                -- EB is not ready to be copied
                traceWith tracer $
                  TraceLeiosDbCopyError
                    (show ebHash)
                    "pinned EB has no complete closure in the volatile partition"
                pure False
              Just _copiedTxs -> do
                -- successfully copied, bump the stats
                bumpImmutableStats statsVar 1
                traceWith tracer $ TraceLeiosDbCopiedToImmutable 1
                pure True
      -- make the EB as copied
      when copied $
        useStmt ccMarkAsCopied $ do
          dbBindBlob ccMarkAsCopied 1 ebHash.ebHashBytes
          dbStep1Safe ccMarkAsCopied

  -- Attempt to do the actual copying.
  --
  -- Returns the number of copied body rows, or Nothing if
  -- the volatile partition does not hold the full closure.
  appendToImmutable :: CopierConn -> EbHash -> IO (Maybe Int)
  appendToImmutable CopierConn{ccDb, ccCompleteness, ccInsertEb, ccInsertEbTxs, ccInsertTxs} ebHash =
    dbWithTransaction ccDb $ do
      (bodyCount, closureCount) <- useStmt ccCompleteness $ do
        dbBindBlob ccCompleteness 1 ebHash.ebHashBytes
        -- step the first time, expecting a single result row
        dbStepSafe ccCompleteness >>= \case
          DB.Done ->
            -- no row: critical error, fail fast and loud.
            -- this should not happen.
            error "sql_copy_completeness: expected a row"
          DB.Row -> do
            n <- DB.columnInt64 ccCompleteness 0
            m <- DB.columnInt64 ccCompleteness 1
            -- step again, expecting no more row
            dbStepSafe ccCompleteness >>= \case
              DB.Done ->
                -- we have our result
                pure (n, m)
              DB.Row ->
                -- another row: critical error, fail fast and loud.
                -- this should not happen.
                error "sql_copy_completeness: expected exactly one row"
      if bodyCount == 0 || bodyCount /= closureCount
        then
          -- the EB is incomplete, don't copy
          pure Nothing
        else do
          -- the EB is complete: body is non-empty and bodyCounty matches closureCount
          --
          -- copy the EB
          useStmt ccInsertEb $ do
            dbBindBlob ccInsertEb 1 ebHash.ebHashBytes
            dbStep1Safe ccInsertEb
          nEbs <- DB.changes ccDb
          if nEbs == 0
            then pure Nothing
            else do
              -- copy the eb-to-transactions mapping
              useStmt ccInsertEbTxs $ do
                dbBindBlob ccInsertEbTxs 1 ebHash.ebHashBytes
                dbStep1Safe ccInsertEbTxs
              nTxs <- DB.changes ccDb
              -- copy the transactions
              useStmt ccInsertTxs $ do
                dbBindBlob ccInsertTxs 1 ebHash.ebHashBytes
                dbStep1Safe ccInsertTxs
              pure (Just nTxs)

  getConn connRef =
    readIORef connRef >>= \case
      Just conn -> pure conn
      Nothing -> do
        ccDb <- openRawConnection immPath
        conn <-
          orCloseOnError ccDb $ do
            withStmt ccDb "ATTACH ? AS vol" $ \stmt -> do
              dbBindUtf8 stmt 1 (fromString volPath)
              dbStep1Safe stmt
            ccHasEb <- dbPrepare ccDb (fromString sql_imm_has_eb)
            ccCompleteness <- dbPrepare ccDb (fromString sql_copy_completeness)
            ccInsertEb <- dbPrepare ccDb (fromString sql_copy_insert_eb)
            ccInsertEbTxs <- dbPrepare ccDb (fromString sql_copy_insert_ebTxs)
            ccInsertTxs <- dbPrepare ccDb (fromString sql_copy_insert_txs)
            ccMarkAsCopied <- dbPrepare ccDb (fromString sql_mark_as_copied)
            pure CopierConn{..}
        writeIORef connRef (Just conn)
        pure conn

  -- Never throws (the caller is the copier's error handler): statements are
  -- finalized first, then the connection closed with the result checked --
  -- @sqlite3_close@ refuses (and would leak) if statements were still
  -- outstanding, and silencing that would hide a bug of this function.
  dropCopierConn connRef ebHash =
    readIORef connRef >>= \case
      Nothing -> pure ()
      Just CopierConn{..} -> do
        writeIORef connRef Nothing
        mapM_
          dbFinalize
          [ccHasEb, ccCompleteness, ccInsertEb, ccInsertEbTxs, ccInsertTxs, ccMarkAsCopied]
        DB.close ccDb >>= \case
          Right () -> pure ()
          Left err ->
            traceWith tracer $
              TraceLeiosDbCopyError
                (show ebHash)
                ("failed to close the copier's connection: " <> show err)

-- | Close the connection if the action throws, then rethrow. For partial
-- acquisition of 'CopierConn': without it, a persistently failing attach or
-- prepare would leak one connection per retry.
orCloseOnError :: DB.Database -> IO a -> IO a
orCloseOnError db act =
  act `MonadThrow.catch` \(e :: SomeException) -> do
    _ <- DB.close db
    throwIO e

-- * Garbage collection of the volatile partition

-- | How many EBs one sweep transaction.
gcBatchSize :: Int64
gcBatchSize = 4

-- | Pause between GC sweep batches.
gcBatchPauseMicros :: Int
gcBatchPauseMicros = 100000

-- | How many orphaned txs to GC in one sweep.
gcOrphanTxBatchSize :: Int64
gcOrphanTxBatchSize = 1024

-- | Page size of the 'reinitialiseGcTxCandidates' scan.
gcCandidatesPageSize :: Int64
gcCandidatesPageSize = 4096

-- | The MARK phase of GC mark-and-sweep:
--   - mark for GC (@status = 3@) every EB hash all of whose announcements are older
--     than the given slot and not pinned (@status = 1@);
--   - stage its txs as GC candidates;
--   - wake up the sweeper thread.
--
-- Note: this function does not do much work, but rather primes the state for the sweeper thread
-- to do the actual GC (see 'startSweeper').
sqlGarbageCollect ::
  HasCallStack =>
  Tracer IO TraceLeiosDb ->
  CallCtx IO ->
  FilePath ->
  StrictTBQueue IO EbHash ->
  MVar () ->
  SlotNo ->
  IO ()
sqlGarbageCollect tracer rootCtx volPath copyQueue sweepDoorbell gcSlot =
  gcSpan rootCtx "sqlGarbageCollect" (unSlotNo gcSlot) $ \gcCtx ->
    withMaintenanceConn volPath $ \db ->
      withGcStmts db $ \GcStmts{gsStalePins, gsHasWork, gsAddGcCandidatesTxs, gsMarkEbForGC} -> do
        -- Self-heal: re-enqueue pinned-but-not-yet-copied EBs to the copier.
        -- Recovers a crashed copier.
        gcSpan gcCtx "selfHeal" () $ \_ -> do
          stalePins <- useStmt gsStalePins $ do
            dbBindInt64 gsStalePins 1 slot
            map MkEbHash <$> collectBlobs gsStalePins
          forM_ stalePins $ enqueueCopy tracer copyQueue
        -- check if GC has any work to do
        hasWork <- gcSpan gcCtx "noopGuard" () $ \_ ->
          useStmt gsHasWork $ do
            dbBindInt64 gsHasWork 1 slot
            (/= 0) <$> readSingleInt64 gsHasWork
        when hasWork $ do
          (nTxsStagedAsGCCandidates, nEbsMarked) <- gcSpan gcCtx "mark" () $ \_ ->
            dbWithWriteTransactionRaw tracer db $ do
              -- transactions must be marked for GC before their EBs,
              -- due to the way the sql statements are written.
              -- mark transactions b for GC
              useStmt gsAddGcCandidatesTxs $ do
                dbBindInt64 gsAddGcCandidatesTxs 1 slot
                dbStep1Safe gsAddGcCandidatesTxs
              nTxsStagedAsGCCandidates <- DB.changes db
              -- now mark the EB
              useStmt gsMarkEbForGC $ do
                dbBindInt64 gsMarkEbForGC 1 slot
                dbStep1Safe gsMarkEbForGC
              nEbsMarked <- DB.changes db
              pure (nTxsStagedAsGCCandidates, nEbsMarked)
          when (nTxsStagedAsGCCandidates > 0 || nEbsMarked > 0) $
            void $
              tryPutMVar sweepDoorbell ()
 where
  slot = fromIntegral (unSlotNo gcSlot)

  gcSpan ::
    (Aeson.ToJSON arg, Aeson.ToJSON res) =>
    CallCtx IO -> CallName -> arg -> (CallCtx IO -> IO res) -> IO res
  gcSpan = callTraceSameThread (traceWith tracer . TraceLeiosDbCall . SomeJsonCallTrace)

-- | One GC tick's prepared statements, prepared once per maintenance
-- connection and finalized in one place strictly before it closes -- the
-- same discipline as 'VolStmts', for the same use-after-free reasons.
data GcStmts = GcStmts
  { gsStalePins :: !DB.Statement
  -- ^ 'sql_gc_stale_pins'
  , gsHasWork :: !DB.Statement
  -- ^ 'sql_gc_has_work'
  , gsAddGcCandidatesTxs :: !DB.Statement
  -- ^ 'sql_gc_stage_marked'
  , gsMarkEbForGC :: !DB.Statement
  -- ^ 'sql_gc_mark'
  }

-- | Prepare 'GcStmts', run the action, finalize them (before the caller
-- closes the connection).
withGcStmts :: HasCallStack => DB.Database -> (GcStmts -> IO a) -> IO a
withGcStmts db =
  MonadThrow.bracket
    ( do
        gsStalePins <- dbPrepare db (fromString sql_gc_stale_pins)
        gsHasWork <- dbPrepare db (fromString sql_gc_has_work)
        gsAddGcCandidatesTxs <- dbPrepare db (fromString sql_gc_stage_marked)
        gsMarkEbForGC <- dbPrepare db (fromString sql_gc_mark)
        pure GcStmts{..}
    )
    ( \GcStmts{..} ->
        mapM_
          dbFinalize
          [ gsStalePins
          , gsHasWork
          , gsAddGcCandidatesTxs
          , gsMarkEbForGC
          ]
    )

-- | Step a statement (safe FFI) to completion, collecting blob column 0.
collectBlobs :: HasCallStack => DB.Statement -> IO [ByteString]
collectBlobs stmt = loop []
 where
  loop acc =
    dbStepSafe stmt >>= \case
      DB.Done -> pure (reverse acc)
      DB.Row -> do
        b <- DB.columnBlob stmt 0
        loop (b : acc)

-- | Bind a JSON payload ('jsonHexArray') to parameter 1 and execute the statement
--   via non-blocking Safe FFI.
execJson :: HasCallStack => DB.Statement -> ByteString -> IO ()
execJson stmt json =
  useStmt stmt $ do
    dbBindUtf8 stmt 1 json
    dbStep1Safe stmt

-- * Sweeping GC-marked rows out of the volatile partition

-- | The sweeper's long-lived connection and prepared statements; lifecycle
-- mirrors 'CopierConn'.
data SweeperConn = SweeperConn
  { swDb :: !DB.Database
  , swPickMarked :: !DB.Statement
  -- ^ 'sql_sweep_pick_marked'
  , swEvictEbTxs :: !DB.Statement
  -- ^ 'sql_gc_ebTxs'
  , swEvictMissingTxs :: !DB.Statement
  -- ^ 'sql_gc_missing_txs'
  , swEvictEbs :: !DB.Statement
  -- ^ 'sql_gc_ebs_by_hash'
  , swAnyMarked :: !DB.Statement
  -- ^ 'sql_sweep_any_marked'
  , swPickOrphans :: !DB.Statement
  -- ^ 'sql_sweep_pick_orphans'
  , swOrphanTxs :: !DB.Statement
  -- ^ 'sql_sweep_orphan_txs'
  , swPopOrphans :: !DB.Statement
  -- ^ 'sql_sweep_pop_orphans'
  , swHasUnstagedGcCandidates :: !DB.Statement
  -- ^ 'sql_has_unstaged_gc_candidates'
  , swUnstagedGcCandidatesPage :: !DB.Statement
  -- ^ 'sql_unstaged_gc_candidates_page'
  , swInsertGcCandidates :: !DB.Statement
  -- ^ 'sql_insert_gc_candidates'
  }

-- | The SWEEP phase of GC mark-and-sweep: evict what 'sqlGarbageCollect' marked.
--
-- Use short transactions with 'gcBatchPauseMicros' pauses in between,
-- so eviction never holds the volatile write lock for long.
startSweeper ::
  Tracer IO TraceLeiosDb ->
  IORef LeiosDbStats ->
  FilePath ->
  MVar () ->
  IO ()
startSweeper tracer statsVar volPath doorbell = do
  let taskName = "leiosdb-sweeper"
  rootCtx <- rootCallCtx taskName
  connRef <- newIORef Nothing
  gcInitialisationDoneRef <- newIORef False
  threadId <- forkIO $ forever $ do
    takeMVar doorbell
    exists <- doesFileExist volPath
    when exists $
      sweepPass rootCtx connRef gcInitialisationDoneRef
        `MonadThrow.catch` \(e :: SomeException) -> do
          traceWith tracer $ TraceLeiosDbGCError (show e)
          -- The connection may be poisoned (e.g. an open transaction);
          -- drop it, pause, and re-ring the doorbell to retry the pass.
          dropSweeperConn connRef
          threadDelay 1000000
          void $ tryPutMVar doorbell ()
  labelThread threadId taskName
 where
  sweepSpan ::
    (Aeson.ToJSON arg, Aeson.ToJSON res) =>
    CallCtx IO -> CallName -> arg -> (CallCtx IO -> IO res) -> IO res
  sweepSpan = callTraceSameThread (traceWith tracer . TraceLeiosDbCall . SomeJsonCallTrace)

  -- this launches one sweep pass: ebLoop + orphanTxsLoop
  sweepPass rootCtx connRef gcInitialisationDoneRef =
    sweepSpan rootCtx "sweepPass" () $ \passCtx -> do
      conn <- getConn connRef
      reinitialiseGcTxCandidates passCtx conn gcInitialisationDoneRef
      -- GC EBs
      nEbs <- ebLoop passCtx conn 0
      -- GC their transactions
      nTxs <- orphanTxsLoop passCtx conn 0
      when (nEbs > 0 || nTxs > 0) $ do
        -- Flush the WAL only after real work; the hot paths' autocheckpoint
        -- covers the rest.
        sweepSpan passCtx "walCheckpoint" () $ \_ ->
          dbExec (swDb conn) "PRAGMA wal_checkpoint(PASSIVE);"
        traceWith tracer (TraceLeiosDbEvicted nEbs)

  -- Evict EBs, 'gcBatchSize' at a time.
  ebLoop passCtx conn !totalEbsEvicted = do
    let SweeperConn{swDb, swPickMarked, swEvictEbTxs, swEvictMissingTxs, swEvictEbs} = conn
    nEbs <- sweepSpan passCtx "sweepEbBatch" () $ \_ ->
      dbWithWriteTransactionRaw tracer swDb $ do
        -- check if any EBs are ready to be evicted
        evictableEbs <- useStmt swPickMarked $ do
          dbBindInt64 swPickMarked 1 gcBatchSize
          collectBlobs swPickMarked
        -- evict EBs if any are ready to be GCed
        if null evictableEbs
          then pure 0
          else do
            let evictableEbsJson = jsonHexArray evictableEbs
            execJson swEvictEbTxs evictableEbsJson
            execJson swEvictMissingTxs evictableEbsJson
            execJson swEvictEbs evictableEbsJson
            DB.changes swDb
    if nEbs == 0
      then
        -- this GC batch did not do any work, we're done
        pure totalEbsEvicted
      else do
        -- this GC batch did work, update stats and continue to the next batch after a delay
        bumpVolatileStatsRef statsVar (negate nEbs)
        threadDelay gcBatchPauseMicros
        ebLoop passCtx conn (totalEbsEvicted + nEbs)

  -- Evict orphaned transactions, 'gcOrphanTxBatchSize' at a time.
  orphanTxsLoop passCtx conn !totalTxsEvicted = do
    let SweeperConn{swDb, swAnyMarked, swPickOrphans, swOrphanTxs, swPopOrphans} = conn
    mEvictedTxs <- sweepSpan passCtx "sweepOrphanBatch" () $ \_ ->
      dbWithWriteTransactionRaw tracer swDb $ do
        -- don't run the sweep if any GC-marked EBs remain
        blocked <- useStmt swAnyMarked $ (/= 0) <$> readSingleInt64 swAnyMarked
        if blocked
          then pure Nothing
          else do
            -- look for txs to GC
            orphanedTxs <- useStmt swPickOrphans $ do
              dbBindInt64 swPickOrphans 1 gcOrphanTxBatchSize
              collectBlobs swPickOrphans
            if null orphanedTxs
              then pure Nothing
              else do
                let orphanedTxsJson = jsonHexArray orphanedTxs
                -- evict transactions
                execJson swOrphanTxs orphanedTxsJson
                nTxs <- DB.changes swDb
                -- and delete them from the GC transaction candidates table
                execJson swPopOrphans orphanedTxsJson
                pure (Just nTxs)
    case mEvictedTxs of
      Nothing ->
        -- this GC batch did not do any work, we're done
        pure totalTxsEvicted
      Just nTxs -> do
        -- this GC batch did work, update stats and continue to the next batch after a delay
        threadDelay gcBatchPauseMicros
        orphanTxsLoop passCtx conn (totalTxsEvicted + nTxs)

  -- Initialise the 'gcTxCandidates' table.
  -- Runs once per process and does useful work after node restart.
  reinitialiseGcTxCandidates passCtx conn gcInitialisationDoneRef = do
    done <- readIORef gcInitialisationDoneRef
    unless done $ do
      sweepSpan passCtx "reinitialiseGcTxCandidates" () $ \_ -> do
        let SweeperConn{swDb, swHasUnstagedGcCandidates, swUnstagedGcCandidatesPage, swInsertGcCandidates} = conn
        anyUnstaged <-
          useStmt swHasUnstagedGcCandidates $
            (/= 0) <$> readSingleInt64 swHasUnstagedGcCandidates
        let pageLoop cursor = do
              page <- useStmt swUnstagedGcCandidatesPage $ do
                dbBindBlob swUnstagedGcCandidatesPage 1 cursor
                dbBindInt64 swUnstagedGcCandidatesPage 2 gcCandidatesPageSize
                collectBlobs swUnstagedGcCandidatesPage
              unless (null page) $ do
                dbWithWriteTransactionRaw tracer swDb $
                  execJson swInsertGcCandidates (jsonHexArray page)
                when (length page == fromIntegral gcCandidatesPageSize) $
                  pageLoop (last page)
        when anyUnstaged $ pageLoop BS.empty
      writeIORef gcInitialisationDoneRef True

  getConn connRef =
    readIORef connRef >>= \case
      Just conn -> pure conn
      Nothing -> do
        swDb <- openVolRawConnection volPath
        conn <-
          orCloseOnError swDb $ do
            swPickMarked <- dbPrepare swDb (fromString sql_sweep_pick_marked)
            swEvictEbTxs <- dbPrepare swDb (fromString sql_gc_ebTxs)
            swEvictMissingTxs <- dbPrepare swDb (fromString sql_gc_missing_txs)
            swEvictEbs <- dbPrepare swDb (fromString sql_gc_ebs_by_hash)
            swAnyMarked <- dbPrepare swDb (fromString sql_sweep_any_marked)
            swPickOrphans <- dbPrepare swDb (fromString sql_sweep_pick_orphans)
            swOrphanTxs <- dbPrepare swDb (fromString sql_sweep_orphan_txs)
            swPopOrphans <- dbPrepare swDb (fromString sql_sweep_pop_orphans)
            swHasUnstagedGcCandidates <- dbPrepare swDb (fromString sql_has_unstaged_gc_candidates)
            swUnstagedGcCandidatesPage <- dbPrepare swDb (fromString sql_unstaged_gc_candidates_page)
            swInsertGcCandidates <- dbPrepare swDb (fromString sql_insert_gc_candidates)
            pure SweeperConn{..}
        writeIORef connRef (Just conn)
        pure conn

  -- Same discipline as 'dropCopierConn': never throws, statements finalized
  -- strictly before the checked close.
  dropSweeperConn connRef =
    readIORef connRef >>= \case
      Nothing -> pure ()
      Just SweeperConn{..} -> do
        writeIORef connRef Nothing
        mapM_
          dbFinalize
          [ swPickMarked
          , swEvictEbTxs
          , swEvictMissingTxs
          , swEvictEbs
          , swAnyMarked
          , swPickOrphans
          , swOrphanTxs
          , swPopOrphans
          , swHasUnstagedGcCandidates
          , swUnstagedGcCandidatesPage
          , swInsertGcCandidates
          ]
        DB.close swDb >>= \case
          Right () -> pure ()
          Left err ->
            traceWith tracer $
              TraceLeiosDbGCError
                ("failed to close the sweeper's connection: " <> show err)

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
data VolStmts = VolStmts
  { stScanEbPoints :: !DB.Statement
  , stInsertEbPoint :: !DB.Statement
  , stLookupEbBody :: !DB.Statement
  , stInsertEbTxsRow :: !DB.Statement
  , stInitMissingCount :: !DB.Statement
  , stInsertTx :: !DB.Statement
  , stDecrMissingCount :: !DB.Statement
  , stInsertMissingTxs :: !DB.Statement
  , stDeleteMissingTxs :: !DB.Statement
  , stFindCompleteEbs :: !DB.Statement
  , stMarkNotifiedEbs :: !DB.Statement
  , stMarkPointNotified :: !DB.Statement
  , stBatchRetrieveTxs :: !DB.Statement
  , stFilterMissingTxs :: !DB.Statement
  , stLookupEbClosure :: !DB.Statement
  , stScanCompleteEbsSince :: !DB.Statement
  }

data Conn = Conn
  { conVolDb :: !DB.Database
  -- ^ The connection to the volatile partition.
  , connVolStmts :: !VolStmts
  -- ^ Precompiled statements used with the volatile partition.
  , connTracer :: !(Tracer IO TraceLeiosDb)
  -- ^ So the write path can report exhausting SQLite's own busy timeout.
  , connStats :: !(IORef LeiosDbStats)
  -- ^ Usage stats for this connection.
  , conImmDb :: !DB.Database
  -- ^ The connection to the immutable partition.
  , connImmStmts :: !ImmStmts
  -- ^ Precompiled statements used with the immutable partition.
  }

-- | Prepared statements of the fallback reads into the immutable partition.
-- The partitions share one schema, so these are the volatile SQL strings
-- prepared against the immutable connection (plus the presence probe).
-- Lifecycle mirrors 'VolStmts': prepared at open time, finalized in 'close'
-- before their connection.
data ImmStmts = ImmStmts
  { immStLookupEbBody :: !DB.Statement
  , immStLookupEbClosure :: !DB.Statement
  , immStBatchRetrieveTxs :: !DB.Statement
  , immStFilterPresent :: !DB.Statement
  }

prepareImmStmts :: HasCallStack => DB.Database -> IO ImmStmts
prepareImmStmts db = do
  immStLookupEbBody <- dbPrepare db (fromString sql_lookup_ebBodies)
  immStLookupEbClosure <- dbPrepare db (fromString sql_lookup_eb_closure)
  immStBatchRetrieveTxs <- dbPrepare db (fromString sql_retrieve_from_ebTxs_json)
  immStFilterPresent <- dbPrepare db (fromString sql_imm_filter_present)
  pure ImmStmts{..}

-- | Same use-after-free discipline as 'finalizeVolStmts'.
finalizeImmStmts :: ImmStmts -> IO ()
finalizeImmStmts ImmStmts{..} = do
  dbFinalize immStLookupEbBody
  dbFinalize immStLookupEbClosure
  dbFinalize immStBatchRetrieveTxs
  dbFinalize immStFilterPresent

-- | Prepare every statement 'VolStmts' names. Order is not observable.
prepareVolStmts :: DB.Database -> IO VolStmts
prepareVolStmts db = do
  stScanEbPoints <- dbPrepare db (fromString sql_scan_ebs)
  stInsertEbPoint <- dbPrepare db (fromString sql_insert_eb)
  stLookupEbBody <- dbPrepare db (fromString sql_lookup_ebBodies)
  stInsertEbTxsRow <- dbPrepare db (fromString sql_insert_ebBody)
  stInitMissingCount <- dbPrepare db (fromString sql_init_missing_tx_count)
  stInsertTx <- dbPrepare db (fromString sql_insert_tx)
  stDecrMissingCount <- dbPrepare db (fromString sql_decrement_missing_tx_count)
  stInsertMissingTxs <- dbPrepare db (fromString sql_insert_missing_txs)
  stDeleteMissingTxs <- dbPrepare db (fromString sql_delete_missing_txs)
  stFindCompleteEbs <- dbPrepare db (fromString sql_find_complete_ebs)
  stMarkNotifiedEbs <- dbPrepare db (fromString sql_mark_notified_ebs)
  stMarkPointNotified <- dbPrepare db (fromString sql_mark_point_notified)
  stBatchRetrieveTxs <- dbPrepare db (fromString sql_retrieve_from_ebTxs_json)
  stFilterMissingTxs <- dbPrepare db (fromString sql_filter_missing_txs_json)
  stLookupEbClosure <- dbPrepare db (fromString sql_lookup_eb_closure)
  stScanCompleteEbsSince <- dbPrepare db (fromString sql_scan_complete_ebs_since)
  pure VolStmts{..}

-- | Finalise every statement in 'VolStmts'. Called from 'close' immediately
-- before 'sqlite3_close_v2', on the connection's owner thread.
finalizeVolStmts :: VolStmts -> IO ()
finalizeVolStmts VolStmts{..} = do
  dbFinalize stScanEbPoints
  dbFinalize stInsertEbPoint
  dbFinalize stLookupEbBody
  dbFinalize stInsertEbTxsRow
  dbFinalize stInitMissingCount
  dbFinalize stInsertTx
  dbFinalize stDecrMissingCount
  dbFinalize stInsertMissingTxs
  dbFinalize stDeleteMissingTxs
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
  FilePath ->
  StrictTChan IO LeiosEbNotification ->
  IORef LeiosDbStats ->
  IO (LeiosDbConnection IO)
openSQLiteConnection tracer volPath immPath notificationChan statsVar = do
  volDb <- openVolRawConnection volPath
  stmts <- prepareVolStmts volDb
  immDb <- openRawConnection immPath
  immStmts <- prepareImmStmts immDb
  let conn =
        Conn
          { conVolDb = volDb
          , connVolStmts = stmts
          , connTracer = tracer
          , connStats = statsVar
          , conImmDb = immDb
          , connImmStmts = immStmts
          }
      notify = atomically . writeTChan notificationChan
  pure $
    LeiosDbConnection
      { close = do
          finalizeImmStmts (connImmStmts conn)
          void (DB.close (conImmDb conn))
          finalizeVolStmts (connVolStmts conn)
          void (DB.close (conVolDb conn))
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
  Conn{conVolDb = db, connVolStmts = VolStmts{stScanEbPoints = stmt}} = conn
  loop acc =
    dbStep stmt >>= \case
      DB.Done -> pure (reverse acc)
      DB.Row -> do
        slot <- SlotNo . fromIntegral <$> DB.columnInt64 stmt 0
        hash <- MkEbHash <$> DB.columnBlob stmt 1
        loop ((slot, hash) : acc)

sqlScanCompleteEbPointsSince :: Conn -> SlotNo -> IO [LeiosPoint]
sqlScanCompleteEbPointsSince conn sinceSlot = do
  (volComplete, recent) <-
    dbWithTransaction db $ do
      volComplete <- useStmt stmt $ do
        dbBindInt64 stmt 1 slot
        pointLoop stmt []
      -- Every recent hash, with or without completeness evidence, from the
      -- same snapshot.
      recent <- withStmt db sql_scan_recent_ebs $ \recentStmt -> do
        dbBindInt64 recentStmt 1 slot
        pointLoop recentStmt []
      pure (volComplete, recent)
  -- A recent hash without volatile completeness evidence may be a copied EB
  -- whose closure rows were evicted (its recent announcement never got a body
  -- insert). Presence in the immutable partition is proof of completeness:
  -- copies are atomic and only complete EBs are copied. Without this probe, a
  -- cert-RB parked across a restart would stay parked forever.
  let volCompleteSet = Set.fromList [ebHashBytes p.pointEbHash | p <- volComplete]
      unknown = [p | p <- recent, ebHashBytes p.pointEbHash `Set.notMember` volCompleteSet]
  if null unknown
    then pure volComplete
    else do
      present <- immFilterPresent conn [ebHashBytes p.pointEbHash | p <- unknown]
      let presentSet = Set.fromList present
      pure $
        volComplete
          <> [p | p <- unknown, ebHashBytes p.pointEbHash `Set.member` presentSet]
 where
  slot = fromIntegral $ unSlotNo sinceSlot
  Conn{conVolDb = db, connVolStmts = VolStmts{stScanCompleteEbsSince = stmt}} = conn

pointLoop :: DB.Statement -> [LeiosPoint] -> IO [LeiosPoint]
pointLoop stmt acc =
  dbStep stmt >>= \case
    DB.Done -> pure (reverse acc)
    DB.Row -> do
      slot <- SlotNo . fromIntegral <$> DB.columnInt64 stmt 0
      hash <- MkEbHash <$> DB.columnBlob stmt 1
      pointLoop stmt (MkLeiosPoint slot hash : acc)

-- | Which of the given EB hashes the immutable partition holds.
immFilterPresent :: Conn -> [ByteString] -> IO [ByteString]
immFilterPresent conn hashes =
  useStmt stmt $ do
    dbBindUtf8 stmt 1 (jsonHexArray hashes)
    let loop acc =
          dbStep stmt >>= \case
            DB.Done -> pure (reverse acc)
            DB.Row -> do
              hashBytes <- DB.columnBlob stmt 0
              loop (hashBytes : acc)
    loop []
 where
  Conn{connImmStmts = ImmStmts{immStFilterPresent = stmt}} = conn

sqlLookupEbBody :: Conn -> EbHash -> IO [(TxHash, BytesSize)]
sqlLookupEbBody conn ebHash = do
  vol <-
    dbWithTransaction db $ useStmt stmt $ do
      dbBindBlob stmt 1 (let MkEbHash bytes = ebHash in bytes)
      bodyLoop stmt []
  -- Bodies insert atomically, so the empty list is a complete miss: the EB
  -- may have been copied to the immutable partition and evicted.
  if null vol then immLookupEbBody conn ebHash else pure vol
 where
  Conn{conVolDb = db, connVolStmts = VolStmts{stLookupEbBody = stmt}} = conn

-- | Immutable-partition fallback of 'sqlLookupEbBody'.
immLookupEbBody :: Conn -> EbHash -> IO [(TxHash, BytesSize)]
immLookupEbBody conn ebHash =
  useStmt stmt $ do
    dbBindBlob stmt 1 (let MkEbHash bytes = ebHash in bytes)
    bodyLoop stmt []
 where
  Conn{connImmStmts = ImmStmts{immStLookupEbBody = stmt}} = conn

bodyLoop :: DB.Statement -> [(TxHash, BytesSize)] -> IO [(TxHash, BytesSize)]
bodyLoop stmt acc =
  dbStep stmt >>= \case
    DB.Done -> pure (reverse acc)
    DB.Row -> do
      txHash <- MkTxHash <$> DB.columnBlob stmt 0
      size <- fromIntegral <$> DB.columnInt64 stmt 1
      bodyLoop stmt ((txHash, size) : acc)

sqlInsertEbPoint :: Conn -> LeiosPoint -> BytesSize -> IO ()
sqlInsertEbPoint conn point ebBytesSize = do
  inserted <- dbWithWriteTransaction conn $ useStmt stmt $ do
    dbBindInt64 stmt 1 (fromIntegral $ unSlotNo point.pointSlotNo)
    dbBindBlob stmt 2 point.pointEbHash.ebHashBytes
    dbBindInt64 stmt 3 (fromIntegral ebBytesSize)
    dbStep1 stmt
    DB.changes db
  bumpVolatileStats conn inserted
 where
  Conn{conVolDb = db, connVolStmts = VolStmts{stInsertEbPoint = stmt}} = conn

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
    -- Record which of this body's txs we still lack, then count them. Both in
    -- this transaction, so an arrival can never see the rows without the count
    -- or the other way round.
    useStmt stInsertMissingTxs $ do
      dbBindBlob stInsertMissingTxs 1 point.pointEbHash.ebHashBytes
      dbStep1 stInsertMissingTxs
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
  Conn{connVolStmts} = conn
  VolStmts
    { stInsertEbTxsRow
    , stInsertMissingTxs
    , stInitMissingCount
    , stMarkPointNotified
    } = connVolStmts

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
      when inserted $ do
        useStmt stDecrMissingCount $ do
          dbBindBlob stDecrMissingCount 1 txHashBytes
          dbStep1 stDecrMissingCount
        -- Strictly after the decrement, which reads these rows.
        useStmt stDeleteMissingTxs $ do
          dbBindBlob stDeleteMissingTxs 1 txHashBytes
          dbStep1 stDeleteMissingTxs
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
  Conn{connVolStmts} = conn
  VolStmts
    { stInsertTx
    , stDecrMissingCount
    , stDeleteMissingTxs
    , stFindCompleteEbs
    , stMarkNotifiedEbs
    } = connVolStmts
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
sqlBatchRetrieveTxs conn ebHash offsets = do
  vol <-
    dbWithTransaction db $ useStmt stmt $ do
      dbBindBlob stmt 1 (let MkEbHash bytes = ebHash in bytes)
      dbBindUtf8 stmt 2 (jsonIntArray offsets)
      retrieveLoop stmt []
  -- Zero rows means the EB's body is absent from the volatile partition
  -- entirely (a present body joins every requested offset): copied+evicted.
  if null vol && not (null offsets)
    then immBatchRetrieveTxs conn ebHash offsets
    else pure vol
 where
  Conn{conVolDb = db, connVolStmts = VolStmts{stBatchRetrieveTxs = stmt}} = conn

-- | Immutable-partition fallback of 'sqlBatchRetrieveTxs'. Closures land
-- there whole, so the joined tx bytes are never NULL.
immBatchRetrieveTxs ::
  Conn -> EbHash -> [Int] -> IO [(Int, TxHash, Maybe ByteString)]
immBatchRetrieveTxs conn ebHash offsets =
  useStmt stmt $ do
    dbBindBlob stmt 1 (let MkEbHash bytes = ebHash in bytes)
    dbBindUtf8 stmt 2 (jsonIntArray offsets)
    retrieveLoop stmt []
 where
  Conn{connImmStmts = ImmStmts{immStBatchRetrieveTxs = stmt}} = conn

retrieveLoop ::
  DB.Statement ->
  [(Int, TxHash, Maybe ByteString)] ->
  IO [(Int, TxHash, Maybe ByteString)]
retrieveLoop stmt acc =
  dbStep stmt >>= \case
    DB.Done -> pure (reverse acc)
    DB.Row -> do
      offset <- fromIntegral <$> DB.columnInt64 stmt 0
      txHash <- MkTxHash <$> DB.columnBlob stmt 1
      -- Column 2 is from LEFT JOIN, NULL if tx not in txs table
      txBytes <- DB.columnBlob stmt 2
      let mbTxBytes = if txBytes == mempty then Nothing else Just txBytes
      retrieveLoop stmt ((offset, txHash, mbTxBytes) : acc)

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
  Conn{conVolDb = db, connVolStmts = VolStmts{stFilterMissingTxs = stmt}} = conn
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
sqlLookupEbClosure conn ebHash = do
  vol <-
    dbWithTransaction db $ useStmt stmt $ do
      dbBindBlob stmt 1 (ebHashBytes ebHash)
      -- FIXME(bladyjoker): This should have a SlotNo as the second part of the key
      closureLoop stmt []
  -- 'Nothing' covers both no-body and any-tx-missing, which includes a copied
  -- EB re-announced and mid-refetch: the immutable partition must still
  -- answer for it, or replaying its cert-RB fails.
  case vol of
    Just rows -> pure (Just rows)
    Nothing -> immLookupEbClosure conn ebHash
 where
  Conn{conVolDb = db, connVolStmts = VolStmts{stLookupEbClosure = stmt}} = conn

-- | Immutable-partition fallback of 'sqlLookupEbClosure'. Closures land there
-- atomically and whole, so any rows are all the rows.
immLookupEbClosure :: Conn -> EbHash -> IO (Maybe [(TxHash, ByteString)])
immLookupEbClosure conn ebHash =
  useStmt stmt $ do
    dbBindBlob stmt 1 (ebHashBytes ebHash)
    closureLoop stmt []
 where
  Conn{connImmStmts = ImmStmts{immStLookupEbClosure = stmt}} = conn

closureLoop ::
  DB.Statement -> [(TxHash, ByteString)] -> IO (Maybe [(TxHash, ByteString)])
closureLoop stmt acc =
  dbStep stmt >>= \case
    DB.Done ->
      -- No rows means the EB body hasn't been downloaded yet
      if null acc then pure Nothing else pure $ Just (reverse acc)
    DB.Row -> do
      txHash <- MkTxHash <$> DB.columnBlob stmt 0
      txBytes :: ByteString <- DB.columnBlob stmt 1
      if txBytes == mempty
        then return Nothing
        else closureLoop stmt ((txHash, txBytes) : acc)

-- * SQL strings

-- | Schema of both partitions (@leios.db.vol@ and @leios.db.imm@): identical
-- on purpose, so the fallback reads reuse the volatile SQL verbatim and the
-- copy is a server-side @INSERT ... SELECT@ over ATTACH. In the immutable
-- file 'missingTxCount', @status@ and @ebsMissingTxs@ are unused (rows land
-- complete, with the canonical @missingTxCount = -1, status = 2@).
sql_schema :: String
sql_schema =
  unlines
    [ "CREATE TABLE ebs ("
    , "  ebSlot INTEGER NOT NULL,"
    , "  ebHashBytes BLOB NOT NULL,"
    , "  ebBytesSize INTEGER NOT NULL,"
    , -- NULL = body not downloaded, >0 = txs missing, 0 = just completed, <0 = notified
      "  missingTxCount INTEGER,"
    , -- 0 = volatile, 1 = certified/pinned awaiting copy,
      -- 2 = copied to the immutable partition (evictable),
      -- 3 = marked for GC, awaiting the sweeper
      "  status INTEGER NOT NULL DEFAULT 0,"
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
    , -- The tx -> referencing-EB direction, for GC's orphaned-txs sweep:
      -- without it the sweep's NOT EXISTS probe is a full scan of ebTxs per
      -- candidate tx, O(gcBatchSize * txsPerEb * |ebTxs|) per eviction batch.
      "CREATE INDEX idx_ebTxs_txHashBytes ON ebTxs(txHashBytes);"
    , "CREATE TABLE ebsMissingTxs ("
    , "  txHashBytes BLOB NOT NULL,"
    , "  ebHashBytes BLOB NOT NULL,"
    , "  PRIMARY KEY (txHashBytes, ebHashBytes)"
    , ");"
    , "CREATE INDEX idx_ebsMissingTxs_ebHashBytes ON ebsMissingTxs(ebHashBytes);"
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

-- | Find EBs that are now complete (missingTxCount reached 0). Volatile rows
-- only: completion is decided within the one coherent volatile set.
sql_find_complete_ebs :: String
sql_find_complete_ebs =
  "SELECT ebHashBytes, ebSlot FROM ebs WHERE missingTxCount = 0 AND status = 0"

-- | Mark complete EBs as notified so they are not found again by
-- 'sql_find_complete_ebs'. Uses -1 as a sentinel for "already notified".
sql_mark_notified_ebs :: String
sql_mark_notified_ebs =
  "UPDATE ebs SET missingTxCount = -1 WHERE missingTxCount = 0 AND status = 0"

-- | Decrement missingTxCount for every EB still /waiting/ on the given txHash.
--
-- Uses 'ebsMissingTxs' rather than 'ebTxs', which makes this more efficient
-- than a full scan of 'ebTxs' in the average case.
--
-- Must be paired with 'sql_delete_missing_txs' in the same transaction.
--
-- Parameter 1: txHashBytes
sql_decrement_missing_tx_count :: String
sql_decrement_missing_tx_count =
  "UPDATE ebs SET missingTxCount = missingTxCount - 1\n\
  \WHERE ebHashBytes IN (SELECT ebHashBytes FROM ebsMissingTxs WHERE txHashBytes = ?)\n\
  \  AND status = 0\n\
  \"

-- | Retire the waiting rows for a tx that has just landed.
-- Parameter 1: txHashBytes
sql_delete_missing_txs :: String
sql_delete_missing_txs =
  "DELETE FROM ebsMissingTxs WHERE txHashBytes = ?"

-- | Record which of a freshly-inserted body's txs we do not yet hold.
--
-- One anti-join over the EB's own 'ebTxs' range -- the same work
-- 'sql_init_missing_tx_count' used to do to produce a count, now materialised so
-- that the arrival side reads the rows instead of recomputing them. Paying it
-- here rather than on every tx arrival is what earns the index removal: this
-- runs once per body, against ~4.7 times per tx for the old reverse lookup.
--
-- Parameter 1: ebHashBytes
sql_insert_missing_txs :: String
sql_insert_missing_txs =
  "INSERT OR IGNORE INTO ebsMissingTxs (txHashBytes, ebHashBytes)\n\
  \SELECT e.txHashBytes, e.ebHashBytes FROM ebTxs e\n\
  \LEFT JOIN txs t ON e.txHashBytes = t.txHashBytes\n\
  \WHERE e.ebHashBytes = ? AND t.txHashBytes IS NULL\n\
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
  \    SELECT COUNT(*) FROM ebsMissingTxs WHERE ebHashBytes = ?\n\
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

-- | Every recent announcement, with or without completeness evidence.
-- Companion of 'sql_scan_complete_ebs_since': the difference of the two sets
-- is probed against the immutable partition (see
-- 'sqlScanCompleteEbPointsSince').
sql_scan_recent_ebs :: String
sql_scan_recent_ebs =
  "SELECT MAX(ebSlot), ebHashBytes FROM ebs\n\
  \WHERE ebSlot >= ?\n\
  \GROUP BY ebHashBytes\n\
  \"

-- ** Promoting an EB to the immutable partition

-- | Pin every announcement row of the EB: certified, awaiting copy, never
-- evicted. The durable record that survives a crash before the copy lands.
-- Also rescues GC-marked rows (@status = 3@): a promotion arriving between
-- mark and sweep unmarks the hash, and the sweeper's all-marked pick
-- then skips it.
sql_pin_eb :: String
sql_pin_eb =
  "UPDATE ebs SET status = 1 WHERE ebHashBytes = ? AND status IN (0, 3)"

-- | Mark a pinned EB as copied (evictable), through the copier's connection
-- (the volatile partition is attached as @vol@ there). Only run strictly
-- after the immutable partition committed the EB's closure.
sql_mark_as_copied :: String
sql_mark_as_copied =
  "UPDATE vol.ebs SET status = 2 WHERE ebHashBytes = ? AND status = 1"

-- | Whether the immutable partition already holds the EB.
sql_imm_has_eb :: String
sql_imm_has_eb =
  "SELECT EXISTS (SELECT 1 FROM ebs WHERE ebHashBytes = ?1)"

-- | Body row count and closure row count of the EB in the volatile
-- partition, in one probe: the LEFT JOIN's second count skips missing txs,
-- so the EB's closure is complete iff both counts are equal (and non-zero).
sql_copy_completeness :: String
sql_copy_completeness =
  "SELECT COUNT(*), COUNT(t.txHashBytes)\n\
  \FROM vol.ebTxs e LEFT JOIN vol.txs t ON t.txHashBytes = e.txHashBytes\n\
  \WHERE e.ebHashBytes = ?1\n\
  \"

-- | Copy the EB's newest announcement row, with the canonical immutable
-- column values (@missingTxCount = -1@: complete and notified; @status = 2@:
-- copied).
sql_copy_insert_eb :: String
sql_copy_insert_eb =
  "INSERT INTO ebs (ebSlot, ebHashBytes, ebBytesSize, missingTxCount, status)\n\
  \SELECT ebSlot, ebHashBytes, ebBytesSize, -1, 2 FROM vol.ebs\n\
  \WHERE ebHashBytes = ?1\n\
  \ORDER BY ebSlot DESC LIMIT 1\n\
  \"

-- | Copy the EB's body rows.
sql_copy_insert_ebTxs :: String
sql_copy_insert_ebTxs =
  "INSERT INTO ebTxs (ebHashBytes, txOffset, txHashBytes, txBytesSize)\n\
  \SELECT ebHashBytes, txOffset, txHashBytes, txBytesSize FROM vol.ebTxs\n\
  \WHERE ebHashBytes = ?1\n\
  \"

-- | Copy the EB's txs. @OR IGNORE@: a tx shared with an earlier-copied EB is
-- already present.
sql_copy_insert_txs :: String
sql_copy_insert_txs =
  "INSERT OR IGNORE INTO txs (txHashBytes, txBytes, txBytesSize)\n\
  \SELECT t.txHashBytes, t.txBytes, t.txBytesSize FROM vol.txs t\n\
  \WHERE t.txHashBytes IN\n\
  \  (SELECT txHashBytes FROM vol.ebTxs WHERE ebHashBytes = ?1)\n\
  \"

-- | Which of the given hashes (JSON hex array) the immutable partition holds.
-- Presence is proof of a complete closure: copies land atomically and only
-- complete EBs are copied.
sql_imm_filter_present :: String
sql_imm_filter_present =
  "SELECT unhex(je.value) FROM json_each(?1) je\n\
  \WHERE EXISTS (SELECT 1 FROM ebs e WHERE e.ebHashBytes = unhex(je.value))\n\
  \"

-- ** Garbage collection of the volatile partition

-- | GC-only objects of the volatile partition, applied idempotently on every
-- read-write open ('openVolRawConnection'), so pre-existing files migrate on
-- first open. Deliberately not part of 'sql_schema': in the immutable
-- partition every row has @status = 2@, so @idx_ebs_sweepable@ there would
-- index the whole table for nothing.
sql_schema_gc :: String
sql_schema_gc =
  unlines
    [ -- Persistent orphan-tx hints: txs of GC-marked EBs, deleted only once
      -- provably unreferenced ('sql_sweep_orphan_txs'). Survives restarts
      -- together with the status = 3 marks.
      "CREATE TABLE IF NOT EXISTS gcTxCandidates (txHashBytes BLOB NOT NULL PRIMARY KEY);"
    , -- What the mark scan reads; marking removes the row from it, so
      -- each row is marked at most once.
      "CREATE INDEX IF NOT EXISTS idx_ebs_sweepable ON ebs(ebSlot) WHERE status IN (0, 2);"
    , -- What the sweeper's batch pick reads.
      "CREATE INDEX IF NOT EXISTS idx_ebs_markedForGc ON ebs(ebHashBytes) WHERE status = 3;"
    ]

-- | Pinned EBs the copier has not marked as copied yet, for self-heal
-- re-enqueueing.
sql_gc_stale_pins :: String
sql_gc_stale_pins =
  "SELECT DISTINCT ebHashBytes FROM ebs WHERE status = 1 AND ebSlot < ?1"

-- | Whether a GC at slot @?1@ could has any work, i.e.
--   if there are any old volatile EBs or already copied EBs.
sql_gc_has_work :: String
sql_gc_has_work =
  "SELECT EXISTS (SELECT 1 FROM ebs WHERE status IN (0, 2) AND ebSlot < ?1)"

-- | The markability predicate, shared by 'sql_gc_mark' and
-- 'sql_gc_stage_marked' so the marked set and the staged set can never
-- diverge. @c@ is the row under test; it is markable if it is
--   - old enough (its slot is before the GC frontier @?1@) and
--   - either volatile (status 0) or already copied (status 2) and
--   - not vetoed by a live row of the same hash (pinned, or announced at or
--     after the frontier).
sql_gc_markable :: String
sql_gc_markable =
  "c.status IN (0, 2) AND c.ebSlot < ?1\n\
  \  AND NOT EXISTS\n\
  \    (SELECT 1 FROM ebs live\n\
  \     WHERE live.ebHashBytes = c.ebHashBytes\n\
  \       AND (live.status = 1 OR live.ebSlot >= ?1))"

-- | Add the txs of every EB 'sql_gc_mark' is about to hit as GC candidates.
-- Must run strictly BEFORE 'sql_gc_mark' in the same transaction:
-- the UPDATE changes the 'status' of EBs and orphans the transactions.
sql_gc_stage_marked :: String
sql_gc_stage_marked =
  "INSERT OR IGNORE INTO gcTxCandidates (txHashBytes)\n\
  \SELECT DISTINCT e.txHashBytes FROM ebTxs e\n\
  \WHERE e.ebHashBytes IN\n\
  \  (SELECT DISTINCT c.ebHashBytes FROM ebs c\n\
  \   WHERE "
    <> sql_gc_markable
    <> ")"

-- | Mark for GC (@status = 3@) every row satisfying 'sql_gc_markable'.
sql_gc_mark :: String
sql_gc_mark =
  "UPDATE ebs AS c SET status = 3\n\
  \WHERE " <> sql_gc_markable

-- | Up to @?1@ GC-marked EBs ready to sweep.
--
-- Intuition: give me up to N hash values that are currently in status 3
-- and have never been assigned any status other than 3.
--
-- Note: 'FROM ebs cand' and 'FROM ebs live' allows referring to the rows
-- of the 'ebs' table using the alias 'cand' and 'live'.
--
-- TODO(geo2a): think how to simplify this query.
sql_sweep_pick_marked :: String
sql_sweep_pick_marked =
  "SELECT DISTINCT cand.ebHashBytes FROM ebs cand\n\
  \WHERE cand.status = 3\n\
  \  AND NOT EXISTS\n\
  \    (SELECT 1 FROM ebs live\n\
  \     WHERE live.ebHashBytes = cand.ebHashBytes AND live.status <> 3)\n\
  \LIMIT ?1\n\
  \"

-- | Evict the 'ebTxs' rows with the specified 'ebHashBytes' (a JSON array of byte strings).
sql_gc_ebTxs :: String
sql_gc_ebTxs =
  "DELETE FROM ebTxs WHERE ebHashBytes IN (SELECT unhex(je.value) FROM json_each(?1) je)"

-- | Evict the 'ebsMissingTxs' rows with the specified 'ebHashBytes' (a JSON array of byte strings).
sql_gc_missing_txs :: String
sql_gc_missing_txs =
  "DELETE FROM ebsMissingTxs WHERE ebHashBytes IN (SELECT unhex(je.value) FROM json_each(?1) je)"

-- | Evict the 'ebs' rows with the specified 'ebHashBytes' (a JSON array of byte strings).
sql_gc_ebs_by_hash :: String
sql_gc_ebs_by_hash =
  "DELETE FROM ebs WHERE ebHashBytes IN (SELECT unhex(je.value) FROM json_each(?1) je)"

-- | Whether any GC-marked EBs remain.
sql_sweep_any_marked :: String
sql_sweep_any_marked =
  "SELECT EXISTS (SELECT 1 FROM ebs WHERE status = 3)"

-- | Get up to @?1@ transactions to be evicted.
sql_sweep_pick_orphans :: String
sql_sweep_pick_orphans =
  "SELECT txHashBytes FROM gcTxCandidates LIMIT ?1"

-- | Evict transactions with the specified hashes (a JSON array of byte strings),
--   making sure that they are not referenced by any EBs.
sql_sweep_orphan_txs :: String
sql_sweep_orphan_txs =
  "DELETE FROM txs\n\
  \WHERE txHashBytes IN (SELECT unhex(je.value) FROM json_each(?1) je)\n\
  \  AND NOT EXISTS\n\
  \    (SELECT 1 FROM ebTxs WHERE ebTxs.txHashBytes = txs.txHashBytes)\n\
  \"

-- | Delete GC transaction candidates with the specified hashes (a JSON array of byte strings).
sql_sweep_pop_orphans :: String
sql_sweep_pop_orphans =
  "DELETE FROM gcTxCandidates\n\
  \WHERE txHashBytes IN (SELECT unhex(je.value) FROM json_each(?1) je)\n\
  \"

-- | Whether the volatile partition holds any unstaged GC candidates (txs no
-- EB references).
sql_has_unstaged_gc_candidates :: String
sql_has_unstaged_gc_candidates =
  "SELECT EXISTS (SELECT 1 FROM txs WHERE NOT EXISTS\n\
  \  (SELECT 1 FROM ebTxs WHERE ebTxs.txHashBytes = txs.txHashBytes))\n\
  \"

-- | One keyset page of unstaged GC candidates (txs no EB references), for
-- 'reinitialiseGcTxCandidates': @?1@ = cursor (exclusive), @?2@ = page size.
sql_unstaged_gc_candidates_page :: String
sql_unstaged_gc_candidates_page =
  "SELECT txHashBytes FROM txs\n\
  \WHERE txHashBytes > ?1\n\
  \  AND NOT EXISTS (SELECT 1 FROM ebTxs WHERE ebTxs.txHashBytes = txs.txHashBytes)\n\
  \ORDER BY txHashBytes LIMIT ?2\n\
  \"

-- | Stage one page of GC candidates (JSON hex array @?1@).
sql_insert_gc_candidates :: String
sql_insert_gc_candidates =
  "INSERT OR IGNORE INTO gcTxCandidates (txHashBytes)\n\
  \SELECT unhex(je.value) FROM json_each(?1) je\n\
  \"

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

-- | Finalize a statement, exactly once, ignoring the return code.
--
-- @sqlite3_finalize@ always frees the statement; its return code merely
-- replays the most recent evaluation's error (sticky, like 'DB.reset' -- see
-- 'useStmt'). Neither retrying nor throwing is ever right here: a busy-retry
-- would call @sqlite3_finalize@ on freed memory (the use-after-free behind
-- the devnet segfaults of 2026-08-31), and a throw would propagate from
-- bracket cleanup.
dbFinalize :: DB.Statement -> IO ()
dbFinalize q = void $ DB.finalize q

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
dbWithWriteTransaction Conn{conVolDb, connTracer} =
  dbWithWriteTransactionRaw connTracer conVolDb

-- | 'dbWithWriteTransaction' for the maintenance paths (promotion to immutable, GC), which hold
-- a raw 'DB.Database' rather than a 'Conn'.
dbWithWriteTransactionRaw ::
  HasCallStack => Tracer IO TraceLeiosDb -> DB.Database -> IO a -> IO a
dbWithWriteTransactionRaw tracer db k = getMonotonicTime >>= go 0
 where
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

-- | 'dbStep' through the safe FFI call ('DB.step' rather than
-- 'DB.stepNoCB'): a safe call does not block its RTS capability, so the
-- potentially long-running maintenance statements (copy, GC) must use it.
dbStepSafe :: HasCallStack => DB.Statement -> IO DB.StepResult
dbStepSafe stmt = withDieStmt stmt $ DB.step stmt

-- | 'dbStep1' through the safe FFI call; see 'dbStepSafe'.
dbStep1Safe :: HasCallStack => DB.Statement -> IO ()
dbStep1Safe stmt = withDieDoneStmt stmt $ DB.step stmt

-- | Read a single-row, single-column integer result, stepping (safe FFI)
-- through to completion -- which also suits @RETURNING@ statements, whose
-- write only certainly happened once they report 'DB.Done'.
readSingleInt64 :: HasCallStack => DB.Statement -> IO Int64
readSingleInt64 stmt =
  dbStepSafe stmt >>= \case
    DB.Done -> error "readSingleInt64: expected a row"
    DB.Row -> do
      n <- DB.columnInt64 stmt 0
      dbStepSafe stmt >>= \case
        DB.Done -> pure n
        DB.Row -> error "readSingleInt64: expected exactly one row"

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
