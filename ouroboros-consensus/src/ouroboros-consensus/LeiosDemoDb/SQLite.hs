{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LeiosDemoDb.SQLite
  ( newLeiosDBSQLiteFromEnv
  , newLeiosDBSQLite
  , newLeiosDBSQLiteWithGcBatchSize
  , withLeiosDBSQLite

    -- * Re-exported for internal tooling
  , truncateLeiosDbAfterSlot
  , deleteDanglingTxs
  , vacuumLeiosDb

    -- * SQL strings (re-exported for leios-schedule-gen)
  , sql_schema
  , sql_insert_eb
  , sql_insert_ebBody
  , sql_insert_tx
  ) where

import Cardano.Prelude (forM_, traverse_, when)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTBQueue
  , StrictTChan
  , StrictTMVar
  , StrictTVar
  , check
  , dupTChan
  , isEmptyTBQueue
  , isFullTBQueue
  , modifyTVar
  , newBroadcastTChan
  , newEmptyTMVarIO
  , newTBQueueIO
  , newTVarIO
  , putTMVar
  , readTMVar
  , readTVar
  , readTVarIO
  , tryReadTBQueue
  , writeTBQueue
  , writeTChan
  , writeTVar
  )
import Control.Exception
  ( BlockedIndefinitelyOnSTM (..)
  , SomeException
  , fromException
  , throwIO
  , toException
  )
import Control.Monad (filterM, forever, join, unless, void)
import Control.Monad.Class.MonadThrow
  ( bracket
  , catch
  , displayException
  , finally
  , generalBracket
  , onException
  , try
  )
import Control.Tracer (Tracer, traceWith)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.String (fromString)
import Database.SQLite3
  ( SQLOpenFlag (..)
  , SQLVFS (..)
  , open2
  )
import qualified Database.SQLite3.Direct as DB
import qualified GHC.Conc as IO (atomically)
import GHC.Stack (HasCallStack)
import qualified GHC.Stack
import LeiosDemoDb.Common
  ( CompletedEbs
  , LeiosDbHandle (..)
  , LeiosDbReader (..)
  , LeiosDbWriter (..)
  , LeiosEbNotification (..)
  , Promise (..)
  )
import LeiosDemoDb.Trace (LeiosDbStats (..), TraceLeiosDb (..))
import LeiosDemoException (LeiosDbException (..), throwLeiosDbException)
import LeiosDemoTypes
  ( BytesSize
  , EbHash (..)
  , LeiosEb
  , LeiosPoint (..)
  , TxHash (..)
  , encodeLeiosEbSize
  , leiosEbBodyItems
  , leiosEbTxs
  )
import LeiosUtils.CallTrace
  ( CallCtx
  , CallName
  , SomeJsonCallTrace (..)
  , callTraceSameThread
  , rootCallCtx
  )
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Util.IOLike
  ( ExitCase (..)
  , MonadAsync (async, asyncThreadId)
  , atomically
  , labelThread
  , link
  )
import System.Directory (createDirectoryIfMissing, doesFileExist, getFileSize)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.FilePath (takeDirectory)

-- * Public API

--- | Create a new Leios database connection from environment variable.
--- This looks up the LEIOS_VOL_DB_PATH and LEIOS_VOL_DB_PATH environment variables
--  and opens the database.
newLeiosDBSQLiteFromEnv :: Tracer IO TraceLeiosDb -> IO (LeiosDbHandle IO)
newLeiosDBSQLiteFromEnv tracer = do
  volDbPath <-
    lookupEnv "LEIOS_VOL_DB_PATH" >>= \case
      Nothing -> die "You must define the LEIOS_VOL_DB_PATH variable for this demo."
      Just x -> pure x
  immDbPath <-
    lookupEnv "LEIOS_IMM_DB_PATH" >>= \case
      Nothing -> die "You must define the LEIOS_IMM_DB_PATH variable for this demo."
      Just x -> pure x
  newLeiosDBSQLite tracer volDbPath immDbPath

-- | Create a new Leios database using the SQLite implementation.
--
-- Each call to 'openReader' on the returned handle creates new SQLite
-- connections; readers are not thread-safe and should not be shared across
-- threads. All writers submit to the one write connection created here, on
-- its own worker thread.
--
-- Note: this also starts a thread that samples the database's size.
newLeiosDBSQLite :: Tracer IO TraceLeiosDb -> FilePath -> FilePath -> IO (LeiosDbHandle IO)
newLeiosDBSQLite tracer volLeiosDbPath immLeiosDbPath =
  newLeiosDBSQLiteWithGcBatchSize tracer volLeiosDbPath immLeiosDbPath defaultGcBatchSize

-- | 'newLeiosDBSQLite' with an explicit GC sweep batch size: how many EBs
-- the writer evicts per turn, between the jobs it serves.
--
-- Note that orphan transaction batch size is set by the 'gcOrphanTxBatchSize' constant.
newLeiosDBSQLiteWithGcBatchSize ::
  Tracer IO TraceLeiosDb -> FilePath -> FilePath -> Int64 -> IO (LeiosDbHandle IO)
newLeiosDBSQLiteWithGcBatchSize tracer volLeiosDbPath immLeiosDbPath gcBatchSize = do
  -- The database opens before whoever owns these directories creates them.
  mapM_ (createDirectoryIfMissing True . takeDirectory) [volLeiosDbPath, immLeiosDbPath]
  notificationChan <- atomically newBroadcastTChan
  -- seed the in-memory stats by counting the EB rows once per handle
  statsVar <- newTVarIO =<< initialStats volLeiosDbPath immLeiosDbPath
  -- start a thread to sample the sizes of the LeiosDB
  samplerId <- startVolatileStatsSampler tracer statsVar volLeiosDbPath
  -- Both start set, so a restart picks up whatever the last run left
  -- pinned or marked; the copier and the writer clear them once they find
  -- nothing.
  copyPending <- newTVarIO True
  sweepDoorbell <- newTVarIO True
  gcRootCtx <- rootCallCtx "leiosdb-gc"

  -- The volatile partition's one writer: every write to it -- ingest,
  -- promotion, GC -- happens on this one worker, on the only open write
  -- connection, created with the database and torn down by the returned
  -- action (or by orphanhood, for callers that never tear down).
  writeQueue <-
    startWriter
      tracer
      statsVar
      notificationChan
      sweepDoorbell
      gcBatchSize
      volLeiosDbPath
      immLeiosDbPath
  -- And the immutable partition's one writer, off the queue so that a copy
  -- -- which is O(closure) -- never holds up ingest.
  stopCopier <-
    startCopier
      tracer
      statsVar
      copyPending
      writeQueue
      volLeiosDbPath
      immLeiosDbPath
  pure
    LeiosDbHandle
      { close = close samplerId stopCopier writeQueue
      , openReader = openReader statsVar
      , openWriter = openWriter writeQueue
      , subscribeEbNotifications = atomically (dupTChan notificationChan)
      , leiosDbGarbageCollect = sqlGarbageCollect tracer gcRootCtx writeQueue
      , leiosDbPromoteToImmutable = sqlPromoteToImmutable writeQueue copyPending
      , leiosDbSampleStats = readTVarIO statsVar
      }
 where
  close :: ThreadId -> IO () -> WriteQueue -> IO ()
  close samplerId stopCopier writeQueue = do
    -- The sampler holds no connection -- it only reads counters -- so
    -- killing it is safe at any point.
    killThread samplerId
    -- The copier first: it submits to the writer, so it must be gone
    -- before the writer stops serving. It reports whatever ended it, and
    -- that report must not cost the writer its shutdown.
    stopCopier `finally` shutdownWriter
   where
    -- The queue is FIFO, so serving this job flushes everything
    -- submitted before it; awaiting it waits for the connections to
    -- close, and a failed close propagates -- a leaked connection must
    -- be loud.
    --
    -- A sealed queue refuses the job. Nothing is left to close then: the
    -- worker closes the connections on every exit path before it seals,
    -- and whatever killed it already reached the awaiter of the write
    -- that failed.
    shutdownWriter =
      try (submitJob writeQueue Shutdown) >>= \case
        Left (_writerGone :: LeiosDbException) -> pure ()
        Right promise -> await promise

  openReader statsVar = do
    volDb <- openVolRawConnection volLeiosDbPath
    immDb <- orCloseOnError volDb $ openRawConnection immLeiosDbPath
    conn <- mkConn tracer statsVar volDb immDb
    pure
      LeiosDbReader
        { close = closeConn conn
        , scanEbPoints = sqlScanEbPoints conn
        , scanCompleteEbClosuresNotOlderThanSlot = sqlScanCompleteEbPointsSince conn
        , lookupEbBody = sqlLookupEbBody conn
        , batchRetrieveTxs = sqlBatchRetrieveTxs conn
        , lookupEbClosure = sqlLookupEbClosure conn
        }

  openWriter writeQueue =
    pure
      LeiosDbWriter
        { -- Not a teardown -- the write connection outlives every writer.
          close = void . await =<< submitJob writeQueue Flush
        , writeEbPoint = \point size -> submitJob writeQueue (WriteEbPoint point size)
        , writeEbBody = \point eb -> submitJob writeQueue (WriteEbBody point eb)
        , writeTxs = \txs -> submitJob writeQueue (WriteTxs txs)
        }

-- | 'newLeiosDBSQLite' bracketed with its 'close': on release every pending
-- write has landed, the background threads are gone and the connections are
-- closed, so e.g. the database files can be deleted.
withLeiosDBSQLite ::
  Tracer IO TraceLeiosDb -> FilePath -> FilePath -> (LeiosDbHandle IO -> IO a) -> IO a
withLeiosDBSQLite tracer volLeiosDbPath immLeiosDbPath =
  bracket
    (newLeiosDBSQLite tracer volLeiosDbPath immLeiosDbPath)
    (\db -> db.close)

-- | Initialise 'LeiosDbStats' by counting the EB rows of both partitions.
--   This will only run once per process.
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
  Tracer IO TraceLeiosDb -> StrictTVar IO LeiosDbStats -> FilePath -> IO ThreadId
startVolatileStatsSampler tracer statsVar volPath =
  forkIO $ forever $ do
    -- wait one sample window to side-step contention with starting the LeiosDB
    threadDelay tenSeconds
    stats <- readTVarIO statsVar
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
bumpVolatileStats Conn{connStats} = bumpVolatileStatsVar connStats

-- | 'bumpVolatileStats' for the maintenance paths, which have no 'Conn'.
bumpVolatileStatsVar :: StrictTVar IO LeiosDbStats -> Int -> IO ()
bumpVolatileStatsVar statsVar dEbs =
  unless (dEbs == 0) $
    atomically $
      modifyTVar statsVar $
        \s -> s{volatileEbs = s.volatileEbs + dEbs}

-- | Fold a delta into the immutable EB count of the in-memory 'LeiosDbStats'.
bumpImmutableStats :: StrictTVar IO LeiosDbStats -> Int -> IO ()
bumpImmutableStats statsVar dEbs =
  unless (dEbs == 0) $
    atomically $
      modifyTVar statsVar $
        \s -> s{immutableEbs = s.immutableEbs + dEbs}

-- | Open a strictly read-only connection, for sampling DB statistics.
--
-- Opening fails harmlessly if the database does not exist yet; the caller
-- swallows it and tries again on the next tick.
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

-- | Run a query that yields exactly one integer column.
queryInt64 :: HasCallStack => DB.Database -> String -> IO Int64
queryInt64 db sql =
  bracket (dbPrepare db (fromString sql)) dbFinalize $ \stmt ->
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

-- | Prepare a statement, run the action, finalize.
withStmt :: HasCallStack => DB.Database -> String -> (DB.Statement -> IO a) -> IO a
withStmt db sql = bracket (dbPrepare db (fromString sql)) dbFinalize

-- * Copying EBs to the immutable partition

-- | Implements 'leiosDbPromoteToImmutable':
--   - pin the EB rows in the volatile partition for promotion (@status@ 0 -> 1),
--     through the writer ('PinEb');
--   - put the hash into the queue for the copier to pick up, only once the pin
--     is durable.
sqlPromoteToImmutable :: WriteQueue -> StrictTVar IO Bool -> [LeiosPoint] -> IO ()
sqlPromoteToImmutable writeQueue copyPending points = unless (null points) $ do
  await =<< submitJob writeQueue (PinEb [p.pointEbHash | p <- points])
  -- The pin is the work list; this only saves the writer a lookup when
  -- there is nothing to copy.
  atomically $ writeTVar copyPending True

-- | The copy statements, prepared on the writer's immutable connection --
-- main is the immutable file, the volatile file is ATTACHed as @vol@.
data CopierConn = CopierConn
  { ccDb :: !DB.Database
  -- ^ main = immutable partition, @vol@ = attached volatile partition
  , ccCompleteness :: !DB.Statement
  -- ^ 'sql_copy_completeness'
  , ccInsertEb :: !DB.Statement
  -- ^ 'sql_copy_insert_eb'
  , ccInsertEbTxs :: !DB.Statement
  -- ^ 'sql_copy_insert_ebTxs'
  , ccInsertTxs :: !DB.Statement
  -- ^ 'sql_copy_insert_txs'
  }

-- | Prepare the copy statements on the writer's immutable connection, which
-- must already have the volatile partition ATTACHed as @vol@.
prepareCopierConn :: HasCallStack => DB.Database -> IO CopierConn
prepareCopierConn ccDb = do
  ccCompleteness <- dbPrepare ccDb (fromString sql_copy_completeness)
  ccInsertEb <- dbPrepare ccDb (fromString sql_copy_insert_eb)
  ccInsertEbTxs <- dbPrepare ccDb (fromString sql_copy_insert_ebTxs)
  ccInsertTxs <- dbPrepare ccDb (fromString sql_copy_insert_txs)
  pure CopierConn{..}

-- | Statements only; 'ccDb' is the writer's immutable connection.
finalizeCopierConn :: CopierConn -> IO ()
finalizeCopierConn CopierConn{..} = do
  dbFinalize ccCompleteness
  dbFinalize ccInsertEb
  dbFinalize ccInsertEbTxs
  dbFinalize ccInsertTxs

-- | Copy one pinned EB's closure into the immutable partition. 'True' when
-- it landed, so the caller may have its volatile rows marked as copied.
copyEbToImmutable ::
  Tracer IO TraceLeiosDb ->
  StrictTVar IO LeiosDbStats ->
  CopierConn ->
  EbHash ->
  IO Bool
copyEbToImmutable tracer statsVar conn ebHash =
  appendToImmutable >>= \case
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
 where
  CopierConn{ccDb, ccCompleteness, ccInsertEb, ccInsertEbTxs, ccInsertTxs} = conn

  -- Attempt to do the actual copying.
  --
  -- Returns the number of copied body rows, or Nothing if
  -- the volatile partition does not hold the full closure.
  appendToImmutable :: IO (Maybe Int)
  appendToImmutable =
    dbWithTransaction ccDb $ do
      -- the cert-RB is on our chain, hence the EB must be complete by this point.
      -- Still check if it is, as a defensive programming measure, as it's very cheap.
      (bodyCount, closureCount) <-
        useStmt ccCompleteness $ do
          dbBindBlob ccCompleteness 1 ebHash.ebHashBytes
          -- step the first time, expecting a single result row
          dbStepSafe ccCompleteness >>= \case
            DB.Done ->
              -- no row: critical error, fail fast and loud.
              -- this should not happen.
              throwLeiosDbException "sql_copy_completeness: expected a row"
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
                  throwLeiosDbException "sql_copy_completeness: expected exactly one row"
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

-- | The copier: the only writer into the immutable partition.
--
-- A copy is O(closure) -- every tx body of the EB -- and writes nothing but
-- the immutable partition, so it runs off the writer, on its own connection
-- (main = immutable, the volatile partition ATTACHed as @vol@). Ingest keeps
-- the volatile write lock throughout; WAL lets the copy read the volatile
-- partition while it does. The mark that follows a copy /is/ a volatile
-- write, and goes through the writer like every other one.
--
-- Returns the action that stops the copier and closes its connection.
startCopier ::
  Tracer IO TraceLeiosDb ->
  StrictTVar IO LeiosDbStats ->
  StrictTVar IO Bool ->
  WriteQueue ->
  FilePath ->
  FilePath ->
  IO (IO ())
startCopier tracer statsVar copyPending writeQueue volPath immPath = do
  ccDb <- openRawConnection immPath
  (copierConn, nextPinnedStmt) <-
    ( do
        withStmt ccDb "ATTACH ? AS vol" $ \stmt -> do
          dbBindUtf8 stmt 1 (fromString volPath)
          dbStep1Safe stmt
        copierConn <- prepareCopierConn ccDb
        nextPinnedStmt <- dbPrepare ccDb (fromString sql_next_pinned_eb)
        pure (copierConn, nextPinnedStmt)
    )
      `onException` void (DB.close ccDb)
  stopVar <- newTVarIO False
  stoppedVar <- newEmptyTMVarIO
  let nextPinnedBatch = do
        dbBindInt64 nextPinnedStmt 1 (fromIntegral copyBatchSize)
        useStmt nextPinnedStmt $
          let rows acc =
                dbStepSafe nextPinnedStmt >>= \case
                  DB.Done -> pure (reverse acc)
                  DB.Row -> do
                    h <- MkEbHash <$> DB.columnBlob nextPinnedStmt 0
                    rows (h : acc)
           in rows []

      -- 'True' when the EB is in the immutable partition and its pin can be
      -- retired. The EB stays pinned either way, so it is still next to copy.
      -- Backing off is what keeps a closure that never completes -- or a
      -- partition that keeps refusing the write -- from spinning here.
      copyOne ebHash =
        copyEbToImmutable tracer statsVar copierConn ebHash
          `catch` \(e :: LeiosDbException) -> do
            _ <- DB.exec ccDb "ROLLBACK"
            traceWith tracer $ TraceLeiosDbCopyError (show ebHash) (displayException e)
            pure False

      -- One 'MarkCopied' for the batch: the mark is what makes a copied EB
      -- evictable, and under sync a queue round-trip per EB queues behind
      -- saturated ingest.
      copyBatch ebHashes = do
        copied <- filterM copyOne ebHashes
        if null copied
          then threadDelay copyRetryMicros
          else void . await =<< submitJob writeQueue (MarkCopied copied)

      closeConnection = do
        finalizeCopierConn copierConn
        dbFinalize nextPinnedStmt
        closeChecked ccDb

      -- Check the stop request on every pass, not only when nothing is left
      -- to copy. A steady stream of promotions keeps the batch non-empty, so a
      -- check on the empty batch alone makes 'stopCopier' wait for the copier
      -- to catch up. This bounds the wait at one batch plus one backoff.
      loop = do
        stopping <- readTVarIO stopVar
        unless stopping $ do
          -- Clear before looking, so a pin that lands while we look rings
          -- again instead of being lost.
          atomically $ writeTVar copyPending False
          nextPinnedBatch >>= \case
            batch@(_ : _) -> copyBatch batch >> loop
            [] -> do
              stop <- IO.atomically $ do
                stop <- readTVar stopVar
                pending <- readTVar copyPending
                check (stop || pending)
                pure stop
              unless stop loop
  worker <- async $ do
    outcome <- try (loop `finally` closeConnection)
    atomically $ putTMVar stoppedVar (outcome :: Either SomeException ())
    -- A copier that stopped is a volatile partition that stops being
    -- evictable, so the link takes the node down rather than let it grow.
    either throwIO pure outcome
  labelThread (asyncThreadId worker) "leiosdb-copier"
  link worker
  pure $ do
    atomically $ writeTVar stopVar True
    either throwIO pure =<< atomically (readTMVar stoppedVar)

-- | How long the copier waits before trying a pinned EB again.
copyRetryMicros :: Int
copyRetryMicros = 1000000

-- | How many pinned EBs the copier takes per pass, and so per 'MarkCopied'.
copyBatchSize :: Int
copyBatchSize = 32

-- | Close the connection if the action throws, then rethrow. For whatever
-- is acquired on a fresh connection -- an attach, a schema, a statement --
-- after the open succeeded and before anything owns the close.
orCloseOnError :: DB.Database -> IO a -> IO a
orCloseOnError db act =
  act `catch` \(e :: SomeException) -> do
    _ <- DB.close db
    throwIO e

-- * Garbage collection of the volatile partition

-- | How many EBs one sweep transaction takes, by default.
defaultGcBatchSize :: Int64
defaultGcBatchSize = 4

-- | How many orphaned txs to GC in one sweep.
gcOrphanTxBatchSize :: Int64
gcOrphanTxBatchSize = 1024

-- | Page size of the 'gcReinit' scan.
gcCandidatesPageSize :: Int64
gcCandidatesPageSize = 4096

-- | Implements 'leiosDbGarbageCollect': the MARK phase of GC mark-and-sweep,
-- as a 'GcMark' job on the writer (see 'gcMark').
sqlGarbageCollect ::
  Tracer IO TraceLeiosDb ->
  CallCtx IO ->
  WriteQueue ->
  SlotNo ->
  IO ()
sqlGarbageCollect tracer rootCtx writeQueue gcSlot =
  gcSpan rootCtx "sqlGarbageCollect" (unSlotNo gcSlot) $ \_gcCtx ->
    await =<< submitJob writeQueue (GcMark gcSlot)
 where
  gcSpan ::
    (Aeson.ToJSON arg, Aeson.ToJSON res) =>
    CallCtx IO -> CallName -> arg -> (CallCtx IO -> IO res) -> IO res
  gcSpan = callTraceSameThread (traceWith tracer . TraceLeiosDbCall . SomeJsonCallTrace)

-- | The MARK phase of GC mark-and-sweep:
--   - mark for GC (@status = 3@) every EB hash all of whose announcements are older
--     than the given slot and not pinned (@status = 1@);
--   - stage its txs as GC candidates;
--   - ask for a sweep.
--
-- Runs on the writer. It does not do much work, but rather primes the state
-- for the writer's own sweep steps (see 'startWriter').
gcMark ::
  HasCallStack =>
  StrictTVar IO Bool ->
  DB.Database ->
  GcStmts ->
  SlotNo ->
  IO ()
gcMark sweepDoorbell db gcStmts gcSlot = do
  let GcStmts{gsHasWork, gsAddGcCandidatesTxs, gsMarkEbForGC} = gcStmts
  -- check if GC has any work to do
  hasWork <-
    useStmt gsHasWork $ do
      dbBindInt64 gsHasWork 1 slot
      (/= 0) <$> readSingleInt64 gsHasWork
  when hasWork $ do
    (nTxsStagedAsGCCandidates, nEbsMarked) <-
      dbWithWriteTransactionRaw db $ do
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
      atomically $
        writeTVar sweepDoorbell True
 where
  slot = fromIntegral (unSlotNo gcSlot)

-- | The GC tick's prepared statements, prepared once on the writer's
-- volatile connection.
data GcStmts = GcStmts
  { gsHasWork :: !DB.Statement
  -- ^ 'sql_gc_has_work'
  , gsAddGcCandidatesTxs :: !DB.Statement
  -- ^ 'sql_gc_stage_marked'
  , gsMarkEbForGC :: !DB.Statement
  -- ^ 'sql_gc_mark'
  }

prepareGcStmts :: HasCallStack => DB.Database -> IO GcStmts
prepareGcStmts db = do
  gsHasWork <- dbPrepare db (fromString sql_gc_has_work)
  gsAddGcCandidatesTxs <- dbPrepare db (fromString sql_gc_stage_marked)
  gsMarkEbForGC <- dbPrepare db (fromString sql_gc_mark)
  pure GcStmts{..}

finalizeGcStmts :: GcStmts -> IO ()
finalizeGcStmts GcStmts{..} = do
  dbFinalize gsHasWork
  dbFinalize gsAddGcCandidatesTxs
  dbFinalize gsMarkEbForGC

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

-- | The sweep statements, prepared on the writer's volatile connection;
-- lifecycle mirrors 'CopierConn'.
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

-- | Prepare the sweep statements on the writer's volatile connection.
prepareSweeperConn :: HasCallStack => DB.Database -> IO SweeperConn
prepareSweeperConn swDb = do
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

-- | Statements only; 'swDb' is the writer's volatile connection.
finalizeSweeperConn :: SweeperConn -> IO ()
finalizeSweeperConn SweeperConn{..} = do
  dbFinalize swPickMarked
  dbFinalize swEvictEbTxs
  dbFinalize swEvictMissingTxs
  dbFinalize swEvictEbs
  dbFinalize swAnyMarked
  dbFinalize swPickOrphans
  dbFinalize swOrphanTxs
  dbFinalize swPopOrphans
  dbFinalize swHasUnstagedGcCandidates
  dbFinalize swUnstagedGcCandidatesPage
  dbFinalize swInsertGcCandidates

-- | One 'SweepEbBatch' transaction: evict up to the given number of GC-marked
-- EBs. Runs on the writer.
sweepEbBatch :: SweeperConn -> Int64 -> IO Int
sweepEbBatch conn batchSize = do
  let SweeperConn{swDb, swPickMarked, swEvictEbTxs, swEvictMissingTxs, swEvictEbs} = conn
  dbWithWriteTransactionRaw swDb $ do
    -- check if any EBs are ready to be evicted
    evictableEbs <- useStmt swPickMarked $ do
      -- a negative LIMIT means no limit in SQLite
      dbBindInt64 swPickMarked 1 (if batchSize <= 0 then -1 else batchSize)
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

-- | One 'SweepOrphanBatch' transaction: evict up to the given number of
-- orphaned txs, or 'Nothing' if there was nothing to do. Runs on the writer.
sweepOrphanBatch :: SweeperConn -> Int64 -> IO (Maybe Int)
sweepOrphanBatch conn batchSize = do
  let SweeperConn{swDb, swAnyMarked, swPickOrphans, swOrphanTxs, swPopOrphans} = conn
  dbWithWriteTransactionRaw swDb $ do
    -- don't run the sweep if any GC-marked EBs remain
    blocked <- useStmt swAnyMarked $ (/= 0) <$> readSingleInt64 swAnyMarked
    if blocked
      then pure Nothing
      else do
        -- look for txs to GC
        orphanedTxs <- useStmt swPickOrphans $ do
          dbBindInt64 swPickOrphans 1 batchSize
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

-- | Stage every unstaged GC candidate, one page per transaction.
--
-- The sweeper only ever reads 'gcTxCandidates', which the mark phase fills
-- ('sql_gc_stage_marked'). A tx orphaned by anything else -- a
-- 'truncateLeiosDbAfterSlot' that dropped its EB's rows, a database written
-- before the table existed -- is referenced by nothing and staged nowhere,
-- and would never be collected.
--
-- Only such out-of-band edits can leave that behind, and only before the
-- writer started, so this runs once per process, on the writer.
gcReinit :: SweeperConn -> IO ()
gcReinit conn = do
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
          dbWithWriteTransactionRaw swDb $
            execJson swInsertGcCandidates (jsonHexArray page)
          when (length page == fromIntegral gcCandidatesPageSize) $
            pageLoop (last page)
  when anyUnstaged $ pageLoop BS.empty

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
  , connStats :: !(StrictTVar IO LeiosDbStats)
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
  action `finally` (void $ DB.reset stmt)

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
      throwIO
        LeiosDbException
          { errorMessage = "failed to close the connection: " <> show err
          , callStack = GHC.Stack.prettyCallStack GHC.Stack.callStack
          }
    Right () -> pure ()

-- * The single writer

-- | One queued write, carrying the variable its result lands in.
--
-- The ingest jobs come from 'LeiosDbWriter', the rest from the handle's
-- promote\/GC entry points. One worker running them all is what makes the
-- single-writer property total: no two in-process connections ever contend
-- for a write lock on the volatile partition. Sweeping is not a job at all
-- -- the worker does it between jobs; see 'startWriter'.
data WriteJob
  = WriteEbPoint !LeiosPoint !BytesSize !(WriteResult ())
  | WriteEbBody !LeiosPoint !LeiosEb !(WriteResult CompletedEbs)
  | WriteTxs ![(TxHash, ByteString)] !(WriteResult CompletedEbs)
  | -- | Does nothing; awaiting it after the queue's FIFO order means every
    -- write submitted before it has landed.
    Flush !(WriteResult ())
  | -- | Pin EBs for promotion; see 'sqlPromoteToImmutable'.
    PinEb ![EbHash] !(WriteResult ())
  | -- | The volatile half of a copy: the EB is in the immutable partition
    -- now, so its volatile rows are evictable. Submitted by 'startCopier'.
    MarkCopied ![EbHash] !(WriteResult ())
  | -- | The GC MARK phase; see 'gcMark'.
    GcMark !SlotNo !(WriteResult ())
  | -- | Stop the worker: awaiting it after the queue's FIFO order means every
    -- write submitted before it has landed and the connections are closed.
    Shutdown !(WriteResult ())

type WriteResult a = StrictTMVar IO (Either SomeException a)

-- | How far a sweep pass has got. The writer advances it one batch per turn
-- rather than running a pass to completion, so ingest waits for a batch at
-- most.
data SweepState
  = SweepIdle
  | -- | Evicting GC-marked EBs; carries how many so far.
    SweepEbs !Int
  | -- | Evicting the txs they orphaned; carries the EB and tx counts.
    SweepOrphans !Int !Int
  deriving Eq

-- | The writer's submission side: the job queue, and -- once the worker has
-- stopped -- the exception every submission throws instead of queueing.
data WriteQueue = WriteQueue
  { wqJobs :: !(StrictTBQueue IO WriteJob)
  , wqSealed :: !(StrictTVar IO (Maybe SomeException))
  , wqTracer :: !(Tracer IO TraceLeiosDb)
  }

-- | Hand a job to the writer; the 'Promise' waits for its result. Blocks
-- only while the queue is full, which is the backpressure -- and which is
-- traced, since it is the one thing about the writer that no other trace
-- reports: every producer is now waiting on it.
--
-- Checking the seal and queueing are one transaction, so after the worker
-- seals the queue no job can slip in unserved: submission throws the
-- worker's parting exception instead -- also waking any submitter that was
-- blocked on a full queue.
submitJob :: HasCallStack => WriteQueue -> (WriteResult a -> WriteJob) -> IO (Promise IO a)
submitJob WriteQueue{wqJobs, wqSealed, wqTracer} mkJob = do
  resultVar <- newEmptyTMVarIO
  let job = mkJob resultVar
      -- Failures surface far from their submitter -- on the worker, or on
      -- whichever thread awaits -- so name the write and its submission site.
      wrap cause =
        LeiosDbWriteException
          { writeJob = describeJob job
          , submittedFrom = GHC.Stack.prettyCallStack GHC.Stack.callStack
          , writeFailure = cause
          }
      -- Take a slot if there is one; 'Left' once the queue is sealed.
      offer =
        readTVar wqSealed >>= \case
          Just cause -> pure (Left cause)
          Nothing ->
            isFullTBQueue wqJobs >>= \case
              True -> pure (Right False)
              False -> Right True <$ writeTBQueue wqJobs job
      -- And wait for one otherwise.
      park =
        readTVar wqSealed >>= \case
          Just cause -> pure (throwIO (wrap cause))
          Nothing -> writeTBQueue wqJobs job >> pure (pure ())
  atomically offer >>= \case
    Left cause -> throwIO (wrap cause)
    Right True -> pure ()
    Right False -> do
      traceWith wqTracer $ TraceLeiosDbWriterQueueFull (describeJob job)
      -- Otherwise silent: no exception reaches the write path, and there is
      -- no continuation yet to carry one.
      join (atomically park)
        `onException` traceWith wqTracer (TraceLeiosDbWriteAbandoned (describeJob job))
  pure $ Promise (either (throwIO . wrap) pure =<< atomically (readTMVar resultVar))

-- | Name a job for 'LeiosDbWriteException': what it is and what it is about,
-- never its payload.
describeJob :: WriteJob -> String
describeJob = \case
  WriteEbPoint point _ _ -> "WriteEbPoint " <> show point
  WriteEbBody point eb _ -> "WriteEbBody " <> show point <> " (" <> show (length (leiosEbTxs eb)) <> " txs)"
  WriteTxs txs _ -> "WriteTxs (" <> show (length txs) <> " txs)"
  Flush _ -> "Flush"
  PinEb ebHashes _ -> "PinEb (" <> show (length ebHashes) <> " ebs)"
  MarkCopied ebHashes _ -> "MarkCopied (" <> show (length ebHashes) <> " ebs)"
  GcMark slot _ -> "GcMark " <> show slot
  Shutdown _ -> "Shutdown"

-- | Publish the worker's parting exception as a queued job's result.
failJob :: SomeException -> WriteJob -> IO ()
failJob cause = \case
  WriteEbPoint _ _ rv -> put rv
  WriteEbBody _ _ rv -> put rv
  WriteTxs _ rv -> put rv
  Flush rv -> put rv
  PinEb _ rv -> put rv
  MarkCopied _ rv -> put rv
  GcMark _ rv -> put rv
  Shutdown rv -> put rv
 where
  put :: WriteResult a -> IO ()
  put rv = atomically $ putTMVar rv (Left cause)

-- | How many queued jobs the writer serves back-to-back before it takes a
-- turn of maintenance anyway.
--
-- Maintenance runs when the queue drains, which is the common case; this is
-- the floor under a queue that never does. One queue-full: under saturation
-- maintenance still gets a turn as often as the producers can refill, and
-- the resulting share (one turn in @'writerQueueDepth' + 1@) is far above
-- what copying and eviction ask for -- they are per promoted or expired EB,
-- not per write. 'TraceLeiosDbStats' is where a volatile partition that
-- still grows would show up.
maxJobsBetweenMaintenance :: Int
maxJobsBetweenMaintenance = fromIntegral writerQueueDepth

-- | Depth of the write queue. One slot per producer that can be mid-write --
-- each upstream peer's fetch client, the forge, and the maintenance
-- schedulers (copier, sweeper, the ChainDB's GC and promote calls) -- and a
-- little slack. Deliberately shallow: depth beyond that adds no throughput
-- (there is one worker) and only delays the commit-time notifications that
-- gate vote scheduling and chain selection. A producer that outruns the disk
-- blocks on submission, which is the backpressure.
writerQueueDepth :: Natural
writerQueueDepth = numUpstreamPeers + forge + maintenance + slack
 where
  numUpstreamPeers = 20
  forge = 1
  maintenance = 4
  slack = 2

-- | Open the write connections and start the worker draining the write queue.
--
-- The worker publishes each job's outcome into its 'WriteResult'. For ingest
-- jobs it then rethrows a failure: publishing first lets an awaiting producer
-- see the exception rather than block on a promise the dying worker would
-- never fill, and a failed ingest write is not survivable (no caller catches
-- 'LeiosDbException') -- producers still submitting eventually block on the
-- full queue and go down as blocked-indefinitely. A failed maintenance job is
-- the submitting scheduler's problem instead (traced, paced, retried); the
-- worker survives it.
startWriter ::
  Tracer IO TraceLeiosDb ->
  StrictTVar IO LeiosDbStats ->
  StrictTChan IO LeiosEbNotification ->
  StrictTVar IO Bool ->
  Int64 ->
  FilePath ->
  FilePath ->
  IO WriteQueue
startWriter tracer statsVar notificationChan sweepDoorbell gcBatchSize volPath immPath = do
  volDb <- openVolRawConnection volPath
  immDb <- orCloseOnError volDb $ openRawConnection immPath
  -- A throw anywhere below closes both connections (best effort -- an
  -- already-prepared statement can hold a close off) instead of leaking them.
  (conn, sweeperConn, gcStmts, pinStmt, markCopiedStmt) <-
    ( do
        conn <- mkConn tracer statsVar volDb immDb
        sweeperConn <- prepareSweeperConn volDb
        gcStmts <- prepareGcStmts volDb
        pinStmt <- dbPrepare volDb (fromString sql_pin_eb)
        markCopiedStmt <- dbPrepare volDb (fromString sql_mark_as_copied)
        pure (conn, sweeperConn, gcStmts, pinStmt, markCopiedStmt)
    )
      `onException` (void (DB.close immDb) >> void (DB.close volDb))
  queue <- newTBQueueIO writerQueueDepth
  sealedVar <- newTVarIO Nothing
  sweepStateVar <- newTVarIO SweepIdle
  gcReinitDoneVar <- newTVarIO False
  jobsServedVar <- newTVarIO (0 :: Int)
  let notify = atomically . writeTChan notificationChan

      -- Statements before connections; an open statement holds the close off.
      closeConnections = do
        finalizeSweeperConn sweeperConn
        finalizeGcStmts gcStmts
        dbFinalize pinStmt
        dbFinalize markCopiedStmt
        closeConn conn

      runJob :: WriteJob -> IO Bool
      runJob = \case
        Shutdown resultVar -> do
          -- No rethrow: a failed close must not close a second time.
          result <- try closeConnections
          atomically $ putTMVar resultVar result
          pure True
        WriteEbPoint point size resultVar ->
          publish resultVar (sqlInsertEbPoint conn point size) >> pure False
        WriteEbBody point eb resultVar ->
          publish resultVar (sqlInsertEbBody tracer conn notify point eb) >> pure False
        WriteTxs txs resultVar ->
          publish resultVar (sqlInsertTxs tracer conn notify txs) >> pure False
        Flush resultVar ->
          publish resultVar (pure ()) >> pure False
        PinEb ebHashes resultVar -> do
          -- One transaction for the batch: the submitter awaits this while
          -- holding the ImmutableDB write lock, so a round-trip per EB throttles
          -- block immutalisation to this queue's drain rate.
          publishMaintenance volDb immDb resultVar $
            dbWithWriteTransactionRaw volDb $
              forM_ ebHashes $ \ebHash ->
                useStmt pinStmt $ do
                  dbBindBlob pinStmt 1 ebHash.ebHashBytes
                  dbStep1Safe pinStmt
          pure False
        MarkCopied ebHashes resultVar -> do
          -- One transaction for the batch: the mark is what makes a copied EB
          -- evictable, and under sync it otherwise costs a queue round-trip
          -- per EB behind a saturated ingest FIFO.
          publishMaintenance volDb immDb resultVar $
            dbWithWriteTransactionRaw volDb $
              forM_ ebHashes $ \ebHash ->
                useStmt markCopiedStmt $ do
                  dbBindBlob markCopiedStmt 1 ebHash.ebHashBytes
                  dbStep1Safe markCopiedStmt
          pure False
        GcMark slot resultVar -> do
          publishMaintenance volDb immDb resultVar $
            gcMark sweepDoorbell volDb gcStmts slot
          pure False

      -- Queued jobs first: writes arrive in bursts, and the quiet in
      -- between is what maintenance is for. But a burst can go on for as
      -- long as it likes, so 'maxJobsBetweenMaintenance' of them is the
      -- most that may pass before maintenance gets its turn regardless.
      serve = do
        mJob <- atomically $ do
          served <- readTVar jobsServedVar
          if served >= maxJobsBetweenMaintenance
            then pure Nothing
            else
              tryReadTBQueue queue >>= \case
                Nothing -> pure Nothing
                Just job -> Just job <$ writeTVar jobsServedVar (served + 1)
        case mJob of
          Just job -> do
            stop <- runJob job
            traceWith tracer $ TraceLeiosDbWriteJobDone (describeJob job)
            unless stop serve
          Nothing -> do
            quiet <- stepMaintenance
            atomically $ writeTVar jobsServedVar 0
            when quiet blockUntilWork
            serve

      -- Nothing queued and no sweep outstanding: wait for either.
      blockUntilWork = IO.atomically $ do
        noJobs <- isEmptyTBQueue queue
        rung <- readTVar sweepDoorbell
        check (not noJobs || rung)

      -- Advance a sweep by one batch; 'True' when there was nothing to do.
      -- A failure here is the maintenance's problem, not the writer's:
      -- trace it, roll back anything left open, and let the next GC tick
      -- bring the work back.
      stepMaintenance =
        step `catch` \(e :: LeiosDbException) -> do
          _ <- DB.exec volDb "ROLLBACK"
          _ <- DB.exec immDb "ROLLBACK"
          traceWith tracer $ TraceLeiosDbGCError (displayException e)
          atomically $ writeTVar sweepStateVar SweepIdle
          pure True
       where
        step =
          readTVarIO sweepStateVar >>= \case
            SweepIdle -> do
              asked <- atomically $ do
                rung <- readTVar sweepDoorbell
                when rung $ do
                  writeTVar sweepDoorbell False
                  writeTVar sweepStateVar (SweepEbs 0)
                pure rung
              if not asked
                then pure True
                else do
                  -- Stages the GC tx candidates a restart left behind.
                  done <- readTVarIO gcReinitDoneVar
                  unless done $ do
                    gcReinit sweeperConn
                    atomically $ writeTVar gcReinitDoneVar True
                  pure False
            SweepEbs nEbs -> do
              evicted <- sweepEbBatch sweeperConn gcBatchSize
              if evicted == 0
                then atomically $ writeTVar sweepStateVar (SweepOrphans nEbs 0)
                else do
                  bumpVolatileStatsVar statsVar (negate evicted)
                  atomically $ writeTVar sweepStateVar (SweepEbs (nEbs + evicted))
              pure False
            SweepOrphans nEbs nTxs ->
              sweepOrphanBatch sweeperConn gcOrphanTxBatchSize >>= \case
                Just evicted -> do
                  atomically $ writeTVar sweepStateVar (SweepOrphans nEbs (nTxs + evicted))
                  pure False
                Nothing -> do
                  when (nEbs > 0 || nTxs > 0) $ do
                    -- Flush the WAL only after real work.
                    dbExec volDb "PRAGMA wal_checkpoint(PASSIVE);"
                    traceWith tracer $ TraceLeiosDbEvicted nEbs
                  atomically $ writeTVar sweepStateVar SweepIdle
                  pure False

  worker <- async $ do
    outcome <- try serve
    let orphaned e = isJust (fromException e :: Maybe BlockedIndefinitelyOnSTM)
    cause <- case outcome of
      -- A served 'Shutdown' has already closed the connections.
      Right () -> pure closedException
      Left e -> do
        -- Close on the way down; on orphanhood (the handle was dropped
        -- without teardown, so nobody can submit again) this is the only
        -- close there will be.
        void (try closeConnections :: IO (Either SomeException ()))
        pure $ if orphaned e then closedException else e
    -- Seal, then fail what was already queued: nothing can be queued after
    -- the seal ('submitJob'), so afterwards the queue stays empty forever.
    atomically $ writeTVar sealedVar (Just cause)
    let drain =
          atomically (tryReadTBQueue queue) >>= \case
            Nothing -> pure ()
            Just job -> failJob cause job >> drain
    drain
    -- Then out, so the link takes the node down where the write failed
    -- rather than at whatever submits next. Orphanhood is not a failure.
    case outcome of
      Left e | not (orphaned e) -> throwIO e
      _ -> pure ()
  labelThread (asyncThreadId worker) "leiosdb-writer"
  link worker
  pure WriteQueue{wqJobs = queue, wqSealed = sealedVar, wqTracer = tracer}
 where
  closedException =
    toException
      LeiosDbException
        { errorMessage = "the LeiosDB writer is closed"
        , callStack = GHC.Stack.prettyCallStack GHC.Stack.callStack
        }
  -- Only a 'LeiosDbException' is a failed write; anything else -- a
  -- cancellation above all -- belongs to this thread, not to the job.
  publish :: WriteResult a -> IO a -> IO ()
  publish resultVar action =
    try action >>= \case
      Right x -> atomically $ putTMVar resultVar (Right x)
      Left (e :: LeiosDbException) -> do
        atomically $ putTMVar resultVar (Left (toException e))
        throwIO e

  -- The transaction brackets have already rolled back on failure; the extra
  -- best-effort ROLLBACKs cover only the case where that rollback itself
  -- failed and would otherwise leave a transaction open under every later job.
  publishMaintenance :: DB.Database -> DB.Database -> WriteResult a -> IO a -> IO ()
  publishMaintenance volDb immDb resultVar action =
    try action >>= \case
      Right x -> atomically $ putTMVar resultVar (Right x)
      Left (e :: LeiosDbException) -> do
        _ <- DB.exec volDb "ROLLBACK"
        _ <- DB.exec immDb "ROLLBACK"
        atomically $ putTMVar resultVar (Left (toException e))

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
    throwLeiosDbException "writeEbBody: empty EB body (programmer error)"
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
  ebBytesSize = encodeLeiosEbSize eb
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
      throwLeiosDbException "readReturningInt64: expected one row from RETURNING, got Done"
    DB.Row -> do
      n <- DB.columnInt64 stmt 0
      dbStep stmt >>= \case
        DB.Done -> pure n
        DB.Row -> throwLeiosDbException "readReturningInt64: expected exactly one row from RETURNING"

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

-- | Delete the EBs announced after the given slot.
--
-- For internal tooling.
--
-- Note: this function works for both the volatile or the immutable appreciation,
--       as they share the same schema. It is the responsibility of the caller to
--       pass the right LeiosDB file.
truncateLeiosDbAfterSlot :: HasCallStack => FilePath -> SlotNo -> IO ()
truncateLeiosDbAfterSlot dbPath (SlotNo slot) =
  withExistingLeiosDbFile dbPath $ \db ->
    -- One transaction, so a crash cannot leave an EB that is still announced
    -- but has no body.
    dbWithTransactionAs "BEGIN IMMEDIATE" db $
      dbExec db (fromString deletes)
 where
  deletes =
    unlines
      [ "DELETE FROM ebTxs WHERE ebHashBytes IN (" <> droppedHashes <> ");"
      , "DELETE FROM ebsMissingTxs WHERE ebHashBytes IN (" <> droppedHashes <> ");"
      , "DELETE FROM ebs WHERE ebSlot > " <> show slot <> ";"
      ]

  -- The EBs whose bodies the truncation drops.
  --
  -- 'ebs' holds one row per announcement, so the same EB hash can appear at
  -- several slots. 'ebTxs' and 'ebsMissingTxs' hold one copy per hash and carry
  -- no slot. So an EB announced at slot 5 and again at slot 15 keeps its body
  -- when the cut is at slot 10. That is what the EXCEPT does: take the hashes
  -- announced after the cut, then remove the ones also announced at or before
  -- it.
  droppedHashes =
    "SELECT ebHashBytes FROM ebs WHERE ebSlot > "
      <> show slot
      <> " EXCEPT SELECT ebHashBytes FROM ebs WHERE ebSlot <= "
      <> show slot

-- | Delete the transactions that no EB references.
--
-- For internal tooling.
deleteDanglingTxs :: HasCallStack => FilePath -> IO ()
deleteDanglingTxs dbPath =
  withExistingLeiosDbFile dbPath $ \db ->
    dbExec db . fromString $
      "DELETE FROM txs WHERE txHashBytes NOT IN (SELECT txHashBytes FROM ebTxs)"

-- | Shrink a LeiosDb file to the space its rows need.
--
-- A delete frees pages inside the file without returning them to the
-- filesystem. This rewrites the file, so it needs free space of about the size
-- of the file. For internal tooling.
vacuumLeiosDb :: HasCallStack => FilePath -> IO ()
vacuumLeiosDb dbPath =
  withExistingLeiosDbFile dbPath $ \db ->
    dbExec db (fromString "VACUUM")

-- | Open the LeiosDb file at the given path, which must already exist.
--
-- Unrelated to 'withReader', which brackets a 'LeiosDbReader' that a
-- 'LeiosDbHandle' opens.
--
-- No 'SQLOpenCreate', unlike 'openRawConnection': a wrong path must fail
-- rather than gain an empty database. No 'busy_timeout' either, so a write that
-- meets the node's own write lock gives up after the retries in 'withDie'
-- rather than block.
withExistingLeiosDbFile :: FilePath -> (DB.Database -> IO a) -> IO a
withExistingLeiosDbFile dbPath =
  bracket
    (open2 (fromString dbPath) [SQLOpenReadWrite] SQLVFSDefault)
    (void . DB.close)

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

-- | Schema of both partitions (@leios.vol.db@ and @leios.imm.db@): identical
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
    , -- This index speeds up tx -> EB lookups, which is necessary for GCing orphaned transactions
      -- after their EB was GCed.
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
    , -- Pinned EBs awaiting the copy into the immutable partition; see
      -- 'sql_next_pinned_eb'.
      "CREATE INDEX IF NOT EXISTS idx_ebs_pinned ON ebs(ebSlot) WHERE status = 1;"
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

-- | Mark a pinned EB as copied (evictable). A volatile write, so it goes
-- through the writer as 'MarkCopied' -- only ever strictly after the
-- immutable partition committed the EB's closure.
sql_mark_as_copied :: String
sql_mark_as_copied =
  "UPDATE ebs SET status = 2 WHERE ebHashBytes = ? AND status = 1"

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
--
-- TODO(geo2a): should not need to copy missingTxCount and status to immutable,
-- this is only relevant for the volatile.
-- @OR IGNORE@: the mark that retires the pin is a separate volatile write, so
-- a crash in between leaves the EB copied and still pinned, and the copier
-- repeats the copy on the next start.
sql_copy_insert_eb :: String
sql_copy_insert_eb =
  "INSERT OR IGNORE INTO ebs (ebSlot, ebHashBytes, ebBytesSize, missingTxCount, status)\n\
  \SELECT ebSlot, ebHashBytes, ebBytesSize, -1, 2 FROM vol.ebs\n\
  \WHERE ebHashBytes = ?1\n\
  \ORDER BY ebSlot DESC LIMIT 1\n\
  \"

-- | Copy the EB's body rows. @OR IGNORE@ for the same reason as
-- 'sql_copy_insert_eb'.
sql_copy_insert_ebTxs :: String
sql_copy_insert_ebTxs =
  "INSERT OR IGNORE INTO ebTxs (ebHashBytes, txOffset, txHashBytes, txBytesSize)\n\
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

-- | Pinned EBs the copier has not marked as copied yet, for self-heal
-- re-enqueueing.
-- | The next EB waiting to be copied into the immutable partition: pinned
-- ('sql_pin_eb') but not yet marked copied ('sql_mark_as_copied'). Oldest
-- first, and covered by @idx_ebs_pinned@.
--
-- The pin is the copy work list. It is in the database rather than in
-- memory, so a copy interrupted by a crash is simply still pending on the
-- next start.
sql_next_pinned_eb :: String
sql_next_pinned_eb =
  "SELECT ebHashBytes FROM vol.ebs WHERE status = 1 ORDER BY ebSlot LIMIT ?1"

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
  \WHERE "
    <> sql_gc_markable

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
-- 'gcReinit': @?1@ = cursor (exclusive), @?2@ = page size.
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
dbWithWriteTransaction Conn{conVolDb} = dbWithWriteTransactionRaw conVolDb

-- | 'dbWithWriteTransaction' for the maintenance paths (promotion to immutable, GC), which hold
-- a raw 'DB.Database' rather than a 'Conn'.
dbWithWriteTransactionRaw :: HasCallStack => DB.Database -> IO a -> IO a
dbWithWriteTransactionRaw = dbWithTransactionAs "BEGIN IMMEDIATE"

dbWithTransactionAs :: HasCallStack => String -> DB.Database -> IO a -> IO a
dbWithTransactionAs begin db k =
  do
    fmap fst
    $ generalBracket
      (dbExec db (fromString begin))
      ( \() -> \case
          ExitCaseSuccess _ -> dbExec db (fromString "COMMIT")
          ExitCaseException _ -> dbExec db (fromString "ROLLBACK")
          ExitCaseAbort -> dbExec db (fromString "ROLLBACK")
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
    DB.Done -> throwLeiosDbException "readSingleInt64: expected a row"
    DB.Row -> do
      n <- DB.columnInt64 stmt 0
      dbStepSafe stmt >>= \case
        DB.Done -> pure n
        DB.Row -> throwLeiosDbException "readSingleInt64: expected exactly one row"

-- | Like 'dbStep1' but returns 'True' on success and 'False' on constraint
-- violation (duplicate key). Other errors are thrown as usual.
dbStepInsert :: HasCallStack => DB.Statement -> IO Bool
dbStepInsert stmt =
  DB.stepNoCB stmt >>= \case
    Left DB.ErrorConstraint -> pure False
    Left e -> DB.getStatementDatabase stmt >>= \db -> throwDbException db e
    Right DB.Done -> pure True
    Right DB.Row -> throwLeiosDbException "dbStepInsert: unexpected Row result"

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

-- | Run a database action that may return an error, and throw a
-- 'LeiosDbException' if it does.
--
-- Including 'DB.ErrorBusy': the connections set a 'busy_timeout', so SQLite
-- has already waited that long in C before reporting it. Waiting again on
-- top of that only converts a lock nobody is going to release into an
-- unbounded stall -- and with one writer per partition, a lock nobody is
-- going to release means another process has the file.
withDie :: HasCallStack => DB.Database -> IO (Either DB.Error a) -> IO a
withDie db io =
  io >>= \case
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
