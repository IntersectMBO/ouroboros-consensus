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
  , sql_schema_imm
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
import Control.Exception (SomeException, throwIO)
import Control.Monad (forever, unless, void)
import Control.Monad.Class.MonadThrow (generalBracket)
import qualified Control.Monad.Class.MonadThrow as MonadThrow
import Control.Tracer (Tracer, traceWith)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (isSuffixOf)
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
  startCopier tracer statsVar copyQueue volLeiosDbPath immLeiosDbPath
  gcRootCtx <- rootCallCtx "leiosdb-gc"
  noGCYetDoneRef <- newIORef True -- True until the first GC of this handle
  pure $
    LeiosDbHandle
      { subscribeEbNotifications =
          atomically (dupTChan notificationChan)
      , leiosDbGarbageCollect =
          sqlGarbageCollect tracer gcRootCtx volLeiosDbPath noGCYetDoneRef statsVar copyQueue
      , leiosDbPromoteToImmutable = sqlPromoteToImmutable tracer volLeiosDbPath copyQueue
      -- TODO(geo2a): ^ this should probably need the imm path as well?
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
      , dbFileBytes = 0
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
  HasCallStack => Tracer IO TraceLeiosDb -> IORef LeiosDbStats -> FilePath -> IO ()
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

-- * Maintenance connections

-- | Open a read-write connection to the given file, creating it and running
-- the given schema DDL if it does not exist yet.
openRawConnection :: HasCallStack => FilePath -> String -> IO DB.Database
openRawConnection path schema = do
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
    dbExec db (fromString schema)
  pure db

-- | Open a connection to the volatile partition for a maintenance operation
-- (pin, GC) and close it afterwards. Fresh per call: maintenance is driven by
-- ChainDB background threads, which hold no LeiosDb connection.
withMaintenanceConn :: HasCallStack => FilePath -> (DB.Database -> IO a) -> IO a
withMaintenanceConn volPath =
  MonadThrow.bracket (openRawConnection volPath sql_schema) (void . DB.close)

-- | Prepare a statement, run the action, finalize. For the maintenance paths,
-- which run rarely enough that preparing per call is fine.
withStmt :: HasCallStack => DB.Database -> String -> (DB.Statement -> IO a) -> IO a
withStmt db sql = MonadThrow.bracket (dbPrepare db (fromString sql)) dbFinalize

-- * Copying EBs to the immutable partition

-- | The queue between 'sqlPromoteToImmutable' (producer) and the background
-- copier thread (consumer). Losing an entry is harmless: every GC run
-- re-enqueues the pinned-but-not-yet-copied EBs (self-heal).
type CopyQueue = StrictTBQueue IO EbHash

copyQueueCapacity :: Natural
copyQueueCapacity = 1024

-- | Implements 'leiosDbPromoteToImmutable': durably pin the EB's announcement
-- rows in the volatile partition (@status@ 0 -> 1, which vetoes eviction
-- forever) and hand the hash to the background copier.
--
-- The caller runs under the ChainDB's ImmutableDB write lock, so the
-- synchronous part stays O(1): one indexed UPDATE, no reads, no counting.
sqlPromoteToImmutable ::
  HasCallStack =>
  Tracer IO TraceLeiosDb -> FilePath -> CopyQueue -> LeiosPoint -> IO ()
sqlPromoteToImmutable tracer volPath copyQueue point = do
  withMaintenanceConn volPath $ \db ->
    -- Take the write lock via the retry-forever path: GC eviction batches
    -- hold it for whole seconds at a time, longer than one statement's
    -- 'withDie' retry budget, and a throw here propagates out of a linked
    -- ChainDB background thread and kills the node (devnet, 2026-08-31).
    -- Once BEGIN IMMEDIATE succeeds the UPDATE itself cannot go busy.
    dbWithWriteTransactionRaw tracer db $
      withStmt db sql_pin_eb $ \stmt -> do
        dbBindBlob stmt 1 point.pointEbHash.ebHashBytes
        dbStep1Safe stmt
  -- Enqueue even when no row was pinned (re-certification): the copier's
  -- presence check makes duplicates cheap.
  enqueueCopy tracer copyQueue point.pointEbHash

-- | Non-blocking enqueue: a full queue drops the hash and traces. GC
-- self-heal re-delivers dropped hashes.
enqueueCopy :: Tracer IO TraceLeiosDb -> CopyQueue -> EbHash -> IO ()
enqueueCopy tracer copyQueue ebHash = do
  accepted <- atomically $ do
    full <- isFullTBQueue copyQueue
    unless full $ writeTBQueue copyQueue ebHash
    pure (not full)
  unless accepted $
    traceWith tracer $
      TraceLeiosDbCopyQueueFull (show ebHash)

-- | The copier's two long-lived connections and its prepared statements.
-- Prepared once when the connections open and finalized in one place
-- ('dropCopierConns') strictly before their connections close -- the same
-- discipline as 'VolStmts', for the same use-after-free reasons.
data CopierConns = CopierConns
  { ccVolDb :: !DB.Database
  , ccImmDb :: !DB.Database
  , ccReadEb :: !DB.Statement
  -- ^ 'sql_copy_read_eb', on the volatile connection
  , ccCountBody :: !DB.Statement
  -- ^ 'sql_copy_count_body', on the volatile connection
  , ccReadClosure :: !DB.Statement
  -- ^ 'sql_copy_read_closure', on the volatile connection
  , ccFlipCopied :: !DB.Statement
  -- ^ 'sql_flip_copied', on the volatile connection
  , ccHasEb :: !DB.Statement
  -- ^ 'sql_imm_has_eb', on the immutable connection
  , ccInsertEb :: !DB.Statement
  -- ^ 'sql_imm_insert_eb', on the immutable connection
  , ccInsertEbTx :: !DB.Statement
  -- ^ 'sql_imm_insert_ebTx', on the immutable connection
  }

-- | Fork the copier thread: it drains 'CopyQueue', appending each pinned EB's
-- closure to the immutable partition and then flipping the volatile rows to
-- @status = 2@ (evictable). Writes to the immutable file never touch the
-- volatile file's write lock, so copying cannot obstruct the hot paths.
--
-- Its two long-lived connections open lazily on the first dequeue -- a dequeue
-- implies the ChainDB is already up, so creating files is allowed by then
-- (contrast 'initialStats').
--
-- Errors are traced and the connections dropped, then the thread continues
-- with the next item after a pause; the EB stays pinned, so no data is lost
-- and GC self-heal retries it. The thread dies with the handle: once every
-- producer is gone the dequeue blocks forever and the RTS delivers
-- 'Control.Exception.BlockedIndefinitelyOnSTM', which is not caught.
startCopier ::
  Tracer IO TraceLeiosDb ->
  IORef LeiosDbStats ->
  CopyQueue ->
  FilePath ->
  FilePath ->
  IO ()
startCopier tracer statsVar copyQueue volPath immPath = do
  rootCtx <- rootCallCtx "leiosdb-copier"
  connsRef <- newIORef Nothing
  void $ forkIO $ forever $ do
    ebHash <- atomically $ readTBQueue copyQueue
    copyOne rootCtx connsRef ebHash
      `MonadThrow.catch` \(e :: SomeException) -> do
        traceWith tracer $ TraceLeiosDbCopyError (show ebHash) (show e)
        -- The connections may be poisoned (e.g. an open transaction);
        -- drop them and reopen on the next item.
        dropCopierConns connsRef ebHash
        threadDelay 1000000
 where
  copySpan ::
    (Aeson.ToJSON arg, Aeson.ToJSON res) =>
    CallCtx IO -> CallName -> arg -> (CallCtx IO -> IO res) -> IO res
  copySpan = callTraceSameThread (traceWith tracer . TraceLeiosDbCall . SomeJsonCallTrace)

  getConns connsRef =
    readIORef connsRef >>= \case
      Just conns -> pure conns
      Nothing -> do
        ccVolDb <- openRawConnection volPath sql_schema
        conns <-
          orCloseOnError ccVolDb $ do
            ccImmDb <- openRawConnection immPath sql_schema_imm
            orCloseOnError ccImmDb $ do
              ccReadEb <- dbPrepare ccVolDb (fromString sql_copy_read_eb)
              ccCountBody <- dbPrepare ccVolDb (fromString sql_copy_count_body)
              ccReadClosure <- dbPrepare ccVolDb (fromString sql_copy_read_closure)
              ccFlipCopied <- dbPrepare ccVolDb (fromString sql_flip_copied)
              ccHasEb <- dbPrepare ccImmDb (fromString sql_imm_has_eb)
              ccInsertEb <- dbPrepare ccImmDb (fromString sql_imm_insert_eb)
              ccInsertEbTx <- dbPrepare ccImmDb (fromString sql_imm_insert_ebTx)
              pure CopierConns{..}
        writeIORef connsRef (Just conns)
        pure conns

  -- Never throws (the caller is the copier's error handler): statements are
  -- finalized first, then the connections closed with the result checked --
  -- @sqlite3_close@ refuses (and would leak) if statements were still
  -- outstanding, and silencing that would hide a bug of this function.
  dropCopierConns connsRef ebHash =
    readIORef connsRef >>= \case
      Nothing -> pure ()
      Just CopierConns{..} -> do
        writeIORef connsRef Nothing
        mapM_
          dbFinalize
          [ccReadEb, ccCountBody, ccReadClosure, ccFlipCopied, ccHasEb, ccInsertEb, ccInsertEbTx]
        forM_ [("volatile", ccVolDb), ("immutable", ccImmDb)] $ \(label, db) ->
          DB.close db >>= \case
            Right () -> pure ()
            Left err ->
              traceWith tracer $
                TraceLeiosDbCopyError
                  (show ebHash)
                  ("failed to close the copier's " <> label <> " connection: " <> show err)

  copyOne rootCtx connsRef ebHash =
    copySpan rootCtx "copyToImmutable" (show ebHash) $ \_ctx -> do
      conns <- getConns connsRef
      let CopierConns{ccHasEb, ccFlipCopied} = conns
      present <- useStmt ccHasEb $ do
        dbBindBlob ccHasEb 1 ebHash.ebHashBytes
        (/= 0) <$> readSingleInt64 ccHasEb
      copied <-
        if present
          then pure True -- crash-before-flip replay, or duplicate delivery
          else
            readVolatileClosure conns ebHash >>= \case
              Nothing -> do
                -- Never destroy data: leave the EB pinned (visible to
                -- self-heal) and move on.
                traceWith tracer $
                  TraceLeiosDbCopyError
                    (show ebHash)
                    "pinned EB has no complete closure in the volatile partition"
                pure False
              Just (ebSlot, ebBytesSize, closureRows) -> do
                appendToImmutable conns ebHash ebSlot ebBytesSize closureRows
                bumpImmutableStats statsVar 1
                traceWith tracer $
                  TraceLeiosDbCopiedToImmutable
                    { copiedEbs = 1
                    , copiedEbTxs = length closureRows
                    , copiedTxs = length closureRows
                    }
                pure True
      -- Flip strictly AFTER the immutable COMMIT; per-hash order
      -- pin < copy < flip < evict is what makes crashes safe without any
      -- cross-file atomicity.
      when copied $
        useStmt ccFlipCopied $ do
          dbBindBlob ccFlipCopied 1 ebHash.ebHashBytes
          dbStep1Safe ccFlipCopied

  -- One DEFERRED read transaction (read-only, so no BUSY_SNAPSHOT hazard):
  -- the EB's newest announcement and its full tx closure.
  readVolatileClosure ::
    CopierConns -> EbHash -> IO (Maybe (Int64, Int64, [(Int64, ByteString, Int64, ByteString)]))
  readVolatileClosure CopierConns{ccVolDb, ccReadEb, ccCountBody, ccReadClosure} ebHash =
    dbWithTransaction ccVolDb $ do
      ebInfo <- useStmt ccReadEb $ do
        dbBindBlob ccReadEb 1 ebHash.ebHashBytes
        dbStepSafe ccReadEb >>= \case
          DB.Done -> pure Nothing
          DB.Row -> do
            ebSlot <- DB.columnInt64 ccReadEb 0
            ebBytesSize <- DB.columnInt64 ccReadEb 1
            pure (Just (ebSlot, ebBytesSize))
      case ebInfo of
        Nothing -> pure Nothing
        Just (ebSlot, ebBytesSize) -> do
          bodyCount <- useStmt ccCountBody $ do
            dbBindBlob ccCountBody 1 ebHash.ebHashBytes
            readSingleInt64 ccCountBody
          closureRows <- useStmt ccReadClosure $ do
            dbBindBlob ccReadClosure 1 ebHash.ebHashBytes
            let loop acc =
                  dbStepSafe ccReadClosure >>= \case
                    DB.Done -> pure (reverse acc)
                    DB.Row -> do
                      txOffset <- DB.columnInt64 ccReadClosure 0
                      txHashBytes <- DB.columnBlob ccReadClosure 1
                      txBytesSize <- DB.columnInt64 ccReadClosure 2
                      txBytes <- DB.columnBlob ccReadClosure 3
                      loop ((txOffset, txHashBytes, txBytesSize, txBytes) : acc)
            loop []
          -- The closure JOIN silently drops txs we do not hold; only a
          -- closure covering the whole body may be copied. A pinned EB is
          -- complete by the parking invariant, so a mismatch here is a bug
          -- upstream -- refuse to copy it rather than truncate it.
          pure $
            if null closureRows || bodyCount /= fromIntegral (length closureRows)
              then Nothing
              else Just (ebSlot, ebBytesSize, closureRows)

  -- One IMMEDIATE write transaction on the immutable file: the EB row and its
  -- body rows land atomically, so presence of the 'ebs' row is proof the
  -- whole closure is there.
  appendToImmutable ::
    CopierConns -> EbHash -> Int64 -> Int64 -> [(Int64, ByteString, Int64, ByteString)] -> IO ()
  appendToImmutable CopierConns{ccImmDb, ccInsertEb, ccInsertEbTx} ebHash ebSlot ebBytesSize closureRows =
    dbWithTransactionAs "BEGIN IMMEDIATE" ccImmDb $ do
      ebSeq <- useStmt ccInsertEb $ do
        dbBindBlob ccInsertEb 1 ebHash.ebHashBytes
        dbBindInt64 ccInsertEb 2 ebSlot
        dbBindInt64 ccInsertEb 3 ebBytesSize
        readSingleInt64 ccInsertEb
      useStmt ccInsertEbTx $
        forM_ closureRows $ \(txOffset, txHashBytes, txBytesSize, txBytes) -> do
          dbBindInt64 ccInsertEbTx 1 (packEbSeqOffset ebSeq txOffset)
          dbBindBlob ccInsertEbTx 2 txHashBytes
          dbBindInt64 ccInsertEbTx 3 txBytesSize
          dbBindBlob ccInsertEbTx 4 txBytes
          dbStep1Safe ccInsertEbTx
          void $ DB.reset ccInsertEbTx

-- | Close the connection if the action throws, then rethrow. For partial
-- acquisition of 'CopierConns': without it, a persistently failing open of
-- the second file would leak one connection per retry.
orCloseOnError :: DB.Database -> IO a -> IO a
orCloseOnError db act =
  act `MonadThrow.catch` \(e :: SomeException) -> do
    _ <- DB.close db
    throwIO e

-- | The immutable partition's @ebTxs@ rowid: @(ebSeq << 32) | txOffset@. One
-- B-tree, strictly right-edge appends, per-EB reads as rowid range scans.
-- @ebSeq < 2^31@ and @txOffset < 2^32@ hold by orders of magnitude.
packEbSeqOffset :: Int64 -> Int64 -> Int64
packEbSeqOffset ebSeq txOffset = (ebSeq `shiftL` 32) .|. txOffset

-- * Garbage collection of the volatile partition

-- | How many stale EB hashes one GC transaction may evict. Each batch runs in
-- its own short IMMEDIATE transaction with the write lock released in
-- between, so this bounds the worst-case stall it can inflict on the hot
-- write paths (a devnet EB is ~2,700 orphan-tx deletes). Steady state is 1-3
-- stale hashes per GC tick, i.e. a single batch.
gcBatchSize :: Int64
gcBatchSize = 4

-- | Pause between eviction batches. Each batch is its own IMMEDIATE
-- transaction, but back-to-back transactions on one connection re-take the
-- write lock before any waiter gets a turn, so a long eviction run
-- effectively holds the lock continuously (observed: 13s on the devnet,
-- starving the pin path to death). The pause is the window in which waiters
-- (pin, hot write paths) actually acquire the lock.
gcBatchPauseMicros :: Int
gcBatchPauseMicros = 100000

-- | Implements 'leiosDbGarbageCollect': evict every EB hash all of whose
-- announcements are older than the given slot and not pinned (@status = 1@
-- vetoes eviction until the copier has flipped the hash to @status = 2@),
-- then flush the WAL.
sqlGarbageCollect ::
  HasCallStack =>
  Tracer IO TraceLeiosDb ->
  CallCtx IO ->
  FilePath ->
  IORef Bool ->
  IORef LeiosDbStats ->
  CopyQueue ->
  SlotNo ->
  IO ()
sqlGarbageCollect tracer rootCtx volPath noGCYetDoneRef statsVar copyQueue gcSlot =
  gcSpan rootCtx "sqlGarbageCollect" (unSlotNo gcSlot) $ \gcCtx ->
    withMaintenanceConn volPath $ \db -> do
      -- The staging table for the orphaned-txs sweep. TEMP: it lives with
      -- this connection, never in leios.vol.db, and its rows never outlive
      -- the batch transaction that stages them. Must exist before
      -- 'withGcStmts' prepares the statements that reference it.
      dbExec db (fromString sql_gc_create_candidates)
      withGcStmts db $ \GcStmts{..} -> do
        -- check if we're doing the first ever GC during this node's run
        -- and flip the flag if so
        firstGc <- atomicModifyIORef' noGCYetDoneRef (\b -> (False, b))
        -- Self-heal: re-enqueue pinned-but-not-yet-copied EBs to the copier.
        -- Recovers a crashed copier, a dropped queue entry and a crash between
        -- copy and flip. These hashes are never evicted this round.
        gcSpan gcCtx "selfHeal" () $ \_ -> do
          stalePins <- useStmt gsStalePins $ do
            dbBindInt64 gsStalePins 1 slot
            let loop acc =
                  dbStepSafe gsStalePins >>= \case
                    DB.Done -> pure (reverse acc)
                    DB.Row -> do
                      ebHash <- MkEbHash <$> DB.columnBlob gsStalePins 0
                      loop (ebHash : acc)
            loop []
          forM_ stalePins $ enqueueCopy tracer copyQueue
        hasWork <- gcSpan gcCtx "noopGuard" () $ \_ ->
          useStmt gsHasWork $ do
            dbBindInt64 gsHasWork 1 slot
            (/= 0) <$> readSingleInt64 gsHasWork
        when (hasWork || firstGc) $ do
          (evictedEbs, evictedEbTxs, evictedTxs) <-
            gcSpan gcCtx "eviction" () $ \evCtx -> do
              nTxsFromFirstGC <-
                if firstGc
                  -- on the first GC (for example, after a node restart), reap
                  -- orphans the candidate scheme cannot see
                  then gcSpan evCtx "orphanTxsFullScan" () $ \_ ->
                    dbWithWriteTransactionRaw tracer db $ do
                      useStmt gsOrphanTxsFullScan $ dbStep1Safe gsOrphanTxsFullScan
                      DB.changes db
                  else pure 0
              let batchLoop !accEbs !accEbTxs !accTxs = do
                    (nEbs, nEbTxs, nTxs) <- gcSpan evCtx "evictionBatch" () $ \_ ->
                      dbWithWriteTransactionRaw tracer db $ do
                        -- Pick the batch INSIDE the transaction: the per-hash
                        -- pin/recent-announcement veto is re-evaluated under
                        -- the write lock, so a hash pinned between batches is
                        -- never evicted.
                        staleHashes <- useStmt gsPickStale $ do
                          dbBindInt64 gsPickStale 1 slot
                          dbBindInt64 gsPickStale 2 gcBatchSize
                          let loop acc =
                                dbStepSafe gsPickStale >>= \case
                                  DB.Done -> pure (reverse acc)
                                  DB.Row -> do
                                    hashBytes <- DB.columnBlob gsPickStale 0
                                    loop (hashBytes : acc)
                          loop []
                        if null staleHashes
                          then pure (0, 0, 0)
                          else do
                            let hashesJson = jsonHexArray staleHashes
                                execJson stmt = useStmt stmt $ do
                                  dbBindUtf8 stmt 1 hashesJson
                                  dbStep1Safe stmt
                            execJson gsStageCandidates
                            execJson gsEvictEbTxs
                            nEbTxs <- DB.changes db
                            execJson gsEvictMissingTxs
                            execJson gsEvictEbs
                            nEbs <- DB.changes db
                            useStmt gsOrphanTxs $ dbStep1Safe gsOrphanTxs
                            nTxs <- DB.changes db
                            useStmt gsClearCandidates $ dbStep1Safe gsClearCandidates
                            pure (nEbs, nEbTxs, nTxs)
                    if nEbs == 0 && nEbTxs == 0 && nTxs == 0
                      then pure (accEbs, accEbTxs, accTxs)
                      else do
                        threadDelay gcBatchPauseMicros
                        batchLoop (accEbs + nEbs) (accEbTxs + nEbTxs) (accTxs + nTxs)
              (nEbs, nEbTxs, nTxs) <- batchLoop 0 0 0
              -- Old announcement rows of hashes that survive via a recent (or
              -- pinned) announcement; without this sweep a re-announced hash
              -- leaks one row per old announcement.
              nLeftover <- gcSpan evCtx "leftoverAnnouncements" () $ \_ ->
                dbWithWriteTransactionRaw tracer db $ do
                  useStmt gsLeftoverEbs $ do
                    dbBindInt64 gsLeftoverEbs 1 slot
                    dbStep1Safe gsLeftoverEbs
                  DB.changes db
              pure (nEbs + nLeftover, nEbTxs, nTxs + nTxsFromFirstGC)
          bumpVolatileStatsRef statsVar (negate evictedEbs)
          traceWith tracer TraceLeiosDbEvicted{evictedEbs, evictedEbTxs, evictedTxs}
        -- flush the WAL to prevent unbounded growth, independently of whether
        -- GC happened
        gcSpan gcCtx "walCheckpoint" () $ \_ ->
          dbExec db "PRAGMA wal_checkpoint(PASSIVE);"
 where
  slot = fromIntegral (unSlotNo gcSlot)

  gcSpan ::
    (Aeson.ToJSON arg, Aeson.ToJSON res) =>
    CallCtx IO -> CallName -> arg -> (CallCtx IO -> IO res) -> IO res
  gcSpan = callTraceSameThread (traceWith tracer . TraceLeiosDbCall . SomeJsonCallTrace)

-- | One GC run's prepared statements, prepared once per maintenance
-- connection and finalized in one place strictly before it closes -- the
-- same discipline as 'VolStmts', for the same use-after-free reasons.
data GcStmts = GcStmts
  { gsStalePins :: !DB.Statement
  , gsHasWork :: !DB.Statement
  , gsPickStale :: !DB.Statement
  , gsStageCandidates :: !DB.Statement
  , gsEvictEbTxs :: !DB.Statement
  , gsEvictMissingTxs :: !DB.Statement
  , gsEvictEbs :: !DB.Statement
  , gsOrphanTxs :: !DB.Statement
  , gsOrphanTxsFullScan :: !DB.Statement
  , gsClearCandidates :: !DB.Statement
  , gsLeftoverEbs :: !DB.Statement
  }

-- | Prepare 'GcStmts', run the action, finalize them (before the caller
-- closes the connection). The @gcTxCandidates@ TEMP table must already exist.
withGcStmts :: HasCallStack => DB.Database -> (GcStmts -> IO a) -> IO a
withGcStmts db =
  MonadThrow.bracket
    ( do
        gsStalePins <- dbPrepare db (fromString sql_gc_stale_pins)
        gsHasWork <- dbPrepare db (fromString sql_gc_has_work)
        gsPickStale <- dbPrepare db (fromString sql_gc_pick_stale_hashes)
        gsStageCandidates <- dbPrepare db (fromString sql_gc_stage_orphan_candidates)
        gsEvictEbTxs <- dbPrepare db (fromString sql_gc_ebTxs)
        gsEvictMissingTxs <- dbPrepare db (fromString sql_gc_missing_txs)
        gsEvictEbs <- dbPrepare db (fromString sql_gc_ebs_by_hash)
        gsOrphanTxs <- dbPrepare db (fromString sql_gc_orphan_txs)
        gsOrphanTxsFullScan <- dbPrepare db (fromString sql_gc_orphan_txs_full_scan)
        gsClearCandidates <- dbPrepare db (fromString sql_gc_clear_candidates)
        gsLeftoverEbs <- dbPrepare db (fromString sql_gc_leftover_ebs)
        pure GcStmts{..}
    )
    ( \GcStmts{..} ->
        mapM_
          dbFinalize
          [ gsStalePins
          , gsHasWork
          , gsPickStale
          , gsStageCandidates
          , gsEvictEbTxs
          , gsEvictMissingTxs
          , gsEvictEbs
          , gsOrphanTxs
          , gsOrphanTxsFullScan
          , gsClearCandidates
          , gsLeftoverEbs
          ]
    )

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
  immStLookupEbBody <- dbPrepare db (fromString sql_imm_lookup_ebBodies)
  immStLookupEbClosure <- dbPrepare db (fromString sql_imm_lookup_eb_closure)
  immStBatchRetrieveTxs <- dbPrepare db (fromString sql_imm_retrieve_txs_json)
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
  volDb <- openRawConnection volPath sql_schema
  stmts <- prepareVolStmts volDb
  immDb <- openRawConnection immPath sql_schema_imm
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

-- | Immutable-partition fallback of 'sqlBatchRetrieveTxs': packed-rowid point
-- lookups. Tx bytes are inline there, so a joined row is never NULL.
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

-- | Schema for the volatile partition (@leios.vol.db@).
sql_schema :: String
sql_schema =
  unlines
    [ "CREATE TABLE ebs ("
    , "  ebSlot INTEGER NOT NULL,"
    , "  ebHashBytes BLOB NOT NULL,"
    , "  ebBytesSize INTEGER NOT NULL,"
    , -- NULL = body not downloaded, >0 = txs missing, 0 = just completed, <0 = notified
      "  missingTxCount INTEGER,"
    , -- 0 = volatile, 1 = certified/pinned awaiting copy (never evicted),
      -- 2 = copied to the immutable partition (evictable)
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
sql_pin_eb :: String
sql_pin_eb =
  "UPDATE ebs SET status = 1 WHERE ebHashBytes = ? AND status = 0"

-- | Flip a pinned EB to copied (evictable). Only run strictly after the
-- immutable partition committed the EB's closure.
sql_flip_copied :: String
sql_flip_copied =
  "UPDATE ebs SET status = 2 WHERE ebHashBytes = ? AND status = 1"

-- | The EB's newest announcement, for the copy.
sql_copy_read_eb :: String
sql_copy_read_eb =
  "SELECT ebSlot, ebBytesSize FROM ebs WHERE ebHashBytes = ?1\n\
  \ORDER BY ebSlot DESC LIMIT 1\n\
  \"

-- | How many body rows the EB has; guards the copy against a truncated
-- closure (see 'sql_copy_read_closure').
sql_copy_count_body :: String
sql_copy_count_body =
  "SELECT COUNT(*) FROM ebTxs WHERE ebHashBytes = ?1"

-- | The EB's full closure in offset order. Plain JOIN: a missing tx silently
-- drops its row, so the caller compares the row count against
-- 'sql_copy_count_body'.
sql_copy_read_closure :: String
sql_copy_read_closure =
  "SELECT e.txOffset, e.txHashBytes, e.txBytesSize, t.txBytes\n\
  \FROM ebTxs e JOIN txs t ON t.txHashBytes = e.txHashBytes\n\
  \WHERE e.ebHashBytes = ?1\n\
  \ORDER BY e.txOffset ASC\n\
  \"

-- ** The immutable partition (@leios.imm.db@)

-- | Schema for the immutable partition: append-only and denormalized. Tx
-- bytes are inline (no dedup: measured cross-EB dedup on the devnet was
-- ~0.3%, and a hash-keyed shared table scatters writes across its B-trees,
-- which is what blew the WAL up in earlier iterations). Both tables are rowid
-- tables keyed by a monotone integer, so every insert appends at the right
-- edge and evicted-and-reused pages cannot fragment the file.
sql_schema_imm :: String
sql_schema_imm =
  unlines
    [ "CREATE TABLE IF NOT EXISTS ebs ("
    , -- rowid alias; monotone because rows are never deleted
      "  ebSeq INTEGER PRIMARY KEY,"
    , "  ebHashBytes BLOB NOT NULL UNIQUE,"
    , -- informational; deliberately NOT indexed
      "  ebSlot INTEGER NOT NULL,"
    , "  ebBytesSize INTEGER NOT NULL"
    , ");"
    , "CREATE TABLE IF NOT EXISTS ebTxs ("
    , -- (ebSeq << 32) | txOffset, computed in Haskell ('packEbSeqOffset'):
      -- one B-tree, per-EB reads are rowid range scans
      "  ebSeqOffset INTEGER PRIMARY KEY,"
    , "  txHashBytes BLOB NOT NULL,"
    , -- before txBytes, so body-only reads decode a record prefix
      "  txBytesSize INTEGER NOT NULL,"
    , "  txBytes BLOB NOT NULL"
    , ");"
    ]

sql_imm_has_eb :: String
sql_imm_has_eb =
  "SELECT EXISTS (SELECT 1 FROM ebs WHERE ebHashBytes = ?1)"

sql_imm_insert_eb :: String
sql_imm_insert_eb =
  "INSERT INTO ebs (ebHashBytes, ebSlot, ebBytesSize) VALUES (?, ?, ?)\n\
  \RETURNING ebSeq\n\
  \"

sql_imm_insert_ebTx :: String
sql_imm_insert_ebTx =
  "INSERT INTO ebTxs (ebSeqOffset, txHashBytes, txBytesSize, txBytes) VALUES (?, ?, ?, ?)"

-- | The rowid range holding the EB's body: an absent hash makes the subquery
-- NULL, the BETWEEN never true, and the result empty -- which is exactly the
-- miss signal.
sql_imm_eb_range :: String
sql_imm_eb_range =
  "BETWEEN ((SELECT ebSeq FROM ebs WHERE ebHashBytes = ?1) << 32)\n\
  \    AND (((SELECT ebSeq FROM ebs WHERE ebHashBytes = ?1) << 32) | 0xFFFFFFFF)"

sql_imm_lookup_ebBodies :: String
sql_imm_lookup_ebBodies =
  "SELECT txHashBytes, txBytesSize FROM ebTxs\n\
  \WHERE ebSeqOffset "
    <> sql_imm_eb_range
    <> "\nORDER BY ebSeqOffset ASC\n"

sql_imm_lookup_eb_closure :: String
sql_imm_lookup_eb_closure =
  "SELECT txHashBytes, txBytes FROM ebTxs\n\
  \WHERE ebSeqOffset "
    <> sql_imm_eb_range
    <> "\nORDER BY ebSeqOffset ASC\n"

-- | Batch retrieve by offset, immutable side: each offset is a rowid point
-- lookup. Column shapes match 'sql_retrieve_from_ebTxs_json' (tx bytes are
-- inline here, so the third column is never NULL).
sql_imm_retrieve_txs_json :: String
sql_imm_retrieve_txs_json =
  "SELECT je.value, e.txHashBytes, e.txBytes\n\
  \FROM json_each(?2) je\n\
  \JOIN ebTxs e ON e.ebSeqOffset =\n\
  \  (((SELECT ebSeq FROM ebs WHERE ebHashBytes = ?1) << 32) | je.value)\n\
  \ORDER BY je.value ASC\n\
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

-- | The staging table for the orphaned-txs sweep. TEMP: per-connection, never
-- part of the volatile file; rows never outlive one batch transaction.
sql_gc_create_candidates :: String
sql_gc_create_candidates =
  "CREATE TEMP TABLE IF NOT EXISTS gcTxCandidates (txHashBytes BLOB NOT NULL PRIMARY KEY)"

-- | Pinned EBs the copier has not flipped yet, for self-heal re-enqueueing.
sql_gc_stale_pins :: String
sql_gc_stale_pins =
  "SELECT DISTINCT ebHashBytes FROM ebs WHERE status = 1 AND ebSlot < ?1"

-- | Whether a GC at slot @?1@ would evict anything.
sql_gc_has_work :: String
sql_gc_has_work =
  "SELECT EXISTS (SELECT 1 FROM ebs WHERE status <> 1 AND ebSlot < ?1)"

-- | Up to @?2@ evictable EB hashes: hashes with no pinned announcement and no
-- announcement at or after the GC slot @?1@. The per-hash veto is what makes
-- eviction safe against pins and re-announcements landing between batches.
sql_gc_pick_stale_hashes :: String
sql_gc_pick_stale_hashes =
  "SELECT DISTINCT cand.ebHashBytes FROM ebs cand\n\
  \WHERE cand.status <> 1 AND cand.ebSlot < ?1\n\
  \  AND NOT EXISTS\n\
  \    (SELECT 1 FROM ebs\n\
  \     WHERE ebs.ebHashBytes = cand.ebHashBytes\n\
  \       AND (ebs.status = 1 OR ebs.ebSlot >= ?1))\n\
  \LIMIT ?2\n\
  \"

-- | Stage the txs of the batch's hashes (JSON hex array @?1@) as orphan
-- candidates.
sql_gc_stage_orphan_candidates :: String
sql_gc_stage_orphan_candidates =
  "INSERT OR IGNORE INTO gcTxCandidates (txHashBytes)\n\
  \  SELECT DISTINCT txHashBytes FROM ebTxs\n\
  \  WHERE ebHashBytes IN (SELECT unhex(je.value) FROM json_each(?1) je)\n\
  \"

-- | Evict the body rows of the batch's hashes.
sql_gc_ebTxs :: String
sql_gc_ebTxs =
  "DELETE FROM ebTxs WHERE ebHashBytes IN (SELECT unhex(je.value) FROM json_each(?1) je)"

-- | Evict the waiting-for-txs index rows of the batch's hashes (stale
-- incomplete EBs may still be indexed there).
sql_gc_missing_txs :: String
sql_gc_missing_txs =
  "DELETE FROM ebsMissingTxs WHERE ebHashBytes IN (SELECT unhex(je.value) FROM json_each(?1) je)"

-- | Evict every announcement row of the batch's hashes. Safe to drop them
-- all: 'sql_gc_pick_stale_hashes' guarantees none is pinned or recent, and
-- the batch transaction holds the write lock from pick to delete.
sql_gc_ebs_by_hash :: String
sql_gc_ebs_by_hash =
  "DELETE FROM ebs WHERE ebHashBytes IN (SELECT unhex(je.value) FROM json_each(?1) je)"

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
-- this node restart).
sql_gc_orphan_txs_full_scan :: String
sql_gc_orphan_txs_full_scan =
  "DELETE FROM txs WHERE NOT EXISTS\n\
  \  (SELECT 1 FROM ebTxs WHERE ebTxs.txHashBytes = txs.txHashBytes)\n\
  \"

-- | Drop all staged candidates.
sql_gc_clear_candidates :: String
sql_gc_clear_candidates = "DELETE FROM gcTxCandidates"

-- | Old announcement rows of hashes that survive eviction via a recent (or
-- pinned) announcement; their body and txs stay with the surviving rows.
sql_gc_leftover_ebs :: String
sql_gc_leftover_ebs =
  "DELETE FROM ebs WHERE status <> 1 AND ebSlot < ?1"

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

-- | 'dbWithWriteTransaction' for the maintenance paths (pin, GC), which hold
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
