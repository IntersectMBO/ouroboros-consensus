{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite
  ( newLeiosDBSQLiteFromEnv
  , newLeiosDBSQLite
  , newLeiosDBSQLiteWithGcBatchSize
  , withLeiosDBSQLite

    -- * Re-exported for internal tooling
  , truncateLeiosDbAfterSlot
  , deleteDanglingTxs
  , vacuumLeiosDb

    -- * SQL strings (re-exported for leios-schedule-gen)
  , sql_schema_vol
  , sql_schema_imm
  , sql_insert_eb
  , sql_insert_ebBody
  , sql_insert_tx
  ) where

import Cardano.Prelude (forM_, when)
import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTChan
  , StrictTVar
  , check
  , dupTChan
  , isEmptyTBQueue
  , newBroadcastTChan
  , newTBQueueIO
  , newTVarIO
  , putTMVar
  , readTVar
  , readTVarIO
  , tryReadTBQueue
  , writeTChan
  , writeTVar
  )
import Control.Exception
  ( throwIO
  , toException
  )
import Control.Monad (unless, void)
import Control.Monad.Class.MonadThrow
  ( bracket
  , catch
  , displayException
  , try
  )
import Control.ResourceRegistry
  ( ResourceRegistry
  , Thread
  , cancelThread
  , forkLinkedThread
  , withRegistry
  )
import Control.Tracer (Tracer, traceWith)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import qualified Database.SQLite3.Direct as DB
import qualified GHC.Conc as IO (atomically)
import qualified GHC.Stack
import Ouroboros.Consensus.Leios.Types (EbHash (..))
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbHandle (..)
  , LeiosDbReader (..)
  , LeiosDbWriter (..)
  , LeiosEbNotification (..)
  , Promise (..)
  )
import Ouroboros.Consensus.Storage.LeiosDB.Exception
  ( LeiosDbException (..)
  , LeiosDbFailure (..)
  )
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Connection
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Insert
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Maintenance
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Primitives
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Queries
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Read
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Schema
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Statements
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Tooling
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.WriteQueue
import Ouroboros.Consensus.Storage.LeiosDB.Trace (LeiosDbStats (..), TraceLeiosDb (..))
import Ouroboros.Consensus.Util.IOLike (atomically)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.FilePath (takeDirectory)

-- * Public API

--- | Create a new Leios database connection from environment variable.
--- This looks up the LEIOS_VOL_DB_PATH and LEIOS_VOL_DB_PATH environment variables
--  and opens the database.
newLeiosDBSQLiteFromEnv :: ResourceRegistry IO -> Tracer IO TraceLeiosDb -> IO (LeiosDbHandle IO)
newLeiosDBSQLiteFromEnv registry tracer = do
  volDbPath <-
    lookupEnv "LEIOS_VOL_DB_PATH" >>= \case
      Nothing -> die "You must define the LEIOS_VOL_DB_PATH variable for this demo."
      Just x -> pure x
  immDbPath <-
    lookupEnv "LEIOS_IMM_DB_PATH" >>= \case
      Nothing -> die "You must define the LEIOS_IMM_DB_PATH variable for this demo."
      Just x -> pure x
  newLeiosDBSQLite registry tracer volDbPath immDbPath

-- | Create a new Leios database using the SQLite implementation.
--
-- Each call to 'openReader' on the returned handle creates new SQLite
-- connection; readers are not thread-safe and should not be shared across
-- threads. All writers submit to the one write connection created here, on
-- its own worker thread.
--
-- The registry owns the background threads (writer, copier and a thread that
-- samples the database's size): 'closeLeiosDbHandle' stops them in order, and
-- closing the registry cancels whatever is still running.
--
-- Creates both files with their schemas before returning, so call it only
-- once their directories may be non-empty (after the ChainDB marker check).
newLeiosDBSQLite ::
  ResourceRegistry IO -> Tracer IO TraceLeiosDb -> FilePath -> FilePath -> IO (LeiosDbHandle IO)
newLeiosDBSQLite registry tracer volLeiosDbPath immLeiosDbPath =
  newLeiosDBSQLiteWithGcBatchSize registry tracer volLeiosDbPath immLeiosDbPath defaultGcBatchSize

-- | 'newLeiosDBSQLite' with an explicit GC sweep batch size: how many EBs
-- the writer evicts per turn, between the jobs it serves.
--
-- Note that orphan transaction batch size is set by the 'gcOrphanTxBatchSize' constant.
newLeiosDBSQLiteWithGcBatchSize ::
  ResourceRegistry IO ->
  Tracer IO TraceLeiosDb ->
  FilePath ->
  FilePath ->
  Int64 ->
  IO (LeiosDbHandle IO)
newLeiosDBSQLiteWithGcBatchSize registry tracer volLeiosDbPath immLeiosDbPath gcBatchSize = do
  -- The database opens before whoever owns these directories creates them.
  mapM_ (createDirectoryIfMissing True . takeDirectory) [volLeiosDbPath, immLeiosDbPath]
  -- create .db files
  initialiseLeiosDbFiles volLeiosDbPath immLeiosDbPath
  notificationChan <- atomically newBroadcastTChan
  -- seed the in-memory stats by counting the EB rows once per handle
  statsVar <- newTVarIO =<< initialStats volLeiosDbPath immLeiosDbPath
  -- start a thread to sample the sizes of the LeiosDB
  samplerThread <- startVolatileStatsSampler registry tracer statsVar volLeiosDbPath
  -- Both start set, so a restart picks up whatever the last run left
  -- pinned or marked; the copier and the writer clear them once they find
  -- nothing.
  copierDoorbell <- newTVarIO True
  sweepDoorbell <- newTVarIO True

  -- the volatile partition writer thread
  writeQueue <-
    startWriter
      registry
      tracer
      statsVar
      notificationChan
      sweepDoorbell
      gcBatchSize
      volLeiosDbPath
      immLeiosDbPath
  -- the immutable partition writer thread
  copierThread <-
    startCopier
      registry
      tracer
      statsVar
      copierDoorbell
      writeQueue
      volLeiosDbPath
      immLeiosDbPath
  pure
    LeiosDbHandle
      { closeLeiosDbHandle = close samplerThread copierThread writeQueue
      , openReader = implOpenReader statsVar
      , openWriter = implOpenWriter writeQueue
      , subscribeEbNotifications = atomically (dupTChan notificationChan)
      , leiosDbGarbageCollect = sqlGarbageCollect writeQueue
      , leiosDbPromoteToImmutable = sqlPromoteToImmutable writeQueue copierDoorbell
      , leiosDbSampleStats = readTVarIO statsVar
      }
 where
  close :: Thread IO () -> Thread IO () -> WriteQueue -> IO ()
  close samplerThread copierThread writeQueue = do
    -- The sampler holds no connection -- it only reads counters -- so
    -- cancelling it is safe at any point.
    cancelThread samplerThread
    -- The copier before the writer: it submits to the writer, so it must be
    -- gone before the writer stops serving. Cancelling it rolls back any
    -- copy in flight; the EB stays pinned, so the next start copies it again.
    cancelThread copierThread
    -- The queue is FIFO, so serving this job flushes everything
    -- submitted before it; awaiting it waits for the connections to
    -- close, and a failed close propagates -- a leaked connection must
    -- be loud.
    --
    -- A sealed queue refuses the job. Nothing is left to close then: the
    -- worker closes the connections on every exit path before it seals,
    -- and whatever killed it already reached the awaiter of the write
    -- that failed.
    try (submitJob writeQueue Shutdown) >>= \case
      Left (_writerGone :: LeiosDbException) -> pure ()
      Right promise -> await promise

  implOpenReader statsVar = do
    volDb <- openRawConnection volLeiosDbPath
    immDb <- orCloseOnError volDb $ openRawConnection immLeiosDbPath
    conn <- mkConn tracer statsVar volDb immDb
    pure
      LeiosDbReader
        { closeReader = closeConn conn
        , testScanEbPoints = sqlScanEbPoints conn
        , scanCompleteEbClosuresNotOlderThanSlot = sqlScanCompleteEbPointsSince conn
        , lookupEbBody = sqlLookupEbBody conn
        , batchRetrieveTxs = sqlBatchRetrieveTxs conn
        , lookupEbClosure = sqlLookupEbClosure conn
        }

  implOpenWriter writeQueue =
    pure
      LeiosDbWriter
        { -- Not a teardown -- the write connection outlives every writer.
          closeWriter = void . await =<< submitJob writeQueue Flush
        , writeEbPoint = \point size -> submitJob writeQueue (WriteEbPoint point size)
        , writeEbBody = \point eb -> submitJob writeQueue (WriteEbBody point eb)
        , writeTxs = \txs -> submitJob writeQueue (WriteTxs txs)
        }

-- | 'newLeiosDBSQLite' bracketed with its 'closeLeiosDbHandle': on release
-- every pending write has landed, the background threads are gone and the
-- connections are closed, so e.g. the database files can be deleted.
withLeiosDBSQLite ::
  Tracer IO TraceLeiosDb -> FilePath -> FilePath -> (LeiosDbHandle IO -> IO a) -> IO a
withLeiosDBSQLite tracer volLeiosDbPath immLeiosDbPath k =
  withRegistry $ \registry ->
    bracket
      (newLeiosDBSQLite registry tracer volLeiosDbPath immLeiosDbPath)
      closeLeiosDbHandle
      k

-- | What the writer's worker owns while it serves: its connections to both
-- partitions and the statements prepared on them.
data WriterConns = WriterConns
  { wcVolDb :: !DB.Database
  , wcImmDb :: !DB.Database
  , wcConn :: !Conn
  , wcSweeperConn :: !SweeperConn
  , wcGcStmts :: !GcStmts
  , wcPinStmt :: !DB.Statement
  -- ^ 'sql_pin_eb'
  , wcMarkCopiedStmt :: !DB.Statement
  -- ^ 'sql_mark_as_copied'
  }

-- | Open the writer's connections, prepare its statements, and run the
-- action with them. Each resource has its own bracket, so whatever was
-- acquired is released on every way out, statements before connections: an
-- open statement holds the close off.
withWriterConns ::
  Tracer IO TraceLeiosDb ->
  StrictTVar IO LeiosDbStats ->
  FilePath ->
  FilePath ->
  (WriterConns -> IO a) ->
  IO a
withWriterConns tracer statsVar volPath immPath k =
  bracket (openRawConnection volPath) closeChecked $ \volDb ->
    bracket (openRawConnection immPath) closeChecked $ \immDb ->
      bracket (mkConn tracer statsVar volDb immDb) finalizeConnStmts $ \conn ->
        bracket (prepareSweeperStmts volDb) finalizeSweeperStmts $ \sweeperStmts ->
          bracket (prepareGcStmts volDb) finalizeGcStmts $ \gcStmts ->
            withStmt volDb sql_pin_eb $ \pinStmt ->
              withStmt volDb sql_mark_as_copied $ \markCopiedStmt ->
                k
                  WriterConns
                    { wcVolDb = volDb
                    , wcImmDb = immDb
                    , wcConn = conn
                    , wcSweeperConn = SweeperConn volDb sweeperStmts
                    , wcGcStmts = gcStmts
                    , wcPinStmt = pinStmt
                    , wcMarkCopiedStmt = markCopiedStmt
                    }

-- | Start the worker draining the write queue. The worker opens the write
-- connections itself, on its own thread ('withWriterConns'), and closes them
-- on every way out.
--
-- The worker publishes each job's outcome into its 'WriteResult'. For insert
-- jobs it then rethrows a failure: publishing first lets an awaiting producer
-- see the exception rather than block on a promise the dying worker would
-- never fill, and a failed insert write is not survivable (no caller catches
-- 'LeiosDbException') -- producers still submitting eventually block on the
-- full queue and go down as blocked-indefinitely. A failed maintenance job is
-- the submitting scheduler's problem instead (traced, paced, retried); the
-- worker survives it.
startWriter ::
  ResourceRegistry IO ->
  Tracer IO TraceLeiosDb ->
  StrictTVar IO LeiosDbStats ->
  StrictTChan IO LeiosEbNotification ->
  StrictTVar IO Bool ->
  Int64 ->
  FilePath ->
  FilePath ->
  IO WriteQueue
startWriter registry tracer statsVar notificationChan sweepDoorbell gcBatchSize volPath immPath = do
  queue <- newTBQueueIO writerQueueDepth
  sealedVar <- newTVarIO Nothing
  -- Set by a served 'Shutdown', whose awaiter gets the outcome of the close.
  shutdownVar <- newIORef Nothing
  sweepStateVar <- newTVarIO SweepIdle
  gcReinitDoneVar <- newTVarIO False
  jobsServedVar <- newTVarIO (0 :: Int)
  let notify = atomically . writeTChan notificationChan

      worker :: WriterConns -> IO ()
      worker
        WriterConns
          { wcVolDb = volDb
          , wcImmDb = immDb
          , wcConn = conn
          , wcSweeperConn = sweeperConn
          , wcGcStmts = gcStmts
          , wcPinStmt = pinStmt
          , wcMarkCopiedStmt = markCopiedStmt
          } = serve
         where
          runJob :: WriteJob -> IO Bool
          runJob = \case
            Shutdown resultVar -> do
              -- Only stop serving: the brackets in 'withWriterConns' close the
              -- connections once 'serve' returns, and the worker hands the
              -- outcome of that close to this job's awaiter.
              writeIORef shutdownVar (Just resultVar)
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
                      dbBindBlob pinStmt 1 (ebHashBytes ebHash)
                      dbStep1Safe pinStmt
              pure False
            MarkCopied ebHashes resultVar -> do
              -- One transaction for the batch: the mark is what makes a copied EB
              -- evictable, and under sync it otherwise costs a queue round-trip
              -- per EB behind a saturated insert FIFO.
              publishMaintenance volDb immDb resultVar $
                dbWithWriteTransactionRaw volDb $
                  forM_ ebHashes $ \ebHash ->
                    useStmt markCopiedStmt $ do
                      dbBindBlob markCopiedStmt 1 (ebHashBytes ebHash)
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

      -- Seal, then fail what was already queued: nothing can be queued after
      -- the seal ('submitJob'), so afterwards the queue stays empty forever.
      sealAndDrain cause = do
        atomically $ writeTVar sealedVar (Just cause)
        let drain =
              atomically (tryReadTBQueue queue) >>= \case
                Nothing -> pure ()
                Just job -> failJob cause job >> drain
        drain

  void $ forkLinkedThread registry "leiosdb-writer" $ do
    -- The brackets close the connections on every way out of 'worker': a
    -- served 'Shutdown', a failed write, a cancellation.
    outcome <- try $ withWriterConns tracer statsVar volPath immPath worker
    readIORef shutdownVar >>= \case
      -- Stopped on request: the close outcome, failed or not, is the
      -- awaiter's to report, and the worker ends quietly.
      Just resultVar -> do
        atomically $ putTMVar resultVar outcome
        sealAndDrain closedException
      -- Anything else stopped the worker. Then out, so the link takes the
      -- node down where the write failed rather than at whatever submits
      -- next. Cancellation by the registry is the one stop the link lets pass.
      Nothing -> do
        sealAndDrain (either id (const closedException) outcome)
        either throwIO pure outcome
  pure WriteQueue{wqJobs = queue, wqSealed = sealedVar, wqTracer = tracer}
 where
  closedException =
    toException $
      LeiosDbException
        LeiosDbFailure
          { ldfErrorMessage = "the LeiosDB writer is closed"
          , ldfCallStack = GHC.Stack.prettyCallStack GHC.Stack.callStack
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
