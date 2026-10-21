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
  , vacuumLeiosDb

    -- * SQL strings (re-exported for leios-schedule-gen)
  , sql_schema_vol
  , sql_schema_imm
  , sql_insert_eb
  , sql_insert_ebBody
  , sql_prealloc_ebTxBytes
  , sql_fill_ebTxBytes
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
  , readTVarIO
  , tryReadTBQueue
  , writeTChan
  , writeTVar
  )
import Control.Exception
  ( throwIO
  , toException
  )
import Control.Monad (void)
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
import Data.Maybe (fromMaybe)
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
  -- This variable signals the copier thread to wake up and copy the rows
  -- from the volatile to the immutable LeiosDB partition.
  -- It starts set, so that on a node restart we continue a potentially
  -- interrupted copy pass.
  copierDoorbell <- newTVarIO True

  -- the volatile partition writer thread
  writeQueue <-
    startWriter
      registry
      tracer
      statsVar
      notificationChan
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
    -- and whatever killed it is already taking the node down through the
    -- link.
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
        , writeTxs = \point txs -> submitJob writeQueue (WriteTxs point txs)
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
-- The worker puts each job's result into its 'WriteResult'. A failed write is
-- not survivable: the worker stops and seals the queue with the exception, so
-- awaiting a job it did not serve, or submitting another, throws it.
startWriter ::
  ResourceRegistry IO ->
  Tracer IO TraceLeiosDb ->
  StrictTVar IO LeiosDbStats ->
  StrictTChan IO LeiosEbNotification ->
  Int64 ->
  FilePath ->
  FilePath ->
  IO WriteQueue
startWriter registry tracer statsVar notificationChan gcBatchSize volPath immPath = do
  queue <- newTBQueueIO writerQueueDepth
  sealedVar <- newTVarIO Nothing
  -- Set by a served 'Shutdown', whose awaiter gets the outcome of the close.
  shutdownVar <- newIORef Nothing
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
          } = serve 0 (Just 0)
         where
          -- The write worker takes care of two types of jobs: queued jobs (values of `WriteJob`)
          -- and maintenance jobs.
          -- We prioritise queued jobs first: writes arrive in bursts, and the quiet in
          -- between is the time to do maintenance. But a burst can go on for as
          -- long as it likes, so 'maxJobsBetweenMaintenance' prevents maintenance starvation.
          --
          -- The second argument is the state of the GC sweep pass: 'Nothing'
          -- when no sweep is in progress; 'Just' the EBs evicted so far while a
          -- pass is in progress. It starts as 'Just', to continue a sweep pass
          -- potentially interrupted by a node restart.
          serve :: Int -> Maybe Int -> IO ()
          serve served sweep = do
            mJob <-
              if served >= maxJobsBetweenMaintenance
                then
                  -- we've served enough queued jobs, time to do maintenance
                  pure Nothing
                else
                  -- 'Nothing' when there are no queued jobs: do maintenance
                  atomically $ tryReadTBQueue queue
            case mJob of
              -- do a normal queue job
              Just job -> do
                next <- runJob job
                traceWith tracer $ TraceLeiosDbWriteJobDone (describeJob job)
                case next of
                  Stop -> pure ()
                  Continue -> serve (served + 1) sweep
                  ContinueAndSweep -> serve (served + 1) (Just (fromMaybe 0 sweep))
              -- do maintenance
              Nothing -> do
                sweep' <- stepMaintenance sweep
                case sweep' of
                  Nothing -> blockUntilJob
                  Just _ -> pure ()
                serve 0 sweep'

          runJob :: WriteJob -> IO WhetherContinueServing
          runJob = \case
            Shutdown resultVar -> do
              -- Only stop serving: the brackets in 'withWriterConns' close the
              -- connections once 'serve' returns, and the worker hands the
              -- outcome of that close to this job's awaiter.
              writeIORef shutdownVar (Just resultVar)
              pure Stop
            WriteEbPoint point size resultVar ->
              publish resultVar (sqlInsertEbPoint conn point size) >> pure Continue
            WriteEbBody point eb resultVar ->
              publish resultVar (sqlInsertEbBody tracer conn notify point eb) >> pure Continue
            WriteTxs point txs resultVar ->
              publish resultVar (sqlInsertTxs tracer conn notify point txs) >> pure Continue
            Flush resultVar ->
              publish resultVar (pure ()) >> pure Continue
            PinEb ebHashes resultVar -> do
              -- One transaction for the batch: the submitter awaits this while
              -- holding the ImmutableDB write lock, so a round-trip per EB throttles
              -- block immutalisation to this queue's drain rate.
              publish resultVar $
                dbWithWriteTransactionRaw volDb $
                  forM_ ebHashes $ \ebHash ->
                    useStmt pinStmt $ do
                      dbBindBlob pinStmt 1 (ebHashBytes ebHash)
                      dbStep1Safe pinStmt
              pure Continue
            MarkCopied ebHashes resultVar -> do
              -- One transaction for the batch: the mark is what makes a copied EB
              -- evictable, and under sync it otherwise costs a queue round-trip
              -- per EB behind a saturated insert FIFO.
              publish resultVar $
                dbWithWriteTransactionRaw volDb $
                  forM_ ebHashes $ \ebHash ->
                    useStmt markCopiedStmt $ do
                      dbBindBlob markCopiedStmt 1 (ebHashBytes ebHash)
                      dbStep1Safe markCopiedStmt
              pure Continue
            GcMark slot resultVar -> do
              nEbsMarked <- publish resultVar (gcMark volDb gcStmts slot)
              pure $ if nEbsMarked > 0 then ContinueAndSweep else Continue

          -- Nothing queued and no sweep outstanding: wait for a job.
          blockUntilJob = IO.atomically $ do
            noJobs <- isEmptyTBQueue queue
            check (not noJobs)

          -- Advance a sweep by one batch, returning the new sweep state.
          -- A failure here is the maintenance's problem, not the writer's:
          -- trace it, roll back anything left open, and let the next GC tick
          -- bring the work back. Hence 'catch' rather than 'bracket': the
          -- exception must stop here, not take the writer down, and the
          -- cleanup is for failure only.
          stepMaintenance :: Maybe Int -> IO (Maybe Int)
          stepMaintenance sweep =
            step `catch` \(e :: LeiosDbException) -> do
              _ <- DB.exec volDb "ROLLBACK"
              _ <- DB.exec immDb "ROLLBACK"
              traceWith tracer $ TraceLeiosDbGCError (displayException e)
              pure Nothing
           where
            step =
              case sweep of
                Nothing -> pure Nothing
                Just nEbs -> do
                  evicted <- sweepEbBatch sweeperConn gcBatchSize
                  if evicted == 0
                    then do
                      when (nEbs > 0) $ do
                        -- Flush the WAL only after real work.
                        dbExec volDb "PRAGMA wal_checkpoint(PASSIVE);"
                        traceWith tracer $ TraceLeiosDbEvicted nEbs
                      pure Nothing
                    else do
                      bumpVolatileStatsVar statsVar (negate evicted)
                      pure (Just (nEbs + evicted))

      -- Nothing can be queued after the seal ('submitJob'), and awaiting a
      -- job left in the queue throws the cause.
      seal cause = atomically $ writeTVar sealedVar (Just cause)

  void $ forkLinkedThread registry "leiosdb-writer" $ do
    -- The brackets close the connections on every way out of 'worker': a
    -- served 'Shutdown', a failed write, a cancellation.
    outcome <- try $ withWriterConns tracer statsVar volPath immPath worker
    readIORef shutdownVar >>= \case
      -- Stopped on request: the close outcome, failed or not, is the
      -- awaiter's to report (a failed close through the seal), and the worker
      -- ends quietly.
      Just resultVar -> do
        either (const (pure ())) (atomically . putTMVar resultVar) outcome
        seal (either id (const closedException) outcome)
      -- Anything else stopped the worker. Then out, so the link takes the
      -- node down where the write failed rather than at whatever submits
      -- next. Cancellation by the registry is the one stop the link lets pass.
      Nothing -> do
        seal (either id (const closedException) outcome)
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
  -- The action rolls back its own transaction on failure.
  publish :: WriteResult a -> IO a -> IO a
  publish resultVar action = do
    x <- action
    atomically $ putTMVar resultVar x
    pure x

-- | What the writer does after serving a job.
data WhetherContinueServing
  = -- | Stop serving.
    Stop
  | -- | Keep serving.
    Continue
  | -- | Keep serving, and start a GC sweep pass or extend the one in progress.
    ContinueAndSweep
