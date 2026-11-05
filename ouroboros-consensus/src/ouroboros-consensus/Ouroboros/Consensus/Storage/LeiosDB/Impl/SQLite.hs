{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite
  ( newLeiosDBSQLiteFromEnv
  , newLeiosDBSQLite
  , withLeiosDBSQLite

    -- * Re-exported for internal tooling
  , truncateLeiosDbAfterSlot
  , vacuumLeiosDb

    -- * SQL strings (re-exported for leios-schedule-gen)
  , sql_schema_vol
  , sql_schema_imm
  ) where

import Cardano.Prelude (when)
import Cardano.Slotting.Slot (SlotNo)
import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTVar
  , newTBQueueIO
  , newTVarIO
  , putTMVar
  , readTBQueue
  , readTVarIO
  , writeTVar
  )
import Control.Exception
  ( throwIO
  , toException
  )
import Control.Monad (unless, void)
import Control.Monad.Class.MonadThrow
  ( bracket
  , try
  )
import Control.Monad.Class.MonadTime.SI (diffTime, getMonotonicTime)
import Control.ResourceRegistry
  ( ResourceKey
  , ResourceRegistry
  , Thread
  , allocate
  , cancelThread
  , forkLinkedThread
  , release
  , withRegistry
  )
import Control.Tracer (Tracer, traceWith)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Database.SQLite3.Direct as DB
import qualified GHC.Stack
import Ouroboros.Consensus.Leios.Types (LeiosPoint (..))
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbHandle (..)
  , LeiosDbReader (..)
  , LeiosDbWriter (..)
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
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Read
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Schema
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
-- The registry owns the background threads (writer and a thread that samples
-- the database's size) and the promoter's connection: 'closeLeiosDbHandle'
-- stops and closes them in order, and closing the registry releases whatever
-- is still open.
--
-- Creates both files with their schemas before returning, so call it only
-- once their directories may be non-empty (after the ChainDB marker check).
newLeiosDBSQLite ::
  ResourceRegistry IO -> Tracer IO TraceLeiosDb -> FilePath -> FilePath -> IO (LeiosDbHandle IO)
newLeiosDBSQLite registry tracer volLeiosDbPath immLeiosDbPath = do
  -- The database opens before whoever owns these directories creates them.
  mapM_ (createDirectoryIfMissing True . takeDirectory) [volLeiosDbPath, immLeiosDbPath]
  -- create .db files
  initialiseLeiosDbFiles volLeiosDbPath immLeiosDbPath
  -- seed the in-memory stats by counting the EB rows once per handle
  statsVar <- newTVarIO =<< initialStats volLeiosDbPath immLeiosDbPath
  -- start a thread to sample the sizes of the LeiosDB
  samplerThread <- startVolatileStatsSampler registry tracer statsVar volLeiosDbPath
  -- the immutable partition's only writer
  (promoterKey, promoterVar) <-
    allocate registry (\_ -> openPromoter volLeiosDbPath immLeiosDbPath) closePromoter
  -- the volatile partition writer thread
  writeQueue <-
    startWriter
      registry
      tracer
      statsVar
      volLeiosDbPath
      immLeiosDbPath
  pure
    LeiosDbHandle
      { closeLeiosDbHandle = close samplerThread promoterKey writeQueue
      , openReader = implOpenReader statsVar
      , openWriter = implOpenWriter writeQueue
      , garbageCollect = sqlGarbageCollect writeQueue
      , leiosDbPromoteToImmutable = sqlPromoteToImmutable tracer statsVar promoterVar
      , leiosDbSampleStats = readTVarIO statsVar
      }
 where
  close :: Thread IO () -> ResourceKey IO -> WriteQueue -> IO ()
  close samplerThread promoterKey writeQueue = do
    -- The sampler holds no connection -- it only reads counters -- so
    -- cancelling it is safe at any point.
    cancelThread samplerThread
    -- Waits for a promotion in flight.
    _ <- release promoterKey
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
    let conn = mkConn tracer statsVar volDb immDb
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
-- partitions.
data WriterConns = WriterConns
  { wcVolDb :: !DB.Database
  , wcConn :: !Conn
  }

-- | Open the writer's connections and run the action with them. Each
-- connection has its own bracket, so whatever was opened is closed on every
-- way out.
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
      k
        WriterConns
          { wcVolDb = volDb
          , wcConn = mkConn tracer statsVar volDb immDb
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
  FilePath ->
  FilePath ->
  IO WriteQueue
startWriter registry tracer statsVar volPath immPath = do
  queue <- newTBQueueIO writerQueueDepth
  sealedVar <- newTVarIO Nothing
  -- Set by a served 'Shutdown', whose awaiter gets the outcome of the close.
  shutdownVar <- newIORef Nothing
  -- The slot of the latest GC: the writes of a point before it are dropped,
  -- as GC collected that point. Only the worker touches it.
  gcSlotVar <- newIORef 0
  let worker :: WriterConns -> IO ()
      worker
        WriterConns
          { wcVolDb = volDb
          , wcConn = conn
          } = serve
         where
          serve :: IO ()
          serve = do
            job <- atomically $ readTBQueue queue
            start <- getMonotonicTime
            next <- runJob job
            end <- getMonotonicTime
            traceWith tracer $
              TraceLeiosDbWriteJobDone (describeJob job) (realToFrac (diffTime end start))
            case next of
              Stop -> pure ()
              Continue -> serve

          runJob :: WriteJob -> IO WhetherContinueServing
          runJob = \case
            Shutdown resultVar -> do
              -- Only stop serving: the brackets in 'withWriterConns' close the
              -- connections once 'serve' returns, and the worker hands the
              -- outcome of that close to this job's awaiter.
              writeIORef shutdownVar (Just resultVar)
              pure Stop
            WriteEbPoint point size resultVar -> do
              collected <- isCollected point
              publish resultVar (unless collected $ sqlInsertEbPoint conn point size)
                >> pure Continue
            WriteEbBody point eb resultVar -> do
              collected <- isCollected point
              publish resultVar (unless collected $ sqlInsertEbBody conn point eb)
                >> pure Continue
            WriteTxs point txs resultVar ->
              publish resultVar (sqlInsertTxs conn point txs) >> pure Continue
            Flush resultVar ->
              publish resultVar (pure ()) >> pure Continue
            GarbageCollect slot resultVar ->
              publish resultVar (collect slot) >> pure Continue

          -- Whether GC collected the point, so its writes are dropped.
          isCollected :: LeiosPoint -> IO Bool
          isCollected point = (pointSlotNo point <) <$> readIORef gcSlotVar

          -- Whether rows before the slot are left.
          collect :: SlotNo -> IO Bool
          collect slot = do
            modifyIORef' gcSlotVar (max slot)
            evicted <- deleteEbsBefore volDb gcBatchSize =<< readIORef gcSlotVar
            when (evicted > 0) $ do
              bumpVolatileStatsVar statsVar (negate evicted)
              dbExec volDb "PRAGMA wal_checkpoint(PASSIVE);"
              traceWith tracer $ TraceLeiosDbEvicted evicted
            pure (evicted == gcBatchSize)

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
