{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Maintenance of LeiosDB's volatile and immutable partitions.
--
--   * promotion: volatile EBs are copied to the immutable partition once they are old enough;
--   * garbage collection of the volatile partition;
--   * the in-memory 'LeiosDbStats'.
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Maintenance
  ( -- * Promotion to the immutable partition
    sqlPromoteToImmutable
  , startCopier

    -- * GC
  , sqlGarbageCollect

    -- * GC sweep
  , SweeperConn (..)
  , sweepEbBatch

    -- * GC batch sizes
  , defaultGcBatchSize

    -- * Stats
  , initialStats
  , startVolatileStatsSampler
  , bumpVolatileStats
  , bumpVolatileStatsVar
  , bumpImmutableStats
  ) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTVar
  , check
  , modifyTVar
  , readTVar
  , readTVarIO
  , writeTVar
  )
import Control.Monad (filterM, forM_, forever, unless, void)
import Control.Monad.Class.MonadThrow
  ( bracket
  , catch
  , displayException
  )
import Control.ResourceRegistry
  ( ResourceRegistry
  , Thread
  , forkLinkedThread
  )
import Control.Tracer (Tracer, traceWith)
import Data.Int (Int64)
import Data.String (fromString)
import qualified Database.SQLite3.Direct as DB
import qualified GHC.Conc as IO (atomically)
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.Leios.Types
  ( LeiosPoint (..)
  )
import Ouroboros.Consensus.Storage.LeiosDB.API (Promise (..))
import Ouroboros.Consensus.Storage.LeiosDB.Exception
  ( LeiosDbException (..)
  , throwLeiosDbException
  )
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Connection
  ( Conn (..)
  , closeChecked
  , openRawConnection
  , withReadOnlyConn
  )
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Primitives
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Queries
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Statements
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.WriteQueue
  ( WriteJob (..)
  , WriteQueue
  , submitJob
  )
import Ouroboros.Consensus.Storage.LeiosDB.Trace (LeiosDbStats (..), TraceLeiosDb (..))
import Ouroboros.Consensus.Util.IOLike (atomically)
import System.Directory (doesFileExist, getFileSize)

-- * Copying EBs to the immutable partition

-- | Implements 'leiosDbPromoteToImmutable':
--   - synchronously pin the points' rows in the volatile partition for promotion (@status@ 0 -> 1),
--     through the writer ('PinEb');
--   - ring the copier's doorbell.
sqlPromoteToImmutable :: WriteQueue -> StrictTVar IO Bool -> [LeiosPoint] -> IO ()
sqlPromoteToImmutable writeQueue copierDoorbell points = unless (null points) $ do
  await =<< submitJob writeQueue (PinEb points)
  -- notify the copier thread that there's work to be done
  atomically $ writeTVar copierDoorbell True

-- | The copier's connection and its prepared statements.
data CopierConn = CopierConn
  { ccDb :: !DB.Database
  -- ^ main = immutable partition, @vol@ = attached volatile partition
  , ccStmts :: !CopierStmts
  -- ^ Precompiled statements used by the copier.
  }

-- | Copy one pinned point's closure into the immutable partition. 'True' when
-- it landed, so the caller may have its volatile rows marked as copied.
copyEbToImmutable ::
  Tracer IO TraceLeiosDb ->
  StrictTVar IO LeiosDbStats ->
  CopierConn ->
  LeiosPoint ->
  IO Bool
copyEbToImmutable tracer statsVar conn point =
  appendToImmutable >>= \case
    Nothing -> do
      -- the point is not ready to be copied
      traceWith tracer $
        TraceLeiosDbCopyError
          (show point)
          "pinned EB has no complete closure in the volatile partition"
      pure False
    Just _copiedTxs -> do
      -- successfully copied, bump the stats
      bumpImmutableStats statsVar 1
      traceWith tracer $ TraceLeiosDbCopiedToImmutable 1
      pure True
 where
  CopierConn{ccDb, ccStmts} = conn
  CopierStmts{ccCompleteness, ccInsertEb, ccInsertEbTxs, ccInsertEbTxBytes} = ccStmts

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
          dbBindPoint ccCompleteness point
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
            dbBindPoint ccInsertEb point
            dbStep1Safe ccInsertEb
          -- copy the eb-to-transactions mapping
          useStmt ccInsertEbTxs $ do
            dbBindPoint ccInsertEbTxs point
            dbStep1Safe ccInsertEbTxs
          nTxs <- DB.changes ccDb
          -- copy the tx bytes
          useStmt ccInsertEbTxBytes $ do
            dbBindPoint ccInsertEbTxBytes point
            dbStep1Safe ccInsertEbTxBytes
          pure (Just nTxs)

-- | The copier: the only writer into the immutable partition.
--
-- Returns the copier's thread: cancelling it closes its connection.
startCopier ::
  ResourceRegistry IO ->
  Tracer IO TraceLeiosDb ->
  StrictTVar IO LeiosDbStats ->
  StrictTVar IO Bool ->
  WriteQueue ->
  FilePath ->
  FilePath ->
  IO (Thread IO ())
startCopier registry tracer statsVar copierDoorbell writeQueue volPath immPath =
  forkLinkedThread registry "leiosdb-copier" $
    bracket (openRawConnection immPath) closeChecked $ \db -> do
      withStmt db "ATTACH ? AS vol" $ \stmt -> do
        dbBindUtf8 stmt 1 (fromString volPath)
        dbStep1Safe stmt
      bracket (prepareCopierStmts db) finalizeCopierStmts $ \stmts ->
        bracket (dbPrepare db (fromString sql_next_pinned_eb)) dbFinalize $
          runCopier (CopierConn db stmts)
 where
  runCopier ::
    CopierConn ->
    -- 'sql_next_pinned_eb'
    DB.Statement ->
    IO ()
  runCopier copierConn nextPinnedStmt = loop
   where
    CopierConn{ccDb} = copierConn

    nextPinnedBatch = do
      dbBindInt64 nextPinnedStmt 1 (fromIntegral copyBatchSize)
      useStmt nextPinnedStmt $
        let rows acc =
              dbStepSafe nextPinnedStmt >>= \case
                DB.Done -> pure (reverse acc)
                DB.Row -> do
                  point <- dbColumnPoint nextPinnedStmt 0
                  rows (point : acc)
         in rows []

    -- 'True' when the EB is in the immutable partition and its pin can be
    -- retired. The EB stays pinned either way, so it is still next to copy.
    -- Backing off is what keeps a closure that never completes -- or a
    -- partition that keeps refusing the write -- from spinning here.
    copyOne point =
      copyEbToImmutable tracer statsVar copierConn point
        `catch` \(e :: LeiosDbException) -> do
          _ <- DB.exec ccDb "ROLLBACK"
          traceWith tracer $ TraceLeiosDbCopyError (show point) (displayException e)
          pure False

    -- One 'MarkCopied' for the batch: the mark is what makes a copied EB
    -- evictable, and under sync a queue round-trip per EB queues behind
    -- saturated inserts.
    copyBatch points = do
      copied <- filterM copyOne points
      if null copied
        then threadDelay copyRetryMicros
        else void . await =<< submitJob writeQueue (MarkCopied copied)

    loop = do
      -- Clear the copier doorbell before looking if there's any copying work to do,
      -- so a pin that lands while we look rings again instead of being lost.
      atomically $ writeTVar copierDoorbell False
      nextPinnedBatch >>= \case
        batch@(_ : _) -> copyBatch batch >> loop
        [] -> do
          IO.atomically $ readTVar copierDoorbell >>= check
          loop

-- | How long the copier waits before trying a pinned EB again.
copyRetryMicros :: Int
copyRetryMicros = 1000000

-- | How many pinned EBs the copier takes per pass, and so per 'MarkCopied'.
copyBatchSize :: Int
copyBatchSize = 32

-- * Garbage collection of the volatile partition

-- | How many EBs one sweep transaction takes, by default.
defaultGcBatchSize :: Int64
defaultGcBatchSize = 4

-- | Implements 'Ouroboros.Consensus.Storage.LeiosDB.API.garbageCollect': a
-- 'GarbageCollect' job that raises the writer's GC frontier to the given slot,
-- if that is newer, and starts a sweep pass. The frontier is the newest slot
-- any call has given, so it never moves back.
--
-- A sweep pass evicts the points older than the frontier that are not pinned,
-- oldest first, one batch per turn of the writer between its jobs
-- ('sweepEbBatch'), so an insert waits for a batch at most. The pass ends at
-- the first batch that evicts nothing. So a point that is pinned then, or one
-- written afterwards at a slot older than the frontier, waits for the next
-- call's pass. The call returns once the writer takes up its job, before the
-- pass.
--
-- The frontier is only in memory, not persisted in the database, so after a
-- restart what an interrupted sweep left waits for the next call.
sqlGarbageCollect :: WriteQueue -> SlotNo -> IO ()
sqlGarbageCollect writeQueue gcSlot =
  await =<< submitJob writeQueue (GarbageCollect gcSlot)

-- * Sweeping old points out of the volatile partition

-- | The sweeper's connection and its prepared statements.
data SweeperConn = SweeperConn
  { swDb :: !DB.Database
  -- ^ The writer's connection to the volatile partition.
  , swStmts :: !SweeperStmts
  -- ^ Precompiled statements used by the sweeper.
  }

-- | One sweep transaction: evict up to the given number of the points older
-- than the GC frontier that are not pinned, with their bodies and tx bytes --
-- three range deletes per point. Returns the number of evicted 'ebs' rows.
-- Runs on the writer.
sweepEbBatch :: SweeperConn -> SlotNo -> Int64 -> IO Int
sweepEbBatch conn frontier batchSize = do
  let SweeperConn{swDb, swStmts} = conn
      SweeperStmts{swPick, swEvictEbTxBytes, swEvictEbTxs, swEvictEbs} = swStmts
  dbWithWriteTransactionRaw swDb $ do
    evictable <- useStmt swPick $ do
      dbBindInt64 swPick 1 (fromIntegral (unSlotNo frontier))
      -- batchSize will never be negative, but we handle a negative batch size
      -- gracefully here with a negative LIMIT, which means no limit in SQLite.
      dbBindInt64 swPick 2 (if batchSize <= 0 then -1 else batchSize)
      let loop acc =
            dbStepSafe swPick >>= \case
              DB.Done -> pure (reverse acc)
              DB.Row -> do
                point <- dbColumnPoint swPick 0
                loop (point : acc)
      loop []
    forM_ evictable $ \point ->
      forM_ [swEvictEbTxBytes, swEvictEbTxs, swEvictEbs] $ \stmt ->
        useStmt stmt $ do
          dbBindPoint stmt point
          dbStep1Safe stmt
    pure (length evictable)

-- * Stats

-- | Initialise 'LeiosDbStats' by counting the EB rows of both partitions.
--   This will only run once per process.
initialStats :: HasCallStack => FilePath -> FilePath -> IO LeiosDbStats
initialStats volPath immPath = do
  vol <- countEbs volPath
  imm <- countEbs immPath
  pure
    LeiosDbStats
      { volatileEbs = vol
      , immutableEbs = imm
      , walBytes = 0
      }
 where
  countEbs path =
    fromIntegral <$> withReadOnlyConn path (\db -> queryInt64 db "SELECT COUNT(*) FROM ebs")

-- * Stats sampling

-- | Fork a thread that traces 'TraceLeiosDbStats' every 10 seconds. Samples
-- the volatile partition's file only.
startVolatileStatsSampler ::
  ResourceRegistry IO ->
  Tracer IO TraceLeiosDb ->
  StrictTVar IO LeiosDbStats ->
  FilePath ->
  IO (Thread IO ())
startVolatileStatsSampler registry tracer statsVar volPath =
  forkLinkedThread registry "leiosdb-stats-sampler" $ forever $ do
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
        \s -> s{volatileEbs = volatileEbs s + dEbs}

-- | Fold a delta into the immutable EB count of the in-memory 'LeiosDbStats'.
bumpImmutableStats :: StrictTVar IO LeiosDbStats -> Int -> IO ()
bumpImmutableStats statsVar dEbs =
  unless (dEbs == 0) $
    atomically $
      modifyTVar statsVar $
        \s -> s{immutableEbs = immutableEbs s + dEbs}
