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

    -- * GC mark
  , sqlGarbageCollect

    -- * GC sweep
  , GcFrontier
  , addGcFrontier
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
import Control.Monad (filterM, forever, unless, void)
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
import Ouroboros.Consensus.Leios.Types (LeiosPoint (..))
import Ouroboros.Consensus.Storage.LeiosDB.API (Promise (..))
import Ouroboros.Consensus.Storage.LeiosDB.Exception
  ( LeiosDbException (..)
  )
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Connection
  ( Conn (..)
  , closeChecked
  , openRawConnection
  , withReadOnlyConn
  )
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Primitives
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Queries
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Read (closureMatches, splitClosure)
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
--   - synchronously pin the points in the volatile partition for promotion,
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

-- | Copy one pinned EB into the immutable partition, at its pinned point.
-- 'True' when it landed, so the caller may have its pin retired.
copyEbToImmutable ::
  Tracer IO TraceLeiosDb ->
  StrictTVar IO LeiosDbStats ->
  CopierConn ->
  LeiosPoint ->
  IO Bool
copyEbToImmutable tracer statsVar conn point =
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
  CopierConn{ccDb, ccStmts} = conn
  CopierStmts{ccClosure, ccInsertEb} = ccStmts
  ebHash = pointEbHash point

  -- Attempt to do the actual copying.
  --
  -- Returns the number of copied rows, or Nothing if the volatile
  -- partition holds no complete closure.
  appendToImmutable :: IO (Maybe Int)
  appendToImmutable =
    dbWithTransaction ccDb $ do
      -- The cert-RB is on our chain, hence the fetch logic completed the EB.
      -- Still check, by hashing its txs, since the immutable partition is
      -- never collected and the startup scan takes any EB there as complete.
      complete <-
        useStmt ccClosure $ do
          dbBindHash ccClosure 1 ebHash
          dbStepSafe ccClosure >>= \case
            DB.Done -> pure False
            DB.Row -> do
              body <- DB.columnBlob ccClosure 0
              bounds <- DB.columnBlob ccClosure 1
              txs <- DB.columnBlob ccClosure 2
              pure $ maybe False (closureMatches body) (sequence (splitClosure bounds txs))
      if not complete
        then
          -- the EB has no closure, don't copy
          pure Nothing
        else do
          useStmt ccInsertEb $ do
            dbBindInt64 ccInsertEb 1 (fromIntegral $ unSlotNo (pointSlotNo point))
            dbBindHash ccInsertEb 2 ebHash
            dbStep1Safe ccInsertEb
          Just <$> DB.changes ccDb

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
                  slot <- SlotNo . fromIntegral <$> DB.columnInt64 nextPinnedStmt 0
                  h <- dbColumnHash nextPinnedStmt 1
                  rows (MkLeiosPoint slot h : acc)
         in rows []

    -- 'True' when the EB is in the immutable partition and its pin can be
    -- retired. The EB stays pinned either way, so it is still next to copy.
    -- Backing off is what keeps a closure that never completes -- or a
    -- partition that keeps refusing the write -- from spinning here.
    copyOne point =
      copyEbToImmutable tracer statsVar copierConn point
        `catch` \(e :: LeiosDbException) -> do
          _ <- DB.exec ccDb "ROLLBACK"
          traceWith tracer $ TraceLeiosDbCopyError (show (pointEbHash point)) (displayException e)
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

-- | How many rows one sweep transaction takes, by default.
defaultGcBatchSize :: Int64
defaultGcBatchSize = 4

-- | Implements 'Ouroboros.Consensus.Storage.LeiosDB.API.garbageCollect': the
-- MARK phase of GC mark-and-sweep, as a 'GcMark' job on the writer, which adds
-- a 'GcFrontier' for the sweep. The frontiers are in memory only: after a
-- restart, the next GC adds one again.
sqlGarbageCollect :: WriteQueue -> SlotNo -> IO ()
sqlGarbageCollect writeQueue gcSlot =
  void . await =<< submitJob writeQueue (GcMark gcSlot)

-- * Sweeping collected rows out of the volatile partition

-- | The sweeper's connection and its prepared statements.
data SweeperConn = SweeperConn
  { swDb :: !DB.Database
  -- ^ The writer's connection to the volatile partition.
  , swStmts :: !SweeperStmts
  -- ^ Precompiled statements used by the sweeper.
  }

-- | What one GC collects: the rows before its slot, up to the newest row when
-- it ran, but those of pinned EBs. A point written after it ran stays until
-- the next GC, even if it is before the slot.
data GcFrontier
  = GcFrontier
      -- | Collect the rows before this slot,
      !SlotNo
      -- | up to this row.
      !Int64

-- | Add the frontier of a GC at the given slot to those the sweep has yet to
-- collect. It covers every frontier of a slot no later than its own.
addGcFrontier :: DB.Database -> SlotNo -> [GcFrontier] -> IO [GcFrontier]
addGcFrontier db slot frontiers = do
  rowId <- queryInt64 db sql_gc_watermark
  pure $ GcFrontier slot rowId : [f | f@(GcFrontier s _) <- frontiers, s > slot]

-- | One sweep transaction: delete up to the given number of rows a frontier
-- collects. Returns the number of rows deleted, and the frontiers left: a
-- frontier goes once it holds no rows, and holds those of pinned EBs until
-- they are copied. Runs on the writer, one batch per turn, so an insert waits
-- for a batch at most.
sweepEbBatch :: SweeperConn -> Int64 -> [GcFrontier] -> IO (Int, [GcFrontier])
sweepEbBatch SweeperConn{swDb, swStmts = SweeperStmts{swSweep, swFrontierEmpty}} batchSize = go []
 where
  go kept [] = pure (0, reverse kept)
  go kept (frontier : rest) = do
    deleted <- dbWithWriteTransactionRaw swDb $ useStmt swSweep $ do
      -- batchSize will never be negative, but we handle a negative batch size
      -- gracefully here with a negative LIMIT, which means no limit in SQLite.
      dbBindInt64 swSweep 1 (if batchSize <= 0 then -1 else batchSize)
      bindFrontier swSweep 2 frontier
      dbStep1Safe swSweep
      DB.changes swDb
    if deleted > 0
      then pure (deleted, reverse kept <> (frontier : rest))
      else do
        empty <- useStmt swFrontierEmpty $ do
          bindFrontier swFrontierEmpty 1 frontier
          dbStep swFrontierEmpty >> (/= 0) <$> DB.columnInt64 swFrontierEmpty 0
        go (if empty then kept else frontier : kept) rest

  bindFrontier stmt i (GcFrontier slot rowId) = do
    dbBindInt64 stmt i (fromIntegral (unSlotNo slot))
    dbBindInt64 stmt (i + 1) rowId

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
