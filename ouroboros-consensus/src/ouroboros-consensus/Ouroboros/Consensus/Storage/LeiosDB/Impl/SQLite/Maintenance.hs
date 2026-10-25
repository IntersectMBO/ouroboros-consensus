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
  , gcMark

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
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.String (fromString)
import qualified Database.SQLite3.Direct as DB
import qualified GHC.Conc as IO (atomically)
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.Leios.Types
  ( EbHash
  , LeiosPoint (..)
  , ebHashBytes
  )
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
--   - synchronously pin the EB rows in the volatile partition for promotion (@status@ 0 -> 1),
--     through the writer ('PinEb');
--   - ring the copier's doorbell.
sqlPromoteToImmutable :: WriteQueue -> StrictTVar IO Bool -> [LeiosPoint] -> IO ()
sqlPromoteToImmutable writeQueue copierDoorbell points = unless (null points) $ do
  await =<< submitJob writeQueue (PinEb [pointEbHash p | p <- points])
  -- notify the copier thread that there's work to be done
  atomically $ writeTVar copierDoorbell True

-- | The copier's connection and its prepared statements.
data CopierConn = CopierConn
  { ccDb :: !DB.Database
  -- ^ main = immutable partition, @vol@ = attached volatile partition
  , ccStmts :: !CopierStmts
  -- ^ Precompiled statements used by the copier.
  }

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
          "pinned EB has no closure in the volatile partition"
      pure False
    Just _copiedTxs -> do
      -- successfully copied, bump the stats
      bumpImmutableStats statsVar 1
      traceWith tracer $ TraceLeiosDbCopiedToImmutable 1
      pure True
 where
  CopierConn{ccDb, ccStmts} = conn
  CopierStmts{ccHasClosure, ccInsertEb, ccInsertEbBody, ccInsertEbClosures} = ccStmts

  -- Attempt to do the actual copying.
  --
  -- Returns the number of copied rows, or Nothing if the volatile
  -- partition holds no closure.
  appendToImmutable :: IO (Maybe Int)
  appendToImmutable =
    dbWithTransaction ccDb $ do
      -- the cert-RB is on our chain, hence the fetch logic completed the EB.
      -- Still check that it has a closure at all, as it's very cheap.
      hasClosure <-
        useStmt ccHasClosure $ do
          dbBindBlob ccHasClosure 1 (ebHashBytes ebHash)
          (== DB.Row) <$> dbStepSafe ccHasClosure
      if not hasClosure
        then
          -- the EB has no closure, don't copy
          pure Nothing
        else do
          -- copy the EB
          useStmt ccInsertEb $ do
            dbBindBlob ccInsertEb 1 (ebHashBytes ebHash)
            dbStep1Safe ccInsertEb
          -- copy its body and closure
          useStmt ccInsertEbBody $ do
            dbBindBlob ccInsertEbBody 1 (ebHashBytes ebHash)
            dbStep1Safe ccInsertEbBody
          useStmt ccInsertEbClosures $ do
            dbBindBlob ccInsertEbClosures 1 (ebHashBytes ebHash)
            dbStep1Safe ccInsertEbClosures
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
                  h <- ebHashFromBlob =<< DB.columnBlob nextPinnedStmt 0
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
    -- saturated inserts.
    copyBatch ebHashes = do
      copied <- filterM copyOne ebHashes
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

-- | Implements 'leiosDbGarbageCollect': the MARK phase of GC mark-and-sweep,
-- as a 'GcMark' job on the writer (see 'gcMark').
sqlGarbageCollect :: WriteQueue -> SlotNo -> IO ()
sqlGarbageCollect writeQueue gcSlot =
  void . await =<< submitJob writeQueue (GcMark gcSlot)

-- | The MARK phase of GC mark-and-sweep: mark for GC (@status = 3@) every EB
-- hash all of whose announcements are older than the given slot and not
-- pinned (@status = 1@). Returns the number of 'ebs' rows marked.
--
-- Runs on the writer. It does not do much work, but rather primes the state
-- for the writer's own sweep steps (see 'startWriter'): the writer evicts the
-- marked EBs one batch per turn, so an insert waits for a batch at most.
gcMark ::
  HasCallStack =>
  DB.Database ->
  GcStmts ->
  SlotNo ->
  IO Int
gcMark db gcStmts gcSlot = do
  let GcStmts{gsHasWork, gsMarkEbForGC} = gcStmts
  -- check if GC has any work to do
  hasWork <-
    useStmt gsHasWork $ do
      dbBindInt64 gsHasWork 1 slot
      (/= 0) <$> readSingleInt64 gsHasWork
  if hasWork
    then
      -- mark EBs that are ready to be garbage collected
      dbWithWriteTransactionRaw db $ do
        useStmt gsMarkEbForGC $ do
          dbBindInt64 gsMarkEbForGC 1 slot
          dbStep1Safe gsMarkEbForGC
        DB.changes db
    else pure 0
 where
  slot = fromIntegral (unSlotNo gcSlot)

-- * Sweeping GC-marked rows out of the volatile partition

-- | The sweeper's connection and its prepared statements.
data SweeperConn = SweeperConn
  { swDb :: !DB.Database
  -- ^ The writer's connection to the volatile partition.
  , swStmts :: !SweeperStmts
  -- ^ Precompiled statements used by the sweeper.
  }

-- | One 'SweepEbBatch' transaction: evict up to the given number of GC-marked
-- EBs, with their bodies and closures -- three deletes on the EBs' hashes.
-- Returns the number of evicted 'ebs' rows and the hashes of the evicted EBs.
-- Runs on the writer.
sweepEbBatch :: SweeperConn -> Int64 -> IO (Int, [ByteString])
sweepEbBatch conn batchSize = do
  let SweeperConn{swDb, swStmts} = conn
      SweeperStmts{swPickMarked, swEvictEbBodies, swEvictEbClosures, swEvictEbs} = swStmts
  dbWithWriteTransactionRaw swDb $ do
    -- check if any EBs are ready to be evicted
    evictableEbs <- useStmt swPickMarked $ do
      -- batchSize will never be negative, but we handle a negative batch size
      -- gracefully here with a negative LIMIT, which means no limit in SQLite.
      dbBindInt64 swPickMarked 1 (if batchSize <= 0 then -1 else batchSize)
      collectBlobs swPickMarked
    -- evict EBs if any are ready to be GCed
    if null evictableEbs
      then pure (0, [])
      else do
        let evictableEbsJson = jsonHexArray evictableEbs
        execJson swEvictEbClosures evictableEbsJson
        execJson swEvictEbBodies evictableEbsJson
        -- last, so 'DB.changes' counts the evicted 'ebs' rows
        execJson swEvictEbs evictableEbsJson
        (\n -> (n, evictableEbs)) <$> DB.changes swDb

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
