{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Maintenance of LeiosDB's volatile and immutable partitions.
--
--   * promotion: volatile EBs are copied to the immutable partition once they are old enough;
--   * garbage collection of the volatile partition;
--   * the in-memory 'LeiosDbStats'.
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Maintenance
  ( -- * Promotion to the immutable partition
    Promoter
  , openPromoter
  , closePromoter
  , sqlPromoteToImmutable

    -- * GC mark
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
  , modifyTVar
  , readTVarIO
  )
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad (forever, unless, void)
import Control.ResourceRegistry
  ( ResourceRegistry
  , Thread
  , forkLinkedThread
  )
import Control.Tracer (Tracer, traceWith)
import Data.Int (Int64)
import Data.String (fromString)
import qualified Database.SQLite3.Direct as DB
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.Leios.Types (LeiosPoint (..))
import Ouroboros.Consensus.Storage.LeiosDB.API (Promise (..))
import Ouroboros.Consensus.Storage.LeiosDB.Exception (throwLeiosDbException)
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Connection
  ( Conn (..)
  , closeChecked
  , openRawConnection
  , withReadOnlyConn
  )
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Primitives
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

-- | The connection that copies EBs into the immutable partition, and its
-- prepared statements. One promotion at a time uses it.
data Promoter = Promoter
  { prDb :: !DB.Database
  -- ^ main = immutable partition, @vol@ = attached volatile partition
  , prStmts :: !PromoteStmts
  }

-- | Open the promoter's connection to the immutable partition, with the
-- volatile partition attached, and prepare its statements.
openPromoter :: FilePath -> FilePath -> IO (MVar Promoter)
openPromoter volPath immPath = do
  db <- openRawConnection immPath
  stmts <- orCloseOnError db $ do
    withStmt db "ATTACH ? AS vol" $ \stmt -> do
      dbBindUtf8 stmt 1 (fromString volPath)
      dbStep1Safe stmt
    preparePromoteStmts db
  newMVar (Promoter db stmts)

-- | Close the promoter's connection, once no promotion is using it.
closePromoter :: MVar Promoter -> IO ()
closePromoter promoterVar =
  withMVar promoterVar $ \Promoter{prDb, prStmts} -> do
    finalizePromoteStmts prStmts
    closeChecked prDb

-- | Implements 'leiosDbPromoteToImmutable': copy the EBs into the immutable
-- partition, at the given points, in one transaction.
--
-- The blocks certifying them are on the chain, hence the fetch logic completed
-- the EBs. Still check, by hashing their txs, since the immutable partition is
-- never collected and the startup scan takes any EB there as complete. An EB
-- without its closure fails the whole promotion, and nothing is copied.
sqlPromoteToImmutable ::
  HasCallStack =>
  Tracer IO TraceLeiosDb ->
  StrictTVar IO LeiosDbStats ->
  MVar Promoter ->
  [LeiosPoint] ->
  IO ()
sqlPromoteToImmutable tracer statsVar promoterVar points = unless (null points) $ do
  copied <- withMVar promoterVar $ \Promoter{prDb, prStmts} ->
    -- Deferred, not 'dbWithWriteTransactionRaw': @BEGIN IMMEDIATE@ takes the
    -- write lock of every database on the connection, the attached volatile
    -- partition too, and would hold off the writer for the whole promotion.
    -- Promotion only reads the volatile partition, and the immutable one has
    -- no other writer, so taking its lock at the first insert cannot fail.
    dbWithTransaction prDb $
      sum <$> mapM (copyEb prDb prStmts) points
  bumpImmutableStats statsVar copied
  traceWith tracer $ TraceLeiosDbCopiedToImmutable copied

-- | Copy one EB into the immutable partition. Returns the number of rows
-- copied: none if the immutable partition holds the EB already, as after a
-- crash between this copy and appending the block.
copyEb :: HasCallStack => DB.Database -> PromoteStmts -> LeiosPoint -> IO Int
copyEb db PromoteStmts{psClosure, psInsertEb} point = do
  -- The immutable partition is read by hash, so an EB it holds at any point
  -- is promoted already. Check that first: a crash can lose the volatile
  -- writes that completed the EB, and nothing fetches them again, since the
  -- startup scan takes the immutable copy as complete.
  promoted <-
    withStmt db "SELECT 1 FROM ebs WHERE ebHashBytes = ?1 LIMIT 1" $ \stmt -> do
      dbBindHash stmt 1 ebHash
      dbStepSafe stmt >>= \case
        DB.Done -> pure False
        DB.Row -> pure True
  if promoted then pure 0 else copyFromVolatile
 where
  ebHash = pointEbHash point

  copyFromVolatile = do
    complete <- useStmt psClosure $ do
      dbBindHash psClosure 1 ebHash
      dbStepSafe psClosure >>= \case
        DB.Done -> pure False
        DB.Row -> do
          body <- DB.columnBlob psClosure 0
          bounds <- DB.columnBlob psClosure 1
          txs <- DB.columnBlob psClosure 2
          pure $ maybe False (closureMatches body) (sequence (splitClosure bounds txs))
    unless complete $
      throwLeiosDbException $
        "leiosDbPromoteToImmutable: no complete closure in the volatile partition: " <> show point
    useStmt psInsertEb $ do
      dbBindInt64 psInsertEb 1 (fromIntegral $ unSlotNo (pointSlotNo point))
      dbBindHash psInsertEb 2 ebHash
      dbStep1Safe psInsertEb
    DB.changes db

-- * Garbage collection of the volatile partition

-- | How many rows one sweep transaction takes, by default.
defaultGcBatchSize :: Int64
defaultGcBatchSize = 4

-- | Implements 'Ouroboros.Consensus.Storage.LeiosDB.API.garbageCollect': the
-- MARK phase of GC mark-and-sweep, as a 'GcMark' job on the writer, which
-- raises the slot the sweep collects before. The slot is in memory only: after
-- a restart, the next GC sets it again.
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

-- | One sweep transaction: delete up to the given number of rows before the
-- slot of the latest GC. Returns the number of rows deleted. Runs on the
-- writer, one batch per turn, so an insert waits for a batch at most.
sweepEbBatch :: SweeperConn -> Int64 -> SlotNo -> IO Int
sweepEbBatch SweeperConn{swDb, swStmts = SweeperStmts{swSweep}} batchSize gcSlot =
  dbWithWriteTransactionRaw swDb $ useStmt swSweep $ do
    -- batchSize will never be negative, but we handle a negative batch size
    -- gracefully here with a negative LIMIT, which means no limit in SQLite.
    dbBindInt64 swSweep 1 (if batchSize <= 0 then -1 else batchSize)
    dbBindInt64 swSweep 2 (fromIntegral (unSlotNo gcSlot))
    dbStep1Safe swSweep
    DB.changes swDb

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
