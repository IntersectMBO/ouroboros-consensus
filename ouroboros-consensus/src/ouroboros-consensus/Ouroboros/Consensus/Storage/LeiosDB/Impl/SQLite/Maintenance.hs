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

    -- * Garbage collection
  , sqlGarbageCollect
  , gcBatchSize
  , deleteEbsBefore

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
import Control.Monad (forever, unless, when)
import Control.ResourceRegistry
  ( ResourceRegistry
  , Thread
  , forkLinkedThread
  )
import Control.Tracer (Tracer, traceWith)
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
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.WriteQueue
  ( WriteJob (..)
  , WriteQueue
  , submitJob
  )
import Ouroboros.Consensus.Storage.LeiosDB.Trace (LeiosDbStats (..), TraceLeiosDb (..))
import Ouroboros.Consensus.Util.IOLike (atomically)
import System.Directory (doesFileExist, getFileSize)

-- * Copying EBs to the immutable partition

-- | The connection that copies EBs into the immutable partition: main is the
-- immutable partition, @vol@ the attached volatile one. One promotion at a
-- time uses it.
newtype Promoter = Promoter DB.Database

-- | Open the promoter's connection to the immutable partition, with the
-- volatile partition attached.
openPromoter :: FilePath -> FilePath -> IO (MVar Promoter)
openPromoter volPath immPath = do
  db <- openRawConnection immPath
  orCloseOnError db $
    withStmt db "ATTACH ? AS vol" $ \stmt -> do
      dbBindUtf8 stmt 1 (fromString volPath)
      dbStep1Safe stmt
  newMVar (Promoter db)

-- | Close the promoter's connection, once no promotion is using it.
closePromoter :: MVar Promoter -> IO ()
closePromoter promoterVar =
  withMVar promoterVar $ \(Promoter db) -> closeChecked db

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
  copied <- withMVar promoterVar $ \(Promoter db) ->
    -- Deferred, not 'dbWithWriteTransactionRaw': @BEGIN IMMEDIATE@ takes the
    -- write lock of every database on the connection, the attached volatile
    -- partition too, and would hold off the writer for the whole promotion.
    -- Promotion only reads the volatile partition, and the immutable one has
    -- no other writer, so taking its lock at the first insert cannot fail.
    dbWithTransaction db $
      sum <$> mapM (copyEbToImmutable db) points
  bumpImmutableStats statsVar copied
  traceWith tracer $ TraceLeiosDbCopiedToImmutable copied
 where
  copyEbToImmutable db point = do
    let ebHash = pointEbHash point
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
    if promoted then pure 0 else copyFromVolatile db point ebHash

  copyFromVolatile db point ebHash = do
    complete <-
      withStmt
        db
        "SELECT body, txBounds, txs FROM vol.ebs\n\
        \WHERE ebHashBytes = ?1 AND txCount IS NOT NULL LIMIT 1"
        $ \stmt -> do
          dbBindHash stmt 1 ebHash
          dbStepSafe stmt >>= \case
            DB.Done -> pure False
            DB.Row -> do
              body <- DB.columnBlob stmt 0
              bounds <- DB.columnBlob stmt 1
              txs <- DB.columnBlob stmt 2
              pure $ maybe False (closureMatches body) (sequence (splitClosure bounds txs))
    unless complete $
      throwLeiosDbException $
        "leiosDbPromoteToImmutable: no complete closure in the volatile partition: " <> show point
    withStmt
      db
      "INSERT OR IGNORE INTO ebs\n\
      \  (ebSlot, ebHashBytes, ebBytesSize, txCount, txBounds, body, txs)\n\
      \SELECT ?1, ebHashBytes, ebBytesSize, txCount, txBounds, body, txs FROM vol.ebs\n\
      \WHERE ebHashBytes = ?2 AND txCount IS NOT NULL LIMIT 1"
      $ \stmt -> do
        dbBindInt64 stmt 1 (fromIntegral $ unSlotNo (pointSlotNo point))
        dbBindHash stmt 2 ebHash
        dbStep1Safe stmt
    DB.changes db

-- * Garbage collection of the volatile partition

-- | Implements 'Ouroboros.Consensus.Storage.LeiosDB.API.garbageCollect', as
-- 'GarbageCollect' jobs on the writer, each deleting up to 'gcBatchSize' rows
-- before the slot, until none is left. The writes queued meanwhile run in
-- between. The slot is in memory only: after a restart, the next GC sets it
-- again.
sqlGarbageCollect :: WriteQueue -> SlotNo -> IO ()
sqlGarbageCollect writeQueue gcSlot = do
  more <- await =<< submitJob writeQueue (GarbageCollect gcSlot)
  when more $ sqlGarbageCollect writeQueue gcSlot

-- | How many rows a 'GarbageCollect' job deletes at most, which bounds how
-- long a write waits behind it.
gcBatchSize :: Int
gcBatchSize = 16

-- | Delete up to the given number of rows before the slot, in one
-- transaction. Returns how many.
deleteEbsBefore :: HasCallStack => DB.Database -> Int -> SlotNo -> IO Int
deleteEbsBefore db limit gcSlot =
  withStmt
    db
    "DELETE FROM ebs WHERE rowid IN\n\
    \  (SELECT rowid FROM ebs WHERE ebSlot < ?1 LIMIT ?2)"
    $ \stmt -> dbWithWriteTransactionRaw db $ do
      dbBindInt64 stmt 1 (fromIntegral (unSlotNo gcSlot))
      dbBindInt64 stmt 2 (fromIntegral limit)
      dbStep1Safe stmt
      DB.changes db

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
    withReadOnlyConn path $ \db ->
      fromIntegral <$> queryInt64 db "SELECT COUNT(*) FROM ebs"

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
