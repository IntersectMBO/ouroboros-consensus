{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | A 'BackingStore' implementation based on [LMDB](http://www.lmdb.tech/doc/).
module Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB (
    -- * Opening a database
    LMDBLimits (LMDBLimits, lmdbMapSize, lmdbMaxDatabases, lmdbMaxReaders)
  , newLMDBBackingStore
    -- * Errors
  , LMDBErr (..)
    -- * Internals exposed for @snapshot-converter@
  , DbSeqNo (..)
  , LMDBMK (..)
  , getDb
  , initLMDBTable
  , withDbSeqNoRWMaybeNull
  ) where

import           Cardano.Slotting.Slot (SlotNo, WithOrigin (At))
import qualified Codec.Serialise as S (Serialise (..))
import qualified Control.Concurrent.Class.MonadSTM.TVar as IOLike
import           Control.Monad (forM_, unless, void, when)
import qualified Control.Monad.Class.MonadSTM as IOLike
import           Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Tracer as Trace
import           Data.Functor (($>), (<&>))
import           Data.Functor.Contravariant ((>$<))
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.MemPack
import           Data.Proxy
import qualified Data.Set as Set
import qualified Data.Text as Strict
import qualified Database.LMDB.Simple as LMDB
import qualified Database.LMDB.Simple.Cursor as LMDB.Cursor
import qualified Database.LMDB.Simple.Extra as LMDB
import qualified Database.LMDB.Simple.Internal as LMDB.Internal
import qualified Database.LMDB.Simple.TransactionHandle as TrH
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           Ouroboros.Consensus.Ledger.Tables
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.API as API
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB.Bridge as Bridge
import           Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB.Status
                     (Status (..), StatusLock)
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB.Status as Status
import           Ouroboros.Consensus.Util (foldlM')
import           Ouroboros.Consensus.Util.IOLike (Exception (..), IOLike,
                     MonadCatch (..), MonadThrow (..), bracket)
import qualified System.FS.API as FS

{-------------------------------------------------------------------------------
  Database definition
-------------------------------------------------------------------------------}

-- | The LMDB database that underlies the backing store.
data Db m l = Db {
    -- | The LMDB environment is a pointer to the directory that contains the
    -- @`Db`@.
    dbEnv           :: !(LMDB.Environment LMDB.ReadWrite)
    -- | The on-disk state of the @`Db`@.
    --
    -- The state is kept in an LDMB table with only one key and one value:
    -- The current sequence number of the @`Db`@.
  , dbState         :: !(LMDB.Database () DbSeqNo)
    -- | The LMDB tables with the key-value stores.
  , dbBackingTables :: !(LedgerTables l LMDBMK)
  , dbFilePath      :: !FilePath
  , dbTracer        :: !(Trace.Tracer m API.BackingStoreTrace)
    -- | Status of the LMDB backing store. When 'Closed', all backing store
    -- (value handle) operations will fail.
  , dbStatusLock    :: !(StatusLock m)
    -- | Map of open value handles to cleanup actions. When closing the backing
    -- store, these cleanup actions are used to ensure all value handles cleaned
    -- up.
    --
    -- Note: why not use 'bsvhClose' here? We would get nested lock acquisition
    -- on 'dbStatusLock', which causes a deadlock:
    --
    -- * 'bsClose' acquires a write lock
    --
    -- * 'bsvhClose' is called on a value handle
    --
    -- * 'bsvhClose' tries to acquire a read lock, but it has to wait for
    --   'bsClose' to give up its write lock
  , dbOpenHandles   :: !(IOLike.TVar m (Map Int (Cleanup m)))
  , dbNextId        :: !(IOLike.TVar m Int)
  }

newtype LMDBLimits = MkLMDBLimits {unLMDBLimits :: LMDB.Limits}
  deriving (Show, Eq)

{-# COMPLETE LMDBLimits #-}
-- | Configuration to use for LMDB backing store initialisation.
--
-- Keep the following in mind:
--
-- * @'lmdbMapSize'@ should be a multiple of the OS page size.
--
-- * @'lmdbMaxDatabases'@ should be set to at least 2, since the backing store
--    has 2 internal LMDB databases by default: 1 for the actual tables, and
--    1 for the database state @'DbSeqNo'@.
pattern LMDBLimits :: Int -> Int -> Int -> LMDBLimits
pattern LMDBLimits{lmdbMapSize, lmdbMaxDatabases, lmdbMaxReaders} =
  MkLMDBLimits LMDB.Limits {
    LMDB.mapSize      = lmdbMapSize
  , LMDB.maxDatabases = lmdbMaxDatabases
  , LMDB.maxReaders   = lmdbMaxReaders
  }

-- | The database state consists of only the database sequence number @dbsSeq@.
-- @dbsSeq@ represents the slot up to which we have flushed changes to disk.
-- Note that we only flush changes to disk if they have become immutable.
newtype DbSeqNo = DbSeqNo {
    dbsSeq :: WithOrigin SlotNo
  }
  deriving stock (Show, Generic)
  deriving anyclass S.Serialise

-- | A 'MapKind' that represents an LMDB database handle
data LMDBMK k v = LMDBMK !String !(LMDB.Database k v)

{-------------------------------------------------------------------------------
  Low-level API
-------------------------------------------------------------------------------}

getDb ::
     LMDB.Internal.IsMode mode
  => K2 String k v
  -> LMDB.Transaction mode (LMDBMK k v)
getDb (K2 name) = LMDBMK name <$> LMDB.getDatabase (Just name)

readAll ::
     LedgerTablesOp l
  => Proxy l
  -> LedgerTables l LMDBMK
  -> LMDB.Transaction mode (LedgerTables l ValuesMK)
readAll _ = lttraverse (\(LMDBMK _ dbMK) ->
  ValuesMK <$> Bridge.runCursorAsTransaction'
    LMDB.Cursor.cgetAll
    dbMK)

-- | @'rangeRead' rq dbMK@ performs a range read of @rqCount rq@
-- values from database @dbMK@, starting from some key depending on @rqPrev rq@.
--
-- The @codec@ argument defines how to serialise/deserialise keys and values.
--
-- A range read can return less than @count@ values if there are not enough
-- values to read.
--
-- What the "first" key in the database is, and more generally in which order
-- keys are read, depends on the lexographical ordering of the /serialised/
-- keys. Care should be taken such that the @'Ord'@ instance for @k@ matches the
-- lexicographical ordering of the serialised keys, or the result of this
-- function will be unexpected.
rangeRead ::
     forall mode l.
     LedgerTablesOp l
  => API.RangeQuery (LedgerTables l KeysMK)
  -> LedgerTables l LMDBMK
  -> LMDB.Transaction mode (LedgerTables l ValuesMK)
rangeRead rq =
    case ksMK of
      Nothing -> lttraverse (\(LMDBMK _ db) -> runCursorHelper db Nothing)
      Just lts -> ltzipWith2A (\(KeysMK ks) (LMDBMK _ db) ->
                              case Set.lookupMax ks of
                                Nothing -> pure $ ValuesMK mempty
                                Just lastExcludedKey ->
                                  runCursorHelper db $ Just (lastExcludedKey, LMDB.Cursor.Exclusive)) lts
  where

    API.RangeQuery ksMK count = rq

    runCursorHelper ::
         (MemPack k, Ord k , MemPack v)
      => LMDB.Database k v
      -> Maybe (k, LMDB.Cursor.Bound)    -- ^ Lower bound on read range
      -> LMDB.Transaction mode (ValuesMK k v)
    runCursorHelper db lb =
      ValuesMK <$> Bridge.runCursorAsTransaction'
                   (LMDB.Cursor.cgetMany lb count)
                   db

initLMDBTable ::
     (MemPack v, MemPack k)
  => LMDBMK   k v
  -> ValuesMK k v
  -> LMDB.Transaction LMDB.ReadWrite (EmptyMK k v)
initLMDBTable (LMDBMK tblName db) (ValuesMK utxoVals) =
    EmptyMK <$ lmdbInitTable
  where
    lmdbInitTable = do
      isEmpty <- LMDB.null db
      unless isEmpty $ liftIO . throwIO $ LMDBErrInitialisingNonEmpty tblName
      void $ Map.traverseWithKey
                 (Bridge.put db)
                 utxoVals

readLMDBTable ::
     (MemPack v, MemPack k)
  => Ord k
  => LMDBMK  k v
  -> KeysMK  k v
  -> LMDB.Transaction mode (ValuesMK k v)
readLMDBTable (LMDBMK _ db) (KeysMK keys) =
    ValuesMK <$> lmdbReadTable
  where
    lmdbReadTable = foldlM' go Map.empty (Set.toList keys)
      where
        go m k = Bridge.get db k <&> \case
          Nothing -> m
          Just v  -> Map.insert k v m

writeLMDBTable ::
     (MemPack v, MemPack k)
  => LMDBMK  k v
  -> DiffMK  k v
  -> LMDB.Transaction LMDB.ReadWrite (EmptyMK k v)
writeLMDBTable (LMDBMK _ db) (DiffMK d) =
    EmptyMK <$ lmdbWriteTable
  where
    lmdbWriteTable = void $ Diff.traverseDeltaWithKey_ go d
      where
        go k de = case de of
          Diff.Delete   -> void $ Bridge.delete db k
          Diff.Insert v -> Bridge.put db k v

{-------------------------------------------------------------------------------
 Db state
-------------------------------------------------------------------------------}

readDbSeqNoMaybeNull ::
     LMDB.Database () DbSeqNo
  -> LMDB.Transaction mode (Maybe DbSeqNo)
readDbSeqNoMaybeNull db = LMDB.get db ()

readDbSeqNo ::
     LMDB.Database () DbSeqNo
  -> LMDB.Transaction mode DbSeqNo
readDbSeqNo db = readDbSeqNoMaybeNull db >>= maybe (liftIO . throwIO $ LMDBErrNoDbSeqNo) pure

withDbSeqNoRW ::
     LMDB.Database () DbSeqNo
  -> (DbSeqNo -> LMDB.Transaction LMDB.ReadWrite (a, DbSeqNo))
  -> LMDB.Transaction LMDB.ReadWrite a
withDbSeqNoRW db f = withDbSeqNoRWMaybeNull db $ maybe (liftIO . throwIO $ LMDBErrNoDbSeqNo) f

withDbSeqNoRWMaybeNull ::
      LMDB.Database () DbSeqNo
   -> (Maybe DbSeqNo -> LMDB.Transaction LMDB.ReadWrite (a, DbSeqNo))
   -> LMDB.Transaction LMDB.ReadWrite a
withDbSeqNoRWMaybeNull db f  =
  readDbSeqNoMaybeNull db >>= f >>= \(r, sNew) -> LMDB.put db () (Just sNew) $> r

{-------------------------------------------------------------------------------
 Guards
-------------------------------------------------------------------------------}

data GuardDbDir  = DirMustExist | DirMustNotExist

-- | Guard for the existence/non-existence of a database directory,
-- and create it if missing.
checkAndOpenDbDir ::
     (MonadIO m, IOLike m)
  => GuardDbDir
  -> FS.SomeHasFS m
  -> FS.FsPath
  -> m FilePath
checkAndOpenDbDir mustExistDir (FS.SomeHasFS fs) path = do
  fileEx <- FS.doesFileExist fs path
  when fileEx $
    throwIO $ LMDBErrNotADir path
  dirEx <- FS.doesDirectoryExist fs path
  lmdbFileExists <- FS.doesFileExist fs path { FS.fsPathToList = FS.fsPathToList path ++ [Strict.pack "data.mdb"] }
  filepath <- FS.unsafeToFilePath fs path
  case dirEx of
    True  | DirMustNotExist <- mustExistDir -> throwIO $ LMDBErrDirExists filepath
          | not lmdbFileExists              -> throwIO $ LMDBErrDirIsNotLMDB filepath
          | otherwise                       -> pure ()
    False | DirMustExist    <- mustExistDir -> throwIO $ LMDBErrDirDoesntExist filepath
          | otherwise                       -> pure ()
  FS.createDirectoryIfMissing fs True path
  pure filepath

-- | Same as @`checkAndOpenDbDir`@, but retries the guard if we can make meaningful
-- changes to the filesystem before we perform the retry.
--
-- Note: We only retry if a database directory exists while it shoudn't. In
-- this case, we remove the directory recursively before retrying the guard.
-- This is necessary for initialisation of the LMDB backing store, since the
-- (non-snapshot) tables will probably still be on-disk. These tables are not
-- removed when stopping the node, so they should be "overwritten".
checkAndOpenDbDirWithRetry ::
     (MonadIO m, IOLike m)
  => GuardDbDir
  -> FS.SomeHasFS m
  -> FS.FsPath
  -> m FilePath
checkAndOpenDbDirWithRetry gdd shfs@(FS.SomeHasFS fs) path =
    handle retryHandler (checkAndOpenDbDir gdd shfs path)
  where
    retryHandler e = case (gdd, e) of
      (DirMustNotExist, LMDBErrDirExists _path) -> do
        FS.removeDirectoryRecursive fs path
        checkAndOpenDbDir DirMustNotExist shfs path
      _ -> throwIO e

{-------------------------------------------------------------------------------
 Initialize an LMDB
-------------------------------------------------------------------------------}

-- | Initialise an LMDB database from these provided values.
initFromVals ::
     forall l m.
     (MonadIO m, LedgerTablesOp l)
  => Trace.Tracer m API.BackingStoreTrace
  -> WithOrigin SlotNo
     -- ^ The slot number up to which the ledger tables contain values.
  -> LedgerTables l ValuesMK
     -- ^ The ledger tables to initialise the LMDB database tables with.
  -> LMDB.Environment LMDB.Internal.ReadWrite
     -- ^ The LMDB environment.
  -> LMDB.Database () DbSeqNo
     -- ^ The state of the tables we are going to initialize the db with.
  -> LedgerTables l LMDBMK
  -> m ()
initFromVals tracer dbsSeq vals env st backingTables = do
  Trace.traceWith tracer $ API.BSInitialisingFromValues dbsSeq
  liftIO $ LMDB.readWriteTransaction env $
    withDbSeqNoRWMaybeNull st $ \case
      Nothing -> ltzipWith2A initLMDBTable backingTables vals
                 $> ((), DbSeqNo{dbsSeq})
      Just _ -> liftIO . throwIO $ LMDBErrInitialisingAlreadyHasState
  Trace.traceWith tracer $ API.BSInitialisedFromValues dbsSeq

-- | Initialise an LMDB database from an existing LMDB database.
initFromLMDBs ::
     (MonadIO m, IOLike m)
  => Trace.Tracer m API.BackingStoreTrace
  -> LMDBLimits
     -- ^ Configuration for the LMDB database that we initialise from.
  -> API.SnapshotsFS m
     -- ^ Abstraction over the filesystem.
  -> FS.FsPath
     -- ^ The path that contains the LMDB database that we want to initialise from.
  -> API.LiveLMDBFS m
     -- ^ Abstraction over the filesystem.
  -> FS.FsPath
     -- ^ The path where the new LMDB database should be initialised.
  -> m ()
initFromLMDBs tracer limits (API.SnapshotsFS shfsFrom@(FS.SomeHasFS fsFrom)) from0 (API.LiveLMDBFS shfsTo) to0 = do
    Trace.traceWith tracer $ API.BSInitialisingFromCopy from0
    from <- checkAndOpenDbDir DirMustExist shfsFrom from0
    -- On Windows, if we don't choose the mapsize carefully it will make the
    -- snapshot grow. Therefore we are using the current filesize as mapsize
    -- when opening the snapshot to avoid this.
    stat <- FS.withFile fsFrom (from0 { FS.fsPathToList = FS.fsPathToList from0 ++ [Strict.pack "data.mdb"] }) FS.ReadMode (FS.hGetSize fsFrom)
    to <- checkAndOpenDbDirWithRetry DirMustNotExist shfsTo to0
    bracket
      (liftIO $ LMDB.openEnvironment from ((unLMDBLimits limits) { LMDB.mapSize = fromIntegral stat }))
      (liftIO . LMDB.closeEnvironment)
      (flip (lmdbCopy from0 tracer) to)
    Trace.traceWith tracer $ API.BSInitialisedFromCopy from0

-- | Copy an existing LMDB database to a given directory.
lmdbCopy :: MonadIO m
  => FS.FsPath
  -> Trace.Tracer m API.BackingStoreTrace
  -> LMDB.Environment LMDB.ReadWrite
     -- ^ The environment in which the LMDB database lives.
  -> FilePath
     -- ^ The path where the copy should reside.
  -> m ()
lmdbCopy from0 tracer e to = do
  Trace.traceWith tracer $ API.BSCopying from0
  liftIO $ LMDB.copyEnvironment e to
  Trace.traceWith tracer $ API.BSCopied from0

-- | Initialise a backing store.
newLMDBBackingStore ::
     forall m l. (HasCallStack, MonadIO m, IOLike m, LedgerTablesOp l)
  => Trace.Tracer m API.BackingStoreTrace
  -> LMDBLimits
     -- ^ Configuration parameters for the LMDB database that we
     -- initialise. In case we initialise the LMDB database from
     -- an existing LMDB database, we use these same configuration parameters
     -- to open the existing LMDB database.
  -> API.LiveLMDBFS m
     -- ^ The FS for the LMDB live database
  -> API.SnapshotsFS m
  -> API.InitFrom l (LedgerTables l ValuesMK)
  -> m (API.LedgerBackingStore m l)
newLMDBBackingStore dbTracer limits liveFS@(API.LiveLMDBFS liveFS') snapFS@(API.SnapshotsFS snapFS') initFrom = do
   Trace.traceWith dbTracer API.BSOpening

   db@Db { dbEnv
         , dbState
         , dbBackingTables
         } <- createOrGetDB

   maybePopulate dbEnv dbState dbBackingTables

   Trace.traceWith dbTracer $ API.BSOpened $ Just path

   pure $ mkBackingStore db
 where

   path = FS.mkFsPath ["tables"]

   createOrGetDB :: m (Db m l)
   createOrGetDB = do

     dbOpenHandles <- IOLike.newTVarIO Map.empty
     dbStatusLock  <- Status.new Open

     -- get the filepath for this db creates the directory if appropriate
     dbFilePath <- checkAndOpenDbDirWithRetry DirMustNotExist liveFS' path

     -- copy from another lmdb path if appropriate
     case initFrom of
       API.InitFromCopy _ fp -> initFromLMDBs dbTracer limits snapFS fp liveFS path
       API.InitFromValues{}  -> pure ()

     -- open this database
     dbEnv <- liftIO $ LMDB.openEnvironment dbFilePath (unLMDBLimits limits)

     -- The LMDB.Database that holds the @`DbSeqNo`@ (i.e. sequence number)
     -- This transaction must be read-write because on initialisation it creates the database
     dbState <- liftIO $ LMDB.readWriteTransaction dbEnv $ LMDB.getDatabase (Just "_dbstate")

     -- Here we get the LMDB.Databases for the tables of the ledger state
     -- Must be read-write transaction because tables may need to be created
     dbBackingTables <- liftIO $ LMDB.readWriteTransaction dbEnv $
       lttraverse getDb (ltpure $ K2 "utxo")

     dbNextId <- IOLike.newTVarIO 0

     pure $ Db { dbEnv
               , dbState
               , dbBackingTables
               , dbFilePath
               , dbTracer
               , dbStatusLock
               , dbOpenHandles
               , dbNextId
               }

   maybePopulate :: LMDB.Internal.Environment  LMDB.Internal.ReadWrite
                 -> LMDB.Internal.Database () DbSeqNo
                 -> LedgerTables l LMDBMK
                 -> m ()
   maybePopulate dbEnv dbState dbBackingTables = do
     -- now initialise those tables if appropriate
     case initFrom of
       API.InitFromValues slot vals -> initFromVals dbTracer slot vals dbEnv dbState dbBackingTables
       API.InitFromCopy{}           -> pure ()

   mkBackingStore :: HasCallStack => Db m l -> API.LedgerBackingStore m l
   mkBackingStore db =
       let bsClose :: m ()
           bsClose = Status.withWriteAccess dbStatusLock traceAlreadyClosed $ do
             Trace.traceWith dbTracer API.BSClosing
             openHandles <- IOLike.readTVarIO dbOpenHandles
             forM_ openHandles runCleanup
             IOLike.atomically $ IOLike.writeTVar dbOpenHandles mempty
             liftIO $ LMDB.closeEnvironment dbEnv
             Trace.traceWith dbTracer API.BSClosed
             pure ((), Closed)
            where
              traceAlreadyClosed = Trace.traceWith dbTracer API.BSAlreadyClosed

           bsCopy bsp = Status.withReadAccess dbStatusLock (throwIO LMDBErrClosed) $ do
             to <- checkAndOpenDbDir DirMustNotExist snapFS' bsp
             lmdbCopy path dbTracer dbEnv to

           bsValueHandle = Status.withReadAccess dbStatusLock (throwIO LMDBErrClosed) $ do
             mkLMDBBackingStoreValueHandle db

           bsWrite :: SlotNo -> LedgerTables l DiffMK -> m ()
           bsWrite slot diffs = do
             Trace.traceWith dbTracer $ API.BSWriting slot
             Status.withReadAccess dbStatusLock (throwIO LMDBErrClosed) $ do
               oldSlot <- liftIO $ LMDB.readWriteTransaction dbEnv $ withDbSeqNoRW dbState $ \s@DbSeqNo{dbsSeq} -> do
                 unless (dbsSeq <= At slot) $
                   -- This inequality is non-strict because of EBBs having the
                   -- same slot as its predecessor.
                   liftIO . throwIO $ LMDBErrNonMonotonicSeq (At slot) dbsSeq
                 void $ ltzipWith2A writeLMDBTable dbBackingTables diffs
                 pure (dbsSeq, s {dbsSeq = At slot})
               Trace.traceWith dbTracer $ API.BSWritten oldSlot slot

       in API.BackingStore { API.bsClose       = bsClose
                           , API.bsCopy        = bsCopy
                           , API.bsValueHandle = bsValueHandle
                           , API.bsWrite       = bsWrite
                           }

      where
        Db { dbEnv
           , dbState
           , dbBackingTables
           , dbStatusLock
           , dbOpenHandles
           } = db

-- | Create a backing store value handle that has a consistent view of the
-- current database state.
mkLMDBBackingStoreValueHandle ::
     forall l m.
     (MonadIO m, IOLike m, HasCallStack, LedgerTablesOp l)
  => Db m l
     -- ^ The LMDB database for which the backing store value handle is
     -- created.
  -> m (API.LedgerBackingStoreValueHandle m l)
mkLMDBBackingStoreValueHandle db = do
  vhId <- IOLike.atomically $ do
    vhId <- IOLike.readTVar dbNextId
    IOLike.modifyTVar' dbNextId (+1)
    pure vhId

  let
    dbEnvRo = LMDB.readOnlyEnvironment dbEnv
    tracer = API.BSValueHandleTrace (Just vhId) >$< dbTracer

  Trace.traceWith dbTracer API.BSCreatingValueHandle

  trh <- liftIO $ TrH.newReadOnly dbEnvRo
  mbInitSlot <- liftIO $ TrH.submitReadOnly trh $ readDbSeqNoMaybeNull dbState
  initSlot <- liftIO $ maybe (throwIO LMDBErrUnableToReadSeqNo) (pure . dbsSeq) mbInitSlot

  vhStatusLock <- Status.new Open

  let
    -- | Clean up a backing store value handle by committing its transaction
    -- handle.
    cleanup :: Cleanup m
    cleanup = Cleanup $
      liftIO $ TrH.commit trh

    bsvhClose :: m ()
    bsvhClose =
      Status.withReadAccess dbStatusLock traceAlreadyClosed $ do
      Status.withWriteAccess vhStatusLock traceTVHAlreadyClosed $ do
        Trace.traceWith tracer API.BSVHClosing
        runCleanup cleanup
        IOLike.atomically $ IOLike.modifyTVar' dbOpenHandles (Map.delete vhId)
        Trace.traceWith tracer API.BSVHClosed
        pure ((), Closed)
      where
        traceAlreadyClosed    = Trace.traceWith dbTracer API.BSAlreadyClosed
        traceTVHAlreadyClosed = Trace.traceWith tracer API.BSVHAlreadyClosed

    bsvhRead :: LedgerTables l KeysMK -> m (LedgerTables l ValuesMK)
    bsvhRead keys =
      Status.withReadAccess dbStatusLock (throwIO LMDBErrClosed) $ do
      Status.withReadAccess vhStatusLock (throwIO (LMDBErrNoValueHandle vhId)) $ do
        Trace.traceWith tracer API.BSVHReading
        res <- liftIO $ TrH.submitReadOnly trh $
          ltzipWith2A readLMDBTable dbBackingTables keys
        Trace.traceWith tracer API.BSVHRead
        pure res

    bsvhRangeRead ::
         API.RangeQuery (LedgerTables l KeysMK)
      -> m (LedgerTables l ValuesMK)
    bsvhRangeRead rq =
      Status.withReadAccess dbStatusLock (throwIO LMDBErrClosed) $ do
      Status.withReadAccess vhStatusLock (throwIO (LMDBErrNoValueHandle vhId)) $ do
        Trace.traceWith tracer API.BSVHRangeReading
        res <- liftIO $ TrH.submitReadOnly trh $ rangeRead rq dbBackingTables
        Trace.traceWith tracer API.BSVHRangeRead
        pure res

    bsvhStat :: m API.Statistics
    bsvhStat =
      Status.withReadAccess dbStatusLock (throwIO LMDBErrClosed) $ do
      Status.withReadAccess vhStatusLock (throwIO (LMDBErrNoValueHandle vhId)) $ do
        Trace.traceWith tracer API.BSVHStatting
        let transaction = do
              DbSeqNo{dbsSeq} <- readDbSeqNo dbState
              constn <- lttraverse (\(LMDBMK _ dbx) -> K2 <$> LMDB.size dbx) dbBackingTables
              let n = ltcollapse constn
              pure $ API.Statistics dbsSeq n
        res <- liftIO $ TrH.submitReadOnly trh transaction
        Trace.traceWith tracer API.BSVHStatted
        pure res

    bsvhReadAll :: m (LedgerTables l ValuesMK)
    bsvhReadAll =
      Status.withReadAccess dbStatusLock (throwIO LMDBErrClosed) $ do
      Status.withReadAccess vhStatusLock (throwIO (LMDBErrNoValueHandle vhId)) $ do
        Trace.traceWith tracer API.BSVHRangeReading
        res <- liftIO $ TrH.submitReadOnly trh $ readAll (Proxy @l) dbBackingTables
        Trace.traceWith tracer API.BSVHRangeRead
        pure res

    bsvh = API.BackingStoreValueHandle { API.bsvhAtSlot = initSlot
                                       , API.bsvhClose = bsvhClose
                                       , API.bsvhRead = bsvhRead
                                       , API.bsvhReadAll = bsvhReadAll
                                       , API.bsvhRangeRead = bsvhRangeRead
                                       , API.bsvhStat = bsvhStat
                                       }

  IOLike.atomically $ IOLike.modifyTVar' dbOpenHandles (Map.insert vhId cleanup)

  Trace.traceWith dbTracer API.BSCreatedValueHandle
  pure bsvh

 where
   Db { dbEnv
      , dbTracer
      , dbState
      , dbOpenHandles
      , dbBackingTables
      , dbNextId
      , dbStatusLock
      } = db

-- | A monadic action used for cleaning up resources.
newtype Cleanup m = Cleanup { runCleanup :: m () }

{-------------------------------------------------------------------------------
 Errors
-------------------------------------------------------------------------------}

-- | Errors that can be thrown by LMDB.
--
-- __WARNING__: these errors will be thrown in IO as having a corrupt database
-- is critical for the functioning of Consensus.
data LMDBErr =
    -- | The database state can not be found on-disk.
    LMDBErrNoDbSeqNo
    -- | The sequence number of a @`Db`@ should be monotonically increasing
    -- across calls to @`bsWrite`@, since we use @`bsWrite`@ to flush
    -- /immutable/ changes. That is, we can only flush with a newer sequence
    -- number because the changes should be /immutable/. Note that this does
    -- not mean that values can not be changed in the future, only that we
    -- can not change values in the past.
  | LMDBErrNonMonotonicSeq !(WithOrigin SlotNo) !(WithOrigin SlotNo)
    -- | The database table that is being initialised is non-empty.
  | LMDBErrInitialisingNonEmpty !String
    -- | The database that is being initialized already had a DbSeqNo table
  | LMDBErrInitialisingAlreadyHasState
    -- | Trying to use a non-existing value handle.
  | LMDBErrNoValueHandle !Int
    -- | Couldn't create a value handle because we couldn't read the sequence
    -- number
  | LMDBErrUnableToReadSeqNo
    -- | Failed to read a value from a database table.
  | LMDBErrBadRead
    -- | Failed to read a range of values from a database table.
  | LMDBErrBadRangeRead
    -- | A database directory should not exist already.
  | LMDBErrDirExists !FilePath
    -- | A database directory should exist already.
  | LMDBErrDirDoesntExist !FilePath
    -- | The directory exists but is not an LMDB directory!
  | LMDBErrDirIsNotLMDB !FilePath
    -- | What should be a directory is in fact a file
  | LMDBErrNotADir !FS.FsPath
    -- | The database has been closed, so all backing store operations should
    -- throw an error.
  | LMDBErrClosed

instance Exception LMDBErr

-- | Show instance for pretty printing @`LMDBErr`@s as error messages that
-- include: (i) an indication of the probable cause of the error, and
-- (ii) a descriptive error message for the specific @`LMDBErr`@.
instance Show LMDBErr where
  show dbErr = mconcat
      [ "[LMDB-ERROR] "
      , "The LMDB Backing store has encountered a fatal exception. "
      , "Possibly, the LMDB database is corrupted.\n"
      , "[ERROR-MSG] "
      , prettyPrintLMDBErr dbErr
      ]

-- | Pretty print a @`LMDBErr`@ with a descriptive error message.
prettyPrintLMDBErr :: LMDBErr -> String
prettyPrintLMDBErr = \case
  LMDBErrNoDbSeqNo ->
    "Can not find the database state on-disk."
  LMDBErrNonMonotonicSeq s1 s2 ->
    "Trying to write to the database with a non-monotonic sequence number: "
    <> showParen True (shows s1) ""
    <> " is not <= "
    <> showParen True (shows s2) ""
  LMDBErrInitialisingNonEmpty s ->
    "The database table that is being initialised is non-empty: " <> s
  LMDBErrInitialisingAlreadyHasState ->
    "The database contains no values but still has a table with a sequence number."
  LMDBErrNoValueHandle vh_id ->
    "Trying to use non-existing value handle: " <> show vh_id
  LMDBErrUnableToReadSeqNo ->
    "Reading the sequence number failed thus we couldn't create a value handle."
  LMDBErrBadRead ->
    "Failed to read a value from a database table."
  LMDBErrBadRangeRead ->
    "Failed to read a range of values from a database table."
  LMDBErrDirExists path ->
    "Database directory should not exist already: " <> show path
  LMDBErrDirDoesntExist path ->
    "Database directory should already exist: " <> show path
  LMDBErrDirIsNotLMDB path ->
    "Database directory doesn't contain an LMDB database: "
    <> show path
    <> "\nPre-UTxO-HD and In-Memory implementations are incompatible \
       \ with the LMDB implementation, please delete your ledger database \
       \ if you want to run with LMDB"
  LMDBErrNotADir path ->
    "The path " <> show path <> " should be a directory but it is a file instead."
  LMDBErrClosed -> "The database has been closed."
