{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | A 'BackingStore' implementation based on [LMDB](http://www.lmdb.tech/doc/).
module Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB (
    -- * Opening a database
    LMDBLimits (LMDBLimits, lmdbMapSize, lmdbMaxDatabases, lmdbMaxReaders)
  , newLMDBBackingStore
    -- * Errors
  , LMDBErr (..)
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
import           Data.Monoid (Sum (..))
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
import qualified Ouroboros.Consensus.Ledger.Tables.Diffs as Diffs
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
  , dbState         :: !(LMDB.Database () DbState)
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
--    1 for the database state @'DbState'@.
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
newtype DbState = DbState {
    dbsSeq :: WithOrigin SlotNo
  }
  deriving stock (Show, Generic)
  deriving anyclass S.Serialise

-- | A 'MapKind' that represents an LMDB database
data LMDBMK k v = LMDBMK String !(LMDB.Database k v)

{-------------------------------------------------------------------------------
  Low-level API
-------------------------------------------------------------------------------}

getDb ::
     LMDB.Internal.IsMode mode
  => K2 String k v
  -> LMDB.Transaction mode (LMDBMK k v)
getDb (K2 name) = LMDBMK name <$> LMDB.getDatabase (Just name)

-- | @'rangeRead' n db codec ksMay@ performs a range read of @count@ values from
-- database @db@, starting from some key depending on @ksMay@.
--
-- The @codec@ argument defines how to serialise/deserialise keys and values.
--
-- A range read can return less than @count@ values if there are not enough
-- values to read.
--
-- Note: See @`RangeQuery`@ for more information about range queries. In
-- particular, @'rqPrev'@ describes the role of @ksMay@.
--
-- What the "first" key in the database is, and more generally in which order
-- keys are read, depends on the lexographical ordering of the /serialised/
-- keys. Care should be taken such that the @'Ord'@ instance for @k@ matches the
-- lexicographical ordering of the serialised keys, or the result of this
-- function will be unexpected.
rangeRead ::
     forall k v mode. Ord k
  => Int
  -> LMDBMK k v
  -> CodecMK k v
  -> (Maybe :..: KeysMK) k v
  -> LMDB.Transaction mode (ValuesMK k v)
rangeRead count dbMK codecMK ksMK =
    ValuesMK <$> case unComp2 ksMK of
      Nothing -> runCursorHelper Nothing
      Just (KeysMK ks) -> case Set.lookupMax ks of
        Nothing -> pure mempty
        Just lastExcludedKey ->
          runCursorHelper $ Just (lastExcludedKey, LMDB.Cursor.Exclusive)
  where
    LMDBMK _ db = dbMK

    runCursorHelper ::
         Maybe (k, LMDB.Cursor.Bound)    -- ^ Lower bound on read range
      -> LMDB.Transaction mode (Map k v)
    runCursorHelper lb =
      Bridge.runCursorAsTransaction'
        (LMDB.Cursor.cgetMany lb count)
        db
        codecMK

initLMDBTable ::
     LMDBMK   k v
  -> CodecMK  k v
  -> ValuesMK k v
  -> LMDB.Transaction LMDB.ReadWrite (EmptyMK k v)
initLMDBTable (LMDBMK tblName db) codecMK (ValuesMK utxoVals) =
    EmptyMK <$ lmdbInitTable
  where
    lmdbInitTable  = do
      isEmpty <- LMDB.null db
      unless isEmpty $ liftIO . throwIO $ LMDBErrInitialisingNonEmpty tblName
      void $ Map.traverseWithKey
                 (Bridge.put codecMK db)
                 utxoVals

readLMDBTable ::
     Ord k
  => LMDBMK  k v
  -> CodecMK k v
  -> KeysMK  k v
  -> LMDB.Transaction mode (ValuesMK k v)
readLMDBTable (LMDBMK _ db) codecMK (KeysMK keys) =
    ValuesMK <$> lmdbReadTable
  where
    lmdbReadTable = foldlM' go Map.empty (Set.toList keys)
      where
        go m k = Bridge.get codecMK db k <&> \case
          Nothing -> m
          Just v  -> Map.insert k v m

writeLMDBTable ::
     LMDBMK  k v
  -> CodecMK k v
  -> DiffMK  k v
  -> LMDB.Transaction LMDB.ReadWrite (EmptyMK k v)
writeLMDBTable (LMDBMK _ db) codecMK (DiffMK d) =
    EmptyMK <$ lmdbWriteTable
  where
    lmdbWriteTable = void $ Diffs.traverseDeltaWithKey_ go d
      where
        go k de = case de of
          Diffs.Delete   -> void $ Bridge.delete codecMK db k
          Diffs.Insert v -> Bridge.put codecMK db k v

{-------------------------------------------------------------------------------
 Db state
-------------------------------------------------------------------------------}

readDbStateMaybeNull ::
     LMDB.Database () DbState
  -> LMDB.Transaction mode (Maybe DbState)
readDbStateMaybeNull db = LMDB.get db ()

readDbState ::
     LMDB.Database () DbState
  -> LMDB.Transaction mode DbState
readDbState db = readDbStateMaybeNull db >>= maybe (liftIO . throwIO $ LMDBErrNoDbState) pure

withDbStateRW ::
     LMDB.Database () DbState
  -> (DbState -> LMDB.Transaction LMDB.ReadWrite (a, DbState))
  -> LMDB.Transaction LMDB.ReadWrite a
withDbStateRW db f = withDbStateRWMaybeNull db $ maybe (liftIO . throwIO $ LMDBErrNoDbState) f

withDbStateRWMaybeNull ::
      LMDB.Database () DbState
   -> (Maybe DbState -> LMDB.Transaction LMDB.ReadWrite (a, DbState))
   -> LMDB.Transaction LMDB.ReadWrite a
withDbStateRWMaybeNull db f  =
  readDbStateMaybeNull db >>= f >>= \(r, sNew) -> LMDB.put db () (Just sNew) $> r

{-------------------------------------------------------------------------------
 Guards
-------------------------------------------------------------------------------}

data GuardDbDir  = DirMustExist | DirMustNotExist

-- | Guard for the existence/non-existence of a database directory,
-- and create it if missing.
guardDbDir ::
     (MonadIO m, IOLike m)
  => GuardDbDir
  -> FS.SomeHasFS m
  -> FS.FsPath
  -> m FilePath
guardDbDir mustExistDir (FS.SomeHasFS fs) path = do
  fileEx <- FS.doesFileExist fs path
  when fileEx $
    throwIO $ LMDBErrNotADir path
  dirEx <- FS.doesDirectoryExist fs path
  lmdbFileExists <- FS.doesFileExist fs path { FS.fsPathToList = FS.fsPathToList path ++ [Strict.pack "data.mdb"] }
  filepath <- FS.unsafeToFilePath fs path
  case dirEx of
    True  | DirMustNotExist <- mustExistDir -> throwIO $ LMDBErrDirExists filepath
          | not lmdbFileExists              -> throwIO $ LMDBErrDirIsNotLMDB filepath
    False | DirMustExist    <- mustExistDir -> throwIO $ LMDBErrDirDoesntExist filepath
    _                              -> pure ()
  FS.createDirectoryIfMissing fs True path
  pure filepath

-- | Same as @`guardDbDir`@, but retries the guard if we can make meaningful
-- changes to the filesystem before we perform the retry.
--
-- Note: We only retry if a database directory exists while it shoudn't. In
-- this case, we remove the directory recursively before retrying the guard.
-- This is necessary for initialisation of the LMDB backing store, since the
-- (non-snapshot) tables will probably still be on-disk. These tables are not
-- removed when stopping the node, so they should be "overwritten".
guardDbDirWithRetry ::
     (MonadIO m, IOLike m)
  => GuardDbDir
  -> FS.SomeHasFS m
  -> FS.FsPath
  -> m FilePath
guardDbDirWithRetry gdd shfs@(FS.SomeHasFS fs) path =
    handle retryHandler (guardDbDir gdd shfs path)
  where
    retryHandler e = case (gdd, e) of
      (DirMustNotExist, LMDBErrDirExists _path) -> do
        FS.removeDirectoryRecursive fs path
        guardDbDir DirMustNotExist shfs path
      _ -> throwIO e

{-------------------------------------------------------------------------------
 Initialize an LMDB
-------------------------------------------------------------------------------}

-- | Initialise an LMDB database from these provided values.
initFromVals ::
     (HasLedgerTables l, CanSerializeLedgerTables l, MonadIO m)
  => Trace.Tracer m API.BackingStoreTrace
  -> WithOrigin SlotNo
     -- ^ The slot number up to which the ledger tables contain values.
  -> LedgerTables l ValuesMK
     -- ^ The ledger tables to initialise the LMDB database tables with.
  -> LMDB.Environment LMDB.Internal.ReadWrite
     -- ^ The LMDB environment.
  -> LMDB.Database () DbState
     -- ^ The state of the tables we are going to initialize the db with.
  -> LedgerTables l LMDBMK
  -> m ()
initFromVals tracer dbsSeq vals env st backingTables = do
  Trace.traceWith tracer $ API.BSInitialisingFromValues dbsSeq
  liftIO $ LMDB.readWriteTransaction env $
    withDbStateRWMaybeNull st $ \case
      Nothing -> ltzipWith3A initLMDBTable backingTables codecLedgerTables vals
                 $> ((), DbState{dbsSeq})
      Just _ -> liftIO . throwIO $ LMDBErrInitialisingAlreadyHasState
  Trace.traceWith tracer $ API.BSInitialisedFromValues dbsSeq

-- | Initialise an LMDB database from an existing LMDB database.
initFromLMDBs ::
     (MonadIO m, IOLike m)
  => Trace.Tracer m API.BackingStoreTrace
  -> LMDBLimits
     -- ^ Configuration for the LMDB database that we initialise from.
  -> FS.SomeHasFS m
     -- ^ Abstraction over the filesystem.
  -> FS.SomeHasFS m
     -- ^ Abstraction over the filesystem.
  -> FS.FsPath
     -- ^ The path that contains the LMDB database that we want to initialise from.
  -> FS.FsPath
     -- ^ The path where the new LMDB database should be initialised.
  -> m ()
initFromLMDBs tracer limits ssdhfs shfs@(FS.SomeHasFS fs) from0 to0 = do
    Trace.traceWith tracer $ API.BSInitialisingFromCopy from0
    from <- guardDbDir DirMustExist shfs from0
    -- On Windows, if we don't choose the mapsize carefully it will make the
    -- snapshot grow. Therefore we are using the current filesize as mapsize
    -- when opening the snapshot to avoid this.
    stat <- FS.withFile fs (from0 { FS.fsPathToList = FS.fsPathToList from0 ++ [Strict.pack "data.mdb"] }) FS.ReadMode (FS.hGetSize fs)
    to <- guardDbDirWithRetry DirMustNotExist ssdhfs to0
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
     forall m l. (HasCallStack, HasLedgerTables l, CanSerializeLedgerTables l, MonadIO m, IOLike m)
  => Trace.Tracer m API.BackingStoreTrace
  -> LMDBLimits
     -- ^ Configuration parameters for the LMDB database that we
     -- initialise. In case we initialise the LMDB database from
     -- an existing LMDB database, we use these same configuration parameters
     -- to open the existing LMDB database.
  -> FS.SomeHasFS m
  -> FS.SomeHasFS m
  -> API.InitFrom (LedgerTables l ValuesMK)
  -> m (API.LedgerBackingStore m l)
newLMDBBackingStore dbTracer limits ssdfs snapfs initFrom = do
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
     dbFilePath <- guardDbDirWithRetry DirMustNotExist ssdfs path

     -- copy from another lmdb path if appropriate
     case initFrom of
       API.InitFromCopy fp -> initFromLMDBs dbTracer limits ssdfs snapfs fp path
       _                   -> pure ()

     -- open this database
     dbEnv <- liftIO $ LMDB.openEnvironment dbFilePath (unLMDBLimits limits)

     -- The LMDB.Database that holds the @`DbState`@ (i.e. sequence number)
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
                 -> LMDB.Internal.Database () DbState
                 -> LedgerTables l LMDBMK
                 -> m ()
   maybePopulate dbEnv dbState dbBackingTables = do
     -- now initialise those tables if appropriate
     case initFrom of
       API.InitFromValues slot vals -> initFromVals dbTracer slot vals dbEnv dbState dbBackingTables
       _                            -> pure ()

   mkBackingStore :: HasCallStack => Db m l -> API.LedgerBackingStore m l
   mkBackingStore db =
       let bsClose :: m ()
           bsClose = Status.withWriteAccess' dbStatusLock traceAlreadyClosed $ do
             Trace.traceWith dbTracer API.BSClosing
             openHandles <- IOLike.readTVarIO dbOpenHandles
             forM_ openHandles runCleanup
             IOLike.atomically $ IOLike.writeTVar dbOpenHandles mempty
             liftIO $ LMDB.closeEnvironment dbEnv
             Trace.traceWith dbTracer API.BSClosed
             pure (Closed, ())
            where
              traceAlreadyClosed = Trace.traceWith dbTracer API.BSAlreadyClosed

           bsCopy bsp = Status.withReadAccess dbStatusLock LMDBErrClosed $ do
             to <- guardDbDir DirMustNotExist snapfs bsp
             lmdbCopy path dbTracer dbEnv to

           bsValueHandle = Status.withReadAccess dbStatusLock LMDBErrClosed $ do
             mkLMDBBackingStoreValueHandle db

           bsWrite :: SlotNo -> LedgerTables l DiffMK -> m ()
           bsWrite slot diffs = do
             Trace.traceWith dbTracer $ API.BSWriting slot
             Status.withReadAccess dbStatusLock LMDBErrClosed $ do
               oldSlot <- liftIO $ LMDB.readWriteTransaction dbEnv $ withDbStateRW dbState $ \s@DbState{dbsSeq} -> do
                 unless (dbsSeq <= At slot) $ liftIO . throwIO $ LMDBErrNonMonotonicSeq (At slot) dbsSeq
                 void $ ltzipWith3A writeLMDBTable dbBackingTables codecLedgerTables diffs
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
-- current database state (i.e., the database contents, not to be confused
-- with 'DbState').
mkLMDBBackingStoreValueHandle ::
     forall l m.
     (HasLedgerTables l, CanSerializeLedgerTables l, MonadIO m, IOLike m, HasCallStack)
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
  mbInitSlot <- liftIO $ TrH.submitReadOnly trh $ readDbStateMaybeNull dbState
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
      Status.withReadAccess' dbStatusLock traceAlreadyClosed $ do
      Status.withWriteAccess' vhStatusLock traceTVHAlreadyClosed $ do
        Trace.traceWith tracer API.BSVHClosing
        runCleanup cleanup
        IOLike.atomically $ IOLike.modifyTVar' dbOpenHandles (Map.delete vhId)
        Trace.traceWith tracer API.BSVHClosed
        pure (Closed, ())
      where
        traceAlreadyClosed    = Trace.traceWith dbTracer API.BSAlreadyClosed
        traceTVHAlreadyClosed = Trace.traceWith tracer API.BSVHAlreadyClosed

    bsvhRead :: LedgerTables l KeysMK -> m (LedgerTables l ValuesMK)
    bsvhRead keys =
      Status.withReadAccess dbStatusLock LMDBErrClosed $ do
      Status.withReadAccess vhStatusLock (LMDBErrNoValueHandle vhId) $ do
        Trace.traceWith tracer API.BSVHReading
        res <- liftIO $ TrH.submitReadOnly trh (ltzipWith3A readLMDBTable dbBackingTables codecLedgerTables keys)
        Trace.traceWith tracer API.BSVHRead
        pure res

    bsvhRangeRead ::
         API.RangeQuery (LedgerTables l KeysMK)
      -> m (LedgerTables l ValuesMK)
    bsvhRangeRead rq =
      Status.withReadAccess dbStatusLock LMDBErrClosed $ do
      Status.withReadAccess vhStatusLock (LMDBErrNoValueHandle vhId) $ do
        Trace.traceWith tracer API.BSVHRangeReading

        let
          outsideIn ::
              Maybe (LedgerTables l mk1)
            -> LedgerTables l (Maybe :..: mk1)
          outsideIn Nothing       = ltpure (Comp2 Nothing)
          outsideIn (Just tables) = ltmap (Comp2 . Just) tables

          transaction =
            ltzipWith3A
              (rangeRead rqCount)
              dbBackingTables
              codecLedgerTables
              (outsideIn rqPrev)

        res <- liftIO $ TrH.submitReadOnly trh transaction
        Trace.traceWith tracer API.BSVHRangeRead
        pure res
     where
      API.RangeQuery rqPrev rqCount = rq

    bsvhStat :: m API.Statistics
    bsvhStat =
      Status.withReadAccess dbStatusLock LMDBErrClosed $ do
      Status.withReadAccess vhStatusLock (LMDBErrNoValueHandle vhId) $ do
        Trace.traceWith tracer API.BSVHStatting
        let transaction = do
              DbState{dbsSeq} <- readDbState dbState
              constn <- lttraverse (\(LMDBMK _ dbx) -> K2 <$> LMDB.size dbx) dbBackingTables
              let n = getSum $ ltcollapse $ ltmap (K2 . Sum . unK2) constn
              pure $ API.Statistics dbsSeq n
        res <- liftIO $ TrH.submitReadOnly trh transaction
        Trace.traceWith tracer API.BSVHStatted
        pure res

    bsvh = API.BackingStoreValueHandle { API.bsvhAtSlot = initSlot
                                      , API.bsvhClose = bsvhClose
                                      , API.bsvhRead = bsvhRead
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
    LMDBErrNoDbState
    -- | The sequence number of a @`Db`@ should be monotonically increasing
    -- across calls to @`bsWrite`@, since we use @`bsWrite`@ to flush
    -- /immutable/ changes. That is, we can only flush with a newer sequence
    -- number because the changes should be /immutable/. Note that this does
    -- not mean that values can not be changed in the future, only that we
    -- can not change values in the past.
  | LMDBErrNonMonotonicSeq !(WithOrigin SlotNo) !(WithOrigin SlotNo)
    -- | The database table that is being initialised is non-empty.
  | LMDBErrInitialisingNonEmpty !String
    -- | The database that is being initialized already had a DbState table
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
  LMDBErrNoDbState ->
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
