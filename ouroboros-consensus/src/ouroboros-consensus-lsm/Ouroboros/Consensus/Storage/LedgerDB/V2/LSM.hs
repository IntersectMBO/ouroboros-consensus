{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Implementation of the 'LedgerTablesHandle' interface with LSM trees.
module Ouroboros.Consensus.Storage.LedgerDB.V2.LSM
  ( -- * Backend API
    LSM
  , Backend (..)
  , Args (LSMArgs)
  , mkLSMArgs
  , stdMkBlockIOFS

    -- * Streaming
  , YieldArgs (YieldLSM)
  , mkLSMYieldArgs
  , SinkArgs (SinkLSM)
  , mkLSMSinkArgs

    -- * Exported for tests
  , LSM.Salt
  , SomeHasFSAndBlockIO (..)
  ) where

import Cardano.Binary as CBOR
import Codec.Serialise (decode)
import qualified Control.Monad as Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Fail
import Control.ResourceRegistry
import Control.Tracer
import Data.Bifunctor (first)
import qualified Data.Foldable as Foldable
import Data.Functor.Contravariant ((>$<))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.MemPack
import Data.MemPack.Buffer
import Data.MemPack.Error
import qualified Data.Primitive.ByteArray as PBA
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text as Text
import Data.Typeable
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Primitive as VP
import Data.Void
import Database.LSMTree (Salt, Session, Table)
import qualified Database.LSMTree as LSM
import GHC.Exts
import GHC.Generics
import GHC.Stack
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsProtocol
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Args
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.V2
import Ouroboros.Consensus.Storage.LedgerDB.V2.Backend
import Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory
import Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import Ouroboros.Consensus.Util (chunks)
import Ouroboros.Consensus.Util.CRC
import Ouroboros.Consensus.Util.Enclose
import Ouroboros.Consensus.Util.IOLike hiding (yield)
import Ouroboros.Consensus.Util.IndexedMemPack
import qualified Streaming as S
import qualified Streaming.Prelude as S
import System.FS.API
import qualified System.FS.BlockIO.API as BIO
import System.FS.BlockIO.IO
import System.FS.CRC
import System.FilePath (splitDirectories, splitFileName)
import System.Random
import Prelude hiding (read)

-- | Type alias for convenience
type UTxOTable m = Table m TxInBytes TxOutBytes Void

instance NoThunks (Table m txin txout Void) where
  showTypeOf _ = "Table"
  wNoThunks _ _ = pure Nothing

data LSMClosedExn = LSMClosedExn
  deriving (Show, Exception)

instance Buffer (VP.Vector a) where
  bufferByteCount (VP.Vector _ l _) = l
  buffer (VP.Vector _ _ (PBA.ByteArray barr)) f _ = f barr

{-------------------------------------------------------------------------------
  TxOuts
-------------------------------------------------------------------------------}

-- | Vendored version of unpack which starts from a given initial offset. Could
-- be upstreamed to @mempack@.
unpackWithOffset ::
  forall a b. (MemPack a, Buffer b, HasCallStack) => b -> Int -> Either SomeError a
unpackWithOffset b off = first fromMultipleErrors . runFailAgg $ do
  let len = bufferByteCount b
  (a, consumedBytes) <- do
    res@(_, consumedBytes) <- runStateT (runUnpack unpackM b) off
    Monad.when (consumedBytes > len) $
      error $
        "Potential buffer overflow. Some bug in 'unpackM' was detected while unpacking " <> (typeName @a)
          ++ ". Consumed " <> showBytes (consumedBytes - len) <> " more than allowed from a buffer of length "
          ++ show len
    pure res
  Monad.when (consumedBytes /= len) $
    failT $
      toSomeError $
        NotFullyConsumedError
          { notFullyConsumedRead = consumedBytes
          , notFullyConsumedAvailable = len
          , notFullyConsumedTypeName = (typeName @a)
          }
  pure a
{-# INLINEABLE unpackWithOffset #-}

newtype TxOutBytes = TxOutBytes {unTxOutBytes :: LSM.RawBytes}

toTxOutBytes :: IndexedMemPack (l EmptyMK) (TxOut l) => l EmptyMK -> TxOut l -> TxOutBytes
toTxOutBytes st txout =
  let barr = indexedPackByteArray True st txout
   in TxOutBytes $ LSM.RawBytes (VP.Vector 0 (PBA.sizeofByteArray barr) barr)

fromTxOutBytes :: IndexedMemPack (l EmptyMK) (TxOut l) => l EmptyMK -> TxOutBytes -> TxOut l
fromTxOutBytes st (TxOutBytes (LSM.RawBytes vec@(VP.Vector off _ _))) =
  case indexedUnpackWithOffset st vec off of
    Left err ->
      error $
        unlines
          [ "There was an error deserializing a TxOut from the LSM backend."
          , "This will likely result in a restart-crash loop."
          , "The error: " <> show err
          ]
    Right v -> v

instance LSM.SerialiseValue TxOutBytes where
  serialiseValue = unTxOutBytes
  deserialiseValue = TxOutBytes

deriving via LSM.ResolveAsFirst TxOutBytes instance LSM.ResolveValue TxOutBytes

{-------------------------------------------------------------------------------
  TxIns
-------------------------------------------------------------------------------}

newtype TxInBytes = TxInBytes {unTxInBytes :: LSM.RawBytes}

toTxInBytes :: MemPack (TxIn l) => Proxy l -> TxIn l -> TxInBytes
toTxInBytes _ txin =
  let barr = packByteArray True txin
   in TxInBytes $ LSM.RawBytes (VP.Vector 0 (PBA.sizeofByteArray barr) barr)

fromTxInBytes :: MemPack (TxIn l) => Proxy l -> TxInBytes -> TxIn l
fromTxInBytes _ (TxInBytes (LSM.RawBytes vec@(VP.Vector off _ _))) =
  case unpackWithOffset vec off of
    Left err ->
      error $
        unlines
          [ "There was an error deserializing a TxIn from the LSM backend."
          , "This will likely result in a restart-crash loop."
          , "The error: " <> show err
          ]
    Right v -> v

instance LSM.SerialiseKey TxInBytes where
  serialiseKey = unTxInBytes
  deserialiseKey = TxInBytes

{-------------------------------------------------------------------------------
  Implementation
-------------------------------------------------------------------------------}

-- | Create the initial LSM table from values, which should happen only at the
-- Genesis.
tableFromValuesMK ::
  forall m l.
  (IOLike m, IndexedMemPack (l EmptyMK) (TxOut l), MemPack (TxIn l)) =>
  Tracer m LedgerDBV2Trace ->
  ResourceRegistry m ->
  Session m ->
  l EmptyMK ->
  LedgerTables l ValuesMK ->
  m (ResourceKey m, UTxOTable m)
tableFromValuesMK tracer rr session st (LedgerTables (ValuesMK values)) = do
  res@(_, table) <-
    allocate
      rr
      (\_ -> LSM.newTable session)
      ( \tb -> do
          traceWith tracer TraceLedgerTablesHandleClose
          LSM.closeTable tb
      )
  mapM_ (go table) $ chunks 1000 $ Map.toList values
  pure res
 where
  go table items =
    LSM.inserts table $
      V.fromListN (length items) $
        map (\(k, v) -> (toTxInBytes (Proxy @l) k, toTxOutBytes st v, Nothing)) items

snapshotManager ::
  ( IOLike m
  , LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  ) =>
  Session m ->
  CodecConfig blk ->
  Tracer m (TraceSnapshotEvent blk) ->
  SomeHasFS m ->
  Maybe (NonNativeSnapshotsFS m) ->
  SnapshotManager m m blk (StateRef m (ExtLedgerState blk))
snapshotManager session ccfg tracer fs mNonNative =
  SnapshotManager
    { listSnapshots = defaultListSnapshots fs
    , deleteSnapshot = implDeleteSnapshot session fs tracer
    , takeSnapshot = implTakeSnapshot ccfg tracer fs mNonNative
    }

newLSMLedgerTablesHandle ::
  forall m l.
  ( IOLike m
  , HasLedgerTables l
  , IndexedMemPack (l EmptyMK) (TxOut l)
  ) =>
  Tracer m LedgerDBV2Trace ->
  ResourceRegistry m ->
  (ResourceKey m, UTxOTable m) ->
  m (LedgerTablesHandle m l)
newLSMLedgerTablesHandle tracer rr (resKey, t) = do
  traceWith tracer TraceLedgerTablesHandleCreate
  pure
    LedgerTablesHandle
      { close = do
          Monad.void $ release resKey
      , duplicate = do
          table <-
            allocate
              rr
              (\_ -> LSM.duplicate t)
              ( \t' -> do
                  traceWith tracer TraceLedgerTablesHandleClose
                  LSM.closeTable t'
              )
          newLSMLedgerTablesHandle tracer rr table
      , read = \st (LedgerTables (KeysMK keys)) -> do
          let vec' = V.create $ do
                vec <- VM.new (Set.size keys)
                Monad.foldM_
                  (\i x -> VM.write vec i (toTxInBytes (Proxy @l) x) >> pure (i + 1))
                  0
                  keys
                pure vec
          res <- LSM.lookups t vec'
          pure
            . LedgerTables
            . ValuesMK
            . Foldable.foldl'
              ( \m (k, item) ->
                  case item of
                    LSM.Found v -> Map.insert (fromTxInBytes (Proxy @l) k) (fromTxOutBytes st v) m
                    LSM.NotFound -> m
                    LSM.FoundWithBlob{} -> m
              )
              Map.empty
            $ V.zip vec' res
      , readRange = implReadRange t
      , pushDiffs = const (implPushDiffs t)
      , takeHandleSnapshot = \_ snapshotName -> do
          LSM.saveSnapshot
            (fromString snapshotName)
            (LSM.SnapshotLabel $ Text.pack $ "UTxO table")
            t
          pure Nothing
      , tablesSize = pure Nothing
      }

implReadRange ::
  forall m l.
  (IOLike m, IndexedMemPack (l EmptyMK) (TxOut l)) =>
  HasLedgerTables l =>
  UTxOTable m ->
  l EmptyMK ->
  (Maybe (TxIn l), Int) ->
  m (LedgerTables l ValuesMK, Maybe (TxIn l))
implReadRange table st (mPrev, num) = do
  entries <- maybe cursorFromStart cursorFromKey mPrev
  pure
    ( LedgerTables
        . ValuesMK
        . V.foldl'
          ( \m -> \case
              LSM.Entry k v -> Map.insert (fromTxInBytes (Proxy @l) k) (fromTxOutBytes st v) m
              LSM.EntryWithBlob{} -> m
          )
          Map.empty
        $ entries
    , case snd <$> V.unsnoc entries of
        Nothing -> Nothing
        Just (LSM.Entry k _) -> Just (fromTxInBytes (Proxy @l) k)
        Just (LSM.EntryWithBlob k _ _) -> Just (fromTxInBytes (Proxy @l) k)
    )
 where
  cursorFromStart = LSM.withCursor table (LSM.take num)
  -- Here we ask for one value more and we drop one value because the
  -- cursor returns also the key at which it was opened.
  cursorFromKey k = fmap (V.drop 1) $ LSM.withCursorAtOffset table (toTxInBytes (Proxy @l) k) (LSM.take $ num + 1)

implPushDiffs ::
  forall m l.
  ( IOLike m
  , HasLedgerTables l
  , IndexedMemPack (l EmptyMK) (TxOut l)
  ) =>
  UTxOTable m -> l DiffMK -> m ()
implPushDiffs t !st1 = do
  let LedgerTables (DiffMK (Diff.Diff diffs)) = projectLedgerTables st1
  let vec = V.create $ do
        vec' <- VM.new (Map.size diffs)
        Monad.foldM_
          (\idx (k, item) -> VM.write vec' idx (toTxInBytes (Proxy @l) k, (f item)) >> pure (idx + 1))
          0
          $ Map.toList diffs
        pure vec'
  LSM.updates t vec
 where
  f (Diff.Insert v) = LSM.Insert (toTxOutBytes (forgetLedgerTables st1) v) Nothing
  f Diff.Delete = LSM.Delete

implTakeSnapshot ::
  ( IOLike m
  , LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  ) =>
  CodecConfig blk ->
  Tracer m (TraceSnapshotEvent blk) ->
  SomeHasFS m ->
  Maybe (NonNativeSnapshotsFS m) ->
  Maybe String ->
  StateRef m (ExtLedgerState blk) ->
  m (Maybe (DiskSnapshot, RealPoint blk))
implTakeSnapshot ccfg tracer shfs mNonNativeFS suffix st =
  case pointToWithOriginRealPoint (castPoint (getTip $ state st)) of
    Origin -> return Nothing
    NotOrigin t -> do
      let number = unSlotNo (realPointSlot t)
          snapshot = DiskSnapshot number suffix
      diskSnapshots <- defaultListSnapshots shfs
      if List.any (== DiskSnapshot number suffix) diskSnapshots
        then
          return Nothing
        else do
          stateCRC <-
            encloseTimedWith (TookSnapshot snapshot t >$< tracer) $
              writeSnapshot shfs (encodeDiskExtLedgerState ccfg) snapshot st
          takeNonNativeSnapshot
            (($ t) >$< tracer)
            snapshot
            (duplicate (tables st))
            close
            (\hdl -> yield (Proxy @LSM) (YieldLSM 1000 hdl) (state st))
            (state st)
            stateCRC
            mNonNativeFS

          return $ Just (snapshot, t)

writeSnapshot ::
  MonadThrow m =>
  SomeHasFS m ->
  (ExtLedgerState blk EmptyMK -> Encoding) ->
  DiskSnapshot ->
  StateRef m (ExtLedgerState blk) ->
  m CRC
writeSnapshot fs@(SomeHasFS hasFs) encLedger ds st = do
  createDirectoryIfMissing hasFs True $ snapshotToDirPath ds
  crc1 <- writeExtLedgerState fs encLedger (snapshotToStatePath ds) $ state st
  crc2 <- takeHandleSnapshot (tables st) (state st) $ snapshotToDirName ds
  writeSnapshotMetadata fs ds $
    SnapshotMetadata
      { snapshotBackend = UTxOHDLSMSnapshot
      , snapshotChecksum = maybe crc1 (crcOfConcat crc1) crc2
      , snapshotTablesCodecVersion = TablesCodecVersion1
      }
  pure crc1

-- | Delete snapshot from disk and also from the LSM tree database.
implDeleteSnapshot ::
  IOLike m => Session m -> SomeHasFS m -> Tracer m (TraceSnapshotEvent blk) -> DiskSnapshot -> m ()
implDeleteSnapshot session (SomeHasFS HasFS{doesDirectoryExist, removeDirectoryRecursive}) tracer ss = do
  deleteState `finally` deleteLsmTable
  traceWith tracer (DeletedSnapshot ss)
 where
  deleteState = do
    let p = snapshotToDirPath ss
    exists <- doesDirectoryExist p
    Monad.when exists (removeDirectoryRecursive p)

  deleteLsmTable =
    LSM.deleteSnapshot
      session
      (fromString $ show (dsNumber ss) <> maybe "" ("_" <>) (dsSuffix ss))

-- | Read snapshot from disk.
--
--   Fail on data corruption, i.e. when the checksum of the read data differs
--   from the one tracked by @'DiskSnapshot'@.
loadSnapshot ::
  forall blk m.
  ( LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  , IOLike m
  ) =>
  Tracer m LedgerDBV2Trace ->
  ResourceRegistry m ->
  CodecConfig blk ->
  SomeHasFS m ->
  Session m ->
  DiskSnapshot ->
  ExceptT (SnapshotFailure blk) m (LedgerSeq' m blk, RealPoint blk)
loadSnapshot tracer rr ccfg fs session ds = do
  snapshotMeta <-
    withExceptT (InitFailureRead . ReadMetadataError (snapshotToMetadataPath ds)) $
      loadSnapshotMetadata fs ds
  Monad.when (snapshotBackend snapshotMeta /= UTxOHDLSMSnapshot) $
    throwE $
      InitFailureRead $
        ReadMetadataError (snapshotToMetadataPath ds) MetadataBackendMismatch
  (extLedgerSt, checksumAsRead) <-
    withExceptT
      (InitFailureRead . ReadSnapshotFailed)
      $ readExtLedgerState fs (decodeDiskExtLedgerState ccfg) decode (snapshotToStatePath ds)
  case pointToWithOriginRealPoint (castPoint (getTip extLedgerSt)) of
    Origin -> throwE InitFailureGenesis
    NotOrigin pt -> do
      values <-
        lift $
          allocate
            rr
            ( \_ ->
                LSM.openTableFromSnapshot
                  session
                  (fromString $ snapshotToDirName ds)
                  (LSM.SnapshotLabel $ Text.pack $ "UTxO table")
            )
            ( \t -> do
                traceWith tracer TraceLedgerTablesHandleClose
                LSM.closeTable t
            )
      Monad.when (checksumAsRead /= snapshotChecksum snapshotMeta) $
        throwE $
          InitFailureRead ReadSnapshotDataCorruption
      (,pt) <$> lift (empty extLedgerSt values (newLSMLedgerTablesHandle tracer rr))

stdMkBlockIOFS ::
  FilePath -> ResourceRegistry IO -> IO (ResourceKey IO, SomeHasFSAndBlockIO IO)
stdMkBlockIOFS fastStoragePath rr = do
  (rk1, bio) <-
    allocate
      rr
      (\_ -> ioHasBlockIO (MountPoint fastStoragePath) defaultIOCtxParams)
      (BIO.close . snd)
  pure (rk1, uncurry SomeHasFSAndBlockIO bio)

{-------------------------------------------------------------------------------
  Backend
-------------------------------------------------------------------------------}

data LSM

-- | Create arguments for initializing the LedgerDB using the LSM-trees backend.
mkLSMArgs ::
  ( LedgerSupportsProtocol blk
  , LedgerDbSerialiseConstraints blk
  ) =>
  Proxy blk -> FilePath -> FilePath -> StdGen -> (LedgerDbBackendArgs IO blk, StdGen)
mkLSMArgs _ fp fastStorage gen =
  let (lsmSalt, gen') = genWord64 gen
   in ( LedgerDbBackendArgsV2 $
          SomeBackendArgs $
            LSMArgs (mkFsPath $ splitDirectories fp) lsmSalt (stdMkBlockIOFS fastStorage)
      , gen'
      )

instance
  ( LedgerSupportsProtocol blk
  , IOLike m
  , LedgerDbSerialiseConstraints blk
  , HasLedgerTables (LedgerState blk)
  ) =>
  Backend m LSM blk
  where
  data Args m LSM
    = LSMArgs
        FsPath
        -- \^ The file path relative to the fast storage directory in which the LSM
        -- trees database will be located.
        Salt
        (ResourceRegistry m -> m (ResourceKey m, SomeHasFSAndBlockIO m))

  data Resources m LSM = LSMResources
    { sessionKey :: !(ResourceKey m)
    , sessionResource :: !(Session m)
    , blockIOKey :: !(ResourceKey m)
    }
    deriving Generic

  data Trace m LSM
    = LSMTreeTrace !LSM.LSMTreeTrace
    deriving Show

  mkResources _ trcr (LSMArgs path salt mkFS) reg _ = do
    (rk1, SomeHasFSAndBlockIO fs blockio) <- mkFS reg
    session <-
      allocate
        reg
        ( \_ ->
            LSM.openSession
              (BackendTrace . SomeBackendTrace . LSMTreeTrace >$< trcr)
              fs
              blockio
              salt
              path
        )
        LSM.closeSession
    pure (LSMResources (fst session) (snd session) rk1)

  releaseResources _ l = do
    Monad.void . release . sessionKey $ l
    Monad.void . release . blockIOKey $ l

  newHandleFromSnapshot trcr reg ccfg shfs res ds = do
    loadSnapshot trcr reg ccfg shfs (sessionResource res) ds

  newHandleFromValues trcr reg res st = do
    table <-
      tableFromValuesMK trcr reg (sessionResource res) (forgetLedgerTables st) (ltprj st)
    newLSMLedgerTablesHandle trcr reg table

  snapshotManager _ res = Ouroboros.Consensus.Storage.LedgerDB.V2.LSM.snapshotManager (sessionResource res)

instance
  ( MemPack (TxIn l)
  , IndexedMemPack (l EmptyMK) (TxOut l)
  , IOLike m
  ) =>
  StreamingBackend m LSM l
  where
  data YieldArgs m LSM l
    = -- \| Yield an LSM snapshot
      YieldLSM
        Int
        (LedgerTablesHandle m l)

  data SinkArgs m LSM l
    = SinkLSM
        -- \| Chunk size
        Int
        -- \| Snap name
        String
        (Session m)

  yield _ (YieldLSM chunkSize hdl) = yieldLsmS chunkSize hdl

  sink _ (SinkLSM chunkSize snapName session) = sinkLsmS chunkSize snapName session

data SomeHasFSAndBlockIO m where
  SomeHasFSAndBlockIO ::
    (Eq h, Typeable h) => HasFS m h -> BIO.HasBlockIO m h -> SomeHasFSAndBlockIO m

instance IOLike m => NoThunks (Resources m LSM) where
  wNoThunks ctxt (LSMResources sk _ bk) = wNoThunks ctxt sk >> wNoThunks ctxt bk

{-------------------------------------------------------------------------------
  Streaming
-------------------------------------------------------------------------------}

yieldLsmS ::
  Monad m =>
  Int ->
  LedgerTablesHandle m l ->
  Yield m l
yieldLsmS readChunkSize tb hint k = do
  r <- k (go (Nothing, readChunkSize))
  lift $ S.effects r
 where
  go p = do
    (LedgerTables (ValuesMK values), mx) <- lift $ S.lift $ readRange tb hint p
    if Map.null values
      then pure $ pure Nothing
      else do
        S.each $ Map.toList values
        go (mx, readChunkSize)

sinkLsmS ::
  forall m l.
  ( MonadAsync m
  , MonadMVar m
  , MonadThrow (STM m)
  , MonadMask m
  , MonadST m
  , MonadEvaluate m
  , MemPack (TxIn l)
  , IndexedMemPack (l EmptyMK) (TxOut l)
  ) =>
  Int ->
  String ->
  Session m ->
  Sink m l
sinkLsmS writeChunkSize snapName session st s = do
  tb :: UTxOTable m <- lift $ LSM.newTable session
  r <- go tb writeChunkSize mempty s
  lift $
    LSM.saveSnapshot
      (LSM.toSnapshotName snapName)
      (LSM.SnapshotLabel $ T.pack "UTxO table")
      tb
  lift $ LSM.closeTable tb
  pure (fmap (,Nothing) r)
 where
  go tb 0 m s' = do
    lift $
      LSM.inserts tb $
        V.fromList [(toTxInBytes (Proxy @l) k, toTxOutBytes st v, Nothing) | (k, v) <- m]
    go tb writeChunkSize mempty s'
  go tb n m s' = do
    mbs <- S.uncons s'
    case mbs of
      Nothing -> do
        lift $
          LSM.inserts tb $
            V.fromList
              [(toTxInBytes (Proxy @l) k, toTxOutBytes st v, Nothing) | (k, v) <- m]
        S.effects s'
      Just (item, s'') -> go tb (n - 1) (item : m) s''

-- | Create Yield arguments for LSM
mkLSMYieldArgs ::
  ( IOLike m
  , HasLedgerTables l
  , IndexedMemPack (l EmptyMK) (TxOut l)
  ) =>
  -- | The filepath in which the LSM database lives. Must not have a trailing slash!
  FilePath ->
  -- | The complete name of the snapshot to open, so @<slotno>[_<suffix>]@.
  String ->
  -- | Usually 'stdMkBlockIOFS'
  (FilePath -> ResourceRegistry m -> m (a, SomeHasFSAndBlockIO m)) ->
  -- | Usually 'newStdGen'
  (m StdGen) ->
  l EmptyMK ->
  ResourceRegistry m ->
  m (YieldArgs m LSM l)
mkLSMYieldArgs fp snapName mkFS mkGen _ reg = do
  (_, SomeHasFSAndBlockIO hasFS blockIO) <- mkFS fp reg
  salt <- fst . genWord64 <$> mkGen
  (_, session) <-
    allocate reg (\_ -> LSM.openSession nullTracer hasFS blockIO salt (mkFsPath [])) LSM.closeSession
  tb <-
    allocate
      reg
      ( \_ ->
          LSM.openTableFromSnapshot
            session
            (LSM.toSnapshotName snapName)
            (LSM.SnapshotLabel $ T.pack "UTxO table")
      )
      LSM.closeTable
  YieldLSM 1000 <$> newLSMLedgerTablesHandle nullTracer reg tb

-- | Create Sink arguments for LSM
mkLSMSinkArgs ::
  IOLike m =>
  -- | The filepath in which the LSM database should be opened. Must not have a trailing slash!
  FilePath ->
  -- | The complete name of the snapshot to be created, so @<slotno>[_<suffix>]@.
  String ->
  -- | Usually 'stdMkBlockIOFS'
  (FilePath -> ResourceRegistry m -> m (a, SomeHasFSAndBlockIO m)) ->
  -- | Usually 'newStdGen'
  (m StdGen) ->
  l EmptyMK ->
  ResourceRegistry m ->
  m (SinkArgs m LSM l)
mkLSMSinkArgs
  (splitFileName -> (fp, lsmDir))
  snapName
  mkFS
  mkGen
  _
  reg =
    do
      (_, SomeHasFSAndBlockIO hasFS blockIO) <- mkFS fp reg
      removeDirectoryRecursive hasFS lsmFsPath
      createDirectory hasFS lsmFsPath
      salt <- fst . genWord64 <$> mkGen
      (_, session) <-
        allocate reg (\_ -> LSM.newSession nullTracer hasFS blockIO salt lsmFsPath) LSM.closeSession
      pure (SinkLSM 1000 snapName session)
   where
    lsmFsPath = mkFsPath [lsmDir]
