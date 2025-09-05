{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory
  ( -- * API
    Backend (..)
  , Args (InMemArgs)
  , Mem
  , YieldArgs (YieldInMemory)
  , SinkArgs (SinkInMemory)
  , mkInMemoryArgs

    -- * Canonical snapshots
  , takeCanonicalSnapshot
  ) where

import Cardano.Binary as CBOR
import Cardano.Slotting.Slot
import Codec.CBOR.Read
import qualified Codec.CBOR.Write as CBOR
import Codec.Serialise (decode)
import Control.Monad (replicateM_, unless)
import qualified Control.Monad as Monad
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Except
import Control.Monad.State.Strict (execStateT)
import Control.Monad.Trans.Except
import Control.ResourceRegistry
import Control.Tracer
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Data.Functor.Contravariant ((>$<))
import Data.Functor.Identity
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.MemPack
import qualified Data.Primitive.ByteArray as PBA
import Data.Void
import GHC.Generics
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
import Ouroboros.Consensus.Storage.LedgerDB.V2.Backend
import Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import Ouroboros.Consensus.Util (whenJust)
import Ouroboros.Consensus.Util.CBOR (readIncremental)
import Ouroboros.Consensus.Util.CRC
import Ouroboros.Consensus.Util.Enclose
import Ouroboros.Consensus.Util.IOLike
import Streaming
import qualified Streaming as S
import qualified Streaming.Prelude as S
import System.FS.API
import System.FS.CRC
import qualified System.FilePath as F
import Prelude hiding (read)

{-------------------------------------------------------------------------------
  InMemory implementation of LedgerTablesHandles
-------------------------------------------------------------------------------}

data LedgerTablesHandleState l
  = LedgerTablesHandleOpen !(LedgerTables l ValuesMK)
  | LedgerTablesHandleClosed
  deriving Generic

deriving instance NoThunks (LedgerTables l ValuesMK) => NoThunks (LedgerTablesHandleState l)

data InMemoryClosedExn = InMemoryClosedExn
  deriving (Show, Exception)

guardClosed :: LedgerTablesHandleState l -> (LedgerTables l ValuesMK -> a) -> a
guardClosed LedgerTablesHandleClosed _ = error $ show InMemoryClosedExn
guardClosed (LedgerTablesHandleOpen st) f = f st

newInMemoryLedgerTablesHandle ::
  forall m l.
  ( IOLike m
  , HasLedgerTables l
  , CanUpgradeLedgerTables l
  , SerializeTablesWithHint l
  ) =>
  Tracer m LedgerDBV2Trace ->
  SomeHasFS m ->
  LedgerTables l ValuesMK ->
  m (LedgerTablesHandle m l)
newInMemoryLedgerTablesHandle tracer someFS@(SomeHasFS hasFS) l = do
  !tv <- newTVarIO (LedgerTablesHandleOpen l)
  traceWith tracer TraceLedgerTablesHandleCreate
  pure
    LedgerTablesHandle
      { close = do
          p <- atomically $ swapTVar tv LedgerTablesHandleClosed
          case p of
            LedgerTablesHandleOpen{} -> traceWith tracer TraceLedgerTablesHandleClose
            _ -> pure ()
      , duplicate = do
          hs <- readTVarIO tv
          !x <- guardClosed hs $ newInMemoryLedgerTablesHandle tracer someFS
          pure x
      , read = \_ keys -> do
          hs <- readTVarIO tv
          guardClosed
            hs
            (pure . flip (ltliftA2 (\(ValuesMK v) (KeysMK k) -> ValuesMK $ v `Map.restrictKeys` k)) keys)
      , readRange = \_ (f, t) -> do
          hs <- readTVarIO tv
          guardClosed
            hs
            ( \(LedgerTables (ValuesMK m)) ->
                let m' = Map.take t . (maybe id (\g -> snd . Map.split g) f) $ m
                 in pure (LedgerTables (ValuesMK m'), fst <$> Map.lookupMax m')
            )
      , pushDiffs = \st0 !diffs ->
          atomically $
            modifyTVar
              tv
              ( \r ->
                  guardClosed
                    r
                    ( LedgerTablesHandleOpen
                        . flip
                          (ltliftA2 (\(ValuesMK vals) (DiffMK d) -> ValuesMK (Diff.applyDiff vals d)))
                          (projectLedgerTables diffs)
                        . upgradeTables st0 diffs
                    )
              )
      , takeHandleSnapshot = \hint snapshotName -> do
          h <- readTVarIO tv
          guardClosed h $
            \values ->
              withFile hasFS (mkFsPath [snapshotName, "tables"]) (WriteMode MustBeNew) $ \hf ->
                fmap (Just . snd) $
                  hPutAllCRC hasFS hf $
                    CBOR.toLazyByteString $
                      valuesMKEncoder hint values
      , tablesSize = do
          hs <- readTVarIO tv
          guardClosed hs (pure . Just . Map.size . getValuesMK . getLedgerTables)
      }

{-------------------------------------------------------------------------------
  Snapshots
-------------------------------------------------------------------------------}

snapshotManager ::
  ( IOLike m
  , LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  ) =>
  CodecConfig blk ->
  Tracer m (TraceSnapshotEvent blk) ->
  SomeHasFS m ->
  Maybe (CanonicalSnapshotsFS m) ->
  SnapshotManager m m blk (StateRef m (ExtLedgerState blk))
snapshotManager ccfg tracer fs mCanonical =
  SnapshotManager
    { listSnapshots = defaultListSnapshots fs
    , deleteSnapshot = defaultDeleteSnapshot fs tracer
    , takeSnapshot = implTakeSnapshot ccfg tracer fs mCanonical
    }

-- | The path within the LedgerDB's filesystem to the file that contains the
-- snapshot's serialized ledger state
snapshotToStatePath :: DiskSnapshot -> FsPath
snapshotToStatePath = mkFsPath . (\x -> [x, "state"]) . snapshotToDirName

writeSnapshot ::
  MonadThrow m =>
  SomeHasFS m ->
  (ExtLedgerState blk EmptyMK -> Encoding) ->
  DiskSnapshot ->
  StateRef m (ExtLedgerState blk) ->
  m ()
writeSnapshot fs@(SomeHasFS hasFs) encLedger ds st = do
  createDirectoryIfMissing hasFs True $ snapshotToDirPath ds
  crc1 <- writeExtLedgerState fs encLedger (snapshotToStatePath ds) $ state st
  crc2 <- takeHandleSnapshot (tables st) (state st) $ snapshotToDirName ds
  writeSnapshotMetadata fs ds $
    SnapshotMetadata
      { snapshotBackend = UTxOHDMemSnapshot
      , snapshotChecksum = maybe crc1 (crcOfConcat crc1) crc2
      , snapshotTablesCodecVersion = TablesCodecVersion1
      }

implTakeSnapshot ::
  ( IOLike m
  , LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  ) =>
  CodecConfig blk ->
  Tracer m (TraceSnapshotEvent blk) ->
  SomeHasFS m ->
  Maybe (CanonicalSnapshotsFS m) ->
  Maybe String ->
  StateRef m (ExtLedgerState blk) ->
  m (Maybe (DiskSnapshot, RealPoint blk))
implTakeSnapshot ccfg tracer hasFS mCanonical suffix st = do
  case pointToWithOriginRealPoint (castPoint (getTip $ state st)) of
    Origin -> return Nothing
    NotOrigin t -> do
      let number = unSlotNo (realPointSlot t)
          snapshot = DiskSnapshot number suffix
      diskSnapshots <- defaultListSnapshots hasFS
      if List.any (== DiskSnapshot number suffix) diskSnapshots
        then
          return Nothing
        else do
          encloseTimedWith (TookSnapshot snapshot t >$< tracer) $
            writeSnapshot hasFS (encodeDiskExtLedgerState ccfg) snapshot st
          takeCanonicalSnapshotInMemory (($ t) >$< tracer) mCanonical snapshot
          return $ Just (snapshot, t)

-- | Read snapshot from disk.
--
--   Fail on data corruption, i.e. when the checksum of the read data differs
--   from the one tracked by @'DiskSnapshot'@.
loadSnapshot ::
  forall blk m.
  ( LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  , IOLike m
  , LedgerSupportsInMemoryLedgerDB (LedgerState blk)
  ) =>
  Tracer m LedgerDBV2Trace ->
  ResourceRegistry m ->
  CodecConfig blk ->
  SomeHasFS m ->
  DiskSnapshot ->
  ExceptT (SnapshotFailure blk) m (LedgerSeq' m blk, RealPoint blk)
loadSnapshot tracer _rr ccfg fs ds = do
  snapshotMeta <-
    withExceptT (InitFailureRead . ReadMetadataError (snapshotToMetadataPath ds)) $
      loadSnapshotMetadata fs ds
  Monad.when (snapshotBackend snapshotMeta /= UTxOHDMemSnapshot) $ do
    throwE $ InitFailureRead $ ReadMetadataError (snapshotToMetadataPath ds) MetadataBackendMismatch
  (extLedgerSt, checksumAsRead) <-
    withExceptT
      (InitFailureRead . ReadSnapshotFailed)
      $ readExtLedgerState fs (decodeDiskExtLedgerState ccfg) decode (snapshotToStatePath ds)
  case pointToWithOriginRealPoint (castPoint (getTip extLedgerSt)) of
    Origin -> throwE InitFailureGenesis
    NotOrigin pt -> do
      (values, Identity crcTables) <-
        withExceptT (InitFailureRead . ReadSnapshotFailed) $
          ExceptT $
            readIncremental
              fs
              Identity
              (valuesMKDecoder extLedgerSt)
              (snapshotToDirPath ds </> mkFsPath ["tables"])
      let computedCRC = crcOfConcat checksumAsRead crcTables
      Monad.when (computedCRC /= snapshotChecksum snapshotMeta) $
        throwE $
          InitFailureRead $
            ReadSnapshotDataCorruption
      (,pt) <$> lift (empty extLedgerSt values (newInMemoryLedgerTablesHandle tracer fs))

data Mem

instance
  ( IOLike m
  , LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  , LedgerSupportsInMemoryLedgerDB (LedgerState blk)
  ) =>
  Backend m Mem blk
  where
  data Args m Mem = InMemArgs
  newtype Resources m Mem = Resources (SomeHasFS m)
    deriving newtype NoThunks
  newtype Trace m Mem = NoTrace Void
    deriving newtype Show

  mkResources _ _ _ _ = pure . Resources
  releaseResources _ _ = pure ()
  newHandleFromValues tracer _ (Resources shfs) =
    newInMemoryLedgerTablesHandle tracer shfs . ltprj
  newHandleFromSnapshot trcr reg ccfg shfs _ ds =
    loadSnapshot trcr reg ccfg shfs ds
  snapshotManager _ _ =
    Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory.snapshotManager

-- | Create arguments for initializing the LedgerDB using the InMemory backend.
mkInMemoryArgs ::
  ( IOLike m
  , LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  , LedgerSupportsInMemoryLedgerDB (LedgerState blk)
  ) =>
  a -> (LedgerDbBackendArgs m blk, a)
mkInMemoryArgs = (,) $ LedgerDbBackendArgsV2 $ SomeBackendArgs InMemArgs

instance IOLike m => StreamingBackend m Mem l where
  data YieldArgs m Mem l
    = -- \| Yield an in-memory snapshot
      YieldInMemory
        -- \| How to make a SomeHasFS for @m@
        (MountPoint -> SomeHasFS m)
        -- \| The file path at which the HasFS has to be opened
        FilePath
        (Decoders l)

  data SinkArgs m Mem l
    = SinkInMemory
        Int
        (TxIn l -> Encoding)
        (TxOut l -> Encoding)
        (SomeHasFS m)
        FilePath

  yield _ (YieldInMemory mkFs fp (Decoders decK decV)) =
    yieldInMemoryS mkFs fp decK decV

  sink _ (SinkInMemory chunkSize encK encV shfs fp) =
    sinkInMemoryS chunkSize encK encV shfs fp

{-------------------------------------------------------------------------------
  Streaming
-------------------------------------------------------------------------------}

streamingFile ::
  forall m.
  MonadThrow m =>
  SomeHasFS m ->
  FsPath ->
  ( Stream (Of ByteString) m (Maybe CRC) ->
    ExceptT DeserialiseFailure m (Stream (Of ByteString) m (Maybe CRC, Maybe CRC))
  ) ->
  ExceptT DeserialiseFailure m (Maybe CRC, Maybe CRC)
streamingFile (SomeHasFS fs') path cont =
  ExceptT $ withFile fs' path ReadMode $ \hdl ->
    runExceptT $ cont (getBS hdl initCRC) >>= noRemainingBytes
 where
  getBS h !crc = do
    bs <- S.lift $ hGetSome fs' h (fromIntegral defaultChunkSize)
    if BS.null bs
      then pure (Just crc)
      else do
        S.yield bs
        getBS h $! updateCRC bs crc

  noRemainingBytes s =
    lift (S.uncons s) >>= \case
      Nothing -> lift $ S.effects s
      Just (BS.null -> True, s') -> noRemainingBytes s'
      Just _ -> throwError $ DeserialiseFailure 0 "Remaining bytes"

yieldCborMapS ::
  forall m a b.
  MonadST m =>
  (forall s. Decoder s a) ->
  (forall s. Decoder s b) ->
  Stream (Of ByteString) m (Maybe CRC) ->
  Stream (Of (a, b)) (ExceptT DeserialiseFailure m) (Stream (Of ByteString) m (Maybe CRC))
yieldCborMapS decK decV = execStateT $ do
  hoist lift (decodeCbor decodeListLen >> decodeCbor decodeMapLenOrIndef) >>= \case
    Nothing -> go
    Just n -> replicateM_ n yieldKV
 where
  yieldKV = do
    kv <- hoist lift $ decodeCbor $ (,) <$> decK <*> decV
    lift $ S.yield kv

  go = do
    doBreak <- hoist lift $ decodeCbor decodeBreakOr
    unless doBreak $ yieldKV *> go

  decodeCbor dec =
    StateT $ \s -> go' s =<< lift (stToIO (deserialiseIncremental dec))
   where
    go' s = \case
      Partial k ->
        lift (S.next s) >>= \case
          Right (bs, s') -> go' s' =<< lift (stToIO (k (Just bs)))
          Left r -> go' (pure r) =<< lift (stToIO (k Nothing))
      Codec.CBOR.Read.Done bs _off a -> pure (a, S.yield bs *> s)
      Codec.CBOR.Read.Fail _bs _off err -> throwError err

yieldInMemoryS ::
  (MonadThrow m, MonadST m) =>
  (MountPoint -> SomeHasFS m) ->
  FilePath ->
  (forall s. Decoder s (TxIn l)) ->
  (forall s. Decoder s (TxOut l)) ->
  Yield m l
yieldInMemoryS mkFs (F.splitFileName -> (fp, fn)) decK decV _ k =
  streamingFile (mkFs $ MountPoint fp) (mkFsPath [fn]) $ \s -> do
    k $ yieldCborMapS decK decV s

sinkInMemoryS ::
  forall m l.
  MonadThrow m =>
  Int ->
  (TxIn l -> Encoding) ->
  (TxOut l -> Encoding) ->
  SomeHasFS m ->
  FilePath ->
  Sink m l
sinkInMemoryS writeChunkSize encK encV (SomeHasFS fs) fp _ s =
  ExceptT $ withFile fs (mkFsPath [fp]) (WriteMode MustBeNew) $ \hdl -> do
    let bs = toStrictByteString (encodeListLen 1 <> encodeMapLenIndef)
    let !crc0 = updateCRC bs initCRC
    void $ hPutSome fs hdl bs
    e <- runExceptT $ go hdl crc0 writeChunkSize mempty s
    case e of
      Left err -> pure $ Left err
      Right (r, crc1) -> do
        let bs1 = toStrictByteString encodeBreak
        void $ hPutSome fs hdl bs1
        let !crc2 = updateCRC bs1 crc1
        pure $ Right (fmap (,Just crc2) r)
 where
  go tb !crc 0 m s' = do
    let bs = toStrictByteString $ mconcat [encK k <> encV v | (k, v) <- reverse m]
    lift $ void $ hPutSome fs tb bs
    let !crc1 = updateCRC bs crc
    go tb crc1 writeChunkSize mempty s'
  go tb !crc n m s' = do
    mbs <- S.uncons s'
    case mbs of
      Nothing -> do
        let bs = toStrictByteString $ mconcat [encK k <> encV v | (k, v) <- reverse m]
        lift $ void $ hPutSome fs tb bs
        let !crc1 = updateCRC bs crc
        (,crc1) <$> S.effects s'
      Just (item, s'') -> go tb crc (n - 1) (item : m) s''

{-------------------------------------------------------------------------------
 Canonical snapshots
-------------------------------------------------------------------------------}

-- | A 'Yield' which already was provided the ledger state.
type Yield' m l =
  ( ( Stream
        (Of (TxIn l, TxOut l))
        (ExceptT DeserialiseFailure m)
        (Stream (Of ByteString) m (Maybe CRC)) ->
      ExceptT DeserialiseFailure m (Stream (Of ByteString) m (Maybe CRC, Maybe CRC))
    )
  ) ->
  ExceptT DeserialiseFailure m (Maybe CRC, Maybe CRC)

-- | Take a canonical snapshot, by providing a yielder that will stream the
-- ledger table values.
--
-- The @state@ file is copied into the canonical snapshot.
takeCanonicalSnapshot ::
  (IOLike m, SerializeTablesWithHint l) =>
  Tracer m (RealPoint blk -> TraceSnapshotEvent blk) ->
  DiskSnapshot ->
  -- | Allocate any resources needed (such as 'LedgerTablesHandle's)
  m a ->
  -- | Free the resources
  (a -> m ()) ->
  -- | Create a yield with the allocated resources
  (a -> Yield' m l) ->
  -- | The state for encoding the tables
  l EmptyMK ->
  -- | The CRC resulting from encoding the state
  CRC ->
  Maybe (CanonicalSnapshotsFS m) ->
  m ()
takeCanonicalSnapshot
  tracer
  snapshot
  allocator
  freer
  doYield
  st
  stateCRC
  mCanonicalFS =
    whenJust mCanonicalFS $
      \( CanonicalSnapshotsFS
           nonNativeShfs@(SomeHasFS nonNativeFs)
           (SomeHasFS nativeFs)
         ) ->
          encloseTimedWith (flip (TookCanonicalSnapshot snapshot) >$< tracer) $ do
            let snapFsPath = snapshotToDirPath snapshot
            createDirectoryIfMissing nonNativeFs True snapFsPath
            copyFile
              (nativeFs, snapFsPath </> mkFsPath ["state"])
              (nonNativeFs, snapFsPath </> mkFsPath ["state"])
            eCRCs <- withRegistry $ \rr -> do
              (rk, hdl) <- allocate rr (\_ -> allocator) freer
              eCRCs <-
                runExceptT
                  $ doYield
                    hdl
                  $ sink
                    (Proxy @Mem)
                    (SinkInMemory 1000 (encodeTxInWithHint st) (encodeTxOutWithHint st) nonNativeShfs "tables")
                    st
              Monad.void $ release rk
              pure eCRCs
            case eCRCs of
              Right (_, Just tablesCRC) ->
                writeSnapshotMetadata nonNativeShfs snapshot $
                  SnapshotMetadata
                    { snapshotBackend = UTxOHDMemSnapshot
                    , snapshotChecksum = crcOfConcat stateCRC tablesCRC
                    , snapshotTablesCodecVersion = TablesCodecVersion1
                    }
              _ -> pure ()

-- | A HasFS utility that copies files from one HasFS to another.
copyFile :: IOLike m => (HasFS m h1, FsPath) -> (HasFS m h2, FsPath) -> m ()
copyFile (hfs1, fp1) (hfs2, fp2) = do
  ba <- PBA.newByteArray defaultChunkSize
  withFile hfs1 fp1 ReadMode $ \hdlIn ->
    withFile hfs2 fp2 (WriteMode MustBeNew) $ \hdlOut ->
      go ba hdlIn hdlOut
 where
  go ba hin hout = do
    bytesRead <- hGetBufSome hfs1 hin ba 0 (fromIntegral defaultChunkSize)
    if bytesRead == 0
      then pure ()
      else do
        Monad.void $ hPutBufSome hfs2 hout ba 0 bytesRead
        go ba hin hout

-- | Take a canonical snapshot from an InMemory snapshot
--
-- This is implemented as a copy of the whole snapshot to the new directory.
takeCanonicalSnapshotInMemory ::
  IOLike m =>
  Tracer m (RealPoint blk -> TraceSnapshotEvent blk) ->
  Maybe (CanonicalSnapshotsFS m) ->
  DiskSnapshot ->
  m ()
takeCanonicalSnapshotInMemory tracer mCanonical snapshot =
  whenJust
    mCanonical
    ( \(CanonicalSnapshotsFS (SomeHasFS nonNativeHasFS) (SomeHasFS nativeHasFS)) ->
        encloseTimedWith (flip (TookCanonicalSnapshot snapshot) >$< tracer) $ do
          let snapFsPath = snapshotToDirPath snapshot
          createDirectoryIfMissing nonNativeHasFS True snapFsPath
          let copy = \x ->
                copyFile
                  (nativeHasFS, snapFsPath </> x)
                  (nonNativeHasFS, snapFsPath </> x)
          mapM_ (copy . mkFsPath . (: []))
            =<< listDirectory nativeHasFS snapFsPath
    )
