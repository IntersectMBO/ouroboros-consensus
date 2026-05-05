{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory
  ( Backend (..)
  , Args (InMemArgs)
  , Trace (NoTrace)
  , Mem
  , YieldArgs (YieldInMemory)
  , SinkArgs (SinkInMemory)
  , mkInMemoryArgs
  ) where

import Cardano.Binary as CBOR
import Cardano.Slotting.Slot
import Codec.CBOR.Read
import qualified Codec.CBOR.Write as CBOR
import Codec.Serialise (decode)
import Control.Monad (replicateM_, unless)
import qualified Control.Monad as Monad
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.Except
import Control.Monad.State.Strict (execStateT)
import Control.Monad.Trans.Except
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
import Data.Void
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
import Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq hiding (tables)
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq as StateRef
import Ouroboros.Consensus.Util.CBOR (readIncremental)
import Ouroboros.Consensus.Util.CRC
import Ouroboros.Consensus.Util.Enclose
import Ouroboros.Consensus.Util.IOLike
import Streaming
import qualified Streaming as S
import qualified Streaming.Prelude as S
import System.FS.API
import System.FS.CRC
import Prelude hiding (read)

{-------------------------------------------------------------------------------
  InMemory implementation of LedgerTablesHandles
-------------------------------------------------------------------------------}

newInMemoryLedgerTablesHandle ::
  forall m l blk.
  ( IOLike m
  , HasLedgerTables l blk
  , CanUpgradeLedgerTables l blk
  , SerializeTablesWithHint l blk
  , StandardHash (l blk)
  , GetTip (l blk)
  ) =>
  Tracer m LedgerDBV2Trace ->
  -- | FileSystem in order to take snapshots
  SomeHasFS m ->
  -- | The tables
  Values blk ->
  m (LedgerTablesHandle m l blk)
newInMemoryLedgerTablesHandle !tracer !someFS@(SomeHasFS !hasFS) tables =
  encloseTimedWith (TraceLedgerTablesHandleCreate >$< tracer) $
    let h =
          LedgerTablesHandle
            { close = encloseTimedWith (TraceLedgerTablesHandleClose >$< tracer) (pure ())
            , duplicate = encloseTimedWith (TraceLedgerTablesHandleCreate >$< tracer) $ pure h
            , read = implRead tables
            , readRange = implReadRange tables
            , readAll = \_ -> pure tables
            , duplicateWithDiffs = implDuplicateWithDiffs tracer tables someFS
            , takeHandleSnapshot = implTakeHandleSnapshot tables hasFS
            , tablesSize = Map.size . getValuesMK $ tables
            }
     in pure h

{-# INLINE implRead #-}
{-# INLINE implReadRange #-}
{-# INLINE implDuplicateWithDiffs #-}
{-# INLINE implTakeHandleSnapshot #-}

implRead ::
  ( IOLike m
  , HasLedgerTables l blk
  ) =>
  Values blk ->
  l blk EmptyMK ->
  Keys blk ->
  m (Values blk)
implRead (ValuesMK v) _ (KeysMK k) = do
  pure $ ValuesMK $ v `Map.restrictKeys` k

implReadRange ::
  (IOLike m, HasLedgerTables l blk) =>
  Values blk ->
  l blk EmptyMK ->
  (Maybe (TxIn blk), Int) ->
  m (Values blk, Maybe (TxIn blk))
implReadRange (ValuesMK m) _ (f, t) =
  let m' = Map.take t . (maybe id (\g -> snd . Map.split g) f) $ m
   in pure (ValuesMK m', fst <$> Map.lookupMax m')

implDuplicateWithDiffs ::
  ( IOLike m
  , HasLedgerTables l blk
  , CanUpgradeLedgerTables l blk
  , StandardHash (l blk)
  , GetTip (l blk)
  , SerializeTablesWithHint l blk
  ) =>
  Tracer m LedgerDBV2Trace ->
  Values blk ->
  SomeHasFS m ->
  l blk mk1 ->
  l blk DiffMK ->
  m (LedgerTablesHandle m l blk)
implDuplicateWithDiffs !tracer tables !someFS st0 !diffs = do
  let newtables =
        flip
          (\(ValuesMK vals) (DiffMK d) -> ValuesMK (Diff.applyDiff vals d))
          (projectLedgerTables diffs)
          . upgradeTables st0 diffs
          $ tables
  newInMemoryLedgerTablesHandle tracer someFS newtables

implTakeHandleSnapshot ::
  (IOLike m, SerializeTablesWithHint l blk) =>
  Values blk ->
  HasFS m h ->
  l blk EmptyMK ->
  String ->
  m (Maybe CRC)
implTakeHandleSnapshot values hasFS hint snapshotName = do
  createDirectoryIfMissing hasFS True $ mkFsPath [snapshotName]
  withFile hasFS (mkFsPath [snapshotName, "tables"]) (WriteMode MustBeNew) $ \hf ->
    fmap (Just . snd) $
      hPutAllCRC hasFS hf $
        CBOR.toLazyByteString $
          valuesMKEncoder hint values

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
  SnapshotManager m m blk (StateRef m ExtLedgerState blk)
snapshotManager ccfg tracer fs =
  SnapshotManager
    { listSnapshots = defaultListSnapshots fs
    , deleteSnapshotIfTemporary = defaultDeleteSnapshotIfTemporary fs tracer
    , takeSnapshot = implTakeSnapshot ccfg tracer fs
    }

{-# INLINE implTakeSnapshot #-}
implTakeSnapshot ::
  ( IOLike m
  , LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  ) =>
  CodecConfig blk ->
  Tracer m (TraceSnapshotEvent blk) ->
  SomeHasFS m ->
  Maybe String ->
  StateRef m ExtLedgerState blk ->
  m (Maybe (DiskSnapshot, RealPoint blk))
implTakeSnapshot ccfg tracer shfs@(SomeHasFS hasFS) suffix st = do
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
          encloseTimedWith (TookSnapshot snapshot t >$< tracer) $
            writeSnapshot snapshot
          return $ Just (snapshot, t)
 where
  writeSnapshot ds = do
    createDirectoryIfMissing hasFS True $ snapshotToDirPath ds
    crc1 <- writeExtLedgerState shfs (encodeDiskExtLedgerState ccfg) (snapshotToStatePath ds) $ state st
    crc2 <- takeHandleSnapshot (StateRef.tables st) (state st) $ snapshotToDirName ds
    writeSnapshotMetadata shfs ds $
      SnapshotMetadata
        { snapshotBackend = UTxOHDMemSnapshot
        , snapshotChecksum = maybe crc1 (crcOfConcat crc1) crc2
        , snapshotTablesCodecVersion = TablesCodecVersion1
        }

-- | Read snapshot from disk.
--
--   Fail on data corruption, i.e. when the checksum of the read data differs
--   from the one tracked by @'DiskSnapshot'@.
loadSnapshot ::
  forall blk m.
  ( LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  , CanUpgradeLedgerTables LedgerState blk
  , IOLike m
  ) =>
  Tracer m LedgerDBV2Trace ->
  CodecConfig blk ->
  SomeHasFS m ->
  DiskSnapshot ->
  ExceptT (SnapshotFailure blk) m (StateRef m ExtLedgerState blk, RealPoint blk)
loadSnapshot tracer ccfg fs@(SomeHasFS hfs) ds = do
  fileEx <- lift $ doesFileExist hfs (snapshotToDirPath ds)
  Monad.when fileEx $ throwE $ InitFailureRead ReadSnapshotIsLegacy

  snapshotMeta <-
    withExceptT (InitFailureRead . ReadMetadataError (snapshotToMetadataPath ds)) $
      loadSnapshotMetadata fs ds
  Monad.when (snapshotBackend snapshotMeta /= UTxOHDMemSnapshot) $
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
      (values, Identity crcTables) <-
        withExceptT (InitFailureRead . ReadSnapshotFailed) $
          ExceptT $
            readIncremental
              fs
              Identity
              (valuesMKDecoder extLedgerSt)
              (snapshotToTablesPath ds)
      let computedCRC = crcOfConcat checksumAsRead crcTables
      Monad.when (computedCRC /= snapshotChecksum snapshotMeta) $
        throwE $
          InitFailureRead $
            ReadSnapshotDataCorruption
      h <- lift $ newInMemoryLedgerTablesHandle tracer fs values
      pure (StateRef extLedgerSt h, pt)

snapshotToTablesPath :: DiskSnapshot -> FsPath
snapshotToTablesPath ds = snapshotToDirPath ds </> mkFsPath ["tables"]

type data Mem

instance
  ( IOLike m
  , LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  , CanUpgradeLedgerTables LedgerState blk
  ) =>
  Backend m Mem blk
  where
  data Args m Mem = InMemArgs
  newtype Resources m Mem = Resources (SomeHasFS m)
    deriving newtype NoThunks
  newtype Trace Mem = NoTrace Void
    deriving newtype Show

  mkResources _ _ _ = pure . Resources
  releaseResources _ _ = pure ()
  createAndPopulateStateRefFromGenesis tracer (Resources shfs) values =
    StateRef (forgetLedgerTables values)
      <$> newInMemoryLedgerTablesHandle tracer shfs (projectLedgerTables values)
  openStateRefFromSnapshot trcr ccfg shfs _ ds =
    loadSnapshot trcr ccfg shfs ds
  snapshotManager _ _ =
    Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory.snapshotManager

-- | Create arguments for initializing the LedgerDB using the InMemory backend.
mkInMemoryArgs ::
  ( IOLike m
  , LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  , CanUpgradeLedgerTables LedgerState blk
  ) =>
  a -> (LedgerDbBackendArgs m blk, a)
mkInMemoryArgs = (,) $ LedgerDbBackendArgsV2 $ SomeBackendArgs InMemArgs

instance IOLike m => StreamingBackend m Mem l blk where
  data YieldArgs m Mem l blk
    = -- \| Yield an in-memory snapshot
      YieldInMemory
        -- \| The file system anchored at the snapshots directory
        (SomeHasFS m)
        -- \| The snapshot
        DiskSnapshot
        (Decoders blk)

  data SinkArgs m Mem l blk
    = SinkInMemory
        Int
        (TxIn blk -> Encoding)
        (TxOut blk -> Encoding)
        (SomeHasFS m)
        DiskSnapshot

  releaseYieldArgs _ = pure ()
  yield _ (YieldInMemory fs ds (Decoders decK decV)) =
    yieldInMemoryS fs ds decK decV

  releaseSinkArgs _ = pure ()
  sink _ (SinkInMemory chunkSize encK encV shfs ds) =
    sinkInMemoryS chunkSize encK encV shfs ds

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
  SomeHasFS m ->
  DiskSnapshot ->
  (forall s. Decoder s (TxIn blk)) ->
  (forall s. Decoder s (TxOut blk)) ->
  Yield m l blk
yieldInMemoryS fs ds decK decV _ k =
  streamingFile fs (snapshotToTablesPath ds) $ \s -> do
    k $ yieldCborMapS decK decV s

sinkInMemoryS ::
  forall m l blk.
  MonadThrow m =>
  Int ->
  (TxIn blk -> Encoding) ->
  (TxOut blk -> Encoding) ->
  SomeHasFS m ->
  DiskSnapshot ->
  Sink m l blk
sinkInMemoryS writeChunkSize encK encV (SomeHasFS fs) ds _ s =
  ExceptT $ withFile fs (snapshotToTablesPath ds) (WriteMode MustBeNew) $ \hdl -> do
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
    mbs <- S.next s'
    case mbs of
      Left r -> do
        let bs = toStrictByteString $ mconcat [encK k <> encV v | (k, v) <- reverse m]
        lift $ void $ hPutSome fs tb bs
        let !crc1 = updateCRC bs crc
        pure (r, crc1)
      Right (item, s'') -> go tb crc (n - 1) (item : m) s''
