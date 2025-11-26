{-# LANGUAGE ScopedTypeVariables #-}
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
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

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
import Data.SOP.BasicFunctors
import Data.SOP.Strict
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
import Ouroboros.Consensus.Ledger.LedgerStateType

{-------------------------------------------------------------------------------
  InMemory implementation of LedgerTablesHandles
-------------------------------------------------------------------------------}

data LedgerTablesHandleState blk
  = LedgerTablesHandleOpen !(LedgerTables blk ValuesMK)
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
  , LedgerSupportsInMemoryLedgerDB l
  ) =>
  Tracer m LedgerDBV2Trace ->
  SomeHasFS m ->
  LedgerTables (LBlock l) ValuesMK ->
  m (LedgerTablesHandle m l)
newInMemoryLedgerTablesHandle tracer someFS@(SomeHasFS hasFS) l = do
  !tv <- newTVarIO (LedgerTablesHandleOpen l)
  traceWith tracer TraceLedgerTablesHandleCreate
  pure
    LedgerTablesHandle
      { close = implClose tracer tv
      , duplicate = implDuplicate tracer tv someFS
      , read = implRead tv
      , readRange = implReadRange tv
      , readAll = implReadAll tv
      , pushDiffs = implPushDiffs tv
      , takeHandleSnapshot = implTakeHandleSnapshot tv hasFS
      , tablesSize = implTablesSize (Proxy @l) tv
      , transfer = const (pure ())
      }

{-# INLINE implClose #-}
{-# INLINE implDuplicate #-}
{-# INLINE implRead #-}
{-# INLINE implReadRange #-}
{-# INLINE implReadAll #-}
{-# INLINE implPushDiffs #-}
{-# INLINE implTakeHandleSnapshot #-}
{-# INLINE implTablesSize #-}

implClose ::
  IOLike m =>
  Tracer m LedgerDBV2Trace ->
  StrictTVar m (LedgerTablesHandleState l) ->
  m ()
implClose tracer tv = do
  p <- atomically $ swapTVar tv LedgerTablesHandleClosed
  case p of
    LedgerTablesHandleOpen{} -> traceWith tracer TraceLedgerTablesHandleClose
    _ -> pure ()

implDuplicate ::
  ( IOLike m
  , LedgerSupportsInMemoryLedgerDB l
  ) =>
  Tracer m LedgerDBV2Trace ->
  StrictTVar m (LedgerTablesHandleState (LBlock l)) ->
  SomeHasFS m ->
  ResourceRegistry m ->
  m (ResourceKey m, LedgerTablesHandle m l)
implDuplicate tracer tv someFS rr = do
  hs <- readTVarIO tv
  !x <- guardClosed hs $ \v ->
    allocate
      rr
      (\_ -> newInMemoryLedgerTablesHandle tracer someFS v)
      close
  pure x

implRead ::
  ( IOLike m
  , HasLedgerTables l
  ) =>
  StrictTVar m (LedgerTablesHandleState (LBlock l)) ->
  l EmptyMK ->
  LedgerTables (LBlock l) KeysMK ->
  m (LedgerTables (LBlock l) ValuesMK)
implRead tv _ keys = do
  hs <- readTVarIO tv
  guardClosed
    hs
    (pure . flip (ltliftA2 (\(ValuesMK v) (KeysMK k) -> ValuesMK $ v `Map.restrictKeys` k)) keys)

implReadRange ::
  -- (IOLike m, HasLedgerTables l) =>
  StrictTVar m (LedgerTablesHandleState (LBlock l)) ->
  l EmptyMK ->
  (Maybe (TxIn (LBlock l)), Int) ->
  m (LedgerTables (LBlock l) ValuesMK, Maybe (TxIn (LBlock l)))
implReadRange = undefined -- tv _ (f, t) = undefined -- do
-- hs <- readTVarIO tv
-- guardClosed
--   hs
--   ( \(LedgerTables (ValuesMK m)) ->
--       let m' = Map.take t . (maybe id (\g -> snd . Map.split g) f) $ m
--        in pure (LedgerTables (ValuesMK m'), fst <$> Map.lookupMax m')
--   )

implReadAll ::
  IOLike m =>
  StrictTVar m (LedgerTablesHandleState (LBlock l)) ->
  l EmptyMK ->
  m (LedgerTables (LBlock l) ValuesMK)
implReadAll tv _ = do
  hs <- readTVarIO tv
  guardClosed hs pure

implPushDiffs ::
  ( IOLike m
  , LedgerSupportsInMemoryLedgerDB l
  ) =>
  StrictTVar m (LedgerTablesHandleState (LBlock l)) ->
  l mk1 ->
  l DiffMK ->
  m ()
implPushDiffs tv st0 !diffs =
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

implTakeHandleSnapshot ::
  (IOLike m, LedgerSupportsInMemoryLedgerDB l) =>
  StrictTVar m (LedgerTablesHandleState (LBlock l)) ->
  HasFS m h ->
  l EmptyMK ->
  String ->
  m (Maybe CRC)
implTakeHandleSnapshot tv hasFS hint snapshotName = do
  createDirectoryIfMissing hasFS True $ mkFsPath [snapshotName]
  h <- readTVarIO tv
  guardClosed h $
    \values ->
      withFile hasFS (mkFsPath [snapshotName, "tables"]) (WriteMode MustBeNew) $ \hf ->
        fmap (Just . snd) $
          hPutAllCRC hasFS hf $
            CBOR.toLazyByteString $
              valuesMKEncoder hint values

implTablesSize ::
  (IOLike m, LedgerSupportsInMemoryLedgerDB l) =>
  Proxy l -> StrictTVar m (LedgerTablesHandleState (LBlock l)) ->
  m (Maybe (NP (K Int) (TablesForBlock (LBlock l))))
implTablesSize _ tv = do
  hs <- readTVarIO tv
  guardClosed hs (pure . Just . hmap (K . Map.size . getValuesMK . getTable) . getLedgerTables)

{-------------------------------------------------------------------------------
  Snapshots
-------------------------------------------------------------------------------}

snapshotManager ::
  ( IOLike m
  , LedgerDbSerialiseConstraints (ExtLedgerState blk) blk
  , LedgerSupportsProtocol blk
  ) =>
  CodecConfig blk ->
  Tracer m (TraceSnapshotEvent blk) ->
  SomeHasFS m ->
  SnapshotManager m m blk (StateRef m (ExtLedgerState blk))
snapshotManager ccfg tracer fs =
  SnapshotManager
    { listSnapshots = defaultListSnapshots fs
    , deleteSnapshot = defaultDeleteSnapshot fs tracer
    , takeSnapshot = implTakeSnapshot ccfg tracer fs
    }

{-# INLINE implTakeSnapshot #-}
implTakeSnapshot ::
  ( IOLike m
  , LedgerDbSerialiseConstraints (ExtLedgerState blk) blk
  , LedgerSupportsProtocol blk
  ) =>
  CodecConfig blk ->
  Tracer m (TraceSnapshotEvent blk) ->
  SomeHasFS m ->
  Maybe String ->
  StateRef m (ExtLedgerState blk) ->
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
    crc2 <- takeHandleSnapshot (tables st) (state st) $ snapshotToDirName ds
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
  ( LedgerDbSerialiseConstraints (ExtLedgerState blk) blk
  , LedgerSupportsProtocol blk
  , IOLike m
  , LedgerSupportsInMemoryLedgerDB (ExtLedgerState blk)
  ) =>
  Tracer m LedgerDBV2Trace ->
  ResourceRegistry m ->
  CodecConfig blk ->
  SomeHasFS m ->
  DiskSnapshot ->
  ExceptT (SnapshotFailure blk) m (LedgerSeq' m blk, RealPoint blk)
loadSnapshot tracer _rr ccfg fs@(SomeHasFS hfs) ds = do
  fileEx <- lift $ doesFileExist hfs (snapshotToDirPath ds)
  Monad.when fileEx $ throwE $ InitFailureRead ReadSnapshotIsLegacy

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

type data Mem

instance
  ( IOLike m
  , LedgerDbSerialiseConstraints (ExtLedgerState blk) blk
  , LedgerSupportsProtocol blk
  , LedgerSupportsInMemoryLedgerDB (ExtLedgerState blk)
  ) =>
  Backend m Mem blk
  where
  data Args m Mem = InMemArgs
  newtype Resources m Mem = Resources (SomeHasFS m)
    deriving newtype NoThunks
  newtype Trace Mem = NoTrace Void
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
  , LedgerDbSerialiseConstraints (ExtLedgerState blk) blk
  , LedgerSupportsProtocol blk
  , LedgerSupportsInMemoryLedgerDB (ExtLedgerState blk)
  ) =>
  a -> (LedgerDbBackendArgs m blk, a)
mkInMemoryArgs = (,) $ LedgerDbBackendArgsV2 $ SomeBackendArgs InMemArgs

instance IOLike m => StreamingBackend m Mem blk where
  data YieldArgs m Mem blk
    = -- \| Yield an in-memory snapshot
      YieldInMemory
        -- \| How to make a SomeHasFS for @m@
        (MountPoint -> SomeHasFS m)
        -- \| The file path at which the HasFS has to be opened
        FilePath
        (Decoders blk)

  data SinkArgs m Mem blk
    = SinkInMemory
        Int
        (TxIn blk -> Encoding)
        (TxOut blk -> Encoding)
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
  (forall s. Decoder s (TxIn blk)) ->
  (forall s. Decoder s (TxOut blk)) ->
  Yield m blk
yieldInMemoryS mkFs (F.splitFileName -> (fp, fname)) decK decV _ k =
  streamingFile (mkFs $ MountPoint fp) (mkFsPath [fname]) $ \s -> do
    k $ yieldCborMapS decK decV s

sinkInMemoryS ::
  forall m blk.
  MonadThrow m =>
  Int ->
  (TxIn blk -> Encoding) ->
  (TxOut blk -> Encoding) ->
  SomeHasFS m ->
  FilePath ->
  Sink m blk
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
