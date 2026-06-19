{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
-- Needed for @NoThunks (Table m k v b)@
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Implementation of the 'LedgerTablesHandle' interface with LSM trees.
module Ouroboros.Consensus.Storage.LedgerDB.V2.LSM
  ( -- * Era dispatch
    BlockSupportsLSM (..)

    -- * Backend API
  , LSM
  , Backend (..)
  , Args (LSMArgs)
  , Trace (..)
  , LSM.LSMTreeTrace (..)
  , mkLSMArgsIO
  , stdMkBlockIOFS

    -- * Streaming
  , YieldArgs (YieldLSM)
  , mkLSMYieldArgs
  , mkExportedLSMYieldArgs
  , SinkArgs (SinkLSM)
  , mkLSMSinkArgs
  , mkExportedLSMSinkArgs

    -- * Standalone (exported) snapshots
  , lsmDbExportSnapshot
  , lsmDbImportSnapshot

    -- * Exported for tests
  , LSM.Salt
  , SomeHasFSAndBlockIO (..)
  ) where

import Codec.Serialise (decode)
import Control.Exception (assert)
import qualified Control.Monad as Monad
import Control.Monad.Class.MonadThrow.Trans ()
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe (MaybeT (..), maybeToExceptT)
import Control.ResourceRegistry
import Control.Tracer
import Data.ByteString (toStrict)
import qualified Data.ByteString.Builder as BS
import Data.ByteString.Char8 (readInt)
import qualified Data.List as List
import Data.Maybe
import Data.MemPack
import qualified Data.Primitive.ByteArray as PBA
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text as Text
import Data.Typeable
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import Data.Void
import Data.Word
import Database.LSMTree (Salt, Session, Table)
import qualified Database.LSMTree as LSM
import GHC.Generics
import NoThunks.Class
import Data.SOP.BasicFunctors
import Data.SOP.Index (Index, hcimap, injectNS)
import Data.SOP.Strict (hcmap, hcollapse)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.HardFork.Combinator.Abstract
  ( CanHardFork
  , SingleEraBlock
  , proxySingle
  )
import Ouroboros.Consensus.HardFork.Combinator.Basics (HardForkBlock)
-- For the @BlockSupportsUTxOHD (HardForkBlock xs)@ instance (and hence the
-- @Keys\/Values\/Diff (HardForkBlock xs) = NS Wrap… xs@ equations).
import Ouroboros.Consensus.HardFork.Combinator.Ledger ()
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsProtocol
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Forker
  ( RangeQueryPrevious (..)
  , RangeReadTables
  )
import Ouroboros.Consensus.TypeFamilyWrappers
  ( WrapDiff (..)
  , WrapKeys (..)
  , WrapValues (..)
  )
import Ouroboros.Consensus.Storage.LedgerDB.Args
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.V2.Backend
import Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import Ouroboros.Consensus.Util (chunks, whenJust)
import Ouroboros.Consensus.Util.CRC
import Ouroboros.Consensus.Util.Enclose
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.CBOR (unpackEither)
import qualified Streaming as S
import qualified Streaming.Prelude as S
import qualified System.Directory as D
import System.FS.API
import System.FS.API.Lazy (hGetAll, hPutAll)
import System.FS.CRC (CRC)
import qualified System.FS.BlockIO.API as BIO
import System.FS.BlockIO.IO
import System.FilePath
  ( makeRelative
  , splitDirectories
  , splitFileName
  , takeDirectory
  , takeFileName
  )
import System.Random
import Prelude hiding (read)

{-------------------------------------------------------------------------------
  Era dispatch for the on-disk backend
-------------------------------------------------------------------------------}

-- | Dispatch the opaque, possibly @NS@-structured @'Keys'@\/@'Values'@\/@'Diff'@
-- of a @blk@ to the /current era/ @x@, where they are concrete and the LSM
-- backend can (de)serialise individual @('TxIn' x, 'TxOut' x)@ entries (the
-- byte codecs come from the @'MemPack'@ superclasses of
-- 'SingleEraBlockSupportsUTxOHD').
--
-- This is the on-disk analogue of the LSQ 'EraRangeReaderProvider': @read@,
-- @duplicateWithDiffs@ and genesis population are called by era-agnostic code,
-- so the era is read off the @NS@ tag of their input (the block is always in one
-- era, so every payload carries the current era's tag). The continuation
-- receives the era-@x@ payload plus, for @read@, the injection rebuilding the
-- @blk@-level values from the era's values.
--
-- The 'HardForkBlock' instance dispatches over its eras with @proxySingle@; a
-- single-era block is the trivial @x = blk@ with identity projections.
--
-- Each continuation takes a @'Proxy' x@ for the dispatched era: since
-- @'Keys'@\/@'Values'@\/@'Diff'@ are non-injective, the era @x@ cannot be
-- recovered from the payload, so the (injective) proxy is what lets the caller
-- bind @x@ (a @forall x.@-signed helper) and the hard-fork instance discharge
-- the rank-2 subsumption check.
class BlockSupportsUTxOHD blk => BlockSupportsLSM blk where
  withKeysEra ::
    Keys blk ->
    (forall x. SingleEraBlockSupportsUTxOHD x => Proxy x -> Keys x -> (Values x -> Values blk) -> r) ->
    r
  withDiffEra ::
    Diff blk ->
    (forall x. SingleEraBlockSupportsUTxOHD x => Proxy x -> Diff x -> r) ->
    r
  withValuesEra ::
    Values blk ->
    (forall x. SingleEraBlockSupportsUTxOHD x => Proxy x -> Values x -> r) ->
    r

-- | The hard-fork instance: the payload's @NS@ tag /is/ the current era, so
-- dispatch to that arm (via @proxySingle@, whose @SingleEraBlock@ supplies
-- 'SingleEraBlockSupportsUTxOHD') and hand the era-@x@ payload (and, for keys,
-- the @NS@ injection for the resulting values) to the continuation. The
-- per-arm type @\@a@ is bound explicitly (see the class doc on non-injectivity).
--
-- Orphan because the class is an LSM-backend concern and the 'HardForkBlock'
-- lives in the core library; there is exactly one such instance.
instance CanHardFork xs => BlockSupportsLSM (HardForkBlock xs) where
  withKeysEra ::
    forall r.
    Keys (HardForkBlock xs) ->
    ( forall x.
      SingleEraBlockSupportsUTxOHD x =>
      Proxy x -> Keys x -> (Values x -> Values (HardForkBlock xs)) -> r
    ) ->
    r
  withKeysEra ks k = hcollapse $ hcimap proxySingle go ks
   where
    go :: forall a. SingleEraBlock a => Index xs a -> WrapKeys a -> K r a
    go idx (WrapKeys kx) = K (k (Proxy @a) kx (injectNS idx . WrapValues))

  withDiffEra ::
    forall r.
    Diff (HardForkBlock xs) ->
    (forall x. SingleEraBlockSupportsUTxOHD x => Proxy x -> Diff x -> r) ->
    r
  withDiffEra d k = hcollapse $ hcmap proxySingle go d
   where
    go :: forall a. SingleEraBlock a => WrapDiff a -> K r a
    go (WrapDiff dx) = K (k (Proxy @a) dx)

  withValuesEra ::
    forall r.
    Values (HardForkBlock xs) ->
    (forall x. SingleEraBlockSupportsUTxOHD x => Proxy x -> Values x -> r) ->
    r
  withValuesEra vs k = hcollapse $ hcmap proxySingle go vs
   where
    go :: forall a. SingleEraBlock a => WrapValues a -> K r a
    go (WrapValues vx) = K (k (Proxy @a) vx)

-- | Type alias for convenience
type UTxOTable m = Table m TxInBytes TxOutBytes Void

instance NoThunks (Table m txin txout Void) where
  showTypeOf _ = "Table"
  wNoThunks _ _ = pure Nothing

data LSMClosedExn = LSMClosedExn
  deriving (Show, Exception)

type ExportSnapshot m = LSM.SnapshotName -> m ()

{-------------------------------------------------------------------------------
  TxOuts
-------------------------------------------------------------------------------}

newtype TxOutBytes = TxOutBytes {unTxOutBytes :: LSM.RawBytes}

-- | Serialise an era's @TxOut@ to on-disk bytes.
--
-- Plain 'MemPack' at the (concrete) era: the era's @TxOut@ encoding is
-- forwards-deserialisable, so bytes written at an earlier era decode under a
-- later era's unpacker ('fromTxOutBytes' at the current era). This is why the
-- backend pays nothing at a hard-fork boundary and needs no era-indexed codec.
toTxOutBytes :: MemPack (TxOut blk) => Proxy blk -> TxOut blk -> TxOutBytes
toTxOutBytes _ txout =
  let barr = packByteArray True txout
   in TxOutBytes $ LSM.RawBytes (VP.Vector 0 (PBA.sizeofByteArray barr) barr)

fromTxOutBytes :: MemPack (TxOut blk) => Proxy blk -> TxOutBytes -> TxOut blk
fromTxOutBytes _ (TxOutBytes (LSM.RawBytes vec)) =
  case unpackEither vec of
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

-- | Serialise an era's @TxIn@ to on-disk key bytes.
--
-- Goes through 'packTxInBytes' (not the raw 'MemPack' 'TxIn' codec) so the
-- on-disk keys sort the same as the Haskell @'Ord' ('TxIn' blk)@. The LSM
-- table is byte-ordered and every range read / snapshot stream pages it in that
-- order, so a non-order-preserving codec (e.g. ledger's little-endian Shelley
-- @TxIn@) would skip or repeat keys.
toTxInBytes ::
  forall blk. SingleEraBlockSupportsUTxOHD blk => Proxy blk -> TxIn blk -> TxInBytes
toTxInBytes _ txin =
  let barr = packTxInBytes @blk txin
   in TxInBytes $ LSM.RawBytes (VP.Vector 0 (PBA.sizeofByteArray barr) barr)

fromTxInBytes ::
  forall blk. SingleEraBlockSupportsUTxOHD blk => Proxy blk -> TxInBytes -> TxIn blk
fromTxInBytes _ (TxInBytes (LSM.RawBytes vec)) =
  case unpackTxInBytes @blk vec of
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
  LSM Handle management
-------------------------------------------------------------------------------}

closeLSMTable :: IOLike m => Tracer m LedgerDBV2Trace -> UTxOTable m -> m ()
closeLSMTable tracer t =
  encloseTimedWith (TraceLedgerTablesHandleClose >$< tracer) (LSM.closeTable t)

duplicateLSMTable ::
  IOLike m =>
  Tracer m LedgerDBV2Trace ->
  UTxOTable m ->
  m (UTxOTable m)
duplicateLSMTable tracer t = do
  encloseTimedWith (TraceLedgerTablesHandleDuplicate >$< tracer) $ LSM.duplicate t

{-------------------------------------------------------------------------------
  LedgerTablesHandle
-------------------------------------------------------------------------------}

newLSMLedgerTablesHandle ::
  forall m l blk.
  ( IOLike m
  , BlockSupportsLSM blk
  ) =>
  Tracer m LedgerDBV2Trace ->
  ExportSnapshot m ->
  -- | The size of the tables
  Word64 ->
  UTxOTable m ->
  m (LedgerTablesHandle m l blk)
newLSMLedgerTablesHandle tracer exportSnapshot utxosSize t =
  encloseTimedWith (TraceLedgerTablesHandleCreate >$< tracer) $ do
    pure
      LedgerTablesHandle
        { close = closeLSMTable tracer t
        , duplicateWithDiffs = implDuplicateWithDiffs tracer exportSnapshot t utxosSize
        , duplicate = implDuplicate utxosSize t tracer exportSnapshot
        , read = implRead tracer t
        , readRange = implReadRange @m @blk t
        , takeHandleSnapshot = implTakeHandleSnapshot tracer exportSnapshot t
        , tablesSize = fromIntegral utxosSize
        }

{-# INLINE implDuplicate #-}
{-# INLINE implRead #-}
{-# INLINE implReadRange #-}
{-# INLINE implDuplicateWithDiffs #-}
{-# INLINE implTakeHandleSnapshot #-}

implDuplicate ::
  ( IOLike m
  , BlockSupportsLSM blk
  ) =>
  Word64 ->
  UTxOTable m ->
  Tracer m LedgerDBV2Trace ->
  ExportSnapshot m ->
  m (LedgerTablesHandle m l blk)
implDuplicate size t tracer exportSnapshot =
  duplicateLSMTable tracer t
    >>= newLSMLedgerTablesHandle
      tracer
      exportSnapshot
      size

implDuplicateWithDiffs ::
  forall m l blk.
  ( IOLike m
  , BlockSupportsLSM blk
  ) =>
  Tracer m LedgerDBV2Trace ->
  ExportSnapshot m ->
  UTxOTable m ->
  Word64 ->
  Diff blk ->
  m (LedgerTablesHandle m l blk)
implDuplicateWithDiffs tracer exportSnapshot t0 size diff = do
  t <- duplicateLSMTable tracer t0
  let cont ::
        forall x.
        SingleEraBlockSupportsUTxOHD x =>
        Proxy x -> Diff x -> m (LedgerTablesHandle m l blk)
      cont _ dx = do
        let entries = diffToList @x dx
            toUpdate (Diff.Insert v) = LSM.Insert (toTxOutBytes (Proxy @x) v) Nothing
            toUpdate Diff.Delete = LSM.Delete
            vec =
              V.fromListN (length entries) $
                [(toTxInBytes (Proxy @x) k, toUpdate dd) | (k, dd) <- entries]
            (ins, dels) =
              List.foldl'
                ( \(i, d) (_, delta) -> case delta of
                    Diff.Insert{} -> (i + 1, d)
                    Diff.Delete -> (i, d + 1)
                )
                (0, 0)
                entries
            size' =
              assert (size + ins >= size) $
                assert (size + ins - dels <= size + ins) $
                  size + ins - dels
        encloseTimedWith (BackendTrace . SomeBackendTrace . LSMUpdate >$< tracer) $ LSM.updates t vec
        newLSMLedgerTablesHandle tracer exportSnapshot size' t
  encloseTimedWith (TraceLedgerTablesHandleRead >$< tracer) $
    withDiffEra @blk diff cont

implRead ::
  forall m l blk.
  ( IOLike m
  , BlockSupportsLSM blk
  ) =>
  Tracer m LedgerDBV2Trace ->
  UTxOTable m ->
  l blk ->
  Keys blk ->
  m (Values blk)
implRead tracer t _st keys =
  encloseTimedWith (TraceLedgerTablesHandleRead >$< tracer) $
    withKeysEra @blk keys cont
 where
  cont ::
    forall x.
    SingleEraBlockSupportsUTxOHD x =>
    Proxy x -> Keys x -> (Values x -> Values blk) -> m (Values blk)
  cont _ kx inj = do
    let txins = keysToList @x kx
        vec' = V.fromListN (length txins) (map (toTxInBytes (Proxy @x)) txins)
    res <-
      encloseTimedWith (BackendTrace . SomeBackendTrace . LSMLookup >$< tracer) $ LSM.lookups t vec'
    pure
      . inj
      . valuesFromList @x
      $ [ (fromTxInBytes (Proxy @x) k, fromTxOutBytes (Proxy @x) v)
        | (k, LSM.Found v) <- V.toList (V.zip vec' res)
        ]

implReadRange ::
  forall m blk.
  IOLike m =>
  UTxOTable m ->
  RangeReadTables m blk
implReadRange table = go
 where
  go ::
    forall x.
    SingleEraBlockSupportsUTxOHD x =>
    (Values blk -> Values x) ->
    RangeQueryPrevious x ->
    Int ->
    m (Values x)
  go _proj prev count = do
    entries <- case prev of
      NoPreviousQuery -> LSM.withCursor table (LSM.take count)
      -- Ask for one extra entry and drop it: the cursor also returns the key it
      -- was opened at.
      PreviousQueryWasUpTo k ->
        fmap (V.drop 1) $
          LSM.withCursorAtOffset table (toTxInBytes (Proxy @x) k) (LSM.take (count + 1))
      PreviousQueryWasFinal -> pure V.empty
    pure
      . valuesFromList @x
      $ [ (fromTxInBytes (Proxy @x) k, fromTxOutBytes (Proxy @x) v)
        | LSM.Entry k v <- V.toList entries
        ]

implTakeHandleSnapshot ::
  IOLike m =>
  Tracer m LedgerDBV2Trace ->
  (LSM.SnapshotName -> m ()) ->
  UTxOTable m ->
  l blk ->
  String ->
  m (Maybe CRC)
implTakeHandleSnapshot tracer exportSnapshot t _ snapshotName = do
  encloseTimedWith (BackendTrace . SomeBackendTrace . LSMSnap >$< tracer) $
    LSM.saveSnapshot
      (fromString snapshotName)
      (LSM.SnapshotLabel $ Text.pack $ "UTxO table")
      t
  exportSnapshot (fromString snapshotName)
  pure Nothing

{-------------------------------------------------------------------------------
  SnapshotManager
-------------------------------------------------------------------------------}

-- | Snapshots in LSM trees are split in two parts for now:
--
-- - The @state@ and @meta@ files in the usual location (@./ledger/<slotno>@ in
--   the ChainDB).
--
-- - The ledger tables, which are stored in the LSM-trees session directory,
--   under a @./lsm/snapshots/<slotno>@ directory.
--
-- Note that the name of the folder in which the @state@ file is and the name of
-- the snapshot in the LSM-trees directory have to match. This means that if the
-- user adds a suffix to the snapshot renaming the directory
-- @./ledger/<slotno>@, they will also have to rename the directory
-- @./lsm/snapshots/<slotno>@. Otherwise the initialization logic will exit with
-- failure saying that the snapshot was not found.
--
-- There is [an issue open in
-- LSM-trees](https://github.com/IntersectMBO/lsm-tree/issues/272) such that the
-- ledger tables part of the snapshot could also be stored in the
-- @./ledger/<slotno>@ directory, but it is not implemented yet.
snapshotManager ::
  ( IOLike m
  , LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  ) =>
  Session m ->
  CodecConfig blk ->
  Tracer m (TraceSnapshotEvent blk) ->
  SomeHasFS m ->
  SnapshotManager m blk (StateRef m ExtLedgerState blk)
snapshotManager session ccfg tracer fs =
  SnapshotManager
    { listSnapshots = defaultListSnapshots fs
    , deleteSnapshotIfTemporary = implDeleteSnapshotIfTemporary session fs tracer
    , takeSnapshot = implTakeSnapshot ccfg tracer fs
    }

{-# INLINE implTakeSnapshot #-}
{-# INLINE implDeleteSnapshotIfTemporary #-}

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
implTakeSnapshot ccfg tracer shfs@(SomeHasFS hasFs) suffix st =
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
          let sz = tablesSize (tables st)
          encloseTimedWith (TookSnapshot snapshot t >$< tracer) $
            writeSnapshot sz snapshot
          return $ Just (snapshot, t)
 where
  writeSnapshot sz ds = do
    createDirectoryIfMissing hasFs True $ snapshotToDirPath ds
    crc1 <- writeExtLedgerState shfs (encodeDiskExtLedgerState ccfg) (snapshotToStatePath ds) $ state st
    crc2 <- takeHandleSnapshot (tables st) (state st) $ snapshotToDirName ds
    writeUTxOSizeFile hasFs (snapshotToUTxOSizeFilePath ds) sz
    writeSnapshotMetadata shfs ds $
      SnapshotMetadata
        { snapshotBackend = UTxOHDLSMSnapshot
        , snapshotChecksum = maybe crc1 (crcOfConcat crc1) crc2
        , snapshotTablesCodecVersion = TablesCodecVersion1
        }

snapshotToUTxOSizeFilePath :: DiskSnapshot -> FsPath
snapshotToUTxOSizeFilePath ds = snapshotToDirPath ds </> mkFsPath ["utxoSize"]

writeUTxOSizeFile :: MonadThrow f => HasFS f h -> FsPath -> Int -> f ()
writeUTxOSizeFile hasFs p sz =
  Monad.void $ withFile hasFs p (WriteMode MustBeNew) $ \h ->
    hPutAll hasFs h $ BS.toLazyByteString $ BS.intDec sz

readUTxOSizeFile :: MonadThrow m => HasFS m h -> FsPath -> ExceptT (SnapshotFailure blk) m Word64
readUTxOSizeFile hfs p = do
  exists <- lift $ doesFileExist hfs p
  Monad.unless exists $ throwE (InitFailureRead ReadSnapshotDataCorruption)
  maybeToExceptT (InitFailureRead ReadSnapshotDataCorruption) $
    MaybeT $
      withFile hfs p ReadMode $ \h ->
        ( \case
            Nothing -> Nothing
            Just i ->
              if i < 0
                then Nothing
                else Just (fromIntegral i)
        )
          . fmap fst
          . readInt
          . toStrict
          <$> hGetAll hfs h

-- | Delete snapshot from disk and also from the LSM tree database.
implDeleteSnapshotIfTemporary ::
  forall m blk.
  IOLike m =>
  Session m ->
  SomeHasFS m ->
  Tracer m (TraceSnapshotEvent blk) ->
  DiskSnapshot ->
  m ()
implDeleteSnapshotIfTemporary
  session
  (SomeHasFS HasFS{doesDirectoryExist, removeDirectoryRecursive})
  tracer
  ss =
    Monad.when (diskSnapshotIsTemporary ss) $ do
      -- If an exception comes up while trying to delete snapshots we just
      -- swallow it and continue. We don't really care if the snapshot was half
      -- written or whatever, as the running node does not use existing
      -- snapshots.
      mapM_ (try @m @SomeException) [deleteState, deleteLsmTable]
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

{-------------------------------------------------------------------------------
  Creating the first handle
-------------------------------------------------------------------------------}

-- | Read snapshot from disk.
--
--   Fail on data corruption, i.e. when the checksum of the read data differs
--   from the one tracked by @'DiskSnapshot'@.
loadSnapshot ::
  forall blk m.
  ( LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  , BlockSupportsLSM blk
  , IOLike m
  ) =>
  Tracer m LedgerDBV2Trace ->
  CodecConfig blk ->
  SomeHasFS m ->
  Session m ->
  ExportSnapshot m ->
  DiskSnapshot ->
  ExceptT (SnapshotFailure blk) m (StateRef m ExtLedgerState blk, RealPoint blk)
loadSnapshot tracer ccfg fs@(SomeHasFS hfs) session exportSnapshot ds = do
  fileEx <- lift $ doesFileExist hfs (snapshotToDirPath ds)
  Monad.when fileEx $ throwE $ InitFailureRead ReadSnapshotIsLegacy
  snapshotMeta <-
    withExceptT (InitFailureRead . ReadMetadataError (snapshotToMetadataPath ds)) $
      loadSnapshotMetadata fs ds
  Monad.when (snapshotBackend snapshotMeta /= UTxOHDLSMSnapshot) $
    throwE $
      InitFailureRead $
        ReadMetadataError (snapshotToMetadataPath ds) MetadataBackendMismatch
  (extLedgerSt, checksumAsRead) <-
    withExceptT (InitFailureRead . ReadSnapshotFailed) $
      readExtLedgerState fs (decodeDiskExtLedgerState ccfg) decode (snapshotToStatePath ds)
  msz <- readUTxOSizeFile hfs (snapshotToUTxOSizeFilePath ds)
  case pointToWithOriginRealPoint (castPoint (getTip extLedgerSt)) of
    Origin -> throwE InitFailureGenesis
    NotOrigin pt -> do
      values <-
        lift $
          encloseTimedWith (TraceLedgerTablesHandleCreateFirst >$< tracer) $
            LSM.openTableFromSnapshot
              session
              (fromString $ snapshotToDirName ds)
              (LSM.SnapshotLabel $ Text.pack $ "UTxO table")

      h <- lift $ newLSMLedgerTablesHandle tracer exportSnapshot msz values
      Monad.when
        (checksumAsRead /= snapshotChecksum snapshotMeta)
        $ throwE
        $ InitFailureRead
          ReadSnapshotDataCorruption
      pure (StateRef extLedgerSt h, pt)

-- | Create the initial LSM table from values, which should happen only at
-- Genesis.
tableFromValues ::
  forall m blk.
  ( IOLike m
  , BlockSupportsLSM blk
  ) =>
  Tracer m LedgerDBV2Trace ->
  Session m ->
  Values blk ->
  m (UTxOTable m, Word64)
tableFromValues tracer session values = do
  table <-
    encloseTimedWith (TraceLedgerTablesHandleCreateFirst >$< tracer) $ LSM.newTable session
  let cont :: forall x. SingleEraBlockSupportsUTxOHD x => Proxy x -> Values x -> m Word64
      cont _ vsx = do
        let entries = valuesToList @x vsx
        mapM_
          ( \items ->
              LSM.inserts table $
                V.fromListN (length items) $
                  map (\(k, v) -> (toTxInBytes (Proxy @x) k, toTxOutBytes (Proxy @x) v, Nothing)) items
          )
          (chunks 1000 entries)
        pure (fromIntegral (length entries))
  sz <- withValuesEra @blk values cont
  pure (table, sz)

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

stdMkBlockIOFS ::
  FilePath -> WithTempRegistry st IO (SomeHasFSAndBlockIO IO)
stdMkBlockIOFS fastStoragePath = do
  uncurry SomeHasFSAndBlockIO
    <$> allocateTemp
      (ioHasBlockIO (MountPoint fastStoragePath) defaultIOCtxParams)
      (\(_, bio) -> BIO.close bio >> pure True)
      impossibleToNotTransfer

{-------------------------------------------------------------------------------
  Backend
-------------------------------------------------------------------------------}

type data LSM

-- | Create arguments for initializing the LedgerDB using the LSM-trees backend.
mkLSMArgsIO ::
  ( LedgerSupportsProtocol blk
  , LedgerDbSerialiseConstraints blk
  , BlockSupportsLSM blk
  ) =>
  Proxy blk ->
  -- | LSM database path, relative to the FS root.
  FilePath ->
  -- | LSM export path, relative to the FS root.
  Maybe FilePath ->
  -- | Root for the LSM filesystem.
  FilePath ->
  StdGen ->
  (LedgerDbBackendArgs IO blk, StdGen)
mkLSMArgsIO _ fpDb fpExport fastStorage gen =
  let (lsmSalt, gen') = genWord64 gen
   in ( LedgerDbBackendArgsV2 $
          SomeBackendArgs $
            LSMArgs
              (mkFsPath $ splitDirectories fpDb)
              (fmap (mkFsPath . splitDirectories) fpExport)
              lsmSalt
              (stdMkBlockIOFS fastStorage)
      , gen'
      )

instance
  ( LedgerSupportsProtocol blk
  , IOLike m
  , LedgerDbSerialiseConstraints blk
  , BlockSupportsLSM blk
  ) =>
  Backend m LSM blk
  where
  data Args m LSM
    = LSMArgs
        FsPath
        -- \^ The file path relative to the fast storage directory in which the LSM
        -- trees database will be located.
        (Maybe FsPath)
        -- \^ The file path relative to the fast storage directory in which the LSM
        -- trees database will dump its exports.
        Salt
        (forall st. WithTempRegistry st m (SomeHasFSAndBlockIO m))

  data Resources m LSM = LSMResources
    { sessionResource :: !(Session m)
    , exportSnapshotResource :: !(ExportSnapshot m)
    , someHasFSAndBlockIO :: !(SomeHasFSAndBlockIO m)
    }
    deriving Generic

  data Trace LSM
    = LSMTreeTrace !LSM.LSMTreeTrace
    | LSMLookup EnclosingTimed
    | LSMUpdate EnclosingTimed
    | LSMSnap EnclosingTimed
    | LSMOpenSession EnclosingTimed
    deriving Show

  mkResources _ trcr (LSMArgs pathDb pathExp salt mkFS) _ = do
    sblockio@(SomeHasFSAndBlockIO fs blockio) <- mkFS
    lift $ createDirectoryIfMissing fs True pathDb
    whenJust pathExp (lift . createDirectoryIfMissing fs True)
    session <-
      allocateTemp
        ( encloseTimedWith (BackendTrace . SomeBackendTrace . LSMOpenSession >$< trcr) $
            LSM.openSession
              (BackendTrace . SomeBackendTrace . LSMTreeTrace >$< trcr)
              fs
              blockio
              salt
              pathDb
        )
        (\s -> LSM.closeSession s >> pure True)
        impossibleToNotTransfer
    let exportSnap = case pathExp of
          Nothing -> const (pure ())
          Just p -> \snap -> LSM.exportSnapshot session snap p
    pure (LSMResources session exportSnap sblockio)

  releaseResources _ (LSMResources session _ (SomeHasFSAndBlockIO _ blockio)) = do
    LSM.closeSession session
    BIO.close blockio

  openStateRefFromSnapshot trcr ccfg shfs res ds = do
    loadSnapshot trcr ccfg shfs (sessionResource res) (exportSnapshotResource res) ds

  createAndPopulateStateRefFromGenesis trcr res state values = do
    (table, sz) <- tableFromValues @m @blk trcr (sessionResource res) values
    StateRef state <$> newLSMLedgerTablesHandle trcr (exportSnapshotResource res) sz table

  snapshotManager _ res = Ouroboros.Consensus.Storage.LedgerDB.V2.LSM.snapshotManager (sessionResource res)

instance
  ( IOLike m
  , SingleEraBlockSupportsUTxOHD blk
  ) =>
  StreamingBackend m LSM l blk
  where
  data YieldArgs m LSM l blk where
    -- \| Yield an LSM snapshot. The tables handle is opened at an arbitrary block
    -- @hfblk@ (the hard-fork block, for a Cardano snapshot) and 'readRange'
    -- projects it onto the single era @blk@ that is actually streamed: the
    -- on-disk table is era-agnostic bytes, and the projection's /type/ fixes the
    -- era whose @'MemPack'@ codec decodes the entries (see 'yieldLsmS'). A live
    -- database yield ('mkLSMYieldArgs') uses @hfblk ~ blk@ with @'id'@.
    YieldLSM ::
      Int ->
      LedgerTablesHandle m l hfblk ->
      -- \| Project the handle's tables onto the streamed era @blk@.
      (Values hfblk -> Values blk) ->
      -- \| Only to be closed by 'releaseYieldArgs'
      Session m ->
      -- \| Only to be closed by 'releaseYieldArgs'
      SomeHasFSAndBlockIO m ->
      -- \| Cleanup hook run by 'releaseYieldArgs' /after/ the session has been
      -- closed. Used to remove the temporary scratch session created when
      -- yielding from a standalone (exported) snapshot. 'pure ()' for a plain
      -- database yield.
      m () ->
      YieldArgs m LSM l blk

  data SinkArgs m LSM l blk
    = SinkLSM
        -- \| Chunk size
        Int
        -- \| LedgerDB snapshot fs
        (SomeHasFS m)
        -- \| Only to be closed by 'releaseSinkArgs'
        (SomeHasFSAndBlockIO m)
        -- \| DiskSnapshot
        DiskSnapshot
        (Session m)
        -- \| \"After save\" hook, run by 'sink' /while the session is still
        -- open/, right after the snapshot has been saved into it. Used to
        -- export the freshly saved snapshot to a standalone directory. 'pure ()'
        -- for a plain database sink.
        (m ())
        -- \| Cleanup hook run by 'releaseSinkArgs' /after/ the session has been
        -- closed. Used to remove the temporary scratch session created when
        -- sinking to a standalone (exported) snapshot. 'pure ()' for a plain
        -- database sink.
        (m ())

  releaseYieldArgs (YieldLSM _ hdl _proj session (SomeHasFSAndBlockIO _ bio) cleanup) = do
    close hdl
    LSM.closeSession session
    cleanup
    BIO.close bio

  releaseSinkArgs (SinkLSM _ _ (SomeHasFSAndBlockIO _ bio) _ session _afterSave cleanup) = do
    LSM.closeSession session
    cleanup
    BIO.close bio

  yield _ (YieldLSM chunkSize hdl proj _ _ _) = yieldLsmS chunkSize hdl proj

  sink _ (SinkLSM chunkSize shfs _ ds session afterSave _cleanup) =
    sinkLsmS chunkSize shfs ds session afterSave

data SomeHasFSAndBlockIO m where
  SomeHasFSAndBlockIO ::
    (Eq h, Typeable h) => HasFS m h -> BIO.HasBlockIO m h -> SomeHasFSAndBlockIO m

instance IOLike m => NoThunks (Resources m LSM) where
  wNoThunks _ (LSMResources _ _ (SomeHasFSAndBlockIO _ _)) = pure Nothing

{-------------------------------------------------------------------------------
  Streaming
-------------------------------------------------------------------------------}

-- | Stream the table by paging the value-threaded 'readRange', projecting the
-- handle's block @hfblk@ onto the single era @blk@ that is streamed (so
-- @'Yield'@ yields concrete @('TxIn' blk, 'TxOut' blk)@ pairs), recomputing the
-- next cursor as the page's maximum key and stopping on the first empty page.
-- The projection's /type/ selects the era whose @'MemPack'@ codec the cursor
-- uses to (de)serialise on-disk entries; the LSM backend ignores its /value/.
yieldLsmS ::
  forall m l hfblk blk.
  ( Monad m
  , SingleEraBlockSupportsUTxOHD blk
  ) =>
  Int ->
  LedgerTablesHandle m l hfblk ->
  (Values hfblk -> Values blk) ->
  Yield m l blk
yieldLsmS readChunkSize tb proj _hint k = do
  r <- k (go (NoPreviousQuery :: RangeQueryPrevious blk))
  lift $ S.effects r
 where
  go prev = do
    valsX <- lift $ S.lift $ readRange tb proj prev readChunkSize
    case valuesToList @blk valsX of
      [] -> pure $ pure Nothing
      entries -> do
        S.each entries
        go (PreviousQueryWasUpTo (fst (List.last entries)))

sinkLsmS ::
  forall m l blk.
  ( MonadAsync m
  , MonadMVar m
  , MonadThrow (STM m)
  , MonadMask m
  , MonadST m
  , MonadEvaluate m
  , SingleEraBlockSupportsUTxOHD blk
  ) =>
  Int ->
  SomeHasFS m ->
  DiskSnapshot ->
  Session m ->
  -- | \"After save\" hook, run while the session is still open, right after the
  -- snapshot has been saved into it.
  m () ->
  Sink m l blk
sinkLsmS writeChunkSize (SomeHasFS hfs) ds session afterSave _st stream = do
  r <-
    bracket
      (lift $ LSM.newTable session)
      (lift . LSM.closeTable)
      ( \lsmTable -> do
          (r, utxosSize) <- go (0 :: Int) lsmTable writeChunkSize mempty stream
          lift $
            LSM.saveSnapshot
              (LSM.toSnapshotName (snapshotToDirName ds))
              (LSM.SnapshotLabel $ T.pack "UTxO table")
              lsmTable
          lift $ writeUTxOSizeFile hfs (snapshotToUTxOSizeFilePath ds) utxosSize
          lift afterSave
          pure r
      )
  pure (fmap (,Nothing) r)
 where
  writeToTable :: UTxOTable m -> [(TxIn blk, TxOut blk)] -> m ()
  writeToTable lsmTable accUTxOs =
    LSM.inserts lsmTable $
      V.fromList
        [(toTxInBytes (Proxy @blk) txin, toTxOutBytes (Proxy @blk) txout, Nothing) | (txin, txout) <- accUTxOs]

  go utxosSize lsmTable 0 accUTxOs stream' = do
    lift $ writeToTable lsmTable accUTxOs
    go utxosSize lsmTable writeChunkSize mempty stream'
  go utxosSize lsmTable numToRead accUTxOs stream' = do
    mItem <- S.next stream'
    case mItem of
      Left r -> do
        lift $ writeToTable lsmTable accUTxOs
        pure (r, utxosSize)
      Right (item, stream'') -> go (utxosSize + 1) lsmTable (numToRead - 1) (item : accUTxOs) stream''

-- | Create Yield arguments for LSM
mkLSMYieldArgs ::
  forall m l blk.
  ( IOLike m
  , BlockSupportsLSM blk
  ) =>
  -- | The filepath in which the LSM database lives. Must not have a trailing slash!
  FilePath ->
  -- | The complete name of the snapshot to open, so @<slotno>[_<suffix>]@.
  DiskSnapshot ->
  -- | Usually 'stdMkBlockIOFS'
  (FilePath -> WithTempRegistry () m (SomeHasFSAndBlockIO m)) ->
  -- | Usually 'newStdGen'
  (m StdGen) ->
  m (YieldArgs m LSM l blk)
mkLSMYieldArgs lsmDbPath ds mkFS mkGen = do
  shfsbio@(SomeHasFSAndBlockIO hasFS blockIO) <-
    -- The Yield args will be created in the alloc step of a bracket so we do the
    -- 'runWithTempRegistry' here as the resource will be closed by the outer
    -- bracket anyways.
    runWithTempRegistry $ (\x -> (x, ())) <$> mkFS lsmDbPath
  salt <- fst . genWord64 <$> mkGen
  session <- LSM.openSession nullTracer hasFS blockIO salt (mkFsPath [])
  tb <-
    LSM.openTableFromSnapshot
      session
      (LSM.toSnapshotName (snapshotToDirName ds))
      (LSM.SnapshotLabel $ T.pack "UTxO table")
  h <- newLSMLedgerTablesHandle nullTracer (const (pure ())) 0 tb :: m (LedgerTablesHandle m l blk)
  pure $ YieldLSM 1000 h id session shfsbio (pure ())

-- | Create Yield arguments for a standalone (exported) LSM snapshot.
--
-- Unlike 'mkLSMYieldArgs', which reads a snapshot out of a live LSM database,
-- this opens a /temporary/ scratch session next to the exported snapshot,
-- imports the exported snapshot into it, and then streams it as usual. The
-- scratch session is removed by 'releaseYieldArgs'.
--
-- The scratch session is created in the parent directory of the exported
-- snapshot, so that it lives on the same volume (a requirement of importing).
mkExportedLSMYieldArgs ::
  forall hfblk m l blk.
  ( IOLike m
  , BlockSupportsLSM hfblk
  ) =>
  -- | Project the handle's tables (opened at @hfblk@, e.g. the hard-fork block)
  -- onto the single era @blk@ that is streamed. Its type selects the era's
  -- on-disk @'MemPack'@ codec; see 'yieldLsmS'.
  (Values hfblk -> Values blk) ->
  -- | The directory containing the exported snapshot. Must not have a trailing
  -- slash!
  FilePath ->
  -- | The complete name of the snapshot, so @<slotno>[_<suffix>]@.
  DiskSnapshot ->
  -- | Usually 'stdMkBlockIOFS'
  (FilePath -> WithTempRegistry () m (SomeHasFSAndBlockIO m)) ->
  -- | Usually 'newStdGen'
  (m StdGen) ->
  m (YieldArgs m LSM l blk)
mkExportedLSMYieldArgs proj exportDir ds mkFS mkGen = do
  shfsbio@(SomeHasFSAndBlockIO hasFS blockIO) <-
    runWithTempRegistry $ (\x -> (x, ())) <$> mkFS (takeDirectory exportDir)
  nonce <- fst . genWord64 <$> mkGen
  let scratch = scratchSessionPath nonce
  freshDirectory hasFS scratch
  let snapName = LSM.toSnapshotName (snapshotToDirName ds)
  session <-
    LSM.newSession
      nullTracer
      hasFS
      blockIO
      nonce
      scratch
  LSM.importSnapshot
    session
    snapName
    (mkFsPath [takeFileName exportDir])
  tb <-
    LSM.openTableFromSnapshot
      session
      snapName
      (LSM.SnapshotLabel $ T.pack "UTxO table")
  -- A scratch session used only for reading; it never exports snapshots.
  h <- newLSMLedgerTablesHandle nullTracer (const (pure ())) 0 tb :: m (LedgerTablesHandle m l hfblk)
  pure $ YieldLSM 1000 h proj session shfsbio (removeDirectoryRecursive hasFS scratch)

-- | Create Sink arguments for a standalone (exported) LSM snapshot.
--
-- Unlike 'mkLSMSinkArgs', which sinks into a live LSM database, this sinks into
-- a /temporary/ scratch session next to the destination directory, and then
-- exports the resulting snapshot to that directory (see 'LSM.exportSnapshot').
-- The scratch session is removed by 'releaseSinkArgs'.
--
-- The scratch session is created in the parent directory of the destination, so
-- that it lives on the same volume (a requirement of 'LSM.exportSnapshot').
mkExportedLSMSinkArgs ::
  IOLike m =>
  -- | The destination directory for the exported snapshot. It will be
  -- (re)created, and must not have a trailing slash!
  FilePath ->
  -- | The complete name of the snapshot, so @<slotno>[_<suffix>]@.
  DiskSnapshot ->
  -- | Usually 'ioHasFS', for the LedgerDB snapshot (@state@/@meta@) files.
  SomeHasFS m ->
  -- | Usually 'stdMkBlockIOFS'
  (FilePath -> WithTempRegistry () m (SomeHasFSAndBlockIO m)) ->
  -- | Usually 'newStdGen'
  (m StdGen) ->
  m (SinkArgs m LSM l blk)
mkExportedLSMSinkArgs exportDir ds snapFs mkBlockIOFS mkGen = do
  shfsbio@(SomeHasFSAndBlockIO hasFS blockIO) <-
    runWithTempRegistry $ (\x -> (x, ())) <$> mkBlockIOFS (takeDirectory exportDir)
  (nonce, gen') <- genWord64 <$> mkGen
  let salt = fst $ genWord64 gen'
      scratch = scratchSessionPath nonce
      exportFsPath = mkFsPath [takeFileName exportDir]
  freshDirectory hasFS scratch
  -- 'LSM.exportSnapshot' requires the destination directory to not exist.
  whenM (doesDirectoryExist hasFS exportFsPath) $
    removeDirectoryRecursive hasFS exportFsPath
  session <- LSM.newSession nullTracer hasFS blockIO salt scratch
  let afterSave =
        LSM.exportSnapshot session (LSM.toSnapshotName (snapshotToDirName ds)) exportFsPath
  pure (SinkLSM 1000 snapFs shfsbio ds session afterSave (removeDirectoryRecursive hasFS scratch))

-- | Export a snapshot out of a (offline) LSM database into a standalone
-- directory, which must not exist yet.
--
-- The database session and the destination must live on the same volume.
lsmDbExportSnapshot ::
  -- | The LSM database (session) directory.
  FilePath ->
  -- | The name of the snapshot to export, so @<slotno>[_<suffix>]@.
  String ->
  -- | The destination directory, which must not exist yet.
  FilePath ->
  IO ()
lsmDbExportSnapshot dbPath snapName exportDir = do
  salt <- fst . genWord64 <$> newStdGen
  withRootFS $ \hasFS blockIO -> do
    sessionDir <- toRootFsPath dbPath
    exportFs <- toRootFsPath exportDir
    bracket
      (LSM.openSession nullTracer hasFS blockIO salt sessionDir)
      LSM.closeSession
      (\session -> LSM.exportSnapshot session (LSM.toSnapshotName snapName) exportFs)

-- | Import a snapshot from a standalone directory into a new (offline) LSM
-- database, created at the given (empty or absent) directory.
--
-- The database session and the source must live on the same volume.
lsmDbImportSnapshot ::
  -- | The LSM database (session) directory. Created if it does not exist; must
  -- be empty otherwise.
  FilePath ->
  -- | The name to give the imported snapshot, so @<slotno>[_<suffix>]@.
  String ->
  -- | The source directory containing the exported snapshot.
  FilePath ->
  IO ()
lsmDbImportSnapshot dbPath snapName srcDir =
  withRootFS $ \hasFS blockIO -> do
    sessionDir <- toRootFsPath dbPath
    srcFs <- toRootFsPath srcDir
    createDirectoryIfMissing hasFS True sessionDir
    salt <- fst . genWord64 <$> newStdGen
    bracket
      ( LSM.newSession
          nullTracer
          hasFS
          blockIO
          salt
          sessionDir
      )
      LSM.closeSession
      (\s -> LSM.importSnapshot s (LSM.toSnapshotName snapName) srcFs)

-- | Mount the filesystem at the root, run an action with the resulting handles,
-- and close the underlying block IO afterwards.
--
-- Mounting at the root means that any 'FsPath' (the session directory itself,
-- as well as the import/export directories) can be expressed relative to a
-- single mount point, even when they live in unrelated parts of the filesystem
-- (as long as they are on the same volume, which the caller must guarantee).
withRootFS ::
  (forall h. (Eq h, Typeable h) => HasFS IO h -> BIO.HasBlockIO IO h -> IO a) ->
  IO a
withRootFS act = do
  SomeHasFSAndBlockIO hasFS blockIO <-
    runWithTempRegistry $ (\x -> (x, ())) <$> stdMkBlockIOFS "/"
  act hasFS blockIO
    `finally` BIO.close blockIO

-- | A 'FsPath' (relative to the filesystem root) for a temporary scratch
-- session directory, named after a nonce to make collisions unlikely.
scratchSessionPath :: Word64 -> FsPath
scratchSessionPath nonce = mkFsPath ["lsm-convert-scratch-" <> show nonce]

-- | Interpret a (possibly relative) 'FilePath' as a 'FsPath' relative to the
-- filesystem root, so that it can be used with a session mounted at the root.
toRootFsPath :: FilePath -> IO FsPath
toRootFsPath p = do
  absPath <- D.makeAbsolute p
  pure $ mkFsPath $ splitDirectories $ makeRelative "/" absPath

-- | Ensure a directory exists and is empty.
freshDirectory :: Monad m => HasFS m h -> FsPath -> m ()
freshDirectory hasFS p = do
  whenM (doesDirectoryExist hasFS p) $ removeDirectoryRecursive hasFS p
  createDirectoryIfMissing hasFS True p

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb act = mb >>= \b -> Monad.when b act

-- | Create Sink arguments for LSM
mkLSMSinkArgs ::
  IOLike m =>
  -- | The filepath for the LSM database
  FilePath ->
  -- | The filepath to the snapshot to be created, so @.../.../ledger/<slotno>[_<suffix>]@.
  DiskSnapshot ->
  -- | Usually 'ioHasFS'
  SomeHasFS m ->
  -- | Usually 'stdMkBlockIOFS'
  (FilePath -> WithTempRegistry () m (SomeHasFSAndBlockIO m)) ->
  -- | Usually 'newStdGen'
  (m StdGen) ->
  m (SinkArgs m LSM l blk)
mkLSMSinkArgs (splitFileName -> (lsmDbParentPath, lsmDbPath)) ds snapFs mkBlockIOFS mkGen = do
  shfsbio@(SomeHasFSAndBlockIO hasFS blockIO) <-
    -- The Sink args will be created in the alloc step of a bracket so we do the
    -- 'runWithTempRegistry' here as the resource will be closed by the outer
    -- bracket anyways.
    runWithTempRegistry $ (\x -> (x, ())) <$> mkBlockIOFS lsmDbParentPath
  let lsmDbPath' = mkFsPath [lsmDbPath]
  removeDirectoryRecursive hasFS lsmDbPath'
  createDirectory hasFS lsmDbPath'
  salt <- fst . genWord64 <$> mkGen
  session <- LSM.newSession nullTracer hasFS blockIO salt lsmDbPath'
  pure (SinkLSM 1000 snapFs shfsbio ds session (pure ()) (pure ()))
