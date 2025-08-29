{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- Needed for @NoThunks (Table m k v b)@
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Implementation of the 'LedgerTablesHandle' interface with LSM trees.
module Ouroboros.Consensus.Storage.LedgerDB.V2.LSM
  ( -- * LedgerTablesHandle
    newLSMLedgerTablesHandle
  , tableFromValuesMK
  , UTxOTable

    -- * Snapshots
  , loadSnapshot
  , snapshotManager

    -- * Re-exports
  , LSM.Entry (..)
  , LSM.RawBytes (..)
  , LSM.Salt
  , Session
  , LSM.openSession
  , LSM.closeSession
  , stdMkBlockIOFS

    -- * snapshot-converter
  , implTakeSnapshot
  , LSM.withNewSession
  , toTxInBytes
  , toTxOutBytes
  , LSM.newSession
  , LSM.toSnapshotName
  , LSM.SnapshotLabel (LSM.SnapshotLabel)
  , LSM.openTableFromSnapshot
  , LSM.closeTable
  , LSM.listSnapshots
  ) where

import Codec.Serialise (decode)
import qualified Control.Monad as Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Control.ResourceRegistry
import Control.Tracer
import qualified Data.Foldable as Foldable
import Data.Functor.Contravariant ((>$<))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.MemPack
import qualified Data.Primitive.ByteArray as PBA
import qualified Data.Set as Set
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Primitive as VP
import Data.Void
import Database.LSMTree (Session, Table)
import qualified Database.LSMTree as LSM
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsProtocol
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Args
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.TraceEvent
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.Args as V2
import Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import Ouroboros.Consensus.Util (chunks)
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.CRC
import Ouroboros.Consensus.Util.Enclose
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.IndexedMemPack
import System.FS.API
import qualified System.FS.BlockIO.API as BIO
import System.FS.BlockIO.IO
import Prelude hiding (read)

-- | Type alias for convenience
type UTxOTable m = Table m TxInBytes TxOutBytes Void

instance NoThunks (Table m txin txout Void) where
  showTypeOf _ = "Table"
  wNoThunks _ _ = pure Nothing

data LSMClosedExn = LSMClosedExn
  deriving (Show, Exception)

{-------------------------------------------------------------------------------
  TxOuts
-------------------------------------------------------------------------------}

newtype TxOutBytes = TxOutBytes {unTxOutBytes :: LSM.RawBytes}

toTxOutBytes :: IndexedMemPack (l EmptyMK) (TxOut l) => l EmptyMK -> TxOut l -> TxOutBytes
toTxOutBytes st txout =
  let barr = indexedPackByteArray True st txout
   in TxOutBytes $ LSM.RawBytes (VP.Vector 0 (PBA.sizeofByteArray barr) barr)

fromTxOutBytes :: IndexedMemPack (l EmptyMK) (TxOut l) => l EmptyMK -> TxOutBytes -> TxOut l
fromTxOutBytes st (TxOutBytes (LSM.RawBytes vec)) =
  case indexedUnpackEither st vec of
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
fromTxInBytes _ (TxInBytes (LSM.RawBytes vec)) =
  case unpackEither vec of
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
  LedgerTablesHandle
-------------------------------------------------------------------------------}

newLSMLedgerTablesHandle ::
  forall m l.
  ( IOLike m
  , HasLedgerTables l
  , IndexedMemPack (l EmptyMK) (TxOut l)
  ) =>
  Tracer m V2.FlavorImplSpecificTrace ->
  ResourceRegistry m ->
  (ResourceKey m, UTxOTable m) ->
  m (LedgerTablesHandle m l)
newLSMLedgerTablesHandle tracer rr (resKey, t) = do
  traceWith tracer V2.TraceLedgerTablesHandleCreate
  pure
    LedgerTablesHandle
      { close = implClose resKey
      , duplicate = implDuplicate rr t tracer
      , read = implRead t
      , readRange = implReadRange t
      , readAll = implReadAll t
      , pushDiffs = implPushDiffs t
      , takeHandleSnapshot = implTakeHandleSnapshot t
      , tablesSize = pure Nothing
      }

{-# INLINE implClose #-}
{-# INLINE implDuplicate #-}
{-# INLINE implRead #-}
{-# INLINE implReadRange #-}
{-# INLINE implReadAll #-}
{-# INLINE implPushDiffs #-}
{-# INLINE implTakeHandleSnapshot #-}

implClose :: IOLike m => ResourceKey m -> m ()
implClose = Monad.void . release

implDuplicate ::
  ( IOLike m
  , HasLedgerTables l
  , IndexedMemPack (l EmptyMK) (TxOut l)
  ) =>
  ResourceRegistry m ->
  UTxOTable m ->
  Tracer m V2.FlavorImplSpecificTrace ->
  m (LedgerTablesHandle m l)
implDuplicate rr t tracer = do
  table <-
    allocate
      rr
      (\_ -> LSM.duplicate t)
      ( \t' -> do
          traceWith tracer V2.TraceLedgerTablesHandleClose
          LSM.closeTable t'
      )
  newLSMLedgerTablesHandle tracer rr table

implRead ::
  forall m l.
  ( IOLike m
  , HasLedgerTables l
  , IndexedMemPack (l EmptyMK) (TxOut l)
  ) =>
  UTxOTable m -> l EmptyMK -> LedgerTables l KeysMK -> m (LedgerTables l ValuesMK)
implRead t st (LedgerTables (KeysMK keys)) = do
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

implReadAll ::
  ( IOLike m
  , HasLedgerTables l
  , IndexedMemPack (l EmptyMK) (TxOut l)
  ) =>
  UTxOTable m ->
  l EmptyMK ->
  m (LedgerTables l ValuesMK)
implReadAll t st =
  let readAll' m = do
        (v, n) <- implReadRange t st (m, 100000)
        maybe (pure v) (fmap (ltliftA2 unionValues v) . readAll' . Just) n
   in readAll' Nothing

implPushDiffs ::
  forall m l mk.
  ( IOLike m
  , HasLedgerTables l
  , IndexedMemPack (l EmptyMK) (TxOut l)
  ) =>
  UTxOTable m -> l mk -> l DiffMK -> m ()
implPushDiffs t _ !st1 = do
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

implTakeHandleSnapshot :: IOLike m => UTxOTable m -> t -> String -> m (Maybe a)
implTakeHandleSnapshot t _ snapshotName = do
  LSM.saveSnapshot
    (fromString snapshotName)
    (LSM.SnapshotLabel $ Text.pack $ "UTxO table")
    t
  pure Nothing

{-------------------------------------------------------------------------------
  SnapshotManager
-------------------------------------------------------------------------------}

snapshotManager ::
  ( IOLike m
  , LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  ) =>
  Session m ->
  Complete LedgerDbArgs m blk ->
  SnapshotManager m m blk (StateRef m (ExtLedgerState blk))
snapshotManager session args =
  snapshotManager'
    session
    (configCodec . getExtLedgerCfg . ledgerDbCfg $ lgrConfig args)
    (LedgerDBSnapshotEvent >$< lgrTracer args)
    (lgrHasFS args)

snapshotManager' ::
  ( IOLike m
  , LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  ) =>
  Session m ->
  CodecConfig blk ->
  Tracer m (TraceSnapshotEvent blk) ->
  SomeHasFS m ->
  SnapshotManager m m blk (StateRef m (ExtLedgerState blk))
snapshotManager' session ccfg tracer fs =
  SnapshotManager
    { listSnapshots = defaultListSnapshots fs
    , deleteSnapshot = implDeleteSnapshot session fs tracer
    , takeSnapshot = implTakeSnapshot ccfg tracer fs
    }

{-# INLINE implTakeSnapshot #-}
{-# INLINE implDeleteSnapshot #-}

implTakeSnapshot ::
  ( IOLike m
  , LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  ) =>
  CodecConfig blk ->
  Tracer m (TraceSnapshotEvent blk) ->
  SomeHasFS m ->
  Maybe String ->
  StateRef m (ExtLedgerState blk) ->
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
          encloseTimedWith (TookSnapshot snapshot t >$< tracer) $
            writeSnapshot snapshot
          return $ Just (snapshot, t)
 where
  writeSnapshot ds = do
    createDirectoryIfMissing hasFs True $ snapshotToDirPath ds
    crc1 <- writeExtLedgerState shfs (encodeDiskExtLedgerState ccfg) (snapshotToStatePath ds) $ state st
    crc2 <- takeHandleSnapshot (tables st) (state st) $ snapshotToDirName ds
    writeSnapshotMetadata shfs ds $
      SnapshotMetadata
        { snapshotBackend = UTxOHDLSMSnapshot
        , snapshotChecksum = maybe crc1 (crcOfConcat crc1) crc2
        }

-- | Delete snapshot from disk and also from the LSM tree database.
implDeleteSnapshot ::
  IOLike m =>
  Session m ->
  SomeHasFS m ->
  Tracer m (TraceSnapshotEvent blk) ->
  DiskSnapshot ->
  m ()
implDeleteSnapshot
  session
  (SomeHasFS HasFS{doesDirectoryExist, removeDirectoryRecursive})
  tracer
  ss = do
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
  , IOLike m
  ) =>
  Tracer m V2.FlavorImplSpecificTrace ->
  ResourceRegistry m ->
  CodecConfig blk ->
  SomeHasFS m ->
  Session m ->
  DiskSnapshot ->
  ExceptT (SnapshotFailure blk) m (LedgerSeq' m blk, RealPoint blk)
loadSnapshot tracer rr ccfg fs session ds =
  do
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
                  traceWith tracer V2.TraceLedgerTablesHandleClose
                  LSM.closeTable t
              )
        Monad.when
          (checksumAsRead /= snapshotChecksum snapshotMeta)
          $ throwE
          $ InitFailureRead
            ReadSnapshotDataCorruption
        (,pt)
          <$> lift (empty extLedgerSt values (newLSMLedgerTablesHandle tracer rr))

-- | Create the initial LSM table from values, which should happen only at
-- Genesis.
tableFromValuesMK ::
  forall m l.
  (IOLike m, IndexedMemPack (l EmptyMK) (TxOut l), MemPack (TxIn l)) =>
  Tracer m V2.FlavorImplSpecificTrace ->
  ResourceRegistry m ->
  Session m ->
  l EmptyMK ->
  LedgerTables l ValuesMK ->
  m (ResourceKey m, UTxOTable m)
tableFromValuesMK tracer rr session st (LedgerTables (ValuesMK values)) = do
  res@(_, table) <-
    allocate
      rr
      ( \_ ->
          LSM.newTableWith (LSM.defaultTableConfig{LSM.confFencePointerIndex = LSM.OrdinaryIndex}) session
      )
      ( \tb -> do
          traceWith tracer V2.TraceLedgerTablesHandleClose
          LSM.closeTable tb
      )
  mapM_ (go table) $ chunks 1000 $ Map.toList values
  pure res
 where
  go table items =
    LSM.inserts table $
      V.fromListN (length items) $
        map (\(k, v) -> (toTxInBytes (Proxy @l) k, toTxOutBytes st v, Nothing)) items

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

stdMkBlockIOFS ::
  FilePath -> ResourceRegistry IO -> IO (ResourceKey IO, V2.SomeHasFSAndBlockIO IO)
stdMkBlockIOFS fastStoragePath rr = do
  (rk1, bio) <-
    allocate
      rr
      (\_ -> ioHasBlockIO (MountPoint fastStoragePath) defaultIOCtxParams)
      (BIO.close . snd)
  pure (rk1, uncurry V2.SomeHasFSAndBlockIO bio)
