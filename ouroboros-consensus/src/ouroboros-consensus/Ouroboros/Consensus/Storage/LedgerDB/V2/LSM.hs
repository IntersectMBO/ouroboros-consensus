{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Implementation of the 'LedgerTablesHandle' interface with LSM trees.
module Ouroboros.Consensus.Storage.LedgerDB.V2.LSM
  ( -- * LedgerTablesHandle
    newLSMLedgerTablesHandle
  , tableFromValuesMK

    -- * LSM TxOuts
  , LedgerSupportsLSMLedgerDB (..)
  , LSM.FencePointerIndexType (..)

    -- * Snapshots
  , loadSnapshot
  , snapshotToStatePath
  , snapshotManager

    -- * Serialise helpers
  , serialiseLSMViaMemPack
  , deserialiseLSMViaMemPack

    -- * Re-exports
  , LSM.Entry (..)
  , LSM.SerialiseKey (..)
  , LSM.SerialiseValue (..)
  , LSM.ResolveValue (..)
  , LSM.ResolveAsFirst (..)
  , LSM.RawBytes (..)
  , LSM.Salt
  , Session
  , LSM.openSession
  , LSM.closeSession
  , stdGenSalt
  , stdMkBlockIOFS

    -- * snapshot-converter
  , implTakeSnapshot
  , LSM.withNewSession
  ) where

import Cardano.Binary as CBOR
import Codec.Serialise (decode)
import Control.Monad (foldM)
import qualified Control.Monad as Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Control.ResourceRegistry
import Control.Tracer
import qualified Data.Foldable as Foldable
import Data.Foldable.WithIndex
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
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.CRC
import Ouroboros.Consensus.Util.Enclose
import Ouroboros.Consensus.Util.IOLike
import System.FS.API
import qualified System.FS.BlockIO.API as BIO
import System.FS.BlockIO.IO
import System.Random
import Prelude hiding (read)

-- | Type alias for convenience
type UTxOTable m l = Table m (TxIn l) (LSMTxOut l) Void

instance NoThunks (Table m txin txout Void) where
  showTypeOf _ = "Table"
  wNoThunks _ _ = pure Nothing

data LSMClosedExn = LSMClosedExn
  deriving (Show, Exception)

-- | Create the initial LSM table from values, which should happen only at the
-- Genesis.
tableFromValuesMK ::
  forall m l.
  IOLike m =>
  LedgerSupportsLSMLedgerDB l =>
  ResourceRegistry m ->
  Session m ->
  LedgerTables l ValuesMK ->
  m (ResourceKey m, UTxOTable m l)
tableFromValuesMK rr session (LedgerTables (ValuesMK values)) = do
  res@(_, table) <-
    allocate
      rr
      ( \_ ->
          LSM.newTableWith (LSM.defaultTableConfig{LSM.confFencePointerIndex = lsmIndex (Proxy @l)}) session
      )
      LSM.closeTable
  let sz = min 1000 (Map.size values)
  vec <- VM.unsafeNew sz -- can use unsafe
  (n, vec') <- ifoldlM (go table) (0, vec) values
  case n of
    0 -> pure ()
    _ -> do
      vec'' <- V.unsafeFreeze vec'
      LSM.inserts table vec''
  pure res
 where
  go table k (n, vec) v
    | n == 1000 =
        do
          vec' <- V.unsafeFreeze vec -- can use unsafe because vec won't be used afterwards
          LSM.inserts table vec'
          vec'' <- VM.unsafeNew 1000
          pure (0, vec'')
    | otherwise =
        do
          VM.unsafeWrite vec n (k, toLSMTxOut (Proxy @l) v, Nothing) -- can use unsafe
          pure (n + 1, vec)

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

newLSMLedgerTablesHandle ::
  forall m l.
  ( IOLike m
  , HasLedgerTables l
  , LedgerSupportsLSMLedgerDB l
  ) =>
  Tracer m V2.FlavorImplSpecificTrace ->
  ResourceRegistry m ->
  (ResourceKey m, UTxOTable m l) ->
  m (LedgerTablesHandle m l)
newLSMLedgerTablesHandle tracer rr (resKey, t) = do
  traceWith tracer V2.TraceLedgerTablesHandleCreate
  pure
    LedgerTablesHandle
      { close = do
          Monad.void $ release resKey
          traceWith tracer V2.TraceLedgerTablesHandleClose
      , duplicate = do
          table <- allocate rr (\_ -> LSM.duplicate t) LSM.closeTable
          newLSMLedgerTablesHandle tracer rr table
      , read = \st (LedgerTables (KeysMK keys)) -> do
          vec <- VM.unsafeNew (Set.size keys)
          _ <-
            Foldable.foldl'
              (\m x -> m >>= \i -> VM.write vec i x >> pure (i + 1))
              (pure 0)
              keys
          vec' <- V.freeze vec
          res <- LSM.lookups t vec'
          pure
            . LedgerTables
            . ValuesMK
            . Foldable.foldl'
              ( \m (k, item) ->
                  case item of
                    LSM.Found v -> Map.insert k (fromLSMTxOut st v) m
                    LSM.NotFound -> m
                    LSM.FoundWithBlob{} -> m
              )
              Map.empty
            $ V.zip vec' res
      , readRange = implReadRange t
      , readAll = \st ->
          let readAll' m = do
                (v, n) <- implReadRange t st (m, 100000)
                maybe (pure v) (fmap (ltliftA2 unionValues v) . readAll' . Just) n
           in readAll' Nothing
      , pushDiffs = const (implPushDiffs t)
      , takeHandleSnapshot = \_ snapshotName -> do
          LSM.saveSnapshot
            (fromString snapshotName)
            (LSM.SnapshotLabel $ Text.pack $ "UTxO table: " ++ lsmSnapLabel (Proxy @l))
            t
          pure Nothing
      , tablesSize = pure Nothing
      }

implReadRange ::
  IOLike m =>
  HasLedgerTables l =>
  LedgerSupportsLSMLedgerDB l =>
  UTxOTable m l ->
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
              LSM.Entry k v -> Map.insert k (fromLSMTxOut st v) m
              LSM.EntryWithBlob{} -> m
          )
          Map.empty
        $ entries
    , case snd <$> V.unsnoc entries of
        Nothing -> Nothing
        Just (LSM.Entry k _) -> Just k
        Just (LSM.EntryWithBlob k _ _) -> Just k
    )
 where
  cursorFromStart = LSM.withCursor table (LSM.take num)
  -- Here we ask for one value more and we drop one value because the
  -- cursor returns also the key at which it was opened.
  cursorFromKey k = fmap (V.drop 1) $ LSM.withCursorAtOffset table k (LSM.take $ num + 1)

foldMWithKey :: Monad m => (a -> k -> v -> m a) -> a -> Map.Map k v -> m a
foldMWithKey f z m = foldM step z (Map.toAscList m)
 where
  step acc (k, v) = f acc k v

implPushDiffs ::
  forall m l.
  ( LedgerSupportsLSMLedgerDB l
  , IOLike m
  , HasLedgerTables l
  ) =>
  UTxOTable m l -> l DiffMK -> m ()
implPushDiffs t !st1 = do
  let LedgerTables (DiffMK (Diff.Diff diffs)) = projectLedgerTables st1
  vec <- VM.unsafeNew (Map.size diffs)
  Monad.void $
    foldMWithKey
      ( \idx k item -> do
          VM.unsafeWrite vec idx (k, (f item))
          pure (idx + 1)
      )
      0
      diffs
  LSM.updates t =<< V.freeze vec
 where
  f (Diff.Insert v) = LSM.Insert (toLSMTxOut (Proxy @l) v) Nothing
  f Diff.Delete = LSM.Delete

-- | The path within the LedgerDB's filesystem to the file that contains the
-- snapshot's serialized ledger state
snapshotToStatePath :: DiskSnapshot -> FsPath
snapshotToStatePath = mkFsPath . (\x -> [x, "state"]) . snapshotToDirName

instance
  ( LSM.SerialiseKey (TxIn (LedgerState blk))
  , LSM.SerialiseValue (LSMTxOut (LedgerState blk))
  , LSM.ResolveValue (LSMTxOut (LedgerState blk))
  , LedgerSupportsLSMLedgerDB (LedgerState blk)
  ) =>
  LedgerSupportsLSMLedgerDB (ExtLedgerState blk)
  where
  type LSMTxOut (ExtLedgerState blk) = LSMTxOut (LedgerState blk)

  toLSMTxOut _ = toLSMTxOut (Proxy @(LedgerState blk))
  fromLSMTxOut l = fromLSMTxOut (ledgerState l)
  lsmIndex _ = lsmIndex (Proxy @(LedgerState blk))
  lsmSnapLabel _ = lsmSnapLabel (Proxy @(LedgerState blk))

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
implTakeSnapshot ccfg tracer hasFS suffix st = case pointToWithOriginRealPoint (castPoint (getTip $ state st)) of
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
        return $ Just (snapshot, t)

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
      { snapshotBackend = UTxOHDLSMSnapshot
      , snapshotChecksum = maybe crc1 (crcOfConcat crc1) crc2
      }

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
  , LedgerSupportsLSMLedgerDB (ExtLedgerState blk)
  ) =>
  Tracer m V2.FlavorImplSpecificTrace ->
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
      values <-
        lift $
          allocate
            rr
            ( \_ ->
                LSM.openTableFromSnapshot
                  session
                  (fromString $ snapshotToDirName ds)
                  (LSM.SnapshotLabel $ Text.pack $ "UTxO table: " ++ lsmSnapLabel (Proxy @(ExtLedgerState blk)))
            )
            LSM.closeTable
      Monad.when (checksumAsRead /= snapshotChecksum snapshotMeta) $
        throwE $
          InitFailureRead ReadSnapshotDataCorruption
      (,pt) <$> lift (empty extLedgerSt values (newLSMLedgerTablesHandle tracer rr))

-- | Helper for implementing 'serialiseKey' and 'serialiseValue' for types that
-- are serialized via 'MemPack'.
serialiseLSMViaMemPack :: MemPack a => a -> LSM.RawBytes
serialiseLSMViaMemPack a =
  let barr = pack a
   in LSM.RawBytes (VP.Vector 0 (PBA.sizeofByteArray barr) barr)

-- | Helper for implementing 'deserialiseKey' and 'deserialiseValue' for types
-- that are serialized via 'MemPack'.
deserialiseLSMViaMemPack :: MemPack b => LSM.RawBytes -> b
deserialiseLSMViaMemPack (LSM.RawBytes (VP.Vector _ _ barr)) = unpackError barr

stdGenSalt :: IO LSM.Salt
stdGenSalt = fst . genWord64 <$> initStdGen

stdMkBlockIOFS ::
  FilePath -> ResourceRegistry IO -> IO (ResourceKey IO, V2.SomeHasFSAndBlockIO IO)
stdMkBlockIOFS fastStoragePath rr = do
  (rk1, bio) <-
    allocate
      rr
      (\_ -> ioHasBlockIO (MountPoint fastStoragePath) defaultIOCtxParams)
      (BIO.close . snd)
  pure (rk1, uncurry V2.SomeHasFSAndBlockIO bio)
