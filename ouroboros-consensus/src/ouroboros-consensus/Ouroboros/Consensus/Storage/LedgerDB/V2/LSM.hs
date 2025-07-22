{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Implementation of the 'LedgerTablesHandle' interface with LSM trees.
module Ouroboros.Consensus.Storage.LedgerDB.V2.LSM
  ( -- * LedgerTablesHandle
    newLSMLedgerTablesHandle
  , tableFromValuesMK

    -- * LSM TxOuts
  , LSMTxOut
  , HasLSMTxOut (..)

    -- * Snapshots
  , loadSnapshot
  , snapshotToStatePath
  , snapshotManagement

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
  ) where

import Cardano.Binary as CBOR
import Codec.Serialise (decode)
import qualified Control.Monad as Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Control.ResourceRegistry
import Control.Tracer
import Data.Functor.Contravariant ((>$<))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.MemPack
import qualified Data.Primitive.ByteArray as PBA
import qualified Data.Set as Set
import Data.String (fromString)
import qualified Data.Vector as V
import Data.Vector.Primitive (Vector (..))
import Data.Void
import Database.LSMTree (Session, Table)
import qualified Database.LSMTree as LSM
import qualified Database.LSMTree.Internal.Types as LSM
import GHC.Generics
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
import qualified System.FilePath as FilePath
import System.Random
import Prelude hiding (read)

-- | Type alias for convenience
type UTxOTable m l = Table m (TxIn l) (LSMTxOut l) Void

data LedgerTablesHandleState m l
  = LedgerTablesHandleOpen !(UTxOTable m l)
  | LedgerTablesHandleClosed
  deriving Generic

deriving instance NoThunks (LedgerTablesHandleState m l)

instance NoThunks (Table m txin txout Void) where
  showTypeOf _ = "Table"
  wNoThunks _ (LSM.Table _tbl) = pure Nothing

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
  res@(_, table) <- allocate rr (\_ -> LSM.newTable session) LSM.closeTable
  LSM.inserts table $
    V.fromList $
      [(k, toLSMTxOut (Proxy @l) v, Nothing) | (k, v) <- Map.toList values]
  pure res

guardClosed ::
  MonadSTM m => StrictTVar m (LedgerTablesHandleState m l) -> (UTxOTable m l -> m a) -> m a
guardClosed tv f =
  readTVarIO tv >>= \case
    LedgerTablesHandleClosed -> error $ show LSMClosedExn
    LedgerTablesHandleOpen st -> f st

snapshotManagement ::
  ( IOLike m
  , LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  ) =>
  Session m ->
  Complete LedgerDbArgs m blk ->
  SnapshotManager m m blk (StateRef m (ExtLedgerState blk))
snapshotManagement session args =
  snapshotManagement'
    session
    (configCodec . getExtLedgerCfg . ledgerDbCfg $ lgrConfig args)
    (LedgerDBSnapshotEvent >$< lgrTracer args)
    (lgrHasFS args)

snapshotManagement' ::
  ( IOLike m
  , LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  ) =>
  Session m ->
  CodecConfig blk ->
  Tracer m (TraceSnapshotEvent blk) ->
  SomeHasFS m ->
  SnapshotManager m m blk (StateRef m (ExtLedgerState blk))
snapshotManagement' session ccfg tracer fs =
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
  Session m ->
  (ResourceKey m, UTxOTable m l) ->
  m (LedgerTablesHandle m l)
newLSMLedgerTablesHandle tracer rr session (resKey, t0) = do
  !tv <- newTVarIO (LedgerTablesHandleOpen t0)
  traceWith tracer V2.TraceLedgerTablesHandleCreate
  pure
    LedgerTablesHandle
      { close = do
          Monad.void $ release resKey
          atomically $ writeTVar tv LedgerTablesHandleClosed
          traceWith tracer V2.TraceLedgerTablesHandleClose
      , duplicate = guardClosed tv $ \t -> do
          table <- allocate rr (\_ -> LSM.duplicate t) LSM.closeTable
          newLSMLedgerTablesHandle tracer rr session table
      , read = \st (LedgerTables (KeysMK keys)) ->
          guardClosed
            tv
            ( \t -> do
                let keys' = Set.toList keys
                res <- LSM.lookups t (V.fromList keys')
                pure $
                  LedgerTables $
                    ValuesMK $
                      Map.fromList [(k, fromLSMTxOut st v) | (k, LSM.Found v) <- zip keys' (V.toList res)]
            )
      , readRange = implReadRange tv
      , readAll = \st ->
          let readAll' m = do
                (v, n) <- implReadRange tv st (m, 100000)
                maybe (pure v) (fmap (ltliftA2 unionValues v) . readAll' . Just) n
           in readAll' Nothing
      , pushDiffs = const (implPushDiffs tv)
      , takeHandleSnapshot = \_ snapshotName -> guardClosed tv $
          \table -> do
            LSM.saveSnapshot (fromString snapshotName) "UTxO table" table
            pure Nothing
      , tablesSize = pure Nothing
      }

implReadRange ::
  IOLike m =>
  HasLedgerTables l =>
  LedgerSupportsLSMLedgerDB l =>
  StrictTVar m (LedgerTablesHandleState m l) ->
  l EmptyMK ->
  (Maybe (TxIn l), Int) ->
  m (LedgerTables l ValuesMK, Maybe (TxIn l))
implReadRange tv st (mPrev, num) =
  guardClosed
    tv
    ( \table ->
        let
          cursorFromStart = LSM.withCursor table (LSM.take num)
          -- Here we ask for one value more and we drop one value because the
          -- cursor returns also the key at which it was opened.
          cursorFromKey k = fmap (V.drop 1) $ LSM.withCursorAtOffset table k (LSM.take $ num + 1)
         in
          do
            entries <- V.toList <$> maybe cursorFromStart cursorFromKey mPrev
            pure
              ( LedgerTables . ValuesMK . Map.fromList $
                  [(k, (fromLSMTxOut st v)) | LSM.Entry k v <- entries]
              , case snd <$> unsnoc entries of
                  Nothing -> Nothing
                  Just (LSM.Entry k _) -> Just k
                  Just (LSM.EntryWithBlob k _ _) -> Just k
              )
    )
 where
#if __GLASGOW_HASKELL__ < 908
    unsnoc :: [a] -> Maybe ([a], a)
    unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
#else
    unsnoc = List.unsnoc
#endif

implPushDiffs ::
  forall m l.
  ( LedgerSupportsLSMLedgerDB l
  , IOLike m
  , HasLedgerTables l
  ) =>
  StrictTVar m (LedgerTablesHandleState m l) -> l DiffMK -> m ()
implPushDiffs tv !st1 = do
  let LedgerTables (DiffMK (Diff.Diff diffs)) = projectLedgerTables st1
  guardClosed
    tv
    ( \t ->
        LSM.updates t $
          V.fromList
            [ ( k
              , case h of
                  Diff.Insert v -> LSM.Insert (toLSMTxOut (Proxy @l) v) Nothing
                  Diff.Delete -> LSM.Delete
              )
            | (k, h) <- Map.toList diffs
            ]
    )

-- | The path within the LedgerDB's filesystem to the file that contains the
-- snapshot's serialized ledger state
snapshotToStatePath :: DiskSnapshot -> FsPath
snapshotToStatePath = mkFsPath . (\x -> [x, "state"]) . snapshotToDirName

type instance LSMTxOut (ExtLedgerState blk) = LSMTxOut (LedgerState blk)

instance HasLSMTxOut (LedgerState blk) => HasLSMTxOut (ExtLedgerState blk) where
  toLSMTxOut _ = toLSMTxOut (Proxy @(LedgerState blk))
  fromLSMTxOut l = fromLSMTxOut (ledgerState l)

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
  crc2 <-
    takeHandleSnapshot (tables st) (state st) $
      show (dsNumber ds) <> maybe "" (("_" <>) . show) (dsSuffix ds)
  writeSnapshotMetadata fs ds $
    SnapshotMetadata
      { snapshotBackend = UTxOHDLSMSnapshot
      , snapshotChecksum = maybe crc1 (crcOfConcat crc1) crc2
      }

-- | Delete snapshot from disk and also from the LSM tree database.
implDeleteSnapshot ::
  IOLike m => Session m -> SomeHasFS m -> Tracer m (TraceSnapshotEvent blk) -> DiskSnapshot -> m ()
implDeleteSnapshot session (SomeHasFS HasFS{doesDirectoryExist, removeDirectoryRecursive}) tracer ss = do
  let p = snapshotToDirPath ss
  exists <- doesDirectoryExist p
  Monad.when exists (removeDirectoryRecursive p)
  LSM.deleteSnapshot
    session
    (fromString $ show (dsNumber ss) <> maybe "" (("_" <>) . show) (dsSuffix ss))
  traceWith tracer (DeletedSnapshot ss)

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
                  (fromString $ show (dsNumber ds) <> maybe "" (("_" <>) . show) (dsSuffix ds))
                  "UTxO table"
            )
            LSM.closeTable
      Monad.when (checksumAsRead /= snapshotChecksum snapshotMeta) $
        throwE $
          InitFailureRead ReadSnapshotDataCorruption
      (,pt) <$> lift (empty extLedgerSt values (newLSMLedgerTablesHandle tracer rr session))

-- | Helper for implementing 'serialiseKey' and 'serialiseValue' for types that
-- are serialized via 'MemPack'.
serialiseLSMViaMemPack :: MemPack a => a -> LSM.RawBytes
serialiseLSMViaMemPack a =
  let barr = pack a
   in LSM.RawBytes (Vector 0 (PBA.sizeofByteArray barr) barr)

-- | Helper for implementing 'deserialiseKey' and 'deserialiseValue' for types
-- that are serialized via 'MemPack'.
deserialiseLSMViaMemPack :: MemPack b => LSM.RawBytes -> b
deserialiseLSMViaMemPack (LSM.RawBytes (Vector _ _ barr)) = unpackError barr

stdGenSalt :: IO LSM.Salt
stdGenSalt = fst . genWord64 <$> initStdGen

stdMkBlockIOFS ::
  FilePath -> ResourceRegistry IO -> FilePath -> IO (ResourceKey IO, V2.SomeHasFSAndBlockIO IO)
stdMkBlockIOFS fastStoragePath rr relPath = do
  (rk1, bio) <-
    allocate
      rr
      (\_ -> ioHasBlockIO (MountPoint $ fastStoragePath FilePath.</> relPath) defaultIOCtxParams)
      (BIO.close . snd)
  pure (rk1, uncurry V2.SomeHasFSAndBlockIO bio)
