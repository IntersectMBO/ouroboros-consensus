{-# LANGUAGE BangPatterns #-}
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
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Storage.LedgerDB.V2.LSM
  ( -- * LedgerTablesHandle
    newLSMLedgerTablesHandle
  , GoodForLSM
  , LSMTxOut
  , ToLSMTxOut (..)

    -- * Snapshots
  , loadSnapshot
  , snapshotToStatePath
  , takeSnapshot
  , tableFromValuesMK
  , deleteSnapshot
  , serialiseLSMViaMemPack
  , deserialiseLSMViaMemPack
  ) where

import Cardano.Binary as CBOR
import Codec.Serialise (decode)
import qualified Control.Monad as Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Control.ResourceRegistry
import Control.Tracer
import Data.Functor.Contravariant ((>$<))
import Data.Kind
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
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsProtocol
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.Args as V2
import Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import Ouroboros.Consensus.Util.CRC
import Ouroboros.Consensus.Util.Enclose
import Ouroboros.Consensus.Util.IOLike
import System.FS.API
import Prelude hiding (read)

type UTxOTable m l = Table m (TxIn l) (LSMTxOut l) Void

type LSMTxOut :: LedgerStateKind -> Type
type family LSMTxOut l

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

type GoodForLSM l =
  ( LSM.SerialiseKey (TxIn l)
  , LSM.SerialiseValue (LSMTxOut l)
  , LSM.ResolveValue (LSMTxOut l)
  , ToLSMTxOut l
  )

class ToLSMTxOut l where
  toLSMTxOut :: Proxy l -> TxOut l -> LSMTxOut l
  fromLSMTxOut :: l EmptyMK -> LSMTxOut l -> TxOut l

-- | Create the initial table from values
tableFromValuesMK ::
  forall m l.
  IOLike m =>
  GoodForLSM l =>
  ResourceRegistry m ->
  Session m ->
  LedgerTables l ValuesMK ->
  m (ResourceKey m, UTxOTable m l)
tableFromValuesMK rr session (LedgerTables (ValuesMK values)) = do
  res@(_, table) <- allocate rr (\_ -> LSM.newTable session) LSM.closeTable
  LSM.inserts table $
    V.fromList $
      [(k, (toLSMTxOut (Proxy @l) v), Nothing) | (k, v) <- Map.toList values]
  pure res

guardClosed ::
  MonadSTM m => StrictTVar m (LedgerTablesHandleState m l) -> (UTxOTable m l -> m a) -> m a
guardClosed tv f = do
  readTVarIO tv >>= \case
    LedgerTablesHandleClosed -> error $ show LSMClosedExn
    LedgerTablesHandleOpen st -> f st

newLSMLedgerTablesHandle ::
  forall m l.
  ( IOLike m
  , HasLedgerTables l
  , CanUpgradeLedgerTables l
  , SerializeTablesWithHint l
  , GoodForLSM l
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
      , duplicate = do
          guardClosed tv $ \t -> do
            table <- allocate rr (\_ -> LSM.duplicate t) LSM.closeTable
            newLSMLedgerTablesHandle tracer rr session table
      , read = \st (LedgerTables (KeysMK keys)) -> do
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
                v@(LedgerTables (ValuesMK values)) <- implReadRange tv st (m, 100000)
                maybe (pure v) (\k -> fmap (ltliftA2 unionValues v) $ readAll' (Just $ fst k)) $
                  Map.lookupMax values
           in -- TODO!! Be careful with the order of the keys, the last in the map
              -- is maybe not the last in the serialized form
              readAll' Nothing
      , pushDiffs = const (implPushDiffs tv)
      , takeHandleSnapshot = \_ snapshotName -> do
          guardClosed tv $
            \table ->
              LSM.saveSnapshot (fromString snapshotName) "UTxO table" table >> pure Nothing
      , tablesSize = pure Nothing
      }

-- TODO: use bytestrings as values

implReadRange ::
  IOLike m =>
  HasLedgerTables l =>
  GoodForLSM l =>
  StrictTVar m (LedgerTablesHandleState m l) ->
  l EmptyMK ->
  (Maybe (TxIn l), Int) ->
  m (LedgerTables l ValuesMK)
implReadRange tv st = \(mPrev, num) ->
  guardClosed
    tv
    ( \table ->
        let
          cursorFromStart = LSM.withCursor table (LSM.take num)
          cursorFromKey k = fmap (V.drop 1) $ LSM.withCursorAtOffset table k (LSM.take $ num + 1)
         in
          do
            entries <- maybe cursorFromStart cursorFromKey mPrev
            pure . LedgerTables . ValuesMK . Map.fromList $
              [(k, (fromLSMTxOut st v)) | LSM.Entry k v <- V.toList entries]
    )

implPushDiffs ::
  forall m l.
  ( GoodForLSM l
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

instance ToLSMTxOut (LedgerState blk) => ToLSMTxOut (ExtLedgerState blk) where
  toLSMTxOut _ txout = toLSMTxOut (Proxy @(LedgerState blk)) txout
  fromLSMTxOut l txout = fromLSMTxOut (ledgerState l) txout

takeSnapshot ::
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
takeSnapshot ccfg tracer hasFS suffix st = do
  case pointToWithOriginRealPoint (castPoint (getTip $ state st)) of
    Origin -> return Nothing
    NotOrigin t -> do
      let number = unSlotNo (realPointSlot t)
          snapshot = DiskSnapshot number suffix
      diskSnapshots <- listSnapshots hasFS
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
      show (dsNumber ds) <> (maybe "" (("_" <>) . show) (dsSuffix ds))
  writeSnapshotMetadata fs ds $
    SnapshotMetadata
      { snapshotBackend = UTxOHDLSMSnapshot
      , snapshotChecksum = maybe crc1 (crcOfConcat crc1) crc2
      }

-- | Delete snapshot from disk
deleteSnapshot :: IOLike m => Session m -> SomeHasFS m -> DiskSnapshot -> m ()
deleteSnapshot session (SomeHasFS HasFS{doesDirectoryExist, removeDirectoryRecursive}) ss = do
  let p = snapshotToDirPath ss
  exists <- doesDirectoryExist p
  Monad.when exists (removeDirectoryRecursive p)
  LSM.deleteSnapshot
    session
    (fromString $ show (dsNumber ss) <> (maybe "" (("_" <>) . show) (dsSuffix ss)))

-- | Read snapshot from disk.
--
--   Fail on data corruption, i.e. when the checksum of the read data differs
--   from the one tracked by @'DiskSnapshot'@.
loadSnapshot ::
  forall blk m.
  ( LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  , IOLike m
  , LedgerSupportsInMemoryLedgerDB blk
  , GoodForLSM (LedgerState blk)
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
  Monad.when (snapshotBackend snapshotMeta /= UTxOHDMemSnapshot) $ do
    throwE $ InitFailureRead $ ReadMetadataError (snapshotToMetadataPath ds) MetadataBackendMismatch
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
                  (fromString $ show (dsNumber ds) <> (maybe "" (("_" <>) . show) (dsSuffix ds)))
                  "UTxO table"
            )
            LSM.closeTable
      Monad.when (checksumAsRead /= snapshotChecksum snapshotMeta) $
        throwE $
          InitFailureRead $
            ReadSnapshotDataCorruption
      (,pt) <$> lift (empty extLedgerSt values (newLSMLedgerTablesHandle tracer rr session))

serialiseLSMViaMemPack :: MemPack a => a -> LSM.RawBytes
serialiseLSMViaMemPack a =
  let barr = pack a
   in LSM.RawBytes (Vector 0 (PBA.sizeofByteArray barr) barr)

deserialiseLSMViaMemPack :: MemPack b => LSM.RawBytes -> b
deserialiseLSMViaMemPack (LSM.RawBytes (Vector _ _ barr)) = unpackError barr
