{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
-- |

module Ouroboros.Consensus.Storage.LedgerDB.V2.LSM
  ( -- * LedgerTablesHandle
    newLSMLedgerTablesHandle
  , GoodForLSM
    -- * Snapshots
  , loadSnapshot
  , snapshotToStatePath
  , takeSnapshot

  , tableFromValuesMK
  ) where

import Cardano.Binary as CBOR
import qualified Codec.CBOR.Write as CBOR
import Codec.Serialise (decode)
import qualified Control.Monad as Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Control.ResourceRegistry
import Control.Tracer
import Data.Functor.Contravariant ((>$<))
import Data.Functor.Identity
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.String (fromString)
import GHC.Generics
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsProtocol
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.Args as V2
import Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import Ouroboros.Consensus.Util.CBOR (readIncremental)
import Ouroboros.Consensus.Util.CRC
import Ouroboros.Consensus.Util.Enclose
import Ouroboros.Consensus.Util.IOLike
import System.FS.API
import System.FS.CRC
import Prelude hiding (read)
import Database.LSMTree (Table, Session)
import qualified Database.LSMTree as LSM
import Data.Void
import qualified Data.Vector as V
import qualified Data.Set as Set

type UTxOTable m l = Table m (TxIn l) (TxOut l) Void

data LedgerTablesHandleState m l
  = LedgerTablesHandleOpen !(UTxOTable m l)
  | LedgerTablesHandleClosed
  deriving Generic

deriving instance NoThunks (LedgerTablesHandleState m l)

instance NoThunks (Table m txin txout Void) where
  showTypeOf = undefined
  wNoThunks = undefined

data LSMClosedExn = LSMClosedExn
  deriving (Show, Exception)

type GoodForLSM l = ( LSM.SerialiseKey (TxIn l)
  , LSM.SerialiseValue (TxOut l)
  , LSM.ResolveValue (TxOut l)
  )

tableFromValuesMK ::
  IOLike m =>
  ( LSM.SerialiseKey (TxIn l)
  , LSM.SerialiseValue (TxOut l)
  , LSM.ResolveValue (TxOut l)
  )
  =>
  ResourceRegistry m -> Session m -> LedgerTables l ValuesMK -> m (ResourceKey m, Table m (TxIn l) (TxOut l) Void)
tableFromValuesMK rr session (LedgerTables (ValuesMK values)) = do
  res@(_, table) <- allocate rr (\_ -> LSM.newTable session) LSM.closeTable
  LSM.inserts table $ V.fromList $ [ (k, v, Nothing) | (k, v) <- Map.toList values ]
  pure res

guardClosed :: LedgerTablesHandleState m l -> (UTxOTable m l -> a) -> a
guardClosed LedgerTablesHandleClosed _ = error $ show LSMClosedExn
guardClosed (LedgerTablesHandleOpen st) f = f st

-- TODO what about era transitions

newLSMLedgerTablesHandle ::
  forall m l.
  ( IOLike m
  , HasLedgerTables l
  , CanUpgradeLedgerTables l
  , SerializeTablesWithHint l
  , LSM.SerialiseKey (TxIn l)
  , LSM.SerialiseValue (TxOut l)
  , LSM.ResolveValue (TxOut l)
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
          _ <- release resKey
          atomically $ writeTVar tv LedgerTablesHandleClosed
          traceWith tracer V2.TraceLedgerTablesHandleClose
      , duplicate = do
          hs <- readTVarIO tv
          !x <- guardClosed hs $ \t -> allocate rr (\_ -> LSM.duplicate t) LSM.closeTable >>= newLSMLedgerTablesHandle tracer rr session
          pure x
      , read = \(LedgerTables (KeysMK keys)) -> do
          hs <- readTVarIO tv
          guardClosed
            hs
            (\t -> do
                let keys' = Set.toList keys
                res <- LSM.lookups t (V.fromList keys')
                pure $ LedgerTables $ ValuesMK $ Map.fromList [ (k, v) | (k, LSM.Found v) <- zip keys' (V.toList res) ]
            )
      , readRange = undefined -- \(f, t) -> do
          -- hs <- readTVarIO tv
          -- guardClosed
          --   hs
          --   ( \(LedgerTables (ValuesMK m)) ->
          --       pure . LedgerTables . ValuesMK . Map.take t . (maybe id (\g -> snd . Map.split g) f) $ m
          --   )
      , readAll = undefined -- do
          -- hs <- readTVarIO tv
          -- guardClosed hs pure
      , pushDiffs = const (implPushDiffs tv)
      , takeHandleSnapshot = \_ snapshotName -> do
          -- TODO create the tables directory
          h <- readTVarIO tv
          guardClosed h $
            \table ->
              LSM.saveSnapshot (fromString snapshotName) "UTxO table" table >> pure Nothing
      , tablesSize = pure Nothing
      }

implPushDiffs ::
  ( LSM.SerialiseKey (TxIn l)
  , LSM.SerialiseValue (TxOut l)
  , LSM.ResolveValue (TxOut l)
  , IOLike m
  , HasLedgerTables l)
  => StrictTVar m (LedgerTablesHandleState m l) -> l DiffMK -> m ()
implPushDiffs tv !st1 = do
          let LedgerTables (DiffMK (Diff.Diff diffs)) = projectLedgerTables st1
          hs <- readTVarIO tv
          guardClosed
            hs
            ( \t ->
                 LSM.updates t $ V.fromList [ (k, case h of
                                     Diff.Insert v -> LSM.Insert v Nothing
                                     Diff.Delete -> LSM.Delete
                                   )
                                 | (k, h) <- Map.toList diffs ]
                )

-- | The path within the LedgerDB's filesystem to the file that contains the
-- snapshot's serialized ledger state
snapshotToStatePath :: DiskSnapshot -> FsPath
snapshotToStatePath = mkFsPath . (\x -> [x, "state"]) . snapshotToDirName

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
  crc2 <- takeHandleSnapshot (tables st) (state st) $ show $ dsNumber ds
  writeSnapshotMetadata fs ds $
    SnapshotMetadata
      { snapshotBackend = UTxOHDLSMSnapshot
      , snapshotChecksum = maybe crc1 (crcOfConcat crc1) crc2
      }

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
  , LSM.ResolveValue (TxOut (LedgerState blk))
  , LSM.SerialiseValue (TxOut (LedgerState blk))
  , LSM.SerialiseKey (TxIn (LedgerState blk))
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
      values <- lift $ allocate rr (\_ -> LSM.openTableFromSnapshot session (fromString $ show $ dsNumber ds) "UTxO table")
                            LSM.closeTable
      Monad.when (checksumAsRead /= snapshotChecksum snapshotMeta) $
        throwE $
          InitFailureRead $
            ReadSnapshotDataCorruption
      (,pt) <$> lift (empty extLedgerSt values (newLSMLedgerTablesHandle tracer rr session))
