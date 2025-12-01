{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
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
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
-- Needed for @NoThunks (Table m k v b)@
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Implementation of the 'LedgerTablesHandle' interface with LSM trees.
module Ouroboros.Consensus.Storage.LedgerDB.V2.LSM
  ( -- * Backend API
    LSM
  , Backend (..)
  , Args (LSMArgs)
  , Trace (LSMTreeTrace)
  , LSM.LSMTreeTrace (..)
  , mkLSMArgs
  , stdMkBlockIOFS
  --   -- * Streaming
  -- , YieldArgs (YieldLSM)
  -- , mkLSMYieldArgs
  -- , SinkArgs (SinkLSM)
  -- , mkLSMSinkArgs

    -- * Exported for tests
  , LSM.Salt
  , SomeHasFSAndBlockIO (..)
  ) where

import Cardano.Ledger.Coin
import Cardano.Ledger.Credential
import Cardano.Ledger.Keys (KeyRole (Staking))
import Codec.Serialise (decode)
import qualified Control.Monad as Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Control.ResourceRegistry
import Control.Tracer
import qualified Data.Foldable as Foldable
import Data.Functor.Contravariant ((>$<))
import Data.Functor.Product
import qualified Data.List as List
import qualified Data.List.Singletons as S
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.MemPack
import qualified Data.Primitive.ByteArray as PBA
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import qualified Data.SOP.Dict as Dict
import Data.SOP.Strict
import qualified Data.Set as Set
import qualified Data.Singletons as S
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text as Text
import Data.Typeable
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Primitive as VP
import Data.Void
import Database.LSMTree (Salt, Session)
import qualified Database.LSMTree as LSM
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
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
import Ouroboros.Consensus.Util (chunks)
import Ouroboros.Consensus.Util.CRC
import Ouroboros.Consensus.Util.Enclose
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.IndexedMemPack
import Ouroboros.Consensus.Util.TypeLevel
import qualified Streaming as S
import qualified Streaming.Prelude as S
import System.FS.API
import qualified System.FS.BlockIO.API as BIO
import System.FS.BlockIO.IO
import System.FilePath (splitDirectories, splitFileName)
import System.Random
import Prelude hiding (read)

-- | Type alias for convenience
type LSMUTxOTable m = LSM.Table m TxIn TxOutBytes Void

type LSMInstantStakeTable m = LSM.Table m (Credential 'Staking) (CompactForm Coin) Void

type family Value' table blk where
  Value' UTxOTable blk = TxOutBytes
  Value' table blk = Value table blk

newtype Table' m blk table = Table' {getTable' :: LSM.Table m (Key table) (Value' table blk) Void}

-- instance NoThunks (LSM.Table m txin txout Void) where
--   showTypeOf _ = "Table"
--   wNoThunks _ _ = pure Nothing

-- data LSMClosedExn = LSMClosedExn
--   deriving (Show, Exception)

data LSMTables m blk = LSMTables
  { getLSMTables :: NP (Table' m blk) (TablesForBlock blk)
  }

class
  ( LSM.SerialiseKey (Key table)
  , LSM.SerialiseValue (Value' table blk)
  , LSM.ResolveValue (Value' table blk)
  ) =>
  SerialiseTable l blk table
  where
  ser :: Proxy table -> Proxy blk -> l blk EmptyMK -> Value table blk -> Value' table blk
  des :: Proxy table -> Proxy blk -> l blk EmptyMK -> Value' table blk -> Value table blk

instance
  (IndexedMemPack l blk UTxOTable, IndexedValue l UTxOTable blk ~ TxOut blk) =>
  SerialiseTable l blk UTxOTable
  where
  ser _ _ = toTxOutBytes
  des _ _ = fromTxOutBytes

instance SerialiseTable l blk InstantStakeTable where
  ser _ _ _ = id
  des _ _ _ = id

{-------------------------------------------------------------------------------
  TxOuts
-------------------------------------------------------------------------------}

newtype TxOutBytes = TxOutBytes {unTxOutBytes :: LSM.RawBytes}

toTxOutBytes ::
  (IndexedMemPack l blk UTxOTable, IndexedValue l UTxOTable blk ~ TxOut blk) =>
  l blk EmptyMK -> TxOut blk -> TxOutBytes
toTxOutBytes st txout =
  let barr = indexedPackByteArray @UTxOTable True st txout
   in TxOutBytes $ LSM.RawBytes (VP.Vector 0 (PBA.sizeofByteArray barr) barr)

fromTxOutBytes ::
  (IndexedMemPack l blk UTxOTable, IndexedValue l UTxOTable blk ~ TxOut blk) =>
  l blk EmptyMK -> TxOutBytes -> TxOut blk
fromTxOutBytes st (TxOutBytes (LSM.RawBytes vec)) =
  case indexedUnpackEither @UTxOTable st vec of
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
deriving via LSM.ResolveAsFirst (CompactForm Coin) instance LSM.ResolveValue (CompactForm Coin)

{-------------------------------------------------------------------------------
  TxIns
-------------------------------------------------------------------------------}

instance LSM.SerialiseKey TxIn where
  serialiseKey = packRawBytes
  deserialiseKey = unpackRawBytes

instance LSM.SerialiseKey (Credential 'Staking) where
  serialiseKey = packRawBytes
  deserialiseKey = unpackRawBytes

instance LSM.SerialiseValue (CompactForm Coin) where
  serialiseValue = packRawBytes
  deserialiseValue = unpackRawBytes

packRawBytes :: MemPack x => x -> LSM.RawBytes
packRawBytes x =
  let barr = packByteArray True x
   in LSM.RawBytes (VP.Vector 0 (PBA.sizeofByteArray barr) barr)

unpackRawBytes :: MemPack x => LSM.RawBytes -> x
unpackRawBytes (LSM.RawBytes vec) =
  case unpackEither vec of
    Left err ->
      error $
        unlines
          [ "There was an error deserializing a TxIn from the LSM backend."
          , "This will likely result in a restart-crash loop."
          , "The error: " <> show err
          ]
    Right v -> v

-- newtype TxInBytes = TxInBytes {unTxInBytes :: LSM.RawBytes}

-- toTxInBytes :: TxIn -> TxInBytes
-- toTxInBytes txin =
--   let barr = packByteArray True txin
--    in TxInBytes $ LSM.RawBytes (VP.Vector 0 (PBA.sizeofByteArray barr) barr)

-- fromTxInBytes :: TxInBytes -> TxIn
-- fromTxInBytes (TxInBytes (LSM.RawBytes vec)) =
--   case unpackEither vec of
--     Left err ->
--       error $
--         unlines
--           [ "There was an error deserializing a TxIn from the LSM backend."
--           , "This will likely result in a restart-crash loop."
--           , "The error: " <> show err
--           ]
--     Right v -> v

-- instance LSM.SerialiseKey TxInBytes where
--   serialiseKey = unTxInBytes
--   deserialiseKey = TxInBytes

{-------------------------------------------------------------------------------
  LedgerTablesHandle
-------------------------------------------------------------------------------}

newLSMLedgerTablesHandle ::
  forall m l blk.
  ( IOLike m
  , HasLedgerTables l blk
  , ToAllDict (MemAndDiskTable l blk) (TablesForBlock blk)
  , All LSMTableLabel (TablesForBlock blk)
  ) =>
  Tracer m LedgerDBV2Trace ->
  (ResourceKey m, LSMTables m blk) ->
  m (LedgerTablesHandle m l blk)
newLSMLedgerTablesHandle tracer (origResKey, t) = do
  traceWith tracer TraceLedgerTablesHandleCreate
  tv <- newTVarIO origResKey
  pure
    LedgerTablesHandle
      { close = implClose tv
      , duplicate = \rr -> implDuplicate rr t tracer
      , read = implRead t
      , readRange = implReadRange t
      , readAll = implReadAll t
      , pushDiffs = implPushDiffs t
      , takeHandleSnapshot = implTakeHandleSnapshot t
      , tablesSize = pure Nothing
      , transfer = atomically . writeTVar tv
      }

{-# INLINE implClose #-}
{-# INLINE implDuplicate #-}
{-# INLINE implRead #-}
{-# INLINE implReadRange #-}
{-# INLINE implReadAll #-}
{-# INLINE implPushDiffs #-}
{-# INLINE implTakeHandleSnapshot #-}

implClose :: (HasCallStack, IOLike m) => StrictTVar m (ResourceKey m) -> m ()
implClose tv =
  Monad.void $ release =<< readTVarIO tv

implDuplicate ::
  ( IOLike m
  , HasLedgerTables l blk
  , ToAllDict (MemAndDiskTable l blk) (TablesForBlock blk)
  , All LSMTableLabel (TablesForBlock blk)
  ) =>
  ResourceRegistry m ->
  LSMTables m blk ->
  Tracer m LedgerDBV2Trace ->
  m (ResourceKey m, LedgerTablesHandle m l blk)
implDuplicate rr (LSMTables tbs) tracer = do
  (rk, tables) <-
    allocate
      rr
      (\_ -> LSMTables <$> htraverse' (fmap Table' . LSM.duplicate . getTable') tbs)
      ( \(LSMTables tbs') -> do
          traceWith tracer TraceLedgerTablesHandleClose
          Monad.void $ htraverse' (fmap K . LSM.closeTable . getTable') tbs'
      )
  (rk,) <$> newLSMLedgerTablesHandle tracer (rk, tables)

class
  ( TableConstraints blk table
  , SerialiseTable l blk table
  , LSMTableLabel table
  ) =>
  MemAndDiskTable l blk table
instance
  ( TableConstraints blk table
  , SerialiseTable l blk table
  , LSMTableLabel table
  ) =>
  MemAndDiskTable l blk table

implRead ::
  forall m l blk.
  ( IOLike m
  , ToAllDict (MemAndDiskTable l blk) (TablesForBlock blk)
  ) =>
  LSMTables m blk ->
  l blk EmptyMK ->
  LedgerTables blk KeysMK ->
  m (LedgerTables blk ValuesMK)
implRead (LSMTables tbs) st (LedgerTables np) =
  LedgerTables
    <$> let dictX = toAllDict @(MemAndDiskTable l blk) @(TablesForBlock blk)
         in withAllDict dictX
              $ hctraverse'
                (Proxy @(MemAndDiskTable l blk))
                f
              $ hzipWith Pair np tbs
 where
  f ::
    forall table.
    ( TableConstraints blk table
    , SerialiseTable l blk table
    ) =>
    Product
      (Table KeysMK blk)
      (Table' m blk)
      table ->
    m (Table ValuesMK blk table)
  f (Pair (Table (KeysMK keys)) (Table' lsmtb)) = do
    let vec' = V.create $ do
          vec <- VM.new (Set.size keys)
          Monad.foldM_
            (\i x -> VM.write vec i x >> pure (i + 1))
            0
            keys
          pure vec
    res <- LSM.lookups lsmtb vec'
    pure
      . Table
      . ValuesMK
      . Foldable.foldl'
        ( \m (k, item) ->
            case item of
              LSM.Found v -> Map.insert k (des (Proxy @table) (Proxy @blk) st v) m
              LSM.NotFound -> m
              LSM.FoundWithBlob{} -> m
        )
        Map.empty
      $ V.zip vec' res

implReadRange ::
  forall m l blk.
  ( IOLike m
  , ToAllDict (MemAndDiskTable l blk) (TablesForBlock blk)
  ) =>
  HasLedgerTables l blk =>
  LSMTables m blk ->
  l blk EmptyMK ->
  (Maybe TxIn, Int) ->
  m (LedgerTables blk ValuesMK, Maybe TxIn)
implReadRange = undefined -- table st (mPrev, num) = do
--  entries <- maybe cursorFromStart cursorFromKey mPrev
--  pure
--    ( LedgerTables
--        . ValuesMK
--        . V.foldl'
--          ( \m -> \case
--              LSM.Entry k v -> Map.insert (fromTxInBytes k) (fromTxOutBytes st v) m
--              LSM.EntryWithBlob{} -> m
--          )
--          Map.empty
--        $ entries
--    , case snd <$> V.unsnoc entries of
--        Nothing -> Nothing
--        Just (LSM.Entry k _) -> Just (fromTxInBytes k)
--        Just (LSM.EntryWithBlob k _ _) -> Just (fromTxInBytes k)
--    )
-- where
--  cursorFromStart = LSM.withCursor table (LSM.take num)
--  -- Here we ask for one value more and we drop one value because the
--  -- cursor returns also the key at which it was opened.
--  cursorFromKey k = fmap (V.drop 1) $ LSM.withCursorAtOffset table (toTxInBytes k) (LSM.take $ num + 1)

implReadAll ::
  ( IOLike m
  , HasLedgerTables l blk
  , ToAllDict (MemAndDiskTable l blk) (TablesForBlock blk)
  ) =>
  LSMTables m blk ->
  l blk EmptyMK ->
  m (LedgerTables blk ValuesMK)
implReadAll t st =
  let readAll' m = do
        (v, n) <- implReadRange t st (m, 100000)
        maybe (pure v) (fmap (ltliftA2 unionValues v) . readAll' . Just) n
   in readAll' Nothing

implPushDiffs ::
  forall m l mk blk.
  ( IOLike m
  , HasLedgerTables l blk
  , ToAllDict (MemAndDiskTable l blk) (TablesForBlock blk)
  ) =>
  LSMTables m blk ->
  l blk mk ->
  l blk DiffMK ->
  m ()
implPushDiffs (LSMTables lsmtbs) _ !st1 = do
  let LedgerTables np = projectLedgerTables st1
  Monad.void $
    let dictX = toAllDict @(MemAndDiskTable l blk) @(TablesForBlock blk)
     in withAllDict dictX
          $ hctraverse'
            (Proxy @(MemAndDiskTable l blk))
            g
          $ hzipWith Pair np lsmtbs
 where
  g ::
    forall table.
    SerialiseTable l blk table =>
    Product
      (Table DiffMK blk)
      (Table' m blk)
      table ->
    m (K () table)
  g (Pair (Table (DiffMK (Diff.Diff diffs))) (Table' lsmt)) =
    do
      let vec = V.create $ do
            vec' <- VM.new (Map.size diffs)
            Monad.foldM_
              (\idx (k, item) -> VM.write vec' idx (k, (f item)) >> pure (idx + 1))
              0
              $ Map.toList diffs
            pure vec'
      K
        <$> LSM.updates
          lsmt
          vec
   where
    f (Diff.Insert v) = LSM.Insert (ser (Proxy @table) (Proxy @blk) (forgetLedgerTables st1) v) Nothing
    f Diff.Delete = LSM.Delete

implTakeHandleSnapshot ::
  forall m blk t a.
  (All LSMTableLabel (TablesForBlock blk), IOLike m) => LSMTables m blk -> t -> String -> m (Maybe a)
implTakeHandleSnapshot (LSMTables tbs) _ snapshotName = do
  Monad.void $ hctraverse' (Proxy @LSMTableLabel) f tbs
  pure Nothing
 where
  f :: forall table. LSMTableLabel table => Table' m blk table -> m (K () table)
  f (Table' t) =
    K
      <$> LSM.saveSnapshot
        (fromString snapshotName)
        (lsmTableLabel (Proxy @table))
        t

type LSMTableLabel :: TABLE -> Constraint
class LSMTableLabel table where
  lsmTableLabel :: Proxy table -> LSM.SnapshotLabel

instance LSMTableLabel UTxOTable where
  lsmTableLabel _ = LSM.SnapshotLabel $ Text.pack $ "UTxO table"

instance LSMTableLabel InstantStakeTable where
  lsmTableLabel _ = LSM.SnapshotLabel $ Text.pack $ "InstantStake table"

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
  SnapshotManager m m blk (StateRef m ExtLedgerState blk)
snapshotManager session ccfg tracer fs =
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
        , snapshotTablesCodecVersion = TablesCodecVersion1
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
  , ToAllDict (MemAndDiskTable ExtLedgerState blk) (TablesForBlock blk)
  , All LSMTableLabel (TablesForBlock blk)
  ) =>
  Tracer m LedgerDBV2Trace ->
  ResourceRegistry m ->
  CodecConfig blk ->
  SomeHasFS m ->
  Session m ->
  DiskSnapshot ->
  ExceptT (SnapshotFailure blk) m (LedgerSeq' m blk, RealPoint blk)
loadSnapshot tracer rr ccfg fs@(SomeHasFS hfs) session ds =
  do
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
      withExceptT
        (InitFailureRead . ReadSnapshotFailed)
        $ readExtLedgerState fs (decodeDiskExtLedgerState ccfg) decode (snapshotToStatePath ds)
    case pointToWithOriginRealPoint (castPoint (getTip extLedgerSt)) of
      Origin -> throwE InitFailureGenesis
      NotOrigin pt -> do
        (rk, values) <-
          lift $
            allocate
              rr
              ( \_ ->
                  fmap LSMTables $
                    htraverse' unComp $
                      foldSing (toAllDict @(MemAndDiskTable ExtLedgerState blk) @(TablesForBlock blk))
              )
              ( \(LSMTables tbs) -> do
                  traceWith tracer TraceLedgerTablesHandleClose
                  Monad.void $ htraverse' (fmap K . LSM.closeTable . getTable') tbs
              )
        Monad.when
          (checksumAsRead /= snapshotChecksum snapshotMeta)
          $ throwE
          $ InitFailureRead
            ReadSnapshotDataCorruption
        (,pt)
          <$> lift (empty extLedgerSt (rk, values) (newLSMLedgerTablesHandle tracer))
 where
  f ::
    forall table.
    Dict.Dict (MemAndDiskTable ExtLedgerState blk) table -> m (Table' m blk table)
  f Dict.Dict =
    Table'
      <$> LSM.openTableFromSnapshot
        session
        (fromString $ snapshotToDirName ds)
        (lsmTableLabel (Proxy @table))

  foldSing :: AllDict (MemAndDiskTable ExtLedgerState blk) tables -> NP (m :.: Table' m blk) tables
  foldSing ANil = Nil
  foldSing (ACons tb tbNext) = Comp (f tb) :* foldSing tbNext

-- | Create the initial LSM table from values, which should happen only at
-- Genesis.
tableFromValuesMK ::
  forall m l blk.
  (IOLike m, All (SerialiseTable l blk) (TablesForBlock blk)) =>
  Tracer m LedgerDBV2Trace ->
  ResourceRegistry m ->
  Session m ->
  l blk EmptyMK ->
  LedgerTables blk ValuesMK ->
  m (ResourceKey m, LSMTables m blk)
tableFromValuesMK tracer rr session st (LedgerTables tbs) = do
  (rk, lsmtbs) <-
    allocate
      rr
      (\_ -> htraverse' unComp (hpure (Comp $ Table' <$> LSM.newTable session)))
      ( \tb -> do
          traceWith tracer TraceLedgerTablesHandleClose
          Monad.void $ htraverse' (fmap K . LSM.closeTable . getTable') tb
      )
  Monad.void $ hctraverse' (Proxy @(SerialiseTable l blk)) go $ hzipWith Pair tbs lsmtbs
  pure (rk, LSMTables lsmtbs)
 where
  go ::
    SerialiseTable l blk table =>
    Product
      (Table ValuesMK blk)
      (Table' m blk)
      table ->
    m (K () table)
  go (Pair (Table (ValuesMK values)) lsmt) =
    fmap K $ mapM_ (go' lsmt) $ chunks 1000 $ Map.toList values

  go' ::
    forall table.
    SerialiseTable l blk table =>
    Table' m blk table ->
    [(Key table, Value table blk)] ->
    m ()
  go' (Table' table) items =
    LSM.inserts table $
      V.fromListN (length items) $
        map (\(k, v) -> (k, ser (Proxy @table) (Proxy @blk) st v, Nothing)) items

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

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

type data LSM

-- | Create arguments for initializing the LedgerDB using the LSM-trees backend.
mkLSMArgs ::
  ( LedgerSupportsProtocol blk
  , LedgerDbSerialiseConstraints blk
  , ToAllDict
      (MemAndDiskTable ExtLedgerState blk)
      (TablesForBlock blk)
  , All LSMTableLabel (TablesForBlock blk)
  , All (SerialiseTable ExtLedgerState blk) (TablesForBlock blk)
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
  , HasLedgerTables LedgerState blk
  , ToAllDict
      (MemAndDiskTable ExtLedgerState blk)
      (TablesForBlock blk)
  , All LSMTableLabel (TablesForBlock blk)
  , All (SerialiseTable ExtLedgerState blk) (TablesForBlock blk)
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

  data Trace LSM
    = LSMTreeTrace !LSM.LSMTreeTrace
    deriving Show

  mkResources _ trcr (LSMArgs path salt mkFS) reg _ = do
    (rk1, SomeHasFSAndBlockIO fs blockio) <- mkFS reg
    createDirectoryIfMissing fs True path
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
    newLSMLedgerTablesHandle trcr table

  snapshotManager _ res = Ouroboros.Consensus.Storage.LedgerDB.V2.LSM.snapshotManager (sessionResource res)

-- instance
--   ( IndexedMemPack l blk table
--   , IOLike m
--   ) =>
--   StreamingBackend m LSM l
--   where
--   data YieldArgs m LSM l
--     = -- \| Yield an LSM snapshot
--       YieldLSM
--         Int
--         (LedgerTablesHandle m l)

--   data SinkArgs m LSM l
--     = SinkLSM
--         -- \| Chunk size
--         Int
--         -- \| Snap name
--         String
--         (Session m)

--   yield _ (YieldLSM chunkSize hdl) = yieldLsmS chunkSize hdl

--   sink _ (SinkLSM chunkSize snapName session) = sinkLsmS chunkSize snapName session

data SomeHasFSAndBlockIO m where
  SomeHasFSAndBlockIO ::
    (Eq h, Typeable h) => HasFS m h -> BIO.HasBlockIO m h -> SomeHasFSAndBlockIO m

instance IOLike m => NoThunks (Resources m LSM) where
  wNoThunks ctxt (LSMResources sk _ bk) = wNoThunks ctxt sk >> wNoThunks ctxt bk

{-------------------------------------------------------------------------------
  Streaming
-------------------------------------------------------------------------------}

-- yieldLsmS ::
--   Monad m =>
--   Int ->
--   LedgerTablesHandle m l blk ->
--   Yield m l
-- yieldLsmS readChunkSize tb hint k = do
--   r <- k (go (Nothing, readChunkSize))
--   lift $ S.effects r
--  where
--   go p = do
--     (LedgerTables (ValuesMK values), mx) <- lift $ S.lift $ readRange tb hint p
--     if Map.null values
--       then pure $ pure Nothing
--       else do
--         S.each $ Map.toList values
--         go (mx, readChunkSize)

-- sinkLsmS ::
--   forall m l blk.
--   ( MonadAsync m
--   , MonadMVar m
--   , MonadThrow (STM m)
--   , MonadMask m
--   , MonadST m
--   , MonadEvaluate m
--   , MemPack (TxIn l)
--   , IndexedMemPack l blk UTxOTable
--   ) =>
--   Int ->
--   String ->
--   Session m ->
--   Sink m l
-- sinkLsmS writeChunkSize snapName session st s = do
--   tb :: LSMUTxOTable m <- lift $ LSM.newTable session
--   r <- go tb writeChunkSize mempty s
--   lift $
--     LSM.saveSnapshot
--       (LSM.toSnapshotName snapName)
--       (LSM.SnapshotLabel $ T.pack "UTxO table")
--       tb
--   lift $ LSM.closeTable tb
--   pure (fmap (,Nothing) r)
--  where
--   go tb 0 m s' = do
--     lift $
--       LSM.inserts tb $
--         V.fromList [(toTxInBytes k, toTxOutBytes st v, Nothing) | (k, v) <- m]
--     go tb writeChunkSize mempty s'
--   go tb n m s' = do
--     mbs <- S.uncons s'
--     case mbs of
--       Nothing -> do
--         lift $
--           LSM.inserts tb $
--             V.fromList
--               [(toTxInBytes k, toTxOutBytes st v, Nothing) | (k, v) <- m]
--         S.effects s'
--       Just (item, s'') -> go tb (n - 1) (item : m) s''

-- -- | Create Yield arguments for LSM
-- mkLSMYieldArgs ::
--   ( IOLike m
--   , HasLedgerTables l blk
--   , IndexedMemPack l blk UTxOTable
--   ) =>
--   -- | The filepath in which the LSM database lives. Must not have a trailing slash!
--   FilePath ->
--   -- | The complete name of the snapshot to open, so @<slotno>[_<suffix>]@.
--   String ->
--   -- | Usually 'stdMkBlockIOFS'
--   (FilePath -> ResourceRegistry m -> m (a, SomeHasFSAndBlockIO m)) ->
--   -- | Usually 'newStdGen'
--   (m StdGen) ->
--   l EmptyMK ->
--   ResourceRegistry m ->
--   m (YieldArgs m LSM l)
-- mkLSMYieldArgs fp snapName mkFS mkGen _ reg = do
--   (_, SomeHasFSAndBlockIO hasFS blockIO) <- mkFS fp reg
--   salt <- fst . genWord64 <$> mkGen
--   (_, session) <-
--     allocate reg (\_ -> LSM.openSession nullTracer hasFS blockIO salt (mkFsPath [])) LSM.closeSession
--   tb <-
--     allocate
--       reg
--       ( \_ ->
--           LSM.openTableFromSnapshot
--             session
--             (LSM.toSnapshotName snapName)
--             (LSM.SnapshotLabel $ T.pack "UTxO table")
--       )
--       LSM.closeTable
--   YieldLSM 1000 <$> newLSMLedgerTablesHandle nullTracer tb

-- -- | Create Sink arguments for LSM
-- mkLSMSinkArgs ::
--   IOLike m =>
--   -- | The filepath in which the LSM database should be opened. Must not have a trailing slash!
--   FilePath ->
--   -- | The complete name of the snapshot to be created, so @<slotno>[_<suffix>]@.
--   String ->
--   -- | Usually 'stdMkBlockIOFS'
--   (FilePath -> ResourceRegistry m -> m (a, SomeHasFSAndBlockIO m)) ->
--   -- | Usually 'newStdGen'
--   (m StdGen) ->
--   l EmptyMK ->
--   ResourceRegistry m ->
--   m (SinkArgs m LSM l)
-- mkLSMSinkArgs
--   (splitFileName -> (fp, lsmDir))
--   snapName
--   mkFS
--   mkGen
--   _
--   reg =
--     do
--       (_, SomeHasFSAndBlockIO hasFS blockIO) <- mkFS fp reg
--       removeDirectoryRecursive hasFS lsmFsPath
--       createDirectory hasFS lsmFsPath
--       salt <- fst . genWord64 <$> mkGen
--       (_, session) <-
--         allocate reg (\_ -> LSM.newSession nullTracer hasFS blockIO salt lsmFsPath) LSM.closeSession
--       pure (SinkLSM 1000 snapName session)
--    where
--     lsmFsPath = mkFsPath [lsmDir]
