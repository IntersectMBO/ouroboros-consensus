{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Backends.LSM
  ( lsmBackendArgsIO
  , mkLSMFactory
  , mkLSMFromSnapshot
  , readUTxOSizeFile
  ) where

import qualified Cardano.Ledger.Core as SL
import qualified Cardano.Ledger.Shelley.API as SL
import Control.Exception
import qualified Control.Monad as Monad
import Control.Monad.Except
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Control.ResourceRegistry
import Data.ByteString (toStrict)
import qualified Data.ByteString.Builder as BS
import Data.ByteString.Char8 (readInt)
import qualified Data.Foldable as Foldable
import Data.Functor.Contravariant ((>$<))
import qualified Data.Map.Strict as Map
import Data.MemPack
import qualified Data.Primitive.ByteArray as PBA
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Primitive as VP
import Data.Void
import Data.Word
import qualified Database.LSMTree as LSM
import Lens.Micro
import Ouroboros.Consensus.Backends (loadSnapshot, mkSnapshotManager)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, CardanoEras)
import Ouroboros.Consensus.Cardano.CanHardFork (CardanoHardForkConstraints)
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common (SerialiseHFC)
import Ouroboros.Consensus.Ledger.Basics
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Ouroboros.Consensus.Shelley.Ledger.Ledger
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.V2.Backend
  ( BackendResources (..)
  , LedgerDBV2Trace (..)
  , LedgerDbBackendArgs (..)
  , SomeBackendTrace (..)
  )
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.IOLike
import System.FS.API
import System.FS.API.Lazy
import qualified System.FS.BlockIO.API as BIO
import System.FS.BlockIO.IO

{-------------------------------------------------------------------------------
  Backend wiring
-------------------------------------------------------------------------------}

-- | Construct the 'LedgerDbBackendArgs' for the LSM-backed Cardano backend.
--
-- Allocates a 'BlockIOFS' and an 'LSM.Session' into the temporary registry;
-- both are torn down by 'brRelease'.
lsmBackendArgsIO ::
  forall c.
  (CardanoHardForkConstraints c, SerialiseHFC (CardanoEras c)) =>
  FsPath -> FilePath -> Word64 -> LedgerDbBackendArgs IO (CardanoBlock c)
lsmBackendArgsIO lsmPath fastStoragePath salt = LedgerDbBackendArgs $ \trcr shfs -> do
  (fs, blockio) <-
    allocateTemp
      (ioHasBlockIO (MountPoint fastStoragePath) defaultIOCtxParams)
      (\(_, bio) -> BIO.close bio >> pure True)
      impossibleToNotTransfer
  lift $ createDirectoryIfMissing fs True lsmPath
  session <-
    allocateTemp
      ( LSM.openSession
          (BackendTrace . SomeBackendTrace >$< trcr)
          fs
          blockio
          salt
          lsmPath
      )
      (\s -> LSM.closeSession s >> pure True)
      impossibleToNotTransfer
  let
    mkH :: MkHandle IO
    mkH = mkLSMFactory session shfs

    mkHs :: MkHandleFromSnapshot IO
    mkHs = mkLSMFromSnapshot session shfs
   in
    pure
      BackendResources
        { brLoadSnapshot = loadSnapshot UTxOHDLSMSnapshot mkHs mkH
        , brSnapshotManager = mkSnapshotManager UTxOHDLSMSnapshot
        , brRelease = do
            LSM.closeSession session
            BIO.close blockio
        , ledgerTablesFactory = mkH
        }

-- | Type alias for convenience
type UTxOTable m = LSM.Table m TxInBytes TxOutBytes Void

instance NoThunks (LSM.Table m txin txout Void) where
  showTypeOf _ = "Table"
  wNoThunks _ _ = pure Nothing

data LSMClosedExn = LSMClosedExn
  deriving (Show, Exception)

{-------------------------------------------------------------------------------
  TxOuts
-------------------------------------------------------------------------------}

newtype TxOutBytes = TxOutBytes {unTxOutBytes :: LSM.RawBytes}

toTxOutBytes :: MemPack (SL.TxOut era) => Proxy era -> SL.TxOut era -> TxOutBytes
toTxOutBytes _ txout =
  let barr = packByteArray True txout
   in TxOutBytes $ LSM.RawBytes (VP.Vector 0 (PBA.sizeofByteArray barr) barr)

fromTxOutBytes :: MemPack (SL.TxOut era) => Proxy era -> TxOutBytes -> SL.TxOut era
fromTxOutBytes _ (TxOutBytes (LSM.RawBytes vec)) =
  case unpack vec of
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

toTxInBytes :: SL.TxIn -> TxInBytes
toTxInBytes txin =
  let barr = packByteArray True txin
   in TxInBytes $ LSM.RawBytes (VP.Vector 0 (PBA.sizeofByteArray barr) barr)

fromTxInBytes :: TxInBytes -> SL.TxIn
fromTxInBytes (TxInBytes (LSM.RawBytes vec)) =
  case unpack vec of
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
 LSM table management
-------------------------------------------------------------------------------}

newLSMTablesHandle ::
  forall m era.
  (IOLike m, MemPack (SL.TxOut era), Eq (SL.TxOut era)) =>
  SomeHasFS m -> SL.NewEpochState era -> Word64 -> UTxOTable m -> m (TablesHandle m era)
newLSMTablesHandle shfs@(SomeHasFS fs) st utxoSize table = do
  pure
    TablesHandle
      { stateWith = \txins -> do
          tbs <- implRead table txins
          pure (st & slUtxoL .~ tbs)
      , stateWithUTxO = \utxos -> st & slUtxoL .~ utxos
      , applyDiff = \d -> do
          handle' <- implApplyDiff shfs table utxoSize st d
          LSM.closeTable table
          pure handle'
      , duplWithDiffs = implDuplicateWithDiffs shfs table utxoSize
      , duplicateHandle = LSM.duplicate table >>= newLSMTablesHandle shfs st utxoSize
      , readUTxOWhole = drainTableFiltered table (const True)
      , readUTxOFiltered = drainTableFiltered table
      , readTxOuts = implRead table
      , closeHandle = LSM.closeTable table
      , getStatsHandle = Statistics $ fromIntegral utxoSize
      , takeHandleSnapshot = \ds -> do
          LSM.saveSnapshot
            (fromString (snapshotToDirName ds))
            (LSM.SnapshotLabel $ Text.pack $ "UTxO table")
            table
          writeUTxOSizeFile fs (snapshotToUTxOSizeFilePath ds) $ fromIntegral utxoSize
          pure (Nothing, UTxOHDLSMSnapshot)
      , castHandle = \st' -> LSM.duplicate table >>= newLSMTablesHandle shfs st' utxoSize
      , injectValues = \nes -> do
          let (SL.UTxO utxos, nes') = nes & slUtxoL <<.~ SL.UTxO Map.empty
          table' <- LSM.duplicate table
          mapM_ (go table') . chunks 1000 . Map.toList $ utxos
          newLSMTablesHandle shfs nes' (utxoSize + fromIntegral (Map.size utxos)) table'
      }
 where
  go ::
    (MemPack (SL.TxOut era), Eq (SL.TxOut era)) => UTxOTable m -> [(SL.TxIn, SL.TxOut era)] -> m ()
  go table' items =
    LSM.inserts table' $
      V.fromListN (length items) $
        map (\(k, v) -> (toTxInBytes k, toTxOutBytes (Proxy @era) v, Nothing)) items

snapshotToUTxOSizeFilePath :: DiskSnapshot -> FsPath
snapshotToUTxOSizeFilePath ds = snapshotToDirPath ds </> mkFsPath ["utxoSize"]

writeUTxOSizeFile :: MonadThrow f => HasFS f h -> FsPath -> Int -> f ()
writeUTxOSizeFile hasFs p sz =
  Monad.void $ withFile hasFs p (WriteMode MustBeNew) $ \h ->
    hPutAll hasFs h $ BS.toLazyByteString $ BS.intDec sz

-- | Page size for full-table cursor scans (used by 'readUTxOWhole' and
-- 'readUTxOFiltered'). Matches the chunk size used by the older
-- 'implReadAll' in @ouroboros-consensus-lsm@.
drainBatchSize :: Int
drainBatchSize = 100000

-- | Scan an entire 'UTxOTable' via a cursor, keeping only those entries whose
-- 'SL.TxOut' satisfies the supplied predicate.
drainTableFiltered ::
  forall m era.
  (IOLike m, MemPack (SL.TxOut era)) =>
  UTxOTable m ->
  (SL.TxOut era -> Bool) ->
  m (SL.UTxO era)
drainTableFiltered table p =
  LSM.withCursor table $ \cursor ->
    SL.UTxO <$> loop cursor Map.empty
 where
  loop :: LSM.Cursor m TxInBytes TxOutBytes Void -> Map.Map SL.TxIn (SL.TxOut era) -> m (Map.Map SL.TxIn (SL.TxOut era))
  loop !cursor !acc = do
    batch <- LSM.take drainBatchSize cursor
    let !acc' = V.foldl' step acc batch
    if V.length batch < drainBatchSize
      then pure acc'
      else loop cursor acc'

  step :: Map.Map SL.TxIn (SL.TxOut era) -> LSM.Entry TxInBytes TxOutBytes (LSM.BlobRef m Void) -> Map.Map SL.TxIn (SL.TxOut era)
  step !m e = case e of
    LSM.Entry k v ->
      let !txout = fromTxOutBytes (Proxy @era) v
       in if p txout
            then Map.insert (fromTxInBytes k) txout m
            else m
    LSM.EntryWithBlob{} -> m

implRead :: (IOLike m, MemPack (SL.TxOut era)) => UTxOTable m -> Set SL.TxIn -> m (SL.UTxO era)
implRead table keys = do
  let vec' = V.create $ do
        vec <- VM.new (Set.size keys)
        Monad.foldM_
          (\i x -> VM.write vec i (toTxInBytes x) >> pure (i + 1))
          0
          keys
        pure vec
  res <- LSM.lookups table vec'
  pure
    . SL.UTxO
    . Foldable.foldl'
      ( \m (k, item) ->
          case item of
            LSM.Found v -> Map.insert (fromTxInBytes k) (fromTxOutBytes (Proxy) v) m
            LSM.NotFound -> m
            LSM.FoundWithBlob{} -> m
      )
      Map.empty
    $ V.zip vec' res

implApplyDiff ::
  (IOLike m, MemPack (SL.TxOut era), Eq (SL.TxOut era)) =>
  SomeHasFS m ->
  UTxOTable m ->
  Word64 ->
  SL.NewEpochState era ->
  Diff.Diff SL.TxIn (SL.TxOut era) ->
  m (TablesHandle m era)
implApplyDiff shfs table size st (Diff.Diff diffs) = do
  table' <- LSM.duplicate table
  let vec = V.create $ do
        vec' <- VM.new (Map.size diffs)
        Monad.foldM_
          (\idx (k, item) -> VM.write vec' idx (toTxInBytes k, (f item)) >> pure (idx + 1))
          0
          $ Map.toList diffs
        pure vec'
  let (ins, dels) =
        Map.foldl'
          ( \(i, d) delta -> case delta of
              Diff.Insert{} -> (i + 1, d)
              Diff.Delete -> (i, d + 1)
          )
          (0, 0)
          diffs
  let size' =
        assert (size + ins >= size) $
          assert (size + ins - dels <= size + ins) $
            size + ins - dels
  LSM.updates table' vec
  newLSMTablesHandle shfs st size' table'
 where
  f (Diff.Insert v) = LSM.Insert (toTxOutBytes (Proxy) v) Nothing
  f Diff.Delete = LSM.Delete

implDuplicateWithDiffs ::
  (IOLike m, MemPack (SL.TxOut era), Eq (SL.TxOut era)) =>
  SomeHasFS m ->
  UTxOTable m ->
  Word64 ->
  SL.NewEpochState era ->
  SL.NewEpochState era ->
  m (TablesHandle m era)
implDuplicateWithDiffs shfs table size st0 st1 = do
  let SL.UTxO utxo0 = st0 ^. slUtxoL
      (SL.UTxO utxo1, st1') = st1 & slUtxoL <<.~ SL.UTxO Map.empty
  implApplyDiff shfs table size st1' $ Diff.diff utxo0 utxo1

mkLSMFactory ::
  forall m. IOLike m => LSM.Session m -> SomeHasFS m -> MkHandle m
mkLSMFactory session shfs = MkHandle $ \st -> do
  table <- LSM.newTable session
  let SL.UTxO utxos = st ^. slUtxoL
  mapM_ (go table) $ chunks 1000 $ Map.toList utxos
  newLSMTablesHandle shfs st (fromIntegral (Map.size utxos)) table
 where
  go ::
    forall era.
    (MemPack (SL.TxOut era), Eq (SL.TxOut era)) => UTxOTable m -> [(SL.TxIn, SL.TxOut era)] -> m ()
  go table items =
    LSM.inserts table $
      V.fromListN (length items) $
        map (\(k, v) -> (toTxInBytes k, toTxOutBytes (Proxy @era) v, Nothing)) items

readUTxOSizeFile :: MonadThrow m => SomeHasFS m -> FsPath -> ExceptT () m Word64
readUTxOSizeFile (SomeHasFS hfs) p = do
  exists <- lift $ doesFileExist hfs p
  Monad.unless exists $ throwError ()
  maybeToExceptT () $
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

mkLSMFromSnapshot :: LSM.Session m -> SomeHasFS m -> MkHandleFromSnapshot m
mkLSMFromSnapshot session shfs = MkHandleFromSnapshot $ \ds st -> do
  msz <-
    withExceptT (const BackendCorruptedData) $ readUTxOSizeFile shfs (snapshotToUTxOSizeFilePath ds)
  h <-
    lift $
      LSM.openTableFromSnapshot
        session
        (fromString $ snapshotToDirName ds)
        (LSM.SnapshotLabel $ Text.pack $ "UTxO table")
  (,Nothing) <$> lift (newLSMTablesHandle shfs st msz h)
