{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
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

    -- * Snapshot streaming
  , lsmSnapshotYielder
  , lsmSnapshotSinker
  ) where

import qualified Cardano.Ledger.Core as SL
import qualified Cardano.Ledger.Shelley.API as SL
import Codec.CBOR.Read (DeserialiseFailure)
import Control.Exception (assert)
import qualified Control.Monad as Monad
import Control.Monad.Except
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Control.ResourceRegistry
import Control.Tracer (Tracer, nullTracer)
import Data.ByteString (ByteString, toStrict)
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
import Ouroboros.Consensus.Shelley.Ledger.SnapshotStream
  ( EntryStream
  , SnapshotSinker (..)
  , SnapshotYielder (..)
  )
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.V2.Backend
  ( BackendResources (..)
  , LedgerDBV2Trace (..)
  , LedgerDbBackendArgs (..)
  , SomeBackendTrace (..)
  )
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.Enclose (EnclosingTimed, encloseTimedWith)
import Ouroboros.Consensus.Util.IOLike
import Streaming (Of, Stream)
import qualified Streaming as S
import qualified Streaming.Prelude as S
import System.FS.API
import System.FS.API.Lazy
import qualified System.FS.BlockIO.API as BIO
import System.FS.BlockIO.IO
import System.FS.CRC (CRC)
import System.FS.IO (HandleIO)
import System.Random (genWord64, newStdGen)

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
      ( encloseTimedWith (lsmBackendTrace LSMOpenSession >$< trcr) $
          LSM.openSession
            (lsmBackendTrace LSMTreeTrace >$< trcr)
            fs
            blockio
            salt
            lsmPath
      )
      (\s -> LSM.closeSession s >> pure True)
      impossibleToNotTransfer
  let
    mkH :: MkHandle IO
    mkH = mkLSMFactory trcr session shfs

    mkHs :: MkHandleFromSnapshot IO
    mkHs = mkLSMFromSnapshot trcr session shfs
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

-- | LSM-specific backend events surfaced through 'BackendTrace'.
--
-- 'LSMOpenSession' / 'LSMLookup' / 'LSMUpdate' / 'LSMSnap' time the LSM
-- operations driven from this module. 'LSMTreeTrace' carries the structured
-- events emitted by the @lsm-tree@ session itself.
data LSMBackendTrace
  = LSMTreeTrace !LSM.LSMTreeTrace
  | LSMLookup !EnclosingTimed
  | LSMUpdate !EnclosingTimed
  | LSMSnap !EnclosingTimed
  | LSMOpenSession !EnclosingTimed
  deriving Show

-- | Wrap an 'LSMBackendTrace' in the existential 'SomeBackendTrace' so it can
-- flow through the LedgerDB tracer.
lsmBackendTrace :: (a -> LSMBackendTrace) -> a -> LedgerDBV2Trace
lsmBackendTrace f = BackendTrace . SomeBackendTrace . f

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
  Tracer m LedgerDBV2Trace ->
  SomeHasFS m ->
  SL.NewEpochState era ->
  Word64 ->
  UTxOTable m ->
  m (TablesHandle m era)
newLSMTablesHandle tracer shfs@(SomeHasFS fs) st utxoSize table = do
  pure
    TablesHandle
      { -- LSM keeps the UTxO on disk: read just the block's input keys
        -- via 'implRead' and place that subset into the supplied ticked
        -- NES. 'duplWithDiffs' will then diff subset-before against
        -- subset-after and push the block's changes to the table.
        stateWith = \keys nes -> do
          SL.UTxO utxos <- implRead tracer table keys
          pure $ nes & slUtxoL .~ SL.UTxO utxos
      , stateWithUTxO = \utxos -> st & slUtxoL .~ utxos
      , applyDiff = \d ->
          encloseTimedWith (TraceLedgerTablesHandlePush >$< tracer) $ do
            handle' <- implApplyDiff tracer shfs table utxoSize st d
            LSM.closeTable table
            pure handle'
      , duplWithDiffs = implDuplicateWithDiffs tracer shfs table utxoSize
      , duplicateHandle =
          encloseTimedWith (TraceLedgerTablesHandleDuplicate >$< tracer) (LSM.duplicate table)
            >>= newLSMTablesHandle tracer shfs st utxoSize
      , readUTxOWhole =
          encloseTimedWith (TraceLedgerTablesHandleRead >$< tracer) $
            drainTableFiltered table (const True)
      , readUTxOFiltered = \p ->
          encloseTimedWith (TraceLedgerTablesHandleRead >$< tracer) $
            drainTableFiltered table p
      , readTxOuts = implRead tracer table
      , closeHandle =
          encloseTimedWith (TraceLedgerTablesHandleClose >$< tracer) (LSM.closeTable table)
      , getStatsHandle = Statistics $ fromIntegral utxoSize
      , takeHandleSnapshot = \ds ->
          encloseTimedWith (lsmBackendTrace LSMSnap >$< tracer) $ do
            LSM.saveSnapshot
              (fromString (snapshotToDirName ds))
              (LSM.SnapshotLabel $ Text.pack $ "UTxO table")
              table
            writeUTxOSizeFile fs (snapshotToUTxOSizeFilePath ds) $ fromIntegral utxoSize
            pure (Nothing, UTxOHDLSMSnapshot)
      , castHandle = \st' ->
          encloseTimedWith (TraceLedgerTablesHandleDuplicate >$< tracer) (LSM.duplicate table)
            >>= newLSMTablesHandle tracer shfs st' utxoSize
      , injectValues = \nes ->
          encloseTimedWith (TraceLedgerTablesHandlePush >$< tracer) $ do
            let (SL.UTxO utxos, nes') = nes & slUtxoL <<.~ SL.UTxO Map.empty
            table' <- LSM.duplicate table
            mapM_ (go table') . chunks 1000 . Map.toList $ utxos
            newLSMTablesHandle tracer shfs nes' (utxoSize + fromIntegral (Map.size utxos)) table'
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
  loop ::
    LSM.Cursor m TxInBytes TxOutBytes Void ->
    Map.Map SL.TxIn (SL.TxOut era) ->
    m (Map.Map SL.TxIn (SL.TxOut era))
  loop !cursor !acc = do
    batch <- LSM.take drainBatchSize cursor
    let !acc' = V.foldl' step acc batch
    if V.length batch < drainBatchSize
      then pure acc'
      else loop cursor acc'

  step ::
    Map.Map SL.TxIn (SL.TxOut era) ->
    LSM.Entry TxInBytes TxOutBytes (LSM.BlobRef m Void) ->
    Map.Map SL.TxIn (SL.TxOut era)
  step !m e = case e of
    LSM.Entry k v ->
      let !txout = fromTxOutBytes (Proxy @era) v
       in if p txout
            then Map.insert (fromTxInBytes k) txout m
            else m
    LSM.EntryWithBlob{} -> m

implRead ::
  (IOLike m, MemPack (SL.TxOut era)) =>
  Tracer m LedgerDBV2Trace ->
  UTxOTable m ->
  Set SL.TxIn ->
  m (SL.UTxO era)
implRead tracer table keys =
  encloseTimedWith (TraceLedgerTablesHandleRead >$< tracer) $ do
    let vec' = V.create $ do
          vec <- VM.new (Set.size keys)
          Monad.foldM_
            (\i x -> VM.write vec i (toTxInBytes x) >> pure (i + 1))
            0
            keys
          pure vec
    res <-
      encloseTimedWith (lsmBackendTrace LSMLookup >$< tracer) $ LSM.lookups table vec'
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
  Tracer m LedgerDBV2Trace ->
  SomeHasFS m ->
  UTxOTable m ->
  Word64 ->
  SL.NewEpochState era ->
  Diff.Diff SL.TxIn (SL.TxOut era) ->
  m (TablesHandle m era)
implApplyDiff tracer shfs table size st (Diff.Diff diffs) = do
  table' <-
    encloseTimedWith (TraceLedgerTablesHandleDuplicate >$< tracer) $ LSM.duplicate table
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
  let !size' =
        assert (size + ins >= size) $
          assert (size + ins - dels <= size + ins) $
            size + ins - dels
  encloseTimedWith (lsmBackendTrace LSMUpdate >$< tracer) $ LSM.updates table' vec
  newLSMTablesHandle tracer shfs st size' table'
 where
  f (Diff.Insert v) = LSM.Insert (toTxOutBytes (Proxy) v) Nothing
  f Diff.Delete = LSM.Delete

implDuplicateWithDiffs ::
  (IOLike m, MemPack (SL.TxOut era), Eq (SL.TxOut era)) =>
  Tracer m LedgerDBV2Trace ->
  SomeHasFS m ->
  UTxOTable m ->
  Word64 ->
  SL.NewEpochState era ->
  SL.NewEpochState era ->
  m (SL.NewEpochState era, TablesHandle m era)
implDuplicateWithDiffs tracer shfs table size st0 st1 =
  encloseTimedWith (TraceLedgerTablesHandlePush >$< tracer) $ do
    let SL.UTxO utxo0 = st0 ^. slUtxoL
        (SL.UTxO utxo1, st1') = st1 & slUtxoL <<.~ SL.UTxO Map.empty
    -- 'st1'' has 'slUtxoL' cleared; it is also what we hand back for
    -- the outer 'ShelleyLedgerState' so the LedgerDB doesn't carry a
    -- per-block subset Map next to the on-disk table.
    h <- implApplyDiff tracer shfs table size st1' $ Diff.diff utxo0 utxo1
    pure (st1', h)

mkLSMFactory ::
  forall m.
  IOLike m =>
  Tracer m LedgerDBV2Trace ->
  LSM.Session m ->
  SomeHasFS m ->
  MkHandle m
mkLSMFactory tracer session shfs = MkHandle $ \st -> do
  table <-
    encloseTimedWith (TraceLedgerTablesHandleCreateFirst >$< tracer) $ LSM.newTable session
  let SL.UTxO utxos = st ^. slUtxoL
  mapM_ (go table) $ chunks 1000 $ Map.toList utxos
  newLSMTablesHandle tracer shfs st (fromIntegral (Map.size utxos)) table
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

mkLSMFromSnapshot ::
  IOLike m =>
  Tracer m LedgerDBV2Trace ->
  LSM.Session m ->
  SomeHasFS m ->
  MkHandleFromSnapshot m
mkLSMFromSnapshot tracer session shfs = MkHandleFromSnapshot $ \ds st -> do
  msz <-
    withExceptT (const BackendCorruptedData) $ readUTxOSizeFile shfs (snapshotToUTxOSizeFilePath ds)
  h <-
    lift $
      encloseTimedWith (TraceLedgerTablesHandleCreateFirst >$< tracer) $
        LSM.openTableFromSnapshot
          session
          (fromString $ snapshotToDirName ds)
          (LSM.SnapshotLabel $ Text.pack $ "UTxO table")
  -- LSM keeps UTxOs on disk only; the pure 'NewEpochState' carries an
  -- empty 'slUtxoL' (which is also what 'encodeShelleyLedgerState'
  -- writes), so we return @st@ unchanged.
  (st,,Nothing) <$> lift (newLSMTablesHandle tracer shfs st msz h)

{-------------------------------------------------------------------------------
  Snapshot streaming

  The LSM backend doesn't have a single on-disk file for the UTxO; the
  table is materialised by LSM's own snapshot machinery, plus a small
  @utxoSize@ file alongside it. The yielder paginates through an
  existing LSM snapshot via a cursor; the sinker builds a fresh table,
  inserts entries in chunks, then calls 'LSM.saveSnapshot' and writes
  the size sidecar.

  Neither side computes a CRC over the on-disk bytes (LSM owns the
  representation), so both ends of the running CRC pair come back as
  'Nothing'.
-------------------------------------------------------------------------------}

-- | Chunk size for LSM cursor reads / batched inserts during snapshot
-- streaming. Matches the chunk size used by the older streaming impl
-- on @main@.
streamChunkSize :: Int
streamChunkSize = 1000

-- | Open a BlockIO mount and return a release action for it.
acquireBlockIO :: FilePath -> IO (HasFS IO HandleIO, BIO.HasBlockIO IO HandleIO, IO ())
acquireBlockIO mountPoint = do
  (fs, blockio) <- ioHasBlockIO (MountPoint mountPoint) defaultIOCtxParams
  pure (fs, blockio, BIO.close blockio)

-- | Generate a fresh salt for opening an LSM session.
freshSalt :: IO LSM.Salt
freshSalt = fst . genWord64 <$> newStdGen

-- | Yield the contents of an LSM snapshot.
--
-- Opens a private BlockIO mount and LSM session for the duration of
-- the yielder; 'releaseYielder' closes both. Pages through the table
-- via a cursor in chunks of 'streamChunkSize'.
lsmSnapshotYielder ::
  -- | Filesystem mount point under which the LSM database lives.
  FilePath ->
  -- | Path of the LSM database within the mount.
  FsPath ->
  DiskSnapshot ->
  IO (SnapshotYielder IO)
lsmSnapshotYielder mountPoint dbPath ds = do
  (fs, blockio, releaseBlockIO) <- acquireBlockIO mountPoint
  salt <- freshSalt
  session <- LSM.openSession nullTracer fs blockio salt dbPath
  table <-
    LSM.openTableFromSnapshot
      session
      (fromString (snapshotToDirName ds))
      (LSM.SnapshotLabel (Text.pack "UTxO table"))
  pure
    SnapshotYielder
      { runYielder = \_nes k -> do
          residue <- k (entryStreamFromTable table)
          -- Drain the residue byte stream; for LSM it has no bytes,
          -- but whatever the sinker layered on top still needs to be
          -- consumed to deliver its CRC pair.
          lift $ S.effects residue
      , releaseYielder = do
          LSM.closeTable table
          LSM.closeSession session
          releaseBlockIO
      }

-- | Stream the contents of a LSM table in pages via a cursor.
--
-- The residue (inner) byte stream is empty; LSM does not have a
-- single-file representation, so there's no input CRC.
entryStreamFromTable ::
  forall era.
  MemPack (SL.TxOut era) =>
  UTxOTable IO ->
  EntryStream IO era (Maybe CRC)
entryStreamFromTable table =
  -- All real work happens in the outer 'Stream', which lives in
  -- 'ExceptT DeserialiseFailure IO'. Once the cursor is drained the
  -- residue is the trivial empty byte stream returning 'Nothing'.
  S.hoist lift (page Nothing) *> pure (pure Nothing)
 where
  page ::
    Maybe TxInBytes ->
    Stream (Of (SL.TxIn, SL.TxOut era)) IO ()
  page mLast = do
    batch <- lift $ readPage mLast
    if V.null batch
      then pure ()
      else do
        S.each (V.toList (V.mapMaybe entryToPair batch))
        case snd <$> V.unsnoc batch of
          Nothing -> pure ()
          Just (LSM.Entry k _) -> page (Just k)
          Just (LSM.EntryWithBlob k _ _) -> page (Just k)

  entryToPair :: LSM.Entry TxInBytes TxOutBytes (LSM.BlobRef IO Void) -> Maybe (SL.TxIn, SL.TxOut era)
  entryToPair = \case
    LSM.Entry k v -> Just (fromTxInBytes k, fromTxOutBytes (Proxy @era) v)
    LSM.EntryWithBlob{} -> Nothing

  -- Read one page worth of entries, starting strictly /after/ the
  -- supplied key (or from the start when none was supplied). The
  -- cursor at-offset variant includes the offset key, so we ask for
  -- one extra and drop it.
  readPage ::
    Maybe TxInBytes ->
    IO (V.Vector (LSM.Entry TxInBytes TxOutBytes (LSM.BlobRef IO Void)))
  readPage = \case
    Nothing -> LSM.withCursor table (LSM.take streamChunkSize)
    Just lastKey ->
      V.drop 1
        <$> LSM.withCursorAtOffset table lastKey (LSM.take (streamChunkSize + 1))

-- | Sink a stream of UTxO entries into a fresh LSM snapshot.
--
-- Creates a new database under @mountPoint\/dbPath@ (wiping any
-- existing one) at construction time, then on every 'runSinker' call:
-- opens a fresh table, drains the entry stream into it in chunks of
-- 'streamChunkSize', and writes both an LSM snapshot and the
-- @utxoSize@ sidecar through @snapshotsFs@ so the resulting snapshot
-- is loadable by 'mkLSMFromSnapshot'.
lsmSnapshotSinker ::
  -- | Filesystem mount point under which the LSM database will live.
  FilePath ->
  -- | Path of the LSM database within the mount.
  FsPath ->
  -- | Filesystem rooted at the snapshots directory, used to write the
  -- @utxoSize@ sidecar.
  SomeHasFS IO ->
  DiskSnapshot ->
  IO (SnapshotSinker IO)
lsmSnapshotSinker mountPoint dbPath (SomeHasFS snapHfs) ds = do
  (fs, blockio, releaseBlockIO) <- acquireBlockIO mountPoint
  salt <- freshSalt
  removeDirectoryRecursive fs dbPath
  createDirectory fs dbPath
  session <- LSM.newSession nullTracer fs blockio salt dbPath
  let releaseSession = do
        LSM.closeSession session
        releaseBlockIO
  pure
    SnapshotSinker
      { runSinker = \entries -> do
          residue <-
            bracketE
              (LSM.newTable session)
              LSM.closeTable
              ( \table -> do
                  (residue, utxosSize) <-
                    drainEntries table 0 streamChunkSize [] entries
                  lift $
                    LSM.saveSnapshot
                      (fromString (snapshotToDirName ds))
                      (LSM.SnapshotLabel (Text.pack "UTxO table"))
                      table
                  lift $
                    writeUTxOSizeFile
                      snapHfs
                      (snapshotToUTxOSizeFilePath ds)
                      utxosSize
                  pure residue
              )
          -- LSM has no single-file representation, so the output CRC
          -- the residue carries onwards is 'Nothing'.
          pure (fmap (,Nothing) residue)
      , releaseSinker = releaseSession
      }
 where
  -- 'ExceptT'-friendly bracket: like 'bracket' from "Control.Exception"
  -- but allocates and releases in the wrapped 'IO' rather than in
  -- 'ExceptT'. Used to make sure the open table is closed even when
  -- the stream-draining action throws.
  bracketE ::
    IO a ->
    (a -> IO ()) ->
    (a -> ExceptT DeserialiseFailure IO b) ->
    ExceptT DeserialiseFailure IO b
  bracketE acquire freeRes action = ExceptT $
    bracket acquire freeRes $
      \a -> runExceptT (action a)

-- | Drain an entry stream into the supplied table, flushing a batch
-- of 'streamChunkSize' rows at a time. Returns the byte-residue
-- stream that was attached to the typed stream and the running count
-- of entries written.
drainEntries ::
  forall era.
  MemPack (SL.TxOut era) =>
  UTxOTable IO ->
  -- | Running count of entries written so far.
  Int ->
  -- | Slots remaining in the current batch.
  Int ->
  -- | Accumulator for the current batch (in reverse order).
  [(SL.TxIn, SL.TxOut era)] ->
  EntryStream IO era (Maybe CRC) ->
  ExceptT
    DeserialiseFailure
    IO
    (Stream (Of ByteString) IO (Maybe CRC), Int)
drainEntries table = loop
 where
  loop !count 0 acc s = do
    lift $ flushBatch table acc
    loop count streamChunkSize [] s
  loop !count !slotsLeft acc s = do
    next <- S.next s
    case next of
      Left residue -> do
        lift $ flushBatch table acc
        pure (residue, count)
      Right (item, s') ->
        loop (count + 1) (slotsLeft - 1) (item : acc) s'

  flushBatch :: UTxOTable IO -> [(SL.TxIn, SL.TxOut era)] -> IO ()
  flushBatch t acc =
    LSM.inserts t $
      V.fromListN (length acc) $
        map
          ( \(k, v) ->
              (toTxInBytes k, toTxOutBytes (Proxy @era) v, Nothing)
          )
          (reverse acc)
