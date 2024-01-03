{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-unused-binds #-}

{- | snapshots

  Snapshotting a ledger state means saving a copy of the in-memory part of the
  ledger state serialized as a file on disk, as well as flushing differences on
  the ledger tables between the last snapshotted ledger state and the one that
  we are snapshotting now and making a copy of that resulting on-disk state.

  == Startup

  During initialisation, the goal is to construct an initial 'LedgerDB' where
  the sequence of in-memory states is empty except for the ledger state at the
  anchor or the 'DbChangelog', which has to correspond to the immutable tip,
  i.e., the block at the tip of the Immutable DB.

  Ideally, we can construct the initial 'LedgerDB' from a snapshot of the ledger
  state that we wrote to disk. Remember that updating a ledger state with a
  block is not invertible: we can apply a block to a ledger state, but we cannot
  /unapply/ a block to a ledger state. This means the snapshot has to be at
  least as old as the anchor. A snapshot matching the anchor can be used as is.
  A snapshot older than the anchor can be used after reapplying the necessary
  blocks. A snapshot newer than the anchor can /not/ be used, as we cannot
  unapply blocks to get the ledger state corresponding to the anchor. This is
  the reason why we only take snapshots of an immutable ledger state, i.e., of
  the anchor of the 'DbChangelog' (or older).

  On startup, the node will:

  1. Find the latest snapshot which will be a directory inside @\<cardano-node
       data dir\>\/\<ledger\>@ named as the slot number of the ledger state that
       was snapshotted:

       > <cardano-node data dir>
       > ├── volatile
       > ├── immutable
       > └── ledger
       >     ├── <snap_0>
       >     │   ├── tables
       >     │   └── state
       >     ├── ...
       >     └── <snap_n>
       >         ├── tables
       >         └── state

       The @tables@ file is a serialization of the in-memory part of the ledger
       state with empty tables (i.e. a @ExtLedgerState blk EmptyMK@), and
       @tables@ will store a persistent copy of the 'LedgerTable's. Depending on
       the 'BackingStore' implementation in use, this might be a file or a
       directory.

  2. Depending on the snapshots found, there are two possibilities:

       - If there is no snapshot to load, create a new @'BackingStore'@ with the
         contents of the Genesis ledger tables and finish.

       - If there is a snapshot found, then deserialize (with @DecodeDisk@) the
         @state@ file. If deserialization fails, delete this snapshot and start
         again. If the snapshot is newer than the immutable tip, delete this
         snapshot and start again.

       In case we found an snapshot, we will overwrite (either literally
       overwriting it or using some feature from the specific backend used) the
       @BackingStore@ tables with the contents from @tables@ from said snapshot
       as it was left in whatever state it was when the node shut down.

  3. The deserialized ledger state and tables will be then used as the initial
       ledger state for the ledger database.

  4. Reapply the immutable blocks after the snapshot to obtain the ledger state
       at the immutable tip. The blocks to reapply are streamed from the Immutable
       DB, using an iterator.

       Note that we can /reapply/ these blocks, which is quicker than applying
       them, as the existence of a snapshot newer than these blocks proves (unless
       the on-disk database has been tampered with, but this is not an attack we
       intend to protect against, as this would mean the machine has already been
       compromised) that they have been successfully applied in the past.

  Reading and applying blocks is costly. Typically, very few blocks need to be
  reapplied in practice. However, there is one exception: when the serialisation
  format of the ledger state changes, all snapshots (written using the old
  serialisation format) will fail to deserialise, and all blocks starting from
  genesis will have to be reapplied.

  At this point, the node carries a @DbChangelog@ that is initialized and ready
  to be applied blocks on the volatile database.

  == Taking snapshots during normal operation

  Snapshots are taken by the @'copyAndSnapshotRunner'@ when the disk policy
  dictates to do so. Whenever the chain grows past @k@ blocks, said runner will
  copy the blocks which are more than @k@ blocks from the tip (i.e. the ones
  that must be considered immutable) to the immutable database and then:

  1. Every time we have processed a specific amount of blocks since the last
       flush (set by default to 100), perform a flush of differences in the
       'DbChangelog' up to the immutable db tip.

  2. If dictated by the disk policy, flush immediately all the differences up to
       the immutable db tip and serialize (using 'EncodeDisk') the DbChangelog
       in-memory ledger states anchor (@ExtLedgerState blk EmptyMK@).

       A directory is created named after the slot number of the ledger state
       being snapshotted, and the serialization from above is written into the
       @\<slotNumber\>/state@ file and the @BackingStore@ tables are copied into
       the @\<slotNumber\>/tables@ file.

  3. There is a maximum number of snapshots that should exist in the disk at any
     time, dictated by the @DiskPolicy@, so if needed, we will trim out old
     snapshots.

  == Flush during startup and snapshot at the end of startup

  Due to the nature of the database having to carry around all the differences
  between the last snapshotted state and the current tip, there is a need to
  flush when replaying the chain as otherwise, for example on a replay from
  genesis to the tip, we would carry millions of differences in memory.

  Because of this, when we are replaying blocks we will flush regularly. As the
  last snapshot that was taken lives in a @\<slotNumber\>/tables@ file, there is
  no risk of destroying it (overwriting tables at another earlier snapshot) by
  flushing. Only when we finish replaying blocks and start the background
  threads (and specifically the @copyAndSnapshotRunner@), we will take a
  snapshot of the current immutable database anchor as described above.

-}
module Ouroboros.Consensus.Storage.LedgerDB.V1.Snapshots (
    decodeSnapshotBackwardsCompatible
  , deleteSnapshot
  , diskSnapshotIsTemporary
  , encodeSnapshot
  , listSnapshots
  , readSnapshot
  , snapshotToStatePath
  , snapshotToTablesPath
  , takeSnapshot
  , trimSnapshots
  , writeSnapshot
  ) where

import           Codec.CBOR.Decoding
import           Codec.CBOR.Encoding
import qualified Codec.CBOR.Write as CBOR
import qualified Codec.Serialise.Decoding as Dec
import           Control.Monad
import           Control.Monad.Except
import           Control.Tracer
import qualified Data.List as List
import           Data.Maybe
import           Data.Ord
import           Data.Set (Set)
import qualified Data.Set as Set
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Storage.LedgerDB.API
import           Ouroboros.Consensus.Storage.LedgerDB.API.DiskPolicy
import           Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.API
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Common
import           Ouroboros.Consensus.Storage.LedgerDB.V1.DbChangelog
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.CBOR (ReadIncrementalErr,
                     decodeWithOrigin, readIncremental)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Versioned
import           System.FS.API
import           System.FS.API.Types
import           Text.Read

-- | Named snapshot are permanent, they will never be deleted when trimming.
diskSnapshotIsPermanent :: DiskSnapshot -> Bool
diskSnapshotIsPermanent = isJust . dsSuffix

-- | The snapshots that are periodically created are temporary, they will be
-- deleted when trimming
diskSnapshotIsTemporary :: DiskSnapshot -> Bool
diskSnapshotIsTemporary = not . diskSnapshotIsPermanent

-- | Read snapshot from disk
readSnapshot ::
     forall m blk. IOLike m
  => SomeHasFS m
  -> (forall s. Decoder s (ExtLedgerState blk EmptyMK))
  -> (forall s. Decoder s (HeaderHash blk))
  -> DiskSnapshot
  -> ExceptT ReadIncrementalErr m (ExtLedgerState blk EmptyMK)
readSnapshot hasFS decLedger decHash = do
      ExceptT
    . readIncremental hasFS decoder
    . snapshotToStatePath
  where
    decoder :: Decoder s (ExtLedgerState blk EmptyMK)
    decoder = decodeSnapshotBackwardsCompatible (Proxy @blk) decLedger decHash

-- | List on-disk snapshots, highest number first.
listSnapshots :: Monad m => SomeHasFS m -> m [DiskSnapshot]
listSnapshots (SomeHasFS HasFS{listDirectory}) =
    aux <$> listDirectory (mkFsPath [])
  where
    aux :: Set String -> [DiskSnapshot]
    aux = List.sortOn (Down . dsNumber) . mapMaybe snapshotFromPath . Set.toList

-- | Try to take a snapshot of the /oldest ledger state/ in the ledger DB
--
-- We write the /oldest/ ledger state to disk because the intention is to only
-- write ledger states to disk that we know to be immutable. Primarily for
-- testing purposes, 'takeSnapshot' returns the block reference corresponding
-- to the snapshot that we wrote.
--
-- If a snapshot with the same number already exists on disk or if the tip is at
-- genesis, no snapshot is taken.
--
-- Note that an EBB can have the same slot number and thus snapshot number as
-- the block after it. This doesn't matter. The one block difference in the
-- ledger state doesn't warrant an additional snapshot. The number in the name
-- of the snapshot is only indicative, we don't rely on it being correct.
--
-- NOTE: This is a lower-level API that takes a snapshot independent from
-- whether this snapshot corresponds to a state that is more than @k@ back.
--
-- TODO: Should we delete the file if an error occurs during writing?
takeSnapshot ::
  forall m blk.
  ( IOLike m
  , LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  )
  => StrictTVar m (DbChangelog' blk)
  -> LedgerDBLock m
  -> CodecConfig blk
  -> Tracer m (TraceSnapshotEvent blk)
  -> SomeHasFS m
  -> BackingStore' m blk
  -> Maybe DiskSnapshot -- ^ Override for snapshot numbering
  -> m (Maybe (DiskSnapshot, RealPoint blk))
takeSnapshot ldbvar lock ccfg tracer hasFS ldbBackingStore dsOverride =
  withReadLock lock $ do
    state <- changelogLastFlushedState <$> readTVarIO ldbvar
    case pointToWithOriginRealPoint (castPoint (getTip state)) of
      Origin ->
        return Nothing
      NotOrigin t -> do
        let number   = unSlotNo (realPointSlot t)
            snapshot = fromMaybe (DiskSnapshot number Nothing) dsOverride
        diskSnapshots <- listSnapshots hasFS
        if List.any ((== number) . dsNumber) diskSnapshots then
          return Nothing
        else do
          writeSnapshot hasFS ldbBackingStore encodeExtLedgerState' snapshot state
          traceWith tracer $ TookSnapshot snapshot t
          return $ Just (snapshot, t)
  where
    encodeExtLedgerState' :: ExtLedgerState blk EmptyMK -> Encoding
    encodeExtLedgerState' = encodeExtLedgerState
                              (encodeDisk ccfg)
                              (encodeDisk ccfg)
                              (encodeDisk ccfg)

-- | Write snapshot to disk
writeSnapshot ::
     forall m blk. MonadThrow m
  => SomeHasFS m
  -> BackingStore' m blk
  -> (ExtLedgerState blk EmptyMK -> Encoding)
  -> DiskSnapshot
  -> ExtLedgerState blk EmptyMK
  -> m ()
writeSnapshot (SomeHasFS hasFS) backingStore encLedger snapshot cs = do
    createDirectory hasFS (snapshotToDirPath snapshot)
    withFile hasFS (snapshotToStatePath snapshot) (WriteMode MustBeNew) $ \h ->
      void $ hPut hasFS h $ CBOR.toBuilder (encoder cs)
    bsCopy
      backingStore
      (snapshotToTablesPath snapshot)
  where
    encoder :: ExtLedgerState blk EmptyMK -> Encoding
    encoder = encodeSnapshot encLedger

-- | Trim the number of on disk snapshots so that at most 'onDiskNumSnapshots'
-- snapshots are stored on disk. The oldest snapshots are deleted.
--
-- The deleted snapshots are returned.
trimSnapshots ::
     Monad m
  => Tracer m (TraceSnapshotEvent r)
  -> SomeHasFS m
  -> DiskPolicy
  -> m [DiskSnapshot]
trimSnapshots tracer hasFS DiskPolicy{onDiskNumSnapshots} = do
    -- We only trim temporary snapshots
    diskSnapshots <- filter diskSnapshotIsTemporary <$> listSnapshots hasFS
    -- The snapshot are most recent first, so we can simply drop from the
    -- front to get the snapshots that are "too" old.
    forM (drop (fromIntegral onDiskNumSnapshots) diskSnapshots) $ \snapshot -> do
      deleteSnapshot hasFS snapshot
      traceWith tracer $ DeletedSnapshot snapshot
      return snapshot

-- | Delete snapshot from disk
deleteSnapshot :: HasCallStack => SomeHasFS m -> DiskSnapshot -> m ()
deleteSnapshot (SomeHasFS HasFS{removeDirectoryRecursive}) = removeDirectoryRecursive . snapshotToDirPath

snapshotToDirName :: DiskSnapshot -> String
snapshotToDirName DiskSnapshot { dsNumber, dsSuffix } =
    show dsNumber <> suffix
  where
    suffix = case dsSuffix of
      Nothing -> ""
      Just s  -> "_" <> s

-- | The path within the LedgerDB's filesystem to the snapshot's directory
snapshotToDirPath :: DiskSnapshot -> FsPath
snapshotToDirPath = mkFsPath . (:[]) . snapshotToDirName

-- | The path within the LedgerDB's filesystem to the file that contains the
-- snapshot's serialized ledger state
snapshotToStatePath :: DiskSnapshot -> FsPath
snapshotToStatePath = mkFsPath . (\x -> [x, "state"]) . snapshotToDirName

-- | The path within the LedgerDB's filesystem to the directory that contains a
-- snapshot's backing store
snapshotToTablesPath :: DiskSnapshot -> FsPath
snapshotToTablesPath = mkFsPath . (\x -> [x, "tables"]) . snapshotToDirName

-- | The path within the LedgerDB's filesystem to the directory that contains the
-- backing store
_tablesPath :: FsPath
_tablesPath = mkFsPath ["tables"]

snapshotFromPath :: String -> Maybe DiskSnapshot
snapshotFromPath fileName = do
    number <- readMaybe prefix
    return $ DiskSnapshot number suffix'
  where
    (prefix, suffix) = break (== '_') fileName

    suffix' :: Maybe String
    suffix' = case suffix of
      ""      -> Nothing
      _ : str -> Just str

-- | Version 1: uses versioning ('Ouroboros.Consensus.Util.Versioned') and only
-- encodes the ledger state @l@.
snapshotEncodingVersion1 :: VersionNumber
snapshotEncodingVersion1 = 1

-- | Encoder to be used in combination with 'decodeSnapshotBackwardsCompatible'.
encodeSnapshot :: (l -> Encoding) -> l -> Encoding
encodeSnapshot encodeLedger l =
    encodeVersion snapshotEncodingVersion1 (encodeLedger l)

-- | To remain backwards compatible with existing snapshots stored on disk, we
-- must accept the old format as well as the new format.
--
-- The old format:
--
-- * The tip: @WithOrigin (RealPoint blk)@
--
-- * The chain length: @Word64@
--
-- * The ledger state: @l@
--
-- The new format is described by 'snapshotEncodingVersion1'.
--
-- This decoder will accept and ignore them. The encoder ('encodeSnapshot') will
-- no longer encode them.
decodeSnapshotBackwardsCompatible ::
     forall l blk.
     Proxy blk
  -> (forall s. Decoder s l)
  -> (forall s. Decoder s (HeaderHash blk))
  -> forall s. Decoder s l
decodeSnapshotBackwardsCompatible _ decodeLedger decodeHash =
    decodeVersionWithHook
      decodeOldFormat
      [(snapshotEncodingVersion1, Decode decodeVersion1)]
  where
    decodeVersion1 :: forall s. Decoder s l
    decodeVersion1 = decodeLedger

    decodeOldFormat :: Maybe Int -> forall s. Decoder s l
    decodeOldFormat (Just 3) = do
        _ <- withOriginRealPointToPoint <$>
               decodeWithOrigin (decodeRealPoint @blk decodeHash)
        _ <- Dec.decodeWord64
        decodeLedger
    decodeOldFormat mbListLen =
        fail $
          "decodeSnapshotBackwardsCompatible: invalid start " <>
          show mbListLen
