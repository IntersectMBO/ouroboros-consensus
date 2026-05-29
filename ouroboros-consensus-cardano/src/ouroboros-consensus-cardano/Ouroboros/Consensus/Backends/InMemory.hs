{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Ouroboros.Consensus.Backends.InMemory
  ( mkInMemoryFactory
  , mkInMemoryFromSnapshot

    -- * Snapshot streaming
  , inMemorySnapshotYielder
  , inMemorySnapshotSinker
  ) where

import Cardano.Ledger.Binary.Decoding
  ( DecShareCBOR
  , Interns
  , Share
  , decShareCBOR
  , decodeMap
  , decodeMemPack
  , internsFromMap
  )
import Cardano.Ledger.Binary.Encoding (encodeMap, encodeMemPack, toPlainEncoding)
import qualified Cardano.Ledger.Conway.State as SL
import qualified Cardano.Ledger.Core as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Codec.CBOR.Read (DeserialiseFailure (..), deserialiseIncremental)
import qualified Codec.CBOR.Read as CBOR.Read
import qualified Codec.CBOR.Write as CBOR
import Control.Monad (replicateM_, unless, void)
import Control.Monad.Except
import Control.Monad.State.Strict (execStateT)
import Control.Monad.Trans.Class (lift)
import Control.Tracer
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Data.Functor.Contravariant ((>$<))
import Data.Functor.Identity
import qualified Data.Map.Strict as Map
import Data.MemPack
import Lens.Micro
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Shelley.Ledger.Ledger
import Ouroboros.Consensus.Shelley.Ledger.SnapshotStream
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.V2.Backend (LedgerDBV2Trace (..))
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.CBOR (ReadIncrementalErr, readIncremental)
import Ouroboros.Consensus.Util.CRC
import Ouroboros.Consensus.Util.Enclose (encloseTimedWith)
import Ouroboros.Consensus.Util.IOLike
import Streaming (Of, Stream, hoist)
import qualified Streaming as S
import qualified Streaming.Prelude as S
import System.FS.API
import System.FS.CRC

newInMemoryTablesHandle ::
  forall m era.
  (SL.Era era, MemPack (SL.TxOut era), IOLike m) =>
  Tracer m LedgerDBV2Trace ->
  SomeHasFS m ->
  SL.NewEpochState era ->
  TablesHandle m era
newInMemoryTablesHandle tracer shfs@(SomeHasFS hasFS) ls =
  let h =
        TablesHandle
          { -- The handle's stored NES already carries the full UTxO,
            -- so populate the supplied ticked NES with that map; the
            -- key set is irrelevant here. BBODY then runs against the
            -- full UTxO and 'duplWithDiffs' can keep the resulting
            -- post-block NES verbatim.
            stateWith = \_keys _is nes ->
              encloseTimedWith (TraceLedgerTablesHandleRead >$< tracer) $
                pure $
                  nes
                    & slUtxoL .~ (ls ^. slUtxoL)
                    & slInstantStakeL .~ (ls ^. slInstantStakeL)
          , -- the post-block NES already carries the full UTxO, so
            -- return it verbatim for the outer 'ShelleyLedgerState'
            -- and build the new handle around the same value.
            duplWithDiffs = \_ st1 ->
              encloseTimedWith (TraceLedgerTablesHandlePush >$< tracer) $
                pure (st1, newInMemoryTablesHandle tracer shfs st1)
          , -- as the state already has the AVVMs, we ignore them
            stateWithUTxO = const ls
          , -- duplicating is just returning the same handle; still trace it
            -- so the per-handle lifecycle remains observable.
            duplicateHandle =
              encloseTimedWith (TraceLedgerTablesHandleCreate >$< tracer) $ pure h
          , -- we return the whole UTxO set in one go
            readUTxOWhole =
              encloseTimedWith (TraceLedgerTablesHandleRead >$< tracer) $
                pure (ls ^. slUtxoL)
          , -- We filter the UTxO set with the given predicate
            readUTxOFiltered = \predicate ->
              encloseTimedWith (TraceLedgerTablesHandleRead >$< tracer) $
                pure $
                  SL.UTxO $
                    Map.filter predicate (SL.unUTxO (ls ^. slUtxoL))
          , -- We access the requested (TxIn,TxOut)
            readTxOuts = \keys ->
              encloseTimedWith (TraceLedgerTablesHandleRead >$< tracer) $
                pure $
                  SL.UTxO $
                    Map.restrictKeys (SL.unUTxO (ls ^. slUtxoL)) keys
          , -- we don't apply AVVM diffs, the ledger already applied them
            applyDiff = \_ _ ->
              encloseTimedWith (TraceLedgerTablesHandlePush >$< tracer) $ pure h
          , -- closing has no effect, but trace it for lifecycle visibility
            closeHandle =
              encloseTimedWith (TraceLedgerTablesHandleClose >$< tracer) (pure ())
          , -- The statistics is the size of the UTxO map
            getStatsHandle = Statistics $ Map.size $ SL.unUTxO $ ls ^. slUtxoL
          , takeHandleSnapshot = \ds -> do
              createDirectoryIfMissing hasFS True $ snapshotToDirPath ds
              (_, crc1) <- withFile hasFS (snapshotToUTxOFilePath ds) (WriteMode MustBeNew) $ \hf ->
                hPutAllCRC hasFS hf $
                  CBOR.toLazyByteString $
                    mconcat
                      [ CBOR.encodeListLen 1
                      , toPlainEncoding (SL.eraProtVerLow @era) $
                          encodeMap encodeMemPack encodeMemPack (SL.unUTxO (ls ^. slUtxoL))
                      ]
              (_, crc2) <- withFile hasFS (snapshotToInstantStakeFilePath ds) (WriteMode MustBeNew) $ \hf ->
                hPutAllCRC hasFS hf $
                  CBOR.toLazyByteString $
                    mconcat
                      [ CBOR.encodeListLen 1
                      , toPlainEncoding (SL.eraProtVerLow @era) $
                          encodeMap encodeMemPack encodeMemPack (ls ^. slInstantStakeL)
                      ]
              pure (Just $ crcOfConcat crc1 crc2, UTxOHDMemSnapshot)
          , castHandle =
              \st ->
                encloseTimedWith (TraceLedgerTablesHandleCreate >$< tracer) $
                  pure $
                    newInMemoryTablesHandle tracer shfs st
          , injectValues =
              \st ->
                encloseTimedWith (TraceLedgerTablesHandleCreate >$< tracer) $
                  pure $
                    newInMemoryTablesHandle tracer shfs st
          }
   in h

mkInMemoryFactory ::
  forall m.
  IOLike m =>
  Tracer m LedgerDBV2Trace ->
  SomeHasFS m ->
  MkHandle m
mkInMemoryFactory tracer shfs =
  MkHandle
    { fromNewEpochState = \st ->
        encloseTimedWith (TraceLedgerTablesHandleCreate >$< tracer) $
          pure (newInMemoryTablesHandle tracer shfs st)
    }

mkInMemoryFromSnapshot ::
  forall m.
  IOLike m =>
  Tracer m LedgerDBV2Trace ->
  SomeHasFS m ->
  MkHandleFromSnapshot m
mkInMemoryFromSnapshot tracer shfs =
  MkHandleFromSnapshot
    { fromSnapshot = withExceptT BackendReadErr .: implFromSnapshot
    }
 where
  implFromSnapshot ::
    forall era.
    ( MemPack (SL.TxOut era)
    , Share (SL.TxOut era) ~ Interns (SL.Credential SL.Staking)
    , DecShareCBOR (SL.TxOut era)
    , Share (SL.InstantStake era) ~ Interns (SL.Credential SL.Staking)
    , DecShareCBOR (SL.InstantStake era)
    , SL.EraCertState era
    ) =>
    DiskSnapshot ->
    SL.NewEpochState era ->
    ExceptT ReadIncrementalErr m (SL.NewEpochState era, TablesHandle m era, Maybe CRC)
  implFromSnapshot ds ls = do
    let certInterns =
          internsFromMap $
            ls
              ^. SL.nesEsL
                . SL.esLStateL
                . SL.lsCertStateL
                . SL.certDStateL
                . SL.accountsL
                . SL.accountsMapL
    (utxo, Identity crcTables1) <-
      ExceptT $
        readIncremental
          shfs
          Identity
          ( do
              -- TODO @js now I realize we are going to put each one in one
              -- file, so this length is unnecessary.
              l <- CBOR.decodeListLen
              case l of
                1 -> SL.eraDecoder @era (decodeMap decodeMemPack (decShareCBOR certInterns))
                _ -> fail $ "Wrong number of tables: " <> show l
          )
          (snapshotToUTxOFilePath ds)

    (istake, Identity crcTables2) <-
      ExceptT $
        readIncremental
          shfs
          Identity
          ( do
              l <- CBOR.decodeListLen
              case l of
                1 -> SL.eraDecoder @era (decShareCBOR certInterns)
                _ -> fail $ "Wrong number of tables: " <> show l
          )
          (snapshotToInstantStakeFilePath ds)
    let _ = istake :: SL.InstantStake era

    -- InMemory keeps the UTxOs in the pure 'NewEpochState': re-populate
    -- 'slUtxoL' from the @utxo@ file before constructing the handle and
    -- hand back the same NES so the consumer can put it into the
    -- 'ShelleyStateHandle'\'s pure-state field.
    let ls' =
          ls
            & slUtxoL .~ SL.UTxO utxo
            & slInstantStakeL .~ (istake ^. instantStakeMapL)
    h <-
      lift $
        encloseTimedWith (TraceLedgerTablesHandleCreate >$< tracer) $
          pure $
            newInMemoryTablesHandle tracer shfs ls'
    pure (ls', h, Just (crcOfConcat crcTables1 crcTables2))

snapshotToUTxOFilePath :: DiskSnapshot -> FsPath
snapshotToUTxOFilePath ds = snapshotToDirPath ds </> mkFsPath ["utxo"]

snapshotToInstantStakeFilePath :: DiskSnapshot -> FsPath
snapshotToInstantStakeFilePath ds = snapshotToDirPath ds </> mkFsPath ["instantStake"]

{-------------------------------------------------------------------------------
  Snapshot streaming

  The in-memory backend lays its UTxO out as a single CBOR file at
  @<snapshot>/utxo@: a one-element list whose only element is an
  indefinite-length map of @(TxIn, TxOut era)@ entries encoded with
  the era's wrapping ('toEraCBOR' \/ 'eraDecoder'). The yielder reads
  the file lazily as a byte stream (tracking the running CRC) and
  exposes the decoded entries on top; the sinker takes a stream of
  entries and writes them back out in the same layout.
-------------------------------------------------------------------------------}

-- | Yield the contents of an in-memory snapshot.
--
-- Constructing the yielder doesn't allocate any resources, so
-- 'releaseYielder' is a no-op.
inMemorySnapshotYielder ::
  forall m.
  (MonadThrow m, MonadST m) =>
  SomeHasFS m ->
  DiskSnapshot ->
  SnapshotYielder m
inMemorySnapshotYielder fs ds =
  SnapshotYielder
    { runYielder = readEntries
    , releaseYielder = pure ()
    }
 where
  readEntries ::
    forall era.
    ( DecShareCBOR (SL.TxOut era)
    , Share (SL.TxOut era) ~ Interns (SL.Credential SL.Staking)
    , SL.EraCertState era
    ) =>
    SL.NewEpochState era ->
    ( EntryStream m era (Maybe CRC) ->
      ExceptT DeserialiseFailure m (Stream (Of ByteString) m (Maybe CRC, Maybe CRC))
    ) ->
    ExceptT DeserialiseFailure m (Maybe CRC, Maybe CRC)
  readEntries nes k =
    let certInterns =
          internsFromMap $
            nes
              ^. SL.nesEsL
                . SL.esLStateL
                . SL.lsCertStateL
                . SL.certDStateL
                . SL.accountsL
                . SL.accountsMapL
     in streamingFile fs (snapshotToUTxOFilePath ds) $ \bytes ->
          k $
            yieldCborMapS
              (SL.eraDecoder @era ((,) <$> decodeMemPack <*> decShareCBOR certInterns))
              bytes

-- | Sink a stream of UTxO entries into a fresh in-memory snapshot.
--
-- Buffers up to 'writeChunkSize' entries before flushing to disk;
-- constructing the sinker doesn't allocate any resources, so
-- 'releaseSinker' is a no-op.
inMemorySnapshotSinker ::
  forall m.
  MonadThrow m =>
  SomeHasFS m ->
  DiskSnapshot ->
  SnapshotSinker m
inMemorySnapshotSinker (SomeHasFS fs) ds =
  SnapshotSinker
    { runSinker = \s -> writeEntries s
    , releaseSinker = pure ()
    }
 where
  writeChunkSize :: Int
  writeChunkSize = 1000

  writeEntries ::
    forall era.
    (SL.Era era, MemPack (SL.TxOut era)) =>
    EntryStream m era (Maybe CRC) ->
    ExceptT DeserialiseFailure m (Stream (Of ByteString) m (Maybe CRC, Maybe CRC))
  writeEntries s =
    ExceptT $ withFile fs (snapshotToUTxOFilePath ds) (WriteMode MustBeNew) $ \hdl -> do
      let encK = SL.toEraCBOR @era . encodeMemPack
          encV :: SL.TxOut era -> CBOR.Encoding
          encV = SL.toEraCBOR @era . encodeMemPack
      -- One-element list wrapping the indefinite-length map.
      let bs = CBOR.toStrictByteString (CBOR.encodeListLen 1 <> CBOR.encodeMapLenIndef)
      let !crc0 = updateCRC bs initCRC
      void $ hPutSome fs hdl bs
      e <- runExceptT $ go encK encV hdl crc0 writeChunkSize mempty s
      case e of
        Left err -> pure $ Left err
        Right (r, crc1) -> do
          let bs1 = CBOR.toStrictByteString CBOR.encodeBreak
          void $ hPutSome fs hdl bs1
          let !crc2 = updateCRC bs1 crc1
          pure $ Right (fmap (,Just crc2) r)
   where
    go encK encV tb !crc 0 acc s' = do
      let bs = CBOR.toStrictByteString $ mconcat [encK k <> encV v | (k, v) <- reverse acc]
      lift $ void $ hPutSome fs tb bs
      let !crc1 = updateCRC bs crc
      go encK encV tb crc1 writeChunkSize mempty s'
    go encK encV tb !crc n acc s' = do
      mbs <- S.next s'
      case mbs of
        Left r -> do
          let bs = CBOR.toStrictByteString $ mconcat [encK k <> encV v | (k, v) <- reverse acc]
          lift $ void $ hPutSome fs tb bs
          let !crc1 = updateCRC bs crc
          pure (r, crc1)
        Right (item, s'') -> go encK encV tb crc (n - 1) (item : acc) s''

{-------------------------------------------------------------------------------
  Shared CBOR/file streaming helpers
-------------------------------------------------------------------------------}

-- | Open a file as a byte stream with a running CRC, hand it off to a
-- continuation that may layer typed decoding on top, and assert that
-- the underlying byte stream has been fully consumed.
streamingFile ::
  forall m.
  MonadThrow m =>
  SomeHasFS m ->
  FsPath ->
  ( Stream (Of ByteString) m (Maybe CRC) ->
    ExceptT DeserialiseFailure m (Stream (Of ByteString) m (Maybe CRC, Maybe CRC))
  ) ->
  ExceptT DeserialiseFailure m (Maybe CRC, Maybe CRC)
streamingFile (SomeHasFS fs') path cont =
  ExceptT $ withFile fs' path ReadMode $ \hdl ->
    runExceptT $ cont (getBS hdl initCRC) >>= noRemainingBytes
 where
  getBS h !crc = do
    bs <- S.lift $ hGetSome fs' h (fromIntegral defaultChunkSize)
    if BS.null bs
      then pure (Just crc)
      else do
        S.yield bs
        getBS h $! updateCRC bs crc

  noRemainingBytes s =
    lift (S.uncons s) >>= \case
      Nothing -> lift $ S.effects s
      Just (BS.null -> True, s') -> noRemainingBytes s'
      Just _ -> throwError $ DeserialiseFailure 0 "Remaining bytes"

-- | Layer typed CBOR decoding on top of a byte stream, treating it as
-- a one-element list whose only element is a CBOR map (definite or
-- indefinite). The caller passes a single (already era-wrapped, plain
-- CBOR) decoder for one @(key, value)@ pair.
yieldCborMapS ::
  forall m era.
  MonadST m =>
  (forall s. Decoder s (SL.TxIn, SL.TxOut era)) ->
  Stream (Of ByteString) m (Maybe CRC) ->
  Stream
    (Of (SL.TxIn, SL.TxOut era))
    (ExceptT DeserialiseFailure m)
    (Stream (Of ByteString) m (Maybe CRC))
yieldCborMapS decKV = execStateT $ do
  -- Outer @list-of-1@ wrapper and map header in one go.
  hoist lift (decodeCbor (CBOR.decodeListLen >> CBOR.decodeMapLenOrIndef)) >>= \case
    Nothing -> go
    Just n -> replicateM_ n yieldKV
 where
  yieldKV = do
    kv <- hoist lift $ decodeCbor decKV
    lift $ S.yield kv

  go = do
    doBreak <- hoist lift $ decodeCbor CBOR.decodeBreakOr
    unless doBreak $ yieldKV *> go

-- | Drive an incremental CBOR 'Decoder' from a byte stream. Returns
-- the decoded value and the remaining bytes (re-yielded so subsequent
-- decoders pick up where this one left off).
decodeCbor ::
  forall m s a.
  MonadST m =>
  (forall t. Decoder t a) ->
  StateT
    (Stream (Of ByteString) m s)
    (ExceptT DeserialiseFailure m)
    a
decodeCbor dec =
  StateT $ \bytes -> go bytes =<< lift (stToIO (deserialiseIncremental dec))
 where
  go bytes = \case
    CBOR.Read.Partial k ->
      lift (S.next bytes) >>= \case
        Right (bs, bytes') -> go bytes' =<< lift (stToIO (k (Just bs)))
        Left r -> go (pure r) =<< lift (stToIO (k Nothing))
    CBOR.Read.Done leftover _off a -> pure (a, S.yield leftover *> bytes)
    CBOR.Read.Fail _bs _off err -> throwError err
