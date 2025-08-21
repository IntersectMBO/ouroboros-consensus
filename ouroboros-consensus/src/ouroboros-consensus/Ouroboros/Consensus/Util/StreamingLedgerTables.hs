{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Util.StreamingLedgerTables
  ( stream
  , yield
  , sink
  , YieldArgs (..)
  , SinkArgs (..)
  , Decoders (..)
  ) where

import Cardano.Slotting.Slot
import Codec.CBOR.Decoding (Decoder, decodeBreakOr, decodeListLen, decodeMapLenOrIndef)
import Codec.CBOR.Encoding (Encoding, encodeBreak, encodeListLen, encodeMapLenIndef)
import Codec.CBOR.Read
import Codec.CBOR.Write
import Control.Concurrent.Class.MonadMVar
import Control.Monad (replicateM_, unless)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.ResourceRegistry
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder.Extra (defaultChunkSize)
import qualified Data.Map.Strict as Map
import Data.MemPack
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import Database.LSMTree
import qualified Debug.Trace as Debug
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Tables.Diff
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.API
import Ouroboros.Consensus.Storage.LedgerDB.V2.LSM
import Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import Ouroboros.Consensus.Util.IndexedMemPack
import Ouroboros.Network.Block
import Streaming
import qualified Streaming as S
import qualified Streaming.Prelude as S
import System.FS.API

data Decoders l
  = Decoders
      (forall s. Codec.CBOR.Decoding.Decoder s (TxIn l))
      (forall s. Codec.CBOR.Decoding.Decoder s (TxOut l))

stream ::
  Constraints l m =>
  l EmptyMK ->
  (l EmptyMK -> ResourceRegistry m -> m (YieldArgs l m)) ->
  (l EmptyMK -> ResourceRegistry m -> m (SinkArgs l m)) ->
  ExceptT DeserialiseFailure m ()
stream st mYieldArgs mSinkArgs =
  ExceptT $
    withRegistry $ \reg -> do
      yArgs <- mYieldArgs st reg
      sArgs <- mSinkArgs st reg
      runExceptT $ yield yArgs st $ sink sArgs st

type Yield l m =
  l EmptyMK ->
  ( ( Stream (Of (TxIn l, TxOut l)) (ExceptT DeserialiseFailure m) (Stream (Of ByteString) m ()) ->
      ExceptT DeserialiseFailure m (Stream (Of ByteString) m ())
    )
  ) ->
  ExceptT DeserialiseFailure m ()

type Sink l m r =
  l EmptyMK ->
  Stream (Of (TxIn l, TxOut l)) (ExceptT DeserialiseFailure m) r ->
  ExceptT DeserialiseFailure m r

instance MonadST m => MonadST (ExceptT e m) where
  stToIO = lift . stToIO

data YieldArgs l m
  = -- | Yield an in-memory snapshot
    YieldInMemory
      -- | How to make a SomeHasFS for @m@
      (MountPoint -> SomeHasFS m)
      -- | The file path at which the HasFS has to be opened
      FilePath
      (Decoders l)
  | -- | Yield an LMDB snapshot
    YieldLMDB
      Int
      (LedgerBackingStoreValueHandle m l)
  | -- | Yield an LSM snapshot
    YieldLSM
      Int
      (LedgerTablesHandle m l)

yield :: Constraints l m => YieldArgs l m -> Yield l m
yield = \case
  YieldInMemory mkFs fp (Decoders decK decV) -> yieldInMemoryS mkFs fp decK decV
  YieldLMDB chunkSize valueHandle -> yieldLmdbS chunkSize valueHandle
  YieldLSM chunkSize hdl -> yieldLsmS chunkSize hdl

type Constraints l m =
  ( LedgerSupportsV1LedgerDB l
  , LedgerSupportsV2LedgerDB l
  , HasLedgerTables l
  , GetTip l
  , IOLike m
  )

sink ::
  Constraints l m =>
  SinkArgs l m -> Sink l m r
sink = \case
  SinkLMDB chunkSize write copy -> sinkLmdbS chunkSize write copy
  SinkLSM chunkSize session -> sinkLsmS chunkSize session
  SinkInMemory chunkSize encK encV shfs fp -> sinkInMemoryS chunkSize encK encV shfs fp

data SinkArgs l m
  = SinkInMemory
      Int
      (TxIn l -> Encoding)
      (TxOut l -> Encoding)
      (SomeHasFS m)
      FilePath
  | SinkLSM
      -- | Chunk size
      Int
      (Session m)
  | SinkLMDB
      -- | Chunk size
      Int
      -- | bsWrite
      (SlotNo -> (l EmptyMK, l EmptyMK) -> LedgerTables l DiffMK -> m ())
      (l EmptyMK -> m ())

{-------------------------------------------------------------------------------
  Yielding InMemory
-------------------------------------------------------------------------------}

streamingFile ::
  forall m.
  MonadThrow m =>
  SomeHasFS m ->
  FsPath ->
  (Stream (Of ByteString) m () -> ExceptT DeserialiseFailure m (Stream (Of ByteString) m ())) ->
  ExceptT DeserialiseFailure m ()
streamingFile (SomeHasFS fs') path cont =
  ExceptT $ withFile fs' path ReadMode $ \hdl ->
    runExceptT $ cont (getBS hdl) >>= noRemainingBytes
 where
  getBS h = do
    bs <- S.lift $ hGetSome fs' h (fromIntegral defaultChunkSize)
    if BS.null bs
      then pure ()
      else do
        S.yield bs
        getBS h

  noRemainingBytes s =
    lift (S.uncons s) >>= \case
      Nothing -> lift $ S.effects s
      Just (BS.null -> True, s') -> noRemainingBytes s'
      Just _ -> throwError $ DeserialiseFailure 0 "Remaining bytes"

yieldCborMapS ::
  forall m a b.
  MonadST m =>
  (forall s. Decoder s a) ->
  (forall s. Decoder s b) ->
  Stream (Of ByteString) m () ->
  Stream (Of (a, b)) (ExceptT DeserialiseFailure m) (Stream (Of ByteString) m ())
yieldCborMapS decK decV = execStateT $ do
  hoist lift (decodeCbor decodeListLen >> decodeCbor decodeMapLenOrIndef) >>= \case
    Nothing -> go
    Just n -> replicateM_ n yieldKV
 where
  yieldKV = do
    kv <- hoist lift $ decodeCbor $ (,) <$> decK <*> decV
    lift $ S.yield kv

  go = do
    doBreak <- hoist lift $ decodeCbor decodeBreakOr
    unless doBreak $ yieldKV *> go

  decodeCbor dec =
    StateT $ \s -> go' s =<< lift (stToIO (deserialiseIncremental dec))
   where
    go' s = \case
      Partial k ->
        lift (S.next s) >>= \case
          Right (bs, s') -> go' s' =<< lift (stToIO (k (Just bs)))
          Left r -> go' (pure r) =<< lift (stToIO (k Nothing))
      Done bs _off a -> pure (a, S.yield bs *> s)
      Fail _bs _off err -> throwError err

yieldInMemoryS ::
  (MonadThrow m, MonadST m) =>
  (MountPoint -> SomeHasFS m) ->
  FilePath ->
  (forall s. Decoder s (TxIn l)) ->
  (forall s. Decoder s (TxOut l)) ->
  Yield l m
yieldInMemoryS mkFs fp decK decV _ k =
  streamingFile (mkFs $ MountPoint fp) (mkFsPath ["tables", "tvar"]) $ \s -> do
    k $ yieldCborMapS decK decV s

{-------------------------------------------------------------------------------
  Yielding OnDisk backends
-------------------------------------------------------------------------------}

yieldLmdbS ::
  Monad m =>
  Int ->
  LedgerBackingStoreValueHandle m l ->
  Yield l m
yieldLmdbS readChunkSize bsvh hint k = do
  r <- k (go (RangeQuery Nothing readChunkSize))
  lift $ S.effects r
 where
  go p = do
    (LedgerTables (ValuesMK values), mx) <- lift $ S.lift $ bsvhRangeRead bsvh hint p
    case mx of
      Nothing -> pure $ pure ()
      Just x -> do
        S.each $ Map.toList values
        go (RangeQuery (Just . LedgerTables . KeysMK $ Set.singleton x) readChunkSize)

yieldLsmS ::
  Monad m =>
  Int ->
  LedgerTablesHandle m l ->
  Yield l m
yieldLsmS readChunkSize tb hint k = do
  r <- k (go (Nothing, readChunkSize))
  lift $ S.effects r
 where
  go p = do
    (LedgerTables (ValuesMK values), mx) <- lift $ S.lift $ readRange tb hint p
    if Map.null values
      then pure $ pure ()
      else do
        S.each $ Map.toList values
        go (mx, readChunkSize)

{-------------------------------------------------------------------------------
  Sink
-------------------------------------------------------------------------------}

sinkLmdbS ::
  forall m l r.
  (Ord (TxIn l), GetTip l, Monad m) =>
  Int ->
  (SlotNo -> (l EmptyMK, l EmptyMK) -> LedgerTables l DiffMK -> m ()) ->
  (l EmptyMK -> m ()) ->
  Sink l m r
sinkLmdbS writeChunkSize bs copyTo hint s = do
  r <- go writeChunkSize mempty s
  lift $ copyTo hint
  pure r
 where
  sl = withOrigin (error "unreachable") id $ pointSlot $ getTip hint

  go 0 m s' = do
    lift $ bs sl (hint, hint) (LedgerTables $ DiffMK $ fromMapInserts m)
    go writeChunkSize mempty s'
  go n m s' = do
    mbs <- S.uncons s'
    case mbs of
      Nothing -> do
        lift $ bs sl (hint, hint) (LedgerTables $ DiffMK $ fromMapInserts m)
        S.effects s'
      Just ((k, v), s'') ->
        go (n - 1) (Map.insert k v m) s''

sinkLsmS ::
  forall l m r.
  ( MonadAsync m
  , MonadMVar m
  , MonadThrow (STM m)
  , MonadMask m
  , MonadST m
  , MonadEvaluate m
  , MemPack (TxIn l)
  , IndexedMemPack (l EmptyMK) (TxOut l)
  , GetTip l
  ) =>
  Int ->
  Session m ->
  Sink l m r
sinkLsmS writeChunkSize session st s = do
  tb :: UTxOTable m <- lift $ newTable session
  r <- go tb writeChunkSize mempty s
  lift $
    saveSnapshot
      (toSnapshotName (show $ unSlotNo $ withOrigin (error "impossible") id $ pointSlot $ getTip st))
      (SnapshotLabel $ T.pack "UTxO table")
      tb
  lift $ closeTable tb
  pure r
 where
  go tb 0 m s' = do
    lift $
      inserts tb $
        V.fromList [(toTxInBytes (Proxy @l) k, toTxOutBytes st v, Nothing) | (k, v) <- m]
    go tb writeChunkSize mempty s'
  go tb n m s' = do
    mbs <- S.uncons s'
    case mbs of
      Nothing -> do
        lift $
          inserts tb $
            V.fromList
              [(toTxInBytes (Proxy @l) k, toTxOutBytes st v, Nothing) | (k, v) <- m]
        S.effects s'
      Just (item, s'') -> go tb (n - 1) (item : m) s''

sinkInMemoryS ::
  forall m l r.
  MonadThrow m =>
  Int ->
  (TxIn l -> Encoding) ->
  (TxOut l -> Encoding) ->
  SomeHasFS m ->
  FilePath ->
  Sink l m r
sinkInMemoryS writeChunkSize encK encV (SomeHasFS fs) fp _ s =
  ExceptT $ withFile fs (mkFsPath [Debug.traceShowId fp]) (WriteMode MustBeNew) $ \hdl -> do
    void $ hPutSome fs hdl $ toStrictByteString (encodeListLen 1 <> encodeMapLenIndef)
    e <- runExceptT $ go hdl writeChunkSize mempty s
    case e of
      Left err -> pure $ Left err
      Right r -> do
        void $ hPutSome fs hdl $ toStrictByteString encodeBreak
        pure $ Right r
 where
  go tb 0 m s' = do
    lift $ void $ hPutSome fs tb $ toStrictByteString $ mconcat [encK k <> encV v | (k, v) <- reverse m]
    go tb writeChunkSize mempty s'
  go tb n m s' = do
    mbs <- S.uncons s'
    case mbs of
      Nothing -> do
        lift $ void $ hPutSome fs tb $ toStrictByteString $ mconcat [encK k <> encV v | (k, v) <- reverse m]
        S.effects s'
      Just (item, s'') -> go tb (n - 1) (item : m) s''
