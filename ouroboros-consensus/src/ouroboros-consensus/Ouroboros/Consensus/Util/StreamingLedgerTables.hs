{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Util.StreamingLedgerTables
  ( -- * Yielding TxOuts
    yieldInMemoryS
  , yieldLsmS
  , yieldLmdbS

    -- * Sinks for TxOuts
  , sinkLmdbS
  , sinkLsmS
  , sinkInMemoryS
  ) where

import Cardano.Slotting.Slot
import Codec.CBOR.Decoding (Decoder, decodeBreakOr, decodeMapLenOrIndef)
import Codec.CBOR.Encoding (Encoding, encodeBreak, encodeMapLenIndef)
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
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder.Extra (defaultChunkSize)
import qualified Data.Map.Strict as Map
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Vector as V
import Database.LSMTree
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Tables.Diff
import Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.API
import Ouroboros.Consensus.Storage.LedgerDB.V2.LSM
import Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import Ouroboros.Network.Block
import Streaming
import qualified Streaming as S
import qualified Streaming.Prelude as S
import System.FS.API

yieldInMemory ::
  SomeHasFS IO ->
  FsPath ->
  (forall s. Decoder s a) ->
  (forall s. Decoder s b) ->
  ( Stream (Of (a, b)) (ExceptT DeserialiseFailure IO) (Stream (Of ByteString) IO ()) ->
    ExceptT DeserialiseFailure IO (Stream (Of ByteString) IO r)
  ) ->
  ExceptT DeserialiseFailure IO r
yieldInMemory shfs fp decK decV k =
  yieldInMemoryS shfs fp decK decV k

streamingFile ::
  forall m r.
  MonadThrow m =>
  SomeHasFS m ->
  FsPath ->
  (Stream (Of ByteString) m () -> ExceptT DeserialiseFailure m (Stream (Of ByteString) m r)) ->
  ExceptT DeserialiseFailure m r
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

noRemainingBytes ::
  Monad m => Stream (Of ByteString) m r -> ExceptT DeserialiseFailure m r
noRemainingBytes s =
  lift (S.uncons s) >>= \case
    Nothing -> lift $ S.effects s
    Just (BS.null -> True, s') -> noRemainingBytes s'
    Just _ -> throwError $ DeserialiseFailure 0 "Remaining bytes"

decodeCbor ::
  MonadST m =>
  (forall s. Decoder s a) ->
  StateT (Stream (Of ByteString) m r) (ExceptT DeserialiseFailure m) a
decodeCbor dec =
  StateT $ \s -> go s =<< lift (stToIO (deserialiseIncremental dec))
 where
  go s = \case
    Partial k ->
      lift (S.next s) >>= \case
        Right (bs, s') -> go s' =<< lift (stToIO (k (Just bs)))
        Left r -> go (pure r) =<< lift (stToIO (k Nothing))
    Done bs _off a -> pure (a, S.yield bs *> s)
    Fail _bs _off err -> throwError err

yieldCborMapS ::
  forall m a b.
  MonadST m =>
  (forall s. Decoder s a) ->
  (forall s. Decoder s b) ->
  Stream (Of ByteString) m () ->
  Stream (Of (a, b)) (ExceptT DeserialiseFailure m) (Stream (Of ByteString) m ())
yieldCborMapS decK decV = execStateT $ do
  hoist lift (decodeCbor decodeMapLenOrIndef) >>= \case
    Nothing -> go
    Just n -> replicateM_ n yieldKV
 where
  yieldKV = do
    kv <- hoist lift $ decodeCbor $ (,) <$> decK <*> decV
    lift $ S.yield kv

  go = do
    doBreak <- hoist lift $ decodeCbor decodeBreakOr
    unless doBreak $ yieldKV *> go

yieldInMemoryS ::
  (MonadThrow m, MonadST m) =>
  SomeHasFS m ->
  FsPath ->
  (forall s. Decoder s a) ->
  (forall s. Decoder s b) ->
  ( ( Stream (Of (a, b)) (ExceptT DeserialiseFailure m) (Stream (Of ByteString) m ()) ->
      ExceptT DeserialiseFailure m (Stream (Of ByteString) m r)
    )
  ) ->
  ExceptT DeserialiseFailure m r
yieldInMemoryS fs fp decK decV k =
  streamingFile fs fp $ \s -> do
    k $ yieldCborMapS decK decV s

instance MonadST m => MonadST (ExceptT e m) where
  stToIO = lift . stToIO

yieldLmdbS ::
  Monad m =>
  Int ->
  l EmptyMK ->
  LedgerBackingStoreValueHandle m l ->
  Stream (Of (TxIn l, TxOut l)) (ExceptT DeserialiseFailure m) ()
yieldLmdbS readChunkSize hint bsvh =
  hoist lift $ go (RangeQuery Nothing readChunkSize)
 where
  go p = do
    LedgerTables (ValuesMK values) <- S.lift $ bsvhRangeRead bsvh hint p
    if Map.null values
      then pure ()
      else do
        S.each $ Map.toList values
        go (RangeQuery (LedgerTables . KeysMK . Set.singleton . fst <$> Map.lookupMax values) readChunkSize)

yieldLsmS ::
  Monad m =>
  Int ->
  l EmptyMK ->
  LedgerTablesHandle m l ->
  Stream (Of (TxIn l, TxOut l)) (ExceptT DeserialiseFailure m) ()
yieldLsmS readChunkSize hint tb = do
  hoist lift $ go (Nothing, readChunkSize)
 where
  go p = do
    (LedgerTables (ValuesMK values), mx) <- S.lift $ readRange tb hint p
    if Map.null values
      then pure ()
      else do
        S.each $ Map.toList values
        go (mx, readChunkSize)

sinkLmdbS ::
  forall m l r.
  (Ord (TxIn l), GetTip l, Monad m) =>
  Int ->
  l EmptyMK ->
  (SlotNo -> (l EmptyMK, l EmptyMK) -> LedgerTables l DiffMK -> m ()) ->
  Stream (Of (TxIn l, TxOut l)) m r ->
  m r
sinkLmdbS writeChunkSize hint bs s = do
  go writeChunkSize mempty s
 where
  sl = withOrigin (error "unreachable") id $ pointSlot $ getTip hint

  go 0 m s' = do
    bs sl (hint, hint) (LedgerTables $ DiffMK $ fromMapInserts m)
    go writeChunkSize mempty s'
  go n m s' = do
    mbs <- S.uncons s'
    case mbs of
      Nothing -> do
        bs sl (hint, hint) (LedgerTables $ DiffMK $ fromMapInserts m)
        S.effects s'
      Just ((k, v), s'') ->
        go (n - 1) (Map.insert k v m) s''

sinkLsmS ::
  forall l m.
  ( SerialiseKey (TxIn l)
  , ResolveValue (LSMTxOut l)
  , SerialiseValue (LSMTxOut l)
  , MonadAsync m
  , MonadMVar m
  , MonadThrow (STM m)
  , MonadMask m
  , MonadST m
  , MonadEvaluate m
  , HasLSMTxOut l
  ) =>
  Proxy l ->
  Int ->
  Session m ->
  Stream (Of (TxIn l, TxOut l)) m () ->
  m ()
sinkLsmS p writeChunkSize session s =
  withTable session $ \(tb :: UTxOTable m l) -> go tb writeChunkSize mempty s
 where
  go tb 0 m s' = do
    inserts tb $ V.fromList [(k, toLSMTxOut p v, Nothing) | (k, v) <- m]
    go tb writeChunkSize mempty s'
  go tb n m s' = do
    mbs <- S.uncons s'
    case mbs of
      Nothing ->
        inserts tb $ V.fromList [(k, toLSMTxOut p v, Nothing) | (k, v) <- m]
      Just (item, s'') -> go tb (n - 1) (item : m) s''

sinkInMemoryS ::
  MonadThrow m =>
  Proxy l ->
  Int ->
  (TxIn l -> Encoding) ->
  (TxOut l -> Encoding) ->
  SomeHasFS m ->
  FilePath ->
  Stream (Of (TxIn l, TxOut l)) m () ->
  m ()
sinkInMemoryS _ writeChunkSize encK encV (SomeHasFS fs) fp s =
  withFile fs (mkFsPath [fp]) (WriteMode MustBeNew) $ \hdl -> do
    void $ hPutSome fs hdl $ toStrictByteString encodeMapLenIndef
    go hdl writeChunkSize mempty s
    void $ hPutSome fs hdl $ toStrictByteString encodeBreak
    pure ()
 where
  go tb 0 m s' = do
    void $ hPutSome fs tb $ toStrictByteString $ mconcat [encK k <> encV v | (k, v) <- m]
    go tb writeChunkSize mempty s'
  go tb n m s' = do
    mbs <- S.uncons s'
    case mbs of
      Nothing ->
        void $ hPutSome fs tb $ toStrictByteString $ mconcat [encK k <> encV v | (k, v) <- m]
      Just (item, s'') -> go tb (n - 1) (item : m) s''
