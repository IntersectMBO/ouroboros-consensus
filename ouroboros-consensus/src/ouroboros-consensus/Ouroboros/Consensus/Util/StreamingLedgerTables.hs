{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Ouroboros.Consensus.Util.StreamingLedgerTables where

import Cardano.Slotting.Slot
import Codec.CBOR.Decoding (Decoder, decodeBreakOr, decodeMapLenIndef)
import Codec.CBOR.Encoding (Encoding, encodeBreak, encodeMapLenIndef)
import Codec.CBOR.Read
import Codec.CBOR.Write
import Control.Concurrent.Class.MonadMVar
import Control.Monad (unless)
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

streamingFile ::
  MonadThrow m =>
  SomeHasFS m ->
  FsPath ->
  (Stream (Of ByteString) m () -> m (Stream (Of ByteString) m r)) ->
  m r
streamingFile fs@(SomeHasFS fs') path cont =
  withFile fs' path ReadMode $ \hdl ->
    cont (foo hdl) >>= noRemainingBytes
 where
  foo h = do
    bs <- S.lift $ hGetSome fs' h (fromIntegral defaultChunkSize)
    if BS.null bs
      then pure ()
      else do
        S.yield bs
        foo h

noRemainingBytes :: Monad m => Stream (Of ByteString) m r -> m r
noRemainingBytes s =
  S.uncons s >>= \case
    Nothing -> S.effects s
    Just (BS.null -> True, s') -> noRemainingBytes s'
    Just _ -> error "Remaining bytes!"

decodeCbor ::
  (MonadST m, MonadError DeserialiseFailure m) =>
  (forall s. Decoder s a) ->
  StateT (Stream (Of ByteString) m r) m a
decodeCbor dec =
  StateT $ \s -> go s =<< stToIO (deserialiseIncremental dec)
 where
  go s = \case
    Partial k ->
      S.next s >>= \case
        Right (bs, s') -> go s' =<< stToIO (k (Just bs))
        Left r -> go (pure r) =<< stToIO (k Nothing)
    Done bs _off a -> pure (a, S.yield bs *> s)
    Fail _bs _off err -> throwError err

yieldCborMapS ::
  forall m a b.
  (MonadST m, MonadError DeserialiseFailure m) =>
  (forall s. Decoder s a) ->
  (forall s. Decoder s b) ->
  Stream (Of ByteString) m () ->
  Stream (Of (a, b)) m (Stream (Of ByteString) m ())
yieldCborMapS decK decV = execStateT $ do
  hoist lift $ decodeCbor decodeMapLenIndef
  go
 where
  go = do
    doBreak <- hoist lift $ decodeCbor decodeBreakOr
    unless doBreak $ do
      kv <- hoist lift $ decodeCbor $ (,) <$> decK <*> decV
      lift $ S.yield kv
      go

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing = Left a

yieldLmdbS ::
  Monad m =>
  l EmptyMK ->
  LedgerBackingStoreValueHandle m l ->
  Stream (Of (TxIn l, TxOut l)) m ()
yieldLmdbS hint bsvh =
  go (RangeQuery Nothing 100000)
 where
  go p = do
    LedgerTables (ValuesMK values) <- S.lift $ bsvhRangeRead bsvh hint p
    if Map.null values
      then pure ()
      else do
        S.each $ Map.toList values
        go (RangeQuery (LedgerTables . KeysMK . Set.singleton . fst <$> Map.lookupMax values) 100000)

yieldLSM_S ::
  Monad m =>
  l EmptyMK ->
  LedgerTablesHandle m l ->
  Stream (Of (TxIn l, TxOut l)) m ()
yieldLSM_S hint tb = do
  go (Nothing, 100000)
 where
  go p = do
    (LedgerTables (ValuesMK values), mx) <- S.lift $ readRange tb hint p
    if Map.null values
      then pure ()
      else do
        S.each $ Map.toList values
        go (mx, 100000)

sinkLmdbS ::
  (Ord (TxIn l), GetTip l, Monad m) =>
  l EmptyMK ->
  LedgerBackingStore m l ->
  Stream (Of (TxIn l, TxOut l)) m () ->
  m ()
sinkLmdbS hint bs s = do
  go 1000 mempty s
 where
  sl = withOrigin (error "unreachable") id $ pointSlot $ getTip hint

  go 0 m s' = do
    bsWrite bs sl (hint, hint) (LedgerTables $ DiffMK $ fromMapInserts m)
    go 1000 mempty s'
  go n m s' = do
    mbs <- S.uncons s'
    case mbs of
      Nothing -> do
        bsWrite bs sl (hint, hint) (LedgerTables $ DiffMK $ fromMapInserts m)
        pure ()
      Just ((k, v), s'') -> go (n - 1) (Map.insert k v m) s''

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
  Session m ->
  Stream (Of (TxIn l, TxOut l)) m () ->
  m ()
sinkLsmS p session s =
  withTable session $ \(tb :: UTxOTable m l) -> go tb 1000 mempty s
 where
  go tb 0 m s' = do
    inserts tb $ V.fromList [(k, toLSMTxOut p v, Nothing) | (k, v) <- m]
    go tb 1000 mempty s'
  go tb n m s' = do
    mbs <- S.uncons s'
    case mbs of
      Nothing -> do
        inserts tb $ V.fromList [(k, toLSMTxOut p v, Nothing) | (k, v) <- m]
        pure ()
      Just (item, s'') -> go tb (n - 1) (item : m) s''

sinkInMemoryS ::
  (Monad m, MonadThrow m) =>
  Proxy l ->
  (TxIn l -> Encoding) ->
  (TxOut l -> Encoding) ->
  SomeHasFS m ->
  FilePath ->
  Stream (Of (TxIn l, TxOut l)) m () ->
  m ()
sinkInMemoryS _ encK encV (SomeHasFS fs) fp s = do
  withFile fs (mkFsPath [fp]) (WriteMode MustBeNew) $ \hdl -> do
    hPutSome fs hdl $ toStrictByteString encodeMapLenIndef
    go hdl 1000 mempty s
    hPutSome fs hdl $ toStrictByteString encodeBreak
    pure ()
 where
  go tb 0 m s' = do
    hPutSome fs tb $ toStrictByteString $ foldl (<>) mempty [encK k <> encV v | (k, v) <- m]
    go tb 1000 mempty s'
  go tb n m s' = do
    mbs <- S.uncons s'
    case mbs of
      Nothing -> do
        hPutSome fs tb $ toStrictByteString $ foldl (<>) mempty [encK k <> encV v | (k, v) <- m]
        pure ()
      Just (item, s'') -> go tb (n - 1) (item : m) s''
