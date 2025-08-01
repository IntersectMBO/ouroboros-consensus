{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Ouroboros.Consensus.Util.StreamingLedgerTables where

import Cardano.Slotting.Slot
import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (Encoding, encodeBreak, encodeMapLenIndef)
import Codec.CBOR.FlatTerm
import Codec.CBOR.Read
import Codec.CBOR.Write
import Control.Concurrent.Class.MonadMVar
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder.Extra (defaultChunkSize)
import qualified Data.Map.Strict as Map
import Data.MemPack
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Vector as V
import Database.LSMTree
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Tables
import Ouroboros.Consensus.Ledger.Tables.Diff
import Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.API
import Ouroboros.Consensus.Storage.LedgerDB.V2.LSM
import Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import Ouroboros.Network.Block
import Streaming
import qualified Streaming as S
import qualified Streaming.Internal as SI
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

yieldCborMapS ::
  forall m a b.
  MonadST m =>
  (forall s. Decoder s a) ->
  (forall s. Decoder s b) ->
  Stream (Of ByteString) m () ->
  Stream (Of (a, b)) m (Stream (Of ByteString) m ())
yieldCborMapS decK decV s = do
  k <- S.lift $ stToIO (deserialiseIncremental decK)
  mbs <- S.lift (S.uncons s)
  case mbs of
    Nothing -> error "Empty stream of bytes"
    Just (bs, s') ->
      case deserialiseFromBytes decodeTermToken (BS.fromStrict bs) of
        Left err -> error $ show err
        Right (bs', TkMapLen n) -> go (Just n) (Left k) $ Right (BS.toStrict bs', s')
        Right (bs', TkMapBegin) -> go Nothing (Left k) $ Right (BS.toStrict bs', s')
        _ -> error "Not a map!"
 where
  go (Just 0) k mbs = case mbs of
    Left s' -> pure s'
    Right (bs, s') -> pure (S.yield bs *> s')
  go remainingItems k mbs = case (k, mbs) of
    -- We have a partial decoding, awaiting for a bytestring

    -- We have read a bytestring from the stream
    (Left (Partial kont), Right (bs, s')) -> do
      k' <- S.lift $ stToIO $ kont $ Just bs
      case k' of
        -- after running the kontinuation, we still require more input,
        -- then read again from the stream
        Partial{} -> go remainingItems (Left k') . maybeToEither s' =<< S.lift (S.uncons s')
        -- We were done with the previous bytestring, so let's
        -- recurse without reading more.
        _ -> go remainingItems (Left k') (Left s')

    -- We are in a partial reading, but we were unable to read more
    -- input, so we call `kont` with `Nothing` which will fail.
    (Left (Partial kont), Left s') -> do
      k' <- S.lift $ stToIO $ kont Nothing
      go remainingItems (Left k') (Left s')

    -- We have read a bytestring from the stream
    (Right (valK, Partial kont), Right (bs, s')) -> do
      k' <- S.lift $ stToIO $ kont $ Just bs
      case k' of
        -- after running the kontinuation, we still require more input,
        -- then read again from the stream
        Partial{} -> go remainingItems (Right (valK, k')) . maybeToEither s' =<< S.lift (S.uncons s')
        -- We were done with the previous bytestring, so let's
        -- recurse without reading more.
        _ -> go remainingItems (Right (valK, k')) (Left s')

    -- We are in a partial reading, but we were unable to read more
    -- input, so we call `kont` with `Nothing` which will fail.
    (Right (valK, Partial kont), Left s') -> do
      k' <- S.lift $ stToIO $ kont Nothing
      go remainingItems (Right (valK, k')) (Left s')

    -- We completed a read
    (Left (Done unused _offset val), Left s') -> do
      if BS.null unused
        then
          -- We have no unused bytes, so read another chunk
          S.lift (S.uncons s') >>= \case
            -- If there is no more input, fail because we were expecting a value!
            Nothing -> error "No value!"
            -- Recurse if there is more input
            Just mbs' -> do
              k' <- S.lift $ stToIO (deserialiseIncremental decV)
              go remainingItems (Right (val, k')) $ Right mbs'
        else do
          -- We still have unused bytes, so use those before reading
          -- again.
          k' <- S.lift $ stToIO (deserialiseIncremental decV)
          go remainingItems (Right (val, k')) (Right (unused, s'))

    -- We completed a read
    (Right (valK, Done unused _offset val), Left s') -> do
      -- yield the pair
      S.yield (valK, val)
      case remainingItems of
        Just 1 -> pure (S.yield unused *> s')
        _ -> do
          k' <- S.lift $ stToIO (deserialiseIncremental decK)
          if BS.null unused
            then
              -- We have no unused bytes, so read another chunk
              S.lift (S.uncons s') >>= \case
                -- If there is no more input, then we are done!
                Nothing ->
                  case remainingItems of
                    Just n -> error $ "Missing " ++ show (n - 1) ++ " items!"
                    Nothing -> error "Missing a break!"
                -- Recurse if there is more input
                Just mbs' -> do
                  go ((\x -> x - 1) <$> remainingItems) (Left k') $ Right mbs'
            else do
              -- We still have unused bytes, so use those before reading
              -- again.
              go ((\x -> x - 1) <$> remainingItems) (Left k') (Right (unused, s'))
    (Left (Done _ _ _), Right _) -> error "unreachable!"
    (Right (_, Done _ _ _), Right _) -> error "unreachable!"
    (Left Fail{}, Right{}) -> error "unreachable!"
    (Right (_, Fail{}), Right{}) -> error "unreachable!"
    (Left (Fail bs _ err), Left s') ->
      case remainingItems of
        Nothing -> case deserialiseFromBytes decodeTermToken (BS.fromStrict bs) of
          Right (bs', TkBreak) -> pure (S.yield (BS.toStrict bs') *> s')
          _ -> error "Break not found!"
        _ ->
          error $ show err
    (Right (_, Fail bs _ err), _) ->
      error $ show err

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
