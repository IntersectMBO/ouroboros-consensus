{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Opt-in transaction-element decoding for the atomic protocol. All other
-- states use the ordinary annotated codec, including its original body bytes.
--
-- Rationale: @docs/fetch-policy/cancellation-contract.md#accept-individual-transactions-during-a-response@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Codec
  ( codecLeiosFetchAtomicProgress
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import Control.Monad (unless, when)
import Control.Monad.Class.MonadST (MonadST (..))
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import qualified Data.Vector.Strict as V
import Network.TypedProtocol.Codec.CBOR
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Type
import Ouroboros.Network.Driver.Progress (ProgressEmitter, WithByteSpan (..))
import Ouroboros.Network.Protocol.Codec.Utils (decodeWithByteSpan)

-- | Use the wire codec's encoder and non-TX decoders unchanged. Decode a TX
-- reply's framing and individual elements in separate CBOR stages, invoking
-- the supplied emitter before decoding the next element. The progress driver
-- bounds these events and the pinned receive scope authenticates them before
-- handing them to storage. Sending never invokes the emitter.
--
-- The final typed vector is retained for protocol compatibility; its consumer
-- must settle the exchange without ingesting or crediting the elements again.
codecLeiosFetchAtomicProgress ::
  forall point eb tx m.
  MonadST m =>
  Int ->
  (point -> CBOR.Encoding) ->
  (forall s. CBOR.Decoder s point) ->
  (eb -> CBOR.Encoding) ->
  (forall s. CBOR.Decoder s (BL.ByteString -> eb)) ->
  (tx -> CBOR.Encoding) ->
  (forall s. CBOR.Decoder s tx) ->
  ProgressEmitter m tx ->
  AnnotatedCodec (LeiosFetchAtomic point eb tx) CBOR.DeserialiseFailure m BL.ByteString
codecLeiosFetchAtomicProgress maxTxs encodeP decodeP encodeEb decodeEb encodeTx decodeTx emit =
  ordinary
    { decode = \tok -> case tok of
        SingBlockTxs -> transactionReply
        _ -> decode ordinary tok
    }
 where
  ordinary = codecLeiosFetchAtomic maxTxs encodeP decodeP encodeEb decodeEb encodeTx decodeTx

  transactionReply ::
    m
      ( DecodeStep
          BL.ByteString
          CBOR.DeserialiseFailure
          m
          (Annotator BL.ByteString (StBusy StBlockTxs :: LeiosFetchAtomic point eb tx))
      )
  transactionReply = do
    header <- stage $ do
      outer <- CBOR.decodeListLenOrIndef
      tag <- CBOR.decodeWord
      case tag of
        6 -> do
          arity outer 1
          close outer
          pure Nothing
        4 -> do
          arity outer 2
          count <- CBOR.decodeListLenOrIndef
          when (maxTxs <= 0 || maybe False (\n -> n <= 0 || n > maxTxs) count) $
            fail "LeiosFetchAtomic: empty or excessive transaction reply"
          offset <- CBOR.peekByteOffset
          pure (Just (outer, count, offset))
        _ -> fail "LeiosFetchAtomic: unexpected transaction reply tag"
    bindStep header $ \result trailing -> case result of
      Nothing -> pure $ DecodeDone (Annotator $ const $ SomeMessage MsgBlockTxsCancelled) trailing
      Just (outer, count, offset) -> items outer count 0 offset [] trailing

  items ::
    Maybe Int ->
    Maybe Int ->
    Int ->
    Int64 ->
    [tx] ->
    Maybe BL.ByteString ->
    m
      ( DecodeStep
          BL.ByteString
          CBOR.DeserialiseFailure
          m
          (Annotator BL.ByteString (StBusy StBlockTxs :: LeiosFetchAtomic point eb tx))
      )
  items outer count seen offset acc trailing =
    case count of
      Just n | seen == n -> finish outer acc trailing
      _ -> do
        next <- stage $ do
          ended <- case count of
            Nothing -> CBOR.decodeBreakOr
            Just _ -> pure False
          if ended
            then do
              when (seen == 0) $ fail "LeiosFetchAtomic: empty transaction reply"
              pure Nothing
            else do
              when (seen >= maxTxs) $ fail "LeiosFetchAtomic: excessive transaction reply"
              Just <$> decodeWithByteSpan decodeTx
        supplied <- supply trailing next
        bindStep supplied $ \result rest -> case result of
          Nothing -> finish outer acc rest
          Just (WithByteSpan (tx, start, end))
            | start < 0 || end <= start || offset > maxBound - end ->
                pure $ DecodeFail $ CBOR.DeserialiseFailure offset "LeiosFetchAtomic: invalid element span"
            | otherwise -> do
                emit (WithByteSpan (tx, offset + start, offset + end))
                items outer count (seen + 1) (offset + end) (tx : acc) rest

  finish ::
    Maybe Int ->
    [tx] ->
    Maybe BL.ByteString ->
    m
      ( DecodeStep
          BL.ByteString
          CBOR.DeserialiseFailure
          m
          (Annotator BL.ByteString (StBusy StBlockTxs :: LeiosFetchAtomic point eb tx))
      )
  finish outer acc trailing = do
    tailStep <- stage (close outer)
    supplied <- supply trailing tailStep
    bindStep supplied $ \() rest ->
      pure $
        DecodeDone (Annotator $ const $ SomeMessage $ MsgLeiosBlockTxs $ V.fromList $ reverse acc) rest

  arity :: Maybe Int -> Int -> CBOR.Decoder s ()
  arity (Just actual) expected = unless (actual == expected) $ fail "LeiosFetchAtomic: invalid reply arity"
  arity Nothing _ = pure ()

  close :: Maybe Int -> CBOR.Decoder s ()
  close Nothing = CBOR.decodeBreakOr >>= \done -> unless done $ fail "LeiosFetchAtomic: missing outer break"
  close Just{} = pure ()

  stage :: (forall s. CBOR.Decoder s a) -> m (DecodeStep BL.ByteString CBOR.DeserialiseFailure m a)
  stage decoder = convertCborDecoderLBS decoder stToIO

-- CBOR starts each independent decoder at offset zero. The caller rebases
-- element spans and preserves the exact trailing input between stages.
bindStep ::
  Monad m =>
  DecodeStep bytes failure m a ->
  (a -> Maybe bytes -> m (DecodeStep bytes failure m b)) ->
  m (DecodeStep bytes failure m b)
bindStep (DecodeDone value trailing) k = k value trailing
bindStep (DecodeFail failure) _ = pure (DecodeFail failure)
bindStep (DecodePartial more) k = pure $ DecodePartial $ \input -> more input >>= (`bindStep` k)

-- A framing-only stage may finish without consuming input. In that case its
-- original trailing bytes must survive, rather than being silently dropped.
supply ::
  Monad m =>
  Maybe BL.ByteString ->
  DecodeStep BL.ByteString failure m a ->
  m (DecodeStep BL.ByteString failure m a)
supply Nothing step = pure step
supply (Just input) (DecodePartial more) = more (Just input)
supply trailing (DecodeDone value Nothing) = pure (DecodeDone value trailing)
supply _ (DecodeFail failure) = pure (DecodeFail failure)
supply (Just input) (DecodeDone value (Just rest)) = pure (DecodeDone value (Just (rest <> input)))
