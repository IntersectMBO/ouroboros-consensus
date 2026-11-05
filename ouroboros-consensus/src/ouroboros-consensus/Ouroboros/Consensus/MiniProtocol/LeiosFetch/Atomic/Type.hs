{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Experimental cancellation-capable LeiosFetch protocol and codecs. The
-- legacy protocol remains separate and keeps its version-16 encoding.
--
-- Rationale: @docs/fetch-policy/cancellation-contract.md#4-messages-and-terminal-results@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Type
  ( LeiosFetchAtomic (..)
  , LeiosFetchAtomicBusy (..)
  , Message (..)
  , SingLeiosFetchAtomic (..)
  , AtomicTxBitmaps
  , byteLimitsLeiosFetchAtomic
  , codecLeiosFetchAtomic
  , codecLeiosFetchAtomicId
  , timeLimitsLeiosFetchAtomic
  ) where

import Cardano.Ledger.Binary.Plain (decodeRecordSum)
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import Control.DeepSeq (NFData (..))
import Control.Monad (when)
import Control.Monad.Class.MonadST (MonadST)
import Data.ByteString.Lazy (ByteString)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Vector.Strict as V
import Data.Word (Word16, Word32, Word64)
import Network.TypedProtocol.Codec.CBOR
  ( ActiveState
  , AnnotatedCodec
  , Annotator (..)
  , AnyMessage (..)
  , Codec
  , CodecF (..)
  , CodecFailure (..)
  , DecodeStep (..)
  , SomeMessage (..)
  , StateTokenI (..)
  , mkCodecCborLazyBS
  , notActiveState
  )
import Network.TypedProtocol.Core
  ( Agency (..)
  , Protocol (..)
  , StateAgency
  )
import Ouroboros.Consensus.Leios.Types (maxEndorserBlockBytesSize)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity (WireOrdinal (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type
  ( bitmapOffsets
  , decodeBitmaps
  , encodeBitmaps
  , maxLeiosTxsRequestBytesSize
  )
import Ouroboros.Consensus.Util.CBOR (decodeListUpTo)
import Ouroboros.Network.Protocol.Limits
  ( ProtocolSizeLimits (..)
  , ProtocolTimeLimits (..)
  , longWait
  , smallByteLimit
  , waitForever
  )
import Ouroboros.Network.Util.ShowProxy (ShowProxy (..))

-----

-- | States of the experimental protocol. Pipelining/lookahead preserve these
-- logical exchanges while allowing later requests and cancels to arrive ahead
-- of earlier FIFO replies.
type LeiosFetchAtomic :: Type -> Type -> Type -> Type
data LeiosFetchAtomic point eb tx = StIdle | StBusy (LeiosFetchAtomicBusy point eb tx) | StDone

type LeiosFetchAtomicBusy :: Type -> Type -> Type -> Type
data LeiosFetchAtomicBusy point eb tx = StBlock | StBlockTxs | StCancel

instance
  ( ShowProxy point
  , ShowProxy eb
  , ShowProxy tx
  ) =>
  ShowProxy (LeiosFetchAtomic point eb tx)
  where
  showProxy _ =
    concat
      [ "LeiosFetchAtomic "
      , showProxy (Proxy :: Proxy point)
      , " "
      , showProxy (Proxy :: Proxy eb)
      , " "
      , showProxy (Proxy :: Proxy tx)
      ]

instance ShowProxy (StIdle :: LeiosFetchAtomic point eb tx) where
  showProxy _ = "StIdle"
instance ShowProxy (StBusy StBlock :: LeiosFetchAtomic point eb tx) where
  showProxy _ = "(StBusy StBlock)"
instance ShowProxy (StBusy StBlockTxs :: LeiosFetchAtomic point eb tx) where
  showProxy _ = "(StBusy StBlockTxs)"
instance ShowProxy (StBusy StCancel :: LeiosFetchAtomic point eb tx) where
  showProxy _ = "(StBusy StCancel)"
instance ShowProxy (StDone :: LeiosFetchAtomic point eb tx) where
  showProxy _ = "StDone"

type SingLeiosFetchAtomic ::
  LeiosFetchAtomic point eb tx ->
  Type
data SingLeiosFetchAtomic st where
  SingIdle :: SingLeiosFetchAtomic StIdle
  SingBlock :: SingLeiosFetchAtomic (StBusy StBlock)
  SingBlockTxs :: SingLeiosFetchAtomic (StBusy StBlockTxs)
  SingCancel :: SingLeiosFetchAtomic (StBusy StCancel)
  SingDone :: SingLeiosFetchAtomic StDone

deriving instance Show (SingLeiosFetchAtomic st)

instance StateTokenI StIdle where stateToken = SingIdle
instance StateTokenI (StBusy StBlock) where stateToken = SingBlock
instance StateTokenI (StBusy StBlockTxs) where stateToken = SingBlockTxs
instance StateTokenI (StBusy StCancel) where stateToken = SingCancel
instance StateTokenI StDone where stateToken = SingDone

-----

-- | Nonempty bitmap runs naming the requested whole transactions. The wire
-- decoder also checks nonzero masks, ascending indices and the static TX bound.
type AtomicTxBitmaps = NonEmpty (Word16, Word64)

instance Protocol (LeiosFetchAtomic point eb tx) where
  data Message (LeiosFetchAtomic point eb tx) from to where
    MsgDone ::
      Message (LeiosFetchAtomic point eb tx) StIdle StDone
    -- \| Request one whole body using the next shared exchange ordinal.
    MsgLeiosBlockRequest ::
      !WireOrdinal ->
      !point ->
      Message (LeiosFetchAtomic point eb tx) StIdle (StBusy StBlock)
    MsgLeiosBlock ::
      !eb ->
      Message (LeiosFetchAtomic point eb tx) (StBusy StBlock) StIdle
    -- \| Request one immutable nonempty transaction batch.
    MsgLeiosBlockTxsRequest ::
      !WireOrdinal ->
      !point ->
      AtomicTxBitmaps ->
      Message (LeiosFetchAtomic point eb tx) StIdle (StBusy StBlockTxs)
    MsgLeiosBlockTxs ::
      !(V.Vector tx) ->
      Message (LeiosFetchAtomic point eb tx) (StBusy StBlockTxs) StIdle
    -- \| Cancel a recent original request; this command has its own ordinal
    -- and acknowledgment, independent of the target's terminal reply.
    MsgTryCancel ::
      !WireOrdinal ->
      !WireOrdinal ->
      Message (LeiosFetchAtomic point eb tx) StIdle (StBusy StCancel)
    -- \| Terminal response to a canceled body request.
    MsgBlockCancelled :: Message (LeiosFetchAtomic point eb tx) (StBusy StBlock) StIdle
    -- \| Terminal response to a canceled transaction request.
    MsgBlockTxsCancelled :: Message (LeiosFetchAtomic point eb tx) (StBusy StBlockTxs) StIdle
    -- \| Terminal response to the cancel command, in its own FIFO position.
    MsgAckTryCancel :: Message (LeiosFetchAtomic point eb tx) (StBusy StCancel) StIdle

  type StateAgency StIdle = ClientAgency
  type StateAgency (StBusy st) = ServerAgency
  type StateAgency StDone = NobodyAgency

  type StateToken = SingLeiosFetchAtomic

instance NFData (Message (LeiosFetchAtomic point eb tx) from to) where
  rnf = \case
    MsgDone -> ()
    MsgLeiosBlockRequest{} -> ()
    MsgLeiosBlock{} -> ()
    MsgLeiosBlockTxsRequest _ordinal _p bitmaps -> rnf bitmaps
    MsgLeiosBlockTxs{} -> ()
    MsgTryCancel{} -> ()
    MsgBlockCancelled -> ()
    MsgBlockTxsCancelled -> ()
    MsgAckTryCancel -> ()

deriving instance
  (Eq point, Eq eb, Eq tx) =>
  Eq (Message (LeiosFetchAtomic point eb tx) from to)

deriving instance
  (Show point, Show eb, Show tx) =>
  Show (Message (LeiosFetchAtomic point eb tx) from to)

-----

-- | The limits assume that peers write the parts of a message that are not
-- hashed, such as the framing around its payloads, in tight CBOR: each
-- integer and length in its fewest bytes. The decoders accept wider ones too,
-- but the limits do not make room for them.
byteLimitsLeiosFetchAtomic ::
  (bytes -> Word) ->
  ProtocolSizeLimits (LeiosFetchAtomic point eb tx) bytes
byteLimitsLeiosFetchAtomic = ProtocolSizeLimits $ \case
  SingIdle -> smallByteLimit
  -- A 'MsgLeiosBlock' adds at most three bytes to an endorser block: the
  -- header of a list of two items, the tag 2, and the closing marker if the
  -- list is indefinite-length. 'decodeEndorserBlock' rejects an endorser block
  -- larger than 'maxEndorserBlockBytesSize' only after decoding it, and this
  -- limit keeps that work close to what the largest acceptable one costs.
  SingBlock -> fromIntegral $ maxEndorserBlockBytesSize + 3
  SingBlockTxs -> withMargin maxLeiosTxsRequestBytesSize
  SingCancel -> smallByteLimit
  st@SingDone -> notActiveState st
 where
  -- The margin covers what a 'MsgLeiosBlockTxs' writes around its payload: at
  -- most seven bytes, plus a CBOR-in-CBOR tag and byte-string header per
  -- transaction: four bytes while the transaction is under 256 bytes, five under
  -- 64 KiB, seven beyond. A valid transaction spends at least one input, so it
  -- takes at least 47 bytes, and the headers add under 9% of
  -- 'maxLeiosTxsRequestBytesSize'.
  withMargin :: Word32 -> Word
  withMargin x = fromIntegral $ x + x `div` 10

timeLimitsLeiosFetchAtomic ::
  ProtocolTimeLimits (LeiosFetchAtomic point eb tx)
timeLimitsLeiosFetchAtomic = ProtocolTimeLimits $ \case
  SingIdle -> waitForever
  SingBlock -> longWait
  SingBlockTxs -> longWait
  SingCancel -> longWait
  st@SingDone -> notActiveState st

-----

codecLeiosFetchAtomic ::
  forall (point :: Type) (eb :: Type) (tx :: Type) m.
  MonadST m =>
  -- | The most txs an EB can hold. The decoder rejects a larger count before
  -- it allocates.
  Int ->
  (point -> CBOR.Encoding) ->
  (forall s. CBOR.Decoder s point) ->
  (eb -> CBOR.Encoding) ->
  -- | Decode an endorser block, given the bytes of the whole message. Its byte
  -- offsets count from the start of the message, as the ledger's annotators
  -- expect, so it slices its own bytes out of them.
  (forall s. CBOR.Decoder s (ByteString -> eb)) ->
  (tx -> CBOR.Encoding) ->
  (forall s. CBOR.Decoder s tx) ->
  AnnotatedCodec (LeiosFetchAtomic point eb tx) CBOR.DeserialiseFailure m ByteString
codecLeiosFetchAtomic maxTxs encodeP decodeP encodeEb decodeEb encodeTx decodeTx =
  mkCodecCborLazyBS
    (encodeLeiosFetchAtomic encodeP encodeEb encodeTx)
    decode
 where
  decode ::
    forall (st :: LeiosFetchAtomic point eb tx).
    ActiveState st =>
    StateToken st ->
    forall s.
    CBOR.Decoder s (Annotator ByteString st)
  decode stok =
    decodeRecordSum
      (T.pack $ "LeiosFetchAtomic (" <> show stok <> ")")
      (decodeLeiosFetchAtomic maxTxs decodeP decodeEb decodeTx stok)

encodeLeiosFetchAtomic ::
  forall
    (point :: Type)
    (eb :: Type)
    (tx :: Type)
    (st :: LeiosFetchAtomic point eb tx)
    (st' :: LeiosFetchAtomic point eb tx).
  (point -> CBOR.Encoding) ->
  (eb -> CBOR.Encoding) ->
  (tx -> CBOR.Encoding) ->
  Message (LeiosFetchAtomic point eb tx) st st' ->
  CBOR.Encoding
encodeLeiosFetchAtomic encodeP encodeEb encodeTx = \case
  MsgDone ->
    CBOR.encodeListLen 1
      <> CBOR.encodeWord 0
  MsgLeiosBlockRequest (WireOrdinal ordinal) p ->
    CBOR.encodeListLen 3
      <> CBOR.encodeWord 1
      <> CBOR.encodeWord64 ordinal
      <> encodeP p
  MsgLeiosBlock x ->
    CBOR.encodeListLen 2
      <> CBOR.encodeWord 2
      <> encodeEb x
  MsgLeiosBlockTxsRequest (WireOrdinal ordinal) p bitmaps ->
    CBOR.encodeListLen 4
      <> CBOR.encodeWord 3
      <> CBOR.encodeWord64 ordinal
      <> encodeP p
      <> encodeBitmaps (NE.toList bitmaps)
  MsgLeiosBlockTxs txs ->
    CBOR.encodeListLen 2
      <> CBOR.encodeWord 4
      <> CBOR.encodeListLen (fromIntegral $ V.length txs)
      <> foldMap encodeTx txs
  MsgTryCancel (WireOrdinal ordinal) (WireOrdinal target) ->
    CBOR.encodeListLen 3 <> CBOR.encodeWord 5 <> CBOR.encodeWord64 ordinal <> CBOR.encodeWord64 target
  MsgBlockCancelled -> CBOR.encodeListLen 1 <> CBOR.encodeWord 6
  MsgBlockTxsCancelled -> CBOR.encodeListLen 1 <> CBOR.encodeWord 6
  MsgAckTryCancel -> CBOR.encodeListLen 1 <> CBOR.encodeWord 7

decodeLeiosFetchAtomic ::
  forall
    (point :: Type)
    (eb :: Type)
    (tx :: Type)
    (st :: LeiosFetchAtomic point eb tx)
    s.
  ActiveState st =>
  Int ->
  (forall s'. CBOR.Decoder s' point) ->
  (forall s'. CBOR.Decoder s' (ByteString -> eb)) ->
  (forall s'. CBOR.Decoder s' tx) ->
  StateToken st ->
  Word ->
  CBOR.Decoder s (Int, Annotator ByteString st)
decodeLeiosFetchAtomic maxTxs decodeP decodeEb decodeTx stok key =
  case (stok, key) of
    (SingIdle, 0) -> fields 1 $ return (dontNeedAnnotatorBytes MsgDone)
    (SingIdle, 1) -> fields 3 $ do
      ordinal <- WireOrdinal <$> CBOR.decodeWord64
      p <- decodeP
      return $ dontNeedAnnotatorBytes $ MsgLeiosBlockRequest ordinal p
    (SingBlock, 2) -> fields 2 $ do
      x <- decodeEb
      return $ Annotator $ SomeMessage . MsgLeiosBlock . x
    (SingIdle, 3) -> fields 4 $ do
      ordinal <- WireOrdinal <$> CBOR.decodeWord64
      p <- decodeP
      bitmaps <- decodeBitmaps maxTxs
      case NE.nonEmpty bitmaps of
        Just nonempty
          | validBitmaps maxTxs nonempty ->
              return $ dontNeedAnnotatorBytes $ MsgLeiosBlockTxsRequest ordinal p nonempty
        _ -> fail "LeiosFetchAtomic: empty or out-of-range transaction request"
    (SingBlockTxs, 4) -> fields 2 $ do
      txs <- decodeListUpTo "MsgLeiosBlockTxs" maxTxs decodeTx
      when (null txs) $ fail "LeiosFetchAtomic: empty transaction reply"
      return $ dontNeedAnnotatorBytes $ MsgLeiosBlockTxs $ V.fromList txs
    (SingIdle, 5) -> fields 3 $ do
      ordinal <- WireOrdinal <$> CBOR.decodeWord64
      target <- WireOrdinal <$> CBOR.decodeWord64
      return $ dontNeedAnnotatorBytes $ MsgTryCancel ordinal target
    (SingBlock, 6) -> fields 1 $ return (dontNeedAnnotatorBytes MsgBlockCancelled)
    (SingBlockTxs, 6) -> fields 1 $ return (dontNeedAnnotatorBytes MsgBlockTxsCancelled)
    (SingCancel, 7) -> fields 1 $ return (dontNeedAnnotatorBytes MsgAckTryCancel)
    (SingDone, _) -> notActiveState stok
    -- failures per protocol state
    (SingIdle, _) -> unexpected
    (SingBlock, _) -> unexpected
    (SingBlockTxs, _) -> unexpected
    (SingCancel, _) -> unexpected
 where
  fields ::
    Int -> CBOR.Decoder s (Annotator ByteString st) -> CBOR.Decoder s (Int, Annotator ByteString st)
  fields n = fmap ((,) n)

  dontNeedAnnotatorBytes ::
    forall st'.
    (StateTokenI st, StateTokenI st') =>
    Message (LeiosFetchAtomic point eb tx) st st' -> Annotator ByteString st
  dontNeedAnnotatorBytes = Annotator . const . SomeMessage

  unexpected :: forall a. CBOR.Decoder s a
  unexpected =
    fail $ "codecLeiosFetchAtomic (" <> show stok <> ") unexpected key " <> show key

codecLeiosFetchAtomicId ::
  forall (point :: Type) (eb :: Type) (tx :: Type) m.
  Monad m =>
  Int ->
  Codec
    (LeiosFetchAtomic point eb tx)
    CodecFailure
    m
    (AnyMessage (LeiosFetchAtomic point eb tx))
codecLeiosFetchAtomicId maxTxs = Codec{encode, decode}
 where
  encode ::
    forall st st'.
    ( ActiveState st
    , StateTokenI st
    ) =>
    Message (LeiosFetchAtomic point eb tx) st st' ->
    AnyMessage (LeiosFetchAtomic point eb tx)
  encode = AnyMessage

  decode ::
    forall (st :: LeiosFetchAtomic point eb tx).
    ActiveState st =>
    StateToken st ->
    m
      ( DecodeStep
          (AnyMessage (LeiosFetchAtomic point eb tx))
          CodecFailure
          m
          (SomeMessage st)
      )
  decode stok = return $ DecodePartial $ \bytes ->
    return $ case (stok, bytes) of
      (SingIdle, Just (AnyMessage msg@MsgDone{})) ->
        DecodeDone (SomeMessage msg) Nothing
      (SingIdle, Just (AnyMessage msg@MsgLeiosBlockRequest{})) ->
        DecodeDone (SomeMessage msg) Nothing
      (SingBlock, Just (AnyMessage msg@MsgLeiosBlock{})) ->
        DecodeDone (SomeMessage msg) Nothing
      (SingIdle, Just (AnyMessage msg@(MsgLeiosBlockTxsRequest _ _ bitmaps)))
        | validBitmaps maxTxs bitmaps ->
            DecodeDone (SomeMessage msg) Nothing
      (SingBlockTxs, Just (AnyMessage msg@(MsgLeiosBlockTxs txs)))
        | not (V.null txs)
        , V.length txs <= maxTxs ->
            DecodeDone (SomeMessage msg) Nothing
      (SingIdle, Just (AnyMessage msg@MsgTryCancel{})) ->
        DecodeDone (SomeMessage msg) Nothing
      (SingBlock, Just (AnyMessage msg@MsgBlockCancelled)) ->
        DecodeDone (SomeMessage msg) Nothing
      (SingBlockTxs, Just (AnyMessage msg@MsgBlockTxsCancelled)) ->
        DecodeDone (SomeMessage msg) Nothing
      (SingCancel, Just (AnyMessage msg@MsgAckTryCancel)) ->
        DecodeDone (SomeMessage msg) Nothing
      (SingDone, _) ->
        notActiveState stok
      (_, _) ->
        DecodeFail $ CodecFailure "codecLeiosFetchAtomicId: no matching message"

-----

-- | The identity codec enforces the same request bounds as the CBOR codec.
validBitmaps :: Int -> AtomicTxBitmaps -> Bool
validBitmaps maxTxs bitmaps =
  let xs = NE.toList bitmaps; indices = map fst xs
   in maxTxs > 0
        && all ((/= 0) . snd) xs
        && and (zipWith (<) indices (drop 1 indices))
        && all (< maxTxs) (bitmapOffsets xs)
