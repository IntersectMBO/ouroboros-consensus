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

-- | The LeiosFetch mini-protocol's types and codec.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type
  ( LeiosFetch (..)
  , LeiosFetchBusy (..)
  , Message (..)
  , SingLeiosFetch (..)
  , TxBitmaps
  , bitmapOffsets
  , byteLimitsLeiosFetch
  , codecLeiosFetch
  , codecLeiosFetchId
  , maxLeiosTxsRequestBytesSize
  , timeLimitsLeiosFetch
  ) where

import Cardano.Ledger.Binary.Plain (decodeRecordSum)
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import Control.DeepSeq (NFData (..))
import Control.Monad (when)
import Control.Monad.Class.MonadST (MonadST)
import qualified Data.Bits as Bits
import Data.ByteString.Lazy (ByteString)
import Data.Kind (Type)
import Data.List (unfoldr)
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
import Ouroboros.Consensus.Util.CBOR (decodeListUpTo)
import Ouroboros.Network.Protocol.Codec.Utils (bytesBetweenOffsets)
import Ouroboros.Network.Protocol.Limits
  ( ProtocolSizeLimits (..)
  , ProtocolTimeLimits (..)
  , longWait
  , smallByteLimit
  , waitForever
  )
import Ouroboros.Network.Util.ShowProxy (ShowProxy (..))

-----

-- | The LeiosFetch mini-protocol's states, as the cardano-blueprint specifies
-- them:
-- <https://cardano-scaling.github.io/cardano-blueprint/network/node-to-node/leios-fetch/index.html?leios=on>.
-- CIP-0164 specifies the Leios mini-protocols,
-- <https://cips.cardano.org/cip/CIP-0164>.
type LeiosFetch :: Type -> Type -> Type -> Type
data LeiosFetch point eb tx = StIdle | StBusy (LeiosFetchBusy point eb tx) | StDone

type LeiosFetchBusy :: Type -> Type -> Type -> Type
data LeiosFetchBusy point eb tx = StBlock | StBlockTxs

instance
  ( ShowProxy point
  , ShowProxy eb
  , ShowProxy tx
  ) =>
  ShowProxy (LeiosFetch point eb tx)
  where
  showProxy _ =
    concat
      [ "LeiosFetch "
      , showProxy (Proxy :: Proxy point)
      , " "
      , showProxy (Proxy :: Proxy eb)
      , " "
      , showProxy (Proxy :: Proxy tx)
      ]

instance ShowProxy (StIdle :: LeiosFetch point eb tx) where
  showProxy _ = "StIdle"
instance ShowProxy (StBusy StBlock :: LeiosFetch point eb tx) where
  showProxy _ = "(StBusy StBlock)"
instance ShowProxy (StBusy StBlockTxs :: LeiosFetch point eb tx) where
  showProxy _ = "(StBusy StBlockTxs)"
instance ShowProxy (StDone :: LeiosFetch point eb tx) where
  showProxy _ = "StDone"

type SingLeiosFetch ::
  LeiosFetch point eb tx ->
  Type
data SingLeiosFetch st where
  SingIdle :: SingLeiosFetch StIdle
  SingBlock :: SingLeiosFetch (StBusy StBlock)
  SingBlockTxs :: SingLeiosFetch (StBusy StBlockTxs)
  SingDone :: SingLeiosFetch StDone

deriving instance Show (SingLeiosFetch st)

instance StateTokenI StIdle where stateToken = SingIdle
instance StateTokenI (StBusy StBlock) where stateToken = SingBlock
instance StateTokenI (StBusy StBlockTxs) where stateToken = SingBlockTxs
instance StateTokenI StDone where stateToken = SingDone

-----

-- | Which of an endorser block's transactions a request names: each entry is
-- the index of a run of 64 transactions and a bitmap of those 64, the most
-- significant bit naming the first of them.
type TxBitmaps = [(Word16, Word64)]

-- | The offsets of the transactions the bitmaps name, in the bitmaps' order.
bitmapOffsets :: TxBitmaps -> [Int]
bitmapOffsets = unfoldr next
 where
  next = \case
    [] -> Nothing
    (index, bitmap) : rest
      | bitmap == 0 -> next rest
      | otherwise ->
          let i = Bits.countLeadingZeros bitmap
           in Just (64 * fromIntegral index + i, (index, Bits.clearBit bitmap (63 - i)) : rest)

instance Protocol (LeiosFetch point eb tx) where
  data Message (LeiosFetch point eb tx) from to where
    MsgDone ::
      Message (LeiosFetch point eb tx) StIdle StDone
    MsgLeiosBlockRequest ::
      !point ->
      Message (LeiosFetch point eb tx) StIdle (StBusy StBlock)
    MsgLeiosBlock ::
      !eb ->
      Message (LeiosFetch point eb tx) (StBusy StBlock) StIdle
    MsgLeiosBlockTxsRequest ::
      !point ->
      TxBitmaps ->
      Message (LeiosFetch point eb tx) StIdle (StBusy StBlockTxs)
    MsgLeiosBlockTxs ::
      !(V.Vector tx) ->
      Message (LeiosFetch point eb tx) (StBusy StBlockTxs) StIdle

  type StateAgency StIdle = ClientAgency
  type StateAgency (StBusy st) = ServerAgency
  type StateAgency StDone = NobodyAgency

  type StateToken = SingLeiosFetch

instance NFData (Message (LeiosFetch point eb tx) from to) where
  rnf = \case
    MsgDone -> ()
    MsgLeiosBlockRequest{} -> ()
    MsgLeiosBlock{} -> ()
    MsgLeiosBlockTxsRequest _p bitmaps -> rnf bitmaps
    MsgLeiosBlockTxs{} -> ()

deriving instance
  (Eq point, Eq eb, Eq tx) =>
  Eq (Message (LeiosFetch point eb tx) from to)

deriving instance
  (Show point, Show eb, Show tx) =>
  Show (Message (LeiosFetch point eb tx) from to)

-----

-- | The most transaction bytes an honest client asks for in one
-- 'MsgLeiosBlockTxsRequest', so that it fetches an endorser block's closure as
-- several smaller requests, smoothing out its load on the peer. The server need
-- not enforce it. The client bounds the size of a 'MsgLeiosBlockTxs' by it.
--
-- Its value is a design choice; no protocol parameter bounds it.
maxLeiosTxsRequestBytesSize :: Word32
maxLeiosTxsRequestBytesSize = 500 * 1000

byteLimitsLeiosFetch ::
  (bytes -> Word) ->
  ProtocolSizeLimits (LeiosFetch point eb tx) bytes
byteLimitsLeiosFetch = ProtocolSizeLimits $ \case
  SingIdle -> smallByteLimit
  -- A 'MsgLeiosBlock' adds two bytes to an endorser block. Bounding its
  -- encoding by 'maxEndorserBlockBytesSize' also bounds its references by
  -- 'maxTxsPerEndorserBlock'.
  SingBlock -> fromIntegral $ maxEndorserBlockBytesSize + 2
  SingBlockTxs -> withMargin maxLeiosTxsRequestBytesSize
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

timeLimitsLeiosFetch ::
  ProtocolTimeLimits (LeiosFetch point eb tx)
timeLimitsLeiosFetch = ProtocolTimeLimits $ \case
  SingIdle -> waitForever
  SingBlock -> longWait
  SingBlockTxs -> longWait
  st@SingDone -> notActiveState st

-----

codecLeiosFetch ::
  forall (point :: Type) (eb :: Type) (tx :: Type) m.
  MonadST m =>
  -- | The most txs an EB can hold. The decoder rejects a larger count before
  -- it allocates.
  Int ->
  (point -> CBOR.Encoding) ->
  (forall s. CBOR.Decoder s point) ->
  (eb -> CBOR.Encoding) ->
  -- | Decode an endorser block, given the bytes it decoded.
  (forall s. CBOR.Decoder s (ByteString -> eb)) ->
  (tx -> CBOR.Encoding) ->
  (forall s. CBOR.Decoder s tx) ->
  AnnotatedCodec (LeiosFetch point eb tx) CBOR.DeserialiseFailure m ByteString
codecLeiosFetch maxTxs encodeP decodeP encodeEb decodeEb encodeTx decodeTx =
  mkCodecCborLazyBS
    (encodeLeiosFetch encodeP encodeEb encodeTx)
    decode
 where
  decode ::
    forall (st :: LeiosFetch point eb tx).
    ActiveState st =>
    StateToken st ->
    forall s.
    CBOR.Decoder s (Annotator ByteString st)
  decode stok =
    decodeRecordSum
      (T.pack $ "LeiosFetch (" <> show stok <> ")")
      (decodeLeiosFetch maxTxs decodeP decodeEb decodeTx stok)

encodeLeiosFetch ::
  forall
    (point :: Type)
    (eb :: Type)
    (tx :: Type)
    (st :: LeiosFetch point eb tx)
    (st' :: LeiosFetch point eb tx).
  (point -> CBOR.Encoding) ->
  (eb -> CBOR.Encoding) ->
  (tx -> CBOR.Encoding) ->
  Message (LeiosFetch point eb tx) st st' ->
  CBOR.Encoding
encodeLeiosFetch encodeP encodeEb encodeTx = \case
  MsgDone ->
    CBOR.encodeListLen 1
      <> CBOR.encodeWord 0
  MsgLeiosBlockRequest p ->
    CBOR.encodeListLen 2
      <> CBOR.encodeWord 1
      <> encodeP p
  MsgLeiosBlock x ->
    CBOR.encodeListLen 2
      <> CBOR.encodeWord 2
      <> encodeEb x
  MsgLeiosBlockTxsRequest p bitmaps ->
    CBOR.encodeListLen 3
      <> CBOR.encodeWord 3
      <> encodeP p
      <> encodeBitmaps bitmaps
  MsgLeiosBlockTxs txs ->
    CBOR.encodeListLen 2
      <> CBOR.encodeWord 4
      <> CBOR.encodeListLen (fromIntegral $ V.length txs)
      <> foldMap encodeTx txs

decodeLeiosFetch ::
  forall
    (point :: Type)
    (eb :: Type)
    (tx :: Type)
    (st :: LeiosFetch point eb tx)
    s.
  ActiveState st =>
  Int ->
  (forall s'. CBOR.Decoder s' point) ->
  (forall s'. CBOR.Decoder s' (ByteString -> eb)) ->
  (forall s'. CBOR.Decoder s' tx) ->
  StateToken st ->
  Word ->
  CBOR.Decoder s (Int, Annotator ByteString st)
decodeLeiosFetch maxTxs decodeP decodeEb decodeTx stok key =
  case (stok, key) of
    (SingIdle, 0) -> fields 1 $ return (plain MsgDone)
    (SingIdle, 1) -> fields 2 $ do
      p <- decodeP
      return $ plain $ MsgLeiosBlockRequest p
    (SingBlock, 2) -> fields 2 $ do
      (x, start, end) <- CBOR.decodeWithByteSpan decodeEb
      return $ Annotator $ \bytes ->
        SomeMessage $ MsgLeiosBlock $ x $ bytesBetweenOffsets start end bytes
    (SingIdle, 3) -> fields 3 $ do
      p <- decodeP
      bitmaps <- decodeBitmaps maxTxs
      return $ plain $ MsgLeiosBlockTxsRequest p bitmaps
    (SingBlockTxs, 4) -> fields 2 $ do
      txs <- decodeListUpTo "MsgLeiosBlockTxs" maxTxs decodeTx
      return $ plain $ MsgLeiosBlockTxs $ V.fromList txs
    (SingDone, _) -> notActiveState stok
    -- failures per protocol state
    (SingIdle, _) -> unexpected
    (SingBlock, _) -> unexpected
    (SingBlockTxs, _) -> unexpected
 where
  fields ::
    Int -> CBOR.Decoder s (Annotator ByteString st) -> CBOR.Decoder s (Int, Annotator ByteString st)
  fields n = fmap ((,) n)

  plain ::
    forall st'.
    (StateTokenI st, StateTokenI st') =>
    Message (LeiosFetch point eb tx) st st' -> Annotator ByteString st
  plain = Annotator . const . SomeMessage

  unexpected :: forall a. CBOR.Decoder s a
  unexpected =
    fail $ "codecLeiosFetch (" <> show stok <> ") unexpected key " <> show key

codecLeiosFetchId ::
  forall (point :: Type) (eb :: Type) (tx :: Type) m.
  Monad m =>
  Codec
    (LeiosFetch point eb tx)
    CodecFailure
    m
    (AnyMessage (LeiosFetch point eb tx))
codecLeiosFetchId = Codec{encode, decode}
 where
  encode ::
    forall st st'.
    ( ActiveState st
    , StateTokenI st
    ) =>
    Message (LeiosFetch point eb tx) st st' ->
    AnyMessage (LeiosFetch point eb tx)
  encode = AnyMessage

  decode ::
    forall (st :: LeiosFetch point eb tx).
    ActiveState st =>
    StateToken st ->
    m
      ( DecodeStep
          (AnyMessage (LeiosFetch point eb tx))
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
      (SingIdle, Just (AnyMessage msg@MsgLeiosBlockTxsRequest{})) ->
        DecodeDone (SomeMessage msg) Nothing
      (SingBlockTxs, Just (AnyMessage msg@MsgLeiosBlockTxs{})) ->
        DecodeDone (SomeMessage msg) Nothing
      (SingDone, _) ->
        notActiveState stok
      (_, _) ->
        DecodeFail $ CodecFailure "codecLeiosFetchId: no matching message"

-----

-- | Always the indefinite-length map, as the cardano-blueprint writes it. Using
-- the definite-length encoding for small maps, as the ledger does, could only
-- ever save one byte.
encodeBitmaps :: TxBitmaps -> CBOR.Encoding
encodeBitmaps bitmaps =
  CBOR.encodeMapLenIndef
    <> foldr
      (\(index, bitmap) r -> CBOR.encodeWord16 index <> CBOR.encodeWord64 bitmap <> r)
      CBOR.encodeBreak
      bitmaps

-- | The most bitmap entries it takes to cover @maxTxs@ txs, 64 txs to an entry.
maxBitmapEntries :: Int -> Int
maxBitmapEntries maxTxs = (maxTxs + 63) `div` 64

-- | Decode at most enough bitmaps to cover @maxTxs@ txs. Each bitmap must be
-- non-zero. The indices must be strictly ascending, and each must be below
-- 'maxBitmapEntries'.
--
-- Accepts either CBOR map encoding. We encode the indefinite one, but a decoder
-- that rejects the definite one would reject a peer that is within spec. A
-- declared length is checked before the loop, so neither form reads more
-- entries than the cap.
decodeBitmaps :: Int -> CBOR.Decoder s TxBitmaps
decodeBitmaps maxTxs =
  CBOR.decodeMapLenOrIndef >>= \case
    Nothing -> go (const CBOR.decodeBreakOr) 0 Nothing []
    Just n
      | n > maxEntries -> fail tooManyEntries
      | otherwise -> go (\k -> pure (k >= n)) 0 Nothing []
 where
  maxEntries = maxBitmapEntries maxTxs
  tooManyEntries = "TxBitmaps: more than " <> show maxEntries <> " entries"
  -- 'done' is how the two encodings differ: a break byte, or the declared count.
  go done !k mbPrevIndex acc =
    done k >>= \case
      True -> pure (reverse acc)
      False
        | k >= maxEntries -> fail tooManyEntries
        | otherwise -> do
            index <- CBOR.decodeWord16
            bitmap <- CBOR.decodeWord64
            when (bitmap == 0) $
              fail $
                "TxBitmaps: bitmap at index " <> show index <> " is zero"
            when (fromIntegral index >= maxEntries) $
              fail $
                "TxBitmaps: index " <> show index <> " is not below " <> show maxEntries
            when (maybe False (index <=) mbPrevIndex) $
              fail $
                "TxBitmaps: index " <> show index <> " is not strictly ascending"
            go done (k + 1 :: Int) (Just index) ((index, bitmap) : acc)
