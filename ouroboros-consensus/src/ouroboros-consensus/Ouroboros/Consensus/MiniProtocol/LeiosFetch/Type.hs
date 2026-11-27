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
  , byteLimitsLeiosFetch
  , codecLeiosFetch
  , codecLeiosFetchId
  , maxLeiosTxsRequestBytesSize
  , timeLimitsLeiosFetch

    -- * Txs bitmap

    -- | The wire's sparse bitmap of requested tx offsets, and the pair of
    -- functions that is the whole of what the rest of the node needs from it:
    -- offsets in, offsets out.
  , TxsBitmap
  , txsBitmapChunkWidth
  , txsBitmapToTxIndices
  , txsBitmapFromTxIndices
    -- | Exposed so the wire validation can be tested without going through a
    -- whole 'Message'.
  , decodeTxsBitmap
  , encodeTxsBitmap
  ) where

import Cardano.Ledger.Binary.Plain (decodeRecordSum)
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import Control.DeepSeq (NFData (..))
import Control.Monad (when)
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Except (Except)
import Data.ByteString.Lazy (ByteString)
import Data.IntSet.NonEmpty (NEIntSet)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Vector.Strict as V
import Data.Word (Word32)
import Network.TypedProtocol.Codec.CBOR
  ( ActiveState
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
import Ouroboros.Consensus.Util.SparseBitmap (SparseBitmap)
import qualified Ouroboros.Consensus.Util.SparseBitmap as SparseBitmap
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

-- | Which of an endorser block's transactions a request names: the sparse
-- bitmap used by the transaction-request wire format.
type TxsBitmap = SparseBitmap

-- | How many transaction offsets one `TxsBitmap` entry covers.
txsBitmapChunkWidth :: Int
txsBitmapChunkWidth = 64

-- | @maxTxs@ rounded up to a multiple of 'txsBitmapChunkWidth', so every
-- chunk -- including the last -- is a full 'txsBitmapChunkWidth' wide, i.e.
-- exactly 8 bytes on the wire, as CIP-0164 specifies. The handful of offsets
-- this adds beyond @maxTxs@ are never valid for any endorser block; naming
-- one only ever gets rejected downstream, the same as any other offset an
-- endorser block does not have.
txsBitmapTotalWidth :: Int -> Int
txsBitmapTotalWidth maxTxs =
  txsBitmapChunkWidth * ((maxTxs + txsBitmapChunkWidth - 1) `div` txsBitmapChunkWidth)

-- | @txsBitmapFromTxIndices maxTxs@ names these offsets in a bitmap sized off
-- @maxTxs@: the node's design ceiling on an endorser block's tx count, so the
-- bitmap can never be asked to name an offset no endorser block can have.
txsBitmapFromTxIndices :: Int -> NEIntSet -> Except String TxsBitmap
txsBitmapFromTxIndices maxTxs = SparseBitmap.fromIndices txsBitmapChunkWidth (txsBitmapTotalWidth maxTxs)

txsBitmapToTxIndices :: TxsBitmap -> NEIntSet
txsBitmapToTxIndices = SparseBitmap.toIndices

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
      TxsBitmap ->
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
    MsgLeiosBlockTxsRequest _p txsBitmap -> rnf txsBitmap
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

-- | The limits assume that peers write the parts of a message that are not
-- hashed, such as the framing around its payloads, in tight CBOR: each
-- integer and length in its fewest bytes. The decoders accept wider ones too,
-- but the limits do not make room for them.
byteLimitsLeiosFetch ::
  (bytes -> Word) ->
  ProtocolSizeLimits (LeiosFetch point eb tx) bytes
byteLimitsLeiosFetch = ProtocolSizeLimits $ \case
  SingIdle -> smallByteLimit
  -- A 'MsgLeiosBlock' adds at most ten bytes to an endorser block: the header
  -- of a list of two items, the tag 2, and the closing marker if the list is
  -- indefinite-length; then the CBOR-in-CBOR tag (two bytes) and the header of
  -- the byte string (five bytes).
  SingBlock -> fromIntegral $ maxEndorserBlockBytesSize + 10
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
  -- | The most txs an EB can hold. Bounds both 'MsgLeiosBlockTxs' and the
  -- txs bitmap's total width; the decoder rejects a larger count, or a
  -- bitmap chunk index past it, before it allocates.
  Int ->
  (point -> CBOR.Encoding) ->
  (forall s. CBOR.Decoder s point) ->
  (eb -> CBOR.Encoding) ->
  (forall s. CBOR.Decoder s eb) ->
  (tx -> CBOR.Encoding) ->
  (forall s. CBOR.Decoder s tx) ->
  Codec (LeiosFetch point eb tx) CBOR.DeserialiseFailure m ByteString
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
    CBOR.Decoder s (SomeMessage st)
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
  MsgLeiosBlockTxsRequest p txsBitmap ->
    CBOR.encodeListLen 3
      <> CBOR.encodeWord 3
      <> encodeP p
      <> encodeTxsBitmap txsBitmap
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
  (forall s'. CBOR.Decoder s' eb) ->
  (forall s'. CBOR.Decoder s' tx) ->
  StateToken st ->
  Word ->
  CBOR.Decoder s (Int, SomeMessage st)
decodeLeiosFetch maxTxs decodeP decodeEb decodeTx stok key =
  case (stok, key) of
    (SingIdle, 0) -> fields 1 $ return (SomeMessage MsgDone)
    (SingIdle, 1) -> fields 2 $ do
      p <- decodeP
      return $ SomeMessage $ MsgLeiosBlockRequest p
    (SingBlock, 2) -> fields 2 $ do
      x <- decodeEb
      return $ SomeMessage $ MsgLeiosBlock x
    (SingIdle, 3) -> fields 3 $ do
      p <- decodeP
      txsBitmap <- decodeTxsBitmap maxTxs
      return $ SomeMessage $ MsgLeiosBlockTxsRequest p txsBitmap
    (SingBlockTxs, 4) -> fields 2 $ do
      txs <- decodeListUpTo "MsgLeiosBlockTxs" maxTxs decodeTx
      -- A request names at least one tx, so a conforming reply carries at
      -- least one.
      when (null txs) $ fail "MsgLeiosBlockTxs: no transactions"
      return $ SomeMessage $ MsgLeiosBlockTxs $ V.fromList txs
    (SingDone, _) -> notActiveState stok
    -- failures per protocol state
    (SingIdle, _) -> unexpected
    (SingBlock, _) -> unexpected
    (SingBlockTxs, _) -> unexpected
 where
  fields :: Int -> CBOR.Decoder s (SomeMessage st) -> CBOR.Decoder s (Int, SomeMessage st)
  fields n = fmap ((,) n)

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

-- | The wire represents chunk indices as 'Data.Word.Word16's.
encodeTxsBitmap :: TxsBitmap -> CBOR.Encoding
encodeTxsBitmap = SparseBitmap.encodeCbor (CBOR.encodeWord16 . fromIntegral)

-- | @decodeTxsBitmap maxTxs@ decodes the wire's 'Data.Word.Word16' chunk
-- indices into a bitmap sized off @maxTxs@. The decoder rejects a chunk index
-- past what @maxTxs@ allows before it allocates.
decodeTxsBitmap :: Int -> CBOR.Decoder s TxsBitmap
decodeTxsBitmap maxTxs =
  SparseBitmap.decodeCbor
    txsBitmapChunkWidth
    (txsBitmapTotalWidth maxTxs)
    (fromIntegral <$> CBOR.decodeWord16)
