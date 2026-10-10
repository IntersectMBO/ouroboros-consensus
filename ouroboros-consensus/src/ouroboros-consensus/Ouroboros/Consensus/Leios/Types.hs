{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The Leios types that the storage layer needs: hashes, points, endorser
-- block bodies and their sizes.
--
-- This is the slice of the Leios prototype's types that
-- "Ouroboros.Consensus.Storage.LeiosDB" depends on. The rest of the Leios
-- machinery (announcements, fetch, voting, certificates) extends this module
-- as it lands.
module Ouroboros.Consensus.Leios.Types
  ( -- * Hashes
    EbHash (..)
  , ebHashBytes
  , mkEbHash
  , encodeEbHash
  , decodeEbHash
  , prettyEbHash
  , RbHash (..)
  , encodeRbHash
  , decodeRbHash
  , prettyRbHash
  , TxHash (..)
  , txHashBytes
  , mkTxHash
  , prettyTxHash

    -- * Points
  , LeiosPoint (..)
  , prettyLeiosPoint

    -- * Endorser blocks
  , BytesSize
  , TxOffset
  , TxLocation (..)
  , LeiosEb (..)
  , leiosEbBodyItems
  , encodeLeiosEb
  , encodeLeiosEbItemSize
  , encodeLeiosEbMaxFramingSize
  , encodeLeiosEbSize
  , leiosReferencesCapacity

    -- * Header checks
  , minCertificationSlot
  ) where

import Cardano.Binary.FixedSizeCodec (decodeFixedSized, encodeFixedSized)
import Cardano.Crypto.PackedBytes (PackedBytes, packByteString, unpackPinnedBytes)
import Cardano.Crypto.Util (SignableRepresentation (..))
import Cardano.Ledger.BaseTypes (Milliseconds32 (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Cardano.Slotting.Time (SlotLength, slotLengthToMillisec)
import Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import Codec.CBOR.Write (toStrictByteString)
import Codec.Serialise (Serialise (..))
import Control.DeepSeq (NFData)
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as BS8
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Vector.Strict (Vector)
import qualified Data.Vector.Strict as V
import Data.Word (Word32)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Ledger.SupportsMempool (ByteSize32 (..))
import Ouroboros.Consensus.Util (ShowProxy (..))

-- * Hashes

-- | Hash of an Endorser Block
--
-- The fixed length makes decoding reject a peer-supplied hash of any other
-- length.
newtype EbHash = MkEbHash (PackedBytes 32)
  deriving newtype (Eq, Ord, NoThunks)
  deriving stock Generic

instance Show EbHash where
  show = prettyEbHash

ebHashBytes :: EbHash -> ByteString
ebHashBytes (MkEbHash bytes) = unpackPinnedBytes bytes

-- | Fails unless the input is exactly 32 bytes.
mkEbHash :: MonadFail m => ByteString -> m EbHash
mkEbHash = fmap MkEbHash . packByteString

encodeEbHash :: EbHash -> Encoding
encodeEbHash (MkEbHash bytes) = encodeFixedSized bytes

decodeEbHash :: Decoder s EbHash
decodeEbHash = MkEbHash <$> decodeFixedSized

prettyEbHash :: EbHash -> String
prettyEbHash = BS8.unpack . BS16.encode . ebHashBytes

-- | Hash of a Ranking Block
--
-- A Ranking Block is the Praos Block. While the regular Praos headers are parameterised
-- over 'blk', we choose to keep 'RbHash' monomorphic. Use the 'ConvertRawHash' type class
-- to convert between this type and 'HeaderHash'.
newtype RbHash = MkRbHash {rbHashBytes :: ByteString}
  deriving newtype (Eq, Ord, NoThunks)
  deriving stock Generic

instance Show RbHash where
  show = prettyRbHash

encodeRbHash :: RbHash -> Encoding
encodeRbHash (MkRbHash bytes) = CBOR.encodeBytes bytes

-- | Fails unless the hash is exactly 32 bytes.
decodeRbHash :: Decoder s RbHash
decodeRbHash = do
  bytes <- CBOR.decodeBytes
  when (BS.length bytes /= 32) $
    fail $
      "RbHash: " <> show (BS.length bytes) <> " bytes, expected 32"
  pure $ MkRbHash bytes

prettyRbHash :: RbHash -> String
prettyRbHash (MkRbHash bytes) = BS8.unpack (BS16.encode bytes)

instance SignableRepresentation RbHash where
  getSignableRepresentation point =
    toStrictByteString $
      encodeRbHash point

-- | Hash of a Leios transaction (the Blake2b-256 hash of its bytes).
--
-- The fixed length makes decoding reject a peer-supplied hash of any other
-- length, and lets the tx cache read the four key words directly.
newtype TxHash = MkTxHash (PackedBytes 32)
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData, NoThunks)

instance Show TxHash where
  show = prettyTxHash

txHashBytes :: TxHash -> ByteString
txHashBytes (MkTxHash bytes) = unpackPinnedBytes bytes

-- | Fails unless the input is exactly 32 bytes.
mkTxHash :: MonadFail m => ByteString -> m TxHash
mkTxHash = fmap MkTxHash . packByteString

prettyTxHash :: TxHash -> String
prettyTxHash = BS8.unpack . BS16.encode . txHashBytes

-- * Points

-- | Uniquely identifies an endorser block in Leios. Could use 'Block SlotNo
-- EbHash' eventually, but a dedicated type is better to explore.
data LeiosPoint = MkLeiosPoint {pointSlotNo :: SlotNo, pointEbHash :: EbHash}
  deriving stock (Eq, Ord, Generic)
  deriving anyclass NoThunks

instance ShowProxy LeiosPoint where showProxy _ = "LeiosPoint"

instance Show LeiosPoint where
  show = prettyLeiosPoint

instance SignableRepresentation LeiosPoint where
  getSignableRepresentation point =
    toStrictByteString $
      -- REVIEW: Flat concatenation expected as what is signed?
      encode (pointSlotNo point)
        <> encodeEbHash (pointEbHash point)

prettyLeiosPoint :: LeiosPoint -> String
prettyLeiosPoint (MkLeiosPoint (SlotNo slotNo) ebHash) =
  "(" ++ show slotNo ++ ", " ++ prettyEbHash ebHash ++ ")"

-- * Endorser blocks

type BytesSize = Word32

-- | A transaction's offset (index) into an EB body.
type TxOffset = Int

-- | Where a tx's durable bytes live: an EB that references it, and the offset
-- of its row there. Recorded in the LeiosTxCache on write confirmation, read
-- back for cross-EB fill: a later EB referencing the same tx copies the bytes
-- locally instead of re-fetching. Staleness is harmless -- the fill's guards
-- make a vanished source a no-op and the tx stays in the fetch set.
data TxLocation = MkTxLocation !EbHash !TxOffset
  deriving (Eq, Show)

-- | An Endorser Block as it is submitted through the network.
-- TODO: Keep track of the slot of an EB?
data LeiosEb = MkLeiosEb
  { leiosEbTxs :: !(Vector (TxHash, BytesSize))
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

instance ShowProxy LeiosEb where showProxy _ = "LeiosEb"

leiosEbBodyItems :: LeiosEb -> [(Int, TxHash, BytesSize)]
leiosEbBodyItems eb =
  leiosEbTxs eb
    & V.imap (\ix (txh, size) -> (ix, txh, size))
    & toList

-- | The bytes one reference occupies for a transaction of the given size: the
-- hash and the size itself, exactly as the EB's CBOR encoding writes them.
--
-- Also used on the Dijkstra transaction measure, so the mempool charges a
-- transaction what it will actually cost in the reference list rather than an
-- approximation of it.
encodeLeiosEbItemSize :: ByteSize32 -> ByteSize32
encodeLeiosEbItemSize (ByteSize32 txSize) =
  ByteSize32 $ cborBytesSize 32 + cborIntBytesSize txSize
 where
  cborBytesSize len = cborIntBytesSize len + len

-- | Encode a 'LeiosEb' as a CBOR map from each transaction hash to the size
-- of that transaction. Must not add more overhead than
-- 'encodeLeiosEbMaxFramingSize', and the encoding of each reference must match
-- 'encodeLeiosEbItemSize'.
--
-- The mempool charges each transaction 'encodeLeiosEbItemSize', and the
-- endorser-block capacity subtracts 'encodeLeiosEbMaxFramingSize'. If the
-- encoding changes, both sizes must change with it.
--
-- Endorser blocks are on chain, so peers on every node-to-node version must
-- write the same bytes. This encoding has no version yet. When an era after
-- Dijkstra changes it, the @ProtVer@ of that era will select the encoding.
encodeLeiosEb :: LeiosEb -> Encoding
encodeLeiosEb (MkLeiosEb references) =
  foldl
    ( \acc (MkTxHash txHash, txBytesSize) ->
        acc <> encodeFixedSized txHash <> CBOR.encodeWord32 txBytesSize
    )
    (CBOR.encodeMapLen $ fromIntegral $ length references)
    references

-- | The widest the framing 'encodeLeiosEb' writes around the references can
-- get: the map header. CBOR map headers have the same widths as
-- 'cborIntBytesSize', and the reference count fits 'BytesSize'.
--
-- A capacity expressed in references ('encodeLeiosEbItemSize' each)
-- subtracts this once at the block level; no single transaction can be
-- charged for it.
encodeLeiosEbMaxFramingSize :: ByteSize32
encodeLeiosEbMaxFramingSize = ByteSize32 $ cborIntBytesSize (maxBound :: BytesSize)

-- | The references capacity a @maxEndorserBlockReferencesSize@ parameter
-- yields: the parameter minus the framing 'encodeLeiosEb' writes ahead of the
-- references. A parameter smaller than the framing exhausts the capacity, so
-- no reference fits, rather than wrapping around to \"no limit\".
leiosReferencesCapacity :: BytesSize -> BytesSize
leiosReferencesCapacity paramLimit = paramLimit - min paramLimit framing
 where
  ByteSize32 framing = encodeLeiosEbMaxFramingSize

-- | Compute the size of a 'LeiosEb' in its CBOR encoding.
encodeLeiosEbSize :: LeiosEb -> BytesSize
encodeLeiosEbSize (MkLeiosEb items) =
  cborIntBytesSize (length items)
    + sum (fmap (unByteSize32 . encodeLeiosEbItemSize . ByteSize32 . snd) items)

-- | Length of a unsigned integer if it were encoded in a "flattened format".
-- See 'encodeInteger'.
cborIntBytesSize :: Integral i => i -> BytesSize
cborIntBytesSize n
  | n < 24 = 1
  | n < 0x100 = 2
  | n < 0x10000 = 3
  | otherwise = 5

-- * Header checks

-- | The earliest slot at which a block may certify an endorser block announced
-- in the given slot.
--
-- The announcement, voting and diffusion periods must all have elapsed. They
-- are wall-clock durations, so the gap rounds up to whole slots: a block is
-- forged at its slot's onset, so the answer is the first slot whose onset is far
-- enough after the announcement.
minCertificationSlot ::
  SlotLength ->
  -- | Announcement period length
  Milliseconds32 ->
  -- | Vote period length
  Milliseconds32 ->
  -- | Diffusion period length
  Milliseconds32 ->
  -- | Slot of the announcing block
  SlotNo ->
  SlotNo
minCertificationSlot slotLength announcement vote diffusion announcingSlot =
  announcingSlot + SlotNo (fromIntegral ((totalMs + slotMs - 1) `div` slotMs))
 where
  totalMs = 3 * ms announcement + ms vote + ms diffusion
  ms = toInteger . unMilliseconds32
  slotMs = case slotLengthToMillisec slotLength of
    0 -> error "minCertificationSlot: zero slot length"
    n -> n
