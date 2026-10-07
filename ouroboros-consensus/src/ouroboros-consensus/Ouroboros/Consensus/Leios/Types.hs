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
  , encodeEbHash
  , decodeEbHash
  , prettyEbHash
  , RbHash (..)
  , encodeRbHash
  , decodeRbHash
  , prettyRbHash
  , TxHash (..)
  , prettyTxHash

    -- * Points
  , LeiosPoint (..)
  , prettyLeiosPoint

    -- * Endorser blocks
  , BytesSize
  , LeiosEb (..)
  , leiosEbBodyItems
  , encodeLeiosEb
  , encodeLeiosEbItemSize
  , encodeLeiosEbMaxFramingSize
  , encodeLeiosEbSize
  , leiosReferencesCapacity

    -- * Certification
  , LeiosPeriods (..)
  , certificationGapElapsed
  ) where

import Cardano.Crypto.Util (SignableRepresentation (..))
import Cardano.Ledger.BaseTypes (Milliseconds32 (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import Codec.CBOR.Write (toStrictByteString)
import Codec.Serialise (Serialise (..))
import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as BS8
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Vector.Strict (Vector)
import qualified Data.Vector.Strict as V
import Data.Word (Word32)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( SlotLength
  , getSlotLength
  )
import Ouroboros.Consensus.Ledger.SupportsMempool (ByteSize32 (..))
import Ouroboros.Consensus.Util (ShowProxy (..))

-- * Hashes

-- | Hash of an Endorser Block
newtype EbHash = MkEbHash {ebHashBytes :: ByteString}
  deriving newtype (Eq, Ord, NoThunks, Serialise)
  deriving stock Generic

instance Show EbHash where
  show = prettyEbHash

encodeEbHash :: EbHash -> Encoding
encodeEbHash (MkEbHash bytes) = CBOR.encodeBytes bytes

decodeEbHash :: Decoder s EbHash
decodeEbHash = MkEbHash <$> CBOR.decodeBytes

prettyEbHash :: EbHash -> String
prettyEbHash (MkEbHash bytes) = BS8.unpack (BS16.encode bytes)

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

decodeRbHash :: Decoder s RbHash
decodeRbHash = MkRbHash <$> CBOR.decodeBytes

prettyRbHash :: RbHash -> String
prettyRbHash (MkRbHash bytes) = BS8.unpack (BS16.encode bytes)

instance SignableRepresentation RbHash where
  getSignableRepresentation point =
    toStrictByteString $
      encodeRbHash point

-- | Hash of a Leios transaction (the Blake2b-256 hash of its bytes).
newtype TxHash = MkTxHash ByteString
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData, NoThunks)

instance Show TxHash where
  show = prettyTxHash

prettyTxHash :: TxHash -> String
prettyTxHash (MkTxHash bytes) = BS8.unpack (BS16.encode bytes)

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
prettyLeiosPoint (MkLeiosPoint (SlotNo slotNo) (MkEbHash bytes)) =
  "(" ++ show slotNo ++ ", " ++ BS8.unpack (BS16.encode bytes) ++ ")"

-- * Endorser blocks

type BytesSize = Word32

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
    ( \acc (MkTxHash bytes, txBytesSize) ->
        acc <> CBOR.encodeBytes bytes <> CBOR.encodeWord32 txBytesSize
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

-- * Certification

-- | The three CIP-0164 periods that set how long a ranking block must wait
-- before it can certify the EB that its parent announced. The names follow the
-- ledger's protocol parameters.
data LeiosPeriods = LeiosPeriods
  { announcementPeriod :: !Milliseconds32
  -- ^ @L_hdr@ in CIP-0164
  , votePeriod :: !Milliseconds32
  -- ^ @L_vote@ in CIP-0164
  , diffusionPeriod :: !Milliseconds32
  -- ^ @L_diff@ in CIP-0164
  }
  deriving stock (Show, Eq)

-- | Whether a ranking block at slot @rbSlot'@ can certify the EB that its
-- parent, at slot @rbSlot@, announced. Block forging and block validation
-- must both use this function, so that they agree on the first allowed slot.
--
-- CIP-0164 (Step 5, rule 3) allows the certificate only if @rbSlot'@ is at
-- least @ceiling (t / slotLength)@ slots after @rbSlot@, with
-- @t = 3 * L_hdr + L_vote + L_diff@. For a whole number of slots @k@,
-- @k >= ceiling (t / slotLength)@ is true exactly when @k * slotLength >= t@.
-- This function checks the second form, because it does not divide by the
-- slot length.
certificationGapElapsed :: SlotLength -> LeiosPeriods -> SlotNo -> SlotNo -> Bool
certificationGapElapsed slotLength periods rbSlot rbSlot' =
  fromInteger slotsAfter * toRational (getSlotLength slotLength)
    >= fromInteger periodsMs / 1000
 where
  slotsAfter = toInteger (unSlotNo rbSlot') - toInteger (unSlotNo rbSlot)
  periodsMs =
    3 * ms (announcementPeriod periods)
      + ms (votePeriod periods)
      + ms (diffusionPeriod periods)
  ms = toInteger . unMilliseconds32
