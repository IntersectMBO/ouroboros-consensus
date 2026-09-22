{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}

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
  , encodeLeiosEbItemSize
  , encodeLeiosEbSize
  ) where

import Cardano.Crypto.Util (SignableRepresentation (..))
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
      encode point.pointSlotNo
        <> encodeEbHash point.pointEbHash

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
