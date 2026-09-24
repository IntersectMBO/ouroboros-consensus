-- | The encoding of a Leios endorser block, and the sizes the mempool charges
-- for it.
module Ouroboros.Consensus.Leios.EndorserBlock
  ( BytesSize
  , LeiosEb (..)
  , TxHash (..)
  , encodeLeiosEb
  , encodeLeiosEbItemSize
  , encodeLeiosEbMaxFramingSize
  , leiosReferencesCapacity
  ) where

import Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import Data.ByteString (ByteString)
import Data.Vector.Strict (Vector)
import Data.Word (Word32)
import Ouroboros.Consensus.Ledger.SupportsMempool (ByteSize32 (..))

-- | Hash of a transaction.
newtype TxHash = MkTxHash ByteString
  deriving (Eq, Show)

type BytesSize = Word32

-- | An endorser block: a reference, hash and size, to each transaction it
-- endorses.
newtype LeiosEb = MkLeiosEb
  { leiosEbTxs :: Vector (TxHash, BytesSize)
  }
  deriving (Eq, Show)

-- | Encode a 'LeiosEb' with all its items. Must not add more overhead than
-- 'encodeLeiosEbMaxFramingSize' and individual item encodings must match
-- 'encodeLeiosEbItemSize'.
encodeLeiosEb :: LeiosEb -> Encoding
encodeLeiosEb (MkLeiosEb v) =
  foldl
    ( \acc (MkTxHash bytes, txBytesSize) ->
        acc <> CBOR.encodeBytes bytes <> CBOR.encodeWord32 txBytesSize
    )
    (CBOR.encodeMapLen $ fromIntegral $ length v)
    v

-- | The widest the map-length prefix 'encodeLeiosEb' writes ahead of the items
-- can get. CBOR map headers have the same widths as 'cborIntBytesSize', and the
-- item count fits 'BytesSize'.
--
-- A capacity expressed in references ('encodeLeiosEbItemSize' each) subtracts
-- this once at the block level; no single transaction can be charged for it.
encodeLeiosEbMaxFramingSize :: ByteSize32
encodeLeiosEbMaxFramingSize = ByteSize32 $ cborIntBytesSize (maxBound :: BytesSize)

-- | The references capacity a @maxEndorserBlockReferencesSize@ parameter
-- yields: the parameter less the framing 'encodeLeiosEb' writes ahead of the
-- references. A parameter smaller than the framing exhausts the capacity, so
-- no reference fits, rather than wrapping around to \"no limit\".
leiosReferencesCapacity :: BytesSize -> BytesSize
leiosReferencesCapacity paramLimit = paramLimit - min paramLimit framing
 where
  ByteSize32 framing = encodeLeiosEbMaxFramingSize

-- | The bytes one reference occupies for a transaction of the given size: the
-- hash and the size itself, exactly as 'encodeLeiosEb' writes them.
--
-- The Dijkstra transaction measure charges this, so the mempool charges a
-- transaction what its reference costs in an endorser block.
encodeLeiosEbItemSize :: ByteSize32 -> ByteSize32
encodeLeiosEbItemSize (ByteSize32 txSize) =
  ByteSize32 $ cborBytesSize 32 + cborIntBytesSize txSize
 where
  cborBytesSize len = cborIntBytesSize len + len

-- | Length of an unsigned integer, or of a CBOR length header, in the shortest
-- form CBOR allows. @cborg@ writes this form.
cborIntBytesSize :: Integral i => i -> BytesSize
cborIntBytesSize n
  | n < 24 = 1
  | n < 0x100 = 2
  | n < 0x10000 = 3
  | otherwise = 5
