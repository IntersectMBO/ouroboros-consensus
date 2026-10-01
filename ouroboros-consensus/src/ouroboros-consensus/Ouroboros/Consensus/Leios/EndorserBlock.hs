-- | The encoding of a Leios endorser block, and the sizes the mempool charges
-- for it.
module Ouroboros.Consensus.Leios.EndorserBlock
  ( BytesSize
  , EndorserBlock (..)
  , TxHash (..)
  , cborIntBytesSize
  , encodeEndorserBlock
  , encodedMaxFramingSize
  , encodedReferenceSize
  , referencesCapacity
  ) where

import Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import Data.ByteString (ByteString)
import Data.Vector.Strict (Vector)
import Data.Word (Word32)
import Ouroboros.Consensus.Ledger.SupportsMempool (ByteSize32 (..))

-- | Hash of a whole encoded transaction. It is deliberately different from
-- a 'TxId', which is the hash of the transaction body only.
newtype TxHash = MkTxHash ByteString
  deriving (Eq, Show)

type BytesSize = Word32

-- | An endorser block: a reference, hash and size, to each transaction it
-- endorses.
newtype EndorserBlock = MkEndorserBlock
  { endorserBlockReferences :: Vector (TxHash, BytesSize)
  }
  deriving (Eq, Show)

-- | Encode an 'EndorserBlock' as a CBOR map from each transaction hash to the
-- size of that transaction. Must not add more overhead than
-- 'encodedMaxFramingSize', and the encoding of each reference must match
-- 'encodedReferenceSize'.
--
-- The mempool charges each transaction 'encodedReferenceSize', and the
-- endorser-block capacity subtracts 'encodedMaxFramingSize'. If the encoding
-- changes, both sizes must change with it.
--
-- Endorser blocks are on chain, so peers on every node-to-node version must
-- write the same bytes. This encoding has no version yet. When an era after
-- Dijkstra changes it, the @ProtVer@ of that era will select the encoding.
encodeEndorserBlock :: EndorserBlock -> Encoding
encodeEndorserBlock (MkEndorserBlock references) =
  foldl
    ( \acc (MkTxHash bytes, txBytesSize) ->
        acc <> CBOR.encodeBytes bytes <> CBOR.encodeWord32 txBytesSize
    )
    (CBOR.encodeMapLen $ fromIntegral $ length references)
    references

-- | The bytes one reference, the hash and the size, occupies for a
-- transaction of the given size, exactly as 'encodeEndorserBlock' writes them.
encodedReferenceSize :: ByteSize32 -> ByteSize32
encodedReferenceSize (ByteSize32 txSize) =
  ByteSize32 $ cborBytesSize 32 + cborIntBytesSize txSize
 where
  cborBytesSize len = cborIntBytesSize len + len

-- | The widest the framing 'encodeEndorserBlock' writes around the
-- references can get: the map header. CBOR map headers have the same widths
-- as 'cborIntBytesSize', and the reference count fits 'BytesSize'.
--
-- A capacity expressed in references ('encodedReferenceSize' each)
-- subtracts this once at the block level; no single transaction can be
-- charged for it.
encodedMaxFramingSize :: ByteSize32
encodedMaxFramingSize = ByteSize32 $ cborIntBytesSize (maxBound :: BytesSize)

-- | The references capacity a @maxEndorserBlockReferencesSize@ parameter
-- yields: the parameter minus the framing 'encodeEndorserBlock' writes ahead of the
-- references. A parameter smaller than the framing exhausts the capacity, so
-- no reference fits, rather than wrapping around to \"no limit\".
referencesCapacity :: BytesSize -> BytesSize
referencesCapacity paramLimit = paramLimit - min paramLimit framing
 where
  ByteSize32 framing = encodedMaxFramingSize

-- | Length of an unsigned integer, or of a CBOR length header, in the shortest
-- form CBOR allows. @cborg@ writes this form.
cborIntBytesSize :: Integral i => i -> BytesSize
cborIntBytesSize n
  | n < 24 = 1
  | n < 0x100 = 2
  | n < 0x10000 = 3
  | otherwise = 5
