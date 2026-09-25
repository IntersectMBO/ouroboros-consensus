-- | The encoding of a Leios endorser block, and the sizes the mempool charges
-- for it.
module Ouroboros.Consensus.Leios.EndorserBlock
  ( BytesSize
  , EndorserBlockFormat (..)
  , LeiosEb (..)
  , TxHash (..)
  , cborIntBytesSize
  , leiosReferencesCapacity
  ) where

import Codec.CBOR.Encoding (Encoding)
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

-- | The encoding of an endorser block in an era, and the sizes that predict
-- it.
--
-- The mempool charges each transaction 'encodeLeiosEbItemSize', and the
-- endorser-block capacity subtracts 'encodeLeiosEbMaxFramingSize'. Both must
-- agree with 'encodeLeiosEb' of the same era. An era that changes the
-- encoding changes all three in its own instance.
class EndorserBlockFormat era where
  -- | Encode a 'LeiosEb' with all its items. Must not add more overhead than
  -- 'encodeLeiosEbMaxFramingSize' and individual item encodings must match
  -- 'encodeLeiosEbItemSize'.
  encodeLeiosEb :: proxy era -> LeiosEb -> Encoding

  -- | The bytes one reference occupies for a transaction of the given size,
  -- exactly as 'encodeLeiosEb' writes them.
  encodeLeiosEbItemSize :: proxy era -> ByteSize32 -> ByteSize32

  -- | The widest the framing 'encodeLeiosEb' writes around the items can get.
  --
  -- A capacity expressed in references ('encodeLeiosEbItemSize' each)
  -- subtracts this once at the block level; no single transaction can be
  -- charged for it.
  encodeLeiosEbMaxFramingSize :: proxy era -> ByteSize32

-- | The references capacity a @maxEndorserBlockReferencesSize@ parameter
-- yields: the parameter less the framing 'encodeLeiosEb' writes ahead of the
-- references. A parameter smaller than the framing exhausts the capacity, so
-- no reference fits, rather than wrapping around to \"no limit\".
leiosReferencesCapacity :: EndorserBlockFormat era => proxy era -> BytesSize -> BytesSize
leiosReferencesCapacity p paramLimit = paramLimit - min paramLimit framing
 where
  ByteSize32 framing = encodeLeiosEbMaxFramingSize p

-- | Length of an unsigned integer, or of a CBOR length header, in the shortest
-- form CBOR allows. @cborg@ writes this form.
cborIntBytesSize :: Integral i => i -> BytesSize
cborIntBytesSize n
  | n < 24 = 1
  | n < 0x100 = 2
  | n < 0x10000 = 3
  | otherwise = 5
