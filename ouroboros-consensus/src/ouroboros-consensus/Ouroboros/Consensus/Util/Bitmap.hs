{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A compact bitmap representation using serialisation-ready ByteStrings.
--
-- Adapted from @Cardano.Leios.BitMapPV@ in the @leios-wfa-ls-demo@ package.
--
-- NOTE: this module is meant to be imported qualified.
module Ouroboros.Consensus.Util.Bitmap
  ( Bitmap
  , fromIndices
  , toIndices
  , logicalUpperBound
  , rawSerialise
  , rawDeserialise
  ) where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Control.Monad (forM_, when)
import Data.Bits
  ( countTrailingZeros
  , popCount
  , unsafeShiftL
  , (.&.)
  , (.|.)
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as ByteString
import Data.Word (Word8)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Storable (peekByteOff, pokeByteOff)

-- | A compact bitmap representation over an index type.
--
-- NOTE: the logical upper bound is stored explicitly so serialisation
-- round-trips exactly.
data Bitmap a
  = Bitmap
      -- | Logical upper bound
      !a
      -- | Payload
      !ByteString
  deriving Eq

instance Show a => Show (Bitmap a) where
  show (Bitmap maxIx bs) =
    "Bitmap{maxIx="
      <> show maxIx
      <> ",bytes="
      <> show (ByteString.length bs)
      <> ",set="
      <> show (countSetBits bs)
      <> "}"
   where
    countSetBits arr =
      sum
        [ popCount (ByteString.index arr i)
        | i <- [0 .. ByteString.length arr - 1]
        ]

-- | Construct a 'Bitmap' from a list of indexes that should be set (flipped to
-- 1) and a maximum index (inclusive logical upper bound).
fromIndices :: Integral a => a -> [a] -> Bitmap a
fromIndices maxIx flipped =
  Bitmap maxIx $
    ByteString.unsafeCreate nBytes $ \ptr -> do
      fillBytes ptr 0 nBytes
      forM_ flipped $ \ix -> do
        let !i = fromIntegral ix :: Int
        when (i >= 0 && i <= maxI) $ do
          let !byteIx = i `quot` 8
          let !bitIx = i `rem` 8
          let !mask = bitMask bitIx
          w <- peekByteOff ptr byteIx :: IO Word8
          pokeByteOff ptr byteIx (w .|. mask)
 where
  !maxI = fromIntegral maxIx :: Int
  !nBytes = (maxI `quot` 8) + 1

  bitMask k = fromIntegral ((1 :: Int) `unsafeShiftL` k)

-- | Retrieve all indexes that are set (flipped to 1) in the bitmap, in
-- ascending order.
toIndices :: Integral a => Bitmap a -> [a]
toIndices (Bitmap maxIx bitmap) =
  goBytes 0
 where
  !maxI = fromIntegral maxIx :: Int
  !nBytes = ByteString.length bitmap

  goBytes !byteIx
    | byteIx >= nBytes = []
    | otherwise =
        let !w = ByteString.index bitmap byteIx
         in goBits (byteIx * 8) w <> goBytes (byteIx + 1)

  goBits !_ 0 = []
  goBits !base !w =
    let !bitIx = countTrailingZeros w
        !i = base + bitIx
        !w' = w .&. (w - 1)
     in if i <= maxI
          then fromIntegral i : goBits base w'
          else []

-- | Get the logical upper bound of a bitmap
logicalUpperBound :: Bitmap a -> a
logicalUpperBound (Bitmap a _) = a

-- | Raw serialisation of the bitmap (just the underlying bytes, without the
-- logical upper bound).
rawSerialise :: Bitmap a -> ByteString
rawSerialise (Bitmap _ bs) = bs

-- | Raw deserialisation of a bitmap from a logical upper bound and a ByteString
--
-- Returns 'Nothing' if the byte string length does not match the expected size
-- for the given upper bound.
rawDeserialise :: Integral a => a -> ByteString -> Maybe (Bitmap a)
rawDeserialise maxIx bs
  | ByteString.length bs /= expectedBytes = Nothing
  | otherwise = Just (Bitmap maxIx bs)
 where
  expectedBytes = (fromIntegral maxIx `quot` 8) + 1

instance ToCBOR a => ToCBOR (Bitmap a) where
  toCBOR (Bitmap maxIx bs) =
    CBOR.encodeListLen 2
      <> toCBOR maxIx
      <> CBOR.encodeBytes bs

instance (Integral a, FromCBOR a) => FromCBOR (Bitmap a) where
  fromCBOR = do
    CBOR.decodeListLenOf 2
    maxIx <- fromCBOR
    bs <- CBOR.decodeBytes
    case rawDeserialise maxIx bs of
      Nothing ->
        fail "Bitmap: invalid bitmap data or size mismatch"
      Just bitmap ->
        pure bitmap
