{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A compact bitmap representation for tracking voter participation or binary
-- flags.
--
-- Adapted from @Cardano.Leios.BitMapPV@ in the @leios-wfa-ls-demo@ package,
-- with the \"PV\" suffix dropped.
module Ouroboros.Consensus.Committee.BitMap
  ( BitMap
  , bitmapFromIndices
  , bitmapToIndices
  , rawSerialiseBitMap
  , rawDeserialiseBitMap
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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Word (Word8)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Storable (peekByteOff, pokeByteOff)

-- | A compact bitmap representation for tracking voter participation or binary
-- flags.  The logical upper bound is stored explicitly so serialization
-- round-trips exactly.
data BitMap a = BitMap !a !BS.ByteString
  deriving Eq

instance Show a => Show (BitMap a) where
  show (BitMap maxIx bs) =
    "BitMap{maxIx="
      ++ show maxIx
      ++ ",bytes="
      ++ show (BS.length bs)
      ++ ",set="
      ++ show (countSetBits bs)
      ++ "}"
   where
    countSetBits arr =
      sum [popCount (BS.index arr i) | i <- [0 .. BS.length arr - 1]]

-- | Construct a 'BitMap' from a list of indexes that should be set (flipped to
-- 1) and a maximum index (inclusive upper bound).
bitmapFromIndices ::
  Integral a =>
  a ->
  [a] ->
  BitMap a
bitmapFromIndices maxIx flipped =
  BitMap maxIx $
    BSI.unsafeCreate nBytes $ \ptr -> do
      fillBytes ptr 0 nBytes
      forM_ flipped $ \ix -> do
        let !i = fromIntegral ix :: Int
        when (i >= 0 && i <= maxI) $ do
          let !byteIx = i `quot` 8
              !bitIx = i `rem` 8
              !mask = bitMask bitIx
          w <- peekByteOff ptr byteIx :: IO Word8
          pokeByteOff ptr byteIx (w .|. mask)
 where
  !maxI = fromIntegral maxIx :: Int
  !nBytes = (maxI `quot` 8) + 1

-- | Retrieve all indexes that are set (flipped to 1) in the bitmap, in
-- ascending order.
bitmapToIndices :: Integral a => BitMap a -> [a]
bitmapToIndices (BitMap maxIx bitmap) =
  goBytes 0
 where
  !maxI = fromIntegral maxIx :: Int
  !nBytes = BS.length bitmap

  goBytes !byteIx
    | byteIx >= nBytes = []
    | otherwise =
        let !w = BS.index bitmap byteIx
         in goBits (byteIx * 8) w ++ goBytes (byteIx + 1)

  goBits !_ 0 = []
  goBits !base !w =
    let !bitIx = countTrailingZeros w
        !i = base + bitIx
        !w' = w .&. (w - 1)
     in if i <= maxI
          then fromIntegral i : goBits base w'
          else []

bitMask :: Int -> Word8
bitMask k = fromIntegral ((1 :: Int) `unsafeShiftL` k)

-- | Raw serialisation of the bitmap (just the underlying bytes, without the
-- logical upper bound).
rawSerialiseBitMap :: BitMap a -> BS.ByteString
rawSerialiseBitMap (BitMap _ bs) = bs

-- | Raw deserialisation of a bitmap from a logical upper bound and a
-- 'BS.ByteString'.  Returns 'Nothing' if the byte string length does not match
-- the expected size for the given upper bound.
rawDeserialiseBitMap :: Integral a => a -> BS.ByteString -> Maybe (BitMap a)
rawDeserialiseBitMap maxIx bs
  | BS.length bs /= expectedBytes = Nothing
  | otherwise = Just (BitMap maxIx bs)
 where
  expectedBytes = (fromIntegral maxIx `quot` 8) + 1

instance ToCBOR a => ToCBOR (BitMap a) where
  toCBOR (BitMap maxIx bs) =
    CBOR.encodeListLen 2
      <> toCBOR maxIx
      <> CBOR.encodeBytes bs

instance (Integral a, FromCBOR a) => FromCBOR (BitMap a) where
  fromCBOR = do
    CBOR.decodeListLenOf 2
    maxIx <- fromCBOR
    bs <- CBOR.decodeBytes
    case rawDeserialiseBitMap maxIx bs of
      Nothing -> fail "BitMap: invalid bitmap data or size mismatch"
      Just bitmap -> pure bitmap
