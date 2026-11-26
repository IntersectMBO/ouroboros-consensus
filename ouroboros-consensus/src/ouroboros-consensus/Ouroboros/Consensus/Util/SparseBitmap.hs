{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- | A non-empty, sparse bitmap: 64-bit chunks keyed by their index, only the
-- non-zero ones present.
--
-- Intended for qualified import:
--
-- > import qualified Ouroboros.Consensus.Util.SparseBitmap as SparseBitmap
module Ouroboros.Consensus.Util.SparseBitmap
  ( SparseBitmap
  , ChunkWidth
  , fromIndices
  , toIndices
  , pretty
  , encodeCbor
  , decodeCbor
  ) where

import qualified Codec.CBOR.ByteArray as CBOR.BA
import qualified Codec.CBOR.ByteArray.Sliced as Sliced
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Control.DeepSeq (NFData)
import Control.Monad (when)
import Data.Bitmap (Bitmap)
import qualified Data.Bitmap as Bitmap
import Data.IntMap.NonEmpty (NEIntMap)
import qualified Data.IntMap.NonEmpty as NEIntMap
import qualified Data.IntSet as IntSet
import Data.IntSet.NonEmpty (NEIntSet)
import qualified Data.IntSet.NonEmpty as NEIntSet
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEList
import Data.Primitive.ByteArray (foldrByteArray)
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import qualified Numeric

-- | The indices one chunk covers.
type ChunkWidth = 64

-- | Chunks keyed by their index, each non-empty.
newtype SparseBitmap = SparseBitmap (NEIntMap (Bitmap ChunkWidth))
  deriving stock (Show, Eq)
  deriving newtype NFData

chunkWidth :: Int
chunkWidth = Bitmap.capacityBits (Proxy @ChunkWidth)

-- | The bitmap naming these indices, which must be non-negative.
fromIndices :: NEIntSet -> SparseBitmap
fromIndices indices =
  SparseBitmap $
    fmap Bitmap.fromIndices $
      NEIntMap.fromListWith IntSet.union $
        fmap (\i -> (i `quot` chunkWidth, IntSet.singleton (i `rem` chunkWidth))) $
          NEIntSet.toList indices

-- | The indices set across all chunks.
toIndices :: SparseBitmap -> NEIntSet
toIndices (SparseBitmap chunks) =
  NEIntSet.unsafeFromSet $
    IntSet.unions
      [ IntSet.map (chunkIndex * chunkWidth +) (Bitmap.toIndices bitmap)
      | (chunkIndex, bitmap) <- NEList.toList (NEIntMap.toList chunks)
      ]

-- | One @index:0x...@ entry per chunk, in ascending chunk-index order.
pretty :: SparseBitmap -> [String]
pretty (SparseBitmap chunks) =
  [ show chunkIndex <> ":0x" <> concatMap hex2 (foldrByteArray (:) [] (Bitmap.toByteArray bitmap))
  | (chunkIndex, bitmap) <- NEList.toList (NEIntMap.toList chunks)
  ]

hex2 :: Word8 -> String
hex2 w = case Numeric.showHex w "" of
  [c] -> ['0', c]
  cs -> cs

-- | An indefinite-length map from chunk index to the chunk's bytes;
-- @encodeIndex@ chooses the wire representation of chunk indices.
encodeCbor :: (Int -> CBOR.Encoding) -> SparseBitmap -> CBOR.Encoding
encodeCbor encodeIndex (SparseBitmap chunks) =
  CBOR.encodeMapLenIndef
    <> NEIntMap.foldrWithKey
      (\chunkIndex bitmap rest -> encodeIndex chunkIndex <> encodeChunk bitmap <> rest)
      CBOR.encodeBreak
      chunks

encodeChunk :: Bitmap ChunkWidth -> CBOR.Encoding
encodeChunk = CBOR.encodeByteArray . Sliced.fromByteArray . Bitmap.toByteArray

-- | @decodeCbor decodeIndex@ accepts either map encoding. Chunk indices must
-- ascend strictly and chunks must be non-zero; the enclosing message's size
-- limit bounds the entry count.
decodeCbor :: CBOR.Decoder s Int -> CBOR.Decoder s SparseBitmap
decodeCbor decodeIndex = do
  entries <-
    CBOR.decodeMapLenOrIndef >>= \case
      Nothing -> CBOR.decodeSequenceLenIndef (flip (:)) [] reverse (decodeEntry decodeIndex)
      Just n -> CBOR.decodeSequenceLenN (flip (:)) [] reverse n (decodeEntry decodeIndex)
  case entries of
    [] -> fail "SparseBitmap: no chunks"
    first : rest -> do
      when (or (zipWith (\(i, _) (j, _) -> i >= j) entries rest)) $
        fail "SparseBitmap: chunk indices are not strictly ascending"
      pure $ SparseBitmap $ NEIntMap.fromDistinctAscList (first :| rest)

decodeEntry :: CBOR.Decoder s Int -> CBOR.Decoder s (Int, Bitmap ChunkWidth)
decodeEntry decodeIndex = do
  chunkIndex <- decodeIndex
  when (chunkIndex < 0) $
    fail $
      "SparseBitmap: chunk index " <> show chunkIndex <> " is negative"
  bytes <- CBOR.BA.unBA <$> CBOR.decodeByteArray
  case Bitmap.fromByteArray bytes of
    Nothing -> fail "SparseBitmap: chunk exceeds 64 bits"
    Just bitmap
      | Bitmap.sizeInBytes bitmap == 0 -> fail "SparseBitmap: chunk is zero"
      | otherwise -> pure (chunkIndex, bitmap)
