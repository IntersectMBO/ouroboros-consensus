{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A non-empty, sparse or chunked bitmap.
--
-- Intended for qualified import:
--
-- > import qualified Ouroboros.Consensus.Util.SparseBitmap as SparseBitmap
module Ouroboros.Consensus.Util.SparseBitmap
  ( SparseBitmap
  , fromIndices
  , toIndices
  , popCount
  , pretty
  , encodeCbor
  , decodeCbor
  , propSparseBitmap
  ) where

import qualified Codec.CBOR.ByteArray as CBOR.BA
import qualified Codec.CBOR.ByteArray.Sliced as Sliced
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import Control.DeepSeq (NFData (rnf))
import Control.Monad (foldM, unless, when)
import Control.Monad.Except (Except, runExcept, throwError)
import qualified Data.Bitmap.Strict.NonEmpty as NEBitmap
import qualified Data.ByteString.Lazy as BSL
import Data.IntMap.NonEmpty (NEIntMap)
import qualified Data.IntMap.NonEmpty as NEIntMap
import Data.IntSet.NonEmpty (NEIntSet)
import qualified Data.IntSet.NonEmpty as NEIntSet
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEList
import Data.Primitive.ByteArray (foldrByteArray)
import Data.Word (Word16, Word8)
import qualified Numeric

-- * API

-- | A sparse bitmap over indices in @[0, 'sbTotalWidth')@. Chunks are keyed by
-- their zero-based index and are non-zero. Every chunk except the last has
-- width 'sbChunkWidth', the last chunk has the remaining width.
data SparseBitmap = SparseBitmap
  { sbMap :: NEIntMap NEBitmap.NonEmptyBitmap
  , sbChunkWidth :: Int
  , sbTotalWidth :: Int
  }
  deriving stock (Show, Eq)

instance NFData SparseBitmap where
  rnf SparseBitmap{..} = rnf sbMap `seq` rnf sbChunkWidth `seq` rnf sbTotalWidth

-- | @fromIndices width totalWidth indices@ constructs a sparse bitmap @totalWidth@ long from absolute bit indices.
--
-- Fails if 'width' or 'totalWidth' is non-positive or an index in @indices@ is outside @[0, totalWidth)@.
fromIndices :: Int -> Int -> NEIntSet -> Except String SparseBitmap
fromIndices chunkWidth _ _ | chunkWidth <= 0 = throwError "SparseBitmap: chunk width must be positive"
fromIndices _ totalWidth _ | totalWidth <= 0 = throwError "SparseBitmap: total width must be positive"
fromIndices chunkWidth totalWidth indices = do
  let (first NEList.:| rest) = NEIntSet.toAscList indices
  (firstChunkIndex, firstIndexInChunk) <- chunkPart first

  indicesByChunk <-
    foldM
      ( \chunks index -> do
          (chunkIndex, indexInChunk) <- chunkPart index
          return $ NEIntMap.insertWith (<>) chunkIndex (NEIntSet.singleton indexInChunk) chunks
      )
      (NEIntMap.singleton firstChunkIndex (NEIntSet.singleton firstIndexInChunk))
      rest

  chunks <-
    NEIntMap.traverseWithKey (\chunkIndex -> mkBitmap (chunkWidthFor' chunkIndex)) indicesByChunk

  pure $ SparseBitmap chunks chunkWidth totalWidth
 where
  mkBitmap :: Int -> NEIntSet -> Except String NEBitmap.NonEmptyBitmap
  mkBitmap width idxs =
    case NEBitmap.fromIndices width (fmap fromIntegral (NEIntSet.toAscList idxs) :: NonEmpty Word16) of
      Nothing -> throwError $ "SparseBitmap: an index is not below the chunk width " <> show width
      Just bitmap -> pure bitmap

  chunkPart :: Int -> Except String (Int, Int)
  chunkPart index
    | index < 0 = throwError $ "SparseBitmap: index " <> show index <> " must be non-negative"
    | index >= totalWidth =
        throwError $ "SparseBitmap: index " <> show index <> " must be below the total width"
  chunkPart index =
    let (chunkIndex, indexInChunk) = index `divMod` chunkWidth
     in pure (chunkIndex, indexInChunk)

  chunkWidthFor' = chunkWidthFor chunkWidth totalWidth

-- | The absolute indices whose bits are set.
toIndices :: SparseBitmap -> NEIntSet
toIndices SparseBitmap{..} =
  NEIntSet.unions $
    fmap indicesInChunk (NEIntMap.toAscList sbMap)
 where
  indicesInChunk (chunkIndex, bitmap) =
    NEIntSet.map (sbChunkWidth * chunkIndex +) (NEBitmap.toIndices bitmap)

-- | The total number of set bits across all chunks.
popCount :: SparseBitmap -> Int
popCount = sum . fmap NEBitmap.numSetBits . NEList.toList . NEIntMap.elems . sbMap

-- | One @index:0x...@ entry per chunk, in ascending chunk-index order.
pretty :: SparseBitmap -> [String]
pretty SparseBitmap{..} =
  fmap showChunk (NEList.toList (NEIntMap.toAscList sbMap))
 where
  showChunk (chunkIndex, bitmap) =
    show chunkIndex
      <> ":0x"
      <> concatMap hex2 (foldrByteArray (:) [] (NEBitmap.bitmapRawByteArray bitmap))

  hex2 :: Word8 -> String
  hex2 word =
    let hex = Numeric.showHex word ""
     in replicate (2 - length hex) '0' <> hex

-- * CBOR

-- | @encodeCbor encodeIndex sparseBitmap@ encodes @sparseBitmap@ as an
-- indefinite-length CBOR map. @encodeIndex@ chooses the wire representation
-- of chunk indices.
encodeCbor :: (Int -> CBOR.Encoding) -> SparseBitmap -> CBOR.Encoding
encodeCbor encodeIndex SparseBitmap{..} =
  encodeSizedNEIntMap encodeIndex encodeBitmap sbMap
 where
  encodeBitmap = CBOR.encodeByteArray . Sliced.fromByteArray . NEBitmap.bitmapRawByteArray

-- | @decodeCbor chunkWidth totalWidth decodeIndex@ decodes a sparse bitmap
-- from either a definite- or indefinite-length CBOR map. @decodeIndex@
-- chooses the wire representation of chunk indices.
--
-- Entries must be strictly ascending, non-zero, and within the bitmap's
-- total width. The final chunk is checked at its remaining width, including
-- that its padding bits are clear.
decodeCbor :: Int -> Int -> CBOR.Decoder s Int -> CBOR.Decoder s SparseBitmap
decodeCbor chunkWidth totalWidth decodeIndex
  | chunkWidth <= 0 = fail "SparseBitmap: chunk width must be positive"
  | totalWidth <= 0 = fail "SparseBitmap: total width must be positive"
  | otherwise = do
      chunks <-
        decodeSizedNEIntMap
          maxChunks
          decodeChunkIndex
          decodeBitmap
      pure SparseBitmap{sbMap = chunks, sbChunkWidth = chunkWidth, sbTotalWidth = totalWidth}
 where
  lastChunkIndex' = lastChunkIndex chunkWidth totalWidth
  maxChunks = lastChunkIndex' + 1

  decodeChunkIndex = do
    chunkIndex <- decodeIndex
    when (chunkIndex < 0) $
      fail $
        "SparseBitmap: chunk index " <> show chunkIndex <> " must be non-negative"
    when (chunkIndex > lastChunkIndex') $
      fail $
        "SparseBitmap: chunk index "
          <> show chunkIndex
          <> " must be less or equal to "
          <> show lastChunkIndex'
    pure chunkIndex

  decodeBitmap chunkIndex = do
    bitmapBytes <- CBOR.decodeByteArray
    NEBitmap.mkNonEmptyBitmap (chunkWidthFor' chunkIndex) (CBOR.BA.unBA bitmapBytes)

  chunkWidthFor' = chunkWidthFor chunkWidth totalWidth

-- | @encodeSizedNEIntMap encodeKey encodeValue entries@ encodes @entries@
-- as an indefinite-length CBOR map. @encodeKey@ and @encodeValue@ choose the
-- wire representations of keys and values.
encodeSizedNEIntMap ::
  (Int -> CBOR.Encoding) ->
  (a -> CBOR.Encoding) ->
  NEIntMap a ->
  CBOR.Encoding
encodeSizedNEIntMap encodeKey encodeValue =
  (CBOR.encodeMapLenIndef <>)
    . NEIntMap.foldrWithKey
      (\key value rest -> encodeKey key <> encodeValue value <> rest)
      CBOR.encodeBreak

-- | @decodeSizedNEIntMap maxEntries decodeKey decodeValue@
-- decodes a non-empty CBOR map containing at most @maxEntries@ entries. Both
-- definite- and indefinite-length maps are accepted.
--
-- Map keys must be strictly ascending. @decodeKey@ performs any
-- caller-specific key validation before @decodeValue@ decodes and validates
-- the associated value.
decodeSizedNEIntMap ::
  Int ->
  CBOR.Decoder s Int ->
  (Int -> CBOR.Decoder s a) ->
  CBOR.Decoder s (NEIntMap a)
decodeSizedNEIntMap maxEntries decodeKey decodeValue
  | maxEntries <= 0 = fail "SizedNEIntMap: maximum size must be positive"
  | otherwise =
      CBOR.decodeMapLenOrIndef >>= \case
        Just entryCount -> decodeDefinite entryCount
        Nothing -> decodeIndefinite
 where
  tooManyEntries = "SizedNEIntMap: map must have at most " <> show maxEntries <> " entries"
  zeroEntries = "SizedNEIntMap: map must have at least one entry"

  decodeDefinite entryCount | entryCount > maxEntries = fail tooManyEntries
  decodeDefinite entryCount | entryCount == 0 = fail zeroEntries
  decodeDefinite entryCount = do
    (firstKey, firstValue) <- decodeEntry
    go (entryCount - 1) firstKey (NEIntMap.singleton firstKey firstValue)
   where
    go 0 _previousKey acc = pure acc
    go remainingEntries previousKey acc = do
      (key, value) <- decodeIncreasingEntry previousKey
      go (remainingEntries - 1) key (NEIntMap.insert key value acc)

  decodeIndefinite =
    CBOR.decodeBreakOr >>= \case
      True -> fail zeroEntries
      False -> do
        (firstKey, firstValue) <- decodeEntry
        go 1 firstKey (NEIntMap.singleton firstKey firstValue)
   where
    go entriesRead previousKey acc =
      CBOR.decodeBreakOr >>= \case
        True -> pure acc
        False
          | entriesRead >= maxEntries -> fail tooManyEntries
          | otherwise -> do
              (key, value) <- decodeIncreasingEntry previousKey
              go (entriesRead + 1) key (NEIntMap.insert key value acc)

  decodeIncreasingEntry previousKey = do
    key <- decodeKey
    when (key <= previousKey) $
      fail "SizedNEIntMap: keys are not strictly ascending"
    value <- decodeValue key
    pure (key, value)

  decodeEntry = do
    key <- decodeKey
    value <- decodeValue key
    pure (key, value)

-- * Properties

-- | 'SparseBitmap' invariants.
propSparseBitmap :: SparseBitmap -> Except String ()
propSparseBitmap sparseBitmap@SparseBitmap{..} = do
  unless (sbChunkWidth > 0) $
    throwError "SparseBitmap: chunk width must be positive"

  unless (sbTotalWidth > 0) $
    throwError "SparseBitmap: total width must be positive"

  mapM_ checkChunk (NEIntMap.toAscList sbMap)

  unless (all (< sbTotalWidth) (NEIntSet.toAscList (toIndices sparseBitmap))) $
    throwError "SparseBitmap: all indices must be below the total width"

  unless
    (runExcept (fromIndices sbChunkWidth sbTotalWidth (toIndices sparseBitmap)) == Right sparseBitmap)
    $ throwError "SparseBitmap: fromIndices . toIndices must be id"

  case CBOR.deserialiseFromBytes
    (decodeCbor sbChunkWidth sbTotalWidth CBOR.decodeInt)
    (CBOR.toLazyByteString $ encodeCbor CBOR.encodeInt sparseBitmap) of
    Right (rest, decodedSparseBitmap) -> do
      unless (BSL.null rest) $ throwError "SparseBitmap: decodeCbor must consume the entire input"
      unless (decodedSparseBitmap == sparseBitmap) $
        throwError
          ("SparseBitmap: decodeCbor . encodeCbor must be id, but got " <> show decodedSparseBitmap)
    Left err -> throwError ("SparseBitmap: decodeCbor . encodeCbor must be id, but got " <> show err)

  unless (NEIntSet.size (toIndices sparseBitmap) == popCount sparseBitmap) $
    throwError "SparseBitmap: popCount must equal the number of set indices"
 where
  checkChunk :: (Int, NEBitmap.NonEmptyBitmap) -> Except String ()
  checkChunk (chunkIndex, _bitmap)
    | chunkIndex < 0 =
        throwError $
          "SparseBitmap: chunk index " <> show chunkIndex <> " must be non-negative"
    | chunkIndex > lastChunkIndex sbChunkWidth sbTotalWidth =
        throwError $
          "SparseBitmap: chunk index " <> show chunkIndex <> " must be below the total width"
    | otherwise = pure ()

-- * Utilities

-- | The width of a valid chunk. The last chunk may be shorter than the other
-- chunks, because it ends at the final valid absolute index.
--
-- Preconditions: both widths are positive and @chunkIndex@ is in
-- @[0, lastChunkIndex chunkWidth totalWidth]@.
chunkWidthFor :: Int -> Int -> Int -> Int
chunkWidthFor chunkWidth totalWidth chunkIndex
  | chunkIndex < lastChunkIndex chunkWidth totalWidth = chunkWidth
  | otherwise = totalWidth - chunkIndex * chunkWidth

-- | The final valid absolute index is @totalWidth - 1@. Its quotient is the
-- last valid chunk index.
--
-- * chunk width 64, total width 128 → last chunk index 1
-- * chunk width 64, total width 130 → last chunk index 2
lastChunkIndex :: Int -> Int -> Int
lastChunkIndex chunkWidth totalWidth = (totalWidth - 1) `div` chunkWidth
