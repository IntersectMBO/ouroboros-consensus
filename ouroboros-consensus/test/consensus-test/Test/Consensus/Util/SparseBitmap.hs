{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}

-- | Property-based tests for 'SparseBitmap'.
module Test.Consensus.Util.SparseBitmap (tests) where

import qualified Codec.CBOR.ByteArray.Sliced as Sliced
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Term as CBOR
import qualified Codec.CBOR.Write as CBOR
import Control.Monad.Except (runExcept)
import qualified Data.Bitmap.Strict.NonEmpty as NEBitmap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap.NonEmpty as NEIntMap
import qualified Data.IntSet as IntSet
import qualified Data.IntSet.NonEmpty as NEIntSet
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEList
import Data.Word (Word16)
import qualified Ouroboros.Consensus.Util.SparseBitmap as SparseBitmap
import Test.QuickCheck (chooseInt, listOf1, oneof, tabulate)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Gen, Property, counterexample, forAll, property, testProperty)

tests :: TestTree
tests =
  testGroup
    "SparseBitmap"
    [ testProperty "propSparseBitmap" test_propSparseBitmap
    , testProperty "fromIndices rejects invalid arguments" test_fromIndicesRejectsInvalidArguments
    , testProperty "decodeCbor rejects invalid CBOR" test_decodeCborRejectsInvalidCbor
    ]

test_propSparseBitmap :: Property
test_propSparseBitmap =
  forAll genSparseBitmap $ \(constructorLabel, sparseBitmap) ->
    tabulate "constructor" [constructorLabel] $
      case runExcept (SparseBitmap.propSparseBitmap sparseBitmap) of
        Left err -> counterexample err (property False)
        Right () -> property True

test_fromIndicesRejectsInvalidArguments :: Property
test_fromIndicesRejectsInvalidArguments =
  forAll genInvalidFromIndicesArgs $ \(FromIndicesArgs chunkWidth totalWidth indices, expectedError, errorType) ->
    tabulate "error" [errorType] $
      case runExcept (SparseBitmap.fromIndices chunkWidth totalWidth indices) of
        Left actualError
          | actualError == expectedError -> property True
          | otherwise ->
              counterexample
                ("expected error: " <> show expectedError <> "; got: " <> show actualError)
                (property False)
        Right sparseBitmap ->
          counterexample ("accepted invalid arguments: " <> show sparseBitmap) (property False)

test_decodeCborRejectsInvalidCbor :: Property
test_decodeCborRejectsInvalidCbor =
  forAll genInvalidDecodeCborArgs $ \(DecodeCborArgs chunkWidth totalWidth cbor, expectedError, errorType) ->
    tabulate "error" [errorType] $
      case CBOR.deserialiseFromBytes
        (SparseBitmap.decodeCbor chunkWidth totalWidth CBOR.decodeInt)
        (CBOR.toLazyByteString $ CBOR.encodeTerm cbor) of
        Left (CBOR.DeserialiseFailure _offset actualError)
          | expectedError == actualError -> property True
          | otherwise ->
              counterexample
                ("expected error containing: " <> show expectedError <> "; got: " <> show actualError)
                (property False)
        Right (_, sparseBitmap) ->
          counterexample ("accepted invalid CBOR: " <> show sparseBitmap) (property False)

genSparseBitmap :: Gen (String, SparseBitmap.SparseBitmap)
genSparseBitmap =
  oneof
    [ ("from indices",) <$> genSparseBitmapFromIndices
    , (\(lbl, x) -> ("from CBOR: " <> lbl, x)) <$> genSparseBitmapFromDecodeCbor
    ]

data FromIndicesArgs = FromIndicesArgs Int Int NEIntSet.NEIntSet deriving stock (Eq, Show)

genValidFromIndicesArgs :: Gen FromIndicesArgs
genValidFromIndicesArgs = do
  chunkWidth <- chooseInt (1, 256)
  totalWidth <- chooseInt (1, 10000)
  indices <- listOf1 (chooseInt (0, totalWidth - 1))
  case NEIntSet.nonEmptySet (IntSet.fromList indices) of
    Nothing -> error "listOf1 produced no indices"
    Just nonEmptyIndices -> pure $ FromIndicesArgs chunkWidth totalWidth nonEmptyIndices

genInvalidFromIndicesArgs :: Gen (FromIndicesArgs, String, String)
genInvalidFromIndicesArgs =
  oneof
    [ do
        chunkWidth <- chooseInt (-256, 0)
        pure
          ( FromIndicesArgs chunkWidth 1 (NEIntSet.singleton 0)
          , "SparseBitmap: chunk width must be positive"
          , "chunk width must be positive"
          )
    , do
        totalWidth <- chooseInt (-10000, 0)
        pure
          ( FromIndicesArgs 1 totalWidth (NEIntSet.singleton 0)
          , "SparseBitmap: total width must be positive"
          , "total width must be positive"
          )
    , do
        chunkWidth <- chooseInt (1, 256)
        index <- chooseInt (-10000, -1)
        pure
          ( FromIndicesArgs chunkWidth 1 (NEIntSet.singleton index)
          , "SparseBitmap: index " <> show index <> " must be non-negative"
          , "must be non-negative"
          )
    , do
        chunkWidth <- chooseInt (1, 256)
        totalWidth <- chooseInt (1, 10000)
        pure
          ( FromIndicesArgs chunkWidth totalWidth (NEIntSet.singleton totalWidth)
          , "SparseBitmap: index " <> show totalWidth <> " must be below the total width"
          , "must be below the total width"
          )
    ]

data DecodeCborArgs = DecodeCborArgs Int Int CBOR.Term deriving stock (Eq, Show)

lastChunkIndex :: Int -> Int -> Int
lastChunkIndex chunkWidth totalWidth = (totalWidth - 1) `div` chunkWidth

maxChunks :: Int -> Int -> Int
maxChunks chunkWidth totalWidth = lastChunkIndex chunkWidth totalWidth + 1

chunkWidthFor :: Int -> Int -> Int -> Int
chunkWidthFor chunkWidth totalWidth chunkIndex
  | chunkIndex < lastChunkIndex chunkWidth totalWidth = chunkWidth
  | otherwise = totalWidth - chunkIndex * chunkWidth

genInvalidDecodeCborArgs :: Gen (DecodeCborArgs, String, String)
genInvalidDecodeCborArgs =
  oneof
    [ invalidateChunkWidth <$> genValidDecodeCborArgs
    , invalidateTotalWidth <$> genValidDecodeCborArgs
    , emptyMap <$> genValidDecodeCborArgs
    , emptyMap <$> genValidDecodeCborArgs
    , tooManyEntries <$> genValidDecodeCborArgs
    , tooManyEntries <$> genValidDecodeCborArgs
    , negativeChunkIndex <$> genValidDecodeCborArgs
    , outOfRangeChunkIndex <$> genValidDecodeCborArgs
    , descendingChunkIndices
    , duplicateChunkIndices
    , malformedBitmap <$> genValidDecodeCborArgs
    , paddingBitsSet
    , zeroBitmap <$> genValidDecodeCborArgs
    ]
 where
  invalidateChunkWidth (DecodeCborArgs _ totalWidth cbor) =
    ( DecodeCborArgs 0 totalWidth cbor
    , "SparseBitmap: chunk width must be positive"
    , "chunk width must be positive"
    )

  invalidateTotalWidth (DecodeCborArgs chunkWidth _ cbor) =
    ( DecodeCborArgs chunkWidth 0 cbor
    , "SparseBitmap: total width must be positive"
    , "total width must be positive"
    )

  mustBeValidCbor cbor = case cbor of
    CBOR.TMap (e : es) -> ("definite map", e : es, CBOR.TMap)
    CBOR.TMapI (e : es) -> ("indefinite map", e : es, CBOR.TMapI)
    _ -> error "must be TMap or TMapI with non empty entries"

  emptyMap (DecodeCborArgs chunkWidth totalWidth cbor) =
    let
      (label, _entries, mkMap) = mustBeValidCbor cbor
     in
      ( DecodeCborArgs chunkWidth totalWidth (mkMap [])
      , "SizedNEIntMap: map must have at least one entry"
      , "empty " <> label
      )

  tooManyEntries (DecodeCborArgs chunkWidth totalWidth cbor) =
    let
      (label, entries, mkMap) = mustBeValidCbor cbor
      filledEntries = fillMissingEntries 0 entries
      extraEntry =
        ( CBOR.TInt (maxChunks chunkWidth totalWidth)
        , oneIndexChunkTerm (chunkWidthFor chunkWidth totalWidth 0)
        )
     in
      ( DecodeCborArgs chunkWidth totalWidth (mkMap (filledEntries <> [extraEntry]))
      , "SizedNEIntMap: map must have at most " <> show (maxChunks chunkWidth totalWidth) <> " entries"
      , "too many entries: " <> label
      )
   where
    lastChunk = lastChunkIndex chunkWidth totalWidth

    fillMissingEntries nextChunk []
      | nextChunk > lastChunk = []
      | otherwise = filler nextChunk : fillMissingEntries (nextChunk + 1) []
    fillMissingEntries nextChunk (entry@(CBOR.TInt chunkIndex, _) : rest)
      | nextChunk < chunkIndex =
          filler nextChunk : fillMissingEntries (nextChunk + 1) (entry : rest)
      | nextChunk == chunkIndex =
          entry : fillMissingEntries (nextChunk + 1) rest
      | otherwise =
          error "valid CBOR entries must be strictly ascending"
    fillMissingEntries _nextChunk _entries = error "invalid CBOR"

    filler chunkIndex =
      ( CBOR.TInt chunkIndex
      , oneIndexChunkTerm (chunkWidthFor chunkWidth totalWidth chunkIndex)
      )

  negativeChunkIndex (DecodeCborArgs chunkWidth totalWidth cbor) =
    case mustBeValidCbor cbor of
      (_label, (_, bitmap) : rest, mkMap) ->
        ( DecodeCborArgs chunkWidth totalWidth (mkMap ((CBOR.TInt (-1), bitmap) : rest))
        , "SparseBitmap: chunk index -1 must be non-negative"
        , "chunk index must be non-negative"
        )
      _ -> error "must be non empty"

  outOfRangeChunkIndex (DecodeCborArgs chunkWidth totalWidth cbor) =
    case mustBeValidCbor cbor of
      (_label, (_, bitmap) : rest, mkMap) ->
        ( DecodeCborArgs chunkWidth totalWidth (mkMap ((CBOR.TInt invalidChunkIndex, bitmap) : rest))
        , "SparseBitmap: chunk index "
            <> show invalidChunkIndex
            <> " must be less or equal to "
            <> show lastChunkIndex'
        , "chunk index must be in range"
        )
      _ -> error "must be non empty"
   where
    lastChunkIndex' = lastChunkIndex chunkWidth totalWidth
    invalidChunkIndex = lastChunkIndex' + 1

  duplicateChunkIndices = do
    chunkWidth <- chooseInt (1, 256)
    totalWidth <- chooseInt (chunkWidth + 1, 10000)
    indices <- listOf1 (chooseInt (0, totalWidth - 1))
    DecodeCborArgs _ _ cbor <- case NEIntSet.nonEmptySet (IntSet.fromList indices) of
      Nothing -> error "listOf1 produced no indices"
      Just nonEmptyIndices -> genValidDecodeCborArgsFromIndices chunkWidth totalWidth nonEmptyIndices

    case mustBeValidCbor cbor of
      (_label, entry : _entries, mkMap) ->
        pure
          ( DecodeCborArgs chunkWidth totalWidth (mkMap [entry, entry])
          , "SizedNEIntMap: keys are not strictly ascending"
          , "chunk indices must be ascending without duplicates"
          )
      _ -> error "must be non empty"

  descendingChunkIndices = do
    -- has 2 entries
    chunkWidth <- chooseInt (1, 256)
    totalWidth <- chooseInt (chunkWidth + 1, 10000)
    DecodeCborArgs _ _ cbor <-
      genValidDecodeCborArgsFromIndices
        chunkWidth
        totalWidth
        (NEIntSet.fromDistinctAscList $ 0 NEList.:| [chunkWidth])

    case mustBeValidCbor cbor of
      (_label, entries, mkMap) ->
        pure
          ( DecodeCborArgs chunkWidth totalWidth (mkMap (reverse entries))
          , "SizedNEIntMap: keys are not strictly ascending"
          , "chunk indices must be ascending"
          )

  malformedBitmap (DecodeCborArgs chunkWidth totalWidth cbor) =
    case mustBeValidCbor cbor of
      (_label, (CBOR.TInt chunkIndex, _bitmap) : rest, mkMap) ->
        let
          width = chunkWidthFor chunkWidth totalWidth chunkIndex
          expectedBytes = (width + 7) `div` 8
         in
          ( DecodeCborArgs chunkWidth totalWidth (mkMap ((CBOR.TInt chunkIndex, CBOR.TBytes BS.empty) : rest))
          , "NonEmptyBitmap: expected "
              <> show expectedBytes
              <> " bytes for "
              <> show width
              <> " bits, got 0"
          , "chunk bitmap must be well formed"
          )
      _ -> error "must be non empty"

  paddingBitsSet = do
    chunkWidth <- chooseInt (2, 256)
    fullChunks <- chooseInt (0, 10)
    chosenLastChunkWidth <- chooseInt (1, min 7 (chunkWidth - 1))
    let
      totalWidth = fullChunks * chunkWidth + chosenLastChunkWidth
      lastChunkIndex' = lastChunkIndex chunkWidth totalWidth
      actualLastChunkWidth = chunkWidthFor chunkWidth totalWidth lastChunkIndex'

    pure
      ( DecodeCborArgs chunkWidth totalWidth $
          CBOR.TMapI
            [
              ( CBOR.TInt lastChunkIndex'
              , CBOR.TBytes (BS.replicate ((actualLastChunkWidth + 7) `div` 8 - 1) 0 <> BS.singleton 1)
              )
            ]
      , "NonEmptyBitmap: padding bits after bit " <> show (actualLastChunkWidth - 1) <> " are not zero"
      , "final chunk padding bits must be clear"
      )

  zeroBitmap (DecodeCborArgs chunkWidth totalWidth cbor) =
    case mustBeValidCbor cbor of
      (_label, entries, mkMap) ->
        ( DecodeCborArgs chunkWidth totalWidth $
            mkMap
              [ (CBOR.TInt chunkIndex, zeroChunkTerm (chunkWidthFor chunkWidth totalWidth chunkIndex))
              | (CBOR.TInt chunkIndex, _bitmap) <- entries
              ]
        , "NonEmptyBitmap: no bit is set"
        , "chunk bitmap must be non-zero"
        )

genSparseBitmapFromIndices :: Gen SparseBitmap.SparseBitmap
genSparseBitmapFromIndices = do
  fromIndicesArgs <- genValidFromIndicesArgs
  pure $ fromIndicesOrError fromIndicesArgs

genSparseBitmapFromDecodeCbor :: Gen (String, SparseBitmap.SparseBitmap)
genSparseBitmapFromDecodeCbor = do
  DecodeCborArgs chunkWidth totalWidth cbor <- genValidDecodeCborArgs
  let bytes = CBOR.toLazyByteString $ CBOR.encodeTerm cbor
  case CBOR.deserialiseFromBytes (SparseBitmap.decodeCbor chunkWidth totalWidth CBOR.decodeInt) bytes of
    Left err ->
      error $
        "generated SparseBitmap CBOR must decode: " <> show err
    Right (rest, decodedSparseBitmap)
      | BSL.null rest -> pure (mapLabel cbor, decodedSparseBitmap)
      | otherwise -> error "SparseBitmap decoder left input after decoding generated CBOR"

genValidDecodeCborArgs :: Gen DecodeCborArgs
genValidDecodeCborArgs = do
  FromIndicesArgs chunkWidth totalWidth indices <- genValidFromIndicesArgs
  genValidDecodeCborArgsFromIndices chunkWidth totalWidth indices

genValidDecodeCborArgsFromIndices ::
  Int ->
  Int ->
  NEIntSet.NEIntSet ->
  Gen DecodeCborArgs
genValidDecodeCborArgsFromIndices chunkWidth totalWidth indices = do
  mkMap <- oneof [pure (CBOR.TMap . NEList.toList), pure (CBOR.TMapI . NEList.toList)]
  pure $ DecodeCborArgs chunkWidth totalWidth (mkMap cborEntries)
 where
  chunks =
    NEIntMap.mapWithKey (\chunkIndex -> mkBitmap (chunkWidthFor chunkWidth totalWidth chunkIndex)) $
      groupByChunk indices

  cborEntries = fmap encodeChunk $ NEIntMap.toAscList chunks

  groupByChunk =
    NEIntMap.fromListWith (<>)
      . fmap chunkPart
      . NEIntSet.toAscList

  chunkPart index =
    let (chunkIndex, indexInChunk) = index `divMod` chunkWidth
     in (chunkIndex, NEIntSet.singleton indexInChunk)

  mkBitmap width idxs =
    case NEBitmap.fromIndices width (fmap fromIntegral (NEIntSet.toAscList idxs) :: NonEmpty Word16) of
      Just bitmap -> bitmap
      Nothing -> error "genValidDecodeCborArgsFromIndices: generated an out-of-range index"

  encodeChunk (chunkIndex, bitmap) =
    ( CBOR.TInt chunkIndex
    , CBOR.TBytes
        . Sliced.toByteString
        . Sliced.fromByteArray
        $ NEBitmap.bitmapRawByteArray bitmap
    )

mapLabel :: CBOR.Term -> String
mapLabel cbor = case cbor of
  CBOR.TMap _ -> "definite map"
  CBOR.TMapI _ -> "indefinite map"
  _ -> error "valid CBOR must be a map"

-- | One chunk naming a single index, for filler entries.
oneIndexChunkTerm :: Int -> CBOR.Term
oneIndexChunkTerm width =
  case NEBitmap.fromIndices width (NEList.singleton 0 :: NonEmpty Word16) of
    Just bitmap -> CBOR.TBytes . Sliced.toByteString . Sliced.fromByteArray $ NEBitmap.bitmapRawByteArray bitmap
    Nothing -> error "oneIndexChunkTerm: index out of range"

-- | A chunk's all-zero bytes: deliberately invalid, since no real chunk names
-- nothing. 'NEBitmap.fromIndices' can't build this (it needs a non-empty
-- 'Foldable1'), so this constructs the bytes directly.
zeroChunkTerm :: Int -> CBOR.Term
zeroChunkTerm width = CBOR.TBytes $ BS.replicate ((width + 7) `div` 8) 0

fromIndicesOrError :: FromIndicesArgs -> SparseBitmap.SparseBitmap
fromIndicesOrError (FromIndicesArgs chunkWidth totalWidth indices) =
  case runExcept (SparseBitmap.fromIndices chunkWidth totalWidth indices) of
    Left err ->
      error $
        "positive chunk width, total width and non-empty indices must construct a SparseBitmap: " <> err
    Right sparseBitmap -> sparseBitmap
