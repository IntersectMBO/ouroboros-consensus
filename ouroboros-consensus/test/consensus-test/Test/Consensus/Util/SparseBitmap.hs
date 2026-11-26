-- | Property-based tests for 'SparseBitmap'.
module Test.Consensus.Util.SparseBitmap (tests) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntSet as IntSet
import Data.IntSet.NonEmpty (NEIntSet)
import qualified Data.IntSet.NonEmpty as NEIntSet
import Data.Word (Word8)
import Ouroboros.Consensus.Util.SparseBitmap (SparseBitmap)
import qualified Ouroboros.Consensus.Util.SparseBitmap as SparseBitmap
import Test.QuickCheck (Gen, chooseInt, listOf1)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (Property, forAll, testProperty, (===))

tests :: TestTree
tests =
  testGroup
    "SparseBitmap"
    [ testProperty "toIndices inverts fromIndices" prop_roundtripIndices
    , testProperty "decodeCbor inverts encodeCbor" prop_roundtripCbor
    , testCase "a chunk takes only the bytes up to its last index" $
        bytesOf [0] @?= BS.pack [0xbf, 0x00, 0x41, 0x80, 0xff]
    , testGroup
        "decodeCbor rejects"
        [ testCase "an empty map" $ decodes [] @?= False
        , testCase "a negative chunk index" $ decodes [(-1, [0x80])] @?= False
        , testCase "a duplicate chunk index" $ decodes [(0, [0x80]), (0, [0x40])] @?= False
        , testCase "descending chunk indices" $ decodes [(1, [0x80]), (0, [0x80])] @?= False
        , testCase "a zero chunk" $ decodes [(0, [0x00])] @?= False
        , testCase "a chunk of more than 8 bytes" $ decodes [(0, replicate 8 0 <> [0x01])] @?= False
        ]
    , testCase "decodeCbor accepts a chunk with trailing zero bytes" $
        decodes [(0, [0x80, 0x00])] @?= True
    ]

prop_roundtripIndices :: Property
prop_roundtripIndices =
  forAll genIndices $ \indices ->
    SparseBitmap.toIndices (SparseBitmap.fromIndices indices) === indices

prop_roundtripCbor :: Property
prop_roundtripCbor =
  forAll genIndices $ \indices ->
    let sparseBitmap = SparseBitmap.fromIndices indices
     in decode (CBOR.toStrictByteString (encode sparseBitmap)) === Just sparseBitmap

-- * Generators

genIndices :: Gen NEIntSet
genIndices = NEIntSet.unsafeFromSet . IntSet.fromList <$> listOf1 (chooseInt (0, 20000))

-- * Helpers

encode :: SparseBitmap -> CBOR.Encoding
encode = SparseBitmap.encodeCbor CBOR.encodeInt

decode :: BS.ByteString -> Maybe SparseBitmap
decode bytes =
  case CBOR.deserialiseFromBytes (SparseBitmap.decodeCbor CBOR.decodeInt) (BSL.fromStrict bytes) of
    Right (rest, sparseBitmap) | BSL.null rest -> Just sparseBitmap
    _ -> Nothing

-- | The encoding of the bitmap naming these indices.
bytesOf :: [Int] -> BS.ByteString
bytesOf indices =
  CBOR.toStrictByteString $
    encode $
      SparseBitmap.fromIndices (NEIntSet.unsafeFromSet (IntSet.fromList indices))

-- | Whether a definite map of these chunk entries decodes.
decodes :: [(Int, [Word8])] -> Bool
decodes entries =
  maybe False (const True) $
    decode $
      CBOR.toStrictByteString $
        CBOR.encodeMapLen (fromIntegral (length entries))
          <> foldMap (\(i, bytes) -> CBOR.encodeInt i <> CBOR.encodeBytes (BS.pack bytes)) entries
