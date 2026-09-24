-- | The sizes the mempool charges for an endorser block agree with the bytes
-- 'encodeLeiosEb' writes.
module Test.Consensus.Leios.EndorserBlock (tests) where

import Cardano.Binary (serialize')
import qualified Codec.CBOR.Encoding as CBOR
import qualified Data.ByteString as BS
import qualified Data.Vector.Strict as V
import Ouroboros.Consensus.Ledger.SupportsMempool (ByteSize32 (..))
import Ouroboros.Consensus.Leios.EndorserBlock
  ( BytesSize
  , LeiosEb (..)
  , TxHash (..)
  , encodeLeiosEb
  , encodeLeiosEbItemSize
  , encodeLeiosEbMaxFramingSize
  , leiosReferencesCapacity
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Leios endorser block"
    [ testProperty "encodeLeiosEbItemSize consistent with encodeLeiosEb" prop_ebItemSizeConsistent
    , testProperty "encodeLeiosEb framing bounded by encodeLeiosEbMaxFramingSize" prop_ebFramingBounded
    , testProperty
        "leiosReferencesCapacity floors at zero instead of wrapping"
        prop_referencesCapacityFloorsAtZero
    ]

genTxHash :: Gen TxHash
genTxHash = MkTxHash . BS.pack <$> vectorOf 32 arbitrary

-- | A tx size with good coverage of CBOR encoding boundaries.
-- Values 0-23 encode in 1 byte, 24-255 in 2 bytes, 256-65535 in 3 bytes.
genTxBytesSize :: Gen BytesSize
genTxBytesSize =
  frequency
    [ (1, pure 55) -- smallest transaction
    , (1, pure 16384) -- largest transaction
    , (1, pure 255) -- boundary: last 2-byte CBOR uint
    , (1, pure 256) -- boundary: first 3-byte CBOR uint
    , (1, pure 65536) -- first 5-byte CBOR uint
    , (5, chooseEnum (55, 16384))
    ]

-- | The number of items with good coverage of CBOR encoding boundaries for
-- the map length: 0-23 in 1 byte, 24-255 in 2 bytes, 256 and more in 3 bytes.
genNumItems :: Gen Int
genNumItems =
  frequency
    [ (1, pure 0)
    , (1, pure 1)
    , (1, pure 23)
    , (1, pure 24)
    , (1, pure 255)
    , (1, pure 256)
    , (3, chooseInt (0, 1000))
    ]

genEb :: Int -> Gen LeiosEb
genEb numTxs =
  MkLeiosEb . V.fromList <$> vectorOf numTxs ((,) <$> genTxHash <*> genTxBytesSize)

-- | The per-item charge 'encodeLeiosEbItemSize', which the mempool charges a
-- transaction for its reference, agrees with the bytes 'encodeLeiosEb' writes
-- for that item.
prop_ebItemSizeConsistent :: Property
prop_ebItemSizeConsistent =
  forAll ((,) <$> genTxHash <*> genTxBytesSize) $ \(txHash@(MkTxHash bytes), txSize) ->
    let encoded = serialize' $ CBOR.encodeBytes bytes <> CBOR.encodeWord32 txSize
        ByteSize32 estimatedSize = encodeLeiosEbItemSize (ByteSize32 txSize)
     in counterexample
          ("item: " <> show (txHash, txSize))
          (estimatedSize === fromIntegral (BS.length encoded))

-- | Whatever 'encodeLeiosEb' writes around the items stays within
-- 'encodeLeiosEbMaxFramingSize', the one-off amount capacities subtract.
prop_ebFramingBounded :: Property
prop_ebFramingBounded =
  forAll (genNumItems >>= genEb) $ \eb ->
    let actualSize = fromIntegral $ BS.length (serialize' (encodeLeiosEb eb))
        itemsSize =
          sum
            [ unByteSize32 (encodeLeiosEbItemSize (ByteSize32 txSize))
            | (_txHash, txSize) <- V.toList (leiosEbTxs eb)
            ]
        framing = actualSize - itemsSize
     in counterexample
          ("items: " <> show (V.length (leiosEbTxs eb)) <> ", framing: " <> show framing)
          (property $ actualSize >= itemsSize && framing <= unByteSize32 encodeLeiosEbMaxFramingSize)

-- | A @maxEndorserBlockReferencesSize@ at or below the framing yields a zero
-- references capacity, so nothing fits. It never wraps around to \"no limit\",
-- which 'Data.Word.Word32' subtraction would do for a parameter of 0.
prop_referencesCapacityFloorsAtZero :: Property
prop_referencesCapacityFloorsAtZero =
  forAll genParamLimit $ \paramLimit ->
    let capacity = leiosReferencesCapacity paramLimit
     in counterexample ("capacity: " <> show capacity) $
          conjoin
            [ counterexample "capacity must never exceed the parameter (wrap-around)" $
                property $
                  capacity <= paramLimit
            , counterexample "a parameter within the framing must yield zero capacity" $
                paramLimit > framing .||. capacity === 0
            ]
 where
  framing = unByteSize32 encodeLeiosEbMaxFramingSize

  -- Weighted towards the boundary a plain subtraction wraps on.
  genParamLimit =
    frequency
      [ (4, chooseEnum (0, framing))
      , (1, chooseEnum (framing + 1, maxBound))
      ]
