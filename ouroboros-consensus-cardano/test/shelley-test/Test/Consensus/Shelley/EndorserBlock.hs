{-# LANGUAGE TypeApplications #-}

-- | The sizes the Dijkstra mempool charges for an endorser block agree with
-- the bytes 'encodeEndorserBlock' writes.
module Test.Consensus.Shelley.EndorserBlock (tests) where

import qualified Codec.CBOR.Encoding as CBOR
import Codec.CBOR.Write (toStrictByteString)
import qualified Data.ByteString as BS
import Data.Proxy (Proxy (..))
import qualified Data.Vector.Strict as V
import Ouroboros.Consensus.Ledger.SupportsMempool (ByteSize32 (..))
import Ouroboros.Consensus.Leios.EndorserBlock
  ( BytesSize
  , EndorserBlock (..)
  , EndorserBlockFormat (..)
  , TxHash (..)
  , referencesCapacity
  )
import Ouroboros.Consensus.Shelley.Eras (DijkstraEra)
import Ouroboros.Consensus.Shelley.Ledger.Mempool ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Dijkstra endorser block"
    [ testProperty "encodedReferenceSize consistent with encodeEndorserBlock" prop_referenceSizeConsistent
    , testProperty "encodeEndorserBlock framing bounded by encodedMaxFramingSize" prop_ebFramingBounded
    , testProperty
        "referencesCapacity floors at zero instead of wrapping"
        prop_referencesCapacityFloorsAtZero
    ]

dijkstra :: Proxy DijkstraEra
dijkstra = Proxy

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

-- | The number of references with good coverage of CBOR encoding boundaries for
-- the map length: 0-23 in 1 byte, 24-255 in 2 bytes, 256 and more in 3 bytes.
genNumReferences :: Gen Int
genNumReferences =
  frequency
    [ (1, pure 0)
    , (1, pure 1)
    , (1, pure 23)
    , (1, pure 24)
    , (1, pure 255)
    , (1, pure 256)
    , (3, chooseInt (0, 1000))
    ]

genEndorserBlock :: Int -> Gen EndorserBlock
genEndorserBlock numTxs =
  MkEndorserBlock . V.fromList <$> vectorOf numTxs ((,) <$> genTxHash <*> genTxBytesSize)

-- | The per-reference charge 'encodedReferenceSize', which the mempool charges a
-- transaction for its reference, agrees with the bytes 'encodeEndorserBlock' writes
-- for that reference.
prop_referenceSizeConsistent :: Property
prop_referenceSizeConsistent =
  forAll ((,) <$> genTxHash <*> genTxBytesSize) $ \(txHash@(MkTxHash bytes), txSize) ->
    let encoded = toStrictByteString $ CBOR.encodeBytes bytes <> CBOR.encodeWord32 txSize
        ByteSize32 estimatedSize = encodedReferenceSize dijkstra (ByteSize32 txSize)
     in counterexample
          ("reference: " <> show (txHash, txSize))
          (estimatedSize === fromIntegral (BS.length encoded))

-- | Whatever 'encodeEndorserBlock' writes around the references stays within
-- 'encodedMaxFramingSize', the one-off amount capacities subtract.
prop_ebFramingBounded :: Property
prop_ebFramingBounded =
  forAll (genNumReferences >>= genEndorserBlock) $ \eb ->
    let actualSize = fromIntegral $ BS.length (toStrictByteString (encodeEndorserBlock dijkstra eb))
        itemsSize =
          sum
            [ unByteSize32 (encodedReferenceSize dijkstra (ByteSize32 txSize))
            | (_txHash, txSize) <- V.toList (endorserBlockReferences eb)
            ]
        framing = actualSize - itemsSize
     in counterexample
          ("references: " <> show (V.length (endorserBlockReferences eb)) <> ", framing: " <> show framing)
          (property $ actualSize >= itemsSize && framing <= unByteSize32 (encodedMaxFramingSize dijkstra))

-- | A @maxEndorserBlockReferencesSize@ at or below the framing yields a zero
-- references capacity, so nothing fits. It never wraps around to \"no limit\",
-- which 'Data.Word.Word32' subtraction would do for a parameter of 0.
prop_referencesCapacityFloorsAtZero :: Property
prop_referencesCapacityFloorsAtZero =
  forAll genParamLimit $ \paramLimit ->
    let capacity = referencesCapacity dijkstra paramLimit
     in counterexample ("capacity: " <> show capacity) $
          conjoin
            [ counterexample "capacity must never exceed the parameter (wrap-around)" $
                property $
                  capacity <= paramLimit
            , counterexample "a parameter within the framing must yield zero capacity" $
                paramLimit > framing .||. capacity === 0
            ]
 where
  framing = unByteSize32 (encodedMaxFramingSize dijkstra)

  -- Weighted towards the boundary a plain subtraction wraps on.
  genParamLimit =
    frequency
      [ (4, chooseEnum (0, framing))
      , (1, chooseEnum (framing + 1, maxBound))
      ]
