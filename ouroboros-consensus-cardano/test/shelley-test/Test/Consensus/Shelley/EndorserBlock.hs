-- | The sizes the mempool charges for an endorser block agree with
-- the bytes 'encodeEndorserBlock' writes.
module Test.Consensus.Shelley.EndorserBlock (tests) where

import Cardano.Binary (ToCBOR (..))
import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Ledger.Core (TopTx, Tx)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Hashes (extractHash, hashAnnotated)
import qualified Codec.CBOR.Encoding as CBOR
import Codec.CBOR.Write (toStrictByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Ouroboros.Consensus.Ledger.SupportsMempool (ByteSize32 (..))
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EndorserBlock
  , TxHash
  , encodeEndorserBlock
  , endorserBlockFromTxBytes
  , endorserBlockReferences
  , mkEndorserBlock
  , referencesCapacity
  , sizeEndorserBlockReference
  )
import Ouroboros.Consensus.Shelley.Eras (ShelleyBasedEra (ebTxBytes))
import Ouroboros.Consensus.Shelley.Ledger.Mempool (txByteSizeWithOverhead)
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Endorser block"
    [ testProperty
        "sizeEndorserBlockReference consistent with encodeEndorserBlock"
        prop_referenceSizeConsistent
    , testProperty
        "encodeEndorserBlock framing bounded by what referencesCapacity subtracts"
        prop_ebFramingBounded
    , testProperty
        "referencesCapacity floors at zero instead of wrapping"
        prop_referencesCapacityFloorsAtZero
    , testProperty
        "the tx size a forged endorser block records does not exceed the tx size the mempool records"
        -- Few runs: the generator of Dijkstra transactions is slow.
        (withNumTests 10 prop_dijkstraEbTxSizeBounded)
    , testProperty
        "a forged endorser block references a Dijkstra transaction by its hashAnnotated"
        (withNumTests 10 prop_dijkstraEbTxHashIsLedgerHash)
    ]

genTxHash :: Gen TxHash
genTxHash = UnsafeHash . SBS.pack <$> vectorOf 32 arbitrary

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
  mkEndorserBlock <$> vectorOf numTxs ((,) <$> genTxHash <*> genTxBytesSize)

-- | The per-reference charge 'sizeEndorserBlockReference', which the mempool charges a
-- transaction for its reference, agrees with the bytes 'encodeEndorserBlock' writes
-- for that reference.
prop_referenceSizeConsistent :: Property
prop_referenceSizeConsistent =
  forAll ((,) <$> genTxHash <*> genTxBytesSize) $ \(txHash, txSize) ->
    let encoded = toStrictByteString $ toCBOR txHash <> CBOR.encodeWord32 txSize
        ByteSize32 estimatedSize = sizeEndorserBlockReference (ByteSize32 txSize)
     in counterexample
          ("reference: " <> show (txHash, txSize))
          (estimatedSize === fromIntegral (BS.length encoded))

-- | Whatever 'encodeEndorserBlock' writes around the references stays within
-- 'maxFraming'.
prop_ebFramingBounded :: Property
prop_ebFramingBounded =
  forAll (genNumReferences >>= genEndorserBlock) $ \eb ->
    let actualSize = fromIntegral $ BS.length (toStrictByteString (encodeEndorserBlock eb))
        itemsSize =
          sum
            [ unByteSize32 (sizeEndorserBlockReference (ByteSize32 txSize))
            | (_txHash, txSize) <- endorserBlockReferences eb
            ]
        framing = actualSize - itemsSize
     in counterexample
          ("references: " <> show (length (endorserBlockReferences eb)) <> ", framing: " <> show framing)
          (property $ actualSize >= itemsSize && framing <= maxFraming)

-- | The framing 'referencesCapacity' subtracts from a parameter. That amount is
-- monotonic in the parameter, so 'maxBound' gives the most.
maxFraming :: BytesSize
maxFraming = capacity - referencesCapacity capacity
 where
  capacity = maxBound

-- | A @maxEndorserBlockReferencesSize@ at or below the framing yields a zero
-- references capacity, so nothing fits. It never wraps around to \"no limit\",
-- which 'Data.Word.Word32' subtraction would do for a parameter of 0.
prop_referencesCapacityFloorsAtZero :: Property
prop_referencesCapacityFloorsAtZero =
  forAll genParamLimit $ \paramLimit ->
    let capacity = referencesCapacity paramLimit
     in counterexample ("capacity: " <> show capacity) $
          conjoin
            [ counterexample "capacity must never exceed the parameter (wrap-around)" $
                property $
                  capacity <= paramLimit
            , counterexample "a parameter within the framing must yield zero capacity" $
                paramLimit > maxFraming .||. capacity === 0
            ]
 where
  -- Weighted towards the boundary a plain subtraction wraps on.
  genParamLimit =
    frequency
      [ (4, chooseEnum (0, maxFraming))
      , (1, chooseEnum (maxFraming + 1, maxBound))
      ]

-- | The length of the 'ebTxBytes' of a Dijkstra transaction never exceeds
-- its 'txByteSizeWithOverhead', which 'txEbMeasureDijkstra' reserves for the
-- reference.
--
-- The ledger computes 'sizeTxF' from one encoding and 'ebTxBytes' uses
-- another, so a ledger change can make the two disagree.
prop_dijkstraEbTxSizeBounded :: Tx TopTx DijkstraEra -> Property
prop_dijkstraEbTxSizeBounded tx =
  counterexample ("EB tx size: " <> show ebTxSize <> ", mempool tx size: " <> show mempoolTxSize) $
    ebTxSize <= mempoolTxSize
 where
  ebTxSize = BS.length (ebTxBytes tx)
  mempoolTxSize = fromIntegral (txByteSizeWithOverhead tx)

-- | 'endorserBlockFromTxBytes' references a transaction by the hash of its
-- 'ebTxBytes'. The Dijkstra ledger defines this reference as the
-- transaction's 'hashAnnotated', which is the hash of its 'originalBytes'.
-- Thus a node can check fetched bytes against the reference without decoding
-- them. Fails if 'ebTxBytes' returns other bytes, for example the 'EncCBOR'
-- encoding, which drops the @is_valid@ flag.
prop_dijkstraEbTxHashIsLedgerHash :: Tx TopTx DijkstraEra -> Property
prop_dijkstraEbTxHashIsLedgerHash tx =
  map fst (endorserBlockReferences (endorserBlockFromTxBytes (pure (ebTxBytes tx))))
    === [extractHash (hashAnnotated tx)]
