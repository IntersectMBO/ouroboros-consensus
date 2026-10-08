{-# LANGUAGE TypeApplications #-}

-- | The sizes that transactions take of an endorser block's capacity agree
-- with the bytes 'encodeLeiosEb' writes.
--
-- 'mkLeiosEb' references each Dijkstra transaction by the hash and the length
-- of its bytes, in the order of the transactions.
module Test.Consensus.Shelley.EndorserBlock (tests) where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Core (TopTx, Tx, toEraCBOR)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..))
import Cardano.Slotting.Time (mkSlotLength)
import qualified Codec.CBOR.Encoding as CBOR
import Codec.CBOR.Write (toStrictByteString)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Proxy (Proxy (..))
import qualified Data.Vector.Strict as V
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( ByteSize32 (..)
  , GenTx
  , Validated
  )
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , LeiosEb (..)
  , TxHash (..)
  , encodeLeiosEb
  , encodeLeiosEbItemSize
  , encodeLeiosEbMaxFramingSize
  , leiosReferencesCapacity
  )
import Ouroboros.Consensus.Shelley.HFEras (StandardDijkstraBlock)
import Ouroboros.Consensus.Shelley.Ledger (mkShelleyValidatedTx)
import Ouroboros.Consensus.Shelley.Node.Leios (mkLeiosEb)
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import qualified Test.Cardano.Ledger.Dijkstra.Examples as Dijkstra
import Test.Cardano.Ledger.Shelley.Examples
  ( LedgerExamples (..)
  , testShelleyGenesis
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Endorser block"
    [ testProperty "encodeLeiosEbItemSize consistent with encodeLeiosEb" prop_referenceSizeConsistent
    , testProperty "encodeLeiosEb framing bounded by encodeLeiosEbMaxFramingSize" prop_ebFramingBounded
    , testProperty
        "leiosReferencesCapacity floors at zero instead of wrapping"
        prop_referencesCapacityFloorsAtZero
    , testGroup
        "mkLeiosEb"
        [ testCase "no transactions give no endorser block" test_mkLeiosEbEmpty
        , testProperty "references keep the order of the transactions" prop_mkLeiosEbKeepsOrder
        , testProperty "reference hash is the hash of the transaction bytes" prop_mkLeiosEbHash
        , testProperty "reference size is the length of the transaction bytes" prop_mkLeiosEbSize
        ]
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

genLeiosEb :: Int -> Gen LeiosEb
genLeiosEb numTxs =
  MkLeiosEb . V.fromList <$> vectorOf numTxs ((,) <$> genTxHash <*> genTxBytesSize)

-- | 'encodeLeiosEbItemSize', the bytes a transaction's reference takes of the
-- endorser-block references capacity, agrees with the bytes 'encodeLeiosEb'
-- writes for that reference.
prop_referenceSizeConsistent :: Property
prop_referenceSizeConsistent =
  forAll ((,) <$> genTxHash <*> genTxBytesSize) $ \(txHash@(MkTxHash bytes), txSize) ->
    let encoded = toStrictByteString $ CBOR.encodeBytes bytes <> CBOR.encodeWord32 txSize
        ByteSize32 estimatedSize = encodeLeiosEbItemSize (ByteSize32 txSize)
     in counterexample
          ("reference: " <> show (txHash, txSize))
          (estimatedSize === fromIntegral (BS.length encoded))

-- | Whatever 'encodeLeiosEb' writes around the references stays within
-- 'encodeLeiosEbMaxFramingSize', the one-off amount capacities subtract.
prop_ebFramingBounded :: Property
prop_ebFramingBounded =
  forAll (genNumReferences >>= genLeiosEb) $ \eb ->
    let actualSize = fromIntegral $ BS.length (toStrictByteString (encodeLeiosEb eb))
        itemsSize =
          sum
            [ unByteSize32 (encodeLeiosEbItemSize (ByteSize32 txSize))
            | (_txHash, txSize) <- V.toList (leiosEbTxs eb)
            ]
        framing = actualSize - itemsSize
     in counterexample
          ("references: " <> show (V.length (leiosEbTxs eb)) <> ", framing: " <> show framing)
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

{-------------------------------------------------------------------------------
  Building an endorser block
-------------------------------------------------------------------------------}

type DijkstraTx = Tx TopTx DijkstraEra

-- | 'SL.unsafeMakeValidatedTx' skips the ledger rules, so the generated
-- transactions need not be valid. 'mkLeiosEb' reads only the transaction.
unsafeValidated :: DijkstraTx -> Validated (GenTx StandardDijkstraBlock)
unsafeValidated =
  mkShelleyValidatedTx
    . SL.unsafeMakeValidatedTx globals (SL.mkMempoolEnv nes 0) (SL.mkMempoolState nes)
 where
  nes = leNewEpochState Dijkstra.ledgerExamples
  globals =
    SL.mkShelleyGlobals
      testShelleyGenesis
      (fixedEpochInfo (EpochSize 10) (mkSlotLength 1))

-- | The references in the endorser block that 'mkLeiosEb' builds.
references :: [DijkstraTx] -> [(TxHash, BytesSize)]
references = maybe [] (V.toList . leiosEbTxs) . mkLeiosEb . map unsafeValidated

-- | The full serialised bytes of a transaction, from the ledger encoder.
txBytes :: DijkstraTx -> ByteString
txBytes = toStrictByteString . toEraCBOR @DijkstraEra

-- | Arbitrary Dijkstra transactions are slow to generate at the default
-- size, so the generator caps the size.
genTx :: Gen DijkstraTx
genTx = resize 3 arbitrary

-- | At least two transactions, so a wrong order changes the references.
genTxs :: Gen [DijkstraTx]
genTxs = chooseInt (2, 5) >>= flip vectorOf genTx

test_mkLeiosEbEmpty :: Assertion
test_mkLeiosEbEmpty =
  mkLeiosEb ([] :: [Validated (GenTx StandardDijkstraBlock)]) @?= Nothing

-- | The endorser block of a list holds, in the order of the list, the
-- reference that each transaction gets on its own.
prop_mkLeiosEbKeepsOrder :: Property
prop_mkLeiosEbKeepsOrder =
  forAll genTxs $ \txs ->
    references txs === concatMap (references . pure) txs

-- | 'Hash.digest' computes the expected hash, so the property does not repeat
-- the code under test.
prop_mkLeiosEbHash :: Property
prop_mkLeiosEbHash =
  forAll genTx $ \tx ->
    map fst (references [tx])
      === [MkTxHash (Hash.digest (Proxy @Hash.Blake2b_256) (txBytes tx))]

prop_mkLeiosEbSize :: Property
prop_mkLeiosEbSize =
  forAll genTx $ \tx ->
    map snd (references [tx]) === [fromIntegral (BS.length (txBytes tx))]
