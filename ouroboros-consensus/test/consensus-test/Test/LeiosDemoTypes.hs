{-# LANGUAGE TypeApplications #-}

module Test.LeiosDemoTypes (tests) where

import Cardano.Binary (serialize')
import Cardano.Crypto.DSIGN
  ( DSIGNAlgorithm (deriveVerKeyDSIGN)
  , genKeyDSIGN
  , seedSizeDSIGN
  )
import qualified Data.ByteString as BS
import Data.Data (Proxy (..))
import Data.List (sort)
import qualified Data.Vector.Strict as V
import LeiosDemoTypes
  ( BytesSize
  , LeiosCommittee (..)
  , LeiosDSIGN
  , LeiosEb (..)
  , LeiosSigningKey
  , LeiosVoter (..)
  , TxHash (..)
  , encodeLeiosEb
  , leiosEbBytesSize
  , maxTxsPerEb
  , mkCommitteeEveryoneVotes
  )
import Test.Crypto.Util (arbitrarySeedOfSize)
import Test.QuickCheck
  ( Gen
  , Property
  , chooseInt
  , counterexample
  , forAll
  , frequency
  , vectorOf
  , (.&&.)
  , (===)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "LeiosDemoTypes"
    [ testProperty "leiosEbBytesSize consistent with encodeLeiosEb" prop_ebBytesSizeConsistent
    , testProperty "mkCommitteeEveryoneVotes normalizes and sorts" prop_committeeNormalizedAndSorted
    ]

-- | Minimum tx size as per the ASSUMPTION in 'leiosEbBytesSize'.
minTxBytesSize :: Int
minTxBytesSize = 55

-- | Maximum tx size as per the ASSUMPTION in 'leiosEbBytesSize'.
maxTxBytesSize :: Int
maxTxBytesSize = 2 ^ (14 :: Int)

-- | Generate a random TxHash (32 random bytes).
genTxHash :: Gen TxHash
genTxHash = MkTxHash . BS.pack <$> vectorOf 32 (fromIntegral <$> chooseInt (0, 255))

-- | Generate a tx size with good coverage of CBOR encoding boundaries.
-- Values 0-23 encode in 1 byte, 24-255 in 2 bytes, 256-65535 in 3 bytes.
genTxBytesSize :: Gen BytesSize
genTxBytesSize =
  frequency
    [ (1, pure $ fromIntegral minTxBytesSize) -- lower bound
    , (1, pure $ fromIntegral maxTxBytesSize) -- upper bound
    , (1, pure 255) -- boundary: last 2-byte CBOR uint
    , (1, pure 256) -- boundary: first 3-byte CBOR uint
    , (6, fromIntegral <$> chooseInt (minTxBytesSize, maxTxBytesSize)) -- uniform
    ]

-- | Generate the number of items with good coverage of CBOR encoding
-- boundaries for the map length (0-23 → 1 byte, 24-255 → 2 bytes,
-- 256+ → 3 bytes) and the extremes.
genNumItems :: Gen Int
genNumItems =
  frequency
    [ (1, pure 0) -- empty EB
    , (1, pure 1) -- singleton
    , (1, pure 23) -- boundary: last 1-byte CBOR map length
    , (1, pure 24) -- boundary: first 2-byte CBOR map length
    , (1, pure 255) -- boundary: last 2-byte CBOR map length
    , (1, pure 256) -- boundary: first 3-byte CBOR map length
    , (1, pure maxTxsPerEb) -- upper bound
    , (3, chooseInt (0, maxTxsPerEb)) -- uniform
    ]

-- | Generate a LeiosEb with the given number of transactions.
genEb :: Int -> Gen LeiosEb
genEb numTxs = do
  txs <- vectorOf numTxs genTxItem
  pure $ MkLeiosEb $ V.fromList txs
 where
  genTxItem = (,) <$> genTxHash <*> genTxBytesSize

-- | The analytical 'leiosEbBytesSize' must agree with the actual length of
-- the CBOR encoding produced by 'encodeLeiosEb'.
prop_ebBytesSizeConsistent :: Property
prop_ebBytesSizeConsistent =
  forAll (genNumItems >>= genEb) $ \eb ->
    let encoded = serialize' $ encodeLeiosEb eb
        actualSize = fromIntegral (BS.length encoded) :: BytesSize
        estimatedSize = leiosEbBytesSize eb
     in counterexample
          ("items: " <> show (V.length (leiosEbTxs eb)))
          (estimatedSize === actualSize)

genLeiosSigningKey :: Gen LeiosSigningKey
genLeiosSigningKey = do
  seed <- arbitrarySeedOfSize (seedSizeDSIGN (Proxy @LeiosDSIGN))
  pure $ genKeyDSIGN seed

-- | 'mkCommitteeEveryoneVotes' must produce weights that sum to 1 and are
-- sorted ascending (so 'LeiosVoterId' assignment by index is stable). Inputs are
-- generated with distinct verification keys, since dedup-by-key is a separate
-- concern not exercised here.
prop_committeeNormalizedAndSorted :: Property
prop_committeeNormalizedAndSorted =
  forAll (chooseInt (1, 20)) $ \n ->
    forAll (vectorOf n genLeiosSigningKey) $ \sks ->
      forAll (vectorOf n (chooseInt (1, 1000))) $ \ws ->
        let inputs = zip (deriveVerKeyDSIGN <$> sks) ws
            committee = mkCommitteeEveryoneVotes inputs
            weights = voterWeight <$> V.toList (committeeVoters committee)
         in counterexample ("committee: " <> show committee) $
              counterexample "weights sum to 1" (sum weights === 1)
                .&&. counterexample "weights sorted ascending" (weights === sort weights)
                .&&. counterexample "preserves cardinality" (length weights === n)
