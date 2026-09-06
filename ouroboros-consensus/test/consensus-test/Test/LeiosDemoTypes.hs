module Test.LeiosDemoTypes (tests) where

import Cardano.Binary (serialize')
import qualified Data.ByteString as BS
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List ((\\))
import Data.Ratio ((%))
import qualified Data.Vector.Strict as V
import LeiosDemoTypes
  ( BytesSize
  , LeiosEb (..)
  , TxHash (..)
  , encodeLeiosEb
  , leiosEbBytesSize
  , maxTxsPerEb
  , selectCommitteeByStake
  )
import Test.QuickCheck
  ( Gen
  , Property
  , checkCoverage
  , chooseInt
  , chooseInteger
  , conjoin
  , counterexample
  , cover
  , forAll
  , forAllShrink
  , frequency
  , genericShrink
  , listOf
  , property
  , shrinkIntegral
  , vectorOf
  , (===)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "LeiosDemoTypes"
    [ testProperty "leiosEbBytesSize consistent with encodeLeiosEb" prop_ebBytesSizeConsistent
    , testProperty
        "selectCommitteeByStake orders by stake and bounds by committee size"
        prop_selectCommitteeByStake
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

-- | 'selectCommitteeByStake' seats the highest-stake pools, bounded by the
-- committee-size parameter rather than by cumulative stake.
--
-- Every property below inspects weights only, so none of them pins which of two
-- equal-stake pools is seated, or at which index.
--
-- TODO: add a tie-break property asserting that equal-stake entries come out in
-- ascending key order, as CIP-164 requires; seat indices are what @voter_id@ and
-- the certificate bitfield are positional over, so disagreement is a chain split.
prop_selectCommitteeByStake :: Property
prop_selectCommitteeByStake =
  forAllShrink (listOf genWeight) genericShrink $ \rawStakes ->
    forAllShrink genCommitteeSize shrinkIntegral $ \targetSize ->
      let weights = snd <$> selectCommitteeByStake targetSize (zip [0 :: Int ..] rawStakes)
          sizeBinds = length rawStakes > fromIntegral targetSize
       in conjoin
            [ seatsExactlySize targetSize rawStakes weights
            , isDescending weights
            , selectsTopStake rawStakes weights
            ]
            & counterexample ("targetSize: " <> show targetSize <> ", weights: " <> show weights)
            & cover 0.1 sizeBinds "size bound binds"
            & cover 0.1 (not sizeBinds) "every pool seated"
            & cover 0.1 (targetSize == 0) "empty committee"
            & checkCoverage
 where
  genWeight = chooseInteger (1, 100) <&> (% 100)

  -- Deliberately overlaps the stake-list length so both regimes are generated:
  -- the bound biting, and every pool fitting.
  genCommitteeSize = fromIntegral <$> chooseInt (0, 12)

  forAllIndices xs f
    | null xs = property True
    | otherwise = forAllShrink (chooseInt (0, length xs - 1)) shrinkIntegral f

  -- The committee is exactly as large as the parameter allows: the whole pool
  -- set when it fits, the parameter when it does not, and empty at zero.
  seatsExactlySize targetSize rawStakes weights =
    length weights === min (fromIntegral targetSize) (length rawStakes)
      & counterexample "committee size does not match the parameter"

  -- Descending order: any prefix outweighs the rest.
  isDescending weights =
    forAllIndices weights $ \i ->
      let (prefix, rest) = splitAt i weights
       in null prefix || null rest || minimum prefix >= maximum rest
            & counterexample ("weights not monotonically decreasing at " <> show i)

  -- Top-stake: no excluded pool outweighs a selected one.
  selectsTopStake rawStakes weights =
    let excluded = rawStakes \\ weights
     in null weights || null excluded || minimum weights >= maximum excluded
          & counterexample ("an excluded pool outweighs a selected one: " <> show excluded)
