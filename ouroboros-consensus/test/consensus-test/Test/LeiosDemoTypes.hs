{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.LeiosDemoTypes (tests) where

import Cardano.Binary (serialize')
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Codec.CBOR.Encoding as CBOR
import Codec.CBOR.Read (DeserialiseFailure (..), deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Data.Bits (bit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (isInfixOf, (\\))
import qualified Data.List as List
import Data.Ratio ((%))
import qualified Data.Vector.Strict as V
import Data.Word (Word16, Word64)
import LeiosDemoOnlyTestFetch
  ( LeiosFetch
  , SingLeiosFetch (..)
  , codecLeiosFetch
  , decodeBitmaps
  , encodeBitmaps
  )
import LeiosDemoTypes
  ( BytesSize
  , LeiosEb (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , TxHash (..)
  , decodeEbHash
  , decodeLeiosEb
  , decodeLeiosPoint
  , decodeLeiosTx
  , decodeRbHash
  , encodeLeiosEb
  , encodeLeiosEbItemSize
  , encodeLeiosEbMaxFramingSize
  , encodeLeiosEbSize
  , encodeLeiosPoint
  , encodeLeiosTx
  , leiosReferencesCapacity
  , maxTxsPerEb
  , selectCommitteeByStake
  , txHashBytes
  )
import Network.TypedProtocol.Codec (ActiveState, CodecF (..), StateToken, runDecoder)
import Ouroboros.Consensus.Ledger.SupportsMempool (ByteSize32 (..))
import Test.QuickCheck
  ( Gen
  , Property
  , checkCoverage
  , chooseEnum
  , chooseInt
  , chooseInteger
  , conjoin
  , counterexample
  , cover
  , forAll
  , forAllShrink
  , frequency
  , genericShrink
  , ioProperty
  , listOf
  , once
  , property
  , shrinkIntegral
  , shrinkList
  , shuffle
  , vectorOf
  , (.||.)
  , (===)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.LeiosHash (unsafeEbHashFromBytes, unsafeTxHashFromBytes)

tests :: TestTree
tests =
  testGroup
    "LeiosDemoTypes"
    [ testProperty "encodeLeiosEbSize consistent with encodeLeiosEb" prop_ebBytesSizeConsistent
    , testProperty "encodeLeiosEbItemSize consistent with encodeLeiosEb" prop_ebItemSizeConsistent
    , testProperty "encodeLeiosEb framing bounded by encodeLeiosEbMaxFramingSize" prop_ebFramingBounded
    , testProperty
        "leiosReferencesCapacity floors at zero instead of wrapping"
        prop_referencesCapacityFloorsAtZero
    , testProperty
        "selectCommitteeByStake orders by stake and bounds by committee size"
        prop_selectCommitteeByStake
    , testProperty "decoders reject hashes that are not 32 bytes" prop_decodersRejectWrongHashLength
    , testProperty
        "decodeLeiosEb rejects item counts outside [1, maxTxsPerEb]"
        prop_decodeLeiosEbBoundsItemCount
    , testProperty
        "MsgLeiosBlockTxs decoder rejects a wrong or too large tx count"
        prop_decodeBlockTxsChecksCount
    , testProperty
        "decodeBitmaps accepts exactly the valid entry lists"
        prop_decodeBitmapsAcceptsExactlyValid
    , testCase
        "decodeBitmaps covers every offset of a full EB, and nothing past them"
        test_decodeBitmapsBoundaries
    ]

-- | Minimum tx size as per the ASSUMPTION in 'encodeLeiosEbSize'.
minTxBytesSize :: Int
minTxBytesSize = 55

-- | Maximum tx size as per the ASSUMPTION in 'encodeLeiosEbSize'.
maxTxBytesSize :: Int
maxTxBytesSize = 2 ^ (14 :: Int)

-- | Generate a random TxHash (32 random bytes).
genTxHash :: Gen TxHash
genTxHash = unsafeTxHashFromBytes . BS.pack <$> vectorOf 32 (fromIntegral <$> chooseInt (0, 255))

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

-- | The analytical 'encodeLeiosEbSize' must agree with the actual length of
-- the CBOR encoding produced by 'encodeLeiosEb'.
prop_ebBytesSizeConsistent :: Property
prop_ebBytesSizeConsistent =
  forAll (genNumItems >>= genEb) $ \eb ->
    let encoded = serialize' $ encodeLeiosEb eb
        actualSize = fromIntegral (BS.length encoded) :: BytesSize
        estimatedSize = encodeLeiosEbSize eb
     in counterexample
          ("items: " <> show (V.length (leiosEbTxs eb)))
          (estimatedSize === actualSize)

-- | The per-item charge 'encodeLeiosEbItemSize' (what the mempool measures a
-- transaction's reference at) must agree with the bytes 'encodeLeiosEb'
-- actually writes for that item.
prop_ebItemSizeConsistent :: Property
prop_ebItemSizeConsistent =
  forAll ((,) <$> genTxHash <*> genTxBytesSize) $ \(txHash, txSize) ->
    let encoded = serialize' $ CBOR.encodeBytes (txHashBytes txHash) <> CBOR.encodeWord32 txSize
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
          (property $ framing <= unByteSize32 encodeLeiosEbMaxFramingSize)

-- | The boundary #2291 fixed: a @maxEndorserBlockReferencesSize@ at or below
-- the framing must yield a zero references capacity -- nothing fits, so no
-- endorser blocks are forged -- and never wrap around to \"no limit\", which
-- is what reading the parameter back through 'Data.Word.Word32' subtraction
-- used to do for a parameter of 0.
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

  -- Weighted towards the boundary the old code wrapped on.
  genParamLimit =
    frequency
      [ (4, chooseEnum (0, framing))
      , (1, chooseEnum (framing + 1, maxBound))
      ]

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

-- | A peer controls the length of every hash it sends. 'decodeLeiosEb' and
-- 'decodeEbHash' must accept exactly 32 bytes and reject every other length.
prop_decodersRejectWrongHashLength :: Property
prop_decodersRejectWrongHashLength =
  once $
    conjoin
      [ counterexample ("hash length " <> show len) $
          conjoin
            [ counterexample "decodeLeiosEb" $
                accepts (deserialiseFromBytes decodeLeiosEb (bytes ebWithHashOfLength)) === (len == 32)
            , counterexample "decodeEbHash" $
                accepts (deserialiseFromBytes decodeEbHash (bytes hashOfLength)) === (len == 32)
            , counterexample "decodeRbHash" $
                accepts (deserialiseFromBytes decodeRbHash (bytes hashOfLength)) === (len == 32)
            ]
      | len <- [0, 1, 31, 32, 33, 64, 100000]
      , let hashBytes = BS.replicate len 0xab
            hashOfLength = CBOR.encodeBytes hashBytes
            ebWithHashOfLength =
              CBOR.encodeMapLen 1 <> CBOR.encodeBytes hashBytes <> CBOR.encodeWord32 100
      ]
 where
  bytes = BSL.fromStrict . serialize'
  -- Accepted means decoded with no bytes left over.
  accepts = either (const False) (BSL.null . fst)

-- | A peer controls the item count that an EB declares. 'decodeLeiosEb' must
-- accept counts from 1 to 'maxTxsPerEb' and reject 0 and anything larger.
prop_decodeLeiosEbBoundsItemCount :: Property
prop_decodeLeiosEbBoundsItemCount =
  once $
    conjoin
      [ counterexample ("item count " <> show count) $
          accepts (deserialiseFromBytes decodeLeiosEb (bytes (encodeLeiosEb (ebOfCount count))))
            === (count >= 1 && count <= maxTxsPerEb)
      | count <- [0, 1, maxTxsPerEb, maxTxsPerEb + 1]
      ]
 where
  ebOfCount count = MkLeiosEb $ V.replicate count (txHash, 100)
  txHash = unsafeTxHashFromBytes $ BS.replicate 32 0xab
  bytes = BSL.fromStrict . serialize'
  -- Accepted means decoded with no bytes left over.
  accepts = either (const False) (BSL.null . fst)

-- | The reply carries its own bitmaps, so the decoder can check the tx count
-- against them before it decodes any tx.
prop_decodeBlockTxsChecksCount :: Property
prop_decodeBlockTxsChecksCount =
  once $
    ioProperty $
      conjoin
        <$> sequence
          [ check "matching count" Nothing (firstTxs 1) 1 1
          , check "one tx too many" (Just "does not match") (firstTxs 1) 2 2
          , check "one tx too few" (Just "does not match") (firstTxs 2) 1 1
          , -- No tx follows the header.
            check "huge declared count" (Just "exceeds") (firstTxs 1) (2 ^ (40 :: Int)) 0
          , -- The bitmaps set bits past the last offset an EB can have, so
            -- they request more txs than an EB can hold. The count matches.
            check "count over maxTxsPerEb" (Just "exceeds") overLimit overLimitCount overLimitCount
          ]
 where
  -- The bitmaps request the first @n@ txs.
  firstTxs n = [(0, sum [bit (63 - i) | i <- [0 .. n - 1]])]
  overLimit = [(fromIntegral i, maxBound) | i <- [0 .. (maxTxsPerEb + 63) `div` 64 - 1]]
  overLimitCount = 64 * length overLimit
  -- The reply declares @declared@ txs and carries @nTxs@ of them.
  check :: String -> Maybe String -> [(Word16, Word64)] -> Int -> Int -> IO Property
  check label expected bitmaps declared nTxs = do
    let msg =
          CBOR.encodeListLen 4
            <> CBOR.encodeWord 4 -- MsgLeiosBlockTxs
            <> encodeLeiosPoint testPoint
            <> encodeBitmapEntries bitmaps
            <> CBOR.encodeListLen (fromIntegral declared)
            <> mconcat (replicate nTxs (encodeLeiosTx (MkLeiosTx BS.empty)))
    failure <- decodeFailure SingBlockTxs msg
    pure $ counterexample label $ failsWith expected failure

-- | 'decodeBitmaps' accepts exactly the entry lists the protocol allows.
--
-- The expectation is stated in the terms the protocol cares about -- which tx
-- offsets a peer may ask for -- and derived from 'maxTxsPerEb' and the wire's
-- 64-offsets-per-entry alone. It deliberately does /not/ reuse (or restate)
-- the decoder's entry cap: a test that shares that arithmetic moves with it,
-- and so can never catch it being wrong.
--
-- Note there is no separate count bound here. Strictly ascending indices that
-- each address a real offset are already at most as many as there are entries,
-- so the decoder's count check is an early guard, not a further rule.
prop_decodeBitmapsAcceptsExactlyValid :: Property
prop_decodeBitmapsAcceptsExactlyValid =
  forAllShrink genEntries shrinkEntries $ \entries ->
    let wellFormed =
          all ((/= 0) . snd) entries
            && all (addressesARealOffset . fst) entries
            && strictlyAscending (map fst entries)
     in -- A generator that drifted to only-valid (or only-invalid) lists would
        -- still pass the equality below, so require both sides to show up.
        checkCoverage $
          cover 15 wellFormed "accepted" $
            cover 15 (not wellFormed) "rejected" $
              counterexample ("entries " <> show entries) $
                counterexample ("expected " <> show wellFormed) $
                  accepts entries === wellFormed
 where
  -- Entry @i@ covers offsets @[64i .. 64i+63]@, and an EB holds at most
  -- 'maxTxsPerEb' txs, so an entry is meaningful iff its first offset exists.
  addressesARealOffset i = 64 * fromIntegral i < maxTxsPerEb
  strictlyAscending xs = and (zipWith (<) xs (drop 1 xs))
  accepts entries =
    case deserialiseFromBytes
      (decodeBitmaps maxTxsPerEb)
      (BSL.fromStrict . serialize' $ encodeBitmaps entries) of
      Left _ -> False
      Right (rest, decoded) -> BSL.null rest && decoded == entries
  -- Mostly-valid lists, with each rule broken often enough to matter: a zero
  -- bitmap, an out-of-range index, and a shuffle that breaks the ordering.
  genEntries = do
    n <- chooseInt (0, 6)
    -- One past the entry holding the EB's last offset, so out-of-range indices
    -- are drawn without naming the decoder's cap.
    ixs <- vectorOf n (chooseInt (0, (maxTxsPerEb - 1) `div` 64 + 1))
    bitmaps <- vectorOf n (frequency [(1, pure 0), (9, chooseInt (1, maxBound))])
    ordered <- frequency [(3, pure (List.sort (List.nub ixs))), (1, shuffle ixs)]
    pure
      [ (fromIntegral i, fromIntegral b)
      | (i, b) <- zip ordered (bitmaps <> repeat 1)
      ]
  shrinkEntries = shrinkList (const [])

-- | The boundary, as a unit test: single inputs with single expected answers,
-- so stating them as a 'Property' would be dressing.
--
-- Phrased as the contract rather than as the cap: every offset a full EB can
-- have must be requestable, and nothing past them.
test_decodeBitmapsBoundaries :: Assertion
test_decodeBitmapsBoundaries = do
  decodes (entriesUpTo lastEntry) @?= True
  decodes (entriesUpTo (lastEntry + 1)) @?= False
 where
  -- The entry holding the last offset an EB can have. Note 'maxTxsPerEb' need
  -- not be a multiple of 64, so this is not @maxTxsPerEb `div` 64@.
  lastEntry = (maxTxsPerEb - 1) `div` 64
  entriesUpTo i = [(fromIntegral j, 1) | j <- [0 .. i]]
  decodes entries =
    case deserialiseFromBytes
      (decodeBitmaps maxTxsPerEb)
      (BSL.fromStrict . serialize' $ encodeBitmaps entries) of
      Left _ -> False
      Right (rest, _) -> BSL.null rest

testPoint :: LeiosPoint
testPoint = MkLeiosPoint (SlotNo 0) (unsafeEbHashFromBytes $ BS.replicate 32 0xab)

encodeBitmapEntries :: [(Word16, Word64)] -> CBOR.Encoding
encodeBitmapEntries entries =
  CBOR.encodeMapLenIndef
    <> foldMap (\(i, b) -> CBOR.encodeWord16 i <> CBOR.encodeWord64 b) entries
    <> CBOR.encodeBreak

-- | @failsWith Nothing@ expects the decoder to accept. @failsWith (Just s)@
-- expects it to reject with a message that contains @s@, so that a case
-- rejected for another reason fails.
failsWith :: Maybe String -> Maybe String -> Property
failsWith expected failure =
  counterexample ("decoder failure: " <> show failure) $
    case (expected, failure) of
      (Nothing, Nothing) -> property True
      (Just s, Just msg) -> property (s `isInfixOf` msg)
      _ -> property False

-- | The failure message of the production LeiosFetch codec on the bytes in
-- the given state, or 'Nothing' if it decodes them.
decodeFailure ::
  ActiveState (st :: LeiosFetch LeiosPoint LeiosEb LeiosTx) =>
  StateToken st -> CBOR.Encoding -> IO (Maybe String)
decodeFailure stok msg = do
  let Codec{decode} =
        codecLeiosFetch
          maxTxsPerEb
          encodeLeiosPoint
          decodeLeiosPoint
          encodeLeiosEb
          decodeLeiosEb
          encodeLeiosTx
          decodeLeiosTx
  step <- decode stok
  either (\(DeserialiseFailure _ reason) -> Just reason) (const Nothing)
    <$> runDecoder [toLazyByteString msg] step
