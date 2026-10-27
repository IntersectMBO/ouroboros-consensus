{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests of LeiosFetch's codec: the bounds and CBOR encodings it accepts.
module Test.Consensus.Leios.LeiosFetch.Codec (tests) where

import Cardano.Binary (ToCBOR (..))
import Cardano.Crypto.Hash (Hash (UnsafeHash), castHash, hashWith)
import Cardano.Ledger.Hashes (HASH)
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import Data.Bits (shiftL, shiftR, (.|.))
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import Data.Containers.ListUtils (nubOrd)
import Data.Functor ((<&>))
import Data.Maybe (isJust)
import qualified Data.Vector.Strict as V
import Data.Word (Word32, Word64, Word8)
import Network.TypedProtocol.Codec
  ( ActiveState
  , AnnotatedCodec
  , Annotator (..)
  , CodecF (..)
  , SomeMessage (..)
  , StateToken
  , runDecoder
  )
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EndorserBlock
  , EndorserBlockHash
  , LeiosPoint (..)
  , LeiosTx (..)
  , TxHash
  , decodeEndorserBlock
  , decodeLeiosPoint
  , decodeLeiosTx
  , encodeEndorserBlock
  , encodeEndorserBlockSize
  , encodeLeiosPoint
  , encodeLeiosTx
  , endorserBlockHash
  , endorserBlockReferences
  , maxEndorserBlockBytesSize
  , mkEndorserBlock
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type
import Ouroboros.Network.Protocol.Limits (ProtocolSizeLimits (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
  ( Gen
  , Property
  , arbitrary
  , choose
  , conjoin
  , counterexample
  , elements
  , forAll
  , ioProperty
  , listOf1
  , oneof
  , testProperty
  , vectorOf
  , (===)
  )
import Test.Util.Serialisation.CBOR (definite, definiteMap, indefinite, indefiniteMap)

tests :: TestTree
tests =
  testGroup
    "codec"
    [ testGroup
        "the codec accepts either CBOR encoding of a list or map"
        [ testCase "definite message, indefinite txs" $
            decodeBlockTxs (definite [txsKey, indefinite twoTxs]) >>= (@?= Just [txA, txB])
        , testCase "indefinite message, definite txs" $
            decodeBlockTxs (indefinite [txsKey, definite twoTxs]) >>= (@?= Just [txA, txB])
        , testCase "indefinite message, indefinite txs" $
            decodeBlockTxs (indefinite [txsKey, indefinite twoTxs]) >>= (@?= Just [txA, txB])
        , testCase "rejects indefinite txs past the bound" $
            decodeBlockTxs (definite [txsKey, indefinite (twoTxs <> [encodeLeiosTx txA])])
              >>= (@?= Nothing)
        , testCase "rejects an indefinite message with an extra field" $
            decodeBlockTxs (indefinite [txsKey, definite twoTxs, CBOR.encodeWord 0])
              >>= (@?= Nothing)
        , testCase "an indefinite point" $
            decodeBlockRequest (definite [CBOR.encodeWord 1, snd onePoint])
              >>= (@?= Just (fst onePoint))
        , testCase "a definite endorser block" $
            decodeBlock (definite [CBOR.encodeWord 2, snd oneRefEb]) >>= (@?= Just (fst oneRefEb))
        , testCase "keeps the bytes of an indefinite endorser block" $ do
            let eb = indefiniteMap [reference (BS.replicate 32 7)]
            decoded <- decodeBlock (definite [CBOR.encodeWord 2, eb])
            fmap (CBOR.toStrictByteString . encodeEndorserBlock) decoded
              @?= Just (CBOR.toStrictByteString eb)
            -- The same reference as 'oneRefEb', but other bytes.
            assertBool "hashes the bytes received" $
              fmap endorserBlockHash decoded /= Just (endorserBlockHash (fst oneRefEb))
        , testProperty
            "keeps, sizes and hashes the bytes of an endorser block in any encoding"
            prop_endorserBlockInAnyEncoding
        , testCase "rejects an empty endorser block" $
            decodeBlock (definite [CBOR.encodeWord 2, definiteMap []]) >>= (@?= Nothing)
        , testCase "rejects an endorser block naming a transaction twice" $
            decodeBlock
              (definite [CBOR.encodeWord 2, definiteMap (replicate 2 (reference (BS.replicate 32 7)))])
              >>= (@?= Nothing)
        , testCase "a definite bitmaps map" $
            decodeBlockTxsRequest (definite [txsRequestKey, somePoint, definiteMap [bitmap0]])
              >>= (@?= Just (fst onePoint, [(0, 1)]))
        , testCase "an indefinite bitmaps map" $
            decodeBlockTxsRequest (definite [txsRequestKey, somePoint, indefiniteMap [bitmap0]])
              >>= (@?= Just (fst onePoint, [(0, 1)]))
        , testCase "rejects indefinite bitmaps past the bound" $
            decodeBlockTxsRequest
              (definite [txsRequestKey, somePoint, indefiniteMap [bitmap0, bitmap1]])
              >>= (@?= Nothing)
        ]
    , testGroup
        "the codec refuses a hash that is not 32 bytes"
        [ testCase "a tx hash of 31 bytes" $
            decodeBlock (definite [CBOR.encodeWord 2, ebReferencing 31]) >>= (@?= Nothing)
        , testCase "a tx hash of 33 bytes" $
            decodeBlock (definite [CBOR.encodeWord 2, ebReferencing 33]) >>= (@?= Nothing)
        , testCase "an endorser block hash of 31 bytes" $
            decodeBlockRequest (definite [CBOR.encodeWord 1, pointOf 31]) >>= (@?= Nothing)
        , testCase "an endorser block hash of 33 bytes" $
            decodeBlockRequest (definite [CBOR.encodeWord 1, pointOf 33]) >>= (@?= Nothing)
        ]
    , testGroup
        "the byte limit admits a MsgLeiosBlock of the largest endorser block"
        [ testCase "a definite message" $ fitsByteLimit definite
        , testCase "an indefinite message" $ fitsByteLimit indefinite
        ]
    , testCase "the codec rejects an endorser block larger than maxEndorserBlockBytesSize" $ do
        let eb = encodeEndorserBlock (endorserBlockOfSize (maxSize + 1))
            msg = definite [CBOR.encodeWord 2, eb]
        BS.length (CBOR.toStrictByteString eb) @?= maxSize + 1
        -- Its message fits the byte limit, so only the codec can reject it.
        assertFitsByteLimit msg
        decodeBlock msg >>= (@?= Nothing)
    ]

-- | The codec accepts a 'MsgLeiosBlock' of an endorser block of
-- 'maxEndorserBlockBytesSize' in this list encoding, and the message fits
-- 'byteLimitsLeiosFetch'.
fitsByteLimit :: ([CBOR.Encoding] -> CBOR.Encoding) -> Assertion
fitsByteLimit list = do
  BS.length (CBOR.toStrictByteString eb) @?= maxSize
  decoded <- decodeBlock msg
  assertBool "the codec rejects the message" (isJust decoded)
  assertFitsByteLimit msg
 where
  eb = encodeEndorserBlock (endorserBlockOfSize maxSize)
  msg = list [CBOR.encodeWord 2, eb]

assertFitsByteLimit :: CBOR.Encoding -> Assertion
assertFitsByteLimit msg =
  assertBool
    (show size <> " bytes exceed the limit of " <> show msgLeiosBlockByteLimit)
    (size <= msgLeiosBlockByteLimit)
 where
  size = fromIntegral (BSL.length (CBOR.toLazyByteString msg))

msgLeiosBlockByteLimit :: Word
msgLeiosBlockByteLimit = sizeLimitForState (byteLimitsLeiosFetch (fromIntegral . BSL.length)) SingBlock

maxSize :: Int
maxSize = fromIntegral maxEndorserBlockBytesSize

-- | An endorser block whose encoding takes this many bytes, for sizes near
-- 'maxSize': after a 3-byte map header, each reference takes 35 bytes, or 36
-- for a size of 100.
endorserBlockOfSize :: Int -> EndorserBlock
endorserBlockOfSize size =
  mkEndorserBlock [(distinctTxHash i, if i < spare then 100 else 1) | i <- [0 .. n - 1]]
 where
  (n, spare) = (size - 3) `divMod` 35

-- | A tx hash for each 'Int' from 0 to @2^32 - 1@.
distinctTxHash :: Int -> TxHash
distinctTxHash i = UnsafeHash $ SBS.pack $ replicate 28 0 <> [fromIntegral (i `shiftR` s) | s <- [24, 16, 8, 0]]

-- | Whatever CBOR encoding of its references an endorser block arrives in, the
-- decoded endorser block names those references, and keeps, sizes and hashes
-- exactly the bytes it arrived in.
prop_endorserBlockInAnyEncoding :: Property
prop_endorserBlockInAnyEncoding = forAll genEncodedEb $ \(refs, bytes) -> ioProperty $ do
  decoded <- decodeBlock (definite [CBOR.encodeWord 2, CBOR.encodePreEncoded bytes])
  pure $ case decoded of
    Nothing -> counterexample "the codec rejects the endorser block" False
    Just eb ->
      conjoin
        [ counterexample "references" $ endorserBlockReferences eb === refs
        , counterexample "bytes" $ CBOR.toStrictByteString (encodeEndorserBlock eb) === bytes
        , counterexample "size" $ encodeEndorserBlockSize eb === fromIntegral (BS.length bytes)
        , counterexample "hash" $ endorserBlockHash eb === castHash (hashWith @HASH id bytes)
        ]

-- | The references of an endorser block, and an encoding of them: either kind
-- of map, and each CBOR head with its argument in its fewest bytes or more.
genEncodedEb :: Gen ([(TxHash, BytesSize)], BS.ByteString)
genEncodedEb = do
  hashes <- nubOrd <$> listOf1 (vectorOf 32 arbitrary)
  refs <- traverse (\hash -> (,) hash <$> genTxSize) hashes
  encodedRefs <- concat <$> traverse encodeRef refs
  framed <-
    oneof
      [ (<> encodedRefs) <$> genHead 5 (fromIntegral (length refs))
      , pure $ [0xbf] <> encodedRefs <> [0xff]
      ]
  pure ([(UnsafeHash (SBS.pack hash), size) | (hash, size) <- refs], BS.pack framed)
 where
  encodeRef (hash, size) = do
    hashHead <- genHead 2 32
    sizeHead <- genHead 0 (fromIntegral size)
    pure $ hashHead <> hash <> sizeHead

  genTxSize :: Gen Word32
  genTxSize = oneof [choose (0, 23), choose (24, 255), choose (256, 65535), arbitrary]

-- | A CBOR head of this major type and argument, the argument in its fewest
-- bytes or in any more that hold it.
genHead :: Word8 -> Word64 -> Gen [Word8]
genHead major n =
  elements $
    [[major `shiftL` 5 .|. fromIntegral n] | n < 24]
      <> [ (major `shiftL` 5 .|. info) : [fromIntegral (n `shiftR` (8 * i)) | i <- [len - 1, len - 2 .. 0]]
         | (info, len) <- [(24, 1), (25, 2), (26, 4), (27, 8)]
         , toInteger n < 2 ^ (8 * len)
         ]

-- | Decode a message with a codec that admits at most two txs.
decodeMsg ::
  forall (st :: LeiosFetch LeiosPoint EndorserBlock LeiosTx).
  ActiveState st =>
  StateToken st ->
  CBOR.Encoding ->
  IO (Maybe (SomeMessage st))
decodeMsg stok enc = do
  step <- decode codec stok
  either (const Nothing) (Just . (`runAnnotator` bytes)) <$> runDecoder [bytes] step
 where
  bytes = CBOR.toLazyByteString enc

  codec ::
    AnnotatedCodec (LeiosFetch LeiosPoint EndorserBlock LeiosTx) CBOR.DeserialiseFailure IO ByteString
  codec =
    codecLeiosFetch
      2
      encodeLeiosPoint
      decodeLeiosPoint
      encodeEndorserBlock
      decodeEndorserBlock
      encodeLeiosTx
      decodeLeiosTx

decodeBlockTxs :: CBOR.Encoding -> IO (Maybe [LeiosTx])
decodeBlockTxs enc =
  decodeMsg SingBlockTxs enc <&> fmap (\case SomeMessage (MsgLeiosBlockTxs txs) -> V.toList txs)

decodeBlock :: CBOR.Encoding -> IO (Maybe EndorserBlock)
decodeBlock enc = decodeMsg SingBlock enc <&> fmap (\case SomeMessage (MsgLeiosBlock eb) -> eb)

-- | Decode a 'MsgLeiosBlockTxsRequest'. The codec's bound of two txs admits one
-- bitmap entry.
decodeBlockTxsRequest :: CBOR.Encoding -> IO (Maybe (LeiosPoint, TxBitmaps))
decodeBlockTxsRequest enc =
  decodeMsg SingIdle enc <&> \case
    Just (SomeMessage (MsgLeiosBlockTxsRequest p bitmaps)) -> Just (p, bitmaps)
    _ -> Nothing

decodeBlockRequest :: CBOR.Encoding -> IO (Maybe LeiosPoint)
decodeBlockRequest enc =
  decodeMsg SingIdle enc <&> \case
    Just (SomeMessage (MsgLeiosBlockRequest p)) -> Just p
    _ -> Nothing

txsRequestKey :: CBOR.Encoding
txsRequestKey = CBOR.encodeWord 3

-- | 'onePoint', encoded as the codec writes it.
somePoint :: CBOR.Encoding
somePoint = encodeLeiosPoint (fst onePoint)

-- | Bitmap entries naming the last tx of the first and of the second run of 64.
bitmap0, bitmap1 :: CBOR.Encoding
bitmap0 = CBOR.encodeWord16 0 <> CBOR.encodeWord64 1
bitmap1 = CBOR.encodeWord16 1 <> CBOR.encodeWord64 1

txsKey :: CBOR.Encoding
txsKey = CBOR.encodeWord 4

twoTxs :: [CBOR.Encoding]
twoTxs = map encodeLeiosTx [txA, txB]

txA, txB :: LeiosTx
txA = MkLeiosTx $ BS.pack [1]
txB = MkLeiosTx $ BS.pack [2, 2]

-- | An endorser block of one reference, and its encoding.
oneRefEb :: (EndorserBlock, CBOR.Encoding)
oneRefEb =
  ( mkEndorserBlock [(UnsafeHash (SBS.toShort hash), 100)]
  , definiteMap [reference hash]
  )
 where
  hash = BS.replicate 32 7

-- | An endorser block referencing one tx by a hash of this many bytes.
ebReferencing :: Int -> CBOR.Encoding
ebReferencing n = definiteMap [reference (BS.replicate n 7)]

-- | A reference to a tx of 100 bytes by these hash bytes.
reference :: BS.ByteString -> CBOR.Encoding
reference hash = CBOR.encodeBytes hash <> CBOR.encodeWord32 100

-- | A point with an endorser block hash of this many bytes.
pointOf :: Int -> CBOR.Encoding
pointOf n = indefinite [CBOR.encodeWord64 1, CBOR.encodeBytes (BS.replicate n 1)]

-- | A point, and its encoding as an indefinite-length list.
onePoint :: (LeiosPoint, CBOR.Encoding)
onePoint =
  ( MkLeiosPoint (SlotNo 1) hash
  , indefinite [CBOR.encodeWord64 1, toCBOR hash]
  )
 where
  hash :: EndorserBlockHash
  hash = UnsafeHash (SBS.replicate 32 1)
