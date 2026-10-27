{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests of LeiosFetch's codec: the bounds and CBOR encodings it accepts.
module Test.Consensus.Leios.LeiosFetch.Codec (tests) where

import Cardano.Binary (ToCBOR (..))
import Cardano.Crypto.Hash (Hash (UnsafeHash))
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Short as SBS
import Data.Functor ((<&>))
import qualified Data.Vector.Strict as V
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
  ( EndorserBlock
  , EndorserBlockHash
  , LeiosPoint (..)
  , LeiosTx (..)
  , decodeEndorserBlock
  , decodeLeiosPoint
  , decodeLeiosTx
  , encodeEndorserBlock
  , encodeLeiosPoint
  , encodeLeiosTx
  , endorserBlockHash
  , mkEndorserBlock
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type
import Test.Tasty
import Test.Tasty.HUnit
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
