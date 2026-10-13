{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests of LeiosFetch's codec: the bounds and CBOR encodings it accepts.
module Test.Consensus.Leios.LeiosFetch.Codec (tests) where

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import Data.Functor ((<&>))
import qualified Data.Vector.Strict as V
import Network.TypedProtocol.Codec
  ( ActiveState
  , Codec
  , CodecF (..)
  , SomeMessage (..)
  , StateToken
  , runDecoder
  )
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Leios.Types
  ( EbHash (..)
  , LeiosEb (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , TxHash (..)
  , decodeLeiosEb
  , decodeLeiosPoint
  , decodeLeiosTx
  , encodeEbHash
  , encodeLeiosEb
  , encodeLeiosPoint
  , encodeLeiosTx
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type
import Test.Tasty
import Test.Tasty.HUnit

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
        , testCase "an indefinite endorser block" $
            decodeBlock (definite [CBOR.encodeWord 2, snd oneRefEb]) >>= (@?= Just (fst oneRefEb))
        , testCase "rejects an empty indefinite endorser block" $
            decodeBlock
              (definite [CBOR.encodeWord 2, CBOR.encodeMapLenIndef <> CBOR.encodeBreak])
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
    ]

-- | Decode a message with a codec that admits at most two txs.
decodeMsg ::
  forall (st :: LeiosFetch LeiosPoint LeiosEb LeiosTx).
  ActiveState st =>
  StateToken st ->
  CBOR.Encoding ->
  IO (Maybe (SomeMessage st))
decodeMsg stok enc = do
  step <- decode codec stok
  either (const Nothing) Just <$> runDecoder [CBOR.toLazyByteString enc] step
 where
  codec :: Codec (LeiosFetch LeiosPoint LeiosEb LeiosTx) CBOR.DeserialiseFailure IO ByteString
  codec =
    codecLeiosFetch
      2
      encodeLeiosPoint
      decodeLeiosPoint
      encodeLeiosEb
      decodeLeiosEb
      encodeLeiosTx
      decodeLeiosTx

decodeBlockTxs :: CBOR.Encoding -> IO (Maybe [LeiosTx])
decodeBlockTxs enc =
  decodeMsg SingBlockTxs enc <&> fmap (\case SomeMessage (MsgLeiosBlockTxs txs) -> V.toList txs)

decodeBlock :: CBOR.Encoding -> IO (Maybe LeiosEb)
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

definite :: [CBOR.Encoding] -> CBOR.Encoding
definite xs = CBOR.encodeListLen (fromIntegral $ length xs) <> mconcat xs

indefinite :: [CBOR.Encoding] -> CBOR.Encoding
indefinite xs = CBOR.encodeListLenIndef <> mconcat xs <> CBOR.encodeBreak

-- | A definite-length map of these key-value pairs.
definiteMap :: [CBOR.Encoding] -> CBOR.Encoding
definiteMap kvs = CBOR.encodeMapLen (fromIntegral $ length kvs) <> mconcat kvs

-- | An indefinite-length map of these key-value pairs.
indefiniteMap :: [CBOR.Encoding] -> CBOR.Encoding
indefiniteMap kvs = CBOR.encodeMapLenIndef <> mconcat kvs <> CBOR.encodeBreak

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

-- | An endorser block of one reference, and its encoding as an
-- indefinite-length map.
oneRefEb :: (LeiosEb, CBOR.Encoding)
oneRefEb =
  ( MkLeiosEb $ V.singleton (MkTxHash hash, 100)
  , CBOR.encodeMapLenIndef
      <> CBOR.encodeBytes hash
      <> CBOR.encodeWord32 100
      <> CBOR.encodeBreak
  )
 where
  hash = BS.replicate 32 7

-- | A point, and its encoding as an indefinite-length list.
onePoint :: (LeiosPoint, CBOR.Encoding)
onePoint =
  ( MkLeiosPoint (SlotNo 1) hash
  , indefinite [CBOR.encodeWord64 1, encodeEbHash hash]
  )
 where
  hash = MkEbHash (BS.replicate 32 1)
