{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- | Tests of LeiosFetch's codec: the bounds and CBOR encodings it accepts.
module Test.Consensus.Leios.LeiosFetch.Codec (tests) where

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.Vector.Strict as V
import Network.TypedProtocol.Codec
  ( Codec
  , CodecF (..)
  , SomeMessage (..)
  , runDecoder
  )
import Ouroboros.Consensus.Leios.Types
  ( LeiosEb
  , LeiosPoint
  , LeiosTx (..)
  , decodeLeiosEb
  , decodeLeiosPoint
  , decodeLeiosTx
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
        "the codec accepts either CBOR list encoding"
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
        ]
    ]

-- | Decode a 'MsgLeiosBlockTxs' with a codec that admits at most two txs.
decodeBlockTxs :: CBOR.Encoding -> IO (Maybe [LeiosTx])
decodeBlockTxs enc = do
  step <- decode codec SingBlockTxs
  runDecoder [CBOR.toLazyByteString enc] step >>= \case
    Left _ -> pure Nothing
    Right (SomeMessage (MsgLeiosBlockTxs txs)) -> pure $ Just $ V.toList txs
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

definite :: [CBOR.Encoding] -> CBOR.Encoding
definite xs = CBOR.encodeListLen (fromIntegral $ length xs) <> mconcat xs

indefinite :: [CBOR.Encoding] -> CBOR.Encoding
indefinite xs = CBOR.encodeListLenIndef <> mconcat xs <> CBOR.encodeBreak

txsKey :: CBOR.Encoding
txsKey = CBOR.encodeWord 4

twoTxs :: [CBOR.Encoding]
twoTxs = map encodeLeiosTx [txA, txB]

txA, txB :: LeiosTx
txA = MkLeiosTx $ BS.pack [1]
txB = MkLeiosTx $ BS.pack [2, 2]
