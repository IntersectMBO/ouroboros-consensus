{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- | Tests of LeiosNotify's codec: the bounds and CBOR encodings it accepts.
module Test.Consensus.Leios.LeiosNotify.Codec
  ( signedVote
  , tests
  ) where

import Cardano.Binary.FixedSizeCodec (encodeFixedSized)
import Cardano.Crypto.DSIGN.Class (genKeyDSIGN, signDSIGN)
import Cardano.Crypto.Leios (LeiosSeatId (..))
import Cardano.Crypto.Seed (mkSeedFromBytes)
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import qualified Data.List.NonEmpty as NE
import Data.Word (Word16)
import Network.TypedProtocol.Codec
  ( CodecF (..)
  , SomeMessage (..)
  , runDecoder
  )
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Leios.Types
  ( EbHash (..)
  , LeiosPoint (..)
  , LeiosVote (..)
  , RbHash (..)
  , decodeLeiosPoint
  , decodeLeiosVote
  , encodeEbHash
  , encodeRbHash
  )
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Type
import Test.Consensus.Leios.LeiosNotify.Protocol (P, codec)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup "codec" $
    [ testCase "the codec rejects an empty MsgLeiosVotes" $
        assertBool "decoded" $
          isLeft $
            runSimOrThrow decodeEmptyVotes
    ]
      <> [ testGroup
             "the codec accepts either CBOR list encoding"
             [ testCase "definite message, indefinite votes" $
                 runSimOrThrow (decodeVotes (definite [votesKey, indefinite twoVotes])) @?= Just [1, 2]
             , testCase "indefinite message, definite votes" $
                 runSimOrThrow (decodeVotes (indefinite [votesKey, definite twoVotes])) @?= Just [1, 2]
             , testCase "indefinite message, indefinite votes" $
                 runSimOrThrow (decodeVotes (indefinite [votesKey, indefinite twoVotes])) @?= Just [1, 2]
             , testCase "rejects indefinite votes past the bound" $
                 runSimOrThrow
                   (decodeVotes (definite [votesKey, indefinite (map CBOR.encodeInt [1 .. 17])]))
                   @?= Nothing
             , testCase "rejects an indefinite message with an extra field" $
                 runSimOrThrow (decodeVotes (indefinite [votesKey, definite twoVotes, CBOR.encodeInt 0]))
                   @?= Nothing
             , testCase "rejects empty indefinite votes" $
                 runSimOrThrow (decodeVotes (definite [votesKey, indefinite []])) @?= Nothing
             , testCase "an indefinite point" $
                 let p = MkLeiosPoint (SlotNo 3) (MkEbHash (BS.replicate 32 7))
                  in decodeWith
                       decodeLeiosPoint
                       (indefinite [CBOR.encodeWord64 3, encodeEbHash (MkEbHash (BS.replicate 32 7))])
                       @?= Just p
             , testCase "an indefinite vote" $
                 let v = signedVote (MkRbHash (BS.replicate 32 1)) 0
                  in decodeWith decodeLeiosVote (indefiniteVote v) @?= Just v
             ]
         ]

-- | Decode a 'MsgLeiosVotes' that holds no votes.
decodeEmptyVotes :: IOSim s (Either CBOR.DeserialiseFailure (SomeMessage (StBusy :: P)))
decodeEmptyVotes = do
  step <- decode codec SingBusy
  runDecoder
    [CBOR.toLazyByteString (CBOR.encodeListLen 2 <> CBOR.encodeWord 7 <> CBOR.encodeListLen 0)]
    step

-- | Decode a 'MsgLeiosVotes'.
decodeVotes :: CBOR.Encoding -> IOSim s (Maybe [Int])
decodeVotes enc = do
  step <- decode codec SingBusy
  runDecoder [CBOR.toLazyByteString enc] step >>= \case
    Right (SomeMessage (MsgLeiosVotes vs)) -> pure (Just (NE.toList vs))
    _ -> pure Nothing

decodeWith :: (forall s. CBOR.Decoder s a) -> CBOR.Encoding -> Maybe a
decodeWith dec enc =
  either (const Nothing) (Just . snd) $
    CBOR.deserialiseFromBytes dec (CBOR.toLazyByteString enc)

indefiniteVote :: LeiosVote -> CBOR.Encoding
indefiniteVote (MkLeiosVote rbHash seat signature) =
  indefinite
    [encodeRbHash rbHash, CBOR.encodeWord16 (leiosSeatIndex seat), encodeFixedSized signature]

definite :: [CBOR.Encoding] -> CBOR.Encoding
definite xs = CBOR.encodeListLen (fromIntegral $ length xs) <> mconcat xs

indefinite :: [CBOR.Encoding] -> CBOR.Encoding
indefinite xs = CBOR.encodeListLenIndef <> mconcat xs <> CBOR.encodeBreak

votesKey :: CBOR.Encoding
votesKey = CBOR.encodeWord 7

twoVotes :: [CBOR.Encoding]
twoVotes = map CBOR.encodeInt [1, 2]

-- | A vote from this seat for this announcing block. The node's LeiosNotify
-- client does not check a vote's signature, so any key will do.
signedVote :: RbHash -> Word16 -> LeiosVote
signedVote rbHash seat =
  MkLeiosVote rbHash (LeiosSeatId seat) $
    signDSIGN () rbHash (genKeyDSIGN (mkSeedFromBytes (BS.replicate 32 0)))
