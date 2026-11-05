{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.Leios.LeiosFetch.AtomicCodec (tests) where

import Cardano.Network.NodeToNode.Version
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import Control.Exception (try)
import Control.Monad (foldM, forM_)
import Data.Bits (bit, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Strict as V
import Data.Word (Word64)
import Network.TypedProtocol.Codec
  ( ActiveState
  , AnnotatedCodec
  , Annotator (..)
  , AnyMessage (..)
  , CodecF (..)
  , SomeMessage (..)
  , StateTokenI
  , runDecoder
  )
import Ouroboros.Consensus.Leios.Types
  ( EndorserBlock
  , LeiosPoint (..)
  , LeiosTx (..)
  , decodeEndorserBlock
  , decodeLeiosPoint
  , decodeLeiosTx
  , encodeEndorserBlock
  , encodeLeiosPoint
  , encodeLeiosTx
  , endorserBlockFromTxBytes
  , endorserBlockHash
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Inbound
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Type as A
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity (WireOrdinal (..))
import Ouroboros.Consensus.Network.LeiosFetch
import Ouroboros.Consensus.Node.NetworkProtocolVersion (withExperimentalAtomicFetchVersion)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (Property, chooseInt, counterexample, forAll, property, testProperty)

type Atomic = A.LeiosFetchAtomic LeiosPoint EndorserBlock LeiosTx

data Sample where
  Sample ::
    (ActiveState st, StateTokenI st, StateTokenI st') =>
    A.SingLeiosFetchAtomic (st :: Atomic) -> A.Message Atomic st st' -> Sample

tests :: TestTree
tests =
  testGroup
    "atomic wire codec"
    [ testCase "all nine message constructors round trip at every byte split" testRoundTrips
    , testCase "all constructors round trip through the identity codec" testIdentity
    , testCase "Word64 maximum ordinals survive encoding and decoding" testOrdinalMaximum
    , testCase "negative and overflowing ordinals are rejected before narrowing" testOrdinalBounds
    , testCase "only the matching busy state accepts its terminal or cancel ACK" testReplyStates
    , testCase "empty TX requests and empty/excess TX replies are rejected" testEmptyAndCount
    , testCase "request bitmaps reject zeros, repeats, unordered and out-of-range offsets" testBitmaps
    , testCase "unknown tags, old request arities and extra fields are rejected" testFraming
    , testCase "body annotation preserves the received encoding" testBodyAnnotation
    , testCase
        "incoming ordinals are consecutive across both data kinds and controls"
        testIncomingSequence
    , testCase "cancel targets are recent data requests and can be canceled only once" testCancelTargets
    , testCase "cancel window is strict and ordinal exhaustion cannot wrap" testIncomingBounds
    , testProperty "incoming history stays bounded by widened W=2D" propHistoryBound
    , testCase
        "both directions select the same frozen capability and legacy cannot run as atomic"
        testConnectionSelection
    , testCase
        "experimental version-map opt-in retains version-16 block serialization"
        testExplicitVersions
    ]

txA, txB :: LeiosTx
txA = MkLeiosTx (BS.pack [1, 2, 3])
txB = MkLeiosTx (BS.pack [4, 5, 6])

body :: EndorserBlock
body = endorserBlockFromTxBytes (leiosTxBytes txA :| [leiosTxBytes txB])

point :: LeiosPoint
point = MkLeiosPoint (SlotNo 5) (endorserBlockHash body)

codec :: AnnotatedCodec Atomic CBOR.DeserialiseFailure IO BSL.ByteString
codec =
  A.codecLeiosFetchAtomic
    2
    encodeLeiosPoint
    decodeLeiosPoint
    encodeEndorserBlock
    decodeEndorserBlock
    encodeLeiosTx
    decodeLeiosTx

samples :: [Sample]
samples =
  [ Sample A.SingIdle A.MsgDone
  , Sample A.SingIdle (A.MsgLeiosBlockRequest (WireOrdinal 0) point)
  , Sample A.SingBlock (A.MsgLeiosBlock body)
  , Sample A.SingIdle (A.MsgLeiosBlockTxsRequest (WireOrdinal 1) point ((0, bit 63 .|. bit 62) :| []))
  , Sample A.SingBlockTxs (A.MsgLeiosBlockTxs (V.fromList [txA, txB]))
  , Sample A.SingIdle (A.MsgTryCancel (WireOrdinal 2) (WireOrdinal 0))
  , Sample A.SingBlock A.MsgBlockCancelled
  , Sample A.SingBlockTxs A.MsgBlockTxsCancelled
  , Sample A.SingCancel A.MsgAckTryCancel
  ]

decoded ::
  ActiveState st =>
  A.SingLeiosFetchAtomic (st :: Atomic) ->
  [BSL.ByteString] ->
  IO (Either CBOR.DeserialiseFailure (SomeMessage st))
decoded stok chunks = do
  step <- decode codec stok
  fmap (\annotation -> runAnnotator annotation (mconcat chunks)) <$> runDecoder chunks step

reject :: ActiveState st => A.SingLeiosFetchAtomic (st :: Atomic) -> CBOR.Encoding -> Assertion
reject stok encoding =
  decoded stok [CBOR.toLazyByteString encoding] >>= assertBool "invalid message was accepted" . isLeft

testRoundTrips :: Assertion
testRoundTrips = forM_ samples $ \(Sample stok message) -> do
  let bytes = encode codec message
  forM_ [0 .. BSL.length bytes] $ \i -> do
    result <- decoded stok [BSL.take i bytes, BSL.drop i bytes]
    case result of
      Left err -> assertFailure (show err)
      Right (SomeMessage received) -> do
        show received @?= show message
        encode codec received @?= bytes

testIdentity :: Assertion
testIdentity = forM_ samples $ \(Sample stok message) -> do
  let identity = A.codecLeiosFetchAtomicId 2
  step <- decode identity stok
  result <- runDecoder [encode identity message] step
  case result of
    Left err -> assertFailure (show err)
    Right (SomeMessage received) -> show received @?= show message

testOrdinalMaximum :: Assertion
testOrdinalMaximum = do
  let messages =
        [ Sample A.SingIdle (A.MsgLeiosBlockRequest (WireOrdinal maxBound) point)
        , Sample A.SingIdle (A.MsgLeiosBlockTxsRequest (WireOrdinal maxBound) point ((0, bit 63) :| []))
        , Sample A.SingIdle (A.MsgTryCancel (WireOrdinal maxBound) (WireOrdinal maxBound))
        ]
  forM_ messages $ \(Sample stok message) -> do
    result <- decoded stok [encode codec message]
    case result of
      Right (SomeMessage received) -> show received @?= show message
      Left err -> assertFailure (show err)

list :: [CBOR.Encoding] -> CBOR.Encoding
list xs = CBOR.encodeListLen (fromIntegral (length xs)) <> mconcat xs

testOrdinalBounds :: Assertion
testOrdinalBounds = forM_ [-1, 2 ^ (64 :: Int)] $ \n -> do
  reject A.SingIdle (list [CBOR.encodeWord 1, CBOR.encodeInteger n, encodeLeiosPoint point])
  reject
    A.SingIdle
    ( list
        [ CBOR.encodeWord 3
        , CBOR.encodeInteger n
        , encodeLeiosPoint point
        , CBOR.encodeMapLen 1 <> CBOR.encodeWord 0 <> CBOR.encodeWord64 (bit 63)
        ]
    )
  reject A.SingIdle (list [CBOR.encodeWord 5, CBOR.encodeWord 1, CBOR.encodeInteger n])
  reject A.SingIdle (list [CBOR.encodeWord 5, CBOR.encodeInteger n, CBOR.encodeWord 0])

testReplyStates :: Assertion
testReplyStates = do
  let canceled = list [CBOR.encodeWord 6]; ack = list [CBOR.encodeWord 7]
  reject A.SingIdle canceled
  reject A.SingCancel canceled
  reject A.SingIdle ack
  reject A.SingBlock ack
  reject A.SingBlockTxs ack
  reject A.SingCancel (encodeLeiosPoint point)
  step <- decode (A.codecLeiosFetchAtomicId 2) A.SingBlock
  result <-
    runDecoder [AnyMessage (A.MsgAckTryCancel :: A.Message Atomic (A.StBusy A.StCancel) A.StIdle)] step
  assertBool "identity codec rejects wrong reply state" (isLeft result)

testEmptyAndCount :: Assertion
testEmptyAndCount = do
  reject
    A.SingIdle
    (list [CBOR.encodeWord 3, CBOR.encodeWord 0, encodeLeiosPoint point, CBOR.encodeMapLen 0])
  reject A.SingBlockTxs (list [CBOR.encodeWord 4, CBOR.encodeListLen 0])
  reject
    A.SingBlockTxs
    (list [CBOR.encodeWord 4, CBOR.encodeListLen 3 <> foldMap encodeLeiosTx [txA, txB, txA]])
  step <- decode (A.codecLeiosFetchAtomicId 2) A.SingBlockTxs
  result <-
    runDecoder
      [AnyMessage (A.MsgLeiosBlockTxs V.empty :: A.Message Atomic (A.StBusy A.StBlockTxs) A.StIdle)]
      step
  assertBool "identity codec rejects empty TX reply" (isLeft result)

testBitmaps :: Assertion
testBitmaps = forM_
  [ [(0, 0)]
  , [(0, bit 63), (0, bit 62)]
  , [(1, bit 63), (0, bit 63)]
  , [(0, bit 61)]
  , [(maxBound, bit 63)]
  ]
  $ \entries ->
    reject
      A.SingIdle
      ( list
          [ CBOR.encodeWord 3
          , CBOR.encodeWord 0
          , encodeLeiosPoint point
          , CBOR.encodeMapLen (fromIntegral (length entries))
              <> foldMap (\(i, b) -> CBOR.encodeWord16 i <> CBOR.encodeWord64 b) entries
          ]
      )

testFraming :: Assertion
testFraming = do
  reject A.SingIdle (list [CBOR.encodeWord 99])
  reject A.SingIdle (list [CBOR.encodeWord 1, encodeLeiosPoint point])
  reject
    A.SingIdle
    (list [CBOR.encodeWord 5, CBOR.encodeWord 1, CBOR.encodeWord 0, CBOR.encodeWord 9])
  reject A.SingBlock (list [CBOR.encodeWord 6, CBOR.encodeWord 0])
  reject A.SingBlockTxs (list [CBOR.encodeWord 7])

testBodyAnnotation :: Assertion
testBodyAnnotation = do
  let rawBody =
        CBOR.encodeMapLenIndef
          <> CBOR.encodeBytes (BS.replicate 32 7)
          <> CBOR.encodeWord32 100
          <> CBOR.encodeBreak
      bytes = CBOR.toLazyByteString (list [CBOR.encodeWord 2, rawBody])
  result <- decoded A.SingBlock [bytes]
  case result of
    Right (SomeMessage (A.MsgLeiosBlock received)) ->
      CBOR.toLazyByteString (encodeEndorserBlock received) @?= CBOR.toLazyByteString rawBody
    Left err -> assertFailure (show err)
    _ -> assertFailure "expected normal body"

initial :: Word -> InboundState
initial d = either (error . show) id (newInboundState (fromIntegral d) maxBound)

bodyRequest :: Word64 -> A.Message Atomic A.StIdle (A.StBusy A.StBlock)
bodyRequest n = A.MsgLeiosBlockRequest (WireOrdinal n) point

cancel :: Word64 -> Word64 -> A.Message Atomic A.StIdle (A.StBusy A.StCancel)
cancel n t = A.MsgTryCancel (WireOrdinal n) (WireOrdinal t)

right :: Show e => Either e a -> a
right = either (error . show) id

testIncomingSequence :: Assertion
testIncomingSequence = do
  let s1 = right $ checkIncomingMessage (bodyRequest 0) (initial 4)
      txRequest = A.MsgLeiosBlockTxsRequest (WireOrdinal 1) point ((0, bit 63) :| [])
      s2 = right $ checkIncomingMessage txRequest s1
      s3 = right $ checkIncomingMessage (cancel 2 0) s2
  incomingNextOrdinal s3 @?= 3
  checkIncomingMessage (bodyRequest 0) s1 @?= Left UnexpectedIncomingOrdinal
  checkIncomingMessage (bodyRequest 2) s1 @?= Left UnexpectedIncomingOrdinal
  let done = right $ checkIncomingMessage (A.MsgDone :: A.Message Atomic A.StIdle A.StDone) s3
  incomingNextOrdinal done @?= 3
  checkIncomingMessage (bodyRequest 3) done @?= Left IncomingSessionClosed

testCancelTargets :: Assertion
testCancelTargets = do
  let s1 = right $ checkIncomingMessage (bodyRequest 0) (initial 3)
      s2 = right $ checkIncomingMessage (cancel 1 0) s1
  checkIncomingMessage (cancel 2 0) s2 @?= Left IncomingRepeatedCancel
  checkIncomingMessage (cancel 2 1) s2 @?= Left IncomingControlTarget
  checkIncomingMessage (cancel 2 2) s2 @?= Left IncomingCancelOutsideWindow
  checkIncomingMessage (cancel 2 maxBound) s2 @?= Left IncomingCancelOutsideWindow

testIncomingBounds :: Assertion
testIncomingBounds = do
  let s1 = right $ checkIncomingMessage (bodyRequest 0) (initial 1)
      s2 = right $ checkIncomingMessage (bodyRequest 1) s1
  checkIncomingMessage (cancel 2 0) s2 @?= Left IncomingCancelOutsideWindow
  assertBool "last in-window target accepted" (not (isLeft (checkIncomingMessage (cancel 2 1) s2)))
  let tiny = right (newInboundState 1 0)
      exhausted = right (checkIncomingMessage (bodyRequest 0) tiny)
  incomingNextOrdinal exhausted @?= 1
  checkIncomingMessage (bodyRequest 0) exhausted @?= Left IncomingOrdinalExhausted
  newInboundState 0 maxBound @?= Left InvalidIncomingLimit

propHistoryBound :: Property
propHistoryBound = forAll (chooseInt (1, 128)) $ \d ->
  case foldM (\s n -> checkIncomingMessage (bodyRequest n) s) (initial (fromIntegral d)) [0 .. 256] of
    Left err -> counterexample (show err) False
    Right s -> property (incomingHistorySize s <= 2 * fromIntegral d && incomingNextOrdinal s == 257)

testConnectionSelection :: Assertion
testConnectionSelection = do
  negotiatedLeiosFetchProtocol NodeToNodeV_16 Nothing @?= Right LegacyLeiosFetch
  let d = getLocalLeiosFetchRequestLimit NodeToNodeV_17
  negotiatedLeiosFetchProtocol NodeToNodeV_17 d
    @?= (AtomicLeiosFetch <$> maybe (Left InvalidLeiosFetchCapability) Right d)
  negotiatedLeiosFetchProtocol NodeToNodeV_17 Nothing @?= Left InvalidLeiosFetchCapability
  negotiatedLeiosFetchProtocol NodeToNodeV_16 d @?= Left InvalidLeiosFetchCapability
  blocked <- try (requireLegacyLeiosFetch NodeToNodeV_17 d :: IO ())
  blocked @?= Left AtomicLeiosFetchNotActivated
  requireLegacyLeiosFetch NodeToNodeV_16 Nothing

testExplicitVersions :: Assertion
testExplicitVersions = do
  let old = Map.fromList [(NodeToNodeV_15, "old"), (NodeToNodeV_16, "block-v2")]
      experimental = withExperimentalAtomicFetchVersion old
  experimental Map.! NodeToNodeV_17 @?= "block-v2"
  Map.delete NodeToNodeV_17 experimental @?= old
