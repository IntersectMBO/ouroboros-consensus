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
import Cardano.Crypto.Hash (Hash (UnsafeHash), castHash, hashToBytes, hashWith)
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
import Data.List (inits, sort, tails)
import Data.Maybe (isJust)
import qualified Data.Vector.Strict as V
import Data.Word (Word16, Word32, Word64, Word8)
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
  , forAllShrink
  , ioProperty
  , listOf
  , listOf1
  , oneof
  , shrink
  , shrinkList
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
            decodeMsgLeiosBlockTxsViaLeiosFetchCodec (definite [txsKey, indefinite twoTxs])
              >>= (@?= Just [txA, txB])
        , testCase "indefinite message, definite txs" $
            decodeMsgLeiosBlockTxsViaLeiosFetchCodec (indefinite [txsKey, definite twoTxs])
              >>= (@?= Just [txA, txB])
        , testCase "indefinite message, indefinite txs" $
            decodeMsgLeiosBlockTxsViaLeiosFetchCodec (indefinite [txsKey, indefinite twoTxs])
              >>= (@?= Just [txA, txB])
        , testCase "rejects indefinite txs past the bound" $
            decodeMsgLeiosBlockTxsViaLeiosFetchCodec
              (definite [txsKey, indefinite (twoTxs <> [encodeLeiosTx txA])])
              >>= (@?= Nothing)
        , testCase "rejects an indefinite message with an extra field" $
            decodeMsgLeiosBlockTxsViaLeiosFetchCodec (indefinite [txsKey, definite twoTxs, CBOR.encodeWord 0])
              >>= (@?= Nothing)
        , testCase "an indefinite point" $
            decodeMsgLeiosBlockRequestViaLeiosFetchCodec (definite [CBOR.encodeWord 1, snd onePoint])
              >>= (@?= Just (fst onePoint))
        , testCase "a definite endorser block" $
            decodeMsgLeiosBlockViaLeiosFetchCodec (definite [CBOR.encodeWord 2, snd oneRefEb])
              >>= (@?= Just (fst oneRefEb))
        , testCase "keeps the bytes of an indefinite endorser block" $ do
            let eb = indefiniteMap [reference (BS.replicate 32 7)]
            decoded <- decodeMsgLeiosBlockViaLeiosFetchCodec (definite [CBOR.encodeWord 2, eb])
            fmap (CBOR.toStrictByteString . encodeEndorserBlock) decoded
              @?= Just (CBOR.toStrictByteString eb)
            -- The same reference as 'oneRefEb', but other bytes.
            assertBool "hashes the bytes received" $
              fmap endorserBlockHash decoded /= Just (endorserBlockHash (fst oneRefEb))
        , testProperty
            "slices exactly the bytes of an endorser block out of its MsgLeiosBlock"
            prop_slicesEndorserBlockFromMessage
        , testCase "rejects an empty endorser block" $
            decodeMsgLeiosBlockViaLeiosFetchCodec (definite [CBOR.encodeWord 2, definiteMap []])
              >>= (@?= Nothing)
        , testCase "rejects an endorser block naming a transaction twice" $
            decodeMsgLeiosBlockViaLeiosFetchCodec
              (definite [CBOR.encodeWord 2, definiteMap (replicate 2 (reference (BS.replicate 32 7)))])
              >>= (@?= Nothing)
        , testCase "a definite bitmaps map" $
            decodeMsgLeiosBlockTxsRequestViaLeiosFetchCodec
              (definite [txsRequestKey, somePoint, definiteMap [bitmap0]])
              >>= (@?= Just (fst onePoint, [(0, 1)]))
        , testCase "an indefinite bitmaps map" $
            decodeMsgLeiosBlockTxsRequestViaLeiosFetchCodec
              (definite [txsRequestKey, somePoint, indefiniteMap [bitmap0]])
              >>= (@?= Just (fst onePoint, [(0, 1)]))
        , testCase "rejects indefinite bitmaps past the bound" $
            decodeMsgLeiosBlockTxsRequestViaLeiosFetchCodec
              (definite [txsRequestKey, somePoint, indefiniteMap [bitmap0, bitmap1]])
              >>= (@?= Nothing)
        ]
    , testGroup
        "the codec refuses a hash that is not 32 bytes"
        [ testCase "a tx hash of 31 bytes" $
            decodeMsgLeiosBlockViaLeiosFetchCodec (definite [CBOR.encodeWord 2, ebReferencing 31])
              >>= (@?= Nothing)
        , testCase "a tx hash of 33 bytes" $
            decodeMsgLeiosBlockViaLeiosFetchCodec (definite [CBOR.encodeWord 2, ebReferencing 33])
              >>= (@?= Nothing)
        , testCase "an endorser block hash of 31 bytes" $
            decodeMsgLeiosBlockRequestViaLeiosFetchCodec (definite [CBOR.encodeWord 1, pointOf 31])
              >>= (@?= Nothing)
        , testCase "an endorser block hash of 33 bytes" $
            decodeMsgLeiosBlockRequestViaLeiosFetchCodec (definite [CBOR.encodeWord 1, pointOf 33])
              >>= (@?= Nothing)
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
        decodeMsgLeiosBlockViaLeiosFetchCodec msg >>= (@?= Nothing)
    ]

-- | The codec accepts a 'MsgLeiosBlock' of an endorser block of
-- 'maxEndorserBlockBytesSize' in this list encoding, and the message fits
-- 'byteLimitsLeiosFetch'.
fitsByteLimit :: ([CBOR.Encoding] -> CBOR.Encoding) -> Assertion
fitsByteLimit list = do
  BS.length (CBOR.toStrictByteString eb) @?= maxSize
  decoded <- decodeMsgLeiosBlockViaLeiosFetchCodec msg
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

-- | The codec slices exactly the endorser block's bytes out of a
-- 'MsgLeiosBlock', no matter how the message's
-- @[2, { + (bytes .size 32) => (uint .size 4) }]@ is encoded. That slicing is
-- the codec's own work, whereas decoding the references is the ledger's. So the
-- decoded endorser block names those references, and keeps, sizes and hashes
-- exactly the bytes of the map.
--
-- The encodings vary whether the list and the map are definite or indefinite,
-- whether each hash is definite or in chunks, and the width of every head.
--
-- TODO Maybe upstream some of this test and/or reuse some upstream components.
-- It's good to be self-contained, focused, and exhaustive, but that's in
-- tension with reusing the relevant tools.
prop_slicesEndorserBlockFromMessage :: Property
prop_slicesEndorserBlockFromMessage =
  forAllShrink genMsgRecipe shrinkMsgRecipe $ \recipe@(MsgRecipe _ _ ebRecipe) -> ioProperty $ do
    let bytes = ebRecipeBytes ebRecipe
    decoded <- decodeMsgLeiosBlockViaLeiosFetchCodec (CBOR.encodePreEncoded (msgRecipeBytes recipe))
    pure $ case decoded of
      Nothing -> counterexample "the codec rejects the message" False
      Just eb ->
        conjoin
          [ counterexample "references" $ endorserBlockReferences eb === ebRecipeReferences ebRecipe
          , counterexample "bytes" $ CBOR.toStrictByteString (encodeEndorserBlock eb) === bytes
          , counterexample "size" $ encodeEndorserBlockSize eb === fromIntegral (BS.length bytes)
          , counterexample "hash" $ endorserBlockHash eb === castHash (hashWith @HASH id bytes)
          ]

-- | An encoding of a 'MsgLeiosBlock': the list around it, the width of the head
-- of its first item, @2@, and its endorser block.
data MsgRecipe = MsgRecipe Framing ArgBytes EbBytesRecipe
  deriving Show

-- | An encoding of an endorser block's references.
data EbBytesRecipe = EbBytesRecipe Framing [RefRecipe]
  deriving Show

-- | A list or map: definite, its head of at least this width, or indefinite.
data Framing = Definite ArgBytes | Indefinite
  deriving Show

-- | A reference: its tx hash with the framing of that byte string, and its tx
-- size with the width of that head.
data RefRecipe = RefRecipe (TxHash, BytesFraming) (BytesSize, ArgBytes)
  deriving Show

-- | A byte string: definite, its head of at least this width, or indefinite,
-- in chunks of these lengths, each with its head of at least this width.
--
-- TODO Dijkstra's decoders accept indefinite-length byte strings, but that
-- support will be revoked before Dijkstra ships. This test will then fail on
-- 'Chunks', and we deliberately leave it to whoever integrates that ledger
-- change to remove 'Chunks' and its branches from this recipe.
data BytesFraming = DefiniteBytes ArgBytes | Chunks [(Word16, ArgBytes)]
  deriving Show

-- | The fewest bytes a CBOR head spends on its argument after its first byte.
-- It spends more if the argument needs them.
data ArgBytes = Arg0 | Arg1 | Arg2 | Arg4 | Arg8
  deriving (Show, Eq, Ord, Enum, Bounded)

genMsgRecipe :: Gen MsgRecipe
genMsgRecipe = MsgRecipe <$> genFraming <*> genArgBytes <*> genEbBytesRecipe
 where
  genEbBytesRecipe = do
    hashes <- nubOrd <$> listOf1 (vectorOf 32 arbitrary)
    EbBytesRecipe <$> genFraming <*> traverse genRef hashes

  genRef hash =
    RefRecipe
      <$> ((,) (UnsafeHash (SBS.pack hash)) <$> genBytesFraming)
      <*> ((,) <$> genTxSize <*> genArgBytes)

  genTxSize :: Gen Word32
  genTxSize = oneof [choose (0, 23), choose (24, 255), choose (256, 65535), arbitrary]

  genFraming = oneof [Definite <$> genArgBytes, pure Indefinite]

  genBytesFraming = oneof [DefiniteBytes <$> genArgBytes, Chunks <$> genChunks]

  -- The 32 bytes of a hash, cut at some points along them. Cuts may coincide
  -- or fall at either end, leaving empty chunks.
  genChunks = do
    cuts <- sort <$> listOf (choose (0, 32))
    traverse (\len -> (,) len <$> genArgBytes) $ zipWith (-) (cuts <> [32]) (0 : cuts)

  genArgBytes = elements [minBound .. maxBound]

-- | Drop references, but keep one, since the codec rejects an empty map; shrink
-- each tx size; narrow each head; make the list, the map or a hash definite;
-- and join adjacent chunks. The hashes stay as they are, so they stay
-- distinct.
shrinkMsgRecipe :: MsgRecipe -> [MsgRecipe]
shrinkMsgRecipe (MsgRecipe framing width eb) =
  [MsgRecipe framing' width eb | framing' <- shrinkFraming framing]
    <> [MsgRecipe framing width' eb | width' <- shrinkArgBytes width]
    <> [MsgRecipe framing width eb' | eb' <- shrinkEb eb]
 where
  shrinkEb (EbBytesRecipe ebFraming refs) =
    [EbBytesRecipe ebFraming' refs | ebFraming' <- shrinkFraming ebFraming]
      <> [EbBytesRecipe ebFraming refs' | refs' <- shrinkList shrinkRef refs, not (null refs')]

  shrinkRef (RefRecipe (hash, hashFraming) (size, sizeWidth)) =
    [RefRecipe (hash, hashFraming') (size, sizeWidth) | hashFraming' <- shrinkBytesFraming hashFraming]
      <> [RefRecipe (hash, hashFraming) (size', sizeWidth) | size' <- shrink size]
      <> [RefRecipe (hash, hashFraming) (size, sizeWidth') | sizeWidth' <- shrinkArgBytes sizeWidth]

  shrinkFraming = \case
    Indefinite -> [Definite minBound]
    Definite w -> Definite <$> shrinkArgBytes w

  -- A definite byte string can only narrow its head. Chunks can become the
  -- definite byte string with the narrowest head, join two neighbours into one
  -- chunk with the narrower of their heads, or narrow one chunk's head.
  shrinkBytesFraming = \case
    DefiniteBytes w -> DefiniteBytes <$> shrinkArgBytes w
    Chunks chunks ->
      DefiniteBytes minBound
        : [ Chunks (pre <> [(len1 + len2, min w1 w2)] <> post)
          | (pre, (len1, w1) : (len2, w2) : post) <- splits chunks
          ]
          <> [ Chunks (pre <> [(len, w')] <> post)
             | (pre, (len, w) : post) <- splits chunks
             , w' <- shrinkArgBytes w
             ]

  -- Every way to cut the list in two.
  splits xs = zip (inits xs) (tails xs)

  shrinkArgBytes w = takeWhile (< w) [minBound .. w]

msgRecipeBytes :: MsgRecipe -> BS.ByteString
msgRecipeBytes (MsgRecipe framing width eb) =
  framedBytes 4 framing 2 $ headBytes 0 width 2 <> ebRecipeBytes eb

ebRecipeBytes :: EbBytesRecipe -> BS.ByteString
ebRecipeBytes (EbBytesRecipe framing refs) =
  framedBytes 5 framing (length refs) $ foldMap refBytes refs
 where
  refBytes (RefRecipe (hash, hashFraming) (size, sizeWidth)) =
    hashBytes hashFraming (hashToBytes hash) <> headBytes 0 sizeWidth (fromIntegral size)

  hashBytes hashFraming bytes = case hashFraming of
    DefiniteBytes w -> headBytes 2 w (fromIntegral (BS.length bytes)) <> bytes
    Chunks chunks -> BS.singleton 0x5f <> chunksBytes bytes chunks <> BS.singleton 0xff

  chunksBytes bytes = \case
    [] -> BS.empty
    (len, w) : chunks ->
      let (chunk, rest) = BS.splitAt (fromIntegral len) bytes
       in headBytes 2 w (fromIntegral len) <> chunk <> chunksBytes rest chunks

ebRecipeReferences :: EbBytesRecipe -> [(TxHash, BytesSize)]
ebRecipeReferences (EbBytesRecipe _ refs) = [(hash, size) | RefRecipe (hash, _) (size, _) <- refs]

-- | A list or map of this major type and number of entries, around the bytes of
-- those entries.
framedBytes :: Word8 -> Framing -> Int -> BS.ByteString -> BS.ByteString
framedBytes major framing n entries = case framing of
  Definite width -> headBytes major width (fromIntegral n) <> entries
  Indefinite -> BS.singleton (major `shiftL` 5 .|. 31) <> entries <> BS.singleton 0xff

-- | A CBOR head of this major type and argument.
headBytes :: Word8 -> ArgBytes -> Word64 -> BS.ByteString
headBytes major width n = case width of
  Arg0
    | n < 24 -> BS.singleton (major `shiftL` 5 .|. fromIntegral n)
    | otherwise -> headBytes major Arg1 n
  Arg1
    | n < 0x100 -> argumentIn 24 1
    | otherwise -> headBytes major Arg2 n
  Arg2
    | n < 0x10000 -> argumentIn 25 2
    | otherwise -> headBytes major Arg4 n
  Arg4
    | n < 0x100000000 -> argumentIn 26 4
    | otherwise -> headBytes major Arg8 n
  Arg8 -> argumentIn 27 8
 where
  argumentIn info len =
    BS.pack $
      (major `shiftL` 5 .|. info) : [fromIntegral (n `shiftR` (8 * i)) | i <- [len - 1, len - 2 .. 0]]

-- | Decode a message with a codec that admits at most two txs.
decodeViaLeiosFetchCodec ::
  forall (st :: LeiosFetch LeiosPoint EndorserBlock LeiosTx).
  ActiveState st =>
  StateToken st ->
  CBOR.Encoding ->
  IO (Maybe (SomeMessage st))
decodeViaLeiosFetchCodec stok enc = do
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

decodeMsgLeiosBlockTxsViaLeiosFetchCodec :: CBOR.Encoding -> IO (Maybe [LeiosTx])
decodeMsgLeiosBlockTxsViaLeiosFetchCodec enc =
  decodeViaLeiosFetchCodec SingBlockTxs enc
    <&> fmap (\case SomeMessage (MsgLeiosBlockTxs txs) -> V.toList txs)

decodeMsgLeiosBlockViaLeiosFetchCodec :: CBOR.Encoding -> IO (Maybe EndorserBlock)
decodeMsgLeiosBlockViaLeiosFetchCodec enc =
  decodeViaLeiosFetchCodec SingBlock enc
    <&> fmap (\case SomeMessage (MsgLeiosBlock eb) -> eb)

-- | Decode a 'MsgLeiosBlockTxsRequest'. The codec's bound of two txs admits one
-- bitmap entry.
decodeMsgLeiosBlockTxsRequestViaLeiosFetchCodec ::
  CBOR.Encoding -> IO (Maybe (LeiosPoint, TxBitmaps))
decodeMsgLeiosBlockTxsRequestViaLeiosFetchCodec enc =
  decodeViaLeiosFetchCodec SingIdle enc <&> \case
    Just (SomeMessage (MsgLeiosBlockTxsRequest p bitmaps)) -> Just (p, bitmaps)
    _ -> Nothing

decodeMsgLeiosBlockRequestViaLeiosFetchCodec :: CBOR.Encoding -> IO (Maybe LeiosPoint)
decodeMsgLeiosBlockRequestViaLeiosFetchCodec enc =
  decodeViaLeiosFetchCodec SingIdle enc <&> \case
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
