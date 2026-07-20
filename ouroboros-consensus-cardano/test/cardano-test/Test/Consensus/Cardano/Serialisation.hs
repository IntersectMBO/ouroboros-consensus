{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.Cardano.Serialisation (tests) where

import Cardano.Ledger.Binary.Plain (DecoderError)
import Codec.CBOR.Read (deserialiseFromBytes)
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString.Lazy as Lazy
import Data.Constraint
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Byron.Ledger
import Ouroboros.Consensus.Byron.Node ()
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.HardFork.Combinator.Block
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Node ()
import Ouroboros.Consensus.Storage.Serialisation
import Ouroboros.Network.Block (Serialised (..))
import Paths_ouroboros_consensus (getDataFileName)
import Test.Consensus.Byron.Generators (epochSlots)
import qualified Test.Consensus.Cardano.Examples as Cardano.Examples
import Test.Consensus.Cardano.Generators ()
import Test.Tasty
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (Property, testProperty, (===))
import Test.Util.Orphans.Arbitrary ()
import Test.Util.Serialisation.Roundtrip
import Test.Util.Serialisation.TxWireSize

tests :: TestTree
tests =
  testGroup
    "Cardano"
    [ testGroup "Examples roundtrip" $
        examplesRoundtrip Cardano.Examples.codecConfig Cardano.Examples.examples
    , testGroup
        "GenTx.txWireSize"
        [ testProperty "txSubmission" $ prop_txWireSize_txSubmission testCodecCfg
        , testProperty "tight" $ prop_txWireSize (const Nothing) testCodecCfg
        ]
    , roundtrip_all_skipping
        result
        testCodecCfg
        dictNestedHdr
        -- We would want to use this instead, but the generated blocks
        -- do not quite validate yet or sometimes they are not
        -- entirely coherent, so for now this is commented out.
        --
        -- It is also the case that some (conway in particular) blocks take a
        -- very long time to validate or consume too much memory.
        --
        -- ( Just $
        --     CDDLsForNodeToNode ("ntnblock.cddl", "serialisedCardanoBlock") ("ntnheader.cddl", "header")
        -- )
        Nothing
    , testProperty "BinaryBlockInfo sanity check" prop_CardanoBinaryBlockInfo
    , test_originalBlock67555_roundtripsByteIdentical
    ]
 where
  -- See https://github.com/IntersectMBO/cardano-ledger/issues/3800
  result "roundtrip Result" = DoNotCheckCBORValidity
  result _ = CheckCBORValidity

testCodecCfg :: CardanoCodecConfig StandardCrypto
testCodecCfg =
  CardanoCodecConfig
    (ByronCodecConfig epochSlots)
    ShelleyCodecConfig
    ShelleyCodecConfig
    ShelleyCodecConfig
    ShelleyCodecConfig
    ShelleyCodecConfig
    ShelleyCodecConfig
    ShelleyCodecConfig

dictNestedHdr ::
  forall a.
  NestedCtxt_ (CardanoBlock StandardCrypto) Header a ->
  Dict (Eq a, Show a)
dictNestedHdr = \case
  NCZ (CtxtByronBoundary{}) -> Dict
  NCZ (CtxtByronRegular{}) -> Dict
  NCS (NCZ CtxtShelley) -> Dict
  NCS (NCS (NCZ CtxtShelley)) -> Dict
  NCS (NCS (NCS (NCZ CtxtShelley))) -> Dict
  NCS (NCS (NCS (NCS (NCZ CtxtShelley)))) -> Dict
  NCS (NCS (NCS (NCS (NCS (NCZ CtxtShelley))))) -> Dict
  NCS (NCS (NCS (NCS (NCS (NCS (NCZ CtxtShelley)))))) -> Dict
  NCS (NCS (NCS (NCS (NCS (NCS (NCS (NCZ CtxtShelley))))))) -> Dict

{-------------------------------------------------------------------------------
  BinaryBlockInfo
-------------------------------------------------------------------------------}

prop_CardanoBinaryBlockInfo :: CardanoBlock StandardCrypto -> Property
prop_CardanoBinaryBlockInfo blk =
  encodedNestedHeader === extractedHeader
 where
  BinaryBlockInfo{headerOffset, headerSize} =
    getBinaryBlockInfo blk

  extractedHeader :: Lazy.ByteString
  extractedHeader =
    Lazy.take (fromIntegral headerSize) $
      Lazy.drop (fromIntegral headerOffset) $
        CBOR.toLazyByteString (encodeDisk testCodecCfg blk)

  encodedNestedHeader :: Lazy.ByteString
  encodedNestedHeader = case encodeDepPair testCodecCfg (unnest (getHeader blk)) of
    GenDepPair _ (Serialised bytes) -> bytes

{-------------------------------------------------------------------------------
  Byte-preserving decode/encode of a real Dijkstra block

  Regression test for the ShelleyBlock wire-bytes memo hotfix.

  Ledger's 'EncCBOR' instance for 'SL.Block' re-serializes the body structure
  and can produce bytes that differ from what the block was originally decoded
  from — e.g. an outer body list re-emitted as definite when the producer used
  indefinite framing. Once a haskell relay re-encodes on write-to-disk, the
  on-disk body no longer matches the header's committed 'hbBodySize' /
  'hbBodyHash' and validation on reload fails.

  The file is the original CBOR of Dijkstra RB 67555 (header hash
  70c34f39cf63c8fe9c0f645ef1c6ea3edcf6f72944af43d3eaaf3b40d252761e), fetched
  from the pool relays of the block producer that forged it
  (x.zw3rkpool.com:3003 / y.zw3rkpool.com:3003), so its body is untouched by
  any haskell re-encode. Its header declares 'hbBodySize' = 66007 which
  matches its actual serialized body of 66007 bytes; the previously-affected
  code path would have re-encoded the body to 66006 bytes.

  With the memo in place, decode followed by 'encodeDisk' must yield the exact
  input bytes.
-------------------------------------------------------------------------------}

test_originalBlock67555_roundtripsByteIdentical :: TestTree
test_originalBlock67555_roundtripsByteIdentical =
  testCase "Dijkstra RB 67555 (70c34f39…) round-trips byte-identical" $ do
    path <- getDataFileName "ouroboros-consensus-cardano/test/cardano-test/data/block-67555-70c34f39-original.cbor"
    bytes <- Lazy.readFile path
    case deserialiseFromBytes (decodeDisk testCodecCfg) bytes of
      Left err -> assertFailure $ "CBOR decode failed: " <> show err
      Right (rest, mkBlk) -> do
        assertBool "no trailing bytes after block" (Lazy.null rest)
        let annotated :: Either DecoderError (CardanoBlock StandardCrypto)
            annotated = mkBlk bytes
        case annotated of
          Left err -> assertFailure $ "block annotator failed: " <> show err
          Right blk ->
            CBOR.toLazyByteString (encodeDisk testCodecCfg blk) @?= bytes
