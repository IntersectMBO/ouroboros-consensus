{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.Cardano.Serialisation (tests) where

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
import Test.Consensus.Byron.Generators (epochSlots)
import qualified Test.Consensus.Cardano.Examples as Cardano.Examples
import Test.Consensus.Cardano.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck (Property, testProperty, (===))
import Test.Util.Orphans.Arbitrary ()
import Test.Util.Serialisation.Roundtrip

tests :: TestTree
tests =
  testGroup
    "Cardano"
    [ testGroup "Examples roundtrip" $
        examplesRoundtrip Cardano.Examples.codecConfig Cardano.Examples.examples
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
