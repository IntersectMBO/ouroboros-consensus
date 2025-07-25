{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.Byron.Serialisation (tests) where

import Cardano.Chain.Block (ABlockOrBoundary (..))
import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.Update as CC.Update
import Codec.CBOR.Write (toLazyByteString)
import qualified Data.ByteString.Lazy as Lazy
import Data.Constraint
import Ouroboros.Consensus.Byron.Ledger hiding
  ( byronProtocolVersion
  , byronSoftwareVersion
  )
import Ouroboros.Consensus.Byron.Node
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Node.Serialisation ()
import Ouroboros.Consensus.Storage.Common (BinaryBlockInfo (..))
import qualified Test.Cardano.Chain.Genesis.Dummy as CC
import Test.Consensus.Byron.Generators
import Test.QuickCheck hiding (Result)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Util.Corruption
import Test.Util.Orphans.Arbitrary ()
import Test.Util.Serialisation.Roundtrip

tests :: TestTree
tests =
  testGroup
    "Byron"
    [ roundtrip_all testCodecCfg dictNestedHdr Nothing
    , testProperty "BinaryBlockInfo sanity check" prop_byronBinaryBlockInfo
    , testGroup
        "Integrity"
        [ testProperty "detect corruption in RegularBlock" prop_detectCorruption_RegularBlock
        ]
    ]
 where
  dictNestedHdr :: forall a. NestedCtxt_ ByronBlock Header a -> Dict (Eq a, Show a)
  dictNestedHdr (CtxtByronBoundary _) = Dict
  dictNestedHdr (CtxtByronRegular _) = Dict

{-------------------------------------------------------------------------------
  BinaryBlockInfo
-------------------------------------------------------------------------------}

prop_byronBinaryBlockInfo :: ByronBlock -> Property
prop_byronBinaryBlockInfo blk =
  headerAnnotation === extractedHeader
 where
  BinaryBlockInfo{headerOffset, headerSize} =
    byronBinaryBlockInfo blk

  extractedHeader :: Lazy.ByteString
  extractedHeader =
    Lazy.take (fromIntegral headerSize) $
      Lazy.drop (fromIntegral headerOffset) $
        toLazyByteString (encodeByronBlock blk)

  headerAnnotation :: Lazy.ByteString
  headerAnnotation = Lazy.fromStrict $ case byronBlockRaw blk of
    ABOBBoundary b -> CC.Block.boundaryHeaderAnnotation $ CC.Block.boundaryHeader b
    ABOBBlock b -> CC.Block.headerAnnotation $ CC.Block.blockHeader b

{-------------------------------------------------------------------------------
  Integrity
-------------------------------------------------------------------------------}

-- | Test that we can detect random bitflips in blocks.
--
-- We cannot do this for EBBs, as they are not signed nor have a hash, so we
-- only test with regular blocks.
prop_detectCorruption_RegularBlock :: RegularBlock -> Corruption -> Property
prop_detectCorruption_RegularBlock (RegularBlock blk) =
  detectCorruption
    encodeByronBlock
    (decodeByronBlock epochSlots)
    (verifyBlockIntegrity (configBlock testCfg))
    blk

-- | Matches the values used for the generators.
testCfg :: TopLevelConfig ByronBlock
testCfg = pInfoConfig protocolInfo
 where
  protocolInfo :: ProtocolInfo ByronBlock
  protocolInfo =
    protocolInfoByron $
      ProtocolParamsByron
        { byronGenesis = CC.dummyConfig
        , byronPbftSignatureThreshold = Just (PBftSignatureThreshold 0.5)
        , byronProtocolVersion = CC.Update.ProtocolVersion 1 0 0
        , byronSoftwareVersion = CC.Update.SoftwareVersion (CC.Update.ApplicationName "Cardano Test") 2
        , byronLeaderCredentials = Nothing
        }

-- | Matches the values used for the generators.
testCodecCfg :: CodecConfig ByronBlock
testCodecCfg = configCodec testCfg
