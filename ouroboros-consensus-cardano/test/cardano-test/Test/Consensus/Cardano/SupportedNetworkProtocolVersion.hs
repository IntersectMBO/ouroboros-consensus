{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Cardano.SupportedNetworkProtocolVersion (tests) where

import qualified Data.Map.Strict as Map
import Data.Proxy
import Ouroboros.Consensus.Cardano
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.SupportedNetworkProtocolVersion

tests :: TestTree
tests =
  testGroup
    "network protocol versions"
    [ testCase "Cardano contiguous network protocol versions" $
        contiguousSupportedNetworkProtocolVersions (Proxy @(CardanoBlock StandardCrypto))
    , testCase "atomic fetch is present in the normal supported map with version 16 serialization" $ do
        assertBool "version 17 missing from supported versions" (Map.member NodeToNodeV_17 ordinary)
        Map.lookup NodeToNodeV_17 ordinary @?= Map.lookup NodeToNodeV_16 ordinary
    , testCase "atomic support preserves the existing Cardano released-version cutoff" $
        fst (latestReleasedNodeVersion (Proxy @(CardanoBlock StandardCrypto))) @?= Just NodeToNodeV_15
    ]
 where
  ordinary = supportedNodeToNodeVersions (Proxy @(CardanoBlock StandardCrypto))
