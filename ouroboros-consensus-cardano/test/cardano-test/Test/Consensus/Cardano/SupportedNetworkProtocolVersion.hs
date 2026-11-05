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
    , testCase "atomic fetch version is absent from ordinary supported versions" $
        assertBool "version 17 advertised without explicit activation" $
          Map.notMember NodeToNodeV_17 ordinary
    , testCase "explicit atomic fetch selection reuses version 16 serialization" $
        Map.lookup NodeToNodeV_17 (withExperimentalAtomicFetchVersion ordinary)
          @?= Map.lookup NodeToNodeV_16 ordinary
    ]
 where
  ordinary = supportedNodeToNodeVersions (Proxy @(CardanoBlock StandardCrypto))
