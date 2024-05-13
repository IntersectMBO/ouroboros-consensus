{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Cardano.SupportedNetworkProtocolVersion (tests) where

import           Data.Proxy
import           Ouroboros.Consensus.Cardano
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Util.SupportedNetworkProtocolVersion

tests :: TestTree
tests =
      testCase "Cardano exhaustive network protocol versions"
    $ exhaustiveSupportedNetworkProtocolVersions
        (Proxy @(CardanoBlock StandardCrypto))
