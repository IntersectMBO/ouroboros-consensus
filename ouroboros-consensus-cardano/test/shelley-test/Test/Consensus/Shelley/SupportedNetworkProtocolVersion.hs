{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Shelley.SupportedNetworkProtocolVersion (tests) where

import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.Strict
import Data.Typeable (Typeable)
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Node.NetworkProtocolVersion
  ( SupportedNetworkProtocolVersion
  )
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.SupportedNetworkProtocolVersion

tests :: TestTree
tests =
  testCase "Shelley exhaustive network protocol versions"
    . sequence_
    . hcollapse
    . hcmap
      (Proxy @(And Typeable SupportedNetworkProtocolVersion))
      (K . exhaustiveSupportedNetworkProtocolVersions)
    $ shelleyBlocks
 where
  shelleyBlocks :: NP Proxy (CardanoShelleyEras StandardCrypto)
  shelleyBlocks = hpure Proxy
