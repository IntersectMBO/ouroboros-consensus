{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Util.ChainUpdates.Tests (tests) where

import           Cardano.Ledger.BaseTypes (knownNonZeroBounded)
import           Ouroboros.Consensus.Config
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.ChainUpdates

tests :: TestTree
tests = testGroup "Test.Util.ChainUpdates"
    [ testProperty "genChainUpdates" $ prop_genChainUpdates k updatesToGenerate
    ]
  where
    k = SecurityParam $ knownNonZeroBounded @3
    updatesToGenerate = 100
