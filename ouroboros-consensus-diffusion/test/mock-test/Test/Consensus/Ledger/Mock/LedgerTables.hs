{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.Ledger.Mock.LedgerTables (tests) where

import Ouroboros.Consensus.Mock.Ledger
import Ouroboros.Consensus.Protocol.PBFT
import Test.Consensus.Ledger.Mock.Generators ()
import Test.LedgerTables
import Test.Tasty
import Test.Tasty.QuickCheck

type Block = SimpleBlock SimpleMockCrypto (SimplePBftExt SimpleMockCrypto PBftMockCrypto)

tests :: TestTree
tests =
  testGroup
    "LedgerTables"
    [ testProperty "Stowable laws" (prop_stowable_laws @Block)
    , testProperty "HasLedgerTables laws" (prop_hasledgertables_laws @Block)
    ]
