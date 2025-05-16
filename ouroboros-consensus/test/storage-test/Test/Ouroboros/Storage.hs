{-# LANGUAGE CPP #-}

module Test.Ouroboros.Storage (tests) where

import Test.Ouroboros.Storage.ChainDB qualified as ChainDB
import Test.Ouroboros.Storage.ImmutableDB qualified as ImmutableDB
import Test.Ouroboros.Storage.LedgerDB qualified as LedgerDB
import Test.Ouroboros.Storage.VolatileDB qualified as VolatileDB
import Test.Tasty (TestTree, testGroup)

--
-- The list of all tests
--

tests :: TestTree
tests =
  testGroup
    "Storage"
    [ ImmutableDB.tests
    , VolatileDB.tests
    , LedgerDB.tests
    , ChainDB.tests
    ]
