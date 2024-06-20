{-# LANGUAGE CPP #-}

module Test.Consensus.Storage (tests) where

import qualified Test.Consensus.Storage.ChainDB as ChainDB
import qualified Test.Consensus.Storage.ImmutableDB as ImmutableDB
import qualified Test.Consensus.Storage.LedgerDB as LedgerDB
import qualified Test.Consensus.Storage.VolatileDB as VolatileDB
import           Test.Tasty (TestTree, testGroup)

--
-- The list of all tests
--

tests :: TestTree
tests = testGroup "Storage"
    [ ImmutableDB.tests
    , VolatileDB.tests
    , LedgerDB.tests
    , ChainDB.tests
    ]
