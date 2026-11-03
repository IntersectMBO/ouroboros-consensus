-- | Tests of the LeiosTxCache.
module Test.Consensus.Leios.TxCache (tests) where

import qualified Test.Consensus.Leios.TxCache.Optimized as Optimized
import qualified Test.Consensus.Leios.TxCache.Optimized.MutableHashTable as MutableHashTable
import qualified Test.Consensus.Leios.TxCache.Reference as Reference
import Test.Tasty

tests :: TestTree
tests = testGroup "TxCache" [Reference.tests, MutableHashTable.tests, Optimized.tests]
