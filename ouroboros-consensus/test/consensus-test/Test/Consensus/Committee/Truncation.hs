module Test.Consensus.Committee.Truncation (tests) where

import qualified Test.Consensus.Committee.Truncation.Tests as Impl
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "Truncation" [Impl.tests]
