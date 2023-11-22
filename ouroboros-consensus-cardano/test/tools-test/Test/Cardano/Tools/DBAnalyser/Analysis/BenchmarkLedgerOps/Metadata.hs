module Test.Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.Metadata (roundtripJSON) where

import           Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.Metadata
import qualified Test.Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.JSONRoundtrip as JSONRoundtrip
import           Test.Tasty (TestTree)
import           Test.Tasty.QuickCheck (Arbitrary (arbitrary), testProperty)

roundtripJSON :: TestTree
roundtripJSON =
        testProperty "JSON encoding roundtrips"
      $ JSONRoundtrip.test . unTestMetadata

newtype TestMetadata = TestMetadata { unTestMetadata :: Metadata }
  deriving (Eq)

instance Show TestMetadata where
  show (TestMetadata metadata) = show metadata

instance Arbitrary TestMetadata where
  arbitrary = TestMetadata <$> arbitraryMetadata
    where
      arbitraryMetadata =
              Metadata
          <$> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
