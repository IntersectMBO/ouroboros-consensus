module Test.Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.SlotDataPoint (roundtripJSON) where

import           Cardano.Slotting.Slot (SlotNo (SlotNo))
import           Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.SlotDataPoint
import qualified Data.Text as Text
import qualified Test.Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.JSONRoundtrip as JSONRoundtrip
import qualified Test.QuickCheck.Unicode as Unicode
import           Test.Tasty (TestTree)
import           Test.Tasty.QuickCheck (Arbitrary (arbitrary), listOf,
                     testProperty)
import qualified Text.Builder as Builder

roundtripJSON :: TestTree
roundtripJSON =
        testProperty "JSON encoding roundtrips"
      $ JSONRoundtrip.test . unTestSlotDataPoint

newtype TestSlotDataPoint = TestSlotDataPoint { unTestSlotDataPoint :: SlotDataPoint }
  deriving (Eq)

instance Show TestSlotDataPoint where
  show = show . unTestSlotDataPoint

instance Arbitrary TestSlotDataPoint where
  arbitrary = TestSlotDataPoint <$> arbitrarySlotDataPoint
    where
      arbitrarySlotDataPoint =
              SlotDataPoint
          <$> arbitrarySlot
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitraryBlockStats
        where
          arbitrarySlot = SlotNo <$> arbitrary
          arbitraryBlockStats = BlockStats <$> listOf arbitraryBuilder
            where
              arbitraryBuilder = Builder.text . Text.pack <$> Unicode.string
