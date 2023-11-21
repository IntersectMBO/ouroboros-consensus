module Test.Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.SlotDataPoint (roundtripJSON) where

import           Cardano.Slotting.Slot (SlotNo (SlotNo))
import           Cardano.Tools.DBAnalyser.Analysis.BenchmarkLedgerOps.SlotDataPoint
import           Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Test.QuickCheck.Unicode as Unicode
import           Test.Tasty (TestTree)
import           Test.Tasty.QuickCheck (Arbitrary (arbitrary), Property, listOf,
                     testProperty, withMaxSuccess, (===))
import qualified Text.Builder as Builder

roundtripJSON :: TestTree
roundtripJSON =
    testProperty "JSON encoding roundtrips" testRoundtrip
  where
    testRoundtrip :: TestSlotDataPoint -> Property
    testRoundtrip (TestSlotDataPoint dataPoint) =
        withMaxSuccess 1000 $ -- This takes 150 milliseconds in a modern machine
          Aeson.eitherDecode (Aeson.encode dataPoint) === Right dataPoint

newtype TestSlotDataPoint = TestSlotDataPoint SlotDataPoint
  deriving (Eq)

instance Show TestSlotDataPoint where
  show (TestSlotDataPoint dp) = show dp

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
