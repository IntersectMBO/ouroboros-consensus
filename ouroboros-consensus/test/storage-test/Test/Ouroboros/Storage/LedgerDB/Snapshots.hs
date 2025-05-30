{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Ouroboros.Storage.LedgerDB.Snapshots (tests) where

import Data.Aeson
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import System.FS.CRC
import Test.Tasty
import Test.Tasty.QuickCheck hiding (Success)

tests :: TestTree
tests =
  testGroup
    "Snapshots"
    [ testProperty "SnapshotBackend roundtrips" $ prop_roundtrips @SnapshotBackend
    , testProperty "SnapshotMetadata roundtrips" $ prop_roundtrips @SnapshotMetadata
    ]

prop_roundtrips :: (ToJSON a, FromJSON a, Eq a, Show a) => a -> Property
prop_roundtrips a =
  case fromJSON (toJSON a) of
    Error s -> counterexample s False
    Success r -> r === a

instance Arbitrary SnapshotBackend where
  arbitrary =
    elements
      [ UTxOHDMemSnapshot
      , UTxOHDLMDBSnapshot
      ]

instance Arbitrary SnapshotMetadata where
  arbitrary =
    SnapshotMetadata
      <$> arbitrary
      <*> fmap CRC arbitrary
      <*> arbitrary
