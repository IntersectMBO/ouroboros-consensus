{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.Ledger.Tables.DiffSeq (tests) where

import           Control.Monad (liftM)
import qualified Data.FingerTree.RootMeasured.Strict as RMFT
import           Data.Map.Diff.Strict (Delta (..), Diff)
import           Data.Map.Diff.Strict.Internal (DeltaHistory (..), Diff (..))
import           Data.Maybe.Strict (StrictMaybe (..))
import           Data.Sequence.NonEmpty (NESeq (..))
import           Data.Typeable
import           Ouroboros.Consensus.Storage.LedgerDB.V1.DiffSeq
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.DiffSeq as DS
import           Test.Consensus.Ledger.Tables.Diff (lawsTestOne)
import           Test.QuickCheck.Classes
import           Test.QuickCheck.Classes.Semigroup.Cancellative
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests = testGroup "Test.Consensus.Ledger.Tables.DiffSeq" [
    lawsTestOne (Proxy @(RootMeasure Key Val)) [
        semigroupLaws
      , monoidLaws
      , leftReductiveLaws
      , rightReductiveLaws
      , leftCancellativeLaws
      , rightCancellativeLaws
      ]
  , lawsTestOne (Proxy @(InternalMeasure Key Val)) [
        semigroupLaws
      , monoidLaws
      ]
  ]

type Key = Small Int
type Val = Small Int

{------------------------------------------------------------------------------
  Diffs
------------------------------------------------------------------------------}

deriving newtype instance (Ord k, Arbitrary k, Arbitrary v)
                       => Arbitrary (Diff k v)

instance (Arbitrary v) => Arbitrary (DeltaHistory v) where
  arbitrary = DeltaHistory <$>
    ((:<||) <$> arbitrary <*> arbitrary)

instance (Arbitrary v) => Arbitrary (Delta v) where
  arbitrary = oneof [
      Insert <$> arbitrary
    , pure Delete
    ]

{-------------------------------------------------------------------------------
  DiffSeq
-------------------------------------------------------------------------------}

instance (RMFT.SuperMeasured vt vi a, Arbitrary a)
      => Arbitrary (RMFT.StrictFingerTree vt vi a) where
  arbitrary = RMFT.fromList <$> arbitrary

instance (Ord k, Arbitrary k, Arbitrary v)
      => Arbitrary (RootMeasure k v) where
  arbitrary = RootMeasure <$> arbitrary <*> arbitrary
                          <*> arbitrary <*> arbitrary

instance Arbitrary (InternalMeasure k v) where
  arbitrary = InternalMeasure <$> arbitrary <*> arbitrary1 <*> arbitrary1

deriving newtype instance Arbitrary DS.Length
deriving newtype instance Arbitrary DS.SlotNoUB
deriving newtype instance Arbitrary DS.SlotNoLB

instance Arbitrary1 StrictMaybe where
  liftArbitrary arb = frequency [(1, return SNothing), (3, liftM SJust arb)]

  liftShrink shr (SJust x) = SNothing : [ SJust x' | x' <- shr x ]
  liftShrink _   SNothing  = []
