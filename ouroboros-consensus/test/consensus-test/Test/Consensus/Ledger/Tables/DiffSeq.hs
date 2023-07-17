{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.Ledger.Tables.DiffSeq (tests) where

import           Control.Monad (liftM)
import qualified Data.FingerTree.RootMeasured.Strict as RMFT
import           Data.Map.Diff.Strict (Delta (..), Diff)
import           Data.Map.Diff.Strict.Internal (DeltaHistory (..), Diff (..))
import           Data.Maybe.Strict (StrictMaybe (..))
import           Data.Sequence.NonEmpty (NESeq (..))
import           Data.Typeable
import           Ouroboros.Consensus.Ledger.Tables.DiffSeq
import qualified Ouroboros.Consensus.Ledger.Tables.DiffSeq as DS
import           Test.QuickCheck.Classes
import           Test.QuickCheck.Classes.Semigroup.Cancellative
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests = testGroup "DiffSeq" [
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
  Running laws in test trees
------------------------------------------------------------------------------}

lawsTest :: Laws -> TestTree
lawsTest Laws{lawsTypeclass, lawsProperties} = testGroup lawsTypeclass $
    fmap (uncurry testProperty) lawsProperties

lawsTestOne :: Typeable a => Proxy a -> [Proxy a -> Laws] -> TestTree
lawsTestOne p tts =
    testGroup (show $ typeOf p) (fmap (\f -> lawsTest $ f p) tts)

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
    , Delete <$> arbitrary
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
  arbitrary = InternalMeasure <$> arbitrary <*> arbitrary <*> arbitrary

deriving newtype instance Arbitrary DS.Length
deriving newtype instance Arbitrary DS.SlotNoUB
deriving newtype instance Arbitrary DS.SlotNoLB

instance Arbitrary1 StrictMaybe where
  liftArbitrary arb = frequency [(1, return SNothing), (3, liftM SJust arb)]

  liftShrink shr (SJust x) = SNothing : [ SJust x' | x' <- shr x ]
  liftShrink _   SNothing  = []
