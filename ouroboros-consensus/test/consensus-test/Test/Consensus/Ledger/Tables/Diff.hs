{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.Ledger.Tables.Diff (tests) where

import           Data.Foldable as F
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Typeable
import           Ouroboros.Consensus.Ledger.Tables.Diff
import           Test.QuickCheck.Classes
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck hiding (Negative, Positive)

tests :: TestTree
tests = testGroup "Test.Consensus.Ledger.Tables.Diff" [
      testGroup "quickcheck-classes" [
          lawsTestOne (Proxy @(Diff K V)) [
              semigroupLaws
            , monoidLaws
            ]
        ]
    , testGroup "Applying diffs" [
          testProperty "prop_diffThenApply" prop_diffThenApply
        , testProperty "prop_applyMempty" prop_applyMempty
        , testProperty "prop_applySum" prop_applySum
        , testProperty "prop_applyDiffNumInsertsDeletes"  prop_applyDiffNumInsertsDeletes
        , testProperty "prop_applyDiffNumInsertsDeletesExact" prop_applyDiffNumInsertsDeletesExact
        ]
    ]

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
  Applying diffs
------------------------------------------------------------------------------}

type K = Int
type V = Char

-- | Applying a diff computed from a source and target value should
-- produce the target value.
prop_diffThenApply :: Map K V -> Map K V -> Property
prop_diffThenApply x y = applyDiff x (diff x y) === y

-- | Applying an empty diff is the identity function.
prop_applyMempty :: Map K V -> Property
prop_applyMempty x = applyDiff x mempty === x

-- | Applying a sum of diffs is equivalent to applying each @'Diff'@
-- separately (in order).
prop_applySum :: Map K V -> [Diff K V] -> Property
prop_applySum x ds = F.foldl' applyDiff x ds === applyDiff x (foldMap' id ds)

-- | Applying a @'Diff' d@ to a @'Map' m@ increases the size of @m@ by exactly
-- @numInserts d - numDeletes d@ if @d@ inserts only new keys and @d@ only
-- deletes existing keys.
--
-- Diffing two 'Map's that have disjoint keysets creates exactly a diff @d@ that
-- only inserts new keys and deletes existing keys.
prop_applyDiffNumInsertsDeletesExact :: Map K V -> Map K V -> Property
prop_applyDiffNumInsertsDeletesExact m1 m2 =
    Map.keysSet m1 `Set.disjoint` Map.keysSet m2 ==>
      Map.size (applyDiff m1 d) ===
        Map.size m1 + numInserts d - numDeletes d
  where
    d = diff m1 m2

-- | Applying a @'Diff' d@ to a @'Map' m@ may increase/decrease the size of @m@
-- up to bounds depending on the number of inserts and deletes in @d@.
--
-- * The size of @m@ may /decrease/ by up to the number of deletes in @d@. This
--   happens if @d@ does not insert any new keys.
-- * The size of @m@ may /increase/ by up to the number of inserts in @d@. This
--   if @d@ does not delete any existing keys.
prop_applyDiffNumInsertsDeletes :: Map K V -> Diff K V -> Property
prop_applyDiffNumInsertsDeletes m d = property $
    lb <= n' && n' <= ub
  where
    n        = Map.size m
    nInserts = numInserts d
    nDeletes = numDeletes d
    n'  = Map.size (applyDiff m d)
    lb = n - nDeletes
    ub = n + nInserts

{------------------------------------------------------------------------------
  Plain @'Arbitrary'@ instances
------------------------------------------------------------------------------}

deriving newtype instance (Ord k, Arbitrary k, Arbitrary v)
                       => Arbitrary (Diff k v)

instance Arbitrary v => Arbitrary (Delta v) where
  arbitrary = oneof [
      Insert <$> arbitrary
    , pure Delete
    ]
  shrink de = case de of
    Insert x -> Insert <$> shrink x
    Delete   -> []
