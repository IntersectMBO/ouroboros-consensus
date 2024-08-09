{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Test.Consensus.Ledger.Tables.Diff (tests) where

import           Data.Foldable as F
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semigroup.Cancellative
import qualified Data.Set as Set
import           Ouroboros.Consensus.Ledger.Tables.UtxoDiff
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck hiding (Negative, Positive)
import           Test.Util.UtxoDiff

tests :: TestTree
tests = testGroup "Test.Consensus.Ledger.Tables.Diff" [
      testGroup "Applying diffs" [
          testProperty "prop_diffThenApply" prop_diffThenApply
        , testProperty "prop_applyMempty" prop_applyMempty
        , testProperty "prop_applySum" prop_applySum
        , testProperty "prop_applyDiffNumInsertsDeletes"  prop_applyDiffNumInsertsDeletes
        , testProperty "prop_applyDiffNumInsertsDeletesExact" prop_applyDiffNumInsertsDeletesExact
        ]
    , testGroup "reductive and cancellative" [
      testProperty "prop_AllUtxo" prop_AllUtxo
      , testProperty "prop_leftReductive" prop_leftReductive
      , testProperty "prop_rightReductive" prop_rightReductive
      , testProperty "prop_leftCancellative" prop_leftCancellative
      , testProperty "prop_rightCancellative" prop_rightCancellative
    ]
    ]

{------------------------------------------------------------------------------
  Applying diffs
------------------------------------------------------------------------------}

type K = Small Int
type V = Small Int

data TwoUtxos k v = TwoUtxos (Map k v) (Map k v)
  deriving Show

instance (Ord k, Arbitrary k, Arbitrary v, Eq v) => Arbitrary (TwoUtxos k v) where
  arbitrary = do
    m1 <- arbitrary
    TwoUtxos m1 <$> arbitrary `suchThat` (\m2 ->
      -- They cannot have the same key with different values
      m1 `Map.intersection` m2 == m2 `Map.intersection` m1
      )

-- | Applying a diff computed from a source and target value should
-- produce the target value.
prop_diffThenApply :: TwoUtxos K V -> Property
prop_diffThenApply (TwoUtxos x y) = applyUtxoDiff x (diff x y) === y

-- | Applying an empty diff is the identity function.
prop_applyMempty :: Map K V -> Property
prop_applyMempty x = applyUtxoDiff x empty === x

prop_Utxo :: UtxoDiff K V -> Bool
prop_Utxo (UtxoDiff dels ins) =
  Map.keysSet dels `Set.disjoint` Map.keysSet ins

prop_AllUtxo :: UtxoLike K V -> Property
prop_AllUtxo (UtxoLike _ diffs) =
  forAll (elements diffs) prop_Utxo
    .&&. forAll (sublistOf diffs) (\x ->
      case mconcat $ map AUtxoDiff x of
        NotAUtxoDiff -> counterexample (unlines ["Violation of the UTxO Property"
                                                , show x
                                                ]) False
        AUtxoDiff d -> property $ prop_Utxo d
        )

prop_leftReductive :: TwoUtxoLike K V -> Property
prop_leftReductive (TwoUtxoLike a b) =
  case AUtxoDiff a <> AUtxoDiff b of
    NotAUtxoDiff -> counterexample ("Violation of the UTxO property!") $ property False
    AUtxoDiff c -> counterexample ("Composed: " <> show c) $
      maybe (AUtxoDiff c) (AUtxoDiff a <>) (stripPrefix (AUtxoDiff a) (AUtxoDiff c)) === AUtxoDiff c

prop_rightReductive :: TwoUtxoLike K V -> Property
prop_rightReductive (TwoUtxoLike a b) =
  case AUtxoDiff a <> AUtxoDiff b of
    NotAUtxoDiff -> counterexample ("Violation of the UTxO property!") $ property False
    AUtxoDiff c -> counterexample ("Composed: " <> show c) $
     maybe (AUtxoDiff a) (<> AUtxoDiff b) (stripSuffix (AUtxoDiff b) (AUtxoDiff c)) === AUtxoDiff c

prop_leftCancellative :: TwoUtxoLike K V -> Property
prop_leftCancellative (TwoUtxoLike a b) =
  stripPrefix (AUtxoDiff a) (AUtxoDiff a <> AUtxoDiff b) === Just (AUtxoDiff b)

prop_rightCancellative :: TwoUtxoLike K V -> Property
prop_rightCancellative (TwoUtxoLike a b) =
  stripSuffix (AUtxoDiff b) (AUtxoDiff a <> AUtxoDiff b) === Just (AUtxoDiff a)

-- | Applying a sum of diffs is equivalent to applying each @'UtxoDiff'@
-- separately (in order).
prop_applySum :: UtxoLike K V -> Property
prop_applySum (UtxoLike x ds) =
  case foldMap' id $ map AUtxoDiff ds of
    NotAUtxoDiff -> counterexample "Violation of the UtxO property!" False
    AUtxoDiff d  -> F.foldl' applyUtxoDiff x ds === applyUtxoDiff x d

-- | Applying a @'UtxoDiff' d@ to a @'Map' m@ increases the size of @m@ by exactly
-- @numInserts d - numDeletes d@ if @d@ inserts only new keys and @d@ only
-- deletes existing keys.
--
-- Diffing two 'Map's that have disjoint keysets creates exactly a diff @d@ that
-- only inserts new keys and deletes existing keys.
prop_applyDiffNumInsertsDeletesExact :: Map K V -> Map K V -> Property
prop_applyDiffNumInsertsDeletesExact m1 m2 =
    Map.keysSet m1 `Set.disjoint` Map.keysSet m2 ==>
      Map.size (applyUtxoDiff m1 d) ===
        Map.size m1 + numInserts d - numDeletes d
  where
    d = diff m1 m2

-- | Applying a @'UtxoDiff' d@ to a @'Map' m@ may increase/decrease the size of @m@
-- up to bounds depending on the number of inserts and deletes in @d@.
--
-- * The size of @m@ may /decrease/ by up to the number of deletes in @d@. This
--   happens if @d@ does not insert any new keys.
-- * The size of @m@ may /increase/ by up to the number of inserts in @d@. This
--   if @d@ does not delete any existing keys.
prop_applyDiffNumInsertsDeletes :: Map K V -> UtxoDiff K V -> Property
prop_applyDiffNumInsertsDeletes m d = property $
    lb <= n' && n' <= ub
  where
    n        = Map.size m
    nInserts = numInserts d
    nDeletes = numDeletes d
    n'  = Map.size (applyUtxoDiff m d)
    lb = n - nDeletes
    ub = n + nInserts
