{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.UtxoDiff (UtxoLike (..), TwoUtxoLike (..)) where

import           Data.Foldable as F
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Ouroboros.Consensus.Ledger.Tables.UtxoDiff
import           Test.QuickCheck.Arbitrary as QC
import           Test.Tasty.QuickCheck

-- | A sequence of diffs and an initial map, where each of them follow the UTxO
-- property.
data UtxoLike k v = UtxoLike (Map k v) [UtxoDiff k v]
  deriving Show

data TwoUtxoLike k v = TwoUtxoLike (UtxoDiff k v) (UtxoDiff k v)
  deriving Show

iterateM :: Monad m => Int -> (a -> m a) -> a -> m [a]
iterateM 0 _ _ = pure []
iterateM n f x = do
    x' <- f x
    (x':) <$> iterateM (n - 1) f x'

genUtxoLikeDiff :: (Ord k, Arbitrary k, Arbitrary v) => Map k v -> Gen [UtxoDiff k v]
genUtxoLikeDiff m = do
  -- Do produce some diffs
  n <- (getPositive <$> arbitrary) `suchThat` (>=2)
  (\(_, _, d) -> reverse d) . last <$> iterateM n (\(utx, seen, dfs) -> do
     -- delete some
     toDelete <-  Map.fromList <$> sublistOf (Map.toList utx) `suchThat` (not . F.null)
     let utx' = utx `Map.withoutKeys` (Map.keysSet toDelete)
     -- add some
     toAdd <- arbitrary `suchThat`
       (\mmm -> not (Map.null mmm) && all (`Set.notMember` seen) (Map.keys mmm))
     let utx'' = utx' `Map.union` toAdd
     -- record created
     let seen' = seen `Set.union` Map.keysSet toAdd
     pure (utx'', seen', UtxoDiff toDelete toAdd : dfs)
     ) (m, Map.keysSet m, [])

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (UtxoLike k v) where
  arbitrary = do
    m <- arbitrary `suchThat` (not . Map.null)
    UtxoLike m <$> genUtxoLikeDiff m

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (TwoUtxoLike k v) where
  arbitrary = do
    utxolike <- arbitrary
    case utxolike of
      UtxoLike _ (a:b:_) -> pure $ TwoUtxoLike a b
      -- unreachable, we always produce >2 diffs
      _ -> arbitrary

-- Notice that generating a sequence of these might not preserve the UTxO property
instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (UtxoDiff k v) where

  arbitrary = do
    UtxoLike _ diffs <- arbitrary
    elements diffs

  shrink (UtxoDiff m1 m2) =
    [ UtxoDiff m1' m2' | (m1', m2') <- shrink (m1, m2) ]
