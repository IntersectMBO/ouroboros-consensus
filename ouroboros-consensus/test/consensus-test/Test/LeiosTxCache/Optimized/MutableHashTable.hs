{-# LANGUAGE BangPatterns #-}

-- | Model-based test for "LeiosTxCache.Optimized.MutableHashTable": a random sequence of
-- insert\/delete\/lookup, run (purely, in 'ST') against the table and against a
-- 'Data.Map.Strict' oracle, must agree on every lookup and on a final
-- full-domain sweep. This exercises the probing and the backward-shift deletion
-- under churn. The key domain is kept below the capacity so the table never
-- fills.
module Test.LeiosTxCache.Optimized.MutableHashTable (tests) where

import Control.Monad.ST (runST)
import Data.Bits (shiftR, xor)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import qualified LeiosTxCache.Optimized.MutableHashTable as HT
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Arbitrary (..)
  , Property
  , chooseInt
  , oneof
  , testProperty
  , (===)
  )

tests :: TestTree
tests =
  testGroup
    "LeiosTxCache.Optimized.MutableHashTable"
    [ testProperty "agrees with Data.Map under random churn" prop_matchesMap
    ]

shift :: Int
shift = 8 -- capacity 256

domain :: Int
domain = 200 -- distinct keys < capacity, so the table never fills

salt0, salt1 :: Word64
salt0 = 0xD1CED00DFEEDFACE
salt1 = 0x0123456789ABCDEF

data Op = Ins !Int !Word64 | Del !Int | Look !Int
  deriving Show

instance Arbitrary Op where
  arbitrary = do
    k <- chooseInt (0, domain - 1)
    oneof [Ins k <$> arbitrary, pure (Del k), pure (Look k)]

-- | A well-mixed 32-byte key from a domain index.
keyOf :: Int -> HT.Key
keyOf n0 =
  HT.Key (mix (4 * n)) (mix (4 * n + 1)) (mix (4 * n + 2)) (mix (4 * n + 3))
 where
  n = fromIntegral n0 :: Word64
  mix z0 =
    let z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xBF58476D1CE4E5B9
        z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
     in z2 `xor` (z2 `shiftR` 31)

-- | Lookup results in op order, plus a final sweep over the whole domain.
runMutable :: [Op] -> ([Maybe Word64], [Maybe Word64])
runMutable ops = runST $ do
  ht <- HT.new shift salt0 salt1
  looks <- go ht ops
  sweep <- mapM (HT.lookup ht . keyOf) [0 .. domain - 1]
  pure (looks, sweep)
 where
  go _ [] = pure []
  go ht (op : rest) = case op of
    Ins k v -> HT.insert ht (keyOf k) v >> go ht rest
    Del k -> HT.delete ht (keyOf k) >> go ht rest
    Look k -> (:) <$> HT.lookup ht (keyOf k) <*> go ht rest

runModel :: [Op] -> ([Maybe Word64], [Maybe Word64])
runModel ops = (looks, sweep)
 where
  (looks, final) = go ops Map.empty
  sweep = [Map.lookup (keyOf i) final | i <- [0 .. domain - 1]]
  go [] m = ([], m)
  go (op : rest) m = case op of
    Ins k v -> go rest (Map.insert (keyOf k) v m)
    Del k -> go rest (Map.delete (keyOf k) m)
    Look k -> let (rs, m') = go rest m in (Map.lookup (keyOf k) m : rs, m')

prop_matchesMap :: [Op] -> Property
prop_matchesMap ops = runMutable ops === runModel ops
