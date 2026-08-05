{-# LANGUAGE BangPatterns #-}

-- | Model-based test for "LeiosTxCache.Optimized.MutableHashTable": a random
-- sequence of insert\/delete\/lookup, run (purely, in 'ST') against the table and
-- against a 'Data.Map.Strict' oracle, must agree on every lookup and on a final
-- full-domain sweep. This exercises the probing and the backward-shift deletion
-- under churn.
--
-- Crucially it runs across a spread of load factors, /up to one slot shy of
-- full/: with a good hash and few keys no probe clusters form, so the interesting
-- code (long probes, wraparound, mid-cluster deletion) is only stressed when the
-- table is dense. Each config pairs a capacity (@2 ^ shift@) with a key domain
-- sized to a target load; the domain is kept below capacity (see 'Config'), so at
-- least one slot always stays free — which open-addressing linear probing needs
-- to terminate, and which the production sizing guarantees anyway.
--
-- The load-factor 'Config' generator and the salt are also used by the handle
-- equivalence test "Test.LeiosTxCache.Optimized", so they are exported here (they
-- are hash-table concepts — capacity, occupancy, SipHash salt) rather than
-- duplicated.
module Test.LeiosTxCache.Optimized.MutableHashTable
  ( tests

    -- * Load-factor fixtures (shared with "Test.LeiosTxCache.Optimized")
  , Config (..)
  , genConfig
  , salt0
  , salt1
  ) where

import Control.Monad.ST (runST)
import Data.Bits (shiftR, xor)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import qualified LeiosTxCache.Optimized.MutableHashTable as HT
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.QuickCheck
  ( Gen
  , Property
  , QuickCheckTests (..)
  , arbitrary
  , chooseInt
  , forAll
  , forAllShrink
  , frequency
  , shrinkList
  , shuffle
  , tabulate
  , testProperty
  , vectorOf
  , (===)
  )

tests :: TestTree
tests =
  testGroup
    "LeiosTxCache.Optimized.MutableHashTable"
    [ adjustOption (\(QuickCheckTests n) -> QuickCheckTests (n * 10)) $
        testProperty "agrees with Data.Map across load factors" prop_matchesMap
    ]

-- | A table capacity (@2 ^ cfgShift@) paired with a key\/tx domain sized to a
-- target load factor.
--
-- INVARIANT: @1 <= cfgDomain <= 2 ^ cfgShift - 1@ — at least one slot stays free.
-- Open-addressing linear probing needs an empty slot to terminate; in particular
-- the backward-shift 'HT.delete' loops forever on a completely full table (the
-- lone hole gets chased around with no gap to stop at). So the sweep reaches one
-- slot shy of full — the densest /supported/ occupancy — never a literally
-- 100%-full table, which production never reaches either (it sizes for ~46%).
data Config = Config
  { cfgShift :: !Int
  , cfgDomain :: !Int
  }
  deriving Show

-- | Pick a capacity within the given (inclusive) @nshift@ range and a domain
-- sized to a target load, biased to include the extremes: a sparse table and an
-- exactly-full one.
genConfig :: (Int, Int) -> Gen Config
genConfig shiftRange = do
  shift <- chooseInt shiftRange
  let cap = 2 ^ shift :: Int
  loadPct <-
    frequency
      [ (1, pure 100) -- exactly full
      , (2, chooseInt (85, 100)) -- near-full
      , (3, chooseInt (10, 100)) -- the whole range
      ]
  pure Config{cfgShift = shift, cfgDomain = max 1 (min (cap - 1) ((cap * loadPct) `div` 100))}

-- | A fixed 128-bit SipHash salt. Production feeds a securely-random pair; the
-- tests use a constant so runs are deterministic.
salt0, salt1 :: Word64
salt0 = 0xD1CED00DFEEDFACE
salt1 = 0x0123456789ABCDEF

data Op = Ins !Int !Word64 | Del !Int | Look !Int
  deriving Show

genOp :: Int -> Gen Op
genOp domain = do
  k <- chooseInt (0, domain - 1)
  frequency
    [ (3, Ins k <$> arbitrary) -- insert-biased, so occupancy climbs to the target
    , (1, pure (Del k))
    , (2, pure (Look k))
    ]

genOps :: Config -> Gen [Op]
genOps cfg = do
  let dom = cfgDomain cfg
  -- Fill phase: insert every key once, in random order, so occupancy actually
  -- reaches the target load. Random churn alone rarely fills a dense table
  -- (coupon-collector), so without this the high load factors would never be hit.
  fills <- mapM (\k -> Ins k <$> arbitrary) =<< shuffle [0 .. dom - 1]
  -- Churn phase: random ins\/del\/look at that density, to stir the dense table.
  m <- chooseInt (0, 4 * dom)
  churn <- vectorOf m (genOp dom)
  pure (fills ++ churn)

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
runMutable :: Config -> [Op] -> ([Maybe Word64], [Maybe Word64])
runMutable cfg ops = runST $ do
  ht <- HT.new (cfgShift cfg) salt0 salt1
  looks <- go ht ops
  sweep <- mapM (HT.lookup ht . keyOf) [0 .. cfgDomain cfg - 1]
  pure (looks, sweep)
 where
  go _ [] = pure []
  go ht (op : rest) = case op of
    Ins k v -> HT.insert ht (keyOf k) v >> go ht rest
    Del k -> HT.delete ht (keyOf k) >> go ht rest
    Look k -> (:) <$> HT.lookup ht (keyOf k) <*> go ht rest

runModel :: Config -> [Op] -> ([Maybe Word64], [Maybe Word64])
runModel cfg ops = (looks, sweep)
 where
  (looks, final) = go ops Map.empty
  sweep = [Map.lookup (keyOf i) final | i <- [0 .. cfgDomain cfg - 1]]
  go [] m = ([], m)
  go (op : rest) m = case op of
    Ins k v -> go rest (Map.insert (keyOf k) v m)
    Del k -> go rest (Map.delete (keyOf k) m)
    Look k -> let (rs, m') = go rest m in (Map.lookup (keyOf k) m : rs, m')

-- | The peak occupancy the run reaches, in twentieths of capacity (one twentieth
-- = 5%), rounded to the nearest twentieth — so a run that fills the table reads as
-- 20. Reported by the property (via 'tabulate') so the load-factor distribution is
-- visible, in particular that the extremes are actually hit.
--
-- Since the mutable table and the 'Data.Map.Strict' oracle agree (that is the
-- property), the oracle's peak size equals the table's, so it is computed here
-- purely from the same op replay.
peakLoadTwentieths :: Config -> [Op] -> Int
peakLoadTwentieths cfg ops = (20 * peakSize ops + cap `div` 2) `div` cap
 where
  cap = 2 ^ cfgShift cfg :: Int

peakSize :: [Op] -> Int
peakSize = go 0 Map.empty
 where
  go !mx _ [] = mx
  go !mx m (op : rest) = case op of
    Ins k v -> let m' = Map.insert (keyOf k) v m in go (max mx (Map.size m')) m' rest
    Del k -> go mx (Map.delete (keyOf k) m) rest
    Look _ -> go mx m rest

prop_matchesMap :: Property
prop_matchesMap =
  forAll (genConfig (6, 9)) $ \cfg ->
    forAllShrink (genOps cfg) (shrinkList (const [])) $ \ops ->
      tabulate "peak load" [show (peakLoadTwentieths cfg ops) ++ " twentieths"] $
        runMutable cfg ops === runModel cfg ops
