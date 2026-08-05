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
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Word (Word64, Word8)
import qualified LeiosTxCache.Optimized.MutableHashTable as HT
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
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
  , (.&&.)
  , (===)
  )

tests :: TestTree
tests =
  testGroup
    "LeiosTxCache.Optimized.MutableHashTable"
    [ adjustOption (\(QuickCheckTests n) -> QuickCheckTests (n * 10)) $
        testProperty "agrees with Data.Map across load factors" prop_matchesMap
    , testGroup
        "SipHash-2-4"
        [ testCase "reference matches official vector (empty input)" $
            refSipHash24 refK0 refK1 [] @?= 0x726fdb47dd0e0e31
        , testCase "reference matches official vector (15 bytes)" $
            refSipHash24 refK0 refK1 [0 .. 14] @?= 0xa129ca6149be45e5
        , testProperty "ported core matches the reference" prop_siphashMatchesReference
        ]
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
runMutable :: Config -> [Op] -> (([Maybe Word64], [Maybe Word64]), Maybe String)
runMutable cfg ops = runST $ do
  ht <- HT.new (cfgShift cfg) salt0 salt1
  looks <- go ht ops
  sweep <- mapM (HT.lookup ht . keyOf) [0 .. cfgDomain cfg - 1]
  inv <- HT.checkInvariants ht
  pure ((looks, sweep), inv)
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
        let (result, inv) = runMutable cfg ops
         in inv === Nothing .&&. result === runModel cfg ops

{-------------------------------------------------------------------------------
  SipHash-2-4 validation

  'HT.siphash24' is the anti-flooding primitive, and a wrong-but-deterministic
  hash would sail through the model test above while silently defeating the salt.
  So we anchor a spec-following reference to the published test vectors, then check
  the ported core against it over random inputs.
-------------------------------------------------------------------------------}

-- | The published SipHash test key: bytes @0x00 .. 0x0f@ as two little-endian
-- words.
refK0, refK1 :: Word64
refK0 = 0x0706050403020100
refK1 = 0x0f0e0d0c0b0a0908

-- | The ported 4-word core agrees with the byte-oriented reference on any salt
-- and any 32-byte key. (Both read the four words the same way: the reference's
-- little-endian byte encoding round-trips through 'word64ToLE'.)
prop_siphashMatchesReference ::
  Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Property
prop_siphashMatchesReference k0 k1 m0 m1 m2 m3 =
  HT.siphash24 k0 k1 (HT.Key m0 m1 m2 m3)
    === refSipHash24 k0 k1 (concatMap word64ToLE [m0, m1, m2, m3])
 where
  word64ToLE w = [fromIntegral (w `shiftR` (8 * i)) | i <- [0 .. 7]] :: [Word8]

-- | A spec-following SipHash-2-4 over a byte string, used only to validate
-- 'HT.siphash24'; anchored to the published vectors in 'tests'.
refSipHash24 :: Word64 -> Word64 -> [Word8] -> Word64
refSipHash24 k0 k1 msg = final (List.foldl' compress' initial (fullWords ++ [finalWord]))
 where
  initial =
    ( k0 `xor` 0x736f6d6570736575
    , k1 `xor` 0x646f72616e646f6d
    , k0 `xor` 0x6c7967656e657261
    , k1 `xor` 0x7465646279746573
    )
  n = length msg
  nFull = n `div` 8
  fullWords = [leWord (take 8 (drop (8 * i) msg)) | i <- [0 .. nFull - 1]]
  leftover = drop (8 * nFull) msg
  finalWord = leWord leftover .|. (fromIntegral (n .&. 0xff) `shiftL` 56)

  leWord :: [Word8] -> Word64
  leWord = foldr (\b acc -> (acc `shiftL` 8) .|. fromIntegral b) 0

  -- c = 2 compression rounds per message word
  compress' (v0, v1, v2, v3) m =
    let (a0, a1, a2, a3) = sipround' (v0, v1, v2, v3 `xor` m)
        (b0, b1, b2, b3) = sipround' (a0, a1, a2, a3)
     in (b0 `xor` m, b1, b2, b3)

  -- d = 4 finalization rounds after flipping v2
  final (v0, v1, v2, v3) =
    let (w0, w1, w2, w3) = iterate sipround' (v0, v1, v2 `xor` 0xff, v3) !! 4
     in w0 `xor` w1 `xor` w2 `xor` w3

  sipround' (v0, v1, v2, v3) =
    let a = v0 + v1
        b = rotl v1 13 `xor` a
        a' = rotl a 32
        c = v2 + v3
        d = rotl v3 16 `xor` c
        a'' = a' + d
        d' = rotl d 21 `xor` a''
        c' = c + b
        b' = rotl b 17 `xor` c'
        c'' = rotl c' 32
     in (a'', b', c'', d')

  rotl x r = (x `shiftL` r) .|. (x `shiftR` (64 - r))
