-- | Tests for the pure 'LeiosTxCacheIndex': point/body/tx reference counting
-- and the 'maxLeiosPointCount' eviction cascade.
--
-- The pure module is the reference model: a faster implementation is tested
-- for observational equivalence to it, not against these unit tests. These
-- exercise observable behavior only (the exported ops and the internal state
-- constructors), not any representation detail.
module Test.Consensus.Leios.TxCache.Reference (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Data.ByteString.Short as SBS
import Data.Foldable (toList)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import Data.Word (Word64, Word8)
import Ouroboros.Consensus.Leios.TxCache.API (defaultLeiosTxCacheShift)
import Ouroboros.Consensus.Leios.TxCache.Reference
import Ouroboros.Consensus.Leios.Types
  ( EndorserBlockHash
  , LeiosPoint (..)
  , TxHash
  )
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.QuickCheck
  ( Gen
  , Property
  , QuickCheckTests (..)
  , chooseInt
  , conjoin
  , counterexample
  , forAll
  , forAllShrink
  , listOf
  , oneof
  , shrinkList
  , shuffle
  , testProperty
  , vectorOf
  , (===)
  )

tests :: TestTree
tests =
  testGroup
    "TxCache.Reference"
    [ testGroup
        "points"
        [ testCase "taking a body in retains its point -> body rc=1" test_pointOne
        , testCase "taking a body in again at the same point is a no-op" test_pointDup
        , testCase "one EB's body taken in at two points -> body rc=2, tx rc=1" test_pointTwo
        ]
    , testGroup
        "bodies"
        [ testCase "insertBody references its txs (NotYetInserted rc=1)" test_body
        , testCase "insertBody is idempotent" test_bodyIdempotent
        , testCase "lookupBody on an untracked EB is Nothing" test_lookupBodyUntracked
        , testCase "lookupBody after insertBody returns the body" test_lookupBodyInserted
        , testCase "lookupBody after eviction is Nothing" test_lookupBodyEvicted
        ]
    , testGroup
        "txs"
        [ testCase "insertUnappliedTx -> lookupTx Left" test_unapplied
        , testCase "insertAppliedTx -> lookupTx Right" test_applied
        , testCase "insert on an unreferenced tx is a no-op" test_txUnreferenced
        , testCase "insertUnappliedTx preserves the refcount" test_preserveRc
        ]
    , testGroup
        "eviction"
        [ testCase "over-cap insert evicts the oldest point, its body and its txs" test_evict
        , testCase "a body held at two points survives one point's eviction" test_evictSharedBody
        , testCase "a shared tx survives one referrer's eviction" test_evictShared
        , testCase "evictOlderThan drops points below the boundary, cascading txs" test_evictOlderThan
        , testCase "evictOlderThan keeps points at the boundary slot (exclusive)" test_evictOlderThanExclusive
        , testCase "evictOlderThan below every slot evicts nothing" test_evictOlderThanNone
        , testCase "evictOlderThan clears every point in a stale slot" test_evictOlderThanWholeSlot
        , testCase "insert below the pruned slot is ignored" test_prunedIgnoresOlderInsert
        , testCase "insert at the pruned slot is admitted (exclusive)" test_prunedAllowsBoundaryInsert
        , testCase "a lower evictOlderThan boundary does not lower the pruned slot" test_prunedSlotMonotone
        ]
    , testProperty "pointCount = sum of per-slot sizes" prop_countInvariant
    , adjustOption (\(QuickCheckTests n) -> QuickCheckTests (n * 100)) $
        testProperty "refcounts match a recomputed model, through eviction" prop_refcounts
    ]

{-------------------------------------------------------------------------------
  Fixtures
-------------------------------------------------------------------------------}

-- | The tx payloads are 'Int' so 'lookupTx' results are distinguishable.
type Idx = LeiosTxCacheIndex Int Int TestBody

-- | A mock EB body: just the list of tx hashes it references.
newtype TestBody = TestBody [TxHash]
  deriving (Eq, Show)

instance ReferencesTxsByHash TestBody where
  foldTxReferences f z (TestBody hs) =
    List.foldl' (\acc txh -> f acc txh dummySize) z hs
   where
    dummySize = 0

empty :: Idx
empty = emptyLeiosTxCacheIndex

txhOf :: Word8 -> TxHash
txhOf w = UnsafeHash $ SBS.pack (w : replicate 31 0)

ebhOf :: Word8 -> EndorserBlockHash
ebhOf w = UnsafeHash $ SBS.pack (w : replicate 31 0)

-- | Take in the body of EB @e@, referencing these txs, at slot @s@.
body :: Word64 -> Word8 -> [Word8] -> Idx -> Idx
body s e ts idx =
  fst
    ( insertBody
        (2 ^ defaultLeiosTxCacheShift)
        (MkLeiosPoint (SlotNo s) (ebhOf e))
        (TestBody (map txhOf ts))
        ()
        (\() _ _ _ _ -> ())
        idx
    )

-- | Take in the bodies of EBs @lo@..@hi@, each at its own slot and referencing
-- no txs.
bodies :: Int -> Int -> Idx -> Idx
bodies lo hi idx0 =
  List.foldl' (\idx i -> body (fromIntegral i) (fromIntegral i) [] idx) idx0 [lo .. hi]

bodyRC :: Word8 -> Idx -> Maybe RefCount
bodyRC e idx = bodyRefCount <$> Map.lookup (ebhOf e) (bodyState idx)

txRC :: Word8 -> Idx -> Maybe RefCount
txRC t idx = txRefCount <$> Map.lookup (txhOf t) (txState idx)

{-------------------------------------------------------------------------------
  Points
-------------------------------------------------------------------------------}

test_pointOne :: Assertion
test_pointOne = bodyRC 1 (body 1 1 [] empty) @?= Just (MkRefCount 1)

test_pointDup :: Assertion
test_pointDup = do
  let idx = body 1 1 [10] (body 1 1 [10] empty)
  (pointCount idx, bodyRC 1 idx, txRC 10 idx)
    @?= (1, Just (MkRefCount 1), Just (MkRefCount 1))

test_pointTwo :: Assertion
test_pointTwo = do
  let idx = body 2 1 [10] (body 1 1 [10] empty)
  (pointCount idx, bodyRC 1 idx, txRC 10 idx)
    @?= (2, Just (MkRefCount 2), Just (MkRefCount 1))

{-------------------------------------------------------------------------------
  Bodies
-------------------------------------------------------------------------------}

test_body :: Assertion
test_body = do
  let idx = body 1 1 [10, 11] empty
  (txRC 10 idx, txRC 11 idx, lookupTx (txhOf 10) idx)
    @?= (Just (MkRefCount 1), Just (MkRefCount 1), Nothing)

test_bodyIdempotent :: Assertion
test_bodyIdempotent = txRC 10 (body 1 1 [10] (body 1 1 [10] empty)) @?= Just (MkRefCount 1)

test_lookupBodyUntracked :: Assertion
test_lookupBodyUntracked = lookupBody (ebhOf 1) empty @?= Nothing

test_lookupBodyInserted :: Assertion
test_lookupBodyInserted =
  lookupBody (ebhOf 1) (body 1 1 [10, 11] empty)
    @?= Just (TestBody [txhOf 10, txhOf 11])

-- | Evicting the point (its slot falls below the boundary) drops its body too.
test_lookupBodyEvicted :: Assertion
test_lookupBodyEvicted = do
  let (idx', _, _) = evictOlderThan (SlotNo 2) (body 1 1 [10] empty)
  lookupBody (ebhOf 1) idx' @?= Nothing

{-------------------------------------------------------------------------------
  Txs
-------------------------------------------------------------------------------}

test_unapplied :: Assertion
test_unapplied =
  lookupTx (txhOf 10) (fst (insertUnappliedTx (txhOf 10) 7 (body 1 1 [10] empty)))
    @?= Just (Left 7)

test_applied :: Assertion
test_applied =
  lookupTx (txhOf 10) (insertAppliedTx (txhOf 10) 9 (body 1 1 [10] empty))
    @?= Just (Right 9)

test_txUnreferenced :: Assertion
test_txUnreferenced =
  lookupTx (txhOf 10) (fst (insertUnappliedTx (txhOf 10) 7 empty)) @?= Nothing

test_preserveRc :: Assertion
test_preserveRc = do
  let idx0 = body 2 2 [10] (body 1 1 [10] empty)
      idx = fst (insertUnappliedTx (txhOf 10) 7 idx0)
  (txRC 10 idx, lookupTx (txhOf 10) idx) @?= (Just (MkRefCount 2), Just (Left 7))

{-------------------------------------------------------------------------------
  Eviction (at maxLeiosPointCount = 128)
-------------------------------------------------------------------------------}

test_evict :: Assertion
test_evict = do
  let base = bodies 2 maxLeiosPointCount (body 1 1 [200] empty)
      idx' = body 129 129 [] base
  (pointCount idx', lookupBody (ebhOf 1) idx', txRC 200 idx')
    @?= (maxLeiosPointCount, Nothing, Nothing)

test_evictSharedBody :: Assertion
test_evictSharedBody = do
  let base = body 128 1 [200] (bodies 2 127 (body 1 1 [200] empty))
      idx' = body 129 129 [] base
  (bodyRC 1 idx', txRC 200 idx') @?= (Just (MkRefCount 1), Just (MkRefCount 1))

test_evictShared :: Assertion
test_evictShared = do
  let base = bodies 3 maxLeiosPointCount (body 2 2 [200] (body 1 1 [200] empty))
      idx' = body 129 129 [] base
  (bodyRC 1 idx', txRC 200 idx') @?= (Nothing, Just (MkRefCount 1))

-- | EBs 1\/2\/3 at slots 1\/2\/3; 'evictOlderThan' 3 drops the two below slot 3,
-- cascading their txs, and keeps the one at slot 3.
test_evictOlderThan :: Assertion
test_evictOlderThan = do
  let base = body 3 3 [30] (body 2 2 [20] (body 1 1 [10] empty))
      (idx', evEbs, evTxs) = evictOlderThan (SlotNo 3) base
  ( evEbs
    , evTxs
    , bodyRC 1 idx'
    , bodyRC 3 idx'
    , txRC 10 idx'
    , txRC 30 idx'
    )
    @?= ( Set.fromList [ebhOf 1, ebhOf 2]
        , Set.fromList [txhOf 10, txhOf 20]
        , Nothing
        , Just (MkRefCount 1)
        , Nothing
        , Just (MkRefCount 1)
        )

-- | The boundary is exclusive: a point /at/ the boundary slot survives.
test_evictOlderThanExclusive :: Assertion
test_evictOlderThanExclusive = do
  let base = body 3 3 [30] (body 2 2 [20] (body 1 1 [10] empty))
      (_, evEbs, evTxs) = evictOlderThan (SlotNo 2) base
  (evEbs, evTxs) @?= (Set.singleton (ebhOf 1), Set.singleton (txhOf 10))

-- | A boundary at or below the oldest slot evicts nothing.
test_evictOlderThanNone :: Assertion
test_evictOlderThanNone = do
  let (idx', evEbs, evTxs) = evictOlderThan (SlotNo 1) (bodies 1 3 empty)
  (evEbs, evTxs, pointCount idx') @?= (Set.empty, Set.empty, 3)

-- | Two points share the oldest slot; both are evicted together when that slot
-- falls below the boundary (the loop chews through the whole slot).
test_evictOlderThanWholeSlot :: Assertion
test_evictOlderThanWholeSlot = do
  let base = body 2 3 [] (body 1 2 [] (body 1 1 [] empty))
      (idx', evEbs, _) = evictOlderThan (SlotNo 2) base
  (evEbs, bodyRC 3 idx') @?= (Set.fromList [ebhOf 1, ebhOf 2], Just (MkRefCount 1))

-- | 'evictOlderThan' records the boundary; a later body taken in at a point
-- strictly below it is silently ignored (the cache has already been pruned past
-- that slot).
test_prunedIgnoresOlderInsert :: Assertion
test_prunedIgnoresOlderInsert = do
  let (pruned, _, _) = evictOlderThan (SlotNo 5) (bodies 1 10 empty)
      idx' = body 4 40 [] pruned
  (pointCount idx', bodyRC 40 idx') @?= (pointCount pruned, Nothing)

-- | The boundary is exclusive on insertion too: a point /at/ the pruned slot is
-- retained, matching 'evictOlderThan' retaining points at the boundary slot.
test_prunedAllowsBoundaryInsert :: Assertion
test_prunedAllowsBoundaryInsert = do
  let (pruned, _, _) = evictOlderThan (SlotNo 5) (bodies 1 10 empty)
  bodyRC 55 (body 5 55 [] pruned) @?= Just (MkRefCount 1)

-- | The pruned slot is monotone: a later, lower 'evictOlderThan' boundary does not
-- lower it, so an insert below the earlier (higher) boundary stays ignored.
test_prunedSlotMonotone :: Assertion
test_prunedSlotMonotone = do
  let (p1, _, _) = evictOlderThan (SlotNo 5) (bodies 1 10 empty)
      (p2, _, _) = evictOlderThan (SlotNo 3) p1
      idx' = body 4 40 [] p2
  (pointCount idx', bodyRC 40 idx') @?= (pointCount p2, Nothing)

{-------------------------------------------------------------------------------
  Invariant
-------------------------------------------------------------------------------}

-- | 'pointCount' always equals the total number of retained points (the sum of
-- the per-slot set sizes). Ranges are kept below the cap so eviction doesn't
-- enter into it.
prop_countInvariant :: Property
prop_countInvariant = forAll genStream $ \ops ->
  let idx = List.foldl' (\i (s, e) -> body s e [] i) empty ops
   in pointCount idx === sum (NESet.size <$> Map.elems (pointState idx))
 where
  genStream = listOf ((,) <$> gen 1 20 <*> gen 1 5)
  gen lo hi = fromIntegral <$> chooseInt (lo, hi)

{-------------------------------------------------------------------------------
  Model-based refcount properties
-------------------------------------------------------------------------------}

data Op
  = -- | Take in the body of an EB at a slot. EB @e@ references txs @e@ and
    -- @e + 1@, so that neighbouring EBs share a tx.
    OpBody Word64 Word8
  | OpUnappliedTx Word8
  | OpAppliedTx Word8
  deriving Show

applyOp :: Op -> Idx -> Idx
applyOp op = case op of
  OpBody s e -> body s e [e, e + 1]
  OpUnappliedTx t -> fst . insertUnappliedTx (txhOf t) 0
  OpAppliedTx t -> insertAppliedTx (txhOf t) 0

genW :: Num a => Int -> Int -> Gen a
genW lo hi = fromIntegral <$> chooseInt (lo, hi)

genBody :: Gen Op
genBody = OpBody <$> genW 1 1000 <*> genW 1 8

genOp :: Gen Op
genOp =
  oneof
    [ genBody
    , OpUnappliedTx <$> genW 1 10
    , OpAppliedTx <$> genW 1 10
    ]

-- | Every sequence carries a block of ~200 bodies at mostly distinct points so
-- the 128-cap eviction is exercised in every run, interleaved with other ops.
genOps :: Gen [Op]
genOps = do
  bs <- vectorOf 200 genBody
  others <- listOf genOp
  shuffle (bs ++ others)

rcInt :: RefCount -> Int
rcInt (MkRefCount w) = fromIntegral w

txHashesOf :: ReferencesTxsByHash b => b -> [TxHash]
txHashesOf = foldTxReferences (\acc txh _sz -> txh : acc) []

-- | After any sequence of ops the maintained refcounts agree with the refcounts
-- recomputed from first principles: a body's refcount is the number of retained
-- points of that EB, and a tx's refcount is the number of held bodies
-- referencing it. Also confirms the count invariant and the cap survive eviction.
prop_refcounts :: Property
prop_refcounts = forAllShrink genOps (shrinkList (const [])) $ \ops ->
  let idx = List.foldl' (flip applyOp) empty ops
   in conjoin
        [ counterexample "cap exceeded" $
            pointCount idx <= maxLeiosPointCount
        , counterexample "pointCount /= sum of per-slot sizes" $
            pointCount idx === sum (NESet.size <$> Map.elems (pointState idx))
        , counterexample "body refcounts disagree with points" $
            Map.map (rcInt . bodyRefCount) (bodyState idx) === expectedBodyRC idx
        , counterexample "tx refcounts disagree with held bodies" $
            Map.map (rcInt . txRefCount) (txState idx) === expectedTxRC idx
        ]
 where
  expectedBodyRC ix =
    Map.fromListWith
      (+)
      [ (ebh, 1 :: Int)
      | ebhs <- Map.elems (pointState ix)
      , ebh <- toList ebhs
      ]
  expectedTxRC ix =
    Map.fromListWith
      (+)
      [ (txh, 1 :: Int)
      | MkHeldBody _ b <- Map.elems (bodyState ix)
      , txh <- txHashesOf b
      ]
