{-# LANGUAGE TypeApplications #-}

-- | Tests for the pure 'LeiosTxCacheIndex': announcement/body/tx reference
-- counting and the 'maxAnnouncementCount' eviction cascade.
--
-- The pure module is the reference model: the forthcoming mutable-hashtable
-- implementation will be tested for observational equivalence to it, not against
-- these unit tests. These exercise observable behavior only (the exported ops
-- and the internal state constructors), not any representation detail.
module Test.LeiosTxCache.Reference (tests) where

import Cardano.Slotting.Slot (SlotNo (..))
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import qualified Data.List as List
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word (Word64, Word8)
import LeiosDemoTypes (EbHash (..), RbHash (..), TxHash (..))
import LeiosTxCache.Reference
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
    "LeiosTxCache.Reference"
    [ testGroup
        "announcements"
        [ testCase "one announcement -> BodyNotYetInserted rc=1" test_annOne
        , testCase "re-announcing the same (slot, rb) is a no-op" test_annDup
        , testCase "two announcements of one EB -> body rc=2" test_annTwo
        ]
    , testGroup
        "bodies"
        [ testCase "insertBody references its txs (NotYetInserted rc=1)" test_body
        , testCase "insertBody on an unannounced EB is a no-op" test_bodyUnannounced
        , testCase "insertBody is idempotent" test_bodyIdempotent
        , testCase "lookupBody on an untracked EB is Nothing" test_lookupBodyUntracked
        , testCase "lookupBody on an announced-only EB is Nothing" test_lookupBodyAnnouncedOnly
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
        [ testCase "over-cap insert evicts the oldest body and its txs" test_evict
        , testCase "evicting a body-less EB evicts no txs" test_evictBodyless
        , testCase "a shared tx survives one referrer's eviction" test_evictShared
        , testCase "evictOlderThan drops EBs below the boundary, cascading txs" test_evictOlderThan
        , testCase "evictOlderThan keeps EBs at the boundary slot (exclusive)" test_evictOlderThanExclusive
        , testCase "evictOlderThan below every slot evicts nothing" test_evictOlderThanNone
        , testCase "evictOlderThan clears every EB in a stale slot" test_evictOlderThanWholeSlot
        , testCase "insert below the pruned slot is ignored" test_prunedIgnoresOlderInsert
        , testCase "insert at the pruned slot is admitted (exclusive)" test_prunedAllowsBoundaryInsert
        , testCase "a lower evictOlderThan boundary does not lower the pruned slot" test_prunedSlotMonotone
        ]
    , testProperty "announcementCount = sum of per-slot sizes" prop_countInvariant
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
  foldTxReferences f z (TestBody hs) = List.foldl' f z hs

empty :: Idx
empty = emptyLeiosTxCacheIndex

mkTxHash :: Word8 -> TxHash
mkTxHash w = MkTxHash (BS.pack [w])

mkEbHash :: Word8 -> EbHash
mkEbHash w = MkEbHash (BS.pack [w])

mkRbHash :: Word8 -> RbHash
mkRbHash w = MkRbHash (BS.pack [w])

-- | Insert an announcement, discarding the evicted sets.
ann :: Word64 -> Word8 -> Word8 -> Idx -> Idx
ann s r e idx = let (idx', _, _) = insertAnnouncement (SlotNo s) (mkRbHash r) (mkEbHash e) idx in idx'

body :: Word8 -> [Word8] -> Idx -> Idx
body e ts idx = fst (insertBody (mkEbHash e) (TestBody (map mkTxHash ts)) idx)

-- | Announce EBs 1..n, each at its own slot and with its own RB hash.
annN :: Int -> Idx -> Idx
annN n idx0 =
  List.foldl' (\idx i -> ann (fromIntegral i) (fromIntegral i) (fromIntegral i) idx) idx0 [1 .. n]

bodyRC :: Word8 -> Idx -> Maybe RefCount
bodyRC e idx = rc <$> Map.lookup (mkEbHash e) (bodyState idx)
 where
  rc (BodyNotYetInserted r) = r
  rc (BodyAlreadyInserted r _) = r

txRC :: Word8 -> Idx -> Maybe RefCount
txRC t idx = rc <$> Map.lookup (mkTxHash t) (txState idx)
 where
  rc (TxNotYetInserted r) = r
  rc (TxAlreadyInserted r _) = r
  rc (TxAlreadyValidated r _) = r

{-------------------------------------------------------------------------------
  Announcements
-------------------------------------------------------------------------------}

test_annOne :: Assertion
test_annOne = bodyRC 1 (ann 1 1 1 empty) @?= Just (MkRefCount 1)

test_annDup :: Assertion
test_annDup = do
  let (idx1, _, _) = insertAnnouncement (SlotNo 1) (mkRbHash 1) (mkEbHash 1) empty
      (idx2, evEbs, evTxs) = insertAnnouncement (SlotNo 1) (mkRbHash 1) (mkEbHash 1) idx1
  (bodyRC 1 idx2, evEbs, evTxs) @?= (Just (MkRefCount 1), Set.empty, Set.empty)

test_annTwo :: Assertion
test_annTwo = bodyRC 1 (ann 2 2 1 (ann 1 1 1 empty)) @?= Just (MkRefCount 2)

{-------------------------------------------------------------------------------
  Bodies
-------------------------------------------------------------------------------}

test_body :: Assertion
test_body = do
  let idx = body 1 [10, 11] (ann 1 1 1 empty)
  (txRC 10 idx, txRC 11 idx, lookupTx (mkTxHash 10) idx)
    @?= (Just (MkRefCount 1), Just (MkRefCount 1), Nothing)

test_bodyUnannounced :: Assertion
test_bodyUnannounced = txRC 10 (body 1 [10] empty) @?= Nothing

test_bodyIdempotent :: Assertion
test_bodyIdempotent = txRC 10 (body 1 [10] (body 1 [10] (ann 1 1 1 empty))) @?= Just (MkRefCount 1)

test_lookupBodyUntracked :: Assertion
test_lookupBodyUntracked = lookupBody (mkEbHash 1) empty @?= Nothing

-- | Announced but body not inserted ('BodyNotYetInserted') reads as 'Nothing'.
test_lookupBodyAnnouncedOnly :: Assertion
test_lookupBodyAnnouncedOnly = lookupBody (mkEbHash 1) (ann 1 1 1 empty) @?= Nothing

test_lookupBodyInserted :: Assertion
test_lookupBodyInserted =
  lookupBody (mkEbHash 1) (body 1 [10, 11] (ann 1 1 1 empty))
    @?= Just (TestBody [mkTxHash 10, mkTxHash 11])

-- | Evicting the EB (its slot falls below the boundary) drops its body too.
test_lookupBodyEvicted :: Assertion
test_lookupBodyEvicted = do
  let base = body 1 [10] (ann 1 1 1 empty)
      (idx', _, _) = evictOlderThan (SlotNo 2) base
  lookupBody (mkEbHash 1) idx' @?= Nothing

{-------------------------------------------------------------------------------
  Txs
-------------------------------------------------------------------------------}

test_unapplied :: Assertion
test_unapplied =
  lookupTx (mkTxHash 10) (insertUnappliedTx (mkTxHash 10) 7 (body 1 [10] (ann 1 1 1 empty)))
    @?= Just (Left 7)

test_applied :: Assertion
test_applied =
  lookupTx (mkTxHash 10) (insertAppliedTx (mkTxHash 10) 9 (body 1 [10] (ann 1 1 1 empty)))
    @?= Just (Right 9)

test_txUnreferenced :: Assertion
test_txUnreferenced =
  lookupTx (mkTxHash 10) (insertUnappliedTx (mkTxHash 10) 7 empty) @?= Nothing

test_preserveRc :: Assertion
test_preserveRc = do
  let idx0 = body 2 [10] (body 1 [10] (ann 2 2 2 (ann 1 1 1 empty)))
      idx = insertUnappliedTx (mkTxHash 10) 7 idx0
  (txRC 10 idx, lookupTx (mkTxHash 10) idx) @?= (Just (MkRefCount 2), Just (Left 7))

{-------------------------------------------------------------------------------
  Eviction (at maxAnnouncementCount = 128)
-------------------------------------------------------------------------------}

test_evict :: Assertion
test_evict = do
  let base = body 1 [200] (annN maxAnnouncementCount empty)
      (idx', evEbs, evTxs) =
        insertAnnouncement (SlotNo 129) (mkRbHash 129) (mkEbHash 129) base
  (evEbs, evTxs, bodyRC 1 idx', txRC 200 idx')
    @?= (Set.singleton (mkEbHash 1), Set.singleton (mkTxHash 200), Nothing, Nothing)

test_evictBodyless :: Assertion
test_evictBodyless = do
  let (_, evEbs, evTxs) =
        insertAnnouncement (SlotNo 129) (mkRbHash 129) (mkEbHash 129) (annN maxAnnouncementCount empty)
  (evEbs, evTxs) @?= (Set.singleton (mkEbHash 1), Set.empty)

test_evictShared :: Assertion
test_evictShared = do
  let base = body 2 [200] (body 1 [200] (annN maxAnnouncementCount empty))
      (idx', evEbs, evTxs) =
        insertAnnouncement (SlotNo 129) (mkRbHash 129) (mkEbHash 129) base
  (evEbs, evTxs, txRC 200 idx')
    @?= (Set.singleton (mkEbHash 1), Set.empty, Just (MkRefCount 1))

-- | EBs 1\/2\/3 at slots 1\/2\/3, each with a body; 'evictOlderThan' 3 drops the
-- two below slot 3, cascading their txs, and keeps the one at slot 3.
test_evictOlderThan :: Assertion
test_evictOlderThan = do
  let base = body 3 [30] (body 2 [20] (body 1 [10] (annN 3 empty)))
      (idx', evEbs, evTxs) = evictOlderThan (SlotNo 3) base
  ( evEbs
    , evTxs
    , bodyRC 1 idx'
    , bodyRC 3 idx'
    , txRC 10 idx'
    , txRC 30 idx'
    )
    @?= ( Set.fromList [mkEbHash 1, mkEbHash 2]
        , Set.fromList [mkTxHash 10, mkTxHash 20]
        , Nothing
        , Just (MkRefCount 1)
        , Nothing
        , Just (MkRefCount 1)
        )

-- | The boundary is exclusive: an EB /at/ the boundary slot survives.
test_evictOlderThanExclusive :: Assertion
test_evictOlderThanExclusive = do
  let base = body 3 [30] (body 2 [20] (body 1 [10] (annN 3 empty)))
      (_, evEbs, evTxs) = evictOlderThan (SlotNo 2) base
  (evEbs, evTxs) @?= (Set.singleton (mkEbHash 1), Set.singleton (mkTxHash 10))

-- | A boundary at or below the oldest slot evicts nothing.
test_evictOlderThanNone :: Assertion
test_evictOlderThanNone = do
  let base = body 1 [10] (annN 3 empty)
      (idx', evEbs, evTxs) = evictOlderThan (SlotNo 1) base
  (evEbs, evTxs, announcementCount idx') @?= (Set.empty, Set.empty, 3)

-- | Two EBs share the oldest slot; both are evicted together when that slot falls
-- below the boundary (the loop chews through the whole slot).
test_evictOlderThanWholeSlot :: Assertion
test_evictOlderThanWholeSlot = do
  let base = ann 2 3 3 (ann 1 2 2 (ann 1 1 1 empty))
      (idx', evEbs, _) = evictOlderThan (SlotNo 2) base
  (evEbs, bodyRC 3 idx') @?= (Set.fromList [mkEbHash 1, mkEbHash 2], Just (MkRefCount 1))

-- | 'evictOlderThan' records the boundary; a later announcement strictly below it
-- is silently ignored (the cache has already been pruned past that slot).
test_prunedIgnoresOlderInsert :: Assertion
test_prunedIgnoresOlderInsert = do
  let (pruned, _, _) = evictOlderThan (SlotNo 5) (annN 10 empty)
      (idx', evEbs, evTxs) = insertAnnouncement (SlotNo 4) (mkRbHash 40) (mkEbHash 40) pruned
  (announcementCount idx', bodyRC 40 idx', evEbs, evTxs)
    @?= (announcementCount pruned, Nothing, Set.empty, Set.empty)

-- | The boundary is exclusive on insertion too: an announcement /at/ the pruned
-- slot is admitted, matching 'evictOlderThan' retaining EBs at the boundary slot.
test_prunedAllowsBoundaryInsert :: Assertion
test_prunedAllowsBoundaryInsert = do
  let (pruned, _, _) = evictOlderThan (SlotNo 5) (annN 10 empty)
      (idx', _, _) = insertAnnouncement (SlotNo 5) (mkRbHash 55) (mkEbHash 55) pruned
  bodyRC 55 idx' @?= Just (MkRefCount 1)

-- | The pruned slot is monotone: a later, lower 'evictOlderThan' boundary does not
-- lower it, so an insert below the earlier (higher) boundary stays ignored.
test_prunedSlotMonotone :: Assertion
test_prunedSlotMonotone = do
  let (p1, _, _) = evictOlderThan (SlotNo 5) (annN 10 empty)
      (p2, _, _) = evictOlderThan (SlotNo 3) p1
      (idx', evEbs, evTxs) = insertAnnouncement (SlotNo 4) (mkRbHash 40) (mkEbHash 40) p2
  (announcementCount idx', bodyRC 40 idx', evEbs, evTxs)
    @?= (announcementCount p2, Nothing, Set.empty, Set.empty)

{-------------------------------------------------------------------------------
  Invariant
-------------------------------------------------------------------------------}

-- | 'announcementCount' always equals the total number of retained announcements
-- (the sum of the per-slot map sizes). Ranges are kept below the cap so eviction
-- doesn't enter into it.
prop_countInvariant :: Property
prop_countInvariant = forAll genStream $ \ops ->
  let idx = List.foldl' (\i (s, r, e) -> ann s r e i) empty ops
   in announcementCount idx === sum (NEMap.size <$> Map.elems (announcementState idx))
 where
  genStream = listOf ((,,) <$> gen 1 20 <*> gen 1 5 <*> gen 1 10)
  gen lo hi = fromIntegral <$> chooseInt (lo, hi)

{-------------------------------------------------------------------------------
  Model-based refcount properties
-------------------------------------------------------------------------------}

data Op
  = OpAnn Word64 Word8 Word8
  | OpBody Word8 [Word8]
  | OpUnappliedTx Word8
  | OpAppliedTx Word8
  deriving Show

applyOp :: Op -> Idx -> Idx
applyOp op = case op of
  OpAnn s r e -> ann s r e
  OpBody e ts -> body e ts
  OpUnappliedTx t -> insertUnappliedTx (mkTxHash t) 0
  OpAppliedTx t -> insertAppliedTx (mkTxHash t) 0

genW :: Num a => Int -> Int -> Gen a
genW lo hi = fromIntegral <$> chooseInt (lo, hi)

genAnn :: Gen Op
genAnn = OpAnn <$> genW 1 1000 <*> genW 1 5 <*> genW 1 8

genOp :: Gen Op
genOp =
  oneof
    [ genAnn
    , OpBody <$> genW 1 8 <*> listOf (genW 1 20)
    , OpUnappliedTx <$> genW 1 20
    , OpAppliedTx <$> genW 1 20
    ]

-- | Every sequence carries a block of ~200 distinct announcements so the
-- 128-cap eviction is exercised in every run, interleaved with other ops.
genOps :: Gen [Op]
genOps = do
  anns <- vectorOf 200 genAnn
  others <- listOf genOp
  shuffle (anns ++ others)

bodyRefCountOf :: BodyState b -> RefCount
bodyRefCountOf (BodyNotYetInserted r) = r
bodyRefCountOf (BodyAlreadyInserted r _) = r

txRefCountOf :: TxState a v -> RefCount
txRefCountOf (TxNotYetInserted r) = r
txRefCountOf (TxAlreadyInserted r _) = r
txRefCountOf (TxAlreadyValidated r _) = r

rcInt :: RefCount -> Int
rcInt (MkRefCount w) = fromIntegral w

txHashesOf :: ReferencesTxsByHash b => b -> [TxHash]
txHashesOf = foldTxReferences (flip (:)) []

-- | After any sequence of ops the maintained refcounts agree with the refcounts
-- recomputed from first principles: a body's refcount is the number of retained
-- announcements of that EB, and a tx's refcount is the number of inserted bodies
-- referencing it. Also confirms the count invariant and the cap survive eviction.
prop_refcounts :: Property
prop_refcounts = forAllShrink genOps (shrinkList (const [])) $ \ops ->
  let idx = List.foldl' (flip applyOp) empty ops
   in conjoin
        [ counterexample "cap exceeded" $
            announcementCount idx <= maxAnnouncementCount
        , counterexample "announcementCount /= sum of per-slot sizes" $
            announcementCount idx === sum (NEMap.size <$> Map.elems (announcementState idx))
        , counterexample "body refcounts disagree with announcements" $
            Map.map (rcInt . bodyRefCountOf) (bodyState idx) === expectedBodyRC idx
        , counterexample "tx refcounts disagree with inserted bodies" $
            Map.map (rcInt . txRefCountOf) (txState idx) === expectedTxRC idx
        ]
 where
  expectedBodyRC ix =
    Map.fromListWith
      (+)
      [ (ebh, 1 :: Int)
      | nem <- Map.elems (announcementState ix)
      , ebh <- toList nem
      ]
  expectedTxRC ix =
    Map.fromListWith
      (+)
      [ (txh, 1 :: Int)
      | BodyAlreadyInserted _ b <- Map.elems (bodyState ix)
      , txh <- txHashesOf b
      ]
