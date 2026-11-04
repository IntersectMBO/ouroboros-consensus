{-# LANGUAGE LambdaCase #-}

-- | Tests for the pure 'LeiosTxCacheIndex': which points and txs it holds, the
-- 'maxLeiosPointCount' eviction cascade, and tx locations.
--
-- The pure module is the reference model: a faster implementation is tested
-- for observational equivalence to it, not against these unit tests. These
-- exercise observable behavior only (the exported ops and the internal state
-- constructors), not any representation detail.
module Test.Consensus.Leios.TxCache.Reference (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Data.ByteString.Short as SBS
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64, Word8)
import Ouroboros.Consensus.Leios.TxCache.API
  ( LeiosTxCacheInsertBodySummary (..)
  , ReferencesTxsByHash (..)
  , TxArrivalPrior (..)
  , TxState (..)
  , defaultLeiosTxCacheShift
  , maxLeiosPointCount
  )
import Ouroboros.Consensus.Leios.TxCache.Reference
import Ouroboros.Consensus.Leios.Types
  ( EndorserBlockHash
  , LeiosPoint (..)
  , TxHash
  , TxLocation (..)
  , TxOffset
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
  , elements
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
        [ testCase "taking a body in retains its point" test_pointOne
        , testCase "taking a body in again at the same point is a no-op" test_pointDup
        , testCase "one EB's body taken in at two points -> two points, each summarised" test_pointTwo
        ]
    , testGroup
        "bodies"
        [ testCase "taking a body in records none of its txs" test_bodyRecordsNoTxs
        , testCase
            "taking a body in makes its point the youngest reference of the txs the cache holds"
            test_bodyBumpsHeldTxs
        , testCase "taking a body in summarises what the cache holds of its txs" test_bodySummary
        ]
    , testGroup
        "txs"
        [ testCase "landing a tx not yet applied -> lookupTx TxInserted" test_unapplied
        , testCase "landing an applied tx -> lookupTx TxValidated" test_applied
        , testCase "landing at a point the cache does not retain is a no-op" test_landUnretained
        , testCase "landing a tx again counts it as held" test_landAgain
        , testCase "landing an unapplied tx keeps an applied one applied" test_unappliedAfterApplied
        ]
    , testGroup
        "eviction"
        [ testCase "over-cap insert evicts the oldest point and its txs" test_evict
        , testCase "an EB's tx survives the eviction of one of the EB's two points" test_evictSharedBody
        , testCase "a shared tx survives one referrer's eviction" test_evictShared
        , testCase
            "a full cache evicts the oldest point before taking a body in, so the body sees none of that point's txs"
            test_evictBeforeInsert
        , testCase "evictOlderThan drops points below the boundary, cascading txs" test_evictOlderThan
        , testCase "evictOlderThan keeps points at the boundary slot (exclusive)" test_evictOlderThanExclusive
        , testCase "evictOlderThan below every slot evicts nothing" test_evictOlderThanNone
        , testCase "evictOlderThan clears every point in a stale slot" test_evictOlderThanWholeSlot
        , testCase
            "evictOlderThan visits each forgotten tx once, in the body of its youngest reference, oldest point first"
            test_evictOlderThanOrder
        , testCase "insert below the pruned slot is ignored" test_prunedIgnoresOlderInsert
        , testCase "insert at the pruned slot is admitted (exclusive)" test_prunedAllowsBoundaryInsert
        , testCase "a lower evictOlderThan boundary does not lower the pruned slot" test_prunedSlotMonotone
        , testCase "a full cache ignores a body older than every point it retains" test_insertOldest
        ]
    , testGroup
        "locations"
        [ testCase "landing records where a retained point holds its txs" test_land
        , testCase "taking a body in reports its txs' locations" test_bodyReportsLocations
        , testCase "a tx keeps the location of the younger of two points it landed at" test_youngerWins
        , testCase "evicting the point a location names drops the location" test_evictDropsLocation
        , testCase
            "evicting an older point keeps a tx's location at a younger point it landed at"
            test_evictKeepsYoungerLocation
        , testCase
            "landing at a point the cache ignored keeps the retained points' locations"
            test_landIgnoredPointKeepsLocations
        , testCase
            "a tx keeps its location at a retained point while older points land theirs and are evicted"
            test_olderLandingsKeepLocation
        ]
    , adjustOption (\(QuickCheckTests n) -> QuickCheckTests (n * 10)) $
        testProperty "the index matches a recomputed model, through eviction" prop_model
    ]

{-------------------------------------------------------------------------------
  Fixtures
-------------------------------------------------------------------------------}

-- | The tx payloads are 'Int' so 'lookupTx' results are distinguishable.
type Idx = LeiosTxCacheIndex Int Int TestBody

-- | A body carrying its tx hashes.
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

-- | The point of EB @e@ at slot @s@.
pointAt :: Word64 -> Word8 -> LeiosPoint
pointAt s e = MkLeiosPoint (SlotNo s) (ebhOf e)

-- | Take in the body of EB @e@, referencing these txs, at slot @s@: the index
-- after, the summary, and each tx's offset and location, as the insertion
-- reports them.
insert ::
  Word64 ->
  Word8 ->
  [Word8] ->
  Idx ->
  (Idx, Maybe LeiosTxCacheInsertBodySummary, [(TxOffset, Maybe TxLocation)])
insert s e ts idx =
  let (idx', (summary, located)) =
        insertBodyAndFoldl'
          (2 ^ defaultLeiosTxCacheShift)
          (pointAt s e)
          (TestBody (map txhOf ts))
          []
          (\acc off _txh _sz loc -> (off, loc) : acc)
          idx
   in (idx', summary, reverse located)

-- | Take in the body of EB @e@, referencing these txs, at slot @s@.
body :: Word64 -> Word8 -> [Word8] -> Idx -> Idx
body s e ts idx = let (idx', _summary, _located) = insert s e ts idx in idx'

-- | Take in the bodies of EBs @lo@..@hi@, each at its own slot and referencing
-- no txs.
bodies :: Int -> Int -> Idx -> Idx
bodies lo hi idx0 =
  List.foldl' (\idx i -> body (fromIntegral i) (fromIntegral i) [] idx) idx0 [lo .. hi]

-- | Land these txs, not yet applied, each at its offset of EB @e@'s body at slot
-- @s@.
land :: Word64 -> Word8 -> [(TxOffset, Word8)] -> Idx -> Idx
land s e offTxs idx0 =
  List.foldl'
    (\idx (off, t) -> fst (insertLandedTx (pointAt s e) off (txhOf t) (TxInserted 0) idx))
    idx0
    offTxs

-- | Take in the body of EB @e@ at slot @s@, referencing these txs, and land them.
bodyLanded :: Word64 -> Word8 -> [Word8] -> Idx -> Idx
bodyLanded s e ts = land s e (zip [0 ..] ts) . body s e ts

-- | Evict every point older than this slot: the index after, the evicted
-- points, and the forgotten txs, in the order the eviction visits them.
evictBelow :: SlotNo -> Idx -> (Idx, Set LeiosPoint, [TxHash])
evictBelow boundary idx =
  let (idx', evPoints, evTxs) = evictOlderThanFoldl' boundary [] (flip (:)) idx
   in (idx', evPoints, reverse evTxs)

retains :: Word64 -> Word8 -> Idx -> Bool
retains s e idx = Map.member (pointAt s e) (pointState idx)

pointCount :: Idx -> Int
pointCount = Map.size . pointState

-- | The tx's youngest reference, if the index holds it.
youngestRef :: Word8 -> Idx -> Maybe LeiosPoint
youngestRef t idx = ttYoungestReference <$> Map.lookup (txhOf t) (txState idx)

locationOf :: Word8 -> Idx -> Maybe TxLocation
locationOf t = lookupTxLocation (txhOf t)

{-------------------------------------------------------------------------------
  Points
-------------------------------------------------------------------------------}

test_pointOne :: Assertion
test_pointOne = retains 1 1 (body 1 1 [] empty) @?= True

test_pointDup :: Assertion
test_pointDup = do
  let idx = body 1 1 [10] (bodyLanded 1 1 [10] empty)
  (pointCount idx, youngestRef 10 idx) @?= (1, Just (pointAt 1 1))

test_pointTwo :: Assertion
test_pointTwo = do
  let (idx1, summary1, _) = insert 1 1 [10] empty
      (idx2, summary2, _) = insert 2 1 [10] (land 1 1 [(0, 10)] idx1)
  (pointCount idx2, youngestRef 10 idx2, ibsAcquired <$> summary1, ibsAcquired <$> summary2)
    @?= (2, Just (pointAt 2 1), Just 0, Just 1)

{-------------------------------------------------------------------------------
  Bodies
-------------------------------------------------------------------------------}

test_bodyRecordsNoTxs :: Assertion
test_bodyRecordsNoTxs = Map.null (txState (body 1 1 [10, 11] empty)) @?= True

test_bodyBumpsHeldTxs :: Assertion
test_bodyBumpsHeldTxs = do
  let idx = body 2 2 [10, 11] (bodyLanded 1 1 [10] empty)
  (youngestRef 10 idx, youngestRef 11 idx) @?= (Just (pointAt 2 2), Nothing)

test_bodySummary :: Assertion
test_bodySummary = do
  let base = fst (insertLandedTx (pointAt 1 1) 1 (txhOf 11) (TxValidated 0) (bodyLanded 1 1 [10, 11] empty))
      (_, summary, _) = insert 2 2 [10, 11, 12] base
  ((\s -> (ibsTxsInEb s, ibsAcquired s, ibsValidated s, ibsCacheTxCount s)) <$> summary)
    @?= Just (3, 2, 1, 2)

{-------------------------------------------------------------------------------
  Txs
-------------------------------------------------------------------------------}

test_unapplied :: Assertion
test_unapplied =
  lookupTx
    (txhOf 10)
    (fst (insertLandedTx (pointAt 1 1) 0 (txhOf 10) (TxInserted 7) (body 1 1 [10] empty)))
    @?= SJust (TxInserted 7)

test_applied :: Assertion
test_applied =
  lookupTx
    (txhOf 10)
    (fst (insertLandedTx (pointAt 1 1) 0 (txhOf 10) (TxValidated 9) (body 1 1 [10] empty)))
    @?= SJust (TxValidated 9)

test_landUnretained :: Assertion
test_landUnretained = do
  let (idx, prior) = insertLandedTx (pointAt 1 1) 0 (txhOf 10) (TxInserted 7) empty
  (prior, lookupTx (txhOf 10) idx) @?= (TxEvicted, SNothing)

test_landAgain :: Assertion
test_landAgain = do
  let base = body 2 2 [10] (bodyLanded 1 1 [10] empty)
      (idx, prior) = insertLandedTx (pointAt 2 2) 0 (txhOf 10) (TxInserted 7) base
  (prior, youngestRef 10 idx, lookupTx (txhOf 10) idx)
    @?= (TxWasHeld, Just (pointAt 2 2), SJust (TxInserted 7))

-- | A peer sends a tx the node has already applied.
test_unappliedAfterApplied :: Assertion
test_unappliedAfterApplied = do
  let applied = fst (insertLandedTx (pointAt 1 1) 0 (txhOf 10) (TxValidated 9) (body 1 1 [10] empty))
      (idx, prior) = insertLandedTx (pointAt 1 1) 0 (txhOf 10) (TxInserted 7) applied
  (prior, lookupTx (txhOf 10) idx) @?= (TxWasHeld, SJust (TxValidated 9))

{-------------------------------------------------------------------------------
  Eviction (at maxLeiosPointCount = 128)
-------------------------------------------------------------------------------}

test_evict :: Assertion
test_evict = do
  let base = bodies 2 maxLeiosPointCount (bodyLanded 1 1 [200] empty)
      idx' = body 129 129 [] base
  (pointCount idx', youngestRef 200 idx')
    @?= (maxLeiosPointCount, Nothing)

test_evictSharedBody :: Assertion
test_evictSharedBody = do
  let base = body 128 1 [200] (bodies 2 127 (bodyLanded 1 1 [200] empty))
      idx' = body 129 129 [] base
  (retains 1 1 idx', retains 128 1 idx', youngestRef 200 idx')
    @?= (False, True, Just (pointAt 128 1))

test_evictShared :: Assertion
test_evictShared = do
  let base = bodies 3 maxLeiosPointCount (body 2 2 [200] (bodyLanded 1 1 [200] empty))
      idx' = body 129 129 [] base
  (retains 1 1 idx', youngestRef 200 idx') @?= (False, Just (pointAt 2 2))

-- | Only the oldest point and the new body reference the tx.
test_evictBeforeInsert :: Assertion
test_evictBeforeInsert = do
  let base = bodies 2 maxLeiosPointCount (bodyLanded 1 1 [200] empty)
      (idx', summary, located) = insert 129 129 [200] base
  (retains 1 1 idx', youngestRef 200 idx', ibsAcquired <$> summary, located)
    @?= (False, Nothing, Just 0, [(0, Nothing)])

-- | EBs 1\/2\/3 at slots 1\/2\/3; 'evictOlderThanFoldl'' 3 drops the two below
-- slot 3, cascading their txs, and keeps the one at slot 3.
test_evictOlderThan :: Assertion
test_evictOlderThan = do
  let base = bodyLanded 3 3 [30] (bodyLanded 2 2 [20] (bodyLanded 1 1 [10] empty))
      (idx', evPoints, evTxs) = evictBelow (SlotNo 3) base
  ( evPoints
    , evTxs
    , retains 3 3 idx'
    , youngestRef 10 idx'
    , youngestRef 30 idx'
    )
    @?= ( Set.fromList [pointAt 1 1, pointAt 2 2]
        , [txhOf 10, txhOf 20]
        , True
        , Nothing
        , Just (pointAt 3 3)
        )

-- | The boundary is exclusive: a point /at/ the boundary slot survives.
test_evictOlderThanExclusive :: Assertion
test_evictOlderThanExclusive = do
  let base = bodyLanded 3 3 [30] (bodyLanded 2 2 [20] (bodyLanded 1 1 [10] empty))
      (_, evPoints, evTxs) = evictBelow (SlotNo 2) base
  (evPoints, evTxs) @?= (Set.singleton (pointAt 1 1), [txhOf 10])

-- | A boundary at or below the oldest slot evicts nothing.
test_evictOlderThanNone :: Assertion
test_evictOlderThanNone = do
  let (idx', evPoints, evTxs) = evictBelow (SlotNo 1) (bodies 1 3 empty)
  (evPoints, evTxs, pointCount idx') @?= (Set.empty, [], 3)

-- | Two points share the oldest slot; both are evicted together when that slot
-- falls below the boundary.
test_evictOlderThanWholeSlot :: Assertion
test_evictOlderThanWholeSlot = do
  let base = body 2 3 [] (body 1 2 [] (body 1 1 [] empty))
      (idx', evPoints, _) = evictBelow (SlotNo 2) base
  (evPoints, retains 2 3 idx') @?= (Set.fromList [pointAt 1 1, pointAt 1 2], True)

-- | Tx 10 landed at both points, so the eviction visits it only in the younger
-- point's body, which comes after the older point's: 11, then 12, then 10.
test_evictOlderThanOrder :: Assertion
test_evictOlderThanOrder = do
  let base = bodyLanded 2 2 [12, 10] (bodyLanded 1 1 [10, 11] empty)
      (_, _, evTxs) = evictBelow (SlotNo 3) base
  evTxs @?= [txhOf 11, txhOf 12, txhOf 10]

-- | 'evictOlderThanFoldl'' records the boundary; a later body taken in at a
-- point strictly below it is silently ignored (the cache has already been
-- pruned past that slot).
test_prunedIgnoresOlderInsert :: Assertion
test_prunedIgnoresOlderInsert = do
  let (pruned, _, _) = evictBelow (SlotNo 5) (bodies 1 10 empty)
      idx' = body 4 40 [] pruned
  (pointCount idx', retains 4 40 idx') @?= (pointCount pruned, False)

-- | The boundary is exclusive on insertion too: a point /at/ the pruned slot is
-- retained, matching 'evictOlderThanFoldl'' retaining points at the boundary
-- slot.
test_prunedAllowsBoundaryInsert :: Assertion
test_prunedAllowsBoundaryInsert = do
  let (pruned, _, _) = evictBelow (SlotNo 5) (bodies 1 10 empty)
  retains 5 55 (body 5 55 [] pruned) @?= True

-- | The pruned slot is monotone: a later, lower 'evictOlderThanFoldl'' boundary
-- does not lower it, so an insert below the earlier (higher) boundary stays
-- ignored.
test_prunedSlotMonotone :: Assertion
test_prunedSlotMonotone = do
  let (p1, _, _) = evictBelow (SlotNo 5) (bodies 1 10 empty)
      (p2, _, _) = evictBelow (SlotNo 3) p1
      idx' = body 4 40 [] p2
  (pointCount idx', retains 4 40 idx') @?= (pointCount p2, False)

test_insertOldest :: Assertion
test_insertOldest = do
  let full = bodies 2 (maxLeiosPointCount + 1) empty
      (idx', summary, _) = insert 1 250 [250] full
  (retains 1 250 idx', retains 2 2 idx', summary) @?= (False, True, Nothing)

{-------------------------------------------------------------------------------
  Locations
-------------------------------------------------------------------------------}

test_land :: Assertion
test_land = do
  let idx = bodyLanded 1 1 [10, 11] empty
  (locationOf 10 idx, locationOf 11 idx)
    @?= (Just (MkTxLocation (pointAt 1 1) 0), Just (MkTxLocation (pointAt 1 1) 1))

test_bodyReportsLocations :: Assertion
test_bodyReportsLocations = do
  let (_, _, located) = insert 2 2 [11, 10] (bodyLanded 1 1 [10] empty)
  located @?= [(0, Nothing), (1, Just (MkTxLocation (pointAt 1 1) 0))]

test_youngerWins :: Assertion
test_youngerWins = do
  let base = body 5 5 [10] (body 1 1 [11, 10] empty)
      youngerFirst = land 1 1 [(1, 10)] (land 5 5 [(0, 10)] base)
      olderFirst = land 5 5 [(0, 10)] (land 1 1 [(1, 10)] base)
  (locationOf 10 youngerFirst, locationOf 10 olderFirst)
    @?= (Just (MkTxLocation (pointAt 5 5) 0), Just (MkTxLocation (pointAt 5 5) 0))

-- | The tx is still referenced, by a point taken in while it was held.
test_evictDropsLocation :: Assertion
test_evictDropsLocation = do
  let base = body 2 2 [10] (bodyLanded 1 1 [10] empty)
      (idx, _, _) = evictBelow (SlotNo 2) base
  (youngestRef 10 idx, locationOf 10 idx) @?= (Just (pointAt 2 2), Nothing)

test_evictKeepsYoungerLocation :: Assertion
test_evictKeepsYoungerLocation = do
  let base = land 1 1 [(0, 10)] (bodyLanded 5 5 [10] (body 1 1 [10] empty))
      (idx, _, _) = evictBelow (SlotNo 2) base
  locationOf 10 idx @?= Just (MkTxLocation (pointAt 5 5) 0)

-- | Each of a full cache's points has landed its one tx, in slot order. Then a
-- body older than all of them arrives, which the cache ignores, and its tx
-- lands too.
test_landIgnoredPointKeepsLocations :: Assertion
test_landIgnoredPointKeepsLocations = do
  let points = [2 .. fromIntegral maxLeiosPointCount + 1] :: [Word8]
      full = List.foldl' (\idx e -> bodyLanded (fromIntegral e) e [e] idx) empty points
      idx' = bodyLanded 1 250 [250] full
  (locationOf 2 idx', locationOf 250 idx')
    @?= (Just (MkTxLocation (pointAt 2 2) 0), Nothing)

-- | A point lands its tx, then older points fill the cache and land theirs, in
-- turn, and one more pushes the oldest out and lands too.
test_olderLandingsKeepLocation :: Assertion
test_olderLandingsKeepLocation = do
  let step idx e = bodyLanded (fromIntegral e) e [e] idx
      youngest = 200
      idx' = List.foldl' step empty (youngest : [1 .. fromIntegral maxLeiosPointCount])
  (retains 200 youngest idx', locationOf youngest idx')
    @?= (True, Just (MkTxLocation (pointAt 200 youngest) 0))

{-------------------------------------------------------------------------------
  Model-based property
-------------------------------------------------------------------------------}

data Op
  = -- | Take in the body of EB @e@ at a slot. EB @e@ references txs @e@ and
    -- @e + 1@, so that neighbouring EBs share a tx.
    OpBody Word64 Word8
  | -- | Land the txs at these offsets of EB @e@'s body at a slot, each not yet
    -- applied or applied.
    OpLand Word64 Word8 [(TxOffset, TxState Int Int)]
  | OpEvict Word64
  deriving Show

genW :: Num a => Int -> Int -> Gen a
genW lo hi = fromIntegral <$> chooseInt (lo, hi)

-- | Some 200 bodies, so that the cap's eviction happens in every run, at few
-- enough EBs that EBs share txs and are taken in at several points; landings,
-- mostly at those points; and a few boundaries for 'evictOlderThanFoldl''.
genOps :: Gen [Op]
genOps = do
  bs <- vectorOf 200 ((,) <$> genW 1 300 <*> genW 1 20)
  lands <- listOf $ do
    (s, e) <- oneof [elements bs, (,) <$> genW 1 300 <*> genW 1 20]
    offs <- elements [[0], [1], [0, 1]]
    OpLand s e
      <$> traverse (\off -> (,) off <$> oneof [TxInserted <$> genW 0 3, TxValidated <$> genW 0 3]) offs
  nEvicts <- chooseInt (0, 3)
  evicts <- vectorOf nEvicts (OpEvict <$> genW 1 300)
  shuffle (map (uncurry OpBody) bs ++ lands ++ evicts)

-- | What the model records of a tx: the retained points it landed at or that
-- were taken in while the index held it, its payload, and the retained points
-- it landed at with its offsets there.
data ModelTx = MkModelTx
  { mRefs :: Set LeiosPoint
  , mPayload :: TxState Int Int
  , mLandings :: Set (LeiosPoint, TxOffset)
  }

type Model = Map Word8 ModelTx

-- | After each op, the index holds exactly the txs whose model records a
-- retained point, each with the youngest of those points as its youngest
-- reference, its payload, and its youngest landing as its location; and it
-- retains no more than 'maxLeiosPointCount' points.
prop_model :: Property
prop_model = forAllShrink genOps (shrinkList (const [])) $ \ops ->
  conjoin
    [ counterexample ("after " <> show op) $
        conjoin
          [ counterexample "cap exceeded" $ pointCount idx <= maxLeiosPointCount
          , Map.keysSet (txState idx) === Set.map txhOf (Map.keysSet model)
          , [observe idx t | t <- txs] === [expected model t | t <- txs]
          ]
    | (op, (idx, model)) <- zip ops (drop 1 (scanl step (empty, Map.empty) ops))
    ]
 where
  txs = [1 .. 21]

  observe idx t = (youngestRef t idx, lookupTx (txhOf t) idx, locationOf t idx)

  expected model t = case Map.lookup t model of
    Nothing -> (Nothing, SNothing, Nothing)
    Just mtx ->
      ( Set.lookupMax (mRefs mtx)
      , SJust (mPayload mtx)
      , uncurry MkTxLocation <$> Set.lookupMax (mLandings mtx)
      )

  step :: (Idx, Model) -> Op -> (Idx, Model)
  step (idx, model) = \case
    OpBody s e ->
      let idx' = body s e [e, e + 1] idx
          pruned = prune idx idx' model
          inserted = Map.notMember (pointAt s e) (pointState idx) && retains s e idx'
          refer mtx = mtx{mRefs = Set.insert (pointAt s e) (mRefs mtx)}
       in ( idx'
          , if inserted then List.foldl' (flip (Map.adjust refer)) pruned [e, e + 1] else pruned
          )
    OpLand s e offPayloads
      | retains s e idx ->
          ( List.foldl'
              (\i (off, payload) -> fst (insertLandedTx (pointAt s e) off (txhOf (e + fromIntegral off)) payload i))
              idx
              offPayloads
          , List.foldl'
              (\m (off, payload) -> Map.alter (Just . landed off payload) (e + fromIntegral off) m)
              model
              offPayloads
          )
      | otherwise -> (idx, model)
     where
      p = pointAt s e
      landed off payload = \case
        Nothing -> MkModelTx (Set.singleton p) payload (Set.singleton (p, off))
        Just mtx ->
          MkModelTx
            { mRefs = Set.insert p (mRefs mtx)
            , mPayload = case (payload, mPayload mtx) of
                (TxInserted _, held@TxValidated{}) -> held
                _ -> payload
            , mLandings = Set.insert (p, off) (mLandings mtx)
            }
    OpEvict boundary ->
      let (idx', _, _) = evictBelow (SlotNo boundary) idx in (idx', prune idx idx' model)

  -- Drop the points evicted between these two indexes, and each tx that no
  -- longer has a point.
  prune idx idx' =
    Map.mapMaybe $ \mtx ->
      let gone = Map.keysSet (pointState idx) `Set.difference` Map.keysSet (pointState idx')
          refs = mRefs mtx `Set.difference` gone
       in if Set.null refs
            then Nothing
            else
              Just
                mtx
                  { mRefs = refs
                  , mLandings = Set.filter (\(q, _off) -> Set.notMember q gone) (mLandings mtx)
                  }
