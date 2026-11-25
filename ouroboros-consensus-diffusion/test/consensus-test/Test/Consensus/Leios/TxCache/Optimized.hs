{-# LANGUAGE LambdaCase #-}

-- | Observational-equivalence test for the mutable 'LeiosTxCache' handle: run
-- the same random op sequence (bodies taken in at points, txs landed at points,
-- evictions) through both 'newPureLeiosTxCache' and 'newHashTableLeiosTxCache'
-- and require that every op observes the same and that they agree on a final
-- full-domain sweep. Every run takes in bodies at more points than
-- 'maxLeiosPointCount', so the cap's eviction fires in every run.
--
-- Each run also picks a table size and a tx-key domain sized to a target load
-- factor — spanning sparse to /exactly full/ — so the hash table's probing and
-- backward-shift deletion are exercised at high occupancy, not only when nearly
-- empty. The tx domain never exceeds the table capacity (and stays within
-- 'Word8'), so the underlying 'HT.insert' cannot hit its full-table guard.
module Test.Consensus.Leios.TxCache.Optimized (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent (forkIO, killThread, newEmptyMVar, takeMVar, tryPutMVar, yield)
import Control.Monad (forM_, void)
import Data.Bits (shiftR)
import qualified Data.ByteString.Short as SBS
import qualified Data.List as List
import Data.Maybe.Strict (StrictMaybe (..), isSJust)
import Data.Set (Set)
import Data.Word (Word64, Word8)
import Ouroboros.Consensus.Ledger.SupportsMempool (VersionWord16 (..))
import Ouroboros.Consensus.Leios.TxCache
  ( LandedTx (..)
  , LeiosTxCache (..)
  , LeiosTxCacheInsertBodySummary
  , PackedEndorserBlock
  , ReferencesTxsByHash (..)
  , TxState (..)
  , evictOlderThan
  , insertBody
  , newPureLeiosTxCache
  , nullLeiosTxCache
  , packEndorserBlock
  )
import Ouroboros.Consensus.Leios.TxCache.Optimized (newHashTableLeiosTxCache)
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EndorserBlockHash
  , LeiosPoint (..)
  , TxHash
  , TxLocation
  , TxOffset
  , endorserBlockReferences
  , mkEndorserBlock
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace
  ( FetchArrivalBytes
  , fetchArrivalEvicted
  , fetchArrivalExtra
  , fetchArrivalGood
  )
import System.IO.Unsafe (unsafePerformIO)
import Test.Consensus.Leios.TxCache.Optimized.MutableHashTable
  ( Config (..)
  , genConfig
  , salt0
  , salt1
  )
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.QuickCheck
  ( Gen
  , Property
  , QuickCheckTests (..)
  , arbitraryBoundedIntegral
  , chooseInt
  , elements
  , forAll
  , forAllShrink
  , frequency
  , ioProperty
  , listOf
  , listOf1
  , oneof
  , shrinkList
  , shuffle
  , sublistOf
  , testProperty
  , vectorOf
  , (.&&.)
  , (===)
  )

tests :: TestTree
tests =
  testGroup
    "TxCache.Optimized"
    [ adjustOption (\(QuickCheckTests n) -> QuickCheckTests (n * 10)) $
        testProperty "hash-table handle == pure handle" prop_equiv
    , adjustOption (\(QuickCheckTests n) -> QuickCheckTests (n * 10)) $
        testProperty "hash-table handle == pure handle, with evictOlderThan" prop_equivEvict
    , testGroup
        "point lifetime boundaries"
        [ testCase name (testPointLifetime makeCache)
        | (name, makeCache) <-
            [("reference", newPureLeiosTxCache 8), ("optimized", newHashTableLeiosTxCache 8 salt0 salt1)]
        ]
    , testGroup
        "read-only arrival snapshots"
        [ testCase name (testClassification makeCache)
        | (name, makeCache) <-
            [("reference", newPureLeiosTxCache 8), ("optimized", newHashTableLeiosTxCache 8 salt0 salt1)]
        ]
    , testGroup
        "full-width validation versions"
        [ testCase (name <> "/" <> show version) (testValidationVersion makeCache version)
        | (name, makeCache) <-
            [("reference", newPureLeiosTxCache 8), ("optimized", newHashTableLeiosTxCache 8 salt0 salt1)]
        , version <- [MkVersionWord16 1023, MkVersionWord16 1024, maxBound]
        ]
    , testGroup
        "packed body reference geometry"
        [ testCase name (testPackedReferences makeCache)
        | (name, makeCache) <-
            [("reference", newPureLeiosTxCache 8), ("optimized", newHashTableLeiosTxCache 8 salt0 salt1)]
        ]
    , testCase "null cache classifies every occurrence as evicted" $ do
        classifyTxs (nullLeiosTxCache :: H) (pointAt 1 1) [(txhOf 1, 10), (txhOf 1, 10)]
          >>= (@?= fetchArrivalEvicted 20)
    , testCase
        "a thread killed while inserting a body leaves no tx behind once the body's point is evicted"
        $ killedInsertLeaks >>= (@?= 0)
    ]

type H = LeiosTxCache IO () VersionWord16 TestBody

-- | A body carrying its tx hashes.
newtype TestBody = TestBody [TxHash]
  deriving (Eq, Show)

instance ReferencesTxsByHash TestBody where
  foldTxReferences f z (TestBody hs) =
    List.foldl' (\acc txh -> f acc txh dummySize) z hs
   where
    dummySize = 0

txhOf :: Word8 -> TxHash
txhOf n = UnsafeHash $ SBS.pack (n : replicate 31 0)

ebhOf :: Word8 -> EndorserBlockHash
ebhOf n = UnsafeHash $ SBS.pack (n : replicate 31 0)

pointAt :: Word64 -> Word8 -> LeiosPoint
pointAt s e = MkLeiosPoint (SlotNo s) (ebhOf e)

data Op
  = -- | Take in EB @e@'s body, which references these txs, at a slot.
    OpBody !Word64 !Word8 ![Word8]
  | -- | Land these txs, each the one EB @e@'s body lists at its offset, at the
    -- EB's point at a slot.
    OpLand !Word64 !Word8 ![(TxOffset, Word8, TxState () VersionWord16)]
  | OpClassify !Word64 !Word8 ![Word8]
  | OpEvict !Word64
  deriving Show

-- | What an op returns.
data Observed
  = ObservedBody !(Maybe LeiosTxCacheInsertBodySummary) ![(TxOffset, Maybe TxLocation)]
  | ObservedLanded !FetchArrivalBytes
  | ObservedClassified !FetchArrivalBytes
  | -- | The evicted points, and the forgotten txs in the order they were visited.
    ObservedEvicted !(Set LeiosPoint) ![TxHash]
  deriving (Eq, Show)

applyOp :: H -> Op -> IO Observed
applyOp h = \case
  OpBody s e ts ->
    (\(summary, located) -> ObservedBody summary (reverse located))
      <$> insertBodyAndFoldl'
        h
        (pointAt s e)
        (TestBody (map txhOf ts))
        []
        (\acc off _txh _sz loc -> (off, loc) : acc)
  OpLand s e landings ->
    ObservedLanded
      <$> insertLandedTxs
        h
        (pointAt s e)
        [MkLandedTx off (txhOf t) (szOf t) payload | (off, t, payload) <- landings]
  OpClassify s e ts ->
    ObservedClassified <$> classifyTxs h (pointAt s e) [(txhOf t, szOf t) | t <- ts]
  OpEvict boundary ->
    (\(points, txs) -> ObservedEvicted points (reverse txs))
      <$> evictOlderThanFoldl' h (SlotNo boundary) [] (flip (:))

-- | A deterministic per-tx size, so both impls bucket identical bytes into the
-- 'FetchArrivalBytes' and any classification mismatch shows up as a difference.
szOf :: Word8 -> BytesSize
szOf t = 1 + fromIntegral t

sweepLookup :: H -> [Word8] -> IO [StrictMaybe (TxState () VersionWord16)]
sweepLookup h txs = withLookupTx h (\look -> mapM (look . txhOf) txs)

-- | Observe each tx's location the way production does -- through a
-- body insert's per-tx callback. The probe EB's body, taken in last, reports
-- each tx's location as it stood before the insertion; what the insertion then
-- changes is never observed. Driven identically on both handles so any location
-- disagreement shows.
probeEb :: Word8
probeEb = 255

sweepLoc :: H -> [Word8] -> IO [Maybe TxLocation]
sweepLoc h txs = do
  (_noSummary, locs) <-
    insertBodyAndFoldl'
      h
      (pointAt 1000 probeEb)
      (TestBody (map txhOf txs))
      []
      (\acc _off _txh _sz mbLoc -> mbLoc : acc)
  pure (reverse locs)

-- | Ops over a pool of points, more of them than 'maxLeiosPointCount', of 20
-- EBs, each with its own body: the body taken in at each of those points, and
-- more ops at them, mostly landing some of their txs.
genOps :: Int -> Gen [Op]
genOps txDomain = do
  ebs <- vectorOf 20 (List.nub <$> listOf1 genTx)
  pool <- vectorOf 200 ((,) <$> gen 1 300 <*> gen 1 20)
  let txsOf e = ebs !! (fromIntegral e - 1)
      bodyAt (s, e) = OpBody s e (txsOf e)
  n <- chooseInt (0, 400)
  others <- vectorOf n $ do
    (s, e) <- elements pool
    frequency
      [ (1, pure (bodyAt (s, e)))
      , (4, OpLand s e <$> (traverse landing =<< sublistOf (zip [0 ..] (txsOf e))))
      , (2, OpClassify s e <$> sublistOf (txsOf e))
      ]
  shuffle (map bodyAt pool ++ others)
 where
  genTx :: Gen Word8
  genTx = fromIntegral <$> chooseInt (0, txDomain - 1)
  gen :: Num a => Int -> Int -> Gen a
  gen lo hi = fromIntegral <$> chooseInt (lo, hi)
  -- Versions spanning all 16 bits.
  landing (off, t) =
    (,,) off t
      <$> oneof [pure (TxInserted ()), TxValidated . MkVersionWord16 <$> arbitraryBoundedIntegral]

-- | Both handles, fed these ops, observe the same of each, and agree on a final
-- sweep of every tx's state and location.
equivalent :: Int -> [Word8] -> [Op] -> IO Property
equivalent nshift allTxs ops = do
  hp <- newPureLeiosTxCache nshift
  hm <- newHashTableLeiosTxCache nshift salt0 salt1
  resP <- mapM (applyOp hp) ops
  resM <- mapM (applyOp hm) ops
  sweepP <- sweepLookup hp allTxs
  sweepM <- sweepLookup hm allTxs
  sweepLocP <- sweepLoc hp allTxs
  sweepLocM <- sweepLoc hm allTxs
  pure (resP === resM .&&. sweepP === sweepM .&&. sweepLocP === sweepLocM)

prop_equiv :: Property
prop_equiv =
  forAll (genConfig (6, 8)) $ \cfg ->
    forAllShrink (genOps (cfgDomain cfg)) (shrinkList (const [])) $
      ioProperty . equivalent (cfgShift cfg) [0 .. fromIntegral (cfgDomain cfg - 1)]

-- | 'evictOlderThanFoldl'' equivalence, kept out of 'prop_equiv': its draining of the
-- cache would defeat that property's load-factor targeting, so this one runs at a
-- fixed, generous table size where occupancy is not the point.
prop_equivEvict :: Property
prop_equivEvict =
  forAllShrink genEvictOps (shrinkList (const [])) $
    ioProperty . equivalent tableShift [0 .. fromIntegral (txDomain - 1)]
 where
  tableShift = 8 :: Int -- 256 slots; load factor is deliberately not the point here
  txDomain = 40 :: Int -- << 256, so the table never fills
  -- Reuse the ordinary op stream, then sprinkle in evictOlderThan boundaries over
  -- the same slot range and shuffle, so evictions interleave with the rest.
  genEvictOps = do
    base <- genOps txDomain
    evicts <- listOf (OpEvict . fromIntegral <$> chooseInt (1, 300))
    shuffle (base ++ evicts)

-- | The txs land at one point, and then a peer thread taking in a body that
-- lists them at a younger point is killed halfway through, as when the node
-- disconnects the peer. Both points are then evicted. How many txs the cache
-- still holds; it must hold none.
--
-- TODO Interrupting 'evictOlderThanFoldl'' halfway through its fold is not
-- tested, though its callback could pause there as this test's does; nor is
-- interrupting 'insertBodyAndFoldl'' during the eviction before its fold, which
-- has no callback to pause in. They mask asynchronous exceptions the same way.
--
-- Halfway through the body, the inserting thread signals and yields, so that
-- the killing thread runs then: nothing in an insertion itself would let it.
-- Neither blocks, so neither lets an exception through a mask.
killedInsertLeaks :: IO Int
killedInsertLeaks = do
  h <- newHashTableLeiosTxCache 13 salt0 salt1 :: IO H
  void $ insertBody h older (TestBody txs)
  _ <-
    insertLandedTxs
      h
      older
      [MkLandedTx off t 1 (TxInserted ()) | (off, t) <- zip [0 ..] txs]
  halfway <- newEmptyMVar
  let signalHalfway w off
        | off == fromIntegral (length txs `div` 2) =
            unsafePerformIO (tryPutMVar halfway () >> yield) `seq` w
        | otherwise = w
  inserter <-
    forkIO $
      void $
        insertBodyAndFoldl' h younger (TestBody txs) () (\w off _ _ _ -> signalHalfway w off)
  takeMVar halfway
  killThread inserter
  evictOlderThan h (SlotNo 3)
  withLookupTx h $ \look -> length . filter isSJust <$> mapM look txs
 where
  older = MkLeiosPoint (SlotNo 1) (ebhOf 1)
  younger = MkLeiosPoint (SlotNo 2) (ebhOf 2)
  txs = map txhOfInt [0 .. 2 ^ (12 :: Int) - 1]
  txhOfInt :: Int -> TxHash
  txhOfInt n =
    UnsafeHash $ SBS.pack ([fromIntegral (n `shiftR` (8 * i)) | i <- [0 .. 3]] ++ replicate 28 0)

-- | Repeated registration does not change retention; younger references keep
-- held state after the source point is evicted, but cannot keep its location.
testPointLifetime :: IO H -> IO ()
testPointLifetime makeCache = do
  cache <- makeCache
  let old = pointAt 1 1
      shared = pointAt 128 1
      lastPoint = pointAt 129 129
      insert point refs = void $ insertBody cache point (TestBody refs)
      look tx = withLookupTx cache ($ tx)
  insert old [txhOf 1, txhOf 2]
  insert old [txhOf 1, txhOf 2]
  void $
    insertLandedTxs
      cache
      old
      [MkLandedTx off (txhOf t) 1 (TxInserted ()) | (off, t) <- [(0, 1), (1, 2)]]
  insert shared [txhOf 1, txhOf 2]
  mapM_ (\n -> insert (pointAt (fromIntegral n) n) []) [2 .. 127]
  insert lastPoint [txhOf 1, txhOf 3]
  classifyTxs cache old [(txhOf 2, 1)] >>= (@?= fetchArrivalEvicted 1)
  look (txhOf 2) >>= (@?= SJust (TxInserted ()))
  evictOlderThan cache 129
  look (txhOf 1) >>= (@?= SJust (TxInserted ()))
  look (txhOf 2) >>= (@?= SNothing)
  evictOlderThan cache 130
  look (txhOf 1) >>= (@?= SNothing)

-- | Classification neither inserts missing occurrences nor changes existing
-- validation. An unretained point is evicted even when a newer point keeps X.
testClassification :: IO H -> IO ()
testClassification makeCache = do
  cache <- makeCache
  let p = pointAt 1 1
      q = pointAt 2 2
      body = TestBody [txhOf 1, txhOf 2]
      batch = [(txhOf 1, 10), (txhOf 2, 10), (txhOf 2, 10)]
      version = MkVersionWord16 10
  void $ insertBody cache p body
  classifyTxs cache p batch >>= (@?= fetchArrivalGood 30)
  sweepLookup cache [1, 2] >>= (@?= [SNothing, SNothing])
  void $ insertLandedTxs cache p [MkLandedTx 0 (txhOf 1) 10 (TxValidated version)]
  before <- sweepLookup cache [1, 2]
  classifyTxs cache p batch >>= (@?= fetchArrivalExtra 10 <> fetchArrivalGood 20)
  classifyTxs cache p [] >>= (@?= mempty)
  sweepLookup cache [1, 2] >>= (@?= before)
  void $ insertLandedTxs cache p [MkLandedTx 0 (txhOf 1) 10 (TxInserted ())]
  sweepLookup cache [1, 2] >>= (@?= [SJust (TxValidated version), SNothing])
  void $ insertBody cache q body
  evictOlderThan cache 2
  classifyTxs cache p batch >>= (@?= fetchArrivalEvicted 30)
  classifyTxs cache q batch >>= (@?= fetchArrivalExtra 10 <> fetchArrivalGood 20)
  void $ insertLandedTxs cache p [MkLandedTx 1 (txhOf 2) 10 (TxInserted ())]
  sweepLookup cache [1, 2] >>= (@?= [SJust (TxValidated version), SNothing])

-- | Exercise the optimized tag packing at the old 10-bit boundary and at the
-- full 16-bit limit. A later unvalidated insertion must retain that exact tag.
testValidationVersion :: IO H -> VersionWord16 -> IO ()
testValidationVersion makeCache version = do
  cache <- makeCache
  let point = pointAt 1 1
      tx = txhOf 1
      look = withLookupTx cache ($ tx)
  void $ insertBody cache point (TestBody [tx])
  void $ insertLandedTxs cache point [MkLandedTx 0 tx 1 (TxValidated version)]
  look >>= (@?= SJust (TxValidated version))
  void $ insertLandedTxs cache point [MkLandedTx 0 tx 1 (TxInserted ())]
  look >>= (@?= SJust (TxValidated version))

-- Exercise the real packed representation, rather than TestBody. These size
-- boundaries test reference metadata, not the validity of ledger transactions.
-- The ledger constructor canonicalizes repeated hashes before packing. Compare
-- against the expected canonical sequence, including its assigned offsets.
-- Empty and singleton vectors also exercise the ends of the hash blob.
testPackedReferences :: IO (LeiosTxCache IO () VersionWord16 PackedEndorserBlock) -> Assertion
testPackedReferences makeCache =
  forM_ [([], []), (take 1 refs, take 1 refs), (refs ++ take 1 refs, refs)] $ \(input, expected) -> do
    cache <- makeCache
    let point = pointAt 1 1
        body = mkEndorserBlock input
        packed = packEndorserBlock body
    endorserBlockReferences body @?= expected
    reverse (foldTxReferences (\acc h size -> (h, size) : acc) [] packed) @?= expected
    (_, located) <-
      insertBodyAndFoldl' cache point packed [] (\acc off h size loc -> (off, h, size, loc) : acc)
    reverse located @?= [(off, h, size, Nothing) | (off, (h, size)) <- zip [0 ..] expected]
 where
  refs = [(txhOf 3, 360), (txhOf 1, 65536), (txhOf 4, maxBound), (txhOf 2, 0)]
