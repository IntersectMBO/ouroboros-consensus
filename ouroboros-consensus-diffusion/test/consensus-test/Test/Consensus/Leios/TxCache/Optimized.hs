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
import Cardano.Ledger.Binary.Version (Version, allVersions)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent (forkIO, killThread, newEmptyMVar, takeMVar, tryPutMVar, yield)
import Control.Monad (void)
import Data.Bits (shiftR)
import qualified Data.ByteString.Short as SBS
import qualified Data.List as List
import Data.Maybe.Strict (StrictMaybe, isSJust)
import Data.Set (Set)
import Data.Word (Word64, Word8)
import Ouroboros.Consensus.Leios.TxCache
  ( LandedTx (..)
  , LeiosTxCache (..)
  , LeiosTxCacheInsertBodySummary
  , ReferencesTxsByHash (..)
  , TxState (..)
  , evictOlderThan
  , insertBody
  , newPureLeiosTxCache
  )
import Ouroboros.Consensus.Leios.TxCache.Optimized (newHashTableLeiosTxCache)
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EndorserBlockHash
  , LeiosPoint (..)
  , TxHash
  , TxLocation
  , TxOffset
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace (FetchArrivalBytes)
import System.IO.Unsafe (unsafePerformIO)
import Test.Consensus.Leios.TxCache.Optimized.MutableHashTable
  ( Config (..)
  , genConfig
  , salt0
  , salt1
  )
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck
  ( Gen
  , Property
  , QuickCheckTests (..)
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
    , testCase
        "a thread killed while inserting a body leaves no tx behind once the body's point is evicted"
        $ killedInsertLeaks >>= (@?= 0)
    ]

type H = LeiosTxCache IO () Version TestBody

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
    OpLand !Word64 !Word8 ![(TxOffset, Word8, TxState () Version)]
  | OpEvict !Word64
  deriving Show

-- | What an op returns.
data Observed
  = ObservedBody !(Maybe LeiosTxCacheInsertBodySummary) ![(TxOffset, Maybe TxLocation)]
  | ObservedLanded !FetchArrivalBytes
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
  OpEvict boundary ->
    (\(points, txs) -> ObservedEvicted points (reverse txs))
      <$> evictOlderThanFoldl' h (SlotNo boundary) [] (flip (:))

-- | A deterministic per-tx size, so both impls bucket identical bytes into the
-- 'FetchArrivalBytes' and any classification mismatch shows up as a difference.
szOf :: Word8 -> BytesSize
szOf t = 1 + fromIntegral t

sweepLookup :: H -> [Word8] -> IO [StrictMaybe (TxState () Version)]
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
      ]
  shuffle (map bodyAt pool ++ others)
 where
  genTx :: Gen Word8
  genTx = fromIntegral <$> chooseInt (0, txDomain - 1)
  gen :: Num a => Int -> Int -> Gen a
  gen lo hi = fromIntegral <$> chooseInt (lo, hi)
  -- Every version the ledger can represent.
  landing (off, t) =
    (,,) off t
      <$> oneof [pure (TxInserted ()), TxValidated <$> elements allVersions]

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
