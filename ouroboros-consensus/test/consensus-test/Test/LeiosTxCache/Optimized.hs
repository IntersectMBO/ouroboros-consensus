-- | Observational-equivalence test for the mutable 'LeiosTxCache' handle: run
-- the same random op sequence (announcements, bodies, tx inserts) through both
-- 'newPureLeiosTxCache' and 'newHashTableLeiosTxCache' and require that they
-- return the same eviction sets from every announcement and agree on a final
-- full-domain lookup sweep. Op ranges make the ~128-announcement eviction
-- cascade fire in the longer sequences.
--
-- Each run also picks a table size and a tx-key domain sized to a target load
-- factor — spanning sparse to /exactly full/ — so the hash table's probing and
-- backward-shift deletion are exercised at high occupancy, not only when nearly
-- empty. The tx domain never exceeds the table capacity (and stays within
-- 'Word8'), so the underlying 'HT.insert' cannot hit its full-table guard.
module Test.LeiosTxCache.Optimized (tests) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (foldM)
import qualified Data.ByteString as BS
import qualified Data.List as List
import Data.Set (Set)
import Data.Word (Word64, Word8)
import LeiosDemoTypes (BytesSize, EbHash (..), FetchArrivalBytes, RbHash (..), TxHash (..))
import LeiosTxCache (LeiosTxCache (..), ReferencesTxsByHash (..), newPureLeiosTxCache)
import LeiosTxCache.Optimized (newHashTableLeiosTxCache)
import Test.LeiosTxCache.Optimized.MutableHashTable (Config (..), genConfig, salt0, salt1)
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.QuickCheck
  ( Gen
  , Property
  , QuickCheckTests (..)
  , chooseInt
  , forAll
  , forAllShrink
  , frequency
  , ioProperty
  , listOf
  , shrinkList
  , shuffle
  , testProperty
  , vectorOf
  , (.&&.)
  , (===)
  )

tests :: TestTree
tests =
  testGroup
    "LeiosTxCache.Optimized"
    [ adjustOption (\(QuickCheckTests n) -> QuickCheckTests (n * 10)) $
        testProperty "hash-table handle == pure handle" prop_equiv
    , adjustOption (\(QuickCheckTests n) -> QuickCheckTests (n * 10)) $
        testProperty "hash-table handle == pure handle, with evictOlderThan" prop_equivEvict
    ]

type H = LeiosTxCache IO () () TestBody

-- | A body carrying its tx hashes.
newtype TestBody = TestBody [TxHash]
  deriving (Eq, Show)

instance ReferencesTxsByHash TestBody where
  foldTxReferences f z (TestBody hs) =
    List.foldl' (\acc txh -> f acc txh dummySize) z hs
   where
    dummySize = 0

-- A 32-byte tx hash (the mutable table reads exactly 32 bytes).
txhOf :: Word8 -> TxHash
txhOf n = MkTxHash (BS.pack (n : replicate 31 0))

ebhOf :: Word8 -> EbHash
ebhOf n = MkEbHash (BS.pack [n])

rbhOf :: Word8 -> RbHash
rbhOf n = MkRbHash (BS.pack [n])

data Op
  = OpAnnounce !Word64 !Word8 !Word8
  | OpBody !Word8 ![Word8]
  | OpUnapplied ![Word8]
  | OpApplied ![Word8]
  | OpEvict !Word64
  deriving Show

-- | Apply an op, returning its observable output: the eviction sets for an
-- announcement or 'evictOlderThan', and the 'FetchArrivalBytes' for an unapplied
-- insert. Both impls must agree on both (see 'prop_equiv').
applyOp :: H -> Op -> IO (Maybe (Set EbHash, Set TxHash), Maybe FetchArrivalBytes)
applyOp h op = case op of
  OpAnnounce s r e -> evicted <$> insertAnnouncement h (SlotNo s) (rbhOf r) (ebhOf e)
  OpEvict boundary -> evicted <$> evictOlderThan h (SlotNo boundary)
  OpBody e ts ->
    insertBody h (ebhOf e) (TestBody (map txhOf ts)) () (\() _ _ _ -> ()) >> pure (Nothing, Nothing)
  OpUnapplied ts ->
    arrival
      <$> withLockedInsertUnappliedTx h (\z step -> foldM (\acc t -> step acc (txhOf t) (szOf t) ()) z ts)
  OpApplied ts ->
    withLockedInsertAppliedTx h (\z step -> foldM (\acc t -> step acc (txhOf t) ()) z ts)
      >> pure (Nothing, Nothing)
 where
  evicted x = (Just x, Nothing)
  arrival fab = (Nothing, Just fab)

-- | A deterministic per-tx size, so both impls bucket identical bytes into the
-- 'FetchArrivalBytes' and any classification mismatch shows up as a difference.
szOf :: Word8 -> BytesSize
szOf t = 1 + fromIntegral t

sweepLookup :: H -> [Word8] -> IO [Maybe (Either () ())]
sweepLookup h txs = withLookupTx h (\look -> mapM (look . txhOf) txs)

sweepBody :: H -> [Word8] -> IO [Maybe TestBody]
sweepBody h ebs = mapM (lookupBody h . ebhOf) ebs

-- | The EB-hash domain the generators draw from (see 'genOps'): announcements
-- and bodies use ebs @1..20@.
ebDomain :: [Word8]
ebDomain = [1 .. 20]

genOps :: Int -> Gen [Op]
genOps txDomain = do
  n <- chooseInt (0, 400)
  vectorOf n genOp
 where
  genOp =
    frequency
      [ (3, OpAnnounce <$> gen 1 300 <*> gen 1 3 <*> gen 1 20)
      , (2, OpBody <$> gen 1 20 <*> listOf genTx)
      , (2, OpUnapplied <$> listOf genTx)
      , (2, OpApplied <$> listOf genTx)
      ]
  genTx :: Gen Word8
  genTx = fromIntegral <$> chooseInt (0, txDomain - 1)
  gen :: Num a => Int -> Int -> Gen a
  gen lo hi = fromIntegral <$> chooseInt (lo, hi)

prop_equiv :: Property
prop_equiv =
  forAll (genConfig (6, 8)) $ \cfg ->
    forAllShrink (genOps (cfgDomain cfg)) (shrinkList (const [])) $ \ops -> ioProperty $ do
      hp <- newPureLeiosTxCache
      hm <- newHashTableLeiosTxCache (cfgShift cfg) salt0 salt1
      resP <- mapM (applyOp hp) ops
      resM <- mapM (applyOp hm) ops
      sweepP <- sweepLookup hp (allTxs (cfgDomain cfg))
      sweepM <- sweepLookup hm (allTxs (cfgDomain cfg))
      sweepBodyP <- sweepBody hp ebDomain
      sweepBodyM <- sweepBody hm ebDomain
      pure (resP === resM .&&. sweepP === sweepM .&&. sweepBodyP === sweepBodyM)
 where
  allTxs txDomain = [0 .. fromIntegral (txDomain - 1)]

-- | 'evictOlderThan' equivalence, kept out of 'prop_equiv': its draining of the
-- cache would defeat that property's load-factor targeting, so this one runs at a
-- fixed, generous table size where occupancy is not the point. Both handles must
-- still agree on every op's eviction set and on the final lookup sweep.
prop_equivEvict :: Property
prop_equivEvict =
  forAllShrink genEvictOps (shrinkList (const [])) $ \ops -> ioProperty $ do
    hp <- newPureLeiosTxCache
    hm <- newHashTableLeiosTxCache tableShift salt0 salt1
    resP <- mapM (applyOp hp) ops
    resM <- mapM (applyOp hm) ops
    sweepP <- sweepLookup hp allTxs
    sweepM <- sweepLookup hm allTxs
    sweepBodyP <- sweepBody hp ebDomain
    sweepBodyM <- sweepBody hm ebDomain
    pure (resP === resM .&&. sweepP === sweepM .&&. sweepBodyP === sweepBodyM)
 where
  tableShift = 8 :: Int -- 256 slots; load factor is deliberately not the point here
  txDomain = 40 :: Int -- << 256, so the table never fills
  allTxs = [0 .. fromIntegral (txDomain - 1)]
  -- Reuse the ordinary op stream, then sprinkle in evictOlderThan boundaries over
  -- the same slot range and shuffle, so evictions interleave with the rest.
  genEvictOps = do
    base <- genOps txDomain
    evicts <- listOf (OpEvict . fromIntegral <$> chooseInt (1, 300))
    shuffle (base ++ evicts)
