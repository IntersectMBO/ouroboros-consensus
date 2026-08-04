{-# LANGUAGE BangPatterns #-}

-- | Observational-equivalence test for the mutable 'LeiosTxCache' handle: run
-- the same random op sequence (announcements, bodies, tx inserts) through both
-- 'newPureLeiosTxCache' and 'newHashTableLeiosTxCache' and require that they
-- return the same eviction sets from every announcement and agree on a final
-- full-domain lookup sweep. Op ranges make the ~128-announcement eviction
-- cascade fire in the longer sequences.
module Test.LeiosTxCache.Mutable (tests) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (foldM)
import qualified Data.ByteString as BS
import qualified Data.List as List
import Data.Set (Set)
import Data.Word (Word64, Word8)
import LeiosDemoTypes (EbHash (..), RbHash (..), TxHash (..))
import LeiosTxCache (LeiosTxCache (..), newPureLeiosTxCache)
import LeiosTxCache.Mutable (newHashTableLeiosTxCache)
import LeiosTxCacheIndex (ReferencesTxsByHash (..))
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.QuickCheck
  ( Gen
  , Property
  , QuickCheckTests (..)
  , chooseInt
  , forAllShrink
  , frequency
  , ioProperty
  , listOf
  , shrinkList
  , testProperty
  , vectorOf
  , (.&&.)
  , (===)
  )

tests :: TestTree
tests =
  testGroup
    "LeiosTxCache.Mutable"
    [ adjustOption (\(QuickCheckTests n) -> QuickCheckTests (n * 10)) $
        testProperty "hash-table handle == pure handle" prop_equiv
    ]

type H = LeiosTxCache IO () () TestBody

-- | A body carrying its tx hashes.
newtype TestBody = TestBody [TxHash]

instance ReferencesTxsByHash TestBody where
  foldTxReferences f z (TestBody hs) = List.foldl' f z hs

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
  deriving Show

-- | Apply an op, returning the announcement's eviction sets (the only
-- observable output of an op) when it is one.
applyOp :: H -> Op -> IO (Maybe (Set EbHash, Set TxHash))
applyOp h op = case op of
  OpAnnounce s r e -> Just <$> insertAnnouncement h (SlotNo s) (rbhOf r) (ebhOf e)
  OpBody e ts -> insertBody h (ebhOf e) (TestBody (map txhOf ts)) >> pure Nothing
  OpUnapplied ts ->
    withLockedInsertUnappliedTx h (\z step -> foldM (\acc t -> step acc (txhOf t) ()) z ts)
      >> pure Nothing
  OpApplied ts ->
    withLockedInsertAppliedTx h (\z step -> foldM (\acc t -> step acc (txhOf t) ()) z ts)
      >> pure Nothing

sweepLookup :: H -> [Word8] -> IO [Maybe (Either () ())]
sweepLookup h txs = withLookupTx h (\look -> mapM (look . txhOf) txs)

genOps :: Gen [Op]
genOps = do
  n <- chooseInt (0, 400)
  vectorOf n genOp
 where
  genOp =
    frequency
      [ (3, OpAnnounce <$> gen 1 300 <*> gen 1 3 <*> gen 1 20)
      , (2, OpBody <$> gen 1 20 <*> listOf (gen 1 100))
      , (2, OpUnapplied <$> listOf (gen 1 100))
      , (2, OpApplied <$> listOf (gen 1 100))
      ]
  gen :: Num a => Int -> Int -> Gen a
  gen lo hi = fromIntegral <$> chooseInt (lo, hi)

prop_equiv :: Property
prop_equiv = forAllShrink genOps (shrinkList (const [])) $ \ops -> ioProperty $ do
  hp <- newPureLeiosTxCache
  hm <- newHashTableLeiosTxCache 10 0xD1CED00DFEEDFACE 0x0123456789ABCDEF
  resP <- mapM (applyOp hp) ops
  resM <- mapM (applyOp hm) ops
  sweepP <- sweepLookup hp allTxs
  sweepM <- sweepLookup hm allTxs
  pure (resP === resM .&&. sweepP === sweepM)
 where
  allTxs = [1 .. 100]
