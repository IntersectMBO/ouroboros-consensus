{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

-- | The allocation-free @Eq@\/@Ord@ instances for 'OneEraGenTxId' must agree
-- with the raw-hash reference: order by hash bytes, ignore the era.
--
-- The comparison distinguishes four classes of input:
--
--   * same era, equal hashes
--   * same era, unequal hashes
--   * different eras, equal hashes
--   * different eras, unequal hashes
--
-- Only the same-era classes can disagree with the reference. The cross-era
-- branch falls back to the reference, so it agrees by construction.
--
-- The test builds a txid in every era for each of a few hashes, then compares
-- every id with every other and checks the result against the reference. The
-- hashes (see 'positions') are shaped to catch a word whose bytes would compare
-- in the wrong order.
module Test.Consensus.Cardano.TxId (tests) where

import Cardano.Protocol.Crypto (StandardCrypto)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.SOP (Proxy (..), lengthSList)
import Data.SOP.BasicFunctors (K (..))
import Data.SOP.Strict (hcmap, hcollapse)
import Data.Word (Word8)
import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.HardFork.Combinator.Abstract (CanHardFork, proxySingle)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (OneEraGenTxId (..))
import Ouroboros.Consensus.Ledger.SupportsMempool (toRawTxIdHash)
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.TypeFamilyWrappers (unwrapGenTxId)
import Test.Consensus.Cardano.GenTxIdBuilders (oneEraGenTxIds)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "TxIdEqOrd"
    [ testCase "Eq/Ord agree with the raw-hash reference" $
        mapM_ check [(i, j, h1, h2) | i <- eras, j <- eras, h1 <- hashes, h2 <- hashes]
    ]

-- | Copy of the (deliberately unexported) raw-hash walk from
-- "Ouroboros.Consensus.HardFork.Combinator.AcrossEras". It is the reference
-- semantics for the txid @Eq@\/@Ord@ instances: the raw hash bytes, era
-- ignored. Kept in sync with that module by review.
refRawHash :: CanHardFork xs => OneEraGenTxId xs -> ShortByteString
refRawHash =
  hcollapse . hcmap proxySingle (K . toRawTxIdHash . unwrapGenTxId) . getOneEraGenTxId

-- | Check one era/hash combination against the reference, for @compare@ and
-- @==@. The tuple is shown verbatim in the failure message.
check :: (Int, Int, ShortByteString, ShortByteString) -> Assertion
check c@(i, j, h1, h2) = do
  assertEqual (show c ++ " [compare]") (compare (refRawHash a) (refRawHash b)) (compare a b)
  assertEqual (show c ++ " [==]") (refRawHash a == refRawHash b) (a == b)
 where
  mkAt :: Int -> ShortByteString -> OneEraGenTxId (CardanoEras StandardCrypto)
  mkAt e bs = oneEraGenTxIds bs !! e
  a = mkAt i h1
  b = mkAt j h2

-- | The probe hashes: the all-zero hash, plus, for each entry in 'positions', a
-- hash with a 1 at that byte and 0 everywhere else.
hashes :: [ShortByteString]
hashes = zeros : [byteAt p 1 | p <- positions]

-- | Byte positions for the probe hashes. A 32-byte hash is four 64-bit words.
-- Each era's 'Ord' compares those words; the reference compares the raw bytes.
-- The two agree only if each word holds its bytes in the order we expect.
--
-- For each position here, 'hashes' builds a hash whose bytes are all 0 except
-- byte p, which is 1. The positions are the first and last byte of each of the
-- four words, so if a word ordered its bytes the wrong way, two of these hashes
-- would compare differently under the era's 'Ord' than under the reference.
positions :: [Int]
positions = [0, 7, 8, 15, 16, 23, 24, 31]

eras :: [Int]
eras = [0 .. lengthSList (Proxy @(CardanoEras StandardCrypto)) - 1]

zeros :: ShortByteString
zeros = SBS.pack (replicate 32 0)

-- | 32 bytes, all zero except position @p@ set to @v@.
byteAt :: Int -> Word8 -> ShortByteString
byteAt p v = SBS.pack [if i == p then v else 0 | i <- [0 .. 31]]
