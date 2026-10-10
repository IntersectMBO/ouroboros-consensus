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
-- For Cardano, same-era comparisons use the era's own Eq/Ord and cross-era
-- comparisons go through PackedBytes; neither is the raw-hash reference, so
-- every class is a real check. The cross-era cells in particular check that
-- packed-word order agrees with raw-byte order across the Byron and Shelley
-- representations.
--
-- The test builds a txid in every era for each probe hash, then compares every
-- id with every other and checks the result against the reference. The probe
-- hashes (see 'hashes') put a single 1 at each byte in turn, so a wrong byte
-- order anywhere makes the era's 'Ord' disagree with the reference.
module Test.Consensus.Cardano.TxId (tests) where

import Cardano.Protocol.Crypto (StandardCrypto)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.SOP (Proxy (..), lengthSList)
import Data.Word (Word8)
import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.HardFork.Combinator.Abstract (CanHardFork, rawHashNS)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (OneEraGenTxId (..))
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Test.Consensus.Cardano.GenTxIdBuilders (oneEraGenTxIds)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Test.Tasty.QuickCheck
  ( Gen
  , Property
  , arbitrary
  , chooseInt
  , forAll
  , frequency
  , testProperty
  , vectorOf
  , (.&&.)
  , (===)
  )

tests :: TestTree
tests =
  testGroup
    "TxIdEqOrd"
    [ testCase "Eq/Ord agree with the raw-hash reference" $
        mapM_ check [(i, j, h1, h2) | i <- eras, j <- eras, h1 <- hashes, h2 <- hashes]
    , testProperty
        "Eq/Ord agree with the raw-hash reference on random hashes"
        prop_agreeWithReference
    ]

-- | The reference semantics for the txid @Eq@\/@Ord@ instances: the raw hash
-- bytes, era ignored. Imported from the combinator, not copied, so the test and
-- the non-optimizing instances share one reference.
refRawHash :: CanHardFork xs => OneEraGenTxId xs -> ShortByteString
refRawHash = rawHashNS . getOneEraGenTxId

-- | The txid built in era position @e@ from hash bytes @bs@.
mkAt :: Int -> ShortByteString -> OneEraGenTxId (CardanoEras StandardCrypto)
mkAt e bs = oneEraGenTxIds bs !! e

-- | Check one era/hash combination against the reference, for @compare@ and
-- @==@. The tuple is shown verbatim in the failure message.
check :: (Int, Int, ShortByteString, ShortByteString) -> Assertion
check c@(i, j, h1, h2) = do
  assertEqual (show c ++ " [compare]") (compare (refRawHash a) (refRawHash b)) (compare a b)
  assertEqual (show c ++ " [==]") (refRawHash a == refRawHash b) (a == b)
 where
  a = mkAt i h1
  b = mkAt j h2

-- | 'compare' and '==' on random era/hash pairs must agree with the raw-hash
-- reference.
prop_agreeWithReference :: Property
prop_agreeWithReference =
  forAll genEra $ \i ->
    forAll genEra $ \j ->
      forAll genHash $ \h1 ->
        -- @h2@ equals @h1@ 1 time in 5; independent random hashes are almost
        -- never equal, so otherwise '==' would never be tested against True.
        forAll (frequency [(1, pure h1), (4, genHash)]) $ \h2 ->
          let a = mkAt i h1
              b = mkAt j h2
           in compare a b === compare (refRawHash a) (refRawHash b)
                .&&. (a == b) === (refRawHash a == refRawHash b)

-- | A random 32-byte hash.
genHash :: Gen ShortByteString
genHash = SBS.pack <$> vectorOf 32 arbitrary

-- | A random era position.
genEra :: Gen Int
genEra = chooseInt (0, lengthSList (Proxy @(CardanoEras StandardCrypto)) - 1)

-- | The probe hashes: the all-zero hash, plus one hash per byte position, each
-- 0 everywhere except a 1 at that byte. Probing every byte checks that a wrong
-- byte order anywhere makes the era's 'Ord' disagree with the raw-byte
-- reference.
hashes :: [ShortByteString]
hashes = zeros : [byteAt p 1 | p <- [0 .. 31]]

eras :: [Int]
eras = [0 .. lengthSList (Proxy @(CardanoEras StandardCrypto)) - 1]

zeros :: ShortByteString
zeros = SBS.pack (replicate 32 0)

-- | 32 bytes, all zero except position @p@ set to @v@.
byteAt :: Int -> Word8 -> ShortByteString
byteAt p v = SBS.pack [if i == p then v else 0 | i <- [0 .. 31]]
