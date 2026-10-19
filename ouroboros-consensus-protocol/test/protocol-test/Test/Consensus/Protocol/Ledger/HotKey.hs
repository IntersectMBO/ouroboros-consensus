{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the KES 'HotKey'.
--
-- These run in 'IO' on a real (mlocked) 'StandardCrypto' KES key, because
-- 'KES.forgetSignKeyKES' is a no-op under @IOSim@: the effects we care about
-- (refusing to evolve backwards, poisoning on finalisation) are observed
-- through the 'HotKey' API, not through memory.
module Test.Consensus.Protocol.Ledger.HotKey (tests) where

import Cardano.Crypto.DSIGN (genKeyDSIGN, signedDSIGN)
import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Libsodium.MLockedSeed
  ( mlockedSeedFinalize
  , mlockedSeedNewRandom
  )
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Ledger.Keys (DSIGN)
import Cardano.Protocol.Crypto (KES, StandardCrypto)
import Cardano.Protocol.TPraos.OCert
  ( KESPeriod (..)
  , OCert (..)
  , OCertSignable (..)
  )
import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Ouroboros.Consensus.Block.Forging (UpdateInfo (..))
import Ouroboros.Consensus.Protocol.Ledger.HotKey (HotKey (..), KESInfo (..))
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "HotKey"
    [ testCase "evolve forward" test_evolveForward
    , testCase "refuse a key from the future" test_refuseKeyFromTheFuture
    , testCase "before the start period is unchanged" test_beforeStartUnchanged
    , testCase "finalize poisons a file-mode key" test_finalizePoisons
    , testCase "finalize is idempotent" test_finalizeIdempotent
    ]

{-------------------------------------------------------------------------------
  Setup
-------------------------------------------------------------------------------}

type TestKES = KES StandardCrypto

-- | Start period of the key under test.
startPeriod :: KESPeriod
startPeriod = KESPeriod 10

-- | Maximum number of evolutions of a 'StandardCrypto' (Sum6KES) key.
maxKESEvolutions :: Integral a => a
maxKESEvolutions = 64

-- | Create a fresh hot key at evolution 0 for 'startPeriod', run the test
-- against it, and finalise it afterwards. The derived verification key is
-- passed along so tests can check signatures.
withTestHotKey ::
  (HotKey StandardCrypto IO -> KES.VerKeyKES TestKES -> IO a) -> IO a
withTestHotKey k = do
  seed <- mlockedSeedNewRandom
  sk <- KES.genKeyKES @TestKES seed
  mlockedSeedFinalize seed
  vk <- KES.deriveVerKeyKES sk
  -- The hot key takes ownership of @sk@ and erases it when poisoned.
  hotKey <- HotKey.mkHotKey (mkOCert vk) sk startPeriod maxKESEvolutions
  r <- k hotKey vk
  finalize hotKey
  pure r

-- | An operational certificate for the given hot verification key. Only its
-- shape matters to the 'HotKey'; the cold key is a fixed throwaway key.
mkOCert :: KES.VerKeyKES TestKES -> OCert StandardCrypto
mkOCert vkHot =
  OCert
    { ocertVkHot = vkHot
    , ocertN = 0
    , ocertKESPeriod = startPeriod
    , ocertSigma = signedDSIGN () (OCertSignable vkHot 0 startPeriod) coldKey
    }
 where
  coldKey = genKeyDSIGN @DSIGN (mkSeedFromBytes (BS.replicate 32 0))

-- | Evolve and expect success, returning the resulting 'KESInfo'.
evolveUpdated :: HotKey c IO -> Word -> IO KESInfo
evolveUpdated hotKey period =
  evolve hotKey (KESPeriod period) >>= \case
    Updated info -> pure info
    UpdateFailed err ->
      assertFailure $
        "expected Updated for period " <> show period <> ", got " <> show err

assertEvolution :: String -> Word -> KESInfo -> Assertion
assertEvolution what expected KESInfo{kesEvolution} =
  assertEqual what expected kesEvolution

{-------------------------------------------------------------------------------
  Evolving
-------------------------------------------------------------------------------}

-- | The ordinary case: evolving forward updates the evolution.
test_evolveForward :: Assertion
test_evolveForward = withTestHotKey $ \hotKey _vk -> do
  info0 <- getInfo hotKey
  assertEvolution "fresh key" 0 info0
  assertEqual "start period" startPeriod (kesStartPeriod info0)
  assertEqual
    "end period"
    (KESPeriod (unKESPeriod startPeriod + maxKESEvolutions))
    (kesEndPeriod info0)

  evolveUpdated hotKey 12 >>= assertEvolution "after evolving to 12" 2
  getInfo hotKey >>= assertEvolution "getInfo after evolving to 12" 2
  isPoisoned hotKey >>= assertBool "not poisoned" . not

-- | Asking for a period below the key's current one is refused with
-- 'HotKey.KESKeyFromTheFuture', the key is left untouched and is still usable
-- once the clock catches up.
test_refuseKeyFromTheFuture :: Assertion
test_refuseKeyFromTheFuture = withTestHotKey $ \hotKey vk -> do
  void $ evolveUpdated hotKey 12
  infoBefore <- getInfo hotKey
  assertEvolution "at evolution 2 before the test" 2 infoBefore

  -- Period 11 is in range, but below the key's current period 12.
  evolve hotKey (KESPeriod 11) >>= \case
    UpdateFailed (HotKey.KESKeyFromTheFuture info target) -> do
      assertEqual "target period in the error" (KESPeriod 11) target
      assertEvolution "info in the error is the current info" 2 info
      assertEqual "start period in the error" startPeriod (kesStartPeriod info)
    UpdateFailed err -> assertFailure $ "unexpected error: " <> show err
    Updated info ->
      assertFailure $ "expected UpdateFailed, got Updated with " <> show info

  -- Nothing changed and the key is not poisoned.
  infoAfter <- getInfo hotKey
  assertEqual "info unchanged after refusal" (show infoBefore) (show infoAfter)
  isPoisoned hotKey >>= assertBool "not poisoned after refusal" . not
  getOCertMaybe hotKey >>= \case
    Nothing -> assertFailure "OCert gone after refusal"
    Just _ -> pure ()

  -- The same period is still 'Updated'.
  evolveUpdated hotKey 12 >>= assertEvolution "same period again" 2

  -- Once the clock catches up, the key evolves and signs as usual.
  evolveUpdated hotKey 13 >>= assertEvolution "caught up to 13" 3
  let msg = BS.pack [1, 2, 3] :: ByteString
  signed <- HotKey.sign hotKey msg
  assertEqual
    "signature at evolution 3 verifies"
    (Right ())
    (KES.verifySignedKES () vk 3 msg signed)

-- | A period before the start period of the key is not an error: the key is
-- not evolved and 'Updated' is returned (the forging loop rejects such slots
-- through 'checkCanForge' instead).
test_beforeStartUnchanged :: Assertion
test_beforeStartUnchanged = withTestHotKey $ \hotKey _vk -> do
  void $ evolveUpdated hotKey 12
  evolveUpdated hotKey 5 >>= assertEvolution "before start period" 2
  getInfo hotKey >>= assertEvolution "getInfo after period before start" 2
  isPoisoned hotKey >>= assertBool "not poisoned" . not

{-------------------------------------------------------------------------------
  Finalising
-------------------------------------------------------------------------------}

-- | A hot key without a key producer (the file-based credentials) poisons its
-- key on 'finalize', so the previous key is erased deterministically when the
-- node reloads its credentials.
test_finalizePoisons :: Assertion
test_finalizePoisons = withTestHotKey $ \hotKey _vk -> do
  void $ evolveUpdated hotKey 12
  isPoisoned hotKey >>= assertBool "not poisoned before finalize" . not

  finalize hotKey

  isPoisoned hotKey >>= assertBool "poisoned after finalize"
  getOCertMaybe hotKey >>= \case
    Nothing -> pure ()
    Just _ -> assertFailure "OCert still available after finalize"
  evolve hotKey (KESPeriod 13) >>= \case
    UpdateFailed (HotKey.KESKeyAlreadyPoisoned info target) -> do
      assertEqual "target period in the error" (KESPeriod 13) target
      assertEvolution "info in the error is the last info" 2 info
    UpdateFailed err -> assertFailure $ "unexpected error: " <> show err
    Updated info ->
      assertFailure $ "expected UpdateFailed, got Updated with " <> show info

-- | Finalising twice is harmless (the second call finds a poisoned key).
test_finalizeIdempotent :: Assertion
test_finalizeIdempotent = withTestHotKey $ \hotKey _vk -> do
  finalize hotKey
  finalize hotKey
  isPoisoned hotKey >>= assertBool "poisoned after double finalize"
