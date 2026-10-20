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
import Cardano.Crypto.KES.Locked (deserialiseSignKeyKESFromLockedBuf)
import Cardano.Crypto.Libsodium.MLockedSeed
  ( mlockedSeedFinalize
  , mlockedSeedNewRandom
  )
import Cardano.Crypto.Libsodium.Memory
  ( finalizeMLockedForeignPtr
  , withMLockedForeignPtr
  )
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Ledger.Keys (DSIGN)
import Cardano.Protocol.Crypto (KES, StandardCrypto)
import Cardano.Protocol.TPraos.OCert
  ( KESPeriod (..)
  , OCert (..)
  , OCertSignable (..)
  )
import Control.Monad (forM_, void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Ouroboros.Consensus.Block.Forging (UpdateInfo (..))
import Ouroboros.Consensus.Protocol.Ledger.HotKey
  ( HotKey (..)
  , KESInfo (..)
  , KESPersistCallback
  )
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
    , testCase "persist callback runs once per advancing evolve" test_persistCallback
    , testCase "persist callback is not run when the key did not change" test_persistCallbackNoChange
    , testCase "persist callback sees the final key of a multi-step evolve" test_persistCallbackMultiStep
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
withTestHotKey = withTestHotKeyPersist Nothing

-- | As 'withTestHotKey', with an optional persist callback.
withTestHotKeyPersist ::
  Maybe (KESPersistCallback IO) ->
  (HotKey StandardCrypto IO -> KES.VerKeyKES TestKES -> IO a) ->
  IO a
withTestHotKeyPersist callback k = do
  seed <- mlockedSeedNewRandom
  sk <- KES.genKeyKES @TestKES seed
  mlockedSeedFinalize seed
  vk <- KES.deriveVerKeyKES sk
  -- The hot key takes ownership of @sk@ and erases it when poisoned.
  hotKey <-
    HotKey.mkHotKeyAtEvolutionWithPersist callback 0 (mkOCert vk) sk startPeriod maxKESEvolutions
  r <- k hotKey vk
  finalize hotKey
  pure r

-- | A persist callback that records, per call, the period it was given and
-- whether the buffer held a key that signs for the verification key at the
-- evolution that period amounts to. It takes ownership of the buffer, as the
-- contract requires.
recordingCallback ::
  KES.VerKeyKES TestKES -> IO (KESPersistCallback IO, IO [(KESPeriod, Bool)])
recordingCallback vk = do
  calls <- newIORef []
  let msg = BS.pack [7, 8, 9] :: ByteString
      callback period buf len = do
        let evolution = unKESPeriod period - unKESPeriod startPeriod
        sk <- withMLockedForeignPtr buf $ \ptr -> deserialiseSignKeyKESFromLockedBuf @TestKES ptr len
        sig <- KES.signKES () evolution msg sk
        KES.forgetSignKeyKES sk
        finalizeMLockedForeignPtr buf
        let verifies = KES.verifyKES () vk evolution msg sig == Right ()
        atomicModifyIORef' calls (\cs -> (cs ++ [(period, verifies)], ()))
  pure (callback, readIORef calls)

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

{-------------------------------------------------------------------------------
  Persisting
-------------------------------------------------------------------------------}

-- | Run a test with a fresh hot key at evolution 0 whose persist callback
-- records its calls (see 'recordingCallback').
withRecordingHotKey ::
  (HotKey StandardCrypto IO -> IO [(KESPeriod, Bool)] -> IO a) -> IO a
withRecordingHotKey k = do
  seed <- mlockedSeedNewRandom
  sk <- KES.genKeyKES @TestKES seed
  mlockedSeedFinalize seed
  vk <- KES.deriveVerKeyKES sk
  (callback, getCalls) <- recordingCallback vk
  hotKey <-
    HotKey.mkHotKeyAtEvolutionWithPersist (Just callback) 0 (mkOCert vk) sk startPeriod maxKESEvolutions
  r <- k hotKey getCalls
  finalize hotKey
  pure r

-- | Evolving 10 -> 12 invokes the callback exactly once, with period 12 and
-- a buffer that deserialises to the key at evolution 2.
test_persistCallback :: Assertion
test_persistCallback = withRecordingHotKey $ \hotKey getCalls -> do
  getCalls >>= assertEqual "no call before any evolution" []
  void $ evolveUpdated hotKey 12
  getCalls >>= assertEqual "one call, period 12, key at evolution 2" [(KESPeriod 12, True)]
  -- The hot key still has its own key and signs with it.
  getInfo hotKey >>= assertEvolution "hot key at evolution 2" 2
  isPoisoned hotKey >>= assertBool "not poisoned" . not

-- | No call for an evolve that did not advance the key: the same period, a
-- period the key is already past, a period before the start, a period past
-- the end (which poisons the key), and any evolve of a poisoned key.
test_persistCallbackNoChange :: Assertion
test_persistCallbackNoChange = withRecordingHotKey $ \hotKey getCalls -> do
  void $ evolveUpdated hotKey 12
  let afterFirst = [(KESPeriod 12, True)]
  getCalls >>= assertEqual "one call for 10 -> 12" afterFirst

  void $ evolveUpdated hotKey 12
  getCalls >>= assertEqual "no call for the same period" afterFirst

  evolve hotKey (KESPeriod 11) >>= \case
    UpdateFailed HotKey.KESKeyFromTheFuture{} -> pure ()
    other -> assertFailure $ "expected KESKeyFromTheFuture, got " <> show other
  getCalls >>= assertEqual "no call for a key from the future" afterFirst

  void $ evolveUpdated hotKey 5
  getCalls >>= assertEqual "no call for a period before the start" afterFirst

  evolve hotKey (KESPeriod (unKESPeriod startPeriod + maxKESEvolutions)) >>= \case
    UpdateFailed HotKey.KESCouldNotEvolve{} -> pure ()
    other -> assertFailure $ "expected KESCouldNotEvolve, got " <> show other
  isPoisoned hotKey >>= assertBool "poisoned after the end"
  getCalls >>= assertEqual "no call for a period past the end" afterFirst

  evolve hotKey (KESPeriod 13) >>= \case
    UpdateFailed HotKey.KESKeyAlreadyPoisoned{} -> pure ()
    other -> assertFailure $ "expected KESKeyAlreadyPoisoned, got " <> show other
  getCalls >>= assertEqual "no call for a poisoned key" afterFirst

-- | A multi-step evolve (12 -> 20) invokes the callback once, with the final
-- period and the final key.
test_persistCallbackMultiStep :: Assertion
test_persistCallbackMultiStep = withRecordingHotKey $ \hotKey getCalls -> do
  void $ evolveUpdated hotKey 12
  void $ evolveUpdated hotKey 20
  getCalls
    >>= assertEqual "one call per advancing evolve" [(KESPeriod 12, True), (KESPeriod 20, True)]
  forM_ [21, 22] $ \p -> void (evolveUpdated hotKey p)
  getCalls
    >>= assertEqual
      "single steps call once each"
      [(KESPeriod 12, True), (KESPeriod 20, True), (KESPeriod 21, True), (KESPeriod 22, True)]
