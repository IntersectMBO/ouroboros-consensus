{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for loading file-based Praos credentials
-- ('PraosCredentialsFile') into a 'HotKey'.
--
-- These run in 'IO' on a temporary directory with real (mlocked)
-- 'StandardCrypto' keys, because the file read capability of
-- 'MonadKESAgent' only exists in 'IO'. The key files are generated in the
-- exact layout that @cardano-cli node key-gen-KES@ writes (the layout of
-- @kes-agent/fixtures/kes.skey@): a pretty-printed three-field text envelope
-- whose @cborHex@ is one CBOR byte string holding the raw key. The
-- period-carrying files the node persists have the same layout with the
-- @KesSigningKeyWithPeriod_@ type and a two-element CBOR array (the absolute
-- KES period, then the byte string) as @cborHex@.
module Test.Consensus.Protocol.Praos.FileCredentials (tests) where

import Cardano.Crypto.DSIGN (genKeyDSIGN, signedDSIGN)
import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.KES.Locked (deserialiseSignKeyKESFromLockedBuf)
import Cardano.Crypto.Libsodium.Hex (HexDecodeError (..))
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
import Control.Exception (IOException, try)
import Control.Monad (foldM, forM_)
import Control.Tracer (nullTracer)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import Ouroboros.Consensus.Block.Forging (UpdateInfo (..))
import Ouroboros.Consensus.Protocol.Ledger.HotKey (HotKey (..), KESInfo (..))
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import Ouroboros.Consensus.Protocol.Praos.AgentClient (KESPersistHook (..))
import Ouroboros.Consensus.Protocol.Praos.Common
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "PraosCredentialsFile"
    [ testCase "loads a cardano-cli key file" test_loadsCardanoCliLayout
    , testCase "accepts other JSON whitespace and field order" test_loadsOtherLayouts
    , testCase "loads a period-carrying key file at its evolution" test_loadsWithPeriodType
    , testCase "invokes the persist hook after each evolution" test_persistHook
    , testCase "rejects an unknown envelope type" test_rejectsUnknownType
    , testCase "rejects a period before the opcert's start period" test_rejectsPeriodBeforeOCert
    , testCase "rejects a period beyond the opcert's range" test_rejectsEvolutionTooHigh
    , testCase "rejects a period that is not the key's evolution" test_rejectsStructureMismatch
    , testCase "rejects an evolved key in an evolution-zero envelope" test_rejectsEvolvedKeyAsZero
    , testCase "rejects a period-carrying cborHex of the wrong shape" test_rejectsBadCborWithPeriod
    , testCase
        "rejects a key of the wrong length in a period-carrying file"
        test_rejectsWrongLengthWithPeriod
    , testCase "rejects odd-length hex" test_rejectsOddHex
    , testCase "rejects a non-hex byte" test_rejectsNonHex
    , testCase "rejects a key of the wrong length" test_rejectsWrongLength
    , testCase "rejects a cborHex that is not one byte string" test_rejectsBadCbor
    , testCase "rejects a missing field" test_rejectsMissingField
    , testCase "rejects anything but the three-field object" test_rejectsNotAnObject
    , testCase "rejects a key that is not the opcert's hot key" test_rejectsMismatchedOCert
    , testCase "a missing file is an IOError" test_missingFile
    ]

{-------------------------------------------------------------------------------
  Fixtures
-------------------------------------------------------------------------------}

type TestKES = KES StandardCrypto

startPeriod :: KESPeriod
startPeriod = KESPeriod 10

-- | Maximum number of evolutions of a 'StandardCrypto' (Sum6KES) key.
maxKESEvolutions :: Integral a => a
maxKESEvolutions = 64

-- | Raw serialised size of a 'StandardCrypto' signing key.
keySize :: Int
keySize = fromIntegral (KES.signKeySizeKES (Proxy @TestKES))

-- | A generated key: its opcert, verification key, the evolution it has been
-- evolved to and the hex text of its raw serialisation at that evolution
-- (the test's own, heap-allocated copy; the loader under test never makes
-- one).
data Fixture = Fixture
  { fxOCert :: OCert StandardCrypto
  , fxVerKey :: KES.VerKeyKES TestKES
  , fxEvolution :: Word
  , fxRawHex :: ByteString
  }

-- | A fresh key at evolution 0.
genFixture :: IO Fixture
genFixture = genFixtureAt 0

-- | A fresh key evolved to the given evolution with 'KES.updateKES', the
-- intermediate keys erased, as the node's hot key does.
genFixtureAt :: Word -> IO Fixture
genFixtureAt evolution = do
  seed <- mlockedSeedNewRandom
  sk0 <- KES.genKeyKES @TestKES seed
  mlockedSeedFinalize seed
  vk <- KES.deriveVerKeyKES sk0
  sk <- foldM step sk0 (take (fromIntegral evolution) [0 ..])
  raw <- KES.rawSerialiseSignKeyKES sk
  KES.forgetSignKeyKES sk
  pure
    Fixture
      { fxOCert = mkOCert vk
      , fxVerKey = vk
      , fxEvolution = evolution
      , fxRawHex = B16.encode raw
      }
 where
  step sk e =
    KES.updateKES () sk e >>= \case
      Nothing -> assertFailure $ "could not evolve the fixture key past evolution " <> show e
      Just sk' -> KES.forgetSignKeyKES sk >> pure sk'

-- | The absolute KES period of a fixture at its evolution.
fxPeriod :: Fixture -> Word64
fxPeriod fx = fromIntegral (unKESPeriod startPeriod + fxEvolution fx)

-- | An operational certificate for the given hot verification key. Only its
-- shape matters here; the cold key is a fixed throwaway key.
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

evolutionZeroType :: ByteString
evolutionZeroType = "KesSigningKey_" <> BC.pack (KES.algorithmNameKES (Proxy @TestKES))

withPeriodType :: ByteString
withPeriodType = "KesSigningKeyWithPeriod_" <> BC.pack (KES.algorithmNameKES (Proxy @TestKES))

-- | The header of a definite-length CBOR byte string of the given length.
cborBytesHeader :: Int -> ByteString
cborBytesHeader n
  | n < 24 = BS.pack [0x40 + fromIntegral n]
  | n < 256 = BS.pack [0x58, fromIntegral n]
  | otherwise = BS.pack [0x59, fromIntegral (n `div` 256), fromIntegral (n `mod` 256)]

-- | A definite-length CBOR unsigned integer (major type 0), shortest form.
cborUInt :: Word64 -> ByteString
cborUInt n
  | n < 24 = BS.pack [fromIntegral n]
  | n < 0x100 = BS.pack [0x18, fromIntegral n]
  | n < 0x10000 = BS.pack [0x19, fromIntegral (n `div` 0x100), fromIntegral n]
  | otherwise = BS.pack (0x1a : [fromIntegral (n `div` (0x100 ^ k)) | k <- [3, 2, 1, 0 :: Int]])

-- | The @cborHex@ value for a raw key given as hex: a CBOR byte string.
cborHexOf :: ByteString -> ByteString
cborHexOf rawHex = B16.encode (cborBytesHeader (BS.length rawHex `div` 2)) <> rawHex

-- | The @cborHex@ value of a period-carrying file for a raw key given as hex:
-- a two-element CBOR array of the absolute period and the byte string.
cborHexWithPeriod :: Word64 -> ByteString -> ByteString
cborHexWithPeriod period rawHex = B16.encode (BS.singleton 0x82 <> cborUInt period) <> cborHexOf rawHex

-- | A text envelope in the layout @cardano-cli@ writes (and that
-- @kes-agent/fixtures/kes.skey@ has): four-space indentation, the fields in
-- this order, a trailing newline.
cardanoCliEnvelope :: ByteString -> ByteString -> ByteString
cardanoCliEnvelope ty cborHex =
  BS.concat
    [ "{\n"
    , "    \"type\": \"" <> ty <> "\",\n"
    , "    \"description\": \"KES Signing Key\",\n"
    , "    \"cborHex\": \"" <> cborHex <> "\"\n"
    , "}\n"
    ]

-- | The evolution-zero envelope of a fixture key (which should be at
-- evolution 0 for the file to be valid).
keyFile :: Fixture -> ByteString
keyFile fx = cardanoCliEnvelope evolutionZeroType (cborHexOf (fxRawHex fx))

-- | The period-carrying envelope of a fixture key, claiming the given
-- absolute period.
keyFileWithPeriod :: Word64 -> Fixture -> ByteString
keyFileWithPeriod period fx = cardanoCliEnvelope withPeriodType (cborHexWithPeriod period (fxRawHex fx))

-- | Write the contents to @kes.skey@ in a fresh temporary directory and run
-- the test with its path.
withKeyFile :: ByteString -> (FilePath -> IO a) -> IO a
withKeyFile contents k =
  withSystemTempDirectory "protocol-test-kes" $ \dir -> do
    let path = dir </> "kes.skey"
    BS.writeFile path contents
    k path

load :: OCert StandardCrypto -> FilePath -> IO (HotKey StandardCrypto IO)
load ocert = loadWith ocert Nothing

loadWith ::
  OCert StandardCrypto -> Maybe KESPersistHook -> FilePath -> IO (HotKey StandardCrypto IO)
loadWith ocert hook path =
  instantiatePraosCredentials
    maxKESEvolutions
    nullTracer
    (PraosCredentialsFile ocert path hook)

{-------------------------------------------------------------------------------
  Assertions
-------------------------------------------------------------------------------}

-- | Load the file, check that the resulting hot key is the fixture's key at
-- the fixture's evolution, that it signs there and after one more evolution,
-- and finalise it.
assertLoadsFixture :: Fixture -> FilePath -> Assertion
assertLoadsFixture fx path = do
  hotKey <- load (fxOCert fx) path

  isPoisoned hotKey >>= assertBool "not poisoned" . not
  KESInfo{kesStartPeriod, kesEndPeriod, kesEvolution} <- getInfo hotKey
  assertEqual "start period" startPeriod kesStartPeriod
  assertEqual
    "end period"
    (KESPeriod (unKESPeriod startPeriod + maxKESEvolutions))
    kesEndPeriod
  assertEqual "evolution" (fxEvolution fx) kesEvolution
  getOCertMaybe hotKey >>= \case
    Nothing -> assertFailure "no OCert"
    Just ocert -> assertEqual "OCert hot key" (fxVerKey fx) (ocertVkHot ocert)

  -- The key is the one we generated: it signs at its evolution and, when
  -- there is one, at the next evolution after evolving.
  let msg = BS.pack [1, 2, 3] :: ByteString
      e0 = fxEvolution fx
  signed0 <- HotKey.sign hotKey msg
  assertEqual
    ("signature at evolution " <> show e0 <> " verifies")
    (Right ())
    (KES.verifySignedKES () (fxVerKey fx) e0 msg signed0)
  forM_ [e0 + 1 | e0 + 1 < maxKESEvolutions] $ \e1 -> do
    evolve hotKey (KESPeriod (unKESPeriod startPeriod + e1)) >>= \case
      Updated KESInfo{kesEvolution = e} -> assertEqual "evolved" e1 e
      UpdateFailed err -> assertFailure $ "evolve failed: " <> show err
    signed1 <- HotKey.sign hotKey msg
    assertEqual
      ("signature at evolution " <> show e1 <> " verifies")
      (Right ())
      (KES.verifySignedKES () (fxVerKey fx) e1 msg signed1)

  finalize hotKey
  isPoisoned hotKey >>= assertBool "poisoned after finalize"

-- | Load the file and expect a 'KESFileLoadError' with this reason, carrying
-- the path and none of the given file contents.
assertRejects :: OCert StandardCrypto -> KESFileLoadReason -> [ByteString] -> FilePath -> Assertion
assertRejects ocert expected secrets path =
  try (load ocert path) >>= \case
    Right hotKey -> do
      finalize hotKey
      assertFailure $ "expected " <> show expected <> ", but the file loaded"
    Left err@KESFileLoadError{kesFilePath, kesFileReason} -> do
      assertEqual "reason" expected kesFileReason
      assertEqual "path" path kesFilePath
      let shown = show err
      forM_ secrets $ \secret ->
        assertBool
          ("error does not echo file contents: " <> shown)
          (not (BC.unpack secret `isInfixOf` shown))

-- | The string values of a file: what must never appear in an error.
secretsOf :: Fixture -> [ByteString]
secretsOf fx =
  [fxRawHex fx, BS.take 32 (fxRawHex fx), "KES Signing Key", evolutionZeroType, withPeriodType]

{-------------------------------------------------------------------------------
  Loading
-------------------------------------------------------------------------------}

test_loadsCardanoCliLayout :: Assertion
test_loadsCardanoCliLayout = do
  fx <- genFixture
  assertEqual "type string" "KesSigningKey_ed25519_kes_2^6" evolutionZeroType
  assertEqual "key size" 608 keySize
  withKeyFile (keyFile fx) (assertLoadsFixture fx)

test_loadsOtherLayouts :: Assertion
test_loadsOtherLayouts = do
  fx <- genFixture
  let hex = cborHexOf (fxRawHex fx)
      compact =
        "{\"type\":\""
          <> evolutionZeroType
          <> "\",\"description\":\"KES Signing Key\",\"cborHex\":\""
          <> hex
          <> "\"}"
      reordered =
        BS.concat
          [ "\n\t{ \"cborHex\" :\r\n \"" <> hex <> "\" ,\n"
          , "  \"description\": \"\",\n"
          , "  \"type\":\"" <> evolutionZeroType <> "\"\n\n}\n\n"
          ]
      upperHex =
        cardanoCliEnvelope evolutionZeroType (BC.map toUpperHex hex)
  withKeyFile compact (assertLoadsFixture fx)
  withKeyFile reordered (assertLoadsFixture fx)
  withKeyFile upperHex (assertLoadsFixture fx)
 where
  toUpperHex c
    | c >= 'a' && c <= 'f' = toEnum (fromEnum c - 32)
    | otherwise = c

-- | A hot key loaded with a 'KESPersistHook' invokes it, through
-- 'runKESPersistHook', once per evolution that advanced the key, with the
-- key's new absolute period and a serialised copy of the evolved key.
test_persistHook :: Assertion
test_persistHook = do
  fx <- genFixture
  calls <- newIORef []
  let msg = BS.pack [4, 5, 6] :: ByteString
      hook = KESPersistHook $ \period buf len -> do
        -- The buffer is a serialised copy of the key at this period: it
        -- deserialises and signs for the fixture's verification key at the
        -- corresponding evolution.
        let evolution = unKESPeriod period - unKESPeriod startPeriod
        sk <- withMLockedForeignPtr buf $ \ptr -> deserialiseSignKeyKESFromLockedBuf @TestKES ptr len
        sig <- KES.signKES () evolution msg sk
        KES.forgetSignKeyKES sk
        finalizeMLockedForeignPtr buf
        let verifies = KES.verifyKES () (fxVerKey fx) evolution msg sig == Right ()
        atomicModifyIORef' calls (\cs -> (cs ++ [(period, verifies)], ()))
      expectCalls what expected = readIORef calls >>= assertEqual what expected
      start = unKESPeriod startPeriod
  withKeyFile (keyFile fx) $ \path -> do
    hotKey <- loadWith (fxOCert fx) (Just hook) path
    expectCalls "no call on load" []
    evolve hotKey (KESPeriod (start + 1)) >>= \case
      Updated KESInfo{kesEvolution = e} -> assertEqual "evolved to 1" 1 e
      UpdateFailed err -> assertFailure $ "evolve failed: " <> show err
    expectCalls "one call for the first evolution" [(KESPeriod (start + 1), True)]
    -- The same period again and a period before the start do not call.
    forM_ [start + 1, start - 3] $ \p ->
      evolve hotKey (KESPeriod p) >>= \case
        Updated _ -> pure ()
        UpdateFailed err -> assertFailure $ "evolve failed: " <> show err
    expectCalls "no call without an evolution" [(KESPeriod (start + 1), True)]
    -- A multi-step evolution calls once, with the final period.
    evolve hotKey (KESPeriod (start + 4)) >>= \case
      Updated KESInfo{kesEvolution = e} -> assertEqual "evolved to 4" 4 e
      UpdateFailed err -> assertFailure $ "evolve failed: " <> show err
    expectCalls
      "one call for the multi-step evolution"
      [(KESPeriod (start + 1), True), (KESPeriod (start + 4), True)]
    finalize hotKey

-- | A period-carrying file loads at the evolution its period amounts to,
-- for a key that really is at that evolution.
test_loadsWithPeriodType :: Assertion
test_loadsWithPeriodType = do
  assertEqual "type string" "KesSigningKeyWithPeriod_ed25519_kes_2^6" withPeriodType
  forM_ [0, 1, 2, 5, maxKESEvolutions - 1] $ \evolution -> do
    fx <- genFixtureAt evolution
    withKeyFile (keyFileWithPeriod (fxPeriod fx) fx) (assertLoadsFixture fx)

{-------------------------------------------------------------------------------
  Rejecting
-------------------------------------------------------------------------------}

test_rejectsUnknownType :: Assertion
test_rejectsUnknownType = do
  fx <- genFixture
  let file = cardanoCliEnvelope "KesSigningKey_bogus_kes_2^6" (cborHexOf (fxRawHex fx))
  withKeyFile file $
    assertRejects (fxOCert fx) (KESFileEnvelope EnvelopeUnknownType) ("bogus" : secretsOf fx)

test_rejectsPeriodBeforeOCert :: Assertion
test_rejectsPeriodBeforeOCert = do
  fx <- genFixture
  let start = fromIntegral (unKESPeriod startPeriod)
  forM_ [0, start - 1] $ \period ->
    withKeyFile (keyFileWithPeriod period fx) $
      assertRejects (fxOCert fx) (KESFilePeriodBeforeOCert period start) (secretsOf fx)

test_rejectsEvolutionTooHigh :: Assertion
test_rejectsEvolutionTooHigh = do
  fx <- genFixture
  let start = fromIntegral (unKESPeriod startPeriod)
  forM_ [maxKESEvolutions, maxKESEvolutions + 1, 1000] $ \evolution ->
    withKeyFile (keyFileWithPeriod (start + evolution) fx) $
      assertRejects
        (fxOCert fx)
        (KESFileEvolutionTooHigh evolution maxKESEvolutions)
        (secretsOf fx)

-- | A period that is in range but is not the evolution the key has really
-- been evolved to is rejected, naming both evolutions.
test_rejectsStructureMismatch :: Assertion
test_rejectsStructureMismatch = do
  -- A key at evolution 5 claiming 4 and 6, and a key at evolution 0 claiming
  -- 1 and the last evolution.
  fx5 <- genFixtureAt 5
  forM_ [4, 6] $ \claimed ->
    withKeyFile (keyFileWithPeriod (fxPeriod fx5 - 5 + claimed) fx5) $
      assertRejects (fxOCert fx5) (KESFileStructureMismatch claimed 5) (secretsOf fx5)
  fx0 <- genFixture
  forM_ [1, maxKESEvolutions - 1] $ \claimed ->
    withKeyFile (keyFileWithPeriod (fxPeriod fx0 + claimed) fx0) $
      assertRejects (fxOCert fx0) (KESFileStructureMismatch claimed 0) (secretsOf fx0)

-- | An evolved key in the evolution-zero envelope that cardano-cli writes is
-- not at evolution 0 either.
test_rejectsEvolvedKeyAsZero :: Assertion
test_rejectsEvolvedKeyAsZero = do
  fx <- genFixtureAt 3
  withKeyFile (keyFile fx) $
    assertRejects (fxOCert fx) (KESFileStructureMismatch 0 3) (secretsOf fx)

test_rejectsBadCborWithPeriod :: Assertion
test_rejectsBadCborWithPeriod = do
  fx <- genFixture
  let raw = fxRawHex fx
      bytes = cborHexOf raw
      period = B16.encode (cborUInt (fxPeriod fx))
      shapes =
        [ bytes -- the evolution-zero shape under the period-carrying type
        , "83" <> period <> bytes <> "00" -- arity 3
        , "81" <> bytes -- arity 1
        , "82" <> "29" <> bytes -- negative integer (-10) as the period
        , "82" <> "f94900" <> bytes -- half-precision float 10.0 as the period
        , "82" <> "623130" <> bytes -- the string "10" as the period
        , "82" <> "9f" <> period <> "ff" <> bytes -- indefinite-length period
        , "82" <> period <> "5f" <> bytes <> "ff" -- indefinite-length byte string
        , "82" <> period <> bytes <> "00" -- trailing byte
        , "82" <> period -- key missing
        ]
  forM_ shapes $ \cborHex ->
    withKeyFile (cardanoCliEnvelope withPeriodType cborHex) $
      assertRejects (fxOCert fx) (KESFileEnvelope EnvelopeBadCbor) (secretsOf fx)

test_rejectsWrongLengthWithPeriod :: Assertion
test_rejectsWrongLengthWithPeriod = do
  fx <- genFixture
  let file = cardanoCliEnvelope withPeriodType (cborHexWithPeriod (fxPeriod fx) (BS.take 64 (fxRawHex fx)))
  withKeyFile file $
    assertRejects (fxOCert fx) (KESFileEnvelope (EnvelopeBadLength keySize 32)) (secretsOf fx)

test_rejectsOddHex :: Assertion
test_rejectsOddHex = do
  fx <- genFixture
  let hex = cborHexOf (fxRawHex fx)
      file = cardanoCliEnvelope evolutionZeroType (BS.init hex)
  withKeyFile file $
    assertRejects
      (fxOCert fx)
      (KESFileEnvelope (EnvelopeBadHex (HexOddLength (BS.length hex - 1))))
      (secretsOf fx)

test_rejectsNonHex :: Assertion
test_rejectsNonHex = do
  fx <- genFixture
  let hex = cborHexOf (fxRawHex fx)
      -- replace the byte at offset 100 of the hex text by a non-hex character
      file = cardanoCliEnvelope evolutionZeroType (BS.take 100 hex <> "g" <> BS.drop 101 hex)
  withKeyFile file $
    assertRejects
      (fxOCert fx)
      (KESFileEnvelope (EnvelopeBadHex (HexInvalidByteAt 100)))
      (secretsOf fx)

test_rejectsWrongLength :: Assertion
test_rejectsWrongLength = do
  fx <- genFixture
  -- A well-formed CBOR byte string of 32 bytes instead of the key size.
  let file = cardanoCliEnvelope evolutionZeroType (cborHexOf (BS.take 64 (fxRawHex fx)))
  withKeyFile file $
    assertRejects
      (fxOCert fx)
      (KESFileEnvelope (EnvelopeBadLength keySize 32))
      (secretsOf fx)

test_rejectsBadCbor :: Assertion
test_rejectsBadCbor = do
  fx <- genFixture
  let raw = fxRawHex fx
      -- header says 608 bytes but one byte is missing
      shortPayload = B16.encode (cborBytesHeader keySize) <> BS.take (BS.length raw - 2) raw
      -- a CBOR array instead of a byte string
      array = "81" <> cborHexOf raw
      -- a leading byte that is not a byte-string header
      notBytes = "00" <> raw
  forM_ [shortPayload, array, notBytes] $ \cborHex ->
    withKeyFile (cardanoCliEnvelope evolutionZeroType cborHex) $
      assertRejects (fxOCert fx) (KESFileEnvelope EnvelopeBadCbor) (secretsOf fx)

test_rejectsMissingField :: Assertion
test_rejectsMissingField = do
  fx <- genFixture
  let hex = cborHexOf (fxRawHex fx)
      noType = "{\"description\":\"KES Signing Key\",\"cborHex\":\"" <> hex <> "\"}"
      noDescription = "{\"type\":\"" <> evolutionZeroType <> "\",\"cborHex\":\"" <> hex <> "\"}"
      noCborHex = "{\"type\":\"" <> evolutionZeroType <> "\",\"description\":\"KES Signing Key\"}"
      empty = "{}"
  withKeyFile noType $
    assertRejects (fxOCert fx) (KESFileEnvelope (EnvelopeMissingField "type")) (secretsOf fx)
  withKeyFile noDescription $
    assertRejects (fxOCert fx) (KESFileEnvelope (EnvelopeMissingField "description")) (secretsOf fx)
  withKeyFile noCborHex $
    assertRejects (fxOCert fx) (KESFileEnvelope (EnvelopeMissingField "cborHex")) (secretsOf fx)
  withKeyFile empty $
    assertRejects (fxOCert fx) (KESFileEnvelope (EnvelopeMissingField "type")) (secretsOf fx)

test_rejectsNotAnObject :: Assertion
test_rejectsNotAnObject = do
  fx <- genFixture
  let good = keyFile fx
      hex = cborHexOf (fxRawHex fx)
      files =
        [ "" -- empty file
        , "[]" -- not an object
        , BS.init (BS.init good) -- closing brace missing
        , good <> "{}" -- trailing garbage
        , "{\"type\":\""
            <> evolutionZeroType
            <> "\",\"type\":\""
            <> evolutionZeroType
            <> "\",\"description\":\"\",\"cborHex\":\""
            <> hex
            <> "\"}" -- duplicate field
        , "{\"type\":\""
            <> evolutionZeroType
            <> "\",\"description\":\"\",\"cborHex\":\""
            <> hex
            <> "\",\"extra\":\"\"}" -- unknown field
        , "{\"type\":\"" <> evolutionZeroType <> "\",\"description\":\"\",\"cborHex\":\"" <> hex <> "\",}" -- trailing comma
        , "{\"type\":\"" <> evolutionZeroType <> "\",\"description\":null,\"cborHex\":\"" <> hex <> "\"}" -- non-string value
        , "{\"type\":\"" <> evolutionZeroType <> "\",\"description\":\"a\nb\",\"cborHex\":\"" <> hex <> "\"}" -- control character in a string
        ]
  forM_ files $ \file ->
    withKeyFile file $
      assertRejects (fxOCert fx) (KESFileEnvelope EnvelopeNotAnObject) (secretsOf fx)

test_rejectsMismatchedOCert :: Assertion
test_rejectsMismatchedOCert = do
  fx <- genFixture
  other <- genFixture
  withKeyFile (keyFile fx) $
    assertRejects (fxOCert other) KESFileKeyMismatch (secretsOf fx)

test_missingFile :: Assertion
test_missingFile = do
  fx <- genFixture
  withSystemTempDirectory "protocol-test-kes" $ \dir -> do
    let path = dir </> "missing.skey"
    try (load (fxOCert fx) path) >>= \case
      Right hotKey -> do
        finalize hotKey
        assertFailure "a missing file loaded"
      Left (e :: IOException) ->
        assertBool ("is a does-not-exist error: " <> show e) (isDoesNotExistError e)
