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
-- whose @cborHex@ is one CBOR byte string holding the raw key.
module Test.Consensus.Protocol.Praos.FileCredentials (tests) where

import Cardano.Crypto.DSIGN (genKeyDSIGN, signedDSIGN)
import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Libsodium.Hex (HexDecodeError (..))
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
import Control.Exception (IOException, try)
import Control.Monad (forM_)
import Control.Tracer (nullTracer)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.List (isInfixOf)
import Data.Proxy (Proxy (..))
import Ouroboros.Consensus.Block.Forging (UpdateInfo (..))
import Ouroboros.Consensus.Protocol.Ledger.HotKey (HotKey (..), KESInfo (..))
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
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
    , testCase "rejects an unknown envelope type" test_rejectsUnknownType
    , testCase "rejects the period-carrying envelope type" test_rejectsWithPeriodType
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

-- | A generated key: its opcert, verification key and the hex text of its raw
-- serialisation (the test's own, heap-allocated copy; the loader under test
-- never makes one).
data Fixture = Fixture
  { fxOCert :: OCert StandardCrypto
  , fxVerKey :: KES.VerKeyKES TestKES
  , fxRawHex :: ByteString
  }

genFixture :: IO Fixture
genFixture = do
  seed <- mlockedSeedNewRandom
  sk <- KES.genKeyKES @TestKES seed
  mlockedSeedFinalize seed
  vk <- KES.deriveVerKeyKES sk
  raw <- KES.rawSerialiseSignKeyKES sk
  KES.forgetSignKeyKES sk
  pure
    Fixture
      { fxOCert = mkOCert vk
      , fxVerKey = vk
      , fxRawHex = B16.encode raw
      }

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

-- | The @cborHex@ value for a raw key given as hex: a CBOR byte string.
cborHexOf :: ByteString -> ByteString
cborHexOf rawHex = B16.encode (cborBytesHeader (BS.length rawHex `div` 2)) <> rawHex

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

-- | The envelope of a fixture key at evolution 0.
keyFile :: Fixture -> ByteString
keyFile fx = cardanoCliEnvelope evolutionZeroType (cborHexOf (fxRawHex fx))

-- | Write the contents to @kes.skey@ in a fresh temporary directory and run
-- the test with its path.
withKeyFile :: ByteString -> (FilePath -> IO a) -> IO a
withKeyFile contents k =
  withSystemTempDirectory "protocol-test-kes" $ \dir -> do
    let path = dir </> "kes.skey"
    BS.writeFile path contents
    k path

load :: OCert StandardCrypto -> FilePath -> IO (HotKey StandardCrypto IO)
load ocert path =
  instantiatePraosCredentials
    maxKESEvolutions
    nullTracer
    (PraosCredentialsFile ocert path Nothing)

{-------------------------------------------------------------------------------
  Assertions
-------------------------------------------------------------------------------}

-- | Load the file, check that the resulting hot key is the fixture's key at
-- evolution 0, that it signs, and finalise it.
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
  assertEqual "evolution" 0 kesEvolution
  getOCertMaybe hotKey >>= \case
    Nothing -> assertFailure "no OCert"
    Just ocert -> assertEqual "OCert hot key" (fxVerKey fx) (ocertVkHot ocert)

  -- The key is the one we generated: it signs at evolution 0 and, after an
  -- evolution, at evolution 1.
  let msg = BS.pack [1, 2, 3] :: ByteString
  signed0 <- HotKey.sign hotKey msg
  assertEqual
    "signature at evolution 0 verifies"
    (Right ())
    (KES.verifySignedKES () (fxVerKey fx) 0 msg signed0)
  evolve hotKey (KESPeriod (unKESPeriod startPeriod + 1)) >>= \case
    Updated KESInfo{kesEvolution = e} -> assertEqual "evolved" 1 e
    UpdateFailed err -> assertFailure $ "evolve failed: " <> show err
  signed1 <- HotKey.sign hotKey msg
  assertEqual
    "signature at evolution 1 verifies"
    (Right ())
    (KES.verifySignedKES () (fxVerKey fx) 1 msg signed1)

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
secretsOf fx = [fxRawHex fx, BS.take 32 (fxRawHex fx), "KES Signing Key", evolutionZeroType]

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

{-------------------------------------------------------------------------------
  Rejecting
-------------------------------------------------------------------------------}

test_rejectsUnknownType :: Assertion
test_rejectsUnknownType = do
  fx <- genFixture
  let file = cardanoCliEnvelope "KesSigningKey_bogus_kes_2^6" (cborHexOf (fxRawHex fx))
  withKeyFile file $
    assertRejects (fxOCert fx) (KESFileEnvelope EnvelopeUnknownType) ("bogus" : secretsOf fx)

test_rejectsWithPeriodType :: Assertion
test_rejectsWithPeriodType = do
  fx <- genFixture
  let file = cardanoCliEnvelope withPeriodType (cborHexOf (fxRawHex fx))
  withKeyFile file $
    assertRejects (fxOCert fx) KESFileUnsupportedType (withPeriodType : secretsOf fx)

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
