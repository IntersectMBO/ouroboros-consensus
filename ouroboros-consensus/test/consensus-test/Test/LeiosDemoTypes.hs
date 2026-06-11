{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Test.LeiosDemoTypes (tests) where

import qualified Cardano.Crypto.Leios as Leios
import Cardano.Binary (serialize')
import Cardano.Crypto.DSIGN
  ( DSIGNAlgorithm (deriveVerKeyDSIGN)
  , DSIGNAggregatable (..)
  , genKeyDSIGN
  , seedSizeDSIGN
  , signDSIGN
  )
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Data.ByteString as BS
import Data.Data (Proxy (..))
import Data.List (sort)
import qualified Data.Vector as V
import LeiosDemoTypes
  ( BytesSize
  , CertificateInvalid (..)
  , Committee (..)
  , EbHash (..)
  , LeiosDSIGN
  , LeiosEb (..)
  , LeiosPoint (..)
  , LeiosSigningKey
  , TxHash (..)
  , encodeLeiosEb
  , leiosEbBytesSize
  , maxTxsPerEb
  , minCertificationThreshold
  , minSigPoPDST
  , mkCommitteeEveryoneVotes
  , validateLeiosCertificate
  )
import Test.Crypto.Util (arbitrarySeedOfSize)
import Test.QuickCheck
  ( Gen
  , Property
  , chooseInt
  , counterexample
  , forAll
  , frequency
  , generate
  , vectorOf
  , (.&&.)
  , (===)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "LeiosDemoTypes"
    [ testProperty "leiosEbBytesSize consistent with encodeLeiosEb" prop_ebBytesSizeConsistent
    , testProperty "mkCommitteeEveryoneVotes normalizes and sorts" prop_committeeNormalizedAndSorted
    , testCase
        "validateLeiosCertificate rejects cert with bad aggregated sig"
        unit_validateLeiosCertificate_badSig
    , testCase
        "validateLeiosCertificate accepts cert with valid aggregated sig"
        unit_validateLeiosCertificate_goodSig
    ]

-- | Minimum tx size as per the ASSUMPTION in 'leiosEbBytesSize'.
minTxBytesSize :: Int
minTxBytesSize = 55

-- | Maximum tx size as per the ASSUMPTION in 'leiosEbBytesSize'.
maxTxBytesSize :: Int
maxTxBytesSize = 2 ^ (14 :: Int)

-- | Generate a random TxHash (32 random bytes).
genTxHash :: Gen TxHash
genTxHash = MkTxHash . BS.pack <$> vectorOf 32 (fromIntegral <$> chooseInt (0, 255))

-- | Generate a tx size with good coverage of CBOR encoding boundaries.
-- Values 0-23 encode in 1 byte, 24-255 in 2 bytes, 256-65535 in 3 bytes.
genTxBytesSize :: Gen BytesSize
genTxBytesSize =
  frequency
    [ (1, pure $ fromIntegral minTxBytesSize) -- lower bound
    , (1, pure $ fromIntegral maxTxBytesSize) -- upper bound
    , (1, pure 255) -- boundary: last 2-byte CBOR uint
    , (1, pure 256) -- boundary: first 3-byte CBOR uint
    , (6, fromIntegral <$> chooseInt (minTxBytesSize, maxTxBytesSize)) -- uniform
    ]

-- | Generate the number of items with good coverage of CBOR encoding
-- boundaries for the map length (0-23 → 1 byte, 24-255 → 2 bytes,
-- 256+ → 3 bytes) and the extremes.
genNumItems :: Gen Int
genNumItems =
  frequency
    [ (1, pure 0) -- empty EB
    , (1, pure 1) -- singleton
    , (1, pure 23) -- boundary: last 1-byte CBOR map length
    , (1, pure 24) -- boundary: first 2-byte CBOR map length
    , (1, pure 255) -- boundary: last 2-byte CBOR map length
    , (1, pure 256) -- boundary: first 3-byte CBOR map length
    , (1, pure maxTxsPerEb) -- upper bound
    , (3, chooseInt (0, maxTxsPerEb)) -- uniform
    ]

-- | Generate a LeiosEb with the given number of transactions.
genEb :: Int -> Gen LeiosEb
genEb numTxs = do
  txs <- vectorOf numTxs genTxItem
  pure $ MkLeiosEb $ V.fromList txs
 where
  genTxItem = (,) <$> genTxHash <*> genTxBytesSize

-- | The analytical 'leiosEbBytesSize' must agree with the actual length of
-- the CBOR encoding produced by 'encodeLeiosEb'.
prop_ebBytesSizeConsistent :: Property
prop_ebBytesSizeConsistent =
  forAll (genNumItems >>= genEb) $ \eb ->
    let encoded = serialize' $ encodeLeiosEb eb
        actualSize = fromIntegral (BS.length encoded) :: BytesSize
        estimatedSize = leiosEbBytesSize eb
     in counterexample
          ("items: " <> show (V.length (leiosEbTxs eb)))
          (estimatedSize === actualSize)

genLeiosSigningKey :: Gen LeiosSigningKey
genLeiosSigningKey = do
  seed <- arbitrarySeedOfSize (seedSizeDSIGN (Proxy @LeiosDSIGN))
  pure $ genKeyDSIGN seed

-- | 'mkCommitteeEveryoneVotes' must produce weights that sum to 1 and are
-- sorted ascending (so 'VoterId' assignment by index is stable). Inputs are
-- generated with distinct verification keys, since dedup-by-key is a separate
-- concern not exercised here.
prop_committeeNormalizedAndSorted :: Property
prop_committeeNormalizedAndSorted =
  forAll (chooseInt (1, 20)) $ \n ->
    forAll (vectorOf n genLeiosSigningKey) $ \sks ->
      forAll (vectorOf n (chooseInt (1, 1000))) $ \ws ->
        let inputs = zip (deriveVerKeyDSIGN <$> sks) ws
            committee = mkCommitteeEveryoneVotes inputs
            weights = fst <$> voters committee
         in counterexample ("committee: " <> show committee) $
              counterexample "weights sum to 1" (sum weights === 1)
                .&&. counterexample "weights sorted ascending" (weights === sort weights)
                .&&. counterexample "preserves cardinality" (length weights === n)

-- | 'validateLeiosCertificate' must reject a certificate whose
-- 'aggregatedSignature' does not verify against the aggregated keys of
-- the signers in the bitfield (over the cert's @(slotNo,
-- endorserBlockHash)@). Build a committee, mark every member as a
-- signer, but sign a *different* point — the aggregate verify must
-- fail with 'CertificateSignature'.
--
-- This is the Hemingway-bridge test for the BLS aggregate-verify
-- branch: without that check, validateLeiosCertificate currently
-- accepts the cert (weight ≥ threshold) and returns 'Right'.
unit_validateLeiosCertificate_badSig :: IO ()
unit_validateLeiosCertificate_badSig = do
  -- 4 committee members, all signing.
  sks <- generate (vectorOf 4 genLeiosSigningKey)
  let vks = deriveVerKeyDSIGN <$> sks
      -- Equal weights so every member contributes the same amount.
      committee = mkCommitteeEveryoneVotes (zip vks (repeat (1 :: Int)))
      -- 4 signers ⇒ ⌈4/8⌉ = 1 byte with the high 4 bits set.
      signersBitfield = BS.singleton 0xF0
      certEbHash = Leios.MkEbHash (BS.replicate 32 0xAA)
      certPoint = MkLeiosPoint (SlotNo 10) (consEbHash certEbHash)
      -- Each member signs a *different* point (slot 999), then we
      -- aggregate. The aggregate sig won't verify against 'certPoint'.
      wrongPoint = MkLeiosPoint (SlotNo 999) (consEbHash certEbHash)
      individualSigs = [signDSIGN minSigPoPDST wrongPoint sk | sk <- sks]
  aggSig <- case aggregateSigsDSIGN individualSigs of
    Right s -> pure s
    Left e -> assertFailure $ "aggregateSigsDSIGN failed: " <> e
  let cert =
        Leios.LeiosCert
          { Leios.slotNo = certPoint.pointSlotNo
          , Leios.endorserBlockHash = certEbHash
          , Leios.signers = signersBitfield
          , Leios.aggregatedSignature = aggSig
          }
  case validateLeiosCertificate committee minCertificationThreshold cert of
    Left CertificateSignature -> pure ()
    Left other ->
      assertFailure $
        "expected CertificateSignature, got: " <> show other
    Right total ->
      assertBool
        ("expected Left CertificateSignature, got Right " <> show total)
        False
 where
  -- Convert between the two structurally-identical 'EbHash' newtypes
  -- (cardano-crypto-leios vs. consensus's local 'LeiosDemoTypes').
  consEbHash (Leios.MkEbHash bs) = MkEbHash bs

-- | The mirror of the bad-sig test: when every committee member signs
-- the certificate's @(slotNo, endorserBlockHash)@ and the signatures
-- are aggregated correctly, 'validateLeiosCertificate' must accept.
unit_validateLeiosCertificate_goodSig :: IO ()
unit_validateLeiosCertificate_goodSig = do
  sks <- generate (vectorOf 4 genLeiosSigningKey)
  let vks = deriveVerKeyDSIGN <$> sks
      committee = mkCommitteeEveryoneVotes (zip vks (repeat (1 :: Int)))
      signersBitfield = BS.singleton 0xF0
      certEbHash = Leios.MkEbHash (BS.replicate 32 0xAA)
      certPoint = MkLeiosPoint (SlotNo 10) (consEbHash certEbHash)
      individualSigs = [signDSIGN minSigPoPDST certPoint sk | sk <- sks]
  aggSig <- case aggregateSigsDSIGN individualSigs of
    Right s -> pure s
    Left e -> assertFailure $ "aggregateSigsDSIGN failed: " <> e
  let cert =
        Leios.LeiosCert
          { Leios.slotNo = certPoint.pointSlotNo
          , Leios.endorserBlockHash = certEbHash
          , Leios.signers = signersBitfield
          , Leios.aggregatedSignature = aggSig
          }
  case validateLeiosCertificate committee minCertificationThreshold cert of
    Right total -> total @?= 1
    Left invalid ->
      assertFailure $ "expected Right, got Left " <> show invalid
 where
  consEbHash (Leios.MkEbHash bs) = MkEbHash bs
