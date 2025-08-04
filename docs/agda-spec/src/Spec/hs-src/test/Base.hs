{-# LANGUAGE TypeApplications #-}

module Base (
  externalFunctions
, sampleSKeyDSIGN
, deriveVkeyDSIGFromSkeyDSIG
, sampleSKeyKES
, deriveVkeyKESFromSkeyKES
, sampleSKeyVRF
, deriveVkeyVRFFromSkeyVRF
) where

import Lib

import Cardano.Ledger.Hashes (Hash, HASH, HashAlgorithm)
import Cardano.Ledger.Keys (DSIGN, VKey (..))
import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), SignedDSIGN (..), verifySignedDSIGN, signedDSIGN)
import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.VRF as VRF
import qualified Cardano.Crypto.VRF.Praos as VRF
import Cardano.Crypto.Util (naturalToBytes, bytesToNatural)
import Cardano.Crypto.Hash (hashFromBytes, sizeHash)
import Cardano.Crypto.Seed (Seed, mkSeedFromBytes)
import Data.Data (Proxy (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Either (isRight)
import Data.Maybe (fromMaybe)

-- Hashes

integerToHash :: forall h a. HashAlgorithm h => Integer -> Maybe (Hash h a)
integerToHash = hashFromBytes . naturalToBytes (fromIntegral . sizeHash $ Proxy @h) . fromInteger

-- DSIGN

vkeyDSIGNFromInteger :: Integer -> Maybe (VKey kd)
vkeyDSIGNFromInteger = fmap VKey . rawDeserialiseVerKeyDSIGN . naturalToBytes 32 . fromInteger

vkeyDSIGNToInteger :: VKey kd -> Integer
vkeyDSIGNToInteger = toInteger . bytesToNatural . rawSerialiseVerKeyDSIGN . unVKey

skeyDSIGNFromInteger :: DSIGNAlgorithm v => Integer -> Maybe (SignKeyDSIGN v)
skeyDSIGNFromInteger = rawDeserialiseSignKeyDSIGN . naturalToBytes 32 . fromInteger

skeyDSIGNToInteger :: DSIGNAlgorithm v => SignKeyDSIGN v -> Integer
skeyDSIGNToInteger = toInteger . bytesToNatural . rawSerialiseSignKeyDSIGN

sigDSIGNFromInteger :: DSIGNAlgorithm v => Integer -> Maybe (SigDSIGN v)
sigDSIGNFromInteger = rawDeserialiseSigDSIGN . naturalToBytes 64 . fromInteger

sigDSIGNToInteger :: DSIGNAlgorithm v => SigDSIGN v -> Integer
sigDSIGNToInteger = toInteger . bytesToNatural . rawSerialiseSigDSIGN

signedDSIGNFromInteger :: forall v a. DSIGNAlgorithm v => Integer -> SignedDSIGN v a
signedDSIGNFromInteger n =
  SignedDSIGN
    . fromMaybe
      (error "Failed to decode the DSIGN signature")
      $ sigDSIGNFromInteger n

signedDSIGNToInteger :: DSIGNAlgorithm v => SignedDSIGN v a -> Integer
signedDSIGNToInteger (SignedDSIGN x) = sigDSIGNToInteger x

deriveVkeyDSIGFromSkeyDSIG :: Integer -> Integer
deriveVkeyDSIGFromSkeyDSIG sk = vkeyDSIGNToInteger $ VKey $ deriveVerKeyDSIGN skey
  where
    skey =
      fromMaybe
        (error "Failed to convert an Agda DSIGN SKey to a Haskell DSIGN SKey")
        $ skeyDSIGNFromInteger sk

-- KES

type KESAlg = KES.Sum6KES DSIGN HASH

vkeyKESFromInteger :: KES.KESAlgorithm v => Integer -> Maybe (KES.VerKeyKES v)
vkeyKESFromInteger = KES.rawDeserialiseVerKeyKES . naturalToBytes (fromIntegral $ KES.sizeVerKeyKES $ Proxy @KESAlg) . fromInteger

vkeyKESToInteger :: KES.KESAlgorithm v => KES.VerKeyKES v -> Integer
vkeyKESToInteger = toInteger . bytesToNatural . KES.rawSerialiseVerKeyKES

skeyKESFromInteger :: KES.UnsoundPureKESAlgorithm v => Integer -> Maybe (KES.UnsoundPureSignKeyKES v)
skeyKESFromInteger = KES.rawDeserialiseUnsoundPureSignKeyKES . naturalToBytes (fromIntegral $ KES.sizeSignKeyKES $ Proxy @KESAlg) . fromInteger

skeyKESToInteger :: KES.UnsoundPureKESAlgorithm v => KES.UnsoundPureSignKeyKES v -> Integer
skeyKESToInteger = toInteger . bytesToNatural . KES.rawSerialiseUnsoundPureSignKeyKES

sigKESFromInteger :: KES.KESAlgorithm v => Integer -> Maybe (KES.SigKES v)
sigKESFromInteger = KES.rawDeserialiseSigKES . naturalToBytes (fromIntegral $ KES.sizeSigKES $ Proxy @KESAlg) . fromInteger

sigKESToInteger :: KES.KESAlgorithm v => KES.SigKES v -> Integer
sigKESToInteger = toInteger . bytesToNatural . KES.rawSerialiseSigKES

signedKESFromInteger :: forall v a. KES.KESAlgorithm v => Integer -> KES.SignedKES v a
signedKESFromInteger n =
  KES.SignedKES
    . fromMaybe
        (error "Failed to decode the KES signature")
        $ sigKESFromInteger n

signedKESToInteger :: KES.KESAlgorithm v => KES.SignedKES v a -> Integer
signedKESToInteger (KES.SignedKES x) = sigKESToInteger x

deriveVkeyKESFromSkeyKES :: Integer -> Integer
deriveVkeyKESFromSkeyKES sk = vkeyKESToInteger @KESAlg $ KES.unsoundPureDeriveVerKeyKES skey
  where
    skey =
      fromMaybe
        (error "Failed to convert an Agda KES SKey to a Haskell KES SKey")
        $ skeyKESFromInteger sk

-- VRF

type VRFAlg = VRF.PraosVRF

vkeyVRFFromInteger :: VRF.VRFAlgorithm v => Integer -> Maybe (VRF.VerKeyVRF v)
vkeyVRFFromInteger = VRF.rawDeserialiseVerKeyVRF . naturalToBytes (fromIntegral $ VRF.sizeVerKeyVRF $ Proxy @VRFAlg) . fromInteger

vkeyVRFToInteger :: VRF.VRFAlgorithm v => VRF.VerKeyVRF v -> Integer
vkeyVRFToInteger = toInteger . bytesToNatural . VRF.rawSerialiseVerKeyVRF

skeyVRFFromInteger :: Integer -> VRF.SignKeyVRF VRFAlg
skeyVRFFromInteger = fst . VRF.genKeyPairVRF . mkSeedFromBytes . naturalToBytes (fromIntegral $ VRF.sizeSignKeyVRF $ Proxy @VRFAlg) . fromInteger

skeyVRFToInteger :: VRF.VRFAlgorithm v => VRF.SignKeyVRF v -> Integer
skeyVRFToInteger = toInteger . bytesToNatural . VRF.rawSerialiseSignKeyVRF

proofVRFFromInteger :: VRF.VRFAlgorithm v => Integer -> Maybe (VRF.CertVRF v)
proofVRFFromInteger = VRF.rawDeserialiseCertVRF . naturalToBytes (fromIntegral $ VRF.sizeCertVRF $ Proxy @VRFAlg) . fromInteger

proofVRFToInteger :: VRF.VRFAlgorithm v => VRF.CertVRF v -> Integer
proofVRFToInteger = toInteger . bytesToNatural . VRF.rawSerialiseCertVRF

deriveVkeyVRFFromSkeyVRF :: Integer -> Integer
deriveVkeyVRFFromSkeyVRF = vkeyVRFToInteger @VRFAlg . VRF.deriveVerKeyVRF . skeyVRFFromInteger

-- External functions

externalFunctions :: ExternalFunctions
externalFunctions = dummyExternalFunctions
  { extSignDSIG     = extSignDSIG'
  , extIsSignedDSIG = extIsSignedDSIG'
  , extSignKES      = extSignKES'
  , extIsSignedKES  = extIsSignedKES'
  , extEvaluate     = extEvaluate'
  , extVerify       = extVerify'
  }
  where
    extSignDSIG' sk ser =
      signedDSIGNToInteger $
        signedDSIGN
          @DSIGN
          @(Hash HASH ByteString)
          ()
          hash
          skey
      where
        skey =
          fromMaybe
            (error "Failed to convert an Agda SKey to a Haskell SKey")
            $ skeyDSIGNFromInteger sk

        hash =
          fromMaybe
            (error $ "Failed to get hash from integer:\n" <> show ser)
            $ integerToHash ser

    extIsSignedDSIG' vk ser sig =
      isRight $
        verifySignedDSIGN
          @DSIGN
          @(Hash HASH ByteString)
          ()
          vkey
          hash
          signature
      where
        vkey =
          unVKey
            . fromMaybe (error "Failed to convert an Agda VKey to a Haskell VKey")
            $ vkeyDSIGNFromInteger vk

        hash =
          fromMaybe
            (error $ "Failed to get hash from integer:\n" <> show ser)
            $ integerToHash ser

        signature =
          signedDSIGNFromInteger sig

    extSignKES' skf n ser =
      sigKESToInteger $
        KES.unsoundPureSignKES
          @KESAlg
          @(Hash HASH ByteString)
          ()
          kp
          hash
          skey
      where
        skey =
          fromMaybe
            (error "Failed to convert an Agda KES SKey to a Haskell KES SKey")
            $ skeyKESFromInteger (skf n)

        kp =
          fromIntegral n

        hash =
          fromMaybe
            (error $ "Failed to get hash from integer:\n" <> show ser)
            $ integerToHash ser

    extIsSignedKES' vk n ser sig =
      isRight $
        KES.verifySignedKES
          @KESAlg
          @(Hash HASH ByteString)
          ()
          vkey
          kp
          hash
          signature
      where
        vkey =
          fromMaybe
            (error "Failed to convert an Agda KES VKey to a Haskell KES VKey")
            $ vkeyKESFromInteger vk

        kp =
          fromIntegral n

        hash =
          fromMaybe
            (error $ "Failed to get hash from integer:\n" <> show ser)
            $ integerToHash ser

        signature =
          signedKESFromInteger sig

    extEvaluate' sk seed = (proof, ser)
      where
        skey =
          skeyVRFFromInteger sk

        input =
          fromMaybe
            (error $ "Failed to get hash from integer:\n" <> show seed)
            $ integerToHash seed

        VRF.CertifiedVRF output cert =
          VRF.evalCertified
            @VRFAlg
            @(Hash HASH ByteString)
            ()
            input
            skey

        proof =
          proofVRFToInteger cert

        ser =
          (toInteger . bytesToNatural . VRF.getOutputVRFBytes) output

    extVerify' vk seed (proof, ser) =
      VRF.verifyCertified
        @VRFAlg
        @(Hash HASH ByteString)
        ()
        vkey
        input
        (VRF.CertifiedVRF output cert)
      where
        vkey =
          fromMaybe
            (error "Failed to convert an Agda VRF VKey to a Haskell VRF VKey")
            $ vkeyVRFFromInteger vk

        input =
          fromMaybe
            (error $ "Failed to get hash from integer:\n" <> show seed)
            $ integerToHash seed

        output =
          VRF.mkTestOutputVRF $ fromIntegral ser

        cert =
          fromMaybe
            (error "Failed to convert an Agda VRF proof to a Haskell VRF proof")
            $ proofVRFFromInteger proof

-- Utilities

sampleSeed :: Seed
sampleSeed = mkSeedFromBytes $ BC.pack $ replicate 32 '0'

sampleSKeyDSIGN :: Integer
sampleSKeyDSIGN = skeyDSIGNToInteger $ genKeyDSIGN @DSIGN sampleSeed

sampleSKeyKES :: Integer
sampleSKeyKES = skeyKESToInteger @KESAlg $ KES.unsoundPureGenKeyKES sampleSeed

sampleSKeyVRF :: Integer
sampleSKeyVRF = (skeyVRFToInteger @VRFAlg . fst . VRF.genKeyPairVRF) sampleSeed
