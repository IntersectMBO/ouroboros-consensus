{-# LANGUAGE TypeApplications #-}

module Base (
  externalFunctions
, sampleSignKey
, deriveVkFromSk
) where

import Lib

import Cardano.Ledger.Hashes (Hash, HASH, HashAlgorithm)
import Cardano.Ledger.Keys (DSIGN, VKey (..))
import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), SignedDSIGN (..), verifySignedDSIGN, signedDSIGN, Ed25519DSIGN)
import Cardano.Crypto.Util (naturalToBytes, bytesToNatural)
import Cardano.Crypto.Hash (hashFromBytes, sizeHash)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Data.Data (Proxy (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Either (isRight)
import Data.Maybe (fromMaybe)

vkeyFromInteger :: Integer -> Maybe (VKey kd)
vkeyFromInteger = fmap VKey . rawDeserialiseVerKeyDSIGN . naturalToBytes 32 . fromInteger

vkeyToInteger :: VKey kd -> Integer
vkeyToInteger = toInteger . bytesToNatural . rawSerialiseVerKeyDSIGN . unVKey

skeyDSIGNFromInteger :: DSIGNAlgorithm v => Integer -> Maybe (SignKeyDSIGN v)
skeyDSIGNFromInteger = rawDeserialiseSignKeyDSIGN . naturalToBytes 32 . fromInteger

skeyDSIGNToInteger :: DSIGNAlgorithm v => SignKeyDSIGN v -> Integer
skeyDSIGNToInteger = toInteger . bytesToNatural . rawSerialiseSignKeyDSIGN

signatureFromInteger :: DSIGNAlgorithm v => Integer -> Maybe (SigDSIGN v)
signatureFromInteger = rawDeserialiseSigDSIGN . naturalToBytes 64 . fromInteger

signatureToInteger :: DSIGNAlgorithm v => SigDSIGN v -> Integer
signatureToInteger = toInteger . bytesToNatural . rawSerialiseSigDSIGN

integerToHash :: forall h a. HashAlgorithm h => Integer -> Maybe (Hash h a)
integerToHash = hashFromBytes . naturalToBytes (fromIntegral . sizeHash $ Proxy @h) . fromInteger

signedDSIGNFromInteger :: forall v a. DSIGNAlgorithm v => Integer -> SignedDSIGN v a
signedDSIGNFromInteger n =
  SignedDSIGN
    . fromMaybe
      (error "Failed to decode the signature")
    $ signatureFromInteger n

signedDSIGNToInteger :: DSIGNAlgorithm v => SignedDSIGN v a -> Integer
signedDSIGNToInteger (SignedDSIGN x) = signatureToInteger x

externalFunctions :: ExternalFunctions
externalFunctions = dummyExternalFunctions
  { extSignDSIG     = extSignDSIG'
  , extIsSignedDSIG = extIsSignedDSIG'
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
            $ vkeyFromInteger vk
        hash =
          fromMaybe
            (error $ "Failed to get hash from integer:\n" <> show ser)
            $ integerToHash ser
        signature =
          signedDSIGNFromInteger sig

sampleSignKey :: Integer
sampleSignKey = skeyDSIGNToInteger $ genKeyDSIGN @Ed25519DSIGN (mkSeedFromBytes $ BC.pack $ replicate 32 '0')

deriveVkFromSk :: Integer -> Integer
deriveVkFromSk sk = vkeyToInteger $ VKey $ deriveVerKeyDSIGN skey
  where
    skey =
      fromMaybe
        (error "Failed to convert an Agda SKey to a Haskell SKey")
        $ skeyDSIGNFromInteger sk
