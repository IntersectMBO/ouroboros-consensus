{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- DUPLICATE -- adapted from: cardano-api/src/Cardano/Api/KeysPraos.hs

-- | Praos consensus key types and their 'Key' class instances
module Cardano.Api.KeysPraos
  ( -- * Key types
    KesKey
  , VrfKey

    -- * Data family instances
  , AsType (..)
  , Hash (..)
  , SigningKey (..)
  , VerificationKey (..)
  ) where

import Cardano.Api.Any
import Cardano.Api.Key
import Cardano.Api.SerialiseTextEnvelope
import Cardano.Api.SerialiseUsing
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.KES.Class as Crypto
import qualified Cardano.Crypto.VRF.Class as Crypto
import Cardano.Ledger.Hashes (HASH)
import Cardano.Protocol.Crypto (Crypto (..), StandardCrypto)
import Data.String (IsString (..))

--
-- KES keys
--

data KesKey

instance HasTypeProxy KesKey where
  data AsType KesKey = AsKesKey
  proxyToAsType _ = AsKesKey

instance Key KesKey where
  newtype VerificationKey KesKey
    = KesVerificationKey (Crypto.VerKeyKES (KES StandardCrypto))
    deriving stock Eq
    deriving (Show, IsString) via UsingRawBytesHex (VerificationKey KesKey)
    deriving newtype (EncCBOR, DecCBOR, ToCBOR, FromCBOR)
    deriving anyclass SerialiseAsCBOR

  newtype SigningKey KesKey
    = KesSigningKey (Crypto.UnsoundPureSignKeyKES (KES StandardCrypto))
    deriving (Show, IsString) via UsingRawBytesHex (SigningKey KesKey)
    deriving newtype (ToCBOR, FromCBOR)
    deriving anyclass (EncCBOR, DecCBOR, SerialiseAsCBOR)

  -- This loses the mlock safety of the seed, since it starts from a normal in-memory seed.
  deterministicSigningKey :: AsType KesKey -> Crypto.Seed -> SigningKey KesKey
  deterministicSigningKey AsKesKey =
    KesSigningKey . Crypto.unsoundPureGenKeyKES

  deterministicSigningKeySeedSize :: AsType KesKey -> Word
  deterministicSigningKeySeedSize AsKesKey =
    Crypto.seedSizeKES proxy
   where
    proxy :: Proxy (KES StandardCrypto)
    proxy = Proxy

  getVerificationKey :: SigningKey KesKey -> VerificationKey KesKey
  getVerificationKey (KesSigningKey sk) =
    KesVerificationKey (Crypto.unsoundPureDeriveVerKeyKES sk)

  verificationKeyHash :: VerificationKey KesKey -> Hash KesKey
  verificationKeyHash (KesVerificationKey vkey) =
    KesKeyHash (Crypto.hashVerKeyKES vkey)

instance SerialiseAsRawBytes (VerificationKey KesKey) where
  serialiseToRawBytes (KesVerificationKey vk) =
    Crypto.rawSerialiseVerKeyKES vk

  deserialiseFromRawBytes (AsVerificationKey AsKesKey) bs =
    KesVerificationKey
      <$> Crypto.rawDeserialiseVerKeyKES bs

instance SerialiseAsRawBytes (SigningKey KesKey) where
  serialiseToRawBytes (KesSigningKey sk) =
    Crypto.rawSerialiseUnsoundPureSignKeyKES sk

  deserialiseFromRawBytes (AsSigningKey AsKesKey) bs =
    KesSigningKey <$> Crypto.rawDeserialiseUnsoundPureSignKeyKES bs

instance SerialiseAsBech32 (VerificationKey KesKey) where
  bech32PrefixFor _ = "kes_vk"
  bech32PrefixesPermitted _ = ["kes_vk"]

instance SerialiseAsBech32 (SigningKey KesKey) where
  bech32PrefixFor _ = "kes_sk"
  bech32PrefixesPermitted _ = ["kes_sk"]

newtype instance Hash KesKey
  = KesKeyHash
      ( Crypto.Hash
          HASH
          (Crypto.VerKeyKES (KES StandardCrypto))
      )
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash KesKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash KesKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash KesKey) where
  serialiseToRawBytes (KesKeyHash vkh) =
    Crypto.hashToBytes vkh

  deserialiseFromRawBytes (AsHash AsKesKey) bs =
    KesKeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey KesKey) where
  textEnvelopeType _ =
    "KesVerificationKey_"
      <> fromString (Crypto.algorithmNameKES proxy)
   where
    proxy :: Proxy (KES StandardCrypto)
    proxy = Proxy

instance HasTextEnvelope (SigningKey KesKey) where
  textEnvelopeType _ =
    "KesSigningKey_"
      <> fromString (Crypto.algorithmNameKES proxy)
   where
    proxy :: Proxy (KES StandardCrypto)
    proxy = Proxy

--
-- VRF keys
--

data VrfKey

instance HasTypeProxy VrfKey where
  data AsType VrfKey = AsVrfKey
  proxyToAsType _ = AsVrfKey

instance Key VrfKey where
  newtype VerificationKey VrfKey
    = VrfVerificationKey (Crypto.VerKeyVRF (VRF StandardCrypto))
    deriving stock Eq
    deriving (Show, IsString) via UsingRawBytesHex (VerificationKey VrfKey)
    deriving newtype (EncCBOR, DecCBOR, ToCBOR, FromCBOR)
    deriving anyclass SerialiseAsCBOR

  newtype SigningKey VrfKey
    = VrfSigningKey (Crypto.SignKeyVRF (VRF StandardCrypto))
    deriving (Show, IsString) via UsingRawBytesHex (SigningKey VrfKey)
    deriving newtype (EncCBOR, DecCBOR, ToCBOR, FromCBOR)
    deriving anyclass SerialiseAsCBOR

  deterministicSigningKey :: AsType VrfKey -> Crypto.Seed -> SigningKey VrfKey
  deterministicSigningKey AsVrfKey seed =
    VrfSigningKey (Crypto.genKeyVRF seed)

  deterministicSigningKeySeedSize :: AsType VrfKey -> Word
  deterministicSigningKeySeedSize AsVrfKey =
    Crypto.seedSizeVRF proxy
   where
    proxy :: Proxy (VRF StandardCrypto)
    proxy = Proxy

  getVerificationKey :: SigningKey VrfKey -> VerificationKey VrfKey
  getVerificationKey (VrfSigningKey sk) =
    VrfVerificationKey (Crypto.deriveVerKeyVRF sk)

  verificationKeyHash :: VerificationKey VrfKey -> Hash VrfKey
  verificationKeyHash (VrfVerificationKey vkey) =
    VrfKeyHash (Crypto.hashVerKeyVRF vkey)

instance SerialiseAsRawBytes (VerificationKey VrfKey) where
  serialiseToRawBytes (VrfVerificationKey vk) =
    Crypto.rawSerialiseVerKeyVRF vk

  deserialiseFromRawBytes (AsVerificationKey AsVrfKey) bs =
    VrfVerificationKey <$> Crypto.rawDeserialiseVerKeyVRF bs

instance SerialiseAsRawBytes (SigningKey VrfKey) where
  serialiseToRawBytes (VrfSigningKey sk) =
    Crypto.rawSerialiseSignKeyVRF sk

  deserialiseFromRawBytes (AsSigningKey AsVrfKey) bs =
    VrfSigningKey <$> Crypto.rawDeserialiseSignKeyVRF bs

instance SerialiseAsBech32 (VerificationKey VrfKey) where
  bech32PrefixFor _ = "vrf_vk"
  bech32PrefixesPermitted _ = ["vrf_vk"]

instance SerialiseAsBech32 (SigningKey VrfKey) where
  bech32PrefixFor _ = "vrf_sk"
  bech32PrefixesPermitted _ = ["vrf_sk"]

newtype instance Hash VrfKey
  = VrfKeyHash
      ( Crypto.Hash
          HASH
          (Crypto.VerKeyVRF (VRF StandardCrypto))
      )
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash VrfKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash VrfKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash VrfKey) where
  serialiseToRawBytes (VrfKeyHash vkh) =
    Crypto.hashToBytes vkh

  deserialiseFromRawBytes (AsHash AsVrfKey) bs =
    VrfKeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey VrfKey) where
  textEnvelopeType _ = "VrfVerificationKey_" <> fromString (Crypto.algorithmNameVRF proxy)
   where
    proxy :: Proxy (VRF StandardCrypto)
    proxy = Proxy

instance HasTextEnvelope (SigningKey VrfKey) where
  textEnvelopeType _ = "VrfSigningKey_" <> fromString (Crypto.algorithmNameVRF proxy)
   where
    proxy :: Proxy (VRF StandardCrypto)
    proxy = Proxy
