{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

-- DUPLICATE -- adapted from: cardano-api/src/Cardano/Api/KeysPraos.hs

-- | Praos consensus key types and their 'Key' class instances
--
module Cardano.Api.KeysPraos (
    -- * Key types
    UnsoundPureKesKey
  , VrfKey
    -- * Data family instances
  , AsType (..)
  , Hash (..)
  , SigningKey (..)
  , VerificationKey (..)
  ) where

import           Cardano.Api.Any
import           Cardano.Api.Key
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.SerialiseUsing
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.KES.Class as Crypto
import qualified Cardano.Crypto.VRF.Class as Crypto
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Crypto as Shelley (KES, VRF)
import qualified Cardano.Ledger.Keys as Shelley
import           Data.String (IsString (..))

--
-- KES keys
--

data UnsoundPureKesKey

instance HasTypeProxy UnsoundPureKesKey where
    data AsType UnsoundPureKesKey = AsUnsoundPureKesKey
    proxyToAsType _ = AsUnsoundPureKesKey

instance Key UnsoundPureKesKey where

    newtype VerificationKey UnsoundPureKesKey =
        KesVerificationKey (Shelley.VerKeyKES StandardCrypto)
      deriving stock (Eq)
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey UnsoundPureKesKey)
      deriving newtype (EncCBOR, DecCBOR, ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey UnsoundPureKesKey =
        KesSigningKey (Shelley.UnsoundPureSignKeyKES StandardCrypto)
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey UnsoundPureKesKey)
      deriving newtype (EncCBOR, DecCBOR, ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    -- This loses the mlock safety of the seed, since it starts from a normal
    -- in-memory seed.
    deterministicSigningKey :: AsType UnsoundPureKesKey -> Crypto.Seed -> SigningKey UnsoundPureKesKey
    deterministicSigningKey AsUnsoundPureKesKey =
        KesSigningKey . Crypto.unsoundPureGenKeyKES

    deterministicSigningKeySeedSize :: AsType UnsoundPureKesKey -> Word
    deterministicSigningKeySeedSize AsUnsoundPureKesKey =
        Crypto.seedSizeKES proxy
      where
        proxy :: Proxy (Shelley.KES StandardCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey UnsoundPureKesKey -> VerificationKey UnsoundPureKesKey
    getVerificationKey (KesSigningKey sk) =
        KesVerificationKey (Crypto.unsoundPureDeriveVerKeyKES sk)

    verificationKeyHash :: VerificationKey UnsoundPureKesKey -> Hash UnsoundPureKesKey
    verificationKeyHash (KesVerificationKey vkey) =
        UnsoundPureKesKeyHash (Crypto.hashVerKeyKES vkey)


instance SerialiseAsRawBytes (VerificationKey UnsoundPureKesKey) where
    serialiseToRawBytes (KesVerificationKey vk) =
      Crypto.rawSerialiseVerKeyKES vk

    deserialiseFromRawBytes (AsVerificationKey AsUnsoundPureKesKey) bs =
      KesVerificationKey <$>
        Crypto.rawDeserialiseVerKeyKES bs

instance SerialiseAsRawBytes (SigningKey UnsoundPureKesKey) where
    serialiseToRawBytes (KesSigningKey sk) =
      Crypto.rawSerialiseUnsoundPureSignKeyKES sk

    deserialiseFromRawBytes (AsSigningKey AsUnsoundPureKesKey) bs =
      KesSigningKey <$> Crypto.rawDeserialiseUnsoundPureSignKeyKES bs

instance SerialiseAsBech32 (VerificationKey UnsoundPureKesKey) where
    bech32PrefixFor         _ =  "kes_vk"
    bech32PrefixesPermitted _ = ["kes_vk"]

instance SerialiseAsBech32 (SigningKey UnsoundPureKesKey) where
    bech32PrefixFor         _ =  "kes_sk"
    bech32PrefixesPermitted _ = ["kes_sk"]


newtype instance Hash UnsoundPureKesKey =
    UnsoundPureKesKeyHash (Shelley.Hash StandardCrypto
                             (Shelley.VerKeyKES StandardCrypto))
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash UnsoundPureKesKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash UnsoundPureKesKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash UnsoundPureKesKey) where
    serialiseToRawBytes (UnsoundPureKesKeyHash vkh) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsUnsoundPureKesKey) bs =
      UnsoundPureKesKeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey UnsoundPureKesKey) where
    textEnvelopeType _ = "KesVerificationKey_"
                      <> fromString (Crypto.algorithmNameKES proxy)
      where
        proxy :: Proxy (Shelley.KES StandardCrypto)
        proxy = Proxy

instance HasTextEnvelope (SigningKey UnsoundPureKesKey) where
    textEnvelopeType _ = "KesSigningKey_"
                      <> fromString (Crypto.algorithmNameKES proxy)
      where
        proxy :: Proxy (Shelley.KES StandardCrypto)
        proxy = Proxy


--
-- VRF keys
--

data VrfKey

instance HasTypeProxy VrfKey where
    data AsType VrfKey = AsVrfKey
    proxyToAsType _ = AsVrfKey

instance Key VrfKey where

    newtype VerificationKey VrfKey =
        VrfVerificationKey (Shelley.VerKeyVRF StandardCrypto)
      deriving stock (Eq)
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey VrfKey)
      deriving newtype (EncCBOR, DecCBOR, ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey VrfKey =
        VrfSigningKey (Shelley.SignKeyVRF StandardCrypto)
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
        proxy :: Proxy (Shelley.VRF StandardCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey VrfKey -> VerificationKey VrfKey
    getVerificationKey (VrfSigningKey sk) =
        VrfVerificationKey (Crypto.deriveVerKeyVRF sk)

    verificationKeyHash :: VerificationKey VrfKey -> Hash VrfKey
    verificationKeyHash (VrfVerificationKey vkey) =
        VrfKeyHash (Shelley.hashVerKeyVRF vkey)

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
    bech32PrefixFor         _ =  "vrf_vk"
    bech32PrefixesPermitted _ = ["vrf_vk"]

instance SerialiseAsBech32 (SigningKey VrfKey) where
    bech32PrefixFor         _ =  "vrf_sk"
    bech32PrefixesPermitted _ = ["vrf_sk"]

newtype instance Hash VrfKey =
    VrfKeyHash (Shelley.Hash StandardCrypto
                             (Shelley.VerKeyVRF StandardCrypto))
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
        proxy :: Proxy (Shelley.VRF StandardCrypto)
        proxy = Proxy

instance HasTextEnvelope (SigningKey VrfKey) where
    textEnvelopeType _ = "VrfSigningKey_" <> fromString (Crypto.algorithmNameVRF proxy)
      where
        proxy :: Proxy (Shelley.VRF StandardCrypto)
        proxy = Proxy

