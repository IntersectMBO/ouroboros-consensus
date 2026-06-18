{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | The minimal set of @cardano-api@ key roles that the db-tools need in order
-- to load Praos and Byron forging credentials: the stake-pool cold key (carried
-- by an operational certificate), the VRF key, the (unsound, in-memory) KES key
-- and the Byron payment key. Also defines the 'OperationalCertificate' itself.
--
-- The serialisation machinery lives in
-- "Ouroboros.Consensus.Cardano.Api.Serialise".
module Ouroboros.Consensus.Cardano.Api.Keys
  ( -- * The key interface
    Key (..)

    -- * Key roles
  , ByronKey
  , StakePoolKey
  , UnsoundPureKesKey
  , VrfKey

    -- * Operational certificates
  , OperationalCertificate (..)
  , getHotKey

    -- * Data family instances
  , AsType (..)
  , Hash (..)
  , SigningKey (..)
  , VerificationKey (..)
  ) where

import qualified Cardano.Chain.Common as Byron
import Cardano.Crypto.DSIGN (SignKeyDSIGN)
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.Hashing as Byron
import qualified Cardano.Crypto.KES.Class as Crypto
import qualified Cardano.Crypto.Signing as Byron
import qualified Cardano.Crypto.Signing as Crypto
import qualified Cardano.Crypto.VRF.Class as Crypto
import qualified Cardano.Crypto.Wallet as Crypto.HD
import Cardano.Ledger.Binary
  ( byronProtVer
  , fromPlainDecoder
  , toPlainDecoder
  , toPlainEncoding
  )
import qualified Cardano.Ledger.Binary as CBOR
  ( CBORGroup (..)
  , shelleyProtVer
  , toPlainDecoder
  , toPlainEncoding
  )
import Cardano.Ledger.Hashes (HASH)
import Cardano.Ledger.Keys (DSIGN)
import qualified Cardano.Ledger.Keys as Shelley
import Cardano.Protocol.Crypto (Crypto (..), StandardCrypto)
import qualified Cardano.Protocol.TPraos.OCert as Shelley
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR (toStrictByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Kind (Type)
import Data.String (IsString (..))
import Ouroboros.Consensus.Cardano.Api.Serialise

-- ----------------------------------------------------------------------------
-- The key interface
--

-- | An interface for cryptographic keys used for signatures with a 'SigningKey'
-- and a 'VerificationKey' key.
--
-- This interface does not provide actual signing or verifying functions since
-- this API is concerned with the management of keys: deserialising them and
-- relating signing keys, verification keys and their hashes.
class
  ( Eq (VerificationKey keyrole)
  , Show (VerificationKey keyrole)
  , HasTextEnvelope (VerificationKey keyrole)
  , HasTextEnvelope (SigningKey keyrole)
  ) =>
  Key keyrole
  where
  -- | The type of cryptographic verification key, for each key role.
  data VerificationKey keyrole :: Type

  -- | The type of cryptographic signing key, for each key role.
  data SigningKey keyrole :: Type

  -- | Get the corresponding verification key from a signing key.
  getVerificationKey :: SigningKey keyrole -> VerificationKey keyrole

  verificationKeyHash :: VerificationKey keyrole -> Hash keyrole

instance HasTypeProxy a => HasTypeProxy (VerificationKey a) where
  data AsType (VerificationKey a) = AsVerificationKey (AsType a)
  proxyToAsType _ = AsVerificationKey (proxyToAsType (Proxy :: Proxy a))

instance HasTypeProxy a => HasTypeProxy (SigningKey a) where
  data AsType (SigningKey a) = AsSigningKey (AsType a)
  proxyToAsType _ = AsSigningKey (proxyToAsType (Proxy :: Proxy a))

-- ----------------------------------------------------------------------------
-- Stake pool keys
--

data StakePoolKey

instance HasTypeProxy StakePoolKey where
  data AsType StakePoolKey = AsStakePoolKey
  proxyToAsType _ = AsStakePoolKey

instance Key StakePoolKey where
  newtype VerificationKey StakePoolKey
    = StakePoolVerificationKey (Shelley.VKey Shelley.StakePool)
    deriving stock Eq
    deriving (Show, IsString) via UsingRawBytesHex (VerificationKey StakePoolKey)
    deriving newtype (EncCBOR, DecCBOR, ToCBOR, FromCBOR)
    deriving anyclass SerialiseAsCBOR

  newtype SigningKey StakePoolKey
    = StakePoolSigningKey (SignKeyDSIGN DSIGN)
    deriving (Show, IsString) via UsingRawBytesHex (SigningKey StakePoolKey)
    deriving newtype (ToCBOR, FromCBOR)
    deriving anyclass SerialiseAsCBOR

  getVerificationKey :: SigningKey StakePoolKey -> VerificationKey StakePoolKey
  getVerificationKey (StakePoolSigningKey sk) =
    StakePoolVerificationKey (Shelley.VKey (Crypto.deriveVerKeyDSIGN sk))

  verificationKeyHash :: VerificationKey StakePoolKey -> Hash StakePoolKey
  verificationKeyHash (StakePoolVerificationKey vkey) =
    StakePoolKeyHash (Shelley.hashKey vkey)

instance SerialiseAsRawBytes (VerificationKey StakePoolKey) where
  serialiseToRawBytes (StakePoolVerificationKey (Shelley.VKey vk)) =
    Crypto.rawSerialiseVerKeyDSIGN vk

  deserialiseFromRawBytes (AsVerificationKey AsStakePoolKey) bs =
    StakePoolVerificationKey . Shelley.VKey
      <$> Crypto.rawDeserialiseVerKeyDSIGN bs

instance SerialiseAsRawBytes (SigningKey StakePoolKey) where
  serialiseToRawBytes (StakePoolSigningKey sk) =
    Crypto.rawSerialiseSignKeyDSIGN sk

  deserialiseFromRawBytes (AsSigningKey AsStakePoolKey) bs =
    StakePoolSigningKey <$> Crypto.rawDeserialiseSignKeyDSIGN bs

newtype instance Hash StakePoolKey
  = StakePoolKeyHash (Shelley.KeyHash Shelley.StakePool)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash StakePoolKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash StakePoolKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash StakePoolKey) where
  serialiseToRawBytes (StakePoolKeyHash (Shelley.KeyHash vkh)) =
    Crypto.hashToBytes vkh

  deserialiseFromRawBytes (AsHash AsStakePoolKey) bs =
    StakePoolKeyHash . Shelley.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey StakePoolKey) where
  textEnvelopeType _ =
    "StakePoolVerificationKey_"
      <> fromString (Crypto.algorithmNameDSIGN proxy)
   where
    proxy :: Proxy Shelley.DSIGN
    proxy = Proxy

instance HasTextEnvelope (SigningKey StakePoolKey) where
  textEnvelopeType _ =
    "StakePoolSigningKey_"
      <> fromString (Crypto.algorithmNameDSIGN proxy)
   where
    proxy :: Proxy Shelley.DSIGN
    proxy = Proxy

-- ----------------------------------------------------------------------------
-- KES keys
--

data UnsoundPureKesKey

instance HasTypeProxy UnsoundPureKesKey where
  data AsType UnsoundPureKesKey = AsUnsoundPureKesKey
  proxyToAsType _ = AsUnsoundPureKesKey

instance Key UnsoundPureKesKey where
  newtype VerificationKey UnsoundPureKesKey
    = KesVerificationKey (Crypto.VerKeyKES (KES StandardCrypto))
    deriving stock Eq
    deriving (Show, IsString) via UsingRawBytesHex (VerificationKey UnsoundPureKesKey)
    deriving newtype (EncCBOR, DecCBOR, ToCBOR, FromCBOR)
    deriving anyclass SerialiseAsCBOR

  newtype SigningKey UnsoundPureKesKey
    = KesSigningKey (Crypto.UnsoundPureSignKeyKES (KES StandardCrypto))
    deriving (Show, IsString) via UsingRawBytesHex (SigningKey UnsoundPureKesKey)
    deriving newtype (ToCBOR, FromCBOR)
    deriving anyclass (EncCBOR, SerialiseAsCBOR)

  getVerificationKey :: SigningKey UnsoundPureKesKey -> VerificationKey UnsoundPureKesKey
  getVerificationKey (KesSigningKey sk) =
    KesVerificationKey (Crypto.unsoundPureDeriveVerKeyKES sk)

  verificationKeyHash :: VerificationKey UnsoundPureKesKey -> Hash UnsoundPureKesKey
  verificationKeyHash (KesVerificationKey vkey) =
    UnsoundPureKesKeyHash (Crypto.hashVerKeyKES vkey)

instance DecCBOR (SigningKey UnsoundPureKesKey) where
  decCBOR = fromPlainDecoder fromCBOR

instance SerialiseAsRawBytes (VerificationKey UnsoundPureKesKey) where
  serialiseToRawBytes (KesVerificationKey vk) =
    Crypto.rawSerialiseVerKeyKES vk

  deserialiseFromRawBytes (AsVerificationKey AsUnsoundPureKesKey) bs =
    KesVerificationKey
      <$> Crypto.rawDeserialiseVerKeyKES bs

instance SerialiseAsRawBytes (SigningKey UnsoundPureKesKey) where
  serialiseToRawBytes (KesSigningKey sk) =
    Crypto.rawSerialiseUnsoundPureSignKeyKES sk

  deserialiseFromRawBytes (AsSigningKey AsUnsoundPureKesKey) bs =
    KesSigningKey <$> Crypto.rawDeserialiseUnsoundPureSignKeyKES bs

newtype instance Hash UnsoundPureKesKey
  = UnsoundPureKesKeyHash
      ( Crypto.Hash
          HASH
          (Crypto.VerKeyKES (KES StandardCrypto))
      )
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
  textEnvelopeType _ =
    "KesVerificationKey_"
      <> fromString (Crypto.algorithmNameKES proxy)
   where
    proxy :: Proxy (KES StandardCrypto)
    proxy = Proxy

instance HasTextEnvelope (SigningKey UnsoundPureKesKey) where
  textEnvelopeType _ =
    "KesSigningKey_"
      <> fromString (Crypto.algorithmNameKES proxy)
   where
    proxy :: Proxy (KES StandardCrypto)
    proxy = Proxy

-- ----------------------------------------------------------------------------
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

-- ----------------------------------------------------------------------------
-- Byron keys
--

-- | Byron-era payment keys. Used for Byron addresses and witnessing
-- transactions that spend from these addresses.
--
-- These use Ed25519 but with a 32byte \"chaincode\" used in HD derivation.
-- The inclusion of the chaincode is a design mistake but one that cannot
-- be corrected for the Byron era. It is safe to use a zero or random chaincode
-- for new Byron keys.
--
-- This is a type level tag, used with other interfaces like 'Key'.
data ByronKey

instance HasTypeProxy ByronKey where
  data AsType ByronKey = AsByronKey
  proxyToAsType _ = AsByronKey

instance Key ByronKey where
  newtype VerificationKey ByronKey
    = ByronVerificationKey Byron.VerificationKey
    deriving stock Eq
    deriving (Show, IsString) via UsingRawBytesHex (VerificationKey ByronKey)
    deriving newtype (ToCBOR, FromCBOR)
    deriving anyclass SerialiseAsCBOR

  newtype SigningKey ByronKey
    = ByronSigningKey Byron.SigningKey
    deriving (Show, IsString) via UsingRawBytesHex (SigningKey ByronKey)
    deriving newtype (ToCBOR, FromCBOR)
    deriving anyclass SerialiseAsCBOR

  getVerificationKey :: SigningKey ByronKey -> VerificationKey ByronKey
  getVerificationKey (ByronSigningKey sk) =
    ByronVerificationKey (Byron.toVerification sk)

  verificationKeyHash :: VerificationKey ByronKey -> Hash ByronKey
  verificationKeyHash (ByronVerificationKey vkey) =
    ByronKeyHash (Byron.hashKey vkey)

instance HasTextEnvelope (VerificationKey ByronKey) where
  textEnvelopeType _ = "PaymentVerificationKeyByron_ed25519_bip32"

instance HasTextEnvelope (SigningKey ByronKey) where
  textEnvelopeType _ = "PaymentSigningKeyByron_ed25519_bip32"

instance SerialiseAsRawBytes (VerificationKey ByronKey) where
  serialiseToRawBytes (ByronVerificationKey (Byron.VerificationKey xvk)) =
    Crypto.HD.unXPub xvk

  deserialiseFromRawBytes (AsVerificationKey AsByronKey) bs =
    either
      (const Nothing)
      (Just . ByronVerificationKey . Byron.VerificationKey)
      (Crypto.HD.xpub bs)

instance SerialiseAsRawBytes (SigningKey ByronKey) where
  serialiseToRawBytes (ByronSigningKey (Byron.SigningKey xsk)) =
    CBOR.toStrictByteString $ encCBORXPrv xsk
   where
    encCBORXPrv = toPlainEncoding byronProtVer . Crypto.encCBORXPrv

  deserialiseFromRawBytes (AsSigningKey AsByronKey) bs =
    either
      (const Nothing)
      (Just . ByronSigningKey . Byron.SigningKey)
      (snd <$> CBOR.deserialiseFromBytes decCBORXPrv (LB.fromStrict bs))
   where
    decCBORXPrv = toPlainDecoder Nothing byronProtVer Byron.decCBORXPrv

newtype instance Hash ByronKey = ByronKeyHash Byron.KeyHash
  deriving (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash ByronKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash ByronKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash ByronKey) where
  serialiseToRawBytes (ByronKeyHash (Byron.KeyHash vkh)) =
    Byron.abstractHashToBytes vkh

  deserialiseFromRawBytes (AsHash AsByronKey) bs =
    ByronKeyHash . Byron.KeyHash <$> Byron.abstractHashFromBytes bs

-- ----------------------------------------------------------------------------
-- Operational certificates
--

data OperationalCertificate
  = OperationalCertificate
      !(Shelley.OCert StandardCrypto)
      !(VerificationKey StakePoolKey)
  deriving (Eq, Show)
  deriving anyclass SerialiseAsCBOR

instance ToCBOR OperationalCertificate where
  toCBOR = CBOR.toPlainEncoding CBOR.shelleyProtVer . encCBOR

instance FromCBOR OperationalCertificate where
  fromCBOR = CBOR.toPlainDecoder Nothing CBOR.shelleyProtVer decCBOR

instance EncCBOR OperationalCertificate where
  encCBOR (OperationalCertificate ocert vkey) =
    encCBOR (CBOR.CBORGroup ocert, vkey)

instance DecCBOR OperationalCertificate where
  decCBOR = do
    (CBOR.CBORGroup ocert, vkey) <- decCBOR
    return (OperationalCertificate ocert vkey)

instance HasTypeProxy OperationalCertificate where
  data AsType OperationalCertificate = AsOperationalCertificate
  proxyToAsType _ = AsOperationalCertificate

instance HasTextEnvelope OperationalCertificate where
  textEnvelopeType _ = "NodeOperationalCertificate"

getHotKey :: OperationalCertificate -> VerificationKey UnsoundPureKesKey
getHotKey (OperationalCertificate cert _) = KesVerificationKey $ Shelley.ocertVkHot cert
