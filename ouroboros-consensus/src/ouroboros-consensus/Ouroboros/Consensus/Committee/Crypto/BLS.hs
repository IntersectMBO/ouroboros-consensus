{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

-- | BLS crypto helpers to instantiate voting committees.
--
-- NOTE: this module is meant to be imported qualified.
module Ouroboros.Consensus.Committee.Crypto.BLS
  ( -- * BLS crypto helpers to instantiate voting committees
    KeyRole (..)
  , KeyScope
  , PrivateKey
  , rawDeserialisePrivateKey
  , rawSerialisePrivateKey
  , coercePrivateKey
  , derivePublicKey
  , PublicKey
  , rawDeserialisePublicKey
  , rawSerialisePublicKey
  , coercePublicKey
  , Signature
  , ProofOfPossession
  , HasBLSContext (..)
  , signWithRole
  , verifyWithRole
  , createProofOfPossession
  , verifyProofOfPossession

    -- * Aggregate keys and signatures
  , aggregatePublicKeys
  , aggregateSignatures

    -- * VRF signature manipulation
  , signatureNatural
  , signatureNaturalMax
  , toNormalizedVRFOutput

    -- * Linearized VRF output verification
  , linearizeAndVerifyVRFs
  ) where

import Cardano.Binary (FromCBOR, ToCBOR)
import Cardano.Crypto.DSIGN
  ( BLS12381MinSigDSIGN
  , BLS12381SignContext (..)
  , DSIGNAggregatable (..)
  , DSIGNAlgorithm (..)
  , SigDSIGN (..)
  , VerKeyDSIGN (..)
  )
import Cardano.Crypto.EllipticCurve.BLS12_381 (blsIsInf, blsMSM)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Crypto.Util (SignableRepresentation, bytesToNatural)
import Cardano.Ledger.Hashes (HASH, KeyHash (..), StakePool)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Containers.NonEmpty (HasNonEmpty (..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Proxy (Proxy (..))
import GHC.Natural (Natural)
import Ouroboros.Consensus.Committee.Crypto (NormalizedVRFOutput (..))

-- * BLS crypto helpers to instantiate voting committees

-- | Key roles
type data KeyRole
  = -- | Key role for signing votes
    SIGN
  | -- | Key role for local sortition in elections
    VRF
  | -- | Key role for Proof of Possession
    POP

-- | Key scope, later instantiated with usage and network id (e.g. PERAS/MAINNET)
type KeyScope = ByteString

-- | BLS private key type, parameterized by key role
type PrivateKey :: KeyRole -> Type
data PrivateKey r = PrivateKey
  { unPrivateKey :: !(SignKeyDSIGN BLS12381MinSigDSIGN)
  , privateKeyScope :: !KeyScope
  }
  deriving stock (Eq, Show)

rawDeserialisePrivateKey ::
  KeyScope ->
  ByteString ->
  Maybe (PrivateKey r)
rawDeserialisePrivateKey scope bs = do
  key <- rawDeserialiseSignKeyDSIGN bs
  pure $
    PrivateKey
      { unPrivateKey = key
      , privateKeyScope = scope
      }

rawSerialisePrivateKey ::
  PrivateKey r ->
  ByteString
rawSerialisePrivateKey =
  rawSerialiseSignKeyDSIGN . unPrivateKey

coercePrivateKey ::
  forall r2 r1.
  PrivateKey r1 ->
  PrivateKey r2
coercePrivateKey = coerce

derivePublicKey ::
  PrivateKey r ->
  PublicKey r
derivePublicKey sk =
  PublicKey
    { unPublicKey = deriveVerKeyDSIGN (unPrivateKey sk)
    , publicKeyScope = privateKeyScope sk
    }

-- | BLS public key type, parameterized by key role
type PublicKey :: KeyRole -> Type
data PublicKey r = PublicKey
  { unPublicKey :: !(VerKeyDSIGN BLS12381MinSigDSIGN)
  , publicKeyScope :: !(KeyScope)
  }
  deriving stock (Eq, Show)

rawDeserialisePublicKey ::
  KeyScope ->
  ByteString ->
  Maybe (PublicKey r)
rawDeserialisePublicKey scope bs = do
  key <- rawDeserialiseVerKeyDSIGN bs
  pure $
    PublicKey
      { unPublicKey = key
      , publicKeyScope = scope
      }

rawSerialisePublicKey ::
  PublicKey r ->
  ByteString
rawSerialisePublicKey =
  rawSerialiseVerKeyDSIGN . unPublicKey

coercePublicKey ::
  forall r2 r1.
  PublicKey r1 ->
  PublicKey r2
coercePublicKey = coerce

-- | BLS signature type, parameterized by key role
type Signature :: KeyRole -> Type
newtype Signature r = Signature
  { unSignature :: SigDSIGN BLS12381MinSigDSIGN
  }
  deriving stock (Eq, Show)
  deriving newtype (FromCBOR, ToCBOR)

-- | BLS proof of possession type
newtype ProofOfPossession = ProofOfPossession
  { unProofOfPossession :: PossessionProofDSIGN BLS12381MinSigDSIGN
  }
  deriving stock (Eq, Show)
  deriving newtype (FromCBOR, ToCBOR)

-- TODO: get these contexts directly from @cardano-base@ after
-- https://github.com/IntersectMBO/cardano-base/pull/635
-- is merged.

-- Basic over G1:
-- https://www.ietf.org/archive/id/draft-irtf-cfrg-bls-signature-06.html#section-4.2.1-1
minSigSignatureDST :: BLS12381SignContext
minSigSignatureDST =
  BLS12381SignContext
    { blsSignContextDst = Just "BLS_SIG_BLS12381G1_XMD:SHA-256_SSWU_RO_NUL_"
    , blsSignContextAug = Nothing
    }

-- PoP over G1:
-- https://www.ietf.org/archive/id/draft-irtf-cfrg-bls-signature-06.html#section-4.2.3-1
minSigPoPDST :: BLS12381SignContext
minSigPoPDST =
  BLS12381SignContext
    { blsSignContextDst = Just "BLS_SIG_BLS12381G1_XMD:SHA-256_SSWU_RO_POP_"
    , blsSignContextAug = Nothing
    }

-- | Role-separated BLS contexts for  signatures
class HasBLSContext (r :: KeyRole) where
  blsCtx :: Proxy r -> KeyScope -> BLS12381SignContext

instance HasBLSContext SIGN where
  blsCtx _ keyScope =
    minSigSignatureDST
      { blsSignContextAug =
          Just ("VOTE:" <> keyScope <> ":V0")
      }

instance HasBLSContext VRF where
  blsCtx _ keyScope =
    minSigSignatureDST
      { blsSignContextAug =
          Just ("VRF:" <> keyScope <> ":V0")
      }

instance HasBLSContext POP where
  blsCtx _ keyScope =
    minSigPoPDST
      { blsSignContextAug =
          Just ("POP:" <> keyScope <> ":V0")
      }

-- | Sign a message with a  private key, producing a  signature
signWithRole ::
  forall r msg.
  ( SignableRepresentation msg
  , HasBLSContext r
  ) =>
  PrivateKey r ->
  msg ->
  Signature r
signWithRole sk msg =
  Signature
    { unSignature =
        signDSIGN
          (blsCtx (Proxy @r) (privateKeyScope sk))
          msg
          (unPrivateKey sk)
    }

-- | Verify a  signature on a message with a  public key
verifyWithRole ::
  forall r msg.
  ( SignableRepresentation msg
  , HasBLSContext r
  ) =>
  PublicKey r ->
  msg ->
  Signature r ->
  Either String ()
verifyWithRole pk msg (Signature sig) =
  verifyDSIGN
    (blsCtx (Proxy @r) (publicKeyScope pk))
    (unPublicKey pk)
    msg
    sig

-- | Create a proof of possession signature for a  private key
createProofOfPossession ::
  PrivateKey POP ->
  KeyHash StakePool ->
  ProofOfPossession
createProofOfPossession sk stakePoolHash =
  ProofOfPossession
    { unProofOfPossession =
        createPossessionProofDSIGN
          extCtx
          (unPrivateKey sk)
    }
 where
  poolBytes = Hash.hashToBytes (unKeyHash stakePoolHash)
  baseCtx = blsCtx (Proxy @POP) (privateKeyScope sk)
  extCtx = baseCtx{blsSignContextAug = blsSignContextAug baseCtx <> Just poolBytes}

-- | Verify a proof of possession signature for a public key
verifyProofOfPossession ::
  PublicKey POP ->
  KeyHash StakePool ->
  ProofOfPossession ->
  Either String ()
verifyProofOfPossession pk stakePoolHash pop =
  verifyPossessionProofDSIGN
    extCtx
    (unPublicKey pk)
    (unProofOfPossession pop)
 where
  poolBytes = Hash.hashToBytes (unKeyHash stakePoolHash)
  baseCtx = blsCtx (Proxy @POP) (publicKeyScope pk)
  extCtx = baseCtx{blsSignContextAug = blsSignContextAug baseCtx <> Just poolBytes}

-- * Aggregate keys and signatures

-- | Aggregate multiple public keys into a single one.
--
-- PRECONDITION: all keys must have the same scope.
--
-- PRECONDITION: this assumes that proofs of possession have already been
-- verified for all keys in advance.
aggregatePublicKeys ::
  NE [PublicKey r] ->
  Either String (PublicKey r)
aggregatePublicKeys keys@(firstKey :| restKeys) = do
  -- Ensure all keys have the same scope before aggregation
  when (any (/= publicKeyScope firstKey) (fmap publicKeyScope restKeys)) $
    Left "Cannot aggregate public keys with different scopes"
  aggKey <-
    uncheckedAggregateVerKeysDSIGN
      . fmap unPublicKey
      . NonEmpty.toList
      $ keys
  pure $
    PublicKey
      { unPublicKey = aggKey
      , publicKeyScope = publicKeyScope firstKey
      }

-- | Aggregate multiple signatures into a single one
aggregateSignatures ::
  NE [Signature r] ->
  Either String (Signature r)
aggregateSignatures sigs =
  fmap Signature
    . aggregateSigsDSIGN
    . fmap unSignature
    . NonEmpty.toList
    $ sigs

-- * VRF signature manipulation

-- | Convert a BLS signature to a natural number for use in local sortition
signatureNatural ::
  Signature VRF ->
  Natural
signatureNatural sig =
  bytesToNatural
    . Hash.hashToBytes
    . Hash.hashWith @HASH rawSerialiseSigDSIGN
    . unSignature
    $ sig

-- | The maximum natural number that can be produced by a BLS signature
signatureNaturalMax :: Natural
signatureNaturalMax =
  2 ^ ((8 :: Integer) * hashSize) - 1
 where
  hashSize =
    fromIntegral (Hash.hashSize (Proxy :: Proxy HASH))

-- | Create a normalized VRF output from a BLS signature
toNormalizedVRFOutput ::
  Signature VRF ->
  NormalizedVRFOutput
toNormalizedVRFOutput sig =
  NormalizedVRFOutput $
    fromIntegral (signatureNatural sig)
      / fromIntegral signatureNaturalMax

-- * Linearized VRF output verification

-- | Verify a list of VRF outputs against on the same input using linearization.
--
-- The idea is to first aggregate all public keys and VRF outputs into a single
-- aggregate ones. These can then be verified in one go, saving the (higher)
-- cost of multiple signature verifications.
--
-- However, since we later derive a numeric value from each individual VRF
-- output, verifying the aggregate signature alone is not sufficient. This is
-- because an attacker could swap their (bad) VRF output with someone else's
-- (better) one, and a naive signature aggregation and verification approach
-- would still succeed.
--
-- Instead, each VRF output is first linearized using a scalar derived from the
-- signature itself, and then aggregated together. This way, if an attacker
-- tries to swap their VRF output with someone else's, the linearization will
-- produce a different aggregate signature that will fail verification.
--
-- PRECONDITION: all keys must have the same scope.
--
-- PRECONDITION: the number of signatures must match the number of keys.
linearizeAndVerifyVRFs ::
  SignableRepresentation msg =>
  NE [PublicKey VRF] ->
  msg ->
  NE [Signature VRF] ->
  Either String ()
linearizeAndVerifyVRFs keys@(firstKey :| restKeys) msg sigs = do
  when (any (/= publicKeyScope firstKey) (fmap publicKeyScope restKeys)) $
    Left "Cannot aggregate public keys with different scopes"

  when (length sigs /= length keys) $
    Left "Number of signatures must match number of public keys"

  let scalars =
        NonEmpty.map
          (fromIntegral . signatureNatural)
          sigs

  let linearizedKeyPoint =
        blsMSM
          . NonEmpty.toList
          . NonEmpty.zip scalars
          . NonEmpty.map (\(PublicKey (VerKeyBLS12381 p) _) -> p)
          $ keys

  let linearizedSigPoint =
        blsMSM
          . NonEmpty.toList
          . NonEmpty.zip scalars
          . NonEmpty.map (\(Signature (SigBLS12381 p)) -> p)
          $ sigs

  when (blsIsInf linearizedKeyPoint) $
    Left "Resulting key point is at infinity, cannot linearize"

  when (blsIsInf linearizedSigPoint) $
    Left "Resulting signature point is at infinity, cannot linearize"

  let linearizedKey =
        PublicKey
          (VerKeyBLS12381 linearizedKeyPoint)
          (publicKeyScope firstKey)

  let linearizedSig =
        Signature
          (SigBLS12381 linearizedSigPoint)

  verifyWithRole @VRF linearizedKey msg linearizedSig
