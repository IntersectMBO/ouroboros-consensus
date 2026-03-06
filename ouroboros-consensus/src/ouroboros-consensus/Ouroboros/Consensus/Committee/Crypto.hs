{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Crypto interface needed for committee selection
module Ouroboros.Consensus.Committee.Crypto
  ( -- * Types associated to committee selection
    ElectionId

    -- * Signature crypto interface
  , CryptoSupportsSignature (..)

    -- * VRF crypto interface
  , VRFPoolContext (..)
  , CryptoSupportsVRF (..)
  ) where

import Cardano.Ledger.BaseTypes (Nonce)
import Data.Kind (Type)

-- * Types associated to committee selection

-- | Election identifier used for committee seleciton
type family ElectionId c :: Type

-- * Signature crypto interface

-- | Crypto interface used for signing and verifying payloads
class CryptoSupportsSignature c where
  -- | Private key for signing
  type SignaturePrivateKey c :: Type

  -- | Public key used for verification
  type SignaturePublicKey c :: Type

  -- | Payload to be signed
  data Payload c :: Type

  -- | Signature over a payload
  data Signature c :: Type

  -- | Sign a message using the given private key
  signPayload ::
    SignaturePrivateKey c ->
    Payload c ->
    Signature c

  -- | Verify a signature over a payload using the given public key
  verifyPayloadSignature ::
    SignaturePublicKey c ->
    Payload c ->
    Signature c ->
    Either String ()

-- * VRF crypto interface

-- | Context in which a VRF input is evaluated.
--
-- This distinguishes between the case where we want to compute our own VRF
-- output, and the case where we want to verify the VRF output of someone else.
data VRFPoolContext c
  = -- | Compute our own VRF output by signing the VRF input with our signing key
    VRFSignContext (VRFSigningKey c)
  | -- | Verify the local sortition output of another participant by verifying
    -- their signature over the VRF input using their verification key
    VRFVerifyContext (VRFVerifyKey c) (VRFOutput c)

-- | Crypto interface used to evaluate non-persistent voters via local sortition
class CryptoSupportsSignature c => CryptoSupportsVRF c where
  -- | Private key used for computing our own VRF output
  type VRFSigningKey c :: Type

  -- | Public key used for verifying the VRF output of other participants
  type VRFVerifyKey c :: Type

  -- | Input to the verifiable random function.
  --
  -- This is fixed across all participants for a given election.
  data VRFElectionInput c :: Type

  -- | Output of the verifiable random function
  data VRFOutput c :: Type

  -- | Construct a VRF input from a nonce and an election identifier
  mkVRFElectionInput ::
    Nonce ->
    ElectionId c ->
    VRFElectionInput c

  -- | Evaluate a VRF input in a given context
  evalVRF ::
    VRFPoolContext c ->
    VRFElectionInput c ->
    Either String (VRFOutput c)

  -- | Normalize a VRF output to a value in [0, 1]
  normalizeVRFOutput ::
    VRFOutput c ->
    Rational
