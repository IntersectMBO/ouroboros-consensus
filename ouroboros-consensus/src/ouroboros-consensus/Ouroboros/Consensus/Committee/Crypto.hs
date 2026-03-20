{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Crypto interface needed for committee selection
module Ouroboros.Consensus.Committee.Crypto
  ( -- * Types associated to committee selection
    ElectionId

    -- * Vote signature crypto interface
  , CryptoSupportsVoteSigning (..)

    -- * VRF crypto interface
  , VRFPoolContext (..)
  , NormalizedVRFOutput (..)
  , CryptoSupportsVRF (..)
  ) where

import Cardano.Ledger.BaseTypes (Nonce)
import Data.Kind (Type)

-- * Types associated to committee selection

-- | Election identifier used for committee seleciton
type family ElectionId crypto :: Type

-- * Signature crypto interface

-- | Crypto interface used for signing and verifying votes
class CryptoSupportsVoteSigning crypto where
  -- | Private key for signing
  type VoteSignaturePrivateKey crypto :: Type

  -- | Public key used for verification
  type VoteSignaturePublicKey crypto :: Type

  -- | Message to be signed in conjunction with the election identifier
  data VoteMessage crypto :: Type

  -- | Signature over an election identifier
  data VoteSignature crypto :: Type

  -- | Sign a vote election identifier
  signVote ::
    VoteSignaturePrivateKey crypto ->
    ElectionId crypto ->
    VoteMessage crypto ->
    VoteSignature crypto

  -- | Verify a vote signature over an election identifier
  verifyVoteSignature ::
    VoteSignaturePublicKey crypto ->
    ElectionId crypto ->
    VoteMessage crypto ->
    VoteSignature crypto ->
    Either String ()

-- * VRF crypto interface

-- | Context in which a VRF input is evaluated.
--
-- This distinguishes between the case where we want to compute our own VRF
-- output, and the case where we want to verify the VRF output of someone else.
data VRFPoolContext crypto
  = -- | Compute our own VRF output by signing the VRF input with our signing key
    VRFSignContext (VRFSigningKey crypto)
  | -- | Verify the local sortition output of another participant by verifying
    -- their signature over the VRF input using their verification key
    VRFVerifyContext (VRFVerifyKey crypto) (VRFOutput crypto)

-- | Normalized VRF outputs as a rational between 0 and 1
newtype NormalizedVRFOutput = NormalizedVRFOutput
  { unNormalizedVRFOutput :: Rational
  }
  deriving (Eq, Show)

-- | Crypto interface used to evaluate non-persistent voters via local sortition
class CryptoSupportsVRF crypto where
  -- | Private key used for computing our own VRF output
  type VRFSigningKey crypto :: Type

  -- | Public key used for verifying the VRF output of other participants
  type VRFVerifyKey crypto :: Type

  -- | Input to the verifiable random function.
  --
  -- This is fixed across all participants for a given election.
  data VRFElectionInput crypto :: Type

  -- | Output of the verifiable random function
  data VRFOutput crypto :: Type

  -- | Construct a VRF input from a nonce and an election identifier
  mkVRFElectionInput ::
    Nonce ->
    ElectionId crypto ->
    VRFElectionInput crypto

  -- | Evaluate a VRF input in a given context
  evalVRF ::
    VRFPoolContext crypto ->
    VRFElectionInput crypto ->
    Either String (VRFOutput crypto)

  -- | Normalize a VRF output to a value in [0, 1]
  normalizeVRFOutput ::
    VRFOutput crypto ->
    NormalizedVRFOutput
