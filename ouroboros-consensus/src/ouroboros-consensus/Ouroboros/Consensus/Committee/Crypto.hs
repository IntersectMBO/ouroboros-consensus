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
type family ElectionId c :: Type

-- * Signature crypto interface

-- | Crypto interface used for signing and verifying votes
class CryptoSupportsVoteSigning c where
  -- | Private key for signing
  type VoteSignaturePrivateKey c :: Type

  -- | Public key used for verification
  type VoteSignaturePublicKey c :: Type

  -- | Message to be signed in conjunction with the election identifier
  data VoteMessage c :: Type

  -- | Signature over an election identifier
  data VoteSignature c :: Type

  -- | Sign a vote election identifier
  signVote ::
    VoteSignaturePrivateKey c ->
    ElectionId c ->
    VoteMessage c ->
    VoteSignature c

  -- | Verify a vote signature over an election identifier
  verifyVoteSignature ::
    VoteSignaturePublicKey c ->
    ElectionId c ->
    VoteMessage c ->
    VoteSignature c ->
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

-- | Normalized VRF outputs as a rational between 0 and 1
newtype NormalizedVRFOutput = NormalizedVRFOutput
  { unNormalizedVRFOutput :: Rational
  }
  deriving (Eq, Show)

-- | Crypto interface used to evaluate non-persistent voters via local sortition
class CryptoSupportsVRF c where
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
    NormalizedVRFOutput
