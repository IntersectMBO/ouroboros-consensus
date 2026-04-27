{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Generic interface used by implementations of voting committees.
--
-- NOTE: concrete implementations might not need to implement all these
-- interfaces, especially the ones regarding VRF-based eligibility proofs and
-- aggregate vote signature verification.
module Ouroboros.Consensus.Committee.Crypto
  ( -- * Core types associated to voting committees
    PrivateKey
  , PublicKey
  , ElectionId
  , VoteCandidate

    -- * Vote signing interface
  , CryptoSupportsVoteSigning (..)

    -- * VRF-based eligibility proofs interface
  , VRFPoolContext (..)
  , NormalizedVRFOutput (..)
  , CryptoSupportsVRF (..)

    -- * Aggregate verification interface
  , CryptoSupportsAggregateVoteSigning (..)
  , CryptoSupportsBatchVRFVerification (..)
  ) where

import Cardano.Ledger.BaseTypes (Nonce)
import Data.Containers.NonEmpty (HasNonEmpty (..))
import Data.Kind (Type)
import Data.Proxy (Proxy)

-- * Core types associated to voting committees

-- | Private key used within the voting committee
type family PrivateKey crypto :: Type

-- | Public key used within the voting committee
type family PublicKey crypto :: Type

-- | Election identifiers
type family ElectionId crypto :: Type

-- | Vote candidates, i.e., what's being voted for
type family VoteCandidate crypto :: Type

-- * Vote signing interface

-- | Crypto interface used for signing and verifying votes
class CryptoSupportsVoteSigning crypto where
  -- | Key used for signing votes
  type VoteSigningKey crypto :: Type

  -- | Key used for verifying votes
  type VoteVerificationKey crypto :: Type

  -- | Cryptographic signature of a vote
  data VoteSignature crypto :: Type

  -- | Derive a signing key from a voting committee private key
  getVoteSigningKey ::
    Proxy crypto ->
    PrivateKey crypto ->
    VoteSigningKey crypto

  -- | Derive a verification key from a voting committee public key
  getVoteVerificationKey ::
    Proxy crypto ->
    PublicKey crypto ->
    VoteVerificationKey crypto

  -- | Sign a vote candidate in a given election
  signVote ::
    VoteSigningKey crypto ->
    ElectionId crypto ->
    VoteCandidate crypto ->
    VoteSignature crypto

  -- | Verify the signature of a vote candidate in a given election
  verifyVoteSignature ::
    VoteVerificationKey crypto ->
    ElectionId crypto ->
    VoteCandidate crypto ->
    VoteSignature crypto ->
    Either String ()

-- * VRF-based eligibility proofs interface

-- | Context in which a VRF input is evaluated.
--
-- This distinguishes between the case where we want to compute our own VRF
-- output, and the case where we want to verify the VRF output of someone else.
data VRFPoolContext crypto
  = -- | Compute our own VRF output by signing the VRF input with our signing key
    VRFSignContext (VRFSigningKey crypto)
  | -- | Verify the local sortition output of another participant by verifying
    -- their signature over the VRF input using their verification key
    VRFVerifyContext (VRFVerificationKey crypto) (VRFOutput crypto)

-- | Normalized VRF outputs as a rational between 0 and 1
newtype NormalizedVRFOutput = NormalizedVRFOutput
  { unNormalizedVRFOutput :: Rational
  }
  deriving (Eq, Show)

-- | Crypto interface used to proof eligibility via local sortition
class CryptoSupportsVRF crypto where
  -- | Private key used for computing our own VRF output
  type VRFSigningKey crypto :: Type

  -- | Public key used for verifying the VRF output of other participants
  type VRFVerificationKey crypto :: Type

  -- | Input to the verifiable random function.
  --
  -- This is fixed across all participants for a given election.
  data VRFElectionInput crypto :: Type

  -- | Output of the verifiable random function
  data VRFOutput crypto :: Type

  -- | Derive a VRF signing key from a voting committee private key
  getVRFSigningKey ::
    Proxy crypto ->
    PrivateKey crypto ->
    VRFSigningKey crypto

  -- | Derive a VRF verification key from a voting committee public key
  getVRFVerificationKey ::
    Proxy crypto ->
    PublicKey crypto ->
    VRFVerificationKey crypto

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

-- * Aggregate verification interface

-- | Crypto interface used for verifying aggregate vote signatures
class CryptoSupportsAggregateVoteSigning crypto where
  -- | Aggregate vote verification keys
  type AggregateVoteVerificationKey crypto :: Type

  -- | Aggregate vote signatures
  type AggregateVoteSignature crypto :: Type

  -- | Aggregate vote verification keys
  aggregateVoteVerificationKeys ::
    Proxy crypto ->
    NE [VoteVerificationKey crypto] ->
    Either String (AggregateVoteVerificationKey crypto)

  -- | Aggregate vote signatures
  aggregateVoteSignatures ::
    Proxy crypto ->
    NE [VoteSignature crypto] ->
    Either String (AggregateVoteSignature crypto)

  -- | Verify an aggregate vote signature for a given election and candidate.
  verifyAggregateVoteSignature ::
    Proxy crypto ->
    AggregateVoteVerificationKey crypto ->
    ElectionId crypto ->
    VoteCandidate crypto ->
    AggregateVoteSignature crypto ->
    Either String ()

-- | Crypto interface used for verifying multiple VRF outputs at once.
class CryptoSupportsBatchVRFVerification crypto where
  -- | Verify a list of VRF outputs for a given election input using the
  -- corresponding verification keys of their issuers.
  --
  -- NOTE: this expects non-aggregate VRF verification keys and VRF outputs
  -- because the implementation should be able to first bind each key to its
  -- corresponding VRF output via linearization. This is needed to avoid
  -- swap-attacks where an adversary could swap their VRF output with someone
  -- else's before forging a certificate, stealing their (more favorable)
  -- eligibility proof.
  batchVerifyVRFOutputs ::
    NE [VRFVerificationKey crypto] ->
    VRFElectionInput crypto ->
    NE [VRFOutput crypto] ->
    Either String ()
