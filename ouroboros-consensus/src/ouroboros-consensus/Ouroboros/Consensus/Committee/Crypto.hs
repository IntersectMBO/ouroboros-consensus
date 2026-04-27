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

--
-- NOTE: vote signatures and VRF outputs are treated asymmetrically here.
--
-- On one hand, individual vote signatures are used to prove the identity of
-- of their issuers for a given election and candidate being voted for. When
-- forging a certificate, we might want to aggregate the signatures of multiple
-- voters that participated in the same election and voted for the same
-- candidate into a single aggregate signature that attests the participation of
-- the entire group of voters. Such a signature is much smaller than the sum of
-- the individual signatures of each voter, and can be verified more efficiently
-- than verifying each individual signature separately. To do so, verifiers will
-- first need to create an aggregate verification key by combining the
-- verification key of each voter in the group (whose identity will likely have
-- to be declared in the corresponding certificate), and then verify the
-- aggregate signature using the aggregate verification key in a single step.
-- Note that since all voters sign the same thing (election ID and candidate),
-- swapping the signatures of two voters in the group would not have any effect
-- on the aggregate signature.
--
-- On the other hand, VRF outputs attest both the eligibility of a single voter
-- to participate in a given election /and/ the number of seats they are
-- entitled to in such election (which can directly affect their voting power).
-- Because of this, their VRF outputs cannot be aggregated into a single one
-- when forging a certificate, and must instead be included individually. When
-- verifying a certificate, however, we can still take advantage of aggregation
-- to verify the VRF outputs of all voters in a single step, but since each VRF
-- output might grant each voter a different number of seats, we need to be
-- careful about swap-attacks. This is where an adversary could swap their VRF
-- output with someone else's before forging a certificate, stealing their
-- (more favorable) eligibility proof. To avoid this, the interface for batch
-- VRF verification explicitly expects unaggregated VRF verification keys and
-- VRF outputs, so that the implementation should be able to first bind each
-- VRF output to the corresponding voter's verification key via linearization.
-- In layman terms, this means multiplying each VRF output by a unique scalar
-- before aggregating them. This enforces that, during verification, the order
-- of the VRF outputs must match the order of the verification keys verification
-- keys, thus avoiding any attempt of swapping VRF outputs between voters.

-- | Crypto interface used for creating and verifying aggregate vote signatures
class
  CryptoSupportsVoteSigning crypto =>
  CryptoSupportsAggregateVoteSigning crypto
  where
  -- | Aggregate vote verification keys
  type AggregateVoteVerificationKey crypto :: Type

  -- | Aggregate vote signatures
  type AggregateVoteSignature crypto :: Type

  -- | Combine multiple vote verification keys into a single aggregate one
  aggregateVoteVerificationKeys ::
    Proxy crypto ->
    NE [VoteVerificationKey crypto] ->
    Either String (AggregateVoteVerificationKey crypto)

  -- | Combine multiple vote signatures into a single aggregate one
  aggregateVoteSignatures ::
    Proxy crypto ->
    NE [VoteSignature crypto] ->
    Either String (AggregateVoteSignature crypto)

  -- | Verify an aggregate vote signature for a given election and candidate
  verifyAggregateVoteSignature ::
    Proxy crypto ->
    AggregateVoteVerificationKey crypto ->
    ElectionId crypto ->
    VoteCandidate crypto ->
    AggregateVoteSignature crypto ->
    Either String ()

-- | Crypto interface used for verifying multiple VRF outputs at once
class
  CryptoSupportsVRF crypto =>
  CryptoSupportsBatchVRFVerification crypto
  where
  -- | Verify a list of VRF outputs for a given election input using the
  -- corresponding verification keys of their issuers.
  --
  -- NOTE: this expects non-aggregate VRF verification keys and VRF outputs so
  -- that each (key_i, output_i) pair can be bound at verification time (e.g.
  -- via linearization). This per-pair binding defeats swap-attacks where an
  -- adversary swaps their VRF output with someone else's more-favorable one
  -- before forging a certificate.
  batchVerifyVRFOutputs ::
    NE [VRFVerificationKey crypto] ->
    VRFElectionInput crypto ->
    NE [VRFOutput crypto] ->
    Either String ()
