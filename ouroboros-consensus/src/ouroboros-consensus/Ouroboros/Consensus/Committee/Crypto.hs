{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , CryptoSupportsAggregateVoteSigning (..)

    -- ** Trivial aggregate vote signature verification helpers
  , TrivialAggregateVoteVerificationKey (..)
  , TrivialAggregateVoteSignature (..)
  , trivialLiftVoteVerificationKey
  , trivialLiftVoteSignature
  , trvialVerifyAggregateVoteSignature

    -- * VRF-based eligibility proofs interface
  , VRFPoolContext (..)
  , NormalizedVRFOutput (..)
  , CryptoSupportsVRF (..)
  , CryptoSupportsAggregateVRF (..)

    -- ** Trivial aggregate VRF verification helpers
  , TrivialAggregateVRFVerificationKey (..)
  , TrivialAggregateVRFOutput (..)
  , trivialLiftVRFVerificationKey
  , trivialLiftVRFOutput
  , trivialVerifyAggregateVRFOutput
  ) where

import Cardano.Ledger.BaseTypes (Nonce)
import Data.Containers.NonEmpty (HasNonEmpty (..))
import Data.Either (partitionEithers)
import Data.Kind (Type)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NonEmpty
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

  -- | Derive a signing key from a voting committe private key
  getVoteSigningKey ::
    Proxy crypto ->
    PrivateKey crypto ->
    VoteSigningKey crypto

  -- | Derive a verification key from a voting committe public key
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

-- | Crypto interface used for verifying aggregate vote signatures
class
  ( Semigroup (AggregateVoteVerificationKey crypto)
  , Semigroup (AggregateVoteSignature crypto)
  ) =>
  CryptoSupportsAggregateVoteSigning crypto
  where
  -- | Key used for verifying aggregate vote signatures
  type AggregateVoteVerificationKey crypto :: Type

  -- | Aggregate cryptographic signature of a vote
  type AggregateVoteSignature crypto :: Type

  -- | Lift a single vote signature verification key into an aggregate one
  liftVoteVerificationKey ::
    Proxy crypto ->
    VoteVerificationKey crypto ->
    AggregateVoteVerificationKey crypto

  -- | Lift a single vote signature into an aggregate one
  liftVoteSignature ::
    Proxy crypto ->
    VoteSignature crypto ->
    AggregateVoteSignature crypto

  -- | Verify an aggregate vote signature for a given election and candidate
  verifyAggregateVoteSignature ::
    Proxy crypto ->
    AggregateVoteVerificationKey crypto ->
    ElectionId crypto ->
    VoteCandidate crypto ->
    AggregateVoteSignature crypto ->
    Either String ()

-- ** Trivial aggregate vote signature verification helpers

newtype TrivialAggregateVoteVerificationKey crypto
  = TrivialAggregateVoteVerificationKey (NE [VoteVerificationKey crypto])
  deriving newtype Semigroup

newtype TrivialAggregateVoteSignature crypto
  = TrivialAggregateVoteSignature (NE [VoteSignature crypto])
  deriving newtype Semigroup

trivialLiftVoteVerificationKey ::
  Proxy crypto ->
  VoteVerificationKey crypto ->
  TrivialAggregateVoteVerificationKey crypto
trivialLiftVoteVerificationKey _ =
  TrivialAggregateVoteVerificationKey
    . NonEmpty.singleton

trivialLiftVoteSignature ::
  Proxy crypto ->
  VoteSignature crypto ->
  TrivialAggregateVoteSignature crypto
trivialLiftVoteSignature _ =
  TrivialAggregateVoteSignature
    . NonEmpty.singleton

trvialVerifyAggregateVoteSignature ::
  CryptoSupportsVoteSigning crypto =>
  Proxy crypto ->
  TrivialAggregateVoteVerificationKey crypto ->
  ElectionId crypto ->
  VoteCandidate crypto ->
  TrivialAggregateVoteSignature crypto ->
  Either String ()
trvialVerifyAggregateVoteSignature
  _
  (TrivialAggregateVoteVerificationKey keys)
  electionId
  candidate
  (TrivialAggregateVoteSignature signatures)
    | length keys /= length signatures =
        Left $
          "Aggregate vote signature verification failed: "
            <> "number of keys and signatures do not match"
    | not (null errors) =
        Left $
          "Aggregate vote signature verification failed: "
            <> intercalate "; " errors
    | otherwise =
        Right ()
   where
    (errors, _) =
      partitionEithers $
        zipWith
          ( \key sig ->
              verifyVoteSignature key electionId candidate sig
          )
          (NonEmpty.toList keys)
          (NonEmpty.toList signatures)

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

  -- | Derive a VRF signing key from a voting committe private key
  getVRFSigningKey ::
    Proxy crypto ->
    PrivateKey crypto ->
    VRFSigningKey crypto

  -- | Derive a VRF verification key from a voting committe public key
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

-- | Crypto interface used for verifying aggregate VRF signatures
class
  ( Semigroup (AggregateVRFVerificationKey crypto)
  , Semigroup (AggregateVRFOutput crypto)
  ) =>
  CryptoSupportsAggregateVRF crypto
  where
  -- | Key used for verifying aggregate VRF outputs
  type AggregateVRFVerificationKey crypto :: Type

  -- | Aggregate cryptographic signature of a VRF output
  type AggregateVRFOutput crypto :: Type

  -- | Lift a single VRF output verification key into an aggregate one
  liftVRFVerificationKey ::
    Proxy crypto ->
    VRFVerificationKey crypto ->
    AggregateVRFVerificationKey crypto

  -- | Lift a single VRF output into an aggregate one
  liftVRFOutput ::
    Proxy crypto ->
    VRFOutput crypto ->
    AggregateVRFOutput crypto

  -- | Verify an aggregate vote signature for a given election and candidate
  verifyAggregateVRFOutput ::
    AggregateVRFVerificationKey crypto ->
    VRFElectionInput crypto ->
    AggregateVRFOutput crypto ->
    Either String ()

-- ** Trivial aggregate VRF verification helpers

newtype TrivialAggregateVRFVerificationKey crypto
  = TrivialAggregateVRFVerificationKey (NE [VRFVerificationKey crypto])
  deriving newtype Semigroup

newtype TrivialAggregateVRFOutput crypto
  = TrivialAggregateVRFOutput (NE [VRFOutput crypto])
  deriving newtype Semigroup

trivialLiftVRFVerificationKey ::
  Proxy crypto ->
  VRFVerificationKey crypto ->
  TrivialAggregateVRFVerificationKey crypto
trivialLiftVRFVerificationKey _ =
  TrivialAggregateVRFVerificationKey
    . NonEmpty.singleton

trivialLiftVRFOutput ::
  Proxy crypto ->
  VRFOutput crypto ->
  TrivialAggregateVRFOutput crypto
trivialLiftVRFOutput _ =
  TrivialAggregateVRFOutput
    . NonEmpty.singleton

trivialVerifyAggregateVRFOutput ::
  CryptoSupportsVRF crypto =>
  TrivialAggregateVRFVerificationKey crypto ->
  VRFElectionInput crypto ->
  TrivialAggregateVRFOutput crypto ->
  Either String ()
trivialVerifyAggregateVRFOutput
  (TrivialAggregateVRFVerificationKey keys)
  vrfInput
  (TrivialAggregateVRFOutput vrfOutputs)
    | length keys /= length vrfOutputs =
        Left $
          "Aggregate VRF output verification failed: "
            <> "number of keys and outputs do not match"
    | not (null errors) =
        Left $
          "Aggregate VRF output verification failed: "
            <> intercalate "; " errors
    | otherwise =
        Right ()
   where
    (errors, _) =
      partitionEithers $
        zipWith
          ( \key vrfOutput ->
              evalVRF (VRFVerifyContext key vrfOutput) vrfInput
          )
          (NonEmpty.toList keys)
          (NonEmpty.toList vrfOutputs)
