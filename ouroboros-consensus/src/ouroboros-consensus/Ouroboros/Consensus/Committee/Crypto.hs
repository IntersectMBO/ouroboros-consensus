{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Crypto interface needed for committee selection
module Ouroboros.Consensus.Committee.Crypto
  ( -- * Types associated to committee selection
    ElectionId
  , VoteMessage
  , PrivateKeys
  , PublicKeys

    -- * Vote signature crypto interface
  , CryptoSupportsVoteSigning (..)
  , CryptoSupportsGroupVoteSigning (..)
  , CryptoSupportsNaiveGroupVoteSigning

    -- * VRF crypto interface
  , VRFPoolContext (..)
  , NormalizedVRFOutput (..)
  , CryptoSupportsVRF (..)
  , CryptoSupportsGroupVRF (..)
  , CryptoSupportsNaiveGroupVRF
  ) where

import Cardano.Ledger.BaseTypes (Nonce)
import Data.Either (partitionEithers)
import Data.Kind (Type)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy)

-- * Types associated to committee selection

-- | Election identifier used for committee seleciton
type family ElectionId crypto :: Type

type family VoteMessage crypto :: Type

type family PrivateKeys crypto

-- | Public key type for wFA^LS committee membership
type family PublicKeys crypto

-- * Signature crypto interface

-- | Crypto interface used for signing and verifying votes
class
  ( Eq (ElectionId crypto)
  , Show (ElectionId crypto)
  , Eq (VoteMessage crypto)
  , Show (VoteMessage crypto)
  , Eq (PrivateKeys crypto)
  , Show (PrivateKeys crypto)
  , Eq (PublicKeys crypto)
  , Show (PublicKeys crypto)
  , Eq (VoteSignaturePrivateKey crypto)
  , Show (VoteSignaturePrivateKey crypto)
  , Eq (VoteSignaturePublicKey crypto)
  , Show (VoteSignaturePublicKey crypto)
  , Eq (VoteSignature crypto)
  , Show (VoteSignature crypto)
  ) =>
  CryptoSupportsVoteSigning crypto
  where
  -- | Private key for signing
  type VoteSignaturePrivateKey crypto :: Type

  -- | Public key used for verification
  type VoteSignaturePublicKey crypto :: Type

  -- | Signature over an election identifier
  data VoteSignature crypto :: Type

  -- | Cast a committee public key into a vote signature public key
  getVoteSignaturePublicKey ::
    Proxy crypto ->
    PublicKeys crypto ->
    VoteSignaturePublicKey crypto

  -- | Cast a committee private key into a vote signature private key
  getVoteSignaturePrivateKey ::
    Proxy crypto ->
    PrivateKeys crypto ->
    VoteSignaturePrivateKey crypto

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

class
  ( Semigroup (GroupVoteSignaturePublicKey crypto)
  , Semigroup (GroupVoteSignature crypto)
  , Eq (GroupVoteSignaturePublicKey crypto)
  , Show (GroupVoteSignaturePublicKey crypto)
  , Eq (GroupVoteSignature crypto)
  , Show (GroupVoteSignature crypto)
  ) =>
  CryptoSupportsGroupVoteSigning crypto
  where
  type GroupVoteSignaturePublicKey crypto :: Type
  type GroupVoteSignature crypto :: Type

  liftVoteSignaturePublicKey ::
    Proxy crypto -> VoteSignaturePublicKey crypto -> GroupVoteSignaturePublicKey crypto
  liftVoteSignature :: Proxy crypto -> VoteSignature crypto -> GroupVoteSignature crypto

  verifyGroupVoteSignature ::
    Proxy crypto ->
    GroupVoteSignaturePublicKey crypto ->
    ElectionId crypto ->
    VoteMessage crypto ->
    GroupVoteSignature crypto ->
    Either String ()

class CryptoSupportsVoteSigning crypto => CryptoSupportsNaiveGroupVoteSigning crypto

instance CryptoSupportsNaiveGroupVoteSigning crypto => CryptoSupportsGroupVoteSigning crypto where
  type GroupVoteSignaturePublicKey crypto = NonEmpty (VoteSignaturePublicKey crypto)
  type GroupVoteSignature crypto = NonEmpty (VoteSignature crypto)

  liftVoteSignaturePublicKey _proxy pk = NE.singleton pk
  liftVoteSignature _proxy sig = NE.singleton sig

  verifyGroupVoteSignature _proxy groupPublicKey electionId message groupSignature
    | length groupPublicKey /= length groupSignature =
        Left "Group vote signature verification failed: mismatched number of public keys and signatures"
    | otherwise =
        let results =
              zipWith
                (\pk sig -> verifyVoteSignature pk electionId message sig)
                (NE.toList groupPublicKey)
                (NE.toList groupSignature)
            (errors, _) = partitionEithers results
         in case errors of
              [] -> Right ()
              _ -> Left $ "Group vote signature verification failed: " ++ intercalate "; " errors

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

deriving instance
  ( Eq (VRFSigningKey crypto)
  , Eq (VRFVerifyKey crypto)
  , Eq (VRFOutput crypto)
  ) =>
  Eq (VRFPoolContext crypto)

deriving instance
  ( Show (VRFSigningKey crypto)
  , Show (VRFVerifyKey crypto)
  , Show (VRFOutput crypto)
  ) =>
  Show (VRFPoolContext crypto)

-- | Normalized VRF outputs as a rational between 0 and 1
newtype NormalizedVRFOutput = NormalizedVRFOutput
  { unNormalizedVRFOutput :: Rational
  }
  deriving (Eq, Show)

-- | Crypto interface used to evaluate non-persistent voters via local sortition
class
  ( Eq (ElectionId crypto)
  , Show (ElectionId crypto)
  , Eq (PrivateKeys crypto)
  , Show (PrivateKeys crypto)
  , Eq (PublicKeys crypto)
  , Show (PublicKeys crypto)
  , Eq (VRFSigningKey crypto)
  , Show (VRFSigningKey crypto)
  , Eq (VRFVerifyKey crypto)
  , Show (VRFVerifyKey crypto)
  , Eq (VRFElectionInput crypto)
  , Show (VRFElectionInput crypto)
  , Eq (VRFOutput crypto)
  , Show (VRFOutput crypto)
  ) =>
  CryptoSupportsVRF crypto
  where
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

  -- | Cast a committee public key into a VRF verification key
  getVRFVerifyKey ::
    Proxy crypto ->
    PublicKeys crypto ->
    VRFVerifyKey crypto

  -- | Cast a committee private key into a VRF signing key
  getVRFSigningKey ::
    Proxy crypto ->
    PrivateKeys crypto ->
    VRFSigningKey crypto

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

class CryptoSupportsVRF crypto => CryptoSupportsNaiveGroupVRF crypto

class
  ( Semigroup (VRFGroupVerifyKey crypto)
  , Semigroup (VRFGroupOutput crypto)
  , Eq (VRFGroupVerifyKey crypto)
  , Show (VRFGroupVerifyKey crypto)
  , Eq (VRFGroupOutput crypto)
  , Show (VRFGroupOutput crypto)
  ) =>
  CryptoSupportsGroupVRF crypto
  where
  type VRFGroupVerifyKey crypto :: Type
  type VRFGroupOutput crypto :: Type

  liftVRFVerifyKey :: Proxy crypto -> VRFVerifyKey crypto -> VRFGroupVerifyKey crypto
  liftVRFOutput :: Proxy crypto -> VRFOutput crypto -> VRFGroupOutput crypto

  verifyGroupVRF ::
    Proxy crypto ->
    VRFGroupVerifyKey crypto ->
    VRFElectionInput crypto ->
    VRFGroupOutput crypto ->
    Either String ()

instance CryptoSupportsNaiveGroupVRF crypto => CryptoSupportsGroupVRF crypto where
  type VRFGroupVerifyKey crypto = NonEmpty (VRFVerifyKey crypto)
  type VRFGroupOutput crypto = NonEmpty (VRFOutput crypto)

  liftVRFVerifyKey _proxy vk = NE.singleton vk
  liftVRFOutput _proxy output = NE.singleton output

  verifyGroupVRF _proxy groupVerifyKey electionId groupOutput
    | length groupVerifyKey /= length groupOutput =
        Left "Group VRF verification failed: mismatched number of verify keys and outputs"
    | otherwise =
        -- Verify each individual VRF output against the corresponding verify key
        let results =
              zipWith
                (\vk output -> evalVRF (VRFVerifyContext vk output) electionId)
                (NE.toList groupVerifyKey)
                (NE.toList groupOutput)
            (errors, _) = partitionEithers results
         in case errors of
              [] -> Right ()
              _ -> Left $ "Group VRF verification failed: " ++ intercalate "; " errors
