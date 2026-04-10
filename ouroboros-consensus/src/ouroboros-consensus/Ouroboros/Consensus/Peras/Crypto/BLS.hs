{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | BLS-based crypto scheme used in Peras voting committees
module Ouroboros.Consensus.Peras.Crypto.BLS
  ( PerasBLSCrypto
  , ElectionId
  , VoteCandidate
  , PerasPrivateKey (..)
  , PerasPublicKey (..)
  , PerasBLSVoteSignature (..)
  , toPerasBLSVoteSignature
  , fromPerasBLSVoteSignature
  , PerasBLSCertSignature (..)
  , toPerasBLSCertSignature
  , fromPerasBLSCertSignature
  , VoteSignature (..)
  , VRFElectionInput (..)
  , VRFOutput (..)
  , AggregateVoteVerificationKey
  , AggregateVoteSignature
  , AggregateVRFVerificationKey
  , AggregateVRFOutput
  ) where

import Cardano.Binary (FromCBOR, ToCBOR (..))
import Cardano.Crypto.DSIGN (BLS12381MinSigDSIGN, DSIGNAlgorithm (..))
import Cardano.Crypto.Hash (Hash)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.BaseTypes (Nonce (..))
import Cardano.Ledger.Binary (runByteBuilder)
import Cardano.Ledger.Hashes (HASH)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import Data.Coerce (coerce)
import Data.Containers.NonEmpty (HasNonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Ouroboros.Consensus.Block.SupportsPeras
  ( PerasBoostedBlock (..)
  , PerasRoundNo (..)
  )
import Ouroboros.Consensus.Committee.Crypto
  ( CryptoSupportsAggregateVRF (..)
  , CryptoSupportsAggregateVoteSigning (..)
  , CryptoSupportsVRF (..)
  , CryptoSupportsVoteSigning (..)
  , ElectionId
  , PrivateKey
  , PublicKey
  , VRFPoolContext (..)
  , VoteCandidate
  )
import Ouroboros.Consensus.Committee.Crypto.BLS (KeyRole (..))
import qualified Ouroboros.Consensus.Committee.Crypto.BLS as BLS

-- | BLS-based crypto scheme used in Peras voting committees
data PerasBLSCrypto

type instance ElectionId PerasBLSCrypto = PerasRoundNo
type instance VoteCandidate PerasBLSCrypto = PerasBoostedBlock

-- | Private key of a Peras committee member
data PerasPrivateKey
  = PerasPrivateKey
  { perasVoteSignKey :: BLS.PrivateKey SIGN
  , perasVRFSignKey :: BLS.PrivateKey VRF
  }

type instance PrivateKey PerasBLSCrypto = PerasPrivateKey

-- | Public key of a Peras committee member
data PerasPublicKey
  = PerasPublicKey
  { perasVoteVerKey :: BLS.PublicKey SIGN
  , perasVRFVerKey :: BLS.PublicKey VRF
  }

type instance PublicKey PerasBLSCrypto = PerasPublicKey

-- | Hash the message of a Peras vote
--
-- NOTE: this is inspired by the implementation used by the Praos VRF check in
-- 'Ouroboros.Consensus.Protocol.Praos.VRF.mkInputVRF'.
hashVoteSignature ::
  ElectionId PerasBLSCrypto ->
  VoteCandidate PerasBLSCrypto ->
  Hash HASH (SigDSIGN BLS12381MinSigDSIGN)
hashVoteSignature roundNo boostedBlock =
  Hash.castHash
    . Hash.hashWith id
    . runByteBuilder (8 + 32)
    $ roundNoBytes <> boostedBlockBytes
 where
  roundNoBytes =
    BS.word64BE
      . unPerasRoundNo
      $ roundNo
  boostedBlockBytes =
    BS.byteStringCopy
      . Hash.hashToBytes
      . unPerasBoostedBlock
      $ boostedBlock

-- | Hash the input for the VRF used in Peras elections
--
-- NOTE: this is inspired by the implementation used by the Praos VRF check in
-- 'Ouroboros.Consensus.Protocol.Praos.VRF.mkInputVRF'.
hashVRFInput ::
  ElectionId PerasBLSCrypto ->
  Nonce ->
  Hash HASH (SigDSIGN BLS12381MinSigDSIGN)
hashVRFInput roundNo epochNonce =
  Hash.castHash
    . Hash.hashWith id
    . runByteBuilder (8 + 32)
    $ roundNoBytes <> epochNonceBytes
 where
  roundNoBytes =
    BS.word64BE (unPerasRoundNo roundNo)
  epochNonceBytes =
    case epochNonce of
      NeutralNonce -> mempty
      Nonce h -> BS.byteStringCopy (Hash.hashToBytes h)

-- * Signature types

-- | BLS signature on a Peras vote
newtype PerasBLSVoteSignature
  = PerasBLSVoteSignature
  { unPerasBLSVoteSignature :: BLS.Signature SIGN
  }
  deriving stock (Eq, Show)
  deriving newtype (FromCBOR, ToCBOR)

toPerasBLSVoteSignature ::
  VoteSignature PerasBLSCrypto ->
  PerasBLSVoteSignature
toPerasBLSVoteSignature = coerce

fromPerasBLSVoteSignature ::
  PerasBLSVoteSignature ->
  VoteSignature PerasBLSCrypto
fromPerasBLSVoteSignature = coerce

-- | Aggregate BLS signature on a PerasCert.
newtype PerasBLSCertSignature
  = PerasBLSCertSignature
  { unPerasBLSCertSignature :: BLS.Signature SIGN
  }
  deriving stock (Eq, Show)
  deriving newtype (FromCBOR, ToCBOR)

-- | Convert an aggregate vote signature into a certificate signature.
--
-- NOTE: since 'AggregateVoteSignature PerasBLSCrypto' can represent both a
-- collection of individual vote signatures (when forging a certificate locally)
-- and an already aggregated signature (when receiving a certificate forged by
-- someone else), this function will always first try to aggregate all the
-- signatures in the list, which should be a no-op when the list already
-- contains a single aggregated signature.
toPerasBLSCertSignature ::
  AggregateVoteSignature PerasBLSCrypto ->
  Either String PerasBLSCertSignature
toPerasBLSCertSignature sigs =
  fmap PerasBLSCertSignature
    . BLS.aggregateSignatures @SIGN
    . fmap unPerasBLSCryptoVoteSignature
    $ sigs

-- | Convert a certificate signature into an aggregate vote signature.
--
-- NOTE: this returns a collection of signatures with a single element.
fromPerasBLSCertSignature ::
  PerasBLSCertSignature ->
  AggregateVoteSignature PerasBLSCrypto
fromPerasBLSCertSignature (PerasBLSCertSignature sig) =
  NonEmpty.singleton $
    PerasBLSCryptoVoteSignature sig

-- * Crypto instances

instance CryptoSupportsVoteSigning PerasBLSCrypto where
  type VoteSigningKey PerasBLSCrypto = BLS.PrivateKey SIGN
  type VoteVerificationKey PerasBLSCrypto = BLS.PublicKey SIGN

  newtype VoteSignature PerasBLSCrypto
    = PerasBLSCryptoVoteSignature
    { unPerasBLSCryptoVoteSignature ::
        BLS.Signature BLS.SIGN
    }
    deriving stock (Eq, Show)
    deriving newtype (FromCBOR, ToCBOR)

  getVoteSigningKey _ =
    perasVoteSignKey
  getVoteVerificationKey _ =
    perasVoteVerKey

  signVote sk roundNo boostedBlock =
    PerasBLSCryptoVoteSignature
      . BLS.signWithRole @SIGN sk
      $ hashVoteSignature roundNo boostedBlock

  verifyVoteSignature
    pk
    roundNo
    boostedBlock
    (PerasBLSCryptoVoteSignature sig) =
      BLS.verifyWithRole @SIGN
        pk
        (hashVoteSignature roundNo boostedBlock)
        sig

instance CryptoSupportsVRF PerasBLSCrypto where
  type VRFSigningKey PerasBLSCrypto = BLS.PrivateKey VRF
  type VRFVerificationKey PerasBLSCrypto = BLS.PublicKey VRF

  newtype VRFElectionInput PerasBLSCrypto
    = PerasBLSCryptoVRFElectionInput
    { unPerasBLSCryptoVRFElectionInput ::
        Hash HASH (SigDSIGN BLS12381MinSigDSIGN)
    }
    deriving stock (Eq, Show)

  newtype VRFOutput PerasBLSCrypto
    = PerasBLSCryptoVRFOutput
    { unPerasBLSCryptoVRFOutput ::
        BLS.Signature VRF
    }
    deriving stock (Eq, Show)
    deriving newtype (FromCBOR, ToCBOR)

  getVRFSigningKey _ =
    perasVRFSignKey

  getVRFVerificationKey _ =
    perasVRFVerKey

  mkVRFElectionInput roundNo epochNonce =
    PerasBLSCryptoVRFElectionInput $
      hashVRFInput epochNonce roundNo

  evalVRF context (PerasBLSCryptoVRFElectionInput input) =
    case context of
      VRFSignContext sk -> do
        let sig = BLS.signWithRole @VRF (BLS.coercePrivateKey @VRF sk) input
        pure $ PerasBLSCryptoVRFOutput sig
      VRFVerifyContext pk (PerasBLSCryptoVRFOutput sig) -> do
        BLS.verifyWithRole @VRF (BLS.coercePublicKey @VRF pk) input sig
        pure $ PerasBLSCryptoVRFOutput sig

  normalizeVRFOutput (PerasBLSCryptoVRFOutput sig) =
    BLS.toNormalizedVRFOutput sig

-- * Support for aggregate signatures and VRF outputs

instance CryptoSupportsAggregateVoteSigning PerasBLSCrypto where
  -- NOTE: for now, we individually collect the verification keys of all the
  -- voters who contributed to the certificate being verified.
  type
    AggregateVoteVerificationKey PerasBLSCrypto =
      NE [VoteVerificationKey PerasBLSCrypto]

  -- IMPORTANT NOTE: this non-empty list could have two shapes depending on
  -- the context in which it is used:
  --
  --   * When forging a certificate locally, this list will contain a collection
  --     of individual vote signatures from all the votes reaching a quorum we
  --     have received. These are then aggregated into a single one right before
  --     sending the certificate over the network (see
  --     'toPerasBLSCertSignature').
  --
  --   * When receiving a certificate someone else forged over the network, all
  --     the signatures from the votes that contributed to the certificate will
  --     already be aggregated into a single one by the (original) sender, so
  --     this list will contain just one element (see
  --     'fromPerasBLSCertSignature').
  --
  -- Then, when verifying an aggregate vote signature, we don't need to care
  -- about the shape of the list or the context in which it is used, since in
  -- both cases we start by aggregating all the signatures in the list (which
  -- is an identity operation when the list has just one element), and then we
  -- verify the resulting aggregate signature against the aggregate
  -- verification key.
  --
  -- This is notably in contrast to aggregate VRF outputs: since we need to
  -- derive individual non-persistent seat numbers from each VRF output provided
  -- by each non-persistent voter, we cannot aggregate them into a single one
  -- when forging a certificate. However, when verifying a certificate, we can
  -- still verify all the individual VRF outputs in a single batch by first
  -- aggregating them locally.
  type
    AggregateVoteSignature PerasBLSCrypto =
      NE [VoteSignature PerasBLSCrypto]

  liftVoteVerificationKey _ =
    NonEmpty.singleton

  liftVoteSignature _ =
    NonEmpty.singleton

  verifyAggregateVoteSignature
    _
    pks
    roundNo
    boostedBlock
    sigs = do
      aggPk <-
        BLS.aggregatePublicKeys @SIGN pks
      aggSig <-
        BLS.aggregateSignatures @SIGN
          . fmap unPerasBLSCryptoVoteSignature
          $ sigs
      BLS.verifyWithRole @SIGN
        aggPk
        (hashVoteSignature roundNo boostedBlock)
        aggSig

instance CryptoSupportsAggregateVRF PerasBLSCrypto where
  -- NOTE: for now, we individually collect the verification keys of all the
  -- voters who contributed to the certificate being verified.
  type
    AggregateVRFVerificationKey PerasBLSCrypto =
      NE [VRFVerificationKey PerasBLSCrypto]

  -- NOTE: in contrast to vote signatures, we cannot aggregate multiple VRF
  -- outputs into a single one when forging a certificate (because we need to
  -- derive non-persistent seat numbers from each individual one). This means
  -- that, at verification time, this list will always contain one VRF output
  -- per non-persistent voter in the certificate, even when verifying a
  -- certificate forged by someone else that we received over the network.
  --
  -- However, we still want to verify all the VRF outputs in a single batch for
  -- efficiency reasons, and we can do that by first aggregating all the VRF
  -- outputs in the list locally (using linearization to avoid swap-attacks),
  -- and then verifying the resulting aggregate VRF output against the aggregate
  -- VRF verification key.
  type
    AggregateVRFOutput PerasBLSCrypto =
      NE [VRFOutput PerasBLSCrypto]

  liftVRFVerificationKey _ =
    NonEmpty.singleton

  liftVRFOutput _ =
    NonEmpty.singleton

  verifyAggregateVRFOutput
    pks
    (PerasBLSCryptoVRFElectionInput input)
    sigs = do
      BLS.linearizeAndVerifyVRFs pks input
        . fmap unPerasBLSCryptoVRFOutput
        $ sigs
