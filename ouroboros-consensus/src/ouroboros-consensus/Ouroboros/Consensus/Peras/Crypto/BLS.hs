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
  , VoteSignature (..)
  , VRFElectionInput (..)
  , VRFOutput (..)
  , AggregateVoteVerificationKey
  , AggregateVoteSignature

    -- * For testing purposes
  , PerasBLSCryptoAggregateVoteVerificationKey (..)
  , PerasBLSCryptoAggregateVoteSignature (..)
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
import Ouroboros.Consensus.Block.SupportsPeras
  ( PerasBoostedBlock (..)
  , PerasRoundNo (..)
  )
import Ouroboros.Consensus.Committee.Crypto
  ( CryptoSupportsAggregateVoteSigning (..)
  , CryptoSupportsBatchVRFVerification (..)
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

-- | Wrapper around the aggregate vote signatures.
newtype PerasBLSCryptoAggregateVoteVerificationKey
  = PerasBLSCryptoAggregateVoteVerificationKey
  { unPerasBLSCryptoAggregateVoteVerificationKey ::
      BLS.PublicKey SIGN
  }
  deriving stock (Eq, Show)

-- | Wrapper around the aggregate vote verification keys.
newtype PerasBLSCryptoAggregateVoteSignature
  = PerasBLSCryptoAggregateVoteSignature
  { unPerasBLSCryptoAggregateVoteSignature ::
      BLS.Signature SIGN
  }
  deriving stock (Eq, Show)
  deriving newtype (FromCBOR, ToCBOR)

instance CryptoSupportsAggregateVoteSigning PerasBLSCrypto where
  type
    AggregateVoteVerificationKey PerasBLSCrypto =
      PerasBLSCryptoAggregateVoteVerificationKey
  type
    AggregateVoteSignature PerasBLSCrypto =
      PerasBLSCryptoAggregateVoteSignature

  aggregateVoteVerificationKeys _ pks = do
    aggPk <- BLS.aggregatePublicKeys @SIGN pks
    pure (PerasBLSCryptoAggregateVoteVerificationKey aggPk)

  aggregateVoteSignatures _ sigs = do
    aggSig <-
      BLS.aggregateSignatures @SIGN
        . fmap unPerasBLSCryptoVoteSignature
        $ sigs
    pure (PerasBLSCryptoAggregateVoteSignature aggSig)

  verifyAggregateVoteSignature
    _
    aggPk
    roundNo
    boostedBlock
    aggSig = do
      BLS.verifyWithRole @SIGN
        (unPerasBLSCryptoAggregateVoteVerificationKey aggPk)
        (hashVoteSignature roundNo boostedBlock)
        (unPerasBLSCryptoAggregateVoteSignature aggSig)

instance CryptoSupportsBatchVRFVerification PerasBLSCrypto where
  -- NOTE: in contrast to vote signatures, we cannot aggregate multiple VRF
  -- outputs into a single one when forging a certificate (because we need to
  -- derive non-persistent seat numbers from each individual one). This means
  -- that, at verification time, @sigs@ will always contain one VRF output per
  -- non-persistent voter in the certificate, even when verifying a certificate
  -- forged by someone else that we received over the network.
  --
  -- However, we still want to verify all the VRF outputs in a single batch for
  -- efficiency reasons, and we can do that by first aggregating all the VRF
  -- outputs in the list locally (using linearization to avoid swap-attacks),
  -- and then verifying the resulting aggregate VRF output against the aggregate
  -- VRF verification key.
  batchVerifyVRFOutputs
    pks
    (PerasBLSCryptoVRFElectionInput input)
    sigs = do
      BLS.linearizeAndVerifyVRFs
        pks
        input
        . fmap unPerasBLSCryptoVRFOutput
        $ sigs
