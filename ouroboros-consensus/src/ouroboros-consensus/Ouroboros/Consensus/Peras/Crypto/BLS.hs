{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
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

import Cardano.Binary
  ( FromCBOR (..)
  , ToCBOR (..)
  , decodeListLenOf
  , encodeListLen
  )
import Cardano.Crypto.DSIGN (BLS12381MinSigDSIGN, DSIGNAlgorithm (..))
import Cardano.Crypto.Hash (Hash)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.BaseTypes (Nonce (..), SlotNo (..))
import Cardano.Ledger.Binary (runByteBuilder)
import Cardano.Ledger.Hashes (HASH)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Short as BS
import GHC.Base (Any)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract (WithOrigin (..))
import Ouroboros.Consensus.Block.RealPoint
  ( bytes32RealPointHash
  , bytes32RealPointSlot
  )
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
  deriving (Show, Eq, Generic, NoThunks)

type instance ElectionId PerasBLSCrypto = PerasRoundNo
type instance VoteCandidate PerasBLSCrypto = PerasBoostedBlock

-- | Private key of a Peras committee member
newtype PerasPrivateKey
  = PerasPrivateKey (BLS.PrivateKey Any)
  deriving stock Generic
  deriving anyclass NoThunks

type instance PrivateKey PerasBLSCrypto = PerasPrivateKey

-- | Public key of a Peras committee member
data PerasPublicKey
  = PerasPublicKey (BLS.PublicKey Any)
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

type instance PublicKey PerasBLSCrypto = PerasPublicKey

-- NOTE: we include the key scope in the serialised format of the public key
instance FromCBOR PerasPublicKey where
  fromCBOR = do
    decodeListLenOf 2
    keyScope <- fromCBOR
    keyBytes <- fromCBOR
    case BLS.rawDeserialisePublicKey keyScope keyBytes of
      Just pk ->
        pure (PerasPublicKey pk)
      Nothing ->
        fail
          ( "Failed to decode PerasPublicKey, invalid public key bytes: "
              <> show keyBytes
              <> " with scope: "
              <> show keyScope
          )

instance ToCBOR PerasPublicKey where
  toCBOR (PerasPublicKey pk) = do
    encodeListLen 2
      <> toCBOR (BLS.publicKeyScope pk)
      <> toCBOR (BLS.rawSerialisePublicKey pk)

-- | Hash the message of a Peras vote
--
-- NOTE: this is inspired by the implementation used by the Praos VRF check in
-- 'Cardano.Protocol.Praos.VRF.mkInputVRF'.
hashVoteSignature ::
  ElectionId PerasBLSCrypto ->
  VoteCandidate PerasBLSCrypto ->
  Hash HASH (SigDSIGN BLS12381MinSigDSIGN)
hashVoteSignature roundNo boostedBlock =
  Hash.castHash
    . Hash.hashWith id
    . runByteBuilder (8 + 8 + 32)
    $ roundNoBytes <> boostedBlockBytes
 where
  roundNoBytes =
    BS.word64BE
      . unPerasRoundNo
      $ roundNo
  boostedBlockBytes =
    case unPerasBoostedBlock boostedBlock of
      Origin ->
        mempty
      NotOrigin point ->
        bytes32RealPointSlotBytes point
          <> bytes32RealPointHashBytes point

  bytes32RealPointSlotBytes =
    BS.word64BE
      . unSlotNo
      . bytes32RealPointSlot
  bytes32RealPointHashBytes =
    BS.byteStringCopy
      . BS.fromShort
      . bytes32RealPointHash

-- | Hash the input for the VRF used in Peras elections
--
-- NOTE: this is inspired by the implementation used by the Praos VRF check in
-- 'Cardano.Protocol.Praos.VRF.mkInputVRF'.
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
    deriving stock (Show, Eq, Generic)
    deriving newtype (FromCBOR, ToCBOR)
    deriving anyclass NoThunks

  getVoteSigningKey _ (PerasPrivateKey sk) =
    BLS.coercePrivateKey @SIGN sk
  getVoteVerificationKey _ (PerasPublicKey pk) =
    BLS.coercePublicKey @SIGN pk

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
    deriving stock (Show, Eq, Generic)
    deriving newtype (FromCBOR, ToCBOR)
    deriving anyclass NoThunks

  getVRFSigningKey _ (PerasPrivateKey sk) =
    BLS.coercePrivateKey @VRF sk

  getVRFVerificationKey _ (PerasPublicKey pk) =
    BLS.coercePublicKey @VRF pk

  mkVRFElectionInput epochNonce roundNo =
    PerasBLSCryptoVRFElectionInput $
      hashVRFInput roundNo epochNonce

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
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromCBOR, ToCBOR)
  deriving anyclass NoThunks

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
