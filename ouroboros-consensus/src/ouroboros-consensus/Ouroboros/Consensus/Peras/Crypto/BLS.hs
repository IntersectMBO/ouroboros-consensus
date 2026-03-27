{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

-- | BLS crypto interface for Peras.
--
-- NOTE: this module is meant to be imported qualified in the presence of
-- multiple crypto schemes.
module Ouroboros.Consensus.Peras.Crypto.BLS
  ( -- * BLS crypto interface for Peras
    PerasBLSCrypto
  , PerasKeyRole (..)
  , PerasKeyScope
  , PerasPublicKey
  , rawDeserialisePerasPublicKey
  , rawSerialisePerasPublicKey
  , coercePerasPublicKey
  , PerasPrivateKey
  , rawDeserialisePerasPrivateKey
  , rawSerialisePerasPrivateKey
  , coercePerasPrivateKey
  , PerasSignature
  , PerasProofOfPossession
  , HasPerasBLSContext (..)
  , perasSignWithRole
  , perasVerifyWithRole
  , perasCreateProofOfPossession
  , perasVerifyProofOfPossession

    -- * Hashing for Peras signatures
  , hashVoteSignature
  , hashVRFInput

    -- * VRF signature manipulation
  , perasSignatureNatural
  , perasSignatureNaturalMax
  ) where

import Cardano.Binary (FromCBOR, ToCBOR)
import Cardano.Crypto.DSIGN
  ( BLS12381MinSigDSIGN
  , BLS12381SignContext (..)
  , DSIGNAggregatable (..)
  , DSIGNAlgorithm (..)
  )
import Cardano.Crypto.Hash (Hash)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Crypto.Util (SignableRepresentation, bytesToNatural)
import Cardano.Ledger.BaseTypes (Nonce (..))
import Cardano.Ledger.Binary (runByteBuilder)
import Cardano.Ledger.Hashes (HASH, KeyHash (..), KeyRole (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.Natural (Natural)
import Ouroboros.Consensus.Block (HeaderHash)
import Ouroboros.Consensus.Block.Abstract
  ( ConvertRawHash
  , toRawHash
  )
import Ouroboros.Consensus.Block.SupportsPeras (PerasRoundNo (..))

-- * BLS crypto interface for Peras

data PerasBLSCrypto blk

-- | Key roles for Peras
type data PerasKeyRole
  = -- | Key role for signing Peras votes
    SIGN
  | -- | Key role for local sortition in Peras elections
    VRF
  | -- | Key role for Proof of Possession
    POP

-- | Key scope for Peras, later instantiated with network id (e.g MAINNET)
type PerasKeyScope = ByteString

-- | Public key type for Peras, parameterized by key role
type PerasPublicKey :: PerasKeyRole -> Type
data PerasPublicKey r = PerasPublicKey
  { unPerasPublicKey :: !(VerKeyDSIGN BLS12381MinSigDSIGN)
  , perasPublicKeyScope :: !(PerasKeyScope)
  }
  deriving stock (Eq, Show)

rawDeserialisePerasPublicKey ::
  PerasKeyScope ->
  ByteString ->
  Maybe (PerasPublicKey r)
rawDeserialisePerasPublicKey scope bs = do
  key <- rawDeserialiseVerKeyDSIGN bs
  pure $
    PerasPublicKey
      { unPerasPublicKey = key
      , perasPublicKeyScope = scope
      }

rawSerialisePerasPublicKey ::
  PerasPublicKey r ->
  ByteString
rawSerialisePerasPublicKey =
  rawSerialiseVerKeyDSIGN . unPerasPublicKey

coercePerasPublicKey ::
  forall r2 r1.
  PerasPublicKey r1 ->
  PerasPublicKey r2
coercePerasPublicKey = coerce

-- | Private key type for Peras, parameterized by key role
type PerasPrivateKey :: PerasKeyRole -> Type
data PerasPrivateKey r = PerasPrivateKey
  { unPerasPrivateKey :: !(SignKeyDSIGN BLS12381MinSigDSIGN)
  , perasPrivateKeyScope :: !PerasKeyScope
  }
  deriving stock (Eq, Show)

rawDeserialisePerasPrivateKey ::
  PerasKeyScope ->
  ByteString ->
  Maybe (PerasPrivateKey r)
rawDeserialisePerasPrivateKey scope bs = do
  key <- rawDeserialiseSignKeyDSIGN bs
  pure $
    PerasPrivateKey
      { unPerasPrivateKey = key
      , perasPrivateKeyScope = scope
      }

rawSerialisePerasPrivateKey ::
  PerasPrivateKey r ->
  ByteString
rawSerialisePerasPrivateKey =
  rawSerialiseSignKeyDSIGN . unPerasPrivateKey

coercePerasPrivateKey ::
  forall r2 r1.
  PerasPrivateKey r1 ->
  PerasPrivateKey r2
coercePerasPrivateKey = coerce

-- | Signature type for Peras, parameterized by key role
type PerasSignature :: PerasKeyRole -> Type
newtype PerasSignature r = PerasSignature
  { unPerasSignature :: SigDSIGN BLS12381MinSigDSIGN
  }
  deriving stock (Eq, Show)
  deriving newtype (FromCBOR, ToCBOR)

-- | Proof of Possession type for Peras
newtype PerasProofOfPossession = PerasProofOfPossession
  { unPerasProofOfPossession :: PossessionProofDSIGN BLS12381MinSigDSIGN
  }
  deriving stock (Eq, Show)
  deriving newtype (FromCBOR, ToCBOR)

-- TODO: get these contexts directly from @cardano-base@ after
-- https://github.com/IntersectMBO/cardano-base/pull/635
-- is merged.

-- Basic over G1:
-- https://www.ietf.org/archive/id/draft-irtf-cfrg-bls-signature-06.html#section-4.2.1-1
minSigSignatureDST :: BLS12381SignContext
minSigSignatureDST =
  BLS12381SignContext
    { blsSignContextDst = Just "BLS_SIG_BLS12381G1_XMD:SHA-256_SSWU_RO_NUL_"
    , blsSignContextAug = Nothing
    }

-- PoP over G1:
-- https://www.ietf.org/archive/id/draft-irtf-cfrg-bls-signature-06.html#section-4.2.3-1
minSigPoPDST :: BLS12381SignContext
minSigPoPDST =
  BLS12381SignContext
    { blsSignContextDst = Just "BLS_SIG_BLS12381G1_XMD:SHA-256_SSWU_RO_POP_"
    , blsSignContextAug = Nothing
    }

-- | Role-separated BLS contexts for Peras signatures
class HasPerasBLSContext (r :: PerasKeyRole) where
  blsCtx :: Proxy r -> PerasKeyScope -> BLS12381SignContext

instance HasPerasBLSContext SIGN where
  blsCtx _ keyScope =
    minSigSignatureDST
      { blsSignContextAug =
          Just ("PERAS:VOTE:" <> keyScope <> ":V0")
      }

instance HasPerasBLSContext VRF where
  blsCtx _ keyScope =
    minSigSignatureDST
      { blsSignContextAug =
          Just ("PERAS:VRF:" <> keyScope <> ":V0")
      }

instance HasPerasBLSContext POP where
  blsCtx _ keyScope =
    minSigPoPDST
      { blsSignContextAug =
          Just ("PERAS:POP:" <> keyScope <> ":V0")
      }

-- | Sign a message with a Peras private key, producing a Peras signature
perasSignWithRole ::
  forall r msg.
  ( SignableRepresentation msg
  , HasPerasBLSContext r
  ) =>
  PerasPrivateKey r ->
  msg ->
  PerasSignature r
perasSignWithRole sk msg =
  PerasSignature
    { unPerasSignature =
        signDSIGN
          (blsCtx (Proxy @r) (perasPrivateKeyScope sk))
          msg
          (unPerasPrivateKey sk)
    }

-- | Verify a Peras signature on a message with a Peras public key
perasVerifyWithRole ::
  forall r msg.
  ( SignableRepresentation msg
  , HasPerasBLSContext r
  ) =>
  PerasPublicKey r ->
  msg ->
  PerasSignature r ->
  Either String ()
perasVerifyWithRole pk msg (PerasSignature sig) =
  verifyDSIGN
    (blsCtx (Proxy @r) (perasPublicKeyScope pk))
    (unPerasPublicKey pk)
    msg
    sig

-- | Create a proof of possession signature for a Peras private key
perasCreateProofOfPossession ::
  PerasPrivateKey POP ->
  KeyHash StakePool ->
  PerasProofOfPossession
perasCreateProofOfPossession sk stakePoolHash =
  PerasProofOfPossession
    { unPerasProofOfPossession =
        createPossessionProofDSIGN
          extCtx
          (unPerasPrivateKey sk)
    }
 where
  poolBytes = Hash.hashToBytes (unKeyHash stakePoolHash)
  baseCtx = blsCtx (Proxy @POP) (perasPrivateKeyScope sk)
  extCtx = baseCtx{blsSignContextAug = blsSignContextAug baseCtx <> Just poolBytes}

-- | Verify a proof of possession signature for a Peras public key
perasVerifyProofOfPossession ::
  PerasPublicKey POP ->
  KeyHash StakePool ->
  PerasProofOfPossession ->
  Either String ()
perasVerifyProofOfPossession pk stakePoolHash pop =
  verifyPossessionProofDSIGN
    extCtx
    (unPerasPublicKey pk)
    (unPerasProofOfPossession pop)
 where
  poolBytes = Hash.hashToBytes (unKeyHash stakePoolHash)
  baseCtx = blsCtx (Proxy @POP) (perasPublicKeyScope pk)
  extCtx = baseCtx{blsSignContextAug = blsSignContextAug baseCtx <> Just poolBytes}

-- * Hashing for Peras signatures

-- | Hash the message of a Peras vote
hashVoteSignature ::
  ConvertRawHash blk =>
  Proxy blk ->
  PerasRoundNo ->
  HeaderHash blk ->
  Hash HASH (SigDSIGN BLS12381MinSigDSIGN)
hashVoteSignature p roundNo boostedBlock =
  Hash.castHash
    . Hash.hashWith id
    . runByteBuilder (8 + 32)
    $ roundNoBytes <> pointBytes
 where
  roundNoBytes =
    BS.word64BE (unPerasRoundNo roundNo)
  pointBytes =
    BS.byteStringCopy (toRawHash p boostedBlock)

-- | Hash the input for the VRF used in Peras elections
hashVRFInput ::
  PerasRoundNo ->
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

-- * VRF signature manipulation

-- | Convert a Peras VRF signature to a natural number for use in local sortition
perasSignatureNatural ::
  PerasSignature VRF ->
  Natural
perasSignatureNatural sig =
  bytesToNatural
    . Hash.hashToBytes
    . Hash.hashWith @HASH rawSerialiseSigDSIGN
    . unPerasSignature
    $ sig

-- | The maximum natural number that can be produced by a Peras VRF signature
perasSignatureNaturalMax :: Natural
perasSignatureNaturalMax =
  2 ^ ((8 :: Integer) * hashSize) - 1
 where
  hashSize =
    fromIntegral (Hash.hashSize (Proxy :: Proxy HASH))
