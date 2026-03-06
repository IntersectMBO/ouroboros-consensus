{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.Peras.Crypto
  ( -- * Crypto types for Peras
    PerasVoteCrypto
  , PerasKeyRole (..)
  , PerasKeyScope
  , PerasBLSPublicKey
  , PerasBLSPrivateKey
  , PerasSignature
  , PerasProofOfPossession
  , HasPerasBLSContext (..)
  , perasSignWithRole
  , perasVerifyWithRole
  , perasCreateProofOfPossession
  , perasVerifyProofOfPossession
  ) where

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
import Cardano.Ledger.Hashes (HASH, KeyHash (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import GHC.Exts (Any)
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , PerasRoundNo (..)
  , PerasVote (..)
  )
import Ouroboros.Consensus.Committee.Crypto
  ( CryptoSupportsSignature
  , CryptoSupportsVRF
  , ElectionId
  , VRFPoolContext (..)
  )
import qualified Ouroboros.Consensus.Committee.Crypto as CommitteeSelection
import Ouroboros.Consensus.Committee.Types (PoolId (..))
import Ouroboros.Consensus.Committee.WFALS
  ( CryptoSupportsWFALS (..)
  , VoteSupportsWFALS (..)
  )

-- * Generic Peras vote crypto types

-- | Key roles for Peras
data PerasKeyRole
  = -- | Key role for signing Peras votes
    SIGN
  | -- | Key role for local sortition in Peras elections
    VRF
  | -- | Key role for Proof of Possession
    POP

-- | Key scope for Peras, later instantiated with network id (e.g MAINNET)
type PerasKeyScope = ByteString

-- * BLS crypto interface

-- | Public key type for Peras, parameterized by key role
type PerasBLSPublicKey :: PerasKeyRole -> Type
data PerasBLSPublicKey r = PerasBLSPublicKey
  { unPerasBLSPublicKey :: VerKeyDSIGN BLS12381MinSigDSIGN
  , perasBLSPublicKeyScope :: PerasKeyScope
  }
  deriving (Eq, Show)

coercePerasBLSPublicKey ::
  forall r2 r1.
  PerasBLSPublicKey r1 ->
  PerasBLSPublicKey r2
coercePerasBLSPublicKey = coerce

-- | Private key type for Peras, parameterized by key role
type PerasBLSPrivateKey :: PerasKeyRole -> Type
data PerasBLSPrivateKey r = PerasBLSPrivateKey
  { unPerasBLSPrivateKey :: SignKeyDSIGN BLS12381MinSigDSIGN
  , perasBLSPrivateKeyScope :: PerasKeyScope
  }
  deriving (Eq, Show)

coercePerasBLSPrivateKey ::
  forall r2 r1.
  PerasBLSPrivateKey r1 ->
  PerasBLSPrivateKey r2
coercePerasBLSPrivateKey = coerce

-- | Signature type for Peras, parameterized by key role
type PerasSignature :: PerasKeyRole -> Type
newtype PerasSignature r = PerasSignature
  { unPerasSignature :: SigDSIGN BLS12381MinSigDSIGN
  }
  deriving (Eq, Show)

-- | Proof of Possession type for Peras
newtype PerasProofOfPossession = PerasProofOfPossession
  { unPerasProofOfPossession :: PossessionProofDSIGN BLS12381MinSigDSIGN
  }
  deriving (Eq, Show)

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
  PerasBLSPrivateKey r ->
  msg ->
  PerasSignature r
perasSignWithRole sk msg =
  PerasSignature
    { unPerasSignature =
        signDSIGN
          (blsCtx (Proxy @r) (perasBLSPrivateKeyScope sk))
          msg
          (unPerasBLSPrivateKey sk)
    }

-- | Verify a Peras signature on a message with a Peras public key
perasVerifyWithRole ::
  forall r msg.
  ( SignableRepresentation msg
  , HasPerasBLSContext r
  ) =>
  PerasBLSPublicKey r ->
  msg ->
  PerasSignature r ->
  Either String ()
perasVerifyWithRole pk msg (PerasSignature sig) =
  verifyDSIGN
    (blsCtx (Proxy @r) (perasBLSPublicKeyScope pk))
    (unPerasBLSPublicKey pk)
    msg
    sig

-- | Create a proof of possession signature for a Peras private key
perasCreateProofOfPossession ::
  PerasBLSPrivateKey POP ->
  PoolId ->
  PerasProofOfPossession
perasCreateProofOfPossession sk poolId =
  PerasProofOfPossession
    { unPerasProofOfPossession =
        createPossessionProofDSIGN
          extCtx
          (unPerasBLSPrivateKey sk)
    }
 where
  poolBytes = Hash.hashToBytes (unKeyHash (unPoolId poolId))
  baseCtx = blsCtx (Proxy @POP) (perasBLSPrivateKeyScope sk)
  extCtx = baseCtx{blsSignContextAug = blsSignContextAug baseCtx <> Just poolBytes}

-- | Verify a proof of possession signature for a Peras public key
perasVerifyProofOfPossession ::
  PerasBLSPublicKey POP ->
  PoolId ->
  PerasProofOfPossession ->
  Either String ()
perasVerifyProofOfPossession pk poolId pop =
  verifyPossessionProofDSIGN
    extCtx
    (unPerasBLSPublicKey pk)
    (unPerasProofOfPossession pop)
 where
  poolBytes = Hash.hashToBytes (unKeyHash (unPoolId poolId))
  baseCtx = blsCtx (Proxy @POP) (perasBLSPublicKeyScope pk)
  extCtx = baseCtx{blsSignContextAug = blsSignContextAug baseCtx <> Just poolBytes}

-- * Crypto types for votes

data PerasVoteCrypto blk

-- | Peras elections are identified by their round number
type instance ElectionId (PerasVoteCrypto blk) = PerasRoundNo

-- ** Vote crypto for Peras (degenerate instance for now)

instance CryptoSupportsSignature (PerasVoteCrypto blk) where
  type SignaturePrivateKey (PerasVoteCrypto blk) = PerasBLSPrivateKey SIGN
  type SignaturePublicKey (PerasVoteCrypto blk) = PerasBLSPublicKey SIGN

  newtype Payload (PerasVoteCrypto blk)
    = PerasVotePayload (Hash HASH (SigDSIGN BLS12381MinSigDSIGN))
    deriving (Eq, Show)

  newtype Signature (PerasVoteCrypto blk)
    = PerasVoteSignature (PerasSignature SIGN)
    deriving (Eq, Show)

  signPayload sk (PerasVotePayload msg) =
    PerasVoteSignature $
      perasSignWithRole @SIGN sk msg

  verifyPayloadSignature pk (PerasVotePayload msg) (PerasVoteSignature sig) =
    perasVerifyWithRole @SIGN pk msg sig

instance CryptoSupportsWFALS (PerasVoteCrypto blk) where
  type WFALSPrivateKey (PerasVoteCrypto blk) = PerasBLSPrivateKey Any
  type WFALSPublicKey (PerasVoteCrypto blk) = PerasBLSPublicKey Any

  getSignaturePublicKey _ = coerce
  getSignaturePrivateKey _ = coerce
  getVRFVerifyKey _ = coerce
  getVRFSigningKey _ = coerce

instance VoteSupportsWFALS (PerasVoteCrypto blk) (PerasVote blk) where
  getVoteView (PerasVote{}) k =
    k $ error "getWFALSVoteView: not yet implemented for PerasVote"

-- ** VRF crypto for Peras (degenerate instance for now)

instance CryptoSupportsVRF (PerasVoteCrypto blk) where
  type VRFVerifyKey (PerasVoteCrypto blk) = PerasBLSPublicKey VRF
  type VRFSigningKey (PerasVoteCrypto blk) = PerasBLSPrivateKey VRF

  newtype VRFElectionInput (PerasVoteCrypto blk)
    = PerasVoteVRFElectionInput (Hash HASH (SigDSIGN BLS12381MinSigDSIGN))
    deriving (Eq, Show)

  newtype VRFOutput (PerasVoteCrypto blk)
    = PerasVoteVRFOutput (PerasSignature VRF)
    deriving (Eq, Show)

  mkVRFElectionInput epochNonce roundNo =
    PerasVoteVRFElectionInput
      . Hash.castHash
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

  evalVRF context (PerasVoteVRFElectionInput input) =
    case context of
      VRFSignContext sk -> do
        let sig = perasSignWithRole @VRF (coercePerasBLSPrivateKey @VRF sk) input
        pure $ PerasVoteVRFOutput sig
      VRFVerifyContext pk (PerasVoteVRFOutput sig) -> do
        perasVerifyWithRole @VRF (coercePerasBLSPublicKey @VRF pk) input sig
        pure $ PerasVoteVRFOutput sig

  normalizeVRFOutput (PerasVoteVRFOutput sig) =
    toInteger vrfOutputNatural % vrfOutputMax
   where
    vrfOutputNatural =
      bytesToNatural $
        Hash.hashToBytes $
          Hash.hashWith @HASH rawSerialiseSigDSIGN $
            unPerasSignature sig
    vrfOutputMax =
      2 ^ ((8 :: Integer) * hashSize)
    hashSize =
      fromIntegral @Word (Hash.hashSize (Proxy :: Proxy HASH))
