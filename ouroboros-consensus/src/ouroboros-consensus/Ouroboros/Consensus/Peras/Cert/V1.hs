{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Concrete Peras certificate types using BLS signatures.
--
-- NOTE: this module is meant to be imported qualified.
--
-- NOTE: the validation performed during serialization is minimal, and does not
-- cover any of additional semantic and cryptographic checks that must be
-- performed on the certificate later on.
module Ouroboros.Consensus.Peras.Cert.V1
  ( PerasCert (..)
  , PerasCertVoters (..)
  ) where

import Cardano.Binary
  ( FromCBOR (..)
  , ToCBOR (..)
  , decodeListLenOf
  , encodeListLen
  )
import Control.Monad (when)
import Control.Monad.Error.Class (MonadError (..))
import Data.Containers.NonEmpty (HasNonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.NonEmpty as NEMap
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Word (Word16)
import Ouroboros.Consensus.Block.SupportsPeras
  ( PerasBoostedBlock
  , PerasRoundNo
  , PerasSeatIndex (..)
  )
import Ouroboros.Consensus.Committee.Crypto
  ( Aggregate (..)
  , CryptoSupportsVoteSigning (..)
  )
import Ouroboros.Consensus.Peras.Crypto.BLS
  ( PerasBLSCrypto
  , VRFOutput
  )
import Ouroboros.Consensus.Peras.Vote.V1 (PerasVoteEligibilityProof (..))
import Ouroboros.Consensus.Util.Bitmap (Bitmap)
import qualified Ouroboros.Consensus.Util.Bitmap as Bitmap

-- | Concrete Peras certificates using BLS signatures
data PerasCert
  = PerasCert
  { pcRoundNo :: !PerasRoundNo
  -- ^ Election identifier
  , pcBoostedBlock :: !PerasBoostedBlock
  -- ^ Certificate message, i.e., the hash of the block being boosted
  , pcVoters :: !PerasCertVoters
  -- ^ Voters who contributed to this certificate
  , pcSignature :: !(Aggregate (VoteSignature PerasBLSCrypto))
  -- ^ Aggregate BLS signature on the hash of the election identifier and
  -- the certificate message
  }
  deriving (Show, Eq)

instance FromCBOR PerasCert where
  fromCBOR = do
    decodeListLenOf 4
    pcRoundNo <- fromCBOR
    pcBoostedBlock <- fromCBOR
    pcVoters <- fromCBOR
    pcSignature <- Aggregate <$> fromCBOR
    pure
      PerasCert
        { pcRoundNo
        , pcBoostedBlock
        , pcVoters
        , pcSignature
        }

instance ToCBOR PerasCert where
  toCBOR cert =
    encodeListLen 4
      <> toCBOR (pcRoundNo cert)
      <> toCBOR (pcBoostedBlock cert)
      <> toCBOR (pcVoters cert)
      <> toCBOR (unAggregate (pcSignature cert))

-- | Voters contained in a certificate with their appropriate eligibility proof
newtype PerasCertVoters
  = PerasCertVoters
  { unPerasCertVoters ::
      NE (Map PerasSeatIndex PerasVoteEligibilityProof)
  }
  deriving (Eq, Show)

instance FromCBOR PerasCertVoters where
  fromCBOR = do
    decodeListLenOf 2
    votersBitmap <- fromCBOR
    nonPersistentSigs <- fromCBOR

    either (\err -> fail ("Invalid Peras certificate: " <> err)) pure
      . fromCompactRepr
      $ CompactPerasCertVoters
        { votersBitmap
        , nonPersistentSigs
        }

instance ToCBOR PerasCertVoters where
  toCBOR voters =
    encodeListLen 2
      <> toCBOR votersBitmap
      <> toCBOR nonPersistentSigs
   where
    CompactPerasCertVoters
      { votersBitmap
      , nonPersistentSigs
      } =
        toCompactRepr voters

-- | Compact representation of the voters in a Peras certificate.
--
-- This compact representation consists of a bitmap of voter seat indices and a
-- list of non-persistent eligibility proofs (VRF outputs). In this setup, the
-- last @np@ indices in the bitmap that are flipped to 1 correspond to
-- non-persistent voters, where @np@ is the length of the list of non-persistent
-- eligibility proofs. The remaining flipped indices in the bitmap correspond
-- to persistent voters.
--
-- @
--   fromCompactRepr
--      CompactPerasCertVoters {
--        votersBitmap = <01101011>,
--        nonPersistentSigs = [np1, np2, np3]
--      }
--   ==
--   PerasCertVoters {
--     1 => persistent
--     2 => persistent
--     4 => non-persistent(np1)
--     6 => non-persistent(np2)
--     7 => non-persistent(np3)
--   }
-- @
data CompactPerasCertVoters
  = CompactPerasCertVoters
  { votersBitmap :: !(Bitmap Word16)
  , nonPersistentSigs :: !([VRFOutput PerasBLSCrypto])
  }
  deriving (Eq, Show)

-- | Decode 'PerasCertVoters' from their compact representation.
--
-- See 'CompactPerasCertVoters' for the encoding scheme used here.
fromCompactRepr ::
  CompactPerasCertVoters ->
  Either String PerasCertVoters
fromCompactRepr
  CompactPerasCertVoters
    { votersBitmap
    , nonPersistentSigs
    } = do
    let voterSeatIndices =
          PerasSeatIndex <$> Bitmap.toIndices votersBitmap

    when (null voterSeatIndices) $
      throwError "Invalid Peras certificate: empty voters bitmap"

    when (length nonPersistentSigs > length voterSeatIndices) $
      throwError $
        unlines
          [ "Invalid Peras certificate:"
              <> " more non-persistent voter eligibility proofs where provided"
              <> " than the number of voters in the certificate"
          , " * number of voters: "
              <> show (length voterSeatIndices)
          , " * number of proofs: "
              <> show (length nonPersistentSigs)
          ]

    let numPersistentVoters =
          length voterSeatIndices - length nonPersistentSigs
    let persistentProofs =
          take numPersistentVoters (repeat PersistentPerasVoteEligibilityProof)
    let nonPersistentProofs =
          fmap NonPersistentPerasVoteEligibilityProof nonPersistentSigs
    let voters =
          NEMap.fromAscList
            . NonEmpty.fromList
            . zip voterSeatIndices
            $ persistentProofs <> nonPersistentProofs

    pure (PerasCertVoters voters)

-- | Encode 'PerasCertVoters' into their compact representation.
--
-- See 'CompactPerasCertVoters' for the encoding scheme used here.
toCompactRepr ::
  PerasCertVoters ->
  CompactPerasCertVoters
toCompactRepr (PerasCertVoters voters) =
  CompactPerasCertVoters
    { votersBitmap
    , nonPersistentSigs
    }
 where
  logicalUpperBound =
    unPerasSeatIndex (fst (NEMap.findMax voters))
  votersByAscSeatIndex =
    NonEmpty.toList (NEMap.toAscList voters)
  votersSeatIndices =
    fmap (unPerasSeatIndex . fst) votersByAscSeatIndex
  votersBitmap =
    Bitmap.fromIndices logicalUpperBound votersSeatIndices
  nonPersistentSigs =
    catMaybes (fmap getNonPersistentSig votersByAscSeatIndex)
  getNonPersistentSig = \case
    (_, PersistentPerasVoteEligibilityProof) -> Nothing
    (_, NonPersistentPerasVoteEligibilityProof p) -> Just p
