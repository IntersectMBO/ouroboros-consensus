{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Word (Word16)
import Ouroboros.Consensus.Block
  ( ConvertRawHash (..)
  , Point
  , decodeRawHash
  , encodeRawHash
  )
import Ouroboros.Consensus.Block.SupportsPeras
  ( PerasRoundNo
  , PerasSeatIndex (..)
  )
import Ouroboros.Consensus.Committee.Crypto
  ( CryptoSupportsAggregateVoteSigning (..)
  )
import Ouroboros.Consensus.Peras.Crypto.BLS
  ( PerasBLSCrypto
  , VRFOutput
  )
import Ouroboros.Consensus.Peras.Vote.V1 (PerasVoteEligibilityProof (..))
import Ouroboros.Consensus.Util.Bitmap (Bitmap)
import qualified Ouroboros.Consensus.Util.Bitmap as Bitmap
import Ouroboros.Network.Block (decodePoint, encodePoint)

-- | Concrete Peras certificates using BLS signatures
-- 'blk' is mainly used here to ensure type family injectivity.
-- For convenience/preventing annoying conversions, we also use it to indicate
-- in 'Point blk' for the boosted block.
data PerasCert blk
  = PerasCert
  { pcRoundNo :: !PerasRoundNo
  -- ^ Election identifier
  , pcBoostedBlock :: !(Point blk)
  -- ^ Certificate message, i.e., the hash of the block being boosted
  -- TODO: 'blk' here may not refer to the actual era of the boosted block,
  -- see https://github.com/tweag/cardano-peras/issues/251
  , pcVoters :: !(PerasCertVoters blk)
  -- ^ Voters who contributed to this certificate
  , pcSignature :: !(AggregateVoteSignature (PerasBLSCrypto blk))
  -- ^ Aggregate BLS signature on the hash of the election identifier and
  -- the certificate message
  }
  deriving (Show, Eq)

instance (Typeable blk, ConvertRawHash blk) => FromCBOR (PerasCert blk) where
  fromCBOR = do
    decodeListLenOf 4
    pcRoundNo <- fromCBOR
    pcBoostedBlock <- decodePoint (decodeRawHash (Proxy @blk))
    pcVoters <- fromCBOR
    pcSignature <- fromCBOR
    pure
      PerasCert
        { pcRoundNo
        , pcBoostedBlock
        , pcVoters
        , pcSignature
        }

instance (Typeable blk, ConvertRawHash blk) => ToCBOR (PerasCert blk) where
  toCBOR cert =
    encodeListLen 4
      <> toCBOR (pcRoundNo cert)
      <> encodePoint (encodeRawHash (Proxy @blk)) (pcBoostedBlock cert)
      <> toCBOR (pcVoters cert)
      <> toCBOR (pcSignature cert)

-- | Voters contained in a certificate with their appropriate eligibility proof
newtype PerasCertVoters blk
  = PerasCertVoters
  { unPerasCertVoters ::
      NE (Map PerasSeatIndex (PerasVoteEligibilityProof blk))
  }
  deriving (Eq, Show)

instance Typeable blk => FromCBOR (PerasCertVoters blk) where
  fromCBOR = do
    decodeListLenOf 2
    votersBitmap <- fromCBOR
    nonPersistentSigs <- fromCBOR

    either fail pure
      . fromCompactRepr
      $ CompactPerasCertVoters
        { votersBitmap
        , nonPersistentSigs
        }

instance Typeable blk => ToCBOR (PerasCertVoters blk) where
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
data CompactPerasCertVoters blk
  = CompactPerasCertVoters
  { votersBitmap :: !(Bitmap Word16)
  , nonPersistentSigs :: ![VRFOutput (PerasBLSCrypto blk)]
  }
  deriving (Eq, Show)

-- | Decode 'PerasCertVoters' from their compact representation.
--
-- See 'CompactPerasCertVoters' for the encoding scheme used here.
fromCompactRepr ::
  CompactPerasCertVoters blk ->
  Either String (PerasCertVoters blk)
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
              <> " more non-persistent voter eligibility proofs were provided"
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
  PerasCertVoters blk ->
  CompactPerasCertVoters blk
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
