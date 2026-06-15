{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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
  , retagPerasCert
  , PerasCertVoters (..)
  ) where

import Cardano.Binary
  ( FromCBOR (..)
  , ToCBOR (..)
  , decodeListLenOf
  , encodeListLen
  )
import Codec.Serialise (Serialise (..))
import Control.Monad (when)
import Control.Monad.Error.Class (MonadError (..))
import Data.Containers.NonEmpty (HasNonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.NonEmpty as NEMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust)
import Data.Typeable (Proxy (..), Typeable)
import Data.Word (Word16)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))
import Ouroboros.Consensus.Block.Abstract (ConvertRawHash)
import Ouroboros.Consensus.Block.SupportsPeras
  ( BoostedBlock
  , IsPerasCert (..)
  , PerasBoostedBlock (..)
  , PerasCertCompatibleWithVotingCommittee (..)
  , PerasConversionError (..)
  , PerasRoundNo
  , PerasSeatIndex (..)
  , fromPerasSeatIndex
  , toPerasSeatIndex
  )
import Ouroboros.Consensus.Committee.Crypto
  ( CryptoSupportsAggregateVoteSigning (..)
  , CryptoSupportsVRF (..)
  )
import Ouroboros.Consensus.Committee.EveryoneVotes
  ( Cert (..)
  , EveryoneVotes
  )
import Ouroboros.Consensus.Committee.WFA (SeatIndex (..))
import Ouroboros.Consensus.Committee.WFALS (Cert (..), WFALS)
import Ouroboros.Consensus.Node.Serialisation (SerialiseNodeToNode (..))
import Ouroboros.Consensus.Peras.Crypto.BLS
  ( PerasBLSCrypto
  )
import Ouroboros.Consensus.Peras.Vote.V1 (PerasVoteEligibilityProof (..))
import Ouroboros.Consensus.Util.Bitmap (Bitmap)
import qualified Ouroboros.Consensus.Util.Bitmap as Bitmap
import Ouroboros.Network.Util.ShowProxy (ShowProxy (..))

-- | Concrete Peras certificates using BLS signatures
--
-- NOTE: the 'tag' parameter is a phantom type used to track the block type that
-- the certificate is associated with, to ensure injectivity when 'V1.PerasCert'
-- is used as a type instance for 'BlockSupportsPeras' class.
data PerasCert tag
  = PerasCert
  { pcRoundNo :: !PerasRoundNo
  -- ^ Election identifier
  , pcBoostedBlock :: !PerasBoostedBlock
  -- ^ Certificate message, i.e., the hash of the block being boosted
  , pcVoters :: !PerasCertVoters
  -- ^ Voters who contributed to this certificate
  , pcSignature :: !(AggregateVoteSignature PerasBLSCrypto)
  -- ^ Aggregate BLS signature on the hash of the election identifier and
  -- the certificate message
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

-- | Retag a 'PerasCert' to change its phantom type tag.
retagPerasCert :: forall tag' tag. PerasCert tag -> PerasCert tag'
retagPerasCert cert =
  PerasCert
    { pcRoundNo = pcRoundNo cert
    , pcBoostedBlock = pcBoostedBlock cert
    , pcVoters = pcVoters cert
    , pcSignature = pcSignature cert
    }

instance
  ConvertRawHash blk =>
  IsPerasCert (PerasCert blk) blk
  where
  getPerasCertRound = pcRoundNo
  getPerasCertBlock = pcBoostedBlock

type instance BoostedBlock (PerasCert tag) = PerasBoostedBlock

instance Typeable tag => FromCBOR (PerasCert tag) where
  fromCBOR = do
    decodeListLenOf 4
    pcRoundNo <- fromCBOR
    pcBoostedBlock <- fromCBOR
    pcVoters <- fromCBOR
    pcSignature <- fromCBOR
    pure
      PerasCert
        { pcRoundNo
        , pcBoostedBlock
        , pcVoters
        , pcSignature
        }

instance Typeable tag => ToCBOR (PerasCert tag) where
  toCBOR cert =
    encodeListLen 4
      <> toCBOR (pcRoundNo cert)
      <> toCBOR (pcBoostedBlock cert)
      <> toCBOR (pcVoters cert)
      <> toCBOR (pcSignature cert)

instance Typeable tag => SerialiseNodeToNode blk (PerasCert tag) where
  encodeNodeToNode _ccfg _version = toCBOR

  decodeNodeToNode _ccfg _version = fromCBOR

instance Typeable tag => Serialise (PerasCert tag) where
  encode = toCBOR
  decode = fromCBOR

instance ShowProxy tag => ShowProxy (PerasCert tag) where
  showProxy _ = "PerasCert " <> showProxy (Proxy @tag)

-- | Voters contained in a certificate with their appropriate eligibility proof
newtype PerasCertVoters
  = PerasCertVoters
  { unPerasCertVoters ::
      NE (Map PerasSeatIndex PerasVoteEligibilityProof)
  }
  deriving stock (Show, Eq, Generic)

deriving via
  OnlyCheckWhnfNamed "PerasCertVoters" PerasCertVoters
  instance
    NoThunks PerasCertVoters

instance FromCBOR PerasCertVoters where
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
  , nonPersistentSigs :: ![VRFOutput PerasBLSCrypto]
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

-- * Compatibility with voting committee implementations

-- | Convert concrete Peras certificate voters to abstract committee voters
fromPerasCertVoters ::
  PerasCertVoters ->
  NE (Map SeatIndex (Maybe (VRFOutput PerasBLSCrypto)))
fromPerasCertVoters voters =
  NEMap.fromAscList
    . NonEmpty.map
      ( \(seatIndex, proof) ->
          ( fromPerasSeatIndex seatIndex
          , fromPerasVoteEligibilityProof proof
          )
      )
    . NEMap.toAscList
    . unPerasCertVoters
    $ voters
 where
  fromPerasVoteEligibilityProof = \case
    PersistentPerasVoteEligibilityProof -> Nothing
    NonPersistentPerasVoteEligibilityProof vrfOutput -> Just vrfOutput

-- | Convert abstract committee voters to concrete Peras certificate voters
toPerasCertVoters ::
  NE (Map SeatIndex (Maybe (VRFOutput PerasBLSCrypto))) ->
  Either PerasConversionError PerasCertVoters
toPerasCertVoters voters =
  fmap PerasCertVoters
    . fmap NEMap.fromAscList
    . traverse
      ( \(seatIndex, proof) -> do
          seatIndex' <- toPerasSeatIndex seatIndex
          let proof' = toPerasVoteEligibilityProof proof
          pure (seatIndex', proof')
      )
    . NEMap.toAscList
    $ voters
 where
  toPerasVoteEligibilityProof = \case
    Nothing -> PersistentPerasVoteEligibilityProof
    Just vrfOutput -> NonPersistentPerasVoteEligibilityProof vrfOutput

-- 'PerasCert's are compatible with 'WFALS' as long as we make sure to avoid
-- overflowing the `Word16` seat index of each voter.
instance
  PerasCertCompatibleWithVotingCommittee
    (PerasCert tag)
    PerasBLSCrypto
    WFALS
  where
  toPerasCert = \case
    WFALSCert electionId candidate voters sig -> do
      voters' <- toPerasCertVoters voters
      pure $
        PerasCert
          { pcRoundNo = electionId
          , pcBoostedBlock = candidate
          , pcVoters = voters'
          , pcSignature = sig
          }

  fromPerasCert = \case
    PerasCert electionId candidate voters sig -> do
      let voters' = fromPerasCertVoters voters
      pure $
        WFALSCert
          electionId
          candidate
          voters'
          sig

-- 'PerasCert's are compatible with 'EveryoneVotes' as long as we make sure
-- to only accept certificates containing only persistent eligibility proofs
-- (in addition to avoiding overflowing the `Word16` seat index of each voter).
instance
  PerasCertCompatibleWithVotingCommittee
    (PerasCert tag)
    PerasBLSCrypto
    EveryoneVotes
  where
  toPerasCert = \case
    EveryoneVotesCert electionId candidate voters sig -> do
      voters' <-
        toPerasCertVoters
          . NEMap.fromSet (const Nothing)
          $ voters
      pure $
        PerasCert
          { pcRoundNo = electionId
          , pcBoostedBlock = candidate
          , pcVoters = voters'
          , pcSignature = sig
          }

  fromPerasCert = \case
    PerasCert electionId candidate voters sig -> do
      let voters' = fromPerasCertVoters voters
      case nonPersistentVoters voters' of
        Nothing ->
          pure $
            EveryoneVotesCert
              electionId
              candidate
              (NEMap.keysSet voters')
              sig
        Just nonPersistentSeatIndices ->
          Left $
            EveryoneVotesButFoundNonPersistentVotersInCert
              nonPersistentSeatIndices
   where
    nonPersistentVoters voters' =
      case Map.keys (NEMap.filter isJust voters') of
        [] ->
          Nothing
        nonPersistentSeats ->
          Just (NonEmpty.fromList nonPersistentSeats)
