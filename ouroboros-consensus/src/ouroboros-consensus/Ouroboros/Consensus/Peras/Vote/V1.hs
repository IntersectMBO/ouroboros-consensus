{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Concrete Peras vote types using BLS signatures.
--
-- NOTE: this module is meant to be imported qualified.
module Ouroboros.Consensus.Peras.Vote.V1
  ( PerasVote (..)
  , PerasVoteEligibilityProof (..)
  ) where

import Cardano.Binary
  ( FromCBOR (..)
  , ToCBOR (..)
  , decodeListLen
  , decodeListLenOf
  , encodeListLen
  )
import Data.Word (Word8)
import Ouroboros.Consensus.Block.SupportsPeras
  ( PerasBoostedBlock
  , PerasRoundNo
  , PerasSeatIndex
  )
import Ouroboros.Consensus.Peras.Crypto.BLS
  ( PerasBLSCrypto
  , PerasBLSVoteSignature
  , VRFOutput
  )

-- | Concrete Peras votes using BLS signatures
data PerasVote
  = PerasVote
  { pvRoundNo :: !PerasRoundNo
  -- ^ Election identifier
  , pvBoostedBlock :: !PerasBoostedBlock
  -- ^ Vote message, i.e., the hash of the block being voted for
  , pvSeatIndex :: !PerasSeatIndex
  -- ^ Seat index assigned to the committee member (identifies the voter)
  , pvEligibilityProof :: !PerasVoteEligibilityProof
  -- ^ Proof of eligibility for voting, depending on the type of membership to
  -- the committee (persistent vs non-persistent)
  , pvSignature :: !PerasBLSVoteSignature
  -- ^ BLS signature on the hash of the election identifier and vote message
  }
  deriving (Show, Eq)

instance FromCBOR PerasVote where
  fromCBOR = do
    decodeListLenOf 5
    pvRoundNo <- fromCBOR
    pvBoostedBlock <- fromCBOR
    pvSeatIndex <- fromCBOR
    pvEligibilityProof <- fromCBOR
    pvSignature <- fromCBOR
    pure
      PerasVote
        { pvRoundNo
        , pvBoostedBlock
        , pvSeatIndex
        , pvEligibilityProof
        , pvSignature
        }

instance ToCBOR PerasVote where
  toCBOR vote =
    encodeListLen 5
      <> toCBOR (pvRoundNo vote)
      <> toCBOR (pvBoostedBlock vote)
      <> toCBOR (pvSeatIndex vote)
      <> toCBOR (pvEligibilityProof vote)
      <> toCBOR (pvSignature vote)

-- | Proof of eligibility for voting for committee members
data PerasVoteEligibilityProof
  = -- | Persistent committee members require no additional proof of eligibility
    PersistentPerasVoteEligibilityProof
  | -- | Non-persistent committee members provide a VRF proof of eligibility
    NonPersistentPerasVoteEligibilityProof !(VRFOutput PerasBLSCrypto)
  deriving stock (Eq, Show)

instance FromCBOR PerasVoteEligibilityProof where
  fromCBOR = do
    len <- decodeListLen
    tag <- fromCBOR @Word8
    case (len, tag) of
      (1, 0) -> pure PersistentPerasVoteEligibilityProof
      (2, 1) -> NonPersistentPerasVoteEligibilityProof <$> fromCBOR
      _ ->
        fail $
          "Invalid PerasVoteEligibilityProof length/tag: "
            <> show (len, tag)

instance ToCBOR PerasVoteEligibilityProof where
  toCBOR = \case
    PersistentPerasVoteEligibilityProof ->
      encodeListLen 1
        <> toCBOR (0 :: Word8)
    NonPersistentPerasVoteEligibilityProof vrfOutput ->
      encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> toCBOR vrfOutput
