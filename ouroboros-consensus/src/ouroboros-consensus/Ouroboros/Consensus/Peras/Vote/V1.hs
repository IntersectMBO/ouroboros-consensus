{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.SupportsPeras
  ( PerasBoostedBlock (..)
  , PerasRoundNo
  , PerasSeatIndex, IsPerasCert, IsPerasVote (..)
  )
import Ouroboros.Consensus.Committee.Crypto (CryptoSupportsVoteSigning (..))
import Ouroboros.Consensus.Peras.Crypto.BLS
  ( PerasBLSCrypto
  , VRFOutput
  )
import Data.Coerce (Coercible)
import Ouroboros.Consensus.Block.Abstract (HeaderHash)
import Data.ByteString.Short (ShortByteString)
import Ouroboros.Consensus.Block.RealPoint (withOriginRealPointToPoint, fromBytes32RealPoint)

-- | Concrete Peras votes using BLS signatures
--
-- NOTE: the 'tag' parameter is a phantom type used to track the block type that
-- the vote is associated with, to ensure injectivity when 'V1.PerasVote' is
-- used as a type instance for 'BlockSupportsPeras' class.
data PerasVote tag
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
  , pvSignature :: !(VoteSignature PerasBLSCrypto)
  -- ^ BLS signature on the hash of the election identifier and vote message
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

instance
  Coercible (HeaderHash blk) ShortByteString =>
  IsPerasVote (PerasVote blk) blk where
  getPerasVoteRound =
    pvRoundNo
  getPerasVoteBlock =
    withOriginRealPointToPoint
      . fmap fromBytes32RealPoint
      . unPerasBoostedBlock
      . pvBoostedBlock

instance Typeable tag => FromCBOR (PerasVote tag) where
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

instance Typeable tag => ToCBOR (PerasVote tag) where
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
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

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
