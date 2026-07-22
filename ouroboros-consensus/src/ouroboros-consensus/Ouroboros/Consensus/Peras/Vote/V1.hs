{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Concrete Peras vote types using BLS signatures.
--
-- NOTE: this module is meant to be imported qualified.
module Ouroboros.Consensus.Peras.Vote.V1
  ( PerasVote (..)
  , retagPerasVote
  , PerasVoteEligibilityProof (..)
  ) where

import Cardano.Binary
  ( FromCBOR (..)
  , ToCBOR (..)
  , decodeListLen
  , decodeListLenOf
  , encodeListLen
  )
import Data.Coerce (coerce)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract (ConvertRawHash (..))
import Ouroboros.Consensus.Block.SupportsPeras
  ( BoostedBlock
  , IsPerasVote (..)
  , PerasBoostedBlock (..)
  , PerasRoundNo
  , PerasSeatIndex
  )
import Ouroboros.Consensus.Committee.Crypto (CryptoSupportsVoteSigning (..))
import Ouroboros.Consensus.Committee.EveryoneVotes
  ( EveryoneVotes
  , Vote (..)
  )
import Ouroboros.Consensus.Committee.WFALS (Vote (..), WFALS)
import Ouroboros.Consensus.Peras.Crypto.BLS
  ( PerasBLSCrypto
  , VRFOutput
  )
import Ouroboros.Consensus.Peras.Voting.Adapter
  ( PerasConversionError (..)
  , PerasVoteCompatibleWithVotingCommittee (..)
  , fromPerasSeatIndex
  , toPerasSeatIndex
  )
import Ouroboros.Network.Util.ShowProxy

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

-- | Retag a 'PerasVote' to change its phantom type tag.
retagPerasVote :: forall tag' tag. PerasVote tag -> PerasVote tag'
retagPerasVote = coerce

type instance BoostedBlock (PerasVote tag) = PerasBoostedBlock
instance
  ( ConvertRawHash blk
  , HashSize blk ~ 32
  ) =>
  IsPerasVote (PerasVote blk) blk
  where
  getPerasVoteSeatIndex = pvSeatIndex
  getPerasVoteRound = pvRoundNo
  getPerasVoteBlock = pvBoostedBlock

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
  toCBOR
    PerasVote
      { pvRoundNo
      , pvBoostedBlock
      , pvSeatIndex
      , pvEligibilityProof
      , pvSignature
      } =
      encodeListLen 5
        <> toCBOR pvRoundNo
        <> toCBOR pvBoostedBlock
        <> toCBOR pvSeatIndex
        <> toCBOR pvEligibilityProof
        <> toCBOR pvSignature

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

instance ShowProxy tag => ShowProxy (PerasVote tag) where
  showProxy _ = "PerasVote " <> showProxy (Proxy @tag)

-- * Compatibility with voting committee implementations

-- 'PerasVote's are compatible with 'WFALS' as long as we make sure to avoid
-- overflowing their `Word16` seat index.
instance
  PerasVoteCompatibleWithVotingCommittee
    (PerasVote tag)
    PerasBLSCrypto
    WFALS
  where
  toPerasVote = \case
    WFALSPersistentVote seatIndex electionId candidate sig -> do
      perasSeatIndex <- toPerasSeatIndex seatIndex
      pure $
        PerasVote
          { pvRoundNo = electionId
          , pvBoostedBlock = candidate
          , pvSeatIndex = perasSeatIndex
          , pvEligibilityProof = PersistentPerasVoteEligibilityProof
          , pvSignature = sig
          }
    WFALSNonPersistentVote seatIndex electionId candidate vrfOutput sig -> do
      perasSeatIndex <- toPerasSeatIndex seatIndex
      let proof = NonPersistentPerasVoteEligibilityProof vrfOutput
      pure $
        PerasVote
          { pvRoundNo = electionId
          , pvBoostedBlock = candidate
          , pvSeatIndex = perasSeatIndex
          , pvEligibilityProof = proof
          , pvSignature = sig
          }

  fromPerasVote = \case
    PerasVote electionId candidate seatIndex proof sig -> do
      let seatIndex' = fromPerasSeatIndex seatIndex
      case proof of
        PersistentPerasVoteEligibilityProof ->
          pure $
            WFALSPersistentVote
              seatIndex'
              electionId
              candidate
              sig
        NonPersistentPerasVoteEligibilityProof vrfOutput ->
          pure $
            WFALSNonPersistentVote
              seatIndex'
              electionId
              candidate
              vrfOutput
              sig

-- 'PerasVote's are compatible with 'EveryoneVotes' as long as we make sure
-- to only accept votes with persistent eligibility proofs (in addition to
-- avoiding overflowing their `Word16` seat index).
instance
  PerasVoteCompatibleWithVotingCommittee
    (PerasVote tag)
    PerasBLSCrypto
    EveryoneVotes
  where
  toPerasVote = \case
    EveryoneVotesVote seatIndex electionId candidate sig -> do
      perasSeatIndex <- toPerasSeatIndex seatIndex
      pure $
        PerasVote
          { pvRoundNo = electionId
          , pvBoostedBlock = candidate
          , pvSeatIndex = perasSeatIndex
          , pvEligibilityProof = PersistentPerasVoteEligibilityProof
          , pvSignature = sig
          }

  fromPerasVote = \case
    PerasVote electionId candidate seatIndex proof sig -> do
      let seatIndex' = fromPerasSeatIndex seatIndex
      case proof of
        PersistentPerasVoteEligibilityProof ->
          pure $
            EveryoneVotesVote
              seatIndex'
              electionId
              candidate
              sig
        NonPersistentPerasVoteEligibilityProof _ ->
          Left $
            EveryoneVotesButFoundNonPersistentVoterInVote seatIndex'
