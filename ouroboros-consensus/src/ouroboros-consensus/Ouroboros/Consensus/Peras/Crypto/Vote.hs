{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Concrete Peras vote types using BLS signatures.
module Ouroboros.Consensus.Peras.Crypto.Vote
  ( PerasVote (..)
  , PerasVoteElegibilityProof (..)
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
import Ouroboros.Consensus.Block (ConvertRawHash, HeaderHash, StandardHash)
import Ouroboros.Consensus.Block.SupportsPeras (PerasRoundNo)
import qualified Ouroboros.Consensus.Peras.Crypto.BLS as BLS
import Ouroboros.Consensus.Peras.Crypto.Types (PerasBoostedBlock, PerasSeatIndex)

-- | Concrete Peras votes using BLS signatures
data PerasVote blk
  = PerasVote
  { pvRoundNo :: !PerasRoundNo
  -- ^ Election identifier
  , pvBoostedBlock :: !(PerasBoostedBlock blk)
  -- ^ Vote message, i.e., the hash of the block being voted for
  , pvSeatIndex :: !PerasSeatIndex
  -- ^ Seat index assigned to the committee member (identifies the voter)
  , pvElegibilityProof :: !PerasVoteElegibilityProof
  -- ^ Proof of eligibility for voting, depending on the type of membership to
  -- the committee (persistent vs non-persistent)
  , pvSignature :: !(BLS.PerasSignature BLS.SIGN)
  -- ^ BLS signature on the hash of the election identifier and vote message
  }

deriving instance Eq (HeaderHash blk) => Eq (PerasVote blk)
deriving instance Show (HeaderHash blk) => Show (PerasVote blk)

instance
  ( Typeable blk
  , ConvertRawHash blk
  , StandardHash blk
  ) =>
  FromCBOR (PerasVote blk)
  where
  fromCBOR = do
    decodeListLenOf 5
    pvRoundNo <- fromCBOR
    pvBoostedBlock <- fromCBOR
    pvSeatIndex <- fromCBOR
    pvElegibilityProof <- fromCBOR
    pvSignature <- fromCBOR
    pure
      PerasVote
        { pvRoundNo
        , pvBoostedBlock
        , pvSeatIndex
        , pvElegibilityProof
        , pvSignature
        }

instance
  ( Typeable blk
  , ConvertRawHash blk
  , StandardHash blk
  ) =>
  ToCBOR (PerasVote blk)
  where
  toCBOR vote =
    encodeListLen 5
      <> toCBOR (pvRoundNo vote)
      <> toCBOR (pvBoostedBlock vote)
      <> toCBOR (pvSeatIndex vote)
      <> toCBOR (pvElegibilityProof vote)
      <> toCBOR (pvSignature vote)

-- | Proof of eligibility for voting for committee members
data PerasVoteElegibilityProof
  = -- | Persistent committee members require no additional proof of eligibility
    PersistentPerasVoteElegibilityProof
  | -- | Non-persistent committee members provide a VRF proof of eligibility
    NonPersistentPerasVoteElegibilityProof !(BLS.PerasSignature BLS.VRF)
  deriving stock (Eq, Show)

instance FromCBOR PerasVoteElegibilityProof where
  fromCBOR = do
    len <- decodeListLen
    tag <- fromCBOR @Word8
    case (len, tag) of
      (1, 0) -> pure PersistentPerasVoteElegibilityProof
      (2, 1) -> NonPersistentPerasVoteElegibilityProof <$> fromCBOR
      _ -> fail $ "Invalid PerasVoteElegibilityProof length/tag: " <> show (len, tag)

instance ToCBOR PerasVoteElegibilityProof where
  toCBOR = \case
    PersistentPerasVoteElegibilityProof ->
      encodeListLen 1
        <> toCBOR (0 :: Word8)
    NonPersistentPerasVoteElegibilityProof vrfOutput ->
      encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> toCBOR vrfOutput
