{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Word (Word8)
import Ouroboros.Consensus.Block
  ( ConvertRawHash (..)
  , Point
  , decodeRawHash
  , encodeRawHash
  )
import Ouroboros.Consensus.Block.SupportsPeras
  ( PerasRoundNo
  , PerasSeatIndex
  )
import Ouroboros.Consensus.Committee.Crypto (CryptoSupportsVoteSigning (..))
import Ouroboros.Consensus.Peras.Crypto.BLS
  ( PerasBLSCrypto
  , VRFOutput
  )
import Ouroboros.Network.Block (decodePoint, encodePoint)

-- | Concrete Peras votes using BLS signatures
-- 'blk' is mainly used here to ensure type family injectivity.
-- For convenience/preventing annoying conversions, we also use it to indicate
-- in 'Point blk' for the boosted block.
data PerasVote blk
  = PerasVote
  { pvRoundNo :: !PerasRoundNo
  -- ^ Election identifier
  , pvBoostedBlock :: !(Point blk)
  -- ^ Vote message, i.e., the hash of the block being voted for
  -- TODO: 'blk' here may not refer to the actual era of the boosted block,
  -- see https://github.com/tweag/cardano-peras/issues/251
  , pvSeatIndex :: !PerasSeatIndex
  -- ^ Seat index assigned to the committee member (identifies the voter)
  , pvEligibilityProof :: !(PerasVoteEligibilityProof blk)
  -- ^ Proof of eligibility for voting, depending on the type of membership to
  -- the committee (persistent vs non-persistent)
  , pvSignature :: !(VoteSignature (PerasBLSCrypto blk))
  -- ^ BLS signature on the hash of the election identifier and vote message
  }
  deriving (Show, Eq)

instance (Typeable blk, ConvertRawHash blk) => FromCBOR (PerasVote blk) where
  fromCBOR = do
    decodeListLenOf 5
    pvRoundNo <- fromCBOR
    pvBoostedBlock <- decodePoint (decodeRawHash (Proxy @blk))
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

instance (Typeable blk, ConvertRawHash blk) => ToCBOR (PerasVote blk) where
  toCBOR vote =
    encodeListLen 5
      <> toCBOR (pvRoundNo vote)
      <> encodePoint (encodeRawHash (Proxy @blk)) (pvBoostedBlock vote)
      <> toCBOR (pvSeatIndex vote)
      <> toCBOR (pvEligibilityProof vote)
      <> toCBOR (pvSignature vote)

-- | Proof of eligibility for voting for committee members
data PerasVoteEligibilityProof blk
  = -- | Persistent committee members require no additional proof of eligibility
    PersistentPerasVoteEligibilityProof
  | -- | Non-persistent committee members provide a VRF proof of eligibility
    NonPersistentPerasVoteEligibilityProof !(VRFOutput (PerasBLSCrypto blk))
  deriving stock (Eq, Show)

instance Typeable blk => FromCBOR (PerasVoteEligibilityProof blk) where
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

instance Typeable blk => ToCBOR (PerasVoteEligibilityProof blk) where
  toCBOR = \case
    PersistentPerasVoteEligibilityProof ->
      encodeListLen 1
        <> toCBOR (0 :: Word8)
    NonPersistentPerasVoteEligibilityProof vrfOutput ->
      encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> toCBOR vrfOutput
