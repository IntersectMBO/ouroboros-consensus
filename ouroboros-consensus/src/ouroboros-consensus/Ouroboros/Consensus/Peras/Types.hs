{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Base Peras types used throughout the implementation.
module Ouroboros.Consensus.Peras.Types
  ( PerasRoundNo (..)
  , onPerasRoundNo
  , PerasBoostedBlock (..)
  , PerasSeatIndex (..)
  , BoostedBlock
  , BoostedBlockCompatibleWithPoint (..)
  , weightAboveThreshold
  , PerasVoteTarget (..)
  , PerasVoteId (..)
  , VoteWeight (..) -- Re-exported from Committee.Types for convenience
  , VoteWeightDistr (..)
  , lookupVoteWeight
  , PerasConversionError (..)
  , fromPerasSeatIndex
  , toPerasSeatIndex
  )
where

import Cardano.Binary
  ( FromCBOR (..)
  , ToCBOR (..)
  , decodeListLenOf
  , encodeListLen
  )
import Codec.Serialise.Class (Serialise (..))
import Control.DeepSeq (NFData)
import Data.Coerce (coerce)
import Data.Containers.NonEmpty (HasNonEmpty (..))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract (ConvertRawHash, Point, WithOrigin)
import Ouroboros.Consensus.Block.RealPoint
  ( Bytes32RealPoint
  , decodeBytes32RealPoint
  , encodeBytes32RealPoint
  , fromBytes32RealPoint
  , pointToWithOriginRealPoint
  , toBytes32RealPoint
  , withOriginRealPointToPoint
  )
import Ouroboros.Consensus.Committee.Types (VoteWeight (..))
import Ouroboros.Consensus.Committee.WFA (SeatIndex (..))
import Ouroboros.Consensus.Peras.Params
  ( PerasParams (..)
  , PerasQuorumWeightThreshold (..)
  , PerasQuorumWeightThresholdSafetyMargin (..)
  )
import Ouroboros.Consensus.Util (ShowProxy (..))
import Ouroboros.Consensus.Util.CBOR (decodeWithOrigin, encodeWithOrigin)
import Ouroboros.Consensus.Util.Condense (Condense (..))
import Quiet (Quiet (..))

-- * Peras types

class BoostedBlockCompatibleWithPoint boostedBlock blk where
  boostedBlockToPoint :: boostedBlock -> Point blk
  pointToBoostedBlock :: Point blk -> boostedBlock

type family BoostedBlock voteOrCert :: Type

instance BoostedBlockCompatibleWithPoint (Point blk) blk where
  boostedBlockToPoint = id
  pointToBoostedBlock = id

-- ** Round numbers

newtype PerasRoundNo
  = PerasRoundNo
  { unPerasRoundNo :: Word64
  }
  deriving Show via Quiet PerasRoundNo
  deriving stock Generic
  deriving newtype
    ( Enum
    , Eq
    , Ord
    , Num
    , Bounded
    , NoThunks
    , NFData
    , Serialise
    , ToCBOR
    , FromCBOR
    )

instance Condense PerasRoundNo where
  condense = show . unPerasRoundNo

instance ShowProxy PerasRoundNo where
  showProxy _ = "PerasRoundNo"

-- | Lift a binary operation on 'Word64' to 'PerasRoundNo'
onPerasRoundNo ::
  (Word64 -> Word64 -> Word64) ->
  (PerasRoundNo -> PerasRoundNo -> PerasRoundNo)
onPerasRoundNo = coerce

-- ** Boosted blocks

-- | The slot number and 32-byte hash of the block being voted for.
--
-- NOTE: this type is mostly used in production votes and certificates, while
-- mocked votes and certificates generally use the more abstract 'Point blk'.
newtype PerasBoostedBlock
  = PerasBoostedBlock
  { unPerasBoostedBlock :: WithOrigin Bytes32RealPoint
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

instance FromCBOR PerasBoostedBlock where
  fromCBOR = PerasBoostedBlock <$> decodeWithOrigin decodeBytes32RealPoint

instance ToCBOR PerasBoostedBlock where
  toCBOR = encodeWithOrigin encodeBytes32RealPoint . unPerasBoostedBlock

instance
  ConvertRawHash blk =>
  BoostedBlockCompatibleWithPoint PerasBoostedBlock blk
  where
  boostedBlockToPoint =
    withOriginRealPointToPoint
      . fmap fromBytes32RealPoint
      . unPerasBoostedBlock
  pointToBoostedBlock =
    PerasBoostedBlock
      . fmap toBytes32RealPoint
      . pointToWithOriginRealPoint

-- ** Seat indices

-- | Seat index in the voting committee used for Peras
newtype PerasSeatIndex
  = PerasSeatIndex
  { unPerasSeatIndex :: Word16
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromCBOR, ToCBOR, Enum, Bounded, NFData)
  deriving anyclass NoThunks

-- ** Vote parameters

-- | The target of a vote in a Peras election
data PerasVoteTarget blk
  = PerasVoteTarget
  { pvtRoundNo :: !PerasRoundNo
  , pvtBlock :: !(Point blk)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NoThunks

-- | The identifier of a vote in a Peras election
data PerasVoteId
  = PerasVoteId
  { pviRoundNo :: !PerasRoundNo
  , pviSeatIndex :: !PerasSeatIndex
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NoThunks

instance ShowProxy (PerasVoteId) where
  showProxy _ = "PerasVoteId"

instance Serialise (PerasVoteId) where
  encode PerasVoteId{pviRoundNo, pviSeatIndex} =
    encodeListLen 2
      <> encode pviRoundNo
      <> toCBOR (unPerasSeatIndex pviSeatIndex)
  decode = do
    decodeListLenOf 2
    pviRoundNo <- decode
    pviSeatIndex <- PerasSeatIndex <$> fromCBOR
    pure $ PerasVoteId{pviRoundNo, pviSeatIndex}

-- | Check whether a given vote weight is above the quorum threshold.
--
-- NOTE: this function assumes that the 'VoteWeight' and the quorum
-- threshold used in 'PerasParams' are expressed in the same units. That is,
-- both are either absolute or relative (normalized) values. Under the current
-- current implementation of 'PerasParams', this function only makes sense when
-- both values are relative (normalized) values.
weightAboveThreshold :: PerasParams blk -> VoteWeight -> Bool
weightAboveThreshold params voteWeight =
  weight >= quorumThreshold + safetyMargin
 where
  weight =
    unVoteWeight voteWeight
  quorumThreshold =
    unPerasQuorumWeightThreshold
      (perasQuorumWeightThreshold params)
  safetyMargin =
    unPerasQuorumWeightThresholdSafetyMargin
      (perasQuorumWeightThresholdSafetyMargin params)

instance Serialise PerasSeatIndex where
  encode = toCBOR . unPerasSeatIndex
  decode = PerasSeatIndex <$> fromCBOR

-- | Voting weight distribution for a Peras election
-- TODO: remove, at call site an argument of this type will be replaced by a 'PerasVotingCommittee blk'.
newtype VoteWeightDistr
  = VoteWeightDistr
  { unVoteWeightDistr :: Map PerasSeatIndex VoteWeight
  }
  deriving newtype NoThunks
  deriving stock (Show, Eq, Generic)

-- | Lookup the weight of a vote cast by a member of a given weight distribution.
-- TODO: remove this function since it will be replaced by 'eligiblePartyVoteWeight' from Committee.Class
lookupVoteWeight ::
  PerasSeatIndex ->
  VoteWeightDistr ->
  Maybe VoteWeight
lookupVoteWeight seatIndex distr =
  Map.lookup
    seatIndex
    (unVoteWeightDistr distr)

-- ** Conversion errors

-- | Errors that can occur when converting between Peras and committee types
data PerasConversionError
  = EveryoneVotesButFoundNonPersistentVoterInVote SeatIndex
  | EveryoneVotesButFoundNonPersistentVotersInCert (NE [SeatIndex])
  | SeatIndexOverflowError Word64
  | CryptoError String
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

-- ** Seat index conversions

-- | Convert a Peras seat index to a committee seat index.
fromPerasSeatIndex ::
  PerasSeatIndex ->
  SeatIndex
fromPerasSeatIndex (PerasSeatIndex seatIndex) =
  SeatIndex (fromIntegral @Word16 @Word64 seatIndex)

-- | Convert a committee seat index to a Peras seat index
--
-- NOTE: this can fail if the seat index in the committee vote or certificate
-- overflows the smaller 'Word16' type used by Peras votes and certificates.
-- In practice, this should never happen unless there is a bug in the voting
-- committee logic.
toPerasSeatIndex ::
  SeatIndex ->
  Either PerasConversionError PerasSeatIndex
toPerasSeatIndex (SeatIndex seatIndex)
  | seatIndex <= fromIntegral @Word16 @Word64 maxBound =
      Right (PerasSeatIndex (fromIntegral @Word64 @Word16 seatIndex))
  | otherwise =
      Left (SeatIndexOverflowError seatIndex)
