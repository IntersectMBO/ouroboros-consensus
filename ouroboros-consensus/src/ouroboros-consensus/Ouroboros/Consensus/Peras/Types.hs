{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
  , PerasVoteStake (..)
  , BoostedBlock
  , BoostedBlockCompatibleWithPoint (..)
  , stakeAboveThreshold
  , PerasVoteTarget (..)
  , PerasVoteId (..)
  , PerasVoterId (..)
  , PerasVoteStakeDistr (..)
  , lookupPerasVoteStake
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
import Cardano.Ledger.Hashes (KeyHash, KeyRole (..))
import Codec.Serialise.Class (Serialise (..))
import Control.DeepSeq (NFData)
import Data.ByteString.Short (ShortByteString)
import Data.Coerce (Coercible, coerce)
import Data.Containers.NonEmpty (HasNonEmpty (..))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Semigroup (Sum (..))
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract (HeaderHash, Point, WithOrigin)
import Ouroboros.Consensus.Block.RealPoint
  ( Bytes32RealPoint
  , decodeBytes32RealPoint
  , encodeBytes32RealPoint
  , fromBytes32RealPoint
  , pointToWithOriginRealPoint
  , toBytes32RealPoint
  , withOriginRealPointToPoint
  )
import Ouroboros.Consensus.Committee.WFA (SeatIndex (..))
import Ouroboros.Consensus.Peras.Params
  ( PerasParams (..)
  , PerasQuorumStakeThreshold (..)
  , PerasQuorumStakeThresholdSafetyMargin (..)
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
  deriving newtype (Enum, Eq, Ord, Num, Bounded, NoThunks, Serialise, NFData, ToCBOR, FromCBOR)

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

instance Coercible (HeaderHash blk) ShortByteString => BoostedBlockCompatibleWithPoint PerasBoostedBlock blk where
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
  deriving newtype (FromCBOR, ToCBOR, Enum, Bounded)
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
data PerasVoteId blk
  = PerasVoteId
  { pviRoundNo :: !PerasRoundNo
  , pviVoterId :: !PerasVoterId
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NoThunks

instance ShowProxy blk => ShowProxy (PerasVoteId blk) where
  showProxy _ = "PerasVoteId " <> showProxy (Proxy @blk)

instance Serialise (PerasVoteId blk) where
  encode PerasVoteId{pviRoundNo, pviVoterId} =
    encodeListLen 2
      <> encode pviRoundNo
      <> toCBOR (unPerasVoterId pviVoterId)
  decode = do
    decodeListLenOf 2
    pviRoundNo <- decode
    pviVoterId <- PerasVoterId <$> fromCBOR
    pure $ PerasVoteId{pviRoundNo, pviVoterId}

-- NOTE: At the moment there is no consensus from researchers/engineers on how
-- we go from the absolute stake of a voter in the ledger to the relative stake
-- of their vote in the voting commitee (given that the quorum is expressed as
-- a relative value of the voting commitee total stake).
--
-- So, for now you can consider this 'Rational' as the best approximation we
-- have at the moment of the concrete type for a relative vote stake that can be
-- compared to the quorum threshold value (also currently a 'Rational').
newtype PerasVoteStake
  = PerasVoteStake
  { unPerasVoteStake :: Rational
  }
  deriving newtype (Eq, Ord, Num, Fractional, NoThunks, NFData, Serialise)
  deriving stock Generic
  deriving Show via Quiet PerasVoteStake
  deriving Semigroup via Sum Rational
  deriving Monoid via Sum Rational

-- | Check whether a given vote stake is above the quorum threshold.
--
-- TODO: this function assumes that the 'PerasVoteStake' and the quorum
-- threshold used in 'PerasParams' are expressed in the same units. That is,
-- both are either absolute or relative (normalized) values. Under the current
-- current implementation of 'PerasParams', this function only makes sense when
-- both values are relative (normalized) values, so we should either normalize
-- the 'PerasVoteStake' before calling this function, or change this function to
-- accept a stake distribution and perform the normalization internally.
stakeAboveThreshold :: PerasParams -> PerasVoteStake -> Bool
stakeAboveThreshold params voteStake =
  stake >= quorumThreshold + safetyMargin
 where
  stake =
    unPerasVoteStake voteStake
  quorumThreshold =
    unPerasQuorumStakeThreshold
      (perasQuorumStakeThreshold params)
  safetyMargin =
    unPerasQuorumStakeThresholdSafetyMargin
      (perasQuorumStakeThresholdSafetyMargin params)

-- ** Voting stake distributions

-- | The identifier of a voter in a Peras election
newtype PerasVoterId
  = PerasVoterId
  { unPerasVoterId :: KeyHash StakePool
  }
  deriving newtype (NoThunks, NFData, FromCBOR, ToCBOR)
  deriving stock (Eq, Ord, Generic)
  deriving Show via Quiet PerasVoterId

instance Serialise PerasVoterId where
  encode = toCBOR . unPerasVoterId
  decode = PerasVoterId <$> fromCBOR

-- | Voting stake distribution for a Peras election
newtype PerasVoteStakeDistr
  = PerasVoteStakeDistr
  { unPerasVoteStakeDistr :: Map PerasVoterId PerasVoteStake
  }
  deriving newtype NoThunks
  deriving stock (Show, Eq, Generic)

-- | Lookup the stake of a vote cast by a member of a given stake distribution.
lookupPerasVoteStake ::
  PerasVoterId ->
  PerasVoteStakeDistr ->
  Maybe PerasVoteStake
lookupPerasVoteStake voterId distr =
  Map.lookup
    voterId
    (unPerasVoteStakeDistr distr)

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
