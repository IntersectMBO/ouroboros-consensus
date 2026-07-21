{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Base Peras types used throughout the codebase.
module Ouroboros.Consensus.Peras.Types
  ( -- * Peras round numbers
    PerasRoundNo (..)
  , onPerasRoundNo

    -- * Peras boosted blocks
  , PerasBoostedBlock (..)
  , BoostedBlock
  , BoostedBlockCompatibleWithPoint (..)

    -- * Peras seat indices
  , PerasSeatIndex (..)

    -- * Peras vote parameters
  , PerasVoteTarget (..)
  , PerasVoteId (..)
  , VoteWeight (..) -- Re-exported from Committee.Types for convenience
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
import Data.Kind (Type)
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract
  ( ConvertRawHash (..)
  , Point
  , WithOrigin
  )
import Ouroboros.Consensus.Block.RealPoint
  ( Bytes32RealPoint
  , decodeBytes32RealPoint
  , encodeBytes32RealPoint
  , fromBytes32RealPoint
  , pointToWithOriginRealPoint
  , toBytes32RealPoint
  , withOriginRealPointToPoint
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime)
import Ouroboros.Consensus.Committee.Types (VoteWeight (..))
import Ouroboros.Consensus.Util (ShowProxy (..))
import Ouroboros.Consensus.Util.CBOR (decodeWithOrigin, encodeWithOrigin)
import Ouroboros.Consensus.Util.Condense (Condense (..))
import Quiet (Quiet (..))

-- * Peras Round numbers

-- | Round number in a Peras election.
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

-- ** Peras boosted blocks

-- | Conversion back and forth between a boosted blocks and a abstract points.
class BoostedBlockCompatibleWithPoint boostedBlock blk where
  boostedBlockToPoint :: boostedBlock -> Point blk
  pointToBoostedBlock :: Point blk -> boostedBlock

type family BoostedBlock voteOrCert :: Type

instance BoostedBlockCompatibleWithPoint (Point blk) blk where
  boostedBlockToPoint = id
  pointToBoostedBlock = id

type instance BoostedBlock (WithArrivalTime voteOrCert) = BoostedBlock voteOrCert

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

instance
  ( ConvertRawHash blk
  , HashSize blk ~ 32
  ) =>
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

instance FromCBOR PerasBoostedBlock where
  fromCBOR = PerasBoostedBlock <$> decodeWithOrigin decodeBytes32RealPoint

instance ToCBOR PerasBoostedBlock where
  toCBOR = encodeWithOrigin encodeBytes32RealPoint . unPerasBoostedBlock

-- * Peras seat indices

-- | Seat index in the voting committee used for Peras
newtype PerasSeatIndex
  = PerasSeatIndex
  { unPerasSeatIndex :: Word16
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromCBOR, ToCBOR, Enum, Bounded, NFData)
  deriving anyclass NoThunks

-- * Peras vote parameters

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

instance FromCBOR (PerasVoteId) where
  fromCBOR = do
    decodeListLenOf 2
    pviRoundNo <- fromCBOR
    pviSeatIndex <- fromCBOR
    pure
      PerasVoteId
        { pviRoundNo
        , pviSeatIndex
        }

instance ToCBOR (PerasVoteId) where
  toCBOR
    PerasVoteId
      { pviRoundNo
      , pviSeatIndex
      } =
      encodeListLen 2
        <> toCBOR pviRoundNo
        <> toCBOR pviSeatIndex
