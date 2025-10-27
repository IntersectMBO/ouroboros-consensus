{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Peras protocol parameters
module Ouroboros.Consensus.Peras.Params
  ( PerasIgnoranceRounds (..)
  , PerasCooldownRounds (..)
  , PerasBlockMinSlots (..)
  , PerasCertArrivalThreshold (..)
  , PerasParams (..)
  )
where

import Data.Word (Word64)
import GHC.Generics (Generic)
import Ouroboros.Consensus.Util.IOLike (NoThunks)
import Quiet (Quiet (..))

{-------------------------------------------------------------------------------
  Protocol parameters
-------------------------------------------------------------------------------}

-- | Number of rounds for which to ignore certificates after entering a
-- cooldown period.
newtype PerasIgnoranceRounds
  = PerasIgnoranceRounds {unPerasIgnoranceRounds :: Word64}
  deriving Show via Quiet PerasIgnoranceRounds
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, NoThunks)

-- | Minimum number of rounds to wait before voting again after a cooldown
-- period starts.
newtype PerasCooldownRounds
  = PerasCooldownRounds {unPerasCooldownRounds :: Word64}
  deriving Show via Quiet PerasCooldownRounds
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, NoThunks)

-- | Minimum age (in slots) of a block to be voted on at the beginning of a
-- Peras round.
newtype PerasBlockMinSlots
  = PerasBlockMinSlots {unPerasBlockMinSlots :: Word64}
  deriving Show via Quiet PerasBlockMinSlots
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, NoThunks)

-- | Maximum number of slots to after the start of a round to consider a
-- certificate for voting.
newtype PerasCertArrivalThreshold
  = PerasCertArrivalThreshold {unPerasCertArrivalThreshold :: Word64}
  deriving Show via Quiet PerasCertArrivalThreshold
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, NoThunks)

{-------------------------------------------------------------------------------
  Protocol parameters bundle
-------------------------------------------------------------------------------}

data PerasParams = PerasParams
  { perasIgnoranceRounds :: PerasIgnoranceRounds
  , perasCooldownRounds :: PerasCooldownRounds
  , perasBlockMinSlots :: PerasBlockMinSlots
  , perasCertArrivalThreshold :: PerasCertArrivalThreshold
  }
  deriving (Show, Generic, NoThunks)
