{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Peras protocol parameters
module Ouroboros.Consensus.Peras.Params
  ( -- * Protocol parameters
    PerasIgnoranceRounds (..)
  , PerasCooldownRounds (..)
  , PerasBlockMinSlots (..)
  , PerasCertArrivalThreshold (..)
  , PerasRoundLength (..)
  , PerasWeight (..)
  , PerasQuorumStakeThreshold (..)

    -- * Protocol parameters bundle
  , PerasParams (..)
  , mkPerasParams
  )
where

import Data.Semigroup (Sum (..))
import Data.Word (Word64)
import GHC.Generics (Generic)
import Ouroboros.Consensus.Util.Condense (Condense (..))
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
  deriving newtype (Enum, Eq, Ord, NoThunks, Condense)

-- | Minimum number of rounds to wait before voting again after a cooldown
-- period starts.
newtype PerasCooldownRounds
  = PerasCooldownRounds {unPerasCooldownRounds :: Word64}
  deriving Show via Quiet PerasCooldownRounds
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, NoThunks, Condense)

-- | Minimum age in slots of a block before it can be voted for in order to get
-- a boost.
newtype PerasBlockMinSlots
  = PerasBlockMinSlots {unPerasBlockMinSlots :: Word64}
  deriving Show via Quiet PerasBlockMinSlots
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, NoThunks, Condense)

-- | Maximum number of slots to wait for after the start of a round to consider
-- a certificate valid for voting.
newtype PerasCertArrivalThreshold
  = PerasCertArrivalThreshold {unPerasCertArrivalThreshold :: Word64}
  deriving Show via Quiet PerasCertArrivalThreshold
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, NoThunks, Condense)

-- | Length of a Peras round in slots.
newtype PerasRoundLength
  = PerasRoundLength {unPerasRoundLength :: Word64}
  deriving Show via Quiet PerasRoundLength
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, NoThunks)

-- | Weight assigned to a block when boosted by a Peras certificate.
newtype PerasWeight
  = PerasWeight {unPerasWeight :: Word64}
  deriving Show via Quiet PerasWeight
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, NoThunks, Condense)

deriving via Sum Word64 instance Semigroup PerasWeight
deriving via Sum Word64 instance Monoid PerasWeight

-- | Total stake needed to forge a Peras certificate.
newtype PerasQuorumStakeThreshold
  = PerasQuorumStakeThreshold {unPerasQuorumStakeThreshold :: Rational}
  deriving Show via Quiet PerasQuorumStakeThreshold
  deriving stock Generic
  deriving newtype (Eq, Ord, NoThunks, Condense)

{-------------------------------------------------------------------------------
  Protocol parameters bundle
-------------------------------------------------------------------------------}

-- | Peras protocol parameters.
--
-- These are documented in the section 2.1 of the Peras design report:
-- https://tweag.github.io/cardano-peras/peras-design.pdf#section.2.1
--
-- TODO: make fields strict when we have concrete default values for them.
data PerasParams = PerasParams
  { perasIgnoranceRounds :: PerasIgnoranceRounds
  , perasCooldownRounds :: PerasCooldownRounds
  , perasBlockMinSlots :: PerasBlockMinSlots
  , perasCertArrivalThreshold :: PerasCertArrivalThreshold
  , perasRoundLength :: !PerasRoundLength
  , perasWeight :: !PerasWeight
  , perasQuorumStakeThreshold :: !PerasQuorumStakeThreshold
  }
  deriving (Show, Eq, Generic, NoThunks)

-- | Instantiate default Peras protocol parameters.
--
-- NOTE: in the future this will depend on a concrete 'BlockConfig'.
mkPerasParams :: PerasParams
mkPerasParams =
  PerasParams
    { perasIgnoranceRounds =
        error "perasIgnoranceRounds: not yet defined"
    , perasCooldownRounds =
        error "perasCooldownRounds: not yet defined"
    , perasBlockMinSlots =
        error "perasBlockMinSlots: not yet defined"
    , perasCertArrivalThreshold =
        error "perasCertArrivalThreshold: not yet defined"
    , perasRoundLength =
        PerasRoundLength 90
    , perasWeight =
        PerasWeight 15
    , perasQuorumStakeThreshold =
        PerasQuorumStakeThreshold (3 / 4)
    }
