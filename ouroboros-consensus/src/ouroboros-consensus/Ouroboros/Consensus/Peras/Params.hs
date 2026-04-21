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
  , PerasCertMaxRounds (..)
  , PerasCertArrivalThreshold (..)
  , PerasRoundLength (..)
  , PerasWeight (..)
  , PerasQuorumStakeThreshold (..)
  , PerasQuorumStakeThresholdSafetyMargin (..)

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

-- | Maximum age for a certificate to be included in a block, in rounds.
newtype PerasCertMaxRounds
  = PerasCertMaxRounds {unPerasCertMaxRounds :: Word64}
  deriving Show via Quiet PerasCertMaxRounds
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

-- | Safety margin needed on top of the quorum stake threshold.
--
-- NOTE: this is needed to account for an extremely unlikely local sortition
-- where not enough honest non-persistent parties decide to vote in a round.
-- This mostly depend on the expected size of the voting committee.
newtype PerasQuorumStakeThresholdSafetyMargin
  = PerasQuorumStakeThresholdSafetyMargin {unPerasQuorumStakeThresholdSafetyMargin :: Rational}
  deriving Show via Quiet PerasQuorumStakeThresholdSafetyMargin
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
  { perasIgnoranceRounds :: !PerasIgnoranceRounds
  , perasCooldownRounds :: !PerasCooldownRounds
  , perasBlockMinSlots :: !PerasBlockMinSlots
  , perasCertMaxRounds :: !PerasCertMaxRounds
  , perasCertArrivalThreshold :: !PerasCertArrivalThreshold
  , perasRoundLength :: !PerasRoundLength
  , perasWeight :: !PerasWeight
  , perasQuorumStakeThreshold :: !PerasQuorumStakeThreshold
  , perasQuorumStakeThresholdSafetyMargin :: !PerasQuorumStakeThresholdSafetyMargin
  }
  deriving (Show, Eq, Generic, NoThunks)

-- | Instantiate default Peras protocol parameters.
--
-- NOTE: in the future this will depend on a concrete 'BlockConfig'.
mkPerasParams :: PerasParams
mkPerasParams =
  -- Many of these parameters are provided with sensible default values for now,
  -- waiting for a final decision (in a future stage of the project) on the
  -- exact values to use. See https://github.com/tweag/cardano-peras/issues/97.
  --
  -- We set tentatively T_heal to 2B/asc = 600 slots, as the CIP suggests a
  -- bigO(B/asc) for that value so that sufficiently many blocks are produced to
  -- overcome an adversarially boosted block.
  --
  -- We also set tentatively perasCertArrivalThreshold (= X in the formal spec)
  -- to 30 slots (it must be strictly smaller than perasRoundLength)
  -- See https://github.com/tweag/cardano-peras/issues/88 and
  -- https://github.com/tweag/cardano-peras/issues/99 for more information on
  -- this parameter.
  --
  -- We also have T_cp = 129_600 and T_cq = 43_200 as per the design document
  PerasParams
    { -- ceil(T_heal + T_cq) / perasRoundLength) as per the design document
      perasIgnoranceRounds =
        PerasIgnoranceRounds 487
    , -- ceil(T_heal + T_cq + T_cp) / perasRoundLength) + 1 as per the design document
      perasCooldownRounds =
        PerasCooldownRounds 1928
    , -- must be between 30 and 900 as per the design document
      perasBlockMinSlots =
        PerasBlockMinSlots 90
    , -- equal to perasIgnoranceRounds as per the design document
      perasCertMaxRounds =
        PerasCertMaxRounds 487
    , perasCertArrivalThreshold =
        PerasCertArrivalThreshold 30
    , perasRoundLength =
        PerasRoundLength 90
    , perasWeight =
        PerasWeight 15
    , perasQuorumStakeThreshold =
        PerasQuorumStakeThreshold (3 / 4)
    , perasQuorumStakeThresholdSafetyMargin =
        PerasQuorumStakeThresholdSafetyMargin (2 / 100)
    }
