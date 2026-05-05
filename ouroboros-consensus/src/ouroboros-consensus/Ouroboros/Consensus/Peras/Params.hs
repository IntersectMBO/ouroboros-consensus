{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
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
  , PerasQuorumWeightThreshold (..)
  , PerasQuorumWeightThresholdSafetyMargin (..)

    -- * Protocol parameters bundle
  , PerasParams (..)
  , retagPerasParams
  , defaultPerasParams

    -- * Era-dependent default values
  , dijkstraPerasRoundLength

    -- * 'PerasEnabled' wrapper
  , PerasEnabled
  , pattern PerasEnabled
  , pattern NoPerasEnabled
  , PerasEnabledT (..)
  , fromPerasEnabled
  , perasEnabledToMaybe

    -- * Convenience re-exports
  , Committee.TargetCommitteeSize (..)
  )
where

import Cardano.Binary (FromCBOR, ToCBOR)
import Codec.Serialise (Serialise (..))
import Control.Monad (ap, liftM)
import Control.Monad.Trans.Class
import Data.Coerce (coerce)
import Data.Semigroup (Sum (..))
import Data.Word (Word64)
import GHC.Generics (Generic)
import qualified Ouroboros.Consensus.Committee.Types as Committee
import Ouroboros.Consensus.Util.Condense (Condense (..))
import Ouroboros.Consensus.Util.IOLike (NoThunks)
import Quiet (Quiet (..))

-- * Protocol parameters

-- | Number of rounds for which to ignore certificates after entering a
-- cooldown period.
newtype PerasIgnoranceRounds
  = PerasIgnoranceRounds {unPerasIgnoranceRounds :: Word64}
  deriving Show via Quiet PerasIgnoranceRounds
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, NoThunks, Condense, Serialise)

-- | Minimum number of rounds to wait before voting again after a cooldown
-- period starts.
newtype PerasCooldownRounds
  = PerasCooldownRounds {unPerasCooldownRounds :: Word64}
  deriving Show via Quiet PerasCooldownRounds
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, NoThunks, Condense, Serialise)

-- | Minimum age in slots of a block before it can be voted for in order to get
-- a boost.
newtype PerasBlockMinSlots
  = PerasBlockMinSlots {unPerasBlockMinSlots :: Word64}
  deriving Show via Quiet PerasBlockMinSlots
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, NoThunks, Condense, Serialise)

-- | Maximum age for a certificate to be included in a block, in rounds.
newtype PerasCertMaxRounds
  = PerasCertMaxRounds {unPerasCertMaxRounds :: Word64}
  deriving Show via Quiet PerasCertMaxRounds
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, NoThunks, Condense, Serialise)

-- | Maximum number of slots to wait for after the start of a round to consider
-- a certificate valid for voting.
newtype PerasCertArrivalThreshold
  = PerasCertArrivalThreshold {unPerasCertArrivalThreshold :: Word64}
  deriving Show via Quiet PerasCertArrivalThreshold
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, NoThunks, Condense, Serialise)

-- | Length of a Peras round in slots.
newtype PerasRoundLength
  = PerasRoundLength {unPerasRoundLength :: Word64}
  deriving Show via Quiet PerasRoundLength
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, NoThunks, Condense, Serialise)

-- | Weight assigned to a block when boosted by a Peras certificate.
newtype PerasWeight
  = PerasWeight {unPerasWeight :: Word64}
  deriving Show via Quiet PerasWeight
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, NoThunks, Condense, Serialise)

deriving via Sum Word64 instance Semigroup PerasWeight
deriving via Sum Word64 instance Monoid PerasWeight

-- | Total vote weight needed to forge a Peras certificate.
newtype PerasQuorumWeightThreshold
  = PerasQuorumWeightThreshold {unPerasQuorumWeightThreshold :: Rational}
  deriving Show via Quiet PerasQuorumWeightThreshold
  deriving stock Generic
  deriving newtype (Eq, Ord, NoThunks, Condense, Serialise)

-- | Safety margin needed on top of the quorum vote weight threshold.
--
-- NOTE: this is needed to account for an extremely unlikely local sortition
-- where not enough honest non-persistent parties decide to vote in a round.
-- This mostly depend on the expected size of the voting committee.
newtype PerasQuorumWeightThresholdSafetyMargin
  = PerasQuorumWeightThresholdSafetyMargin {unPerasQuorumWeightThresholdSafetyMargin :: Rational}
  deriving Show via Quiet PerasQuorumWeightThresholdSafetyMargin
  deriving stock Generic
  deriving newtype (Eq, Ord, NoThunks, Condense, Serialise)

-- * Protocol parameters bundle

-- | Peras protocol parameters.
--
-- These are documented in the section 2.1 of the Peras design report:
-- https://tweag.github.io/cardano-peras/peras-design.pdf#section.2.1
--
-- TODO: make fields strict when we have concrete default values for them.
data PerasParams blk = PerasParams
  { perasIgnoranceRounds :: !PerasIgnoranceRounds
  , perasCooldownRounds :: !PerasCooldownRounds
  , perasBlockMinSlots :: !PerasBlockMinSlots
  , perasCertMaxRounds :: !PerasCertMaxRounds
  , perasCertArrivalThreshold :: !PerasCertArrivalThreshold
  , perasWeight :: !PerasWeight
  , perasQuorumWeightThreshold :: !PerasQuorumWeightThreshold
  , perasQuorumWeightThresholdSafetyMargin :: !PerasQuorumWeightThresholdSafetyMargin
  , perasTargetCommitteeSize :: !Committee.TargetCommitteeSize
  }
  deriving (Show, Eq, Generic, NoThunks, Serialise)

-- | Retag a 'PerasParams' to change its phantom type tag.
retagPerasParams :: forall blk' blk. PerasParams blk -> PerasParams blk'
retagPerasParams = coerce

-- | Instantiate default Peras protocol parameters.
--
-- NOTE: in the future this will depend on a concrete 'BlockConfig'.
defaultPerasParams :: PerasParams blk
defaultPerasParams =
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
    , perasWeight =
        PerasWeight 15
    , perasQuorumWeightThreshold =
        PerasQuorumWeightThreshold (3 / 4)
    , perasQuorumWeightThresholdSafetyMargin =
        PerasQuorumWeightThresholdSafetyMargin (2 / 100)
    , perasTargetCommitteeSize =
        Committee.TargetCommitteeSize 800
    }

-- * Era-dependent default values

-- | Default value for 'PerasRoundLength' in the Dijkstra eras.
dijkstraPerasRoundLength :: PerasEnabled PerasRoundLength
dijkstraPerasRoundLength = PerasEnabled (PerasRoundLength 90)

-- * 'PerasEnabled' wrapper

-- | A marker for Peras-specific values that are not present in all eras
newtype PerasEnabled a = MkPerasEnabled (Maybe a)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NoThunks
  deriving newtype (Functor, Applicative, Monad, FromCBOR, ToCBOR, Serialise)

pattern PerasEnabled :: a -> PerasEnabled a
pattern PerasEnabled x <- MkPerasEnabled (Just !x)
 where
  PerasEnabled !x = MkPerasEnabled (Just x)

pattern NoPerasEnabled :: PerasEnabled a
pattern NoPerasEnabled = MkPerasEnabled Nothing

{-# COMPLETE PerasEnabled, NoPerasEnabled #-}

-- | A 'fromMaybe'-like eliminator for 'PerasEnabled'
fromPerasEnabled :: a -> PerasEnabled a -> a
fromPerasEnabled defaultValue = \case
  NoPerasEnabled -> defaultValue
  PerasEnabled value -> value

-- | Return the underlying 'Maybe' of a 'PerasEnabled' value.
perasEnabledToMaybe :: PerasEnabled a -> Maybe a
perasEnabledToMaybe = \case
  NoPerasEnabled -> Nothing
  PerasEnabled value -> Just value

-- | A 'MaybeT'-like monad transformer.
--
--   Used solely for the Peras-related hard fork combinator queries,
--   see 'Ouroboros.Consensus.HardFork.History.Qry'.
newtype PerasEnabledT m a = PerasEnabledT
  { runPerasEnabledT :: m (PerasEnabled a)
  }
  deriving stock Functor

instance (Functor m, Monad m) => Applicative (PerasEnabledT m) where
  pure = PerasEnabledT . pure . PerasEnabled
  (<*>) = ap

instance Monad m => Monad (PerasEnabledT m) where
  x >>= f = PerasEnabledT $ do
    v <- runPerasEnabledT x
    case v of
      NoPerasEnabled -> pure NoPerasEnabled
      PerasEnabled y -> runPerasEnabledT (f y)

instance MonadTrans PerasEnabledT where
  lift = PerasEnabledT . liftM PerasEnabled
