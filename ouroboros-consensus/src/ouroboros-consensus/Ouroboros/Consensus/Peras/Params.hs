{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

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
  , maxRollbackWeight
  , PerasQuorumWeightThreshold (..)
  , PerasQuorumWeightThresholdSafetyMargin (..)
  , PerasBootstrapRound (..)

    -- * Protocol parameters bundle
  , PerasParams (..)
  , castPerasParams
  , mkPerasParams

    -- * Era-dependent default values
  , dijkstraPerasRoundLength
  , dijkstraPerasMaxCertSize

    -- * 'PerasEnabled' wrapper
  , PerasEnabled
  , pattern PerasEnabled
  , pattern NoPerasEnabled
  , PerasEnabledT (..)
  , fromPerasEnabled
  , perasEnabledToMaybe

    -- * Convenience re-exports
  , Committee.TargetCommitteeSize (..)

    -- * For testing and debugging purposes only
  , defaultPerasParams
  )
where

import Cardano.Binary
  ( FromCBOR (..)
  , ToCBOR (..)
  , decodeListLenOf
  , encodeListLen
  )
import Cardano.Ledger.BaseTypes
  ( ActiveSlotCoeff
  , BoundedRational (..)
  , activeSlotVal
  , mkActiveSlotCoeff
  )
import Cardano.Ledger.BaseTypes.NonZero (NonZero (..), unsafeNonZero)
import Cardano.Slotting.Slot (SlotInterval (..))
import Control.Monad (ap, liftM)
import Control.Monad.Trans.Class
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Ratio ((%))
import Data.Semigroup (Sum (..))
import Data.Typeable (Typeable)
import Data.Word (Word16, Word32, Word64)
import GHC.Generics (Generic)
import qualified Ouroboros.Consensus.Committee.Types as Committee
import Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import Ouroboros.Consensus.Peras.Protocol (PerasLedgerView (..))
import Ouroboros.Consensus.Peras.Types (PerasCertSize (..))
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
  deriving newtype (Enum, Eq, Ord, NoThunks, Condense, FromCBOR, ToCBOR)

-- | Minimum number of rounds to wait before voting again after a cooldown
-- period starts.
newtype PerasCooldownRounds
  = PerasCooldownRounds {unPerasCooldownRounds :: Word64}
  deriving Show via Quiet PerasCooldownRounds
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, NoThunks, Condense, FromCBOR, ToCBOR)

-- | Minimum age in slots of a block before it can be voted for in order to get
-- a boost.
newtype PerasBlockMinSlots
  = PerasBlockMinSlots {unPerasBlockMinSlots :: Word64}
  deriving Show via Quiet PerasBlockMinSlots
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, NoThunks, Condense, FromCBOR, ToCBOR)

-- | Maximum age for a certificate to be included in a block, in rounds.
newtype PerasCertMaxRounds
  = PerasCertMaxRounds {unPerasCertMaxRounds :: Word64}
  deriving Show via Quiet PerasCertMaxRounds
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, NoThunks, Condense, FromCBOR, ToCBOR)

-- | Maximum number of slots to wait for after the start of a round to consider
-- a certificate valid for voting.
newtype PerasCertArrivalThreshold
  = PerasCertArrivalThreshold {unPerasCertArrivalThreshold :: Word64}
  deriving Show via Quiet PerasCertArrivalThreshold
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, NoThunks, Condense, FromCBOR, ToCBOR)

-- | Length of a Peras round in slots.
newtype PerasRoundLength
  = PerasRoundLength {unPerasRoundLength :: Word64}
  deriving Show via Quiet PerasRoundLength
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, NoThunks, Condense, FromCBOR, ToCBOR)

-- | Weight assigned to a block when boosted by a Peras certificate.
newtype PerasWeight
  = PerasWeight {unPerasWeight :: Word64}
  deriving Show via Quiet PerasWeight
  deriving stock Generic
  deriving newtype (Enum, Eq, Ord, NoThunks, Condense, FromCBOR, ToCBOR)

deriving via Sum Word64 instance Semigroup PerasWeight
deriving via Sum Word64 instance Monoid PerasWeight

-- | The maximum amount of weight we can roll back.
maxRollbackWeight :: SecurityParam -> PerasWeight
maxRollbackWeight = PerasWeight . unNonZero . maxRollbacks

-- | Total vote weight needed to forge a Peras certificate.
newtype PerasQuorumWeightThreshold
  = PerasQuorumWeightThreshold {unPerasQuorumWeightThreshold :: Rational}
  deriving Show via Quiet PerasQuorumWeightThreshold
  deriving stock Generic
  deriving newtype (Eq, Ord, NoThunks, Condense, FromCBOR, ToCBOR)

-- | Safety margin needed on top of the quorum vote weight threshold.
--
-- NOTE: this is needed to account for an extremely unlikely local sortition
-- where not enough honest non-persistent parties decide to vote in a round.
-- This mostly depend on the expected size of the voting committee.
newtype PerasQuorumWeightThresholdSafetyMargin
  = PerasQuorumWeightThresholdSafetyMargin {unPerasQuorumWeightThresholdSafetyMargin :: Rational}
  deriving Show via Quiet PerasQuorumWeightThresholdSafetyMargin
  deriving stock Generic
  deriving newtype (Eq, Ord, NoThunks, Condense, FromCBOR, ToCBOR)

-- | Peras bootstrap round.
--
-- NOTE: this is used to resynchronize the round of the latest certificate on
-- chain across nodes should something go wrong.
newtype PerasBootstrapRound
  = PerasBootstrapRound {unPerasBootstrapRound :: Word64}
  deriving Show via Quiet PerasBootstrapRound
  deriving stock Generic
  deriving newtype (Eq, Ord, NoThunks, Condense, FromCBOR, ToCBOR)

-- * Protocol parameters bundle

-- | Peras protocol parameters.
--
-- These are documented in the section 2.1 of the Peras design report:
-- https://tweag.github.io/cardano-peras/peras-design.pdf#section.2.1
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
  , perasBootstrapRound :: !(StrictMaybe PerasBootstrapRound)
  }
  deriving (Show, Eq, Generic, NoThunks)

instance Typeable blk => FromCBOR (PerasParams blk) where
  fromCBOR = do
    decodeListLenOf 10
    PerasParams
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

instance Typeable blk => ToCBOR (PerasParams blk) where
  toCBOR params =
    encodeListLen 10
      <> toCBOR (perasIgnoranceRounds params)
      <> toCBOR (perasCooldownRounds params)
      <> toCBOR (perasBlockMinSlots params)
      <> toCBOR (perasCertMaxRounds params)
      <> toCBOR (perasCertArrivalThreshold params)
      <> toCBOR (perasWeight params)
      <> toCBOR (perasQuorumWeightThreshold params)
      <> toCBOR (perasQuorumWeightThresholdSafetyMargin params)
      <> toCBOR (perasTargetCommitteeSize params)
      <> toCBOR (perasBootstrapRound params)

-- | Retag a 'PerasParams' with a new phantom @blk@ type.
castPerasParams :: forall blk' blk. PerasParams blk -> PerasParams blk'
castPerasParams = coerce

-- | Construct 'PerasParams' from the security parameter, active slot
-- coefficient, and protocol parameters in a 'PerasLedgerView'.
--
-- NOTE: see Section 2.1 of the Peras design document for more details on how
-- these parameters are derived:
--
-- https://tweag.github.io/cardano-peras/peras-design.pdf
mkPerasParams ::
  SecurityParam ->
  ActiveSlotCoeff ->
  PerasLedgerView ->
  PerasParams blk
mkPerasParams securityParam activeSlotCoeff plv =
  PerasParams
    { perasIgnoranceRounds =
        PerasIgnoranceRounds
          ignoranceRounds
    , perasCooldownRounds =
        PerasCooldownRounds
          cooldownRounds
    , perasBlockMinSlots =
        PerasBlockMinSlots
          blockMinSlots
    , perasCertMaxRounds =
        PerasCertMaxRounds
          certMaxRounds
    , perasCertArrivalThreshold =
        PerasCertArrivalThreshold
          certArrivalThreshold
    , perasWeight =
        PerasWeight
          weight
    , perasQuorumWeightThreshold =
        PerasQuorumWeightThreshold
          quorumWeightThreshold
    , perasQuorumWeightThresholdSafetyMargin =
        PerasQuorumWeightThresholdSafetyMargin
          quorumWeightThresholdSafetyMargin
    , perasTargetCommitteeSize =
        Committee.TargetCommitteeSize
          targetCommitteeSize
    , perasBootstrapRound =
        PerasBootstrapRound
          <$> bootstrapRound
    }
 where
  k :: Word64
  k = unNonZero (maxRollbacks securityParam)

  -- Chain quality window size (k/f)
  tCQ :: Rational
  tCQ = toRational k / unboundRational (activeSlotVal activeSlotCoeff)

  -- Chain growth window size (3k/f)
  tCP :: Rational
  tCP = 3 * (toRational k / unboundRational (activeSlotVal activeSlotCoeff))

  -- Length of a Peras round
  roundLength :: Rational
  roundLength = toRational (unPerasRoundLength dijkstraPerasRoundLength)

  -- Cert boost weight
  weight :: Word64
  weight = fromIntegral @Word16 @Word64 (plvCertBoost plv)

  -- Healing time (in slots)
  tHeal :: Rational
  tHeal =
    unboundRational (plvHealingFactor plv)
      * (toRational (plvCertBoost plv))
      / unboundRational (activeSlotVal activeSlotCoeff)

  ignoranceRounds :: Word64
  ignoranceRounds = certMaxRounds

  cooldownRounds :: Word64
  cooldownRounds = ceiling ((tHeal + tCQ + tCP) / roundLength) + 1

  blockMinSlots :: Word64
  blockMinSlots = fromIntegral @Word32 @Word64 (unSlotInterval (plvMinCandidateBlockAge plv))

  certMaxRounds :: Word64
  certMaxRounds = ceiling ((tHeal + tCQ) / roundLength)

  certArrivalThreshold :: Word64
  certArrivalThreshold = ceiling (roundLength / 3)

  quorumWeightThreshold :: Rational
  quorumWeightThreshold = 3 % 4

  quorumWeightThresholdSafetyMargin :: Rational
  quorumWeightThresholdSafetyMargin = unboundRational (plvQuorumThresholdSafetyMargin plv)

  targetCommitteeSize :: Word64
  targetCommitteeSize = fromIntegral @Word16 @Word64 (plvTargetCommitteeSize plv)

  bootstrapRound :: StrictMaybe Word64
  bootstrapRound = fromIntegral @Word32 @Word64 <$> plvBootstrapRound plv

-- | Instantiate default Peras protocol parameters.
--
-- NOTE: this is only useful for testing and debugging purposes. In production,
-- these are derived from the Peras protocol parameters stored in the ledger.
defaultPerasParams :: PerasParams blk
defaultPerasParams =
  mkPerasParams
    (SecurityParam (unsafeNonZero 2160))
    (mkActiveSlotCoeff (fromMaybe (error "impossible") (boundRational (1 % 20))))
    MkPerasLedgerView
      { plvMinCandidateBlockAge = SlotInterval 30
      , plvHealingFactor = fromMaybe (error "impossible") (boundRational 1)
      , plvCertBoost = 15
      , plvTargetCommitteeSize = 900
      , plvBootstrapRound = SNothing
      , plvQuorumThresholdSafetyMargin = fromMaybe (error "impossible") (boundRational (2 % 100))
      }

-- * Era-dependent default values

-- | Default value for 'PerasRoundLength' in the Dijkstra eras.
dijkstraPerasRoundLength :: PerasRoundLength
dijkstraPerasRoundLength = PerasRoundLength 90

-- | Maximum size of a Peras certificate in bytes, for the Dijkstra eras.
-- Maximum block size is 90_112 bytes in mainnet as of 2026-08-06. Taking half that
-- value still allows for extreme leeway: wFA^LS certificates would only reach that
-- size if we have around 800 non-persistent seats in the voting committee.
dijkstraPerasMaxCertSize :: PerasCertSize
dijkstraPerasMaxCertSize = PerasCertSize 40_000

-- * 'PerasEnabled' wrapper

-- | A marker for Peras-specific values that are not present in all eras
newtype PerasEnabled a = MkPerasEnabled (Maybe a)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NoThunks
  deriving newtype (Functor, Applicative, Monad, FromCBOR, ToCBOR)

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
