{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Ouroboros.Consensus.HardFork.Combinator.State.Types (
    -- * Main types
    Current (..)
  , HardForkState (..)
  , Past (..)
  , sequenceHardForkState
    -- * Supporting types
  , CrossEraForecaster (..)
  , TickedTranslate
  , TransitionInfo (..)
  , Translate
  , Translate' (..)
  ) where

import           Control.Monad.Except
import           Data.SOP.BasicFunctors
import           Data.SOP.Constraint
import           Data.SOP.Strict
import           Data.SOP.Telescope (Telescope)
import qualified Data.SOP.Telescope as Telescope
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.History (Bound)
import           Ouroboros.Consensus.Ticked
import           Prelude

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Generic hard fork state
--
-- This is used both for the consensus state and the ledger state.
--
-- By using a telescope with @f ~ LedgerState@, we will keep track of 'Past'
-- information for eras before the current one:
--
-- > TZ currentByronState
-- > TZ pastByronState $ TZ currentShelleyState
-- > TZ pastByronState $ TS pastShelleyState $ TZ currentAllegraState
-- > ...
--
-- These are some intuitions on how the Telescope operations behave for this
-- type:
--
-- = @extend@
--
-- Suppose we have a telescope containing the ledger state. The "how to extend"
-- argument would take, say, the final Byron state to the initial Shelley state;
-- and "where to extend from" argument would indicate when we want to extend:
-- when the current slot number has gone past the end of the Byron era.
--
-- = @retract@
--
-- Suppose we have a telescope containing the consensus state. When we rewind
-- the consensus state, we might cross a hard fork transition point. So we first
-- /retract/ the telescope /to/ the era containing the slot number that we want
-- to rewind to, and only then call 'rewindChainDepState' on that era. Of course,
-- retraction may fail (we might not /have/ past consensus state to rewind to
-- anymore); this failure would require a choice for a particular monad @m@.
--
-- = @align@
--
-- Suppose we have one telescope containing the already-ticked ledger state, and
-- another telescope containing the consensus state. Since the ledger state has
-- already been ticked, it might have been advanced to the next era. If this
-- happens, we should then align the consensus state with the ledger state,
-- moving /it/ also to the next era, before we can do the consensus header
-- validation check. Note that in this particular example, the ledger state will
-- always be ahead of the consensus state, never behind; 'alignExtend' can be
-- used in this case.
newtype HardForkState f xs = HardForkState {
      getHardForkState :: Telescope (K Past) (Current f) xs
    }

-- | Information about the current era
data Current f blk = Current {
      currentStart :: !Bound
    , currentState :: !(f blk)
    }
  deriving (Generic)

-- | Information about a past era
data Past = Past {
      pastStart :: !Bound
    , pastEnd   :: !Bound
    }
  deriving (Eq, Show, Generic, NoThunks)

-- | Thin wrapper around 'Telescope.sequence'
sequenceHardForkState :: forall m f xs. (All Top xs, Functor m)
                      => HardForkState (m :.: f) xs -> m (HardForkState f xs)
sequenceHardForkState (HardForkState tel) =
      fmap HardForkState
    $ Telescope.sequence
    $ hmap sequenceCurrent tel
  where
    sequenceCurrent :: Current (m :.: f) a -> (m :.: Current f) a
    sequenceCurrent (Current start state) =
      Comp $ Current start <$> unComp state

{-------------------------------------------------------------------------------
  Supporting types
-------------------------------------------------------------------------------}

-- | Translate @f x@ to @g y@ across an era transition with the provided
-- 'Bound', which is the exclusive upper bound of the era of @x@ or equivalently
-- an inclusive lower bound of the era of @y@.
--
-- Typically @f@/@g@ will be ('Ticked')
-- 'Ouroboros.Consensus.Ledger.Basics.LedgerState' or
-- 'Ouroboros.Consensus.TypeFamilyWrappers.WrapChainDepState'.
newtype Translate' f g x y = Translate {
      translateWith :: Bound -> f x -> g y
    }

-- | Homogenous version of 'Translate''.
type Translate f = Translate' f f

-- | Version of 'Translate'' that is translating and "unticking" something from
-- one era to another.
--
-- This is motivated by wanting to first tick something, then translate it, and
-- then tick it again. As ticking always results in something 'Ticked', and we
-- have no generic way to "untick", we do it as part of translating.
type TickedTranslate f = Translate' (Ticked :.: f) f

-- | Forecast a @'Ticked' (view y)@ from a @state x@ across an
-- era transition.
--
-- In addition to the 'Bound' of the transition, this is also told the
-- 'SlotNo' we're constructing a forecast for. This enables the translation
-- function to take into account any scheduled changes that the final ledger
-- view in the preceding era might have.
newtype CrossEraForecaster state view x y = CrossEraForecaster {
      crossEraForecastWith ::
           Bound    -- 'Bound' of the transition (start of the new era)
        -> SlotNo   -- 'SlotNo' we're constructing a forecast for
        -> state x
        -> Except OutsideForecastRange (Ticked (view y))
    }

-- | Knowledge in a particular era of the transition to the next era
data TransitionInfo =
    -- | No transition is yet known for this era
    -- We instead record the ledger tip (which must be in /this/ era)
    --
    -- NOTE: If we are forecasting, this will be set to the slot number of the
    -- (past) ledger state in which the forecast was created. This means that
    -- when we construct an 'EpochInfo' using a 'HardForkLedgerView', the
    -- range of that 'EpochInfo' will extend a safe zone from that /past/
    -- ledger state.
    TransitionUnknown !(WithOrigin SlotNo)

    -- | Transition to the next era is known to happen at this 'EpochNo'
  | TransitionKnown !EpochNo

    -- | The transition is impossible
    --
    -- This can be due to one of two reasons:
    --
    -- * We are in the final era
    -- * This era has not actually begun yet (we are forecasting). In this case,
    --   we cannot look past the safe zone of this era and hence, by definition,
    --   the transition to the /next/ era cannot happen.
  | TransitionImpossible
  deriving (Show, Generic, NoThunks)
