{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Consensus.HardFork.Combinator.State.Types (
    -- * Main types
    Current (..)
  , HardForkState (..)
  , Past (..)
  , sequenceHardForkState
    -- * Supporting types
  , CrossEraForecaster (..)
  , TransitionInfo (..)
  , Translate (..)
  , TranslateLedgerState (..)
  , TranslateLedgerTables (..)
  , TranslateTxOut (..)
  , translateLedgerTablesWith
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
import           Ouroboros.Consensus.Ledger.Basics

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
    } deriving (Generic)

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

-- | Translate @f x@ to @f y@ across an era transition
--
-- Typically @f@ will be 'LedgerState' or 'WrapChainDepState'.
newtype Translate f x y = Translate {
      translateWith :: EpochNo -> f x -> f y
    }

-- | Forecast a @view y@ from a @state x@ across an era transition.
--
-- In addition to the 'Bound' of the transition, this is also told the
-- 'SlotNo' we're constructing a forecast for. This enables the translation
-- function to take into account any scheduled changes that the final ledger
-- view in the preceding era might have.
newtype CrossEraForecaster state view x y = CrossEraForecaster {
      crossEraForecastWith ::
           Bound    -- 'Bound' of the transition (start of the new era)
        -> SlotNo   -- 'SlotNo' we're constructing a forecast for
        -> state x EmptyMK
        -> Except OutsideForecastRange (view y)
    }

-- | Translate a 'LedgerState' across an era transition.
newtype TranslateLedgerState x y = TranslateLedgerState {
  -- | How to translate a 'LedgerState' during the era transition.
  --
  -- When translating between eras, it can be the case that values are modified,
  -- thus requiring this to be a @DiffMK@ on the return type. If no tables are
  -- populated, normally this will be filled with @emptyLedgerTables@.
  --
  -- To make a clear example, in the context of Cardano, there are currently two
  -- cases in which this is of vital importance: Byron->Shelley and
  -- Shelley->Allegra.
  --
  -- On Byron->Shelley we basically dump the whole UTxO set as insertions
  -- because the LedgerTables only exist for Shelley blocks.
  --
  -- On Shelley->Allegra, there were a bunch of UTxOs that were moved around,
  -- related to the AVVMs. In particular they were deleted and included in the
  -- reserves. See the code that performs the translation Shelley->Allegra for
  -- more information.
    translateLedgerStateWith ::
         EpochNo
      -> LedgerState x EmptyMK
      -> LedgerState y DiffMK
  }

-- | Transate a 'LedgerTables' across an era transition.
data TranslateLedgerTables x y =
  TranslateLedgerTables { getTranslateLedgerTables :: forall mk. LedgerTables (LedgerState x) mk -> LedgerTables (LedgerState y) mk }
  -- TranslateLedgerTables {
  --   -- | Translate a 'TxIn' across an era transition.
  --   --
  --   -- See 'translateLedgerTablesWith'.
  --   translateTxInWith  :: !(TxIn (LedgerState x) -> TxIn (LedgerState y))

  --   -- | Translate a 'TxOut' across an era transition.
  --   --
  --   -- See 'translateLedgerTablesWith'.
  -- , translateTxOutWith :: !(TxOut (LedgerState x) -> TxOut (LedgerState y))
  -- }

newtype TranslateTxOut x y = TranslateTxOut (TxOut (LedgerState x) -> TxOut (LedgerState y))

-- | Translate a 'LedgerTables' across an era transition.
--
-- To translate 'LedgerTable's, it's sufficient to know how to translate
-- 'TxIn's and 'TxOut's. Use 'translateLedgerTablesWith' to translate
-- 'LedgerTable's using 'translateTxInWith' and 'translateTxOutWith'.
--
-- This is a rather technical subtlety. When performing a ledger state
-- translation, the provided input ledger state will be initially populated with
-- a @emptyLedgerTables@. This step is required so that the operation provided
-- to 'Telescope.extend' is an automorphism.
--
-- If we only extend by one era, this function is a no-op, as the input will be
-- empty ledger states. However, if we extend across multiple eras, previous
-- eras might populate tables thus creating values that now need to be
-- translated to newer eras. This function fills that hole and allows us to
-- promote tables from one era into tables from the next era.
--
-- NOTE: If either 'translateTxInWith' or 'translateTxOutWith' is a no-op ('id'),
-- mapping over the diff with those functions is also equivalent to a
-- no-op. However, we are still traversing the map in both cases.
--
-- NOTE: This function is only used on ticking, to prepend differences from
-- previous eras, so it will be called only when crossing era boundaries,
-- therefore the translation won't be equivalent to 'id'.
translateLedgerTablesWith ::
     TranslateLedgerTables x y
  -> LedgerTables (LedgerState x) DiffMK
  -> LedgerTables (LedgerState y) DiffMK
translateLedgerTablesWith (TranslateLedgerTables f) = f

  -- upgradeLedgerTables
  --   ( mapKeysMK (translateTxInWith f)
  --   . mapMK (translateTxOutWith f)
  --   )

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
