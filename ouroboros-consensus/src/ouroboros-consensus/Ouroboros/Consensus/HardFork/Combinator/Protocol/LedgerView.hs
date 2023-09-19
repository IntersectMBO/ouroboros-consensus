{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Ouroboros.Consensus.HardFork.Combinator.Protocol.LedgerView (
    -- * Hard fork
    HardForkLedgerView
  , HardForkLedgerView_
    -- * Type family instances
  , Ticked (..)
  ) where

import           Data.Kind (Type)
import           Data.SOP.BasicFunctors
import           Data.SOP.Constraint
import           Data.SOP.Dict
import           Data.SOP.Strict
import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.State.Instances ()
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.TypeFamilyWrappers

{-------------------------------------------------------------------------------
  HardForkLedgerView
-------------------------------------------------------------------------------}

data HardForkLedgerView_ (f :: Type -> Type) (xs :: [Type])

deriving instance CanHardFork xs => Show (HardForkLedgerView_ WrapLedgerView xs)

type HardForkLedgerView = HardForkLedgerView_ WrapLedgerView

{-------------------------------------------------------------------------------
  Ticked
-------------------------------------------------------------------------------}

data instance Ticked (HardForkLedgerView_ f xs) = TickedHardForkLedgerView {
      -- | Information about the transition to the next era, if known
      tickedHardForkLedgerViewTransition :: !TransitionInfo
      -- | The underlying ledger view
    , tickedHardForkLedgerViewPerEra     :: !(HardForkState (Ticked :.: f) xs)
    }

{-------------------------------------------------------------------------------
  Show instance for the benefit of tests
-------------------------------------------------------------------------------}

instance (SListI xs, Show (Ticked a)) => Show (Ticked (HardForkLedgerView_ (K a) xs)) where
  show TickedHardForkLedgerView{..} =
      case (dictPast, dictCurrent) of
        (Dict, Dict) -> show (
            tickedHardForkLedgerViewTransition
          , getHardForkState tickedHardForkLedgerViewPerEra
          )
    where
      dictPast :: Dict (All (Compose Show (K Past))) xs
      dictPast = all_NP $ hpure Dict

      dictCurrent :: Dict (All (Compose Show (Current (Ticked :.: K a)))) xs
      dictCurrent = all_NP $ hpure dictCurrentOne

dictCurrentOne :: forall blk a. Show (Ticked a)
               => Dict (Compose Show (Current (Ticked :.: K a))) blk
dictCurrentOne = Dict
