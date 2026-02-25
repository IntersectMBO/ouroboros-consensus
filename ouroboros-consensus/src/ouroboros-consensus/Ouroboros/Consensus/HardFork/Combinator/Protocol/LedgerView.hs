{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.HardFork.Combinator.Protocol.LedgerView
  ( -- * Hard fork
    HardForkLedgerView
  , HardForkLedgerView_ (..)

    -- * Type family instances
  , Ticked (..)
  ) where

import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.Dict (Dict (..), all_NP)
import Data.SOP.Strict
import Ouroboros.Consensus.HardFork.Combinator.Abstract
import Ouroboros.Consensus.HardFork.Combinator.State.Instances ()
import Ouroboros.Consensus.HardFork.Combinator.State.Types
import Ouroboros.Consensus.Ticked
import Ouroboros.Consensus.TypeFamilyWrappers

{-------------------------------------------------------------------------------
  HardForkLedgerView
-------------------------------------------------------------------------------}

data HardForkLedgerView_ f xs = HardForkLedgerView
  { hardForkLedgerViewTransition :: !TransitionInfo
  -- ^ Information about the transition to the next era, if known
  , hardForkLedgerViewPerEra :: !(HardForkState f xs)
  -- ^ The underlying ledger view
  }

deriving instance CanHardFork xs => Show (HardForkLedgerView_ WrapLedgerView xs)

type HardForkLedgerView = HardForkLedgerView_ WrapLedgerView

{-------------------------------------------------------------------------------
  Show instance for the benefit of tests
-------------------------------------------------------------------------------}

instance (SListI xs, Show a) => Show (HardForkLedgerView_ (K a) xs) where
  show HardForkLedgerView{..} =
    case (dictPast, dictCurrent) of
      (Dict, Dict) ->
        show
          ( hardForkLedgerViewTransition
          , getHardForkState hardForkLedgerViewPerEra
          )
   where
    dictPast :: Dict (All (Compose Show (K Past))) xs
    dictPast = all_NP $ hpure Dict

    dictCurrent :: Dict (All (Compose Show (Current (K a)))) xs
    dictCurrent = all_NP $ hpure Dict
