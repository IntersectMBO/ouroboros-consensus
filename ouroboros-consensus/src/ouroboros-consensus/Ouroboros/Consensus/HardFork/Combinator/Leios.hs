{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Hard-fork dispatch for Leios voting.
--
-- The @'HasLeiosVoting' ('HardForkBlock' xs)@ instance lives here rather than
-- next to the class so that 'LeiosVoting' stays free of hard-fork combinator
-- machinery. Mirrors the convention used by the other
-- @HardFork.Combinator.<Domain>@ modules.
module Ouroboros.Consensus.HardFork.Combinator.Leios () where

import Data.Proxy (Proxy (..))
import Data.SOP.BasicFunctors (K (..))
import Data.SOP.Constraint (All)
import Data.SOP.Functors (Flip (..))
import Data.SOP.Strict (hcmap, hcollapse)
import qualified Data.SOP.Telescope as Telescope
import LeiosVoting (HasLeiosVoting (..))
import Ouroboros.Consensus.HardFork.Combinator.Basics
  ( HardForkBlock (..)
  , LedgerState (HardForkLedgerState)
  )
import Ouroboros.Consensus.HardFork.Combinator.Protocol ()
import Ouroboros.Consensus.HardFork.Combinator.State.Types
  ( Current (..)
  , HardForkState (..)
  )

-- | Dispatch to the active era of a hard-fork chain. Requires every era in
-- the @xs@ list to have a 'HasLeiosVoting' instance.
instance
  All HasLeiosVoting xs =>
  HasLeiosVoting (HardForkBlock xs)
  where
  getLeiosCommittee (HardForkLedgerState (HardForkState tele)) =
    hcollapse $
      hcmap
        (Proxy @HasLeiosVoting)
        (\(Current _ (Flip ls)) -> K (getLeiosCommittee ls))
        (Telescope.tip tele)
