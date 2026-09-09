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
import Data.SOP.Strict (hcmap, hcollapse, hczipWith)
import qualified Data.SOP.Telescope as Telescope
import LeiosVoting (HasLeiosVoting (..))
import Ouroboros.Consensus.HardFork.Combinator.Abstract (CanHardFork)
import Ouroboros.Consensus.HardFork.Combinator.Basics
  ( HardForkBlock (..)
  , LedgerState (HardForkLedgerState)
  , distribLedgerConfig
  )
import Ouroboros.Consensus.HardFork.Combinator.Protocol ()
import Ouroboros.Consensus.HardFork.Combinator.Protocol.LedgerView
  ( HardForkLedgerView_ (HardForkLedgerView)
  )
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import Ouroboros.Consensus.HardFork.Combinator.State.Types
  ( Current (..)
  , HardForkState (..)
  )
import Ouroboros.Consensus.TypeFamilyWrappers
  ( WrapLedgerConfig (..)
  , WrapLedgerView (..)
  )

-- | Dispatch to the active era of a hard-fork chain. Requires every era in
-- the @xs@ list to have a 'HasLeiosVoting' instance.
instance
-- 'CanHardFork' is here only for 'getMinCertificationGap', which has to
-- complete the combinator's partial per-era configs. See the note there.
  (All HasLeiosVoting xs, CanHardFork xs) =>
  HasLeiosVoting (HardForkBlock xs)
  where
  getLeiosCommittee (HardForkLedgerState (HardForkState tele)) =
    hcollapse $
      hcmap
        (Proxy @HasLeiosVoting)
        (\(Current _ (Flip ls)) -> K (getLeiosCommittee ls))
        (Telescope.tip tele)

  getCurrentThreshold (HardForkLedgerState (HardForkState tele)) =
    hcollapse $
      hcmap
        (Proxy @HasLeiosVoting)
        (\(Current _ (Flip ls)) -> K (getCurrentThreshold ls))
        (Telescope.tip tele)

  -- As above, but dispatching on the per-era ledger /view/ the protocol layer
  -- carries instead of the ledger state.
  getLeiosCommitteeFromView _ (HardForkLedgerView _transition (HardForkState tele)) =
    hcollapse $
      hcmap
        (Proxy @HasLeiosVoting)
        (\(Current _ wlv) -> K (getLeiosCommitteeFromView (eraProxy wlv) (unwrapLedgerView wlv)))
        (Telescope.tip tele)
   where
    -- The wrapper is a newtype, hence injective, so matching on it is what
    -- recovers the era that the non-injective 'LedgerView' loses.
    eraProxy :: WrapLedgerView blk -> Proxy blk
    eraProxy _ = Proxy

  -- Unlike the other two, this one needs the era's config as well as its
  -- state, and the combinator only stores partial configs -- hence completing
  -- them against an 'EpochInfo' reconstructed from the very state we are
  -- dispatching on.
  getMinCertificationGap cfg (HardForkLedgerState hfState@(HardForkState tele)) =
    hcollapse $
      hczipWith
        (Proxy @HasLeiosVoting)
        (\(WrapLedgerConfig cfg') (Current _ (Flip ls)) -> K (getMinCertificationGap cfg' ls))
        (distribLedgerConfig (State.epochInfoLedger cfg hfState) cfg)
        (Telescope.tip tele)
