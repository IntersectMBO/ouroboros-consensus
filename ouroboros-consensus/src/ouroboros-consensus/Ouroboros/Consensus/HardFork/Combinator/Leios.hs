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
import Data.SOP.BasicFunctors (I (..), K (..))
import Data.SOP.Constraint (All)
import Data.Functor.Product (Product (..))
import Data.SOP.Functors (Flip (..))
import Data.SOP.Index (Index, hcimap, injectNS)
import qualified Data.SOP.Match as Match
import Data.SOP.Strict (hcmap, hcollapse)
import qualified Data.SOP.Telescope as Telescope
import LeiosDemoTypes (VerificationError)
import LeiosVoting (HasLeiosVoting (..))
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
  ( OneEraBlock (..)
  )
import Ouroboros.Consensus.HardFork.Combinator.Basics
  ( HardForkBlock (..)
  , LedgerState (HardForkLedgerState)
  )
import Ouroboros.Consensus.HardFork.Combinator.Protocol
  ( HardForkChainDepState
  )
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import Ouroboros.Consensus.HardFork.Combinator.State.Types
  ( Current (..)
  , HardForkState (..)
  )
import Ouroboros.Consensus.TypeFamilyWrappers (WrapChainDepState (..))

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

  validateLeiosBlockCert cm cds (HardForkBlock (OneEraBlock ns)) =
    -- Pair the block's era with the parent's chain-dep state era. If they
    -- match, dispatch to the per-era validation with the unwrapped cds.
    -- If they don't (era boundary: parent in era N-1, block in era N), no
    -- announcement could exist on the parent for this block to certify
    -- against, so we treat the cert (if any) as invalid.
    case Match.matchNS ns (State.tip cds) of
      Left _mismatch -> Right (HardForkBlock (OneEraBlock ns))
      Right matched ->
        hcollapse $ hcimap (Proxy @HasLeiosVoting) dispatch matched
   where
    dispatch ::
      forall x.
      HasLeiosVoting x =>
      Index xs x ->
      Product I WrapChainDepState x ->
      K (Either VerificationError (HardForkBlock xs)) x
    dispatch idx (Pair (I blk) (WrapChainDepState cds')) =
      K $
        fmap (HardForkBlock . OneEraBlock . injectNS idx . I) $
          validateLeiosBlockCert cm cds' blk
