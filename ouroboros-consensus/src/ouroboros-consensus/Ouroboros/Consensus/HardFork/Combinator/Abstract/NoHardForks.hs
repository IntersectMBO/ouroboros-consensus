{-# LANGUAGE UndecidableSuperClasses #-}
module Ouroboros.Consensus.HardFork.Combinator.Abstract.NoHardForks
  ( ImmutableEraParams (..)
  , NoHardForks (..)
  , immutableEpochInfo
  ) where

import Cardano.Slotting.EpochInfo
import Data.Functor.Identity (runIdentity)
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.Ledger.Abstract

{-------------------------------------------------------------------------------
  Blocks that don't /have/ any transitions
-------------------------------------------------------------------------------}

-- | A block type for which the 'EraParams' will /never/ change
--
-- Technically, some application of
-- 'Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkBlock' could have an
-- instance for this. But that would only be appropriate if two conditions were
-- met.
--
-- * all the eras in that block have the same 'EraParams'
--
-- * all eras that will /ever/ be added to that block in the future will also
--   have those same 'EraParams'
class ImmutableEraParams blk where
  -- | Extract 'EraParams' from the top-level config
  --
  -- The HFC itself does not care about this, as it must be given the full shape
  -- across /all/ eras.
  immutableEraParams :: TopLevelConfig blk -> EraParams

class (SingleEraBlock blk, ImmutableEraParams blk) => NoHardForks blk where
  -- | Construct partial ledger config from full ledger config
  --
  -- See also 'toPartialConsensusConfig'
  toPartialLedgerConfig ::
    proxy blk ->
    LedgerConfig blk ->
    PartialLedgerConfig blk

immutableEpochInfo ::
  (Monad m, ImmutableEraParams blk) =>
  TopLevelConfig blk ->
  EpochInfo m
immutableEpochInfo cfg =
  hoistEpochInfo (pure . runIdentity) $
    fixedEpochInfo
      (History.eraEpochSize params)
      (History.eraSlotLength params)
 where
  params :: EraParams
  params = immutableEraParams cfg
