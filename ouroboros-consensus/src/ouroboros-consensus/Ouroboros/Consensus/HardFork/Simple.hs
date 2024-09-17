{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Ouroboros.Consensus.HardFork.Simple (TriggerHardFork (..)) where

import           Cardano.Slotting.Slot (EpochNo)
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

-- | The trigger condition that will cause the hard fork transition.
--
-- This type is only intended for use as part of a
-- 'Ouroboros.Consensus.Ledger.Basics.LedgerCfg', which means it is "static":
-- it cannot change during an execution of the node process.
data TriggerHardFork =
    -- | Trigger the transition when the on-chain protocol major version (from
    -- the ledger state) reaches this number.
    --
    -- Note: The HFC logic does not require the trigger version for one era to
    -- be the successor of the trigger version for the previous era.
    TriggerHardForkAtVersion !Word16
    -- | For testing only, trigger the transition at a specific hard-coded
    -- epoch, irrespective of the ledger state.
  | TriggerHardForkAtEpoch !EpochNo
    -- | Ledger states in this era cannot determine when the hard fork
    -- transition will happen.
    --
    -- It's crucial to note that this option does /not/ imply that "the era
    -- will never end". Instead, the era cannot end within this node process
    -- before it restarts with different software and/or configuration for this
    -- era.
  | TriggerHardForkNotDuringThisExecution
  deriving (Show, Generic, NoThunks)
