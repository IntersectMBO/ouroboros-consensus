{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module Ouroboros.Consensus.HardFork.Simple (TriggerHardFork (..)) where

import           Cardano.Slotting.Slot (EpochNo)
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

<<<<<<< HEAD:ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/HardFork/Simple.hs
||||||| parent of 2726854bf... Satisfy new serialisation constraints on LedgerConfig:ouroboros-consensus/src/Ouroboros/Consensus/HardFork/Simple.hs
import           Cardano.Slotting.Slot (EpochNo)

=======
import           Cardano.Binary
import           Cardano.Slotting.Slot (EpochNo)

import           Ouroboros.Consensus.Node.Serialisation

>>>>>>> 2726854bf... Satisfy new serialisation constraints on LedgerConfig:ouroboros-consensus/src/Ouroboros/Consensus/HardFork/Simple.hs
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

instance SerialiseNodeToClient blk TriggerHardFork where
  encodeNodeToClient _ _ triggerHardFork = case triggerHardFork of
    TriggerHardForkAtVersion v -> encodeListLen 2 <> encodeWord8 0 <> toCBOR v
    TriggerHardForkAtEpoch e   -> encodeListLen 2 <> encodeWord8 1 <> toCBOR e
    TriggerHardForkNever       -> encodeListLen 1 <> encodeWord8 2
  decodeNodeToClient _ _ = do
    len <- decodeListLen
    tag <- decodeWord8
    case (len, tag) of
      (2, 0)   -> TriggerHardForkAtVersion <$> fromCBOR @Word16
      (2, 1)   -> TriggerHardForkAtEpoch <$> fromCBOR @EpochNo
      (1, 2)   -> return TriggerHardForkNever
      _ -> fail $ "TriggerHardFork: invalid (len, tag): " <> show (len, tag)
