{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | This module contains the definition of a state in the Genesis State Machine
-- (GSM). The GSM itself is defined in 'ouroboros-consensus-diffusion', but the
-- ChainSync client relies on its state.
module Ouroboros.Consensus.Node.GsmState (GsmState (..)) where

import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

-- | Current state of the Genesis State Machine
data GsmState
  = -- | We are syncing, and the Honest Availability Assumption is not
    -- satisfied.
    PreSyncing
  | -- | We are syncing, and the Honest Availability Assumption is satisfied.
    Syncing
  | -- | We are caught-up.
    CaughtUp
  deriving (Eq, Show, Read, Generic, NoThunks)
