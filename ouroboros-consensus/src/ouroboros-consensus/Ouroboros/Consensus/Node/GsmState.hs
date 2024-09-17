{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | This module contains the definition of a state in the Genesis State Machine
-- (GSM). The GSM itself is defined in 'ouroboros-consensus-diffusion', but the
-- ChainSync client relies on its state.
module Ouroboros.Consensus.Node.GsmState (GsmState (..)) where

import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

-- | Current state of the Genesis State Machine
data GsmState =
    PreSyncing
    -- ^ We are syncing, and the Honest Availability Assumption is not
    -- satisfied.
  |
    Syncing
    -- ^ We are syncing, and the Honest Availability Assumption is satisfied.
  |
    CaughtUp
    -- ^ We are caught-up.
  deriving (Eq, Show, Read, Generic, NoThunks)
