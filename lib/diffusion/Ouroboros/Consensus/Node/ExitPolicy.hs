{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Ouroboros.Consensus.Node.ExitPolicy
  ( NodeToNodeInitiatorResult (..)
  , returnPolicy

    -- * Re-exports
  , ReturnPolicy
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client as CSClient
import Ouroboros.Network.ExitPolicy

-- | Result of any of the `node-to-node` mini-protocols.  We ignore all but
-- `chain-sync` results.
data NodeToNodeInitiatorResult
  = ChainSyncInitiatorResult !CSClient.ChainSyncClientResult
  | NoInitiatorResult
  deriving (Generic, NFData)

returnPolicy :: ReturnPolicy NodeToNodeInitiatorResult
returnPolicy NoInitiatorResult = RepromoteDelay 10
returnPolicy (ChainSyncInitiatorResult result) = case result of
  -- TODO: it would be nice to have additional context to predict when we will
  -- be ready to reconnect.
  CSClient.ForkTooDeep _ _ourTip _theirTip -> RepromoteDelay 120
  CSClient.NoMoreIntersection _ourTip _theirTip -> RepromoteDelay 120
  CSClient.RolledBackPastIntersection
    _
    _ourTip
    _theirTip -> RepromoteDelay 180
  -- the outbound-governor asked for hot to warm demotion; it's up to the
  -- governor to decide to promote the peer to hot.
  CSClient.AskedToTerminate -> RepromoteDelay 10
