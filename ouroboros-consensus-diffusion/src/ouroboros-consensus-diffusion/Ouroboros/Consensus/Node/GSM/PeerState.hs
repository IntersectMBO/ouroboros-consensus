module Ouroboros.Consensus.Node.GSM.PeerState
  ( GsmPeerState (..)
  , maybeChainSyncState
  , maybePerasCertDiffusionState
  , mkGsmPeerStates
  , gsmPeerIsIdle
  )
where

import Cardano.Base.FeatureFlags (CardanoFeatureFlag (..))
import Data.Align (Semialign (..))
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.These (These (..))
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State
  ( ChainSyncClientHandle (..)
  , ChainSyncClientHandleCollection (..)
  , ChainSyncState (..)
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V1.State
  ( ObjectDiffusionInboundHandle (..)
  , ObjectDiffusionInboundHandleCollection (..)
  , ObjectDiffusionInboundState (..)
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.PerasCert (PerasCertDiffusionInboundState)
import Ouroboros.Consensus.Util.IOLike (MonadSTM (..), readTVar)
import Ouroboros.Network.NodeToNode.Version (isPerasEnabled)

-- | State about peers we are connected to during initialization.
newtype GsmPeerState blk = GsmPeerState
  { unGsmPeerState ::
      These
        (ChainSyncState blk)
        (PerasCertDiffusionInboundState blk)
  }

-- | Retrieve the 'ChainSync' state of this peer, if such a connection is established.
maybeChainSyncState :: GsmPeerState blk -> Maybe (ChainSyncState blk)
maybeChainSyncState (GsmPeerState these) =
  case these of
    This csState -> Just csState
    That _ -> Nothing
    These csState _ -> Just csState

-- | Retrieve the 'PerasCertDiffusion' state of this peer, if such a connection is established.
maybePerasCertDiffusionState :: GsmPeerState blk -> Maybe (PerasCertDiffusionInboundState blk)
maybePerasCertDiffusionState (GsmPeerState these) =
  case these of
    This _ -> Nothing
    That pcdState -> Just pcdState
    These _ pcdState -> Just pcdState

-- | Construct a 'GsmPeerState' for all peers we are connected to.
mkGsmPeerStates ::
  (Ord peer, MonadSTM m) =>
  ChainSyncClientHandleCollection peer m blk ->
  ObjectDiffusionInboundHandleCollection peer m blk ->
  STM m (Map peer (GsmPeerState blk))
mkGsmPeerStates csHandles pcdHandles = do
  csPeerStates <- traverse (readTVar . cschState) =<< cschcMap csHandles
  pcdPeerStates <- traverse (readTVar . odihState) =<< odihcMap pcdHandles
  pure (GsmPeerState <$> align csPeerStates pcdPeerStates)

-- | Determine whether our connections to this peer are idle.
gsmPeerIsIdle :: Set CardanoFeatureFlag -> GsmPeerState blk -> Bool
gsmPeerIsIdle featureFlags (GsmPeerState these) =
  case these of
    -- We have both ChainSync and PerasCertDiffusion connections => idle if both are idling
    These csState pcdState -> csIdling csState && odisIdling pcdState
    -- Only a ChainSync connection is available => idle if the ChainSync connection is idling
    This csState | not (perasIsEnabled csState) -> csIdling csState
    -- We will soon establish a PerasCertDiffusion connection => not idling
    This _ -> False
    -- We will soon establish a ChainSync connection => not idling
    That _ -> False
 where
  -- Is the Peras feature flag enabled and the peer is compatible with it?
  perasIsEnabled csState = isPerasEnabled featureFlags (csNodeToNodeVersion csState)
