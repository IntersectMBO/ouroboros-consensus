module Ouroboros.Consensus.Node.GSM.PeerState
  ( GsmPeerState (..)
  , maybeChainSyncState
  , mkGsmPeerStates
  , gsmPeerIsIdle
  )
where

import Data.Align (Semialign (align))
import Data.Map.Strict (Map)
import Data.These (These (That, These, This))
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client
  ( ChainSyncClientHandle (cschState)
  , ChainSyncClientHandleCollection (cschcMap)
  , ChainSyncState (csIdling, csPerasSupport)
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.State
  ( ObjectDiffusionInboundHandle (odihState)
  , ObjectDiffusionInboundHandleCollection (odihcMap)
  , ObjectDiffusionInboundState (odIdling)
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.PerasCert (PerasCertDiffusionInboundState)
import Ouroboros.Consensus.Util.IOLike (MonadSTM (STM), readTVar)
import Ouroboros.Network.PerasSupport (PerasSupport (PerasUnsupported))

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
gsmPeerIsIdle :: GsmPeerState blk -> Bool
gsmPeerIsIdle (GsmPeerState these) =
  case these of
    -- We have both ChainSync and PerasCertDiffusion connections => idle if both are idling
    These csState pcdState -> csIdling csState && odIdling pcdState
    -- Certificate diffusion was not negotiated => ChainSync idling is sufficient
    This csState
      | PerasUnsupported <- csPerasSupport csState ->
          csIdling csState
    -- We will soon establish a PerasCertDiffusion connection => not idling
    This _ -> False
    -- We will soon establish a ChainSync connection => not idling
    That _ -> False
