module Test.Consensus.PointSchedule.Shrinking (shrinkPeerSchedules) where

import           Test.Consensus.PeerSimulator.StateView (StateView)
import           Test.Consensus.PointSchedule (GenesisTest, PeerSchedule)
import           Test.Consensus.PointSchedule.Peers (Peers (..))

-- | Shrink a 'Peers PeerSchedule'.
shrinkPeerSchedules ::
  GenesisTest ->
  Peers PeerSchedule ->
  StateView ->
  [(GenesisTest, Peers PeerSchedule)]
shrinkPeerSchedules _genesisTest _schedule _stateView = []
