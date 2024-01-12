{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

module Test.Consensus.PointSchedule.Shrinking (shrinkPeerSchedules) where

import qualified Data.Map.Strict as Map
import           Test.Consensus.PeerSimulator.StateView (StateView)
import           Test.Consensus.PointSchedule (GenesisTest, PeerSchedule)
import           Test.Consensus.PointSchedule.Peers (Peers (..))
import           Test.QuickCheck (shrinkList)

-- | Shrink a 'Peers PeerSchedule'. This does not affect the honest peer; it
-- does, however, attempt to remove other peers. The block tree is trimmed to
-- keep only parts that are necessary for the shrunk schedule.
shrinkPeerSchedules ::
  GenesisTest ->
  Peers PeerSchedule ->
  StateView ->
  [(GenesisTest, Peers PeerSchedule)]
shrinkPeerSchedules genesisTest schedule _stateView =
  map (genesisTest,) $ shrinkOtherPeers (const []) schedule

-- | Shrink the 'others' field of a 'Peers' structure by attempting to remove
-- peers or by shrinking their values using the given shrinking function.
shrinkOtherPeers :: (a -> [a]) -> Peers a -> [Peers a]
shrinkOtherPeers shrink Peers{honest, others} =
  map (Peers honest . Map.fromList) $
    shrinkList (traverse (traverse shrink)) $ Map.toList others
