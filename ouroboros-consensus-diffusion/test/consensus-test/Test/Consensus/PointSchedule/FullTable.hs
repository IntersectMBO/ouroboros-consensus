{-# LANGUAGE NamedFieldPuns #-}

-- | A 'full table' point schedule is a point schedule that shows the state of
-- all the peers at each tick with absolute times.

module Test.Consensus.PointSchedule.FullTable (
    FullTablePointSchedule (..)
  , FullTableRow (..)
  , fullTableFromActivePointSchedule
  ) where

import           Control.Monad.Class.MonadTime.SI (Time (Time), addTime)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import           Test.Consensus.PointSchedule (NodeState (..),
                     PointSchedule (..), Tick (..))
import           Test.Consensus.PointSchedule.Peers (Peer (..), PeerId (..),
                     Peers (..), peersFromPeerIdList, peersList)

-- | A row in a full-table point schedule. It contains states for all peers and
-- an absolute time at which this state starts holding.
data FullTableRow = FullTableRow {
    ftrStates :: Peers NodeState,
    ftrTime   :: Time
  }
  deriving (Show, Eq)

-- | A full-table point schedule is a (non-empty) list of 'FullTableRow's.
data FullTablePointSchedule = FullTablePointSchedule {
    ftpsRows    :: NonEmpty FullTableRow,
    ftpsPeerIds :: NonEmpty PeerId
  }
  deriving (Show, Eq)

-- | Transform a regular “active” 'PointSchedule' into its full table equivalent.
fullTableFromActivePointSchedule :: PointSchedule -> FullTablePointSchedule
fullTableFromActivePointSchedule PointSchedule{ticks, peerIds} =
    FullTablePointSchedule {
      ftpsRows = go (Time 0) beginningOfTimeRow [] (NonEmpty.toList ticks),
      ftpsPeerIds = peerIds
    }
  where
    -- | A row where time is @0@ and where all peers are offline.
    beginningOfTimeRow = FullTableRow (peersFromPeerIdList peerIds NodeOffline) (Time 0)

    -- | Helper to process all the ticks and turn them into 'FullTableRow's. We
    -- keep the last constructed row apart from the others because we merge
    -- zero-duration ticks into it. We have to be careful with time because
    -- 'PointSchedule' ticks carry durations while our rows carry absolute time
    -- where a row starts being relevant.
    go :: Time -> FullTableRow -> [FullTableRow] -> [Tick] -> NonEmpty FullTableRow
    go _ row rows [] = NonEmpty.reverse (row :| rows)
    go time row@FullTableRow{ftrStates, ftrTime} rows (Tick{active, duration} : otherTicks) =
      let states' = updatePeer active ftrStates
          time' = addTime duration time
          rows' = if time == ftrTime || isDummyBeginningOfTime row then rows else row : rows
       in go time' (FullTableRow states' time) rows' otherTicks

    -- | Update a 'Peers' structure given a 'Peer'.
    updatePeer :: Peer a -> Peers a -> Peers a
    updatePeer peer@(Peer HonestPeer _) peers = peers { honest = peer }
    updatePeer peer@(Peer peerId _) peers = peers { others = Map.insert peerId peer (others peers) }

    -- | If it is a row at time 0 and with all nodes offline, then it is just a
    -- dummy which can be safely ignored.
    isDummyBeginningOfTime FullTableRow{ftrStates, ftrTime} =
      ftrTime == Time 0 &&
        all (\(Peer _ s) -> s == NodeOffline) (NonEmpty.toList (peersList ftrStates))
