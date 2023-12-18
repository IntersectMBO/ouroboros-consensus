-- | A 'full table' point schedule is a point schedule that shows the state of
-- all the peers at each tick with absolute times.

module Test.Consensus.PointSchedule.FullTable (
    FullTablePointSchedule (..)
  , FullTableRow (..)
  ) where

import           Control.Monad.Class.MonadTime.SI (Time)
import           Data.List.NonEmpty (NonEmpty)
import           Test.Consensus.PointSchedule (NodeState (..))
import           Test.Consensus.PointSchedule.Peers (PeerId (..), Peers (..))

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
