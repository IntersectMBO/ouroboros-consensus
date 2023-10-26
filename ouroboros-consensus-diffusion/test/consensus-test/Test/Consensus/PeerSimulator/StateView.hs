module Test.Consensus.PeerSimulator.StateView (
    ChainSyncException (..)
  , StateView (..)
  ) where

import           Ouroboros.Consensus.Util.IOLike (SomeException)
import           Test.Consensus.PointSchedule (PeerId, TestFragH)

-- | A record to associate an exception thrown by the ChainSync
-- thread with the peer that it was running for.
data ChainSyncException = ChainSyncException
       { csePeerId    :: PeerId
       , cseException :: SomeException
       }
    deriving Show

-- | A record that helps inspecting the current state of the simulation. This
-- contains both information on the peer simulator and on the node under test
-- (and in particular its ChainDB).
data StateView = StateView {
    -- | The chain currently selected by the ChainDB.
    svSelectedChain       :: TestFragH,

    -- | A list of exception raised in threads running scheduled ChainSync
    -- servers.
    --
    -- REVIEW: Only the server part?
    svChainSyncExceptions :: [ChainSyncException]
  }
