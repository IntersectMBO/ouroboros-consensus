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

data StateView = StateView {
    svSelectedChain       :: TestFragH,
    svChainSyncExceptions :: [ChainSyncException]
  }
