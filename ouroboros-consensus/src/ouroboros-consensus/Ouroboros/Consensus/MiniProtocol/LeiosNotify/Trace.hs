-- | What the node's LeiosNotify client traces.
module Ouroboros.Consensus.MiniProtocol.LeiosNotify.Trace (TraceLeiosNotifyClientEvent (..)) where

import Ouroboros.Consensus.Leios.Types (LeiosVote)

data TraceLeiosNotifyClientEvent
  = -- | The node added this vote, which the peer was the first to send.
    TraceLeiosNotifyClientVoteAdded !LeiosVote
  deriving Show
