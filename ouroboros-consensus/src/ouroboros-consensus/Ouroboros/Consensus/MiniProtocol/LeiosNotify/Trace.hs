-- | What the node's LeiosNotify client traces.
module Ouroboros.Consensus.MiniProtocol.LeiosNotify.Trace (TraceLeiosNotifyClientEvent (..)) where

import Ouroboros.Consensus.Leios.Types (LeiosVote)

data TraceLeiosNotifyClientEvent
  = -- | The node added this vote, which the peer was the first to send.
    TraceLeiosNotifyClientVoteAdded !LeiosVote
  | -- | The node had this vote only after its deadline, so it neither keeps nor
    -- relays it. Many of these mean the diffusion period is too short for the
    -- network, or that the node verifies votes too slowly.
    TraceLeiosNotifyClientVoteArrivedTooLate !LeiosVote
  deriving Show
