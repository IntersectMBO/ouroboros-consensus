-- | What the node's LeiosFetch server traces.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace (TraceLeiosFetchServerEvent (..)) where

import Ouroboros.Consensus.Leios.Types (LeiosPoint)

data TraceLeiosFetchServerEvent
  = -- | The client requested an endorser block.
    TraceLeiosFetchServerBlockRequested !LeiosPoint
  | -- | The server sent it, which references this many transactions.
    TraceLeiosFetchServerBlockServed !LeiosPoint !Int
  | -- | The client requested this many of an endorser block's transactions.
    TraceLeiosFetchServerTxsRequested !LeiosPoint !Int
  | -- | The server sent them.
    TraceLeiosFetchServerTxsServed
      !LeiosPoint
      -- | the number of transactions
      !Int
      -- | their total size in bytes
      !Int
  | -- | The server does not hold the requested endorser block, so it
    -- disconnects the client.
    TraceLeiosFetchServerUnknownBlock !LeiosPoint
  | -- | The server does not hold the transactions at these offsets, so it
    -- disconnects the client.
    TraceLeiosFetchServerUnknownTxs !LeiosPoint ![Int]
  | -- | Reading the LeiosDB failed, so the server disconnects the client.
    TraceLeiosFetchServerDbException !LeiosPoint !String
  deriving Show
