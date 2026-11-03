-- | What the node's LeiosFetch client and server trace.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace
  ( TraceLeiosFetchClientEvent (..)
  , TraceLeiosFetchServerEvent (..)
  ) where

import Data.Time.Clock (NominalDiffTime)
import Ouroboros.Consensus.Leios.TxCache.API (FetchArrivalBytes)
import Ouroboros.Consensus.Leios.Types (LeiosPoint)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Server (ExnLeiosInvalidRequest)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type (TxBitmaps)
import Ouroboros.Consensus.Storage.LeiosDB.Exception (LeiosDbException)

data TraceLeiosFetchClientEvent
  = -- | An endorser block arrived, its bytes classified by whether the node
    -- needed them.
    TraceLeiosFetchClientBlockArrived !LeiosPoint !FetchArrivalBytes
  | -- | Some of an endorser block's transactions arrived, their bytes
    -- classified by whether the node needed them.
    TraceLeiosFetchClientTxsArrived !LeiosPoint !FetchArrivalBytes
  | -- | The node durably holds the endorser block, this long after its
    -- earliest announcement's slot began, if one was seen.
    TraceLeiosFetchClientBlockAcquired !LeiosPoint !(Maybe NominalDiffTime)
  | -- | The node durably holds all of the endorser block's transactions, this
    -- long after its earliest announcement's slot began, if one was seen.
    TraceLeiosFetchClientBlockTxsAcquired !LeiosPoint !(Maybe NominalDiffTime)
  deriving Show

data TraceLeiosFetchServerEvent
  = -- | The client requested an endorser block.
    TraceLeiosFetchServerBlockRequested !LeiosPoint
  | -- | The server sent it, this many bytes.
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
  | -- | The client requested what the server does not hold, so the server
    -- disconnects it.
    TraceLeiosFetchServerInvalidRequest !ExnLeiosInvalidRequest
  | -- | Reading the LeiosDB for the client's request for this endorser block
    -- failed, so the server disconnects the client.
    TraceLeiosFetchServerBlockDbFailure !LeiosPoint !LeiosDbException
  | -- | Reading the LeiosDB for the client's request for these transactions of
    -- this endorser block failed, so the server disconnects the client.
    TraceLeiosFetchServerTxsDbFailure !LeiosPoint !TxBitmaps !LeiosDbException
  | -- | Opening the server's LeiosDB reader failed, so the server disconnects
    -- the client.
    TraceLeiosFetchServerOpenReaderDbFailure !LeiosDbException
  deriving Show
