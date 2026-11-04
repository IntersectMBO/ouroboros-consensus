-- | What the node's LeiosFetch client and server trace.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace
  ( TraceLeiosFetchClientEvent (..)
  , FetchArrivalBytes (..)
  , fetchArrivalInvalid
  , fetchArrivalEvicted
  , fetchArrivalGood
  , fetchArrivalExtra
  , TraceLeiosFetchServerEvent (..)
  ) where

import Data.Time.Clock (NominalDiffTime)
import Ouroboros.Consensus.Leios.Types (BytesSize, LeiosPoint)
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

-- | The bytes of one LeiosFetch arrival ('MsgLeiosBlock' or 'MsgLeiosBlockTxs'),
-- partitioned by what the node already had of them. The four fields sum to the
-- message's total size.
data FetchArrivalBytes = MkFetchArrivalBytes
  { fabInvalid :: !BytesSize
  -- ^ Bytes of an invalid message (the whole message).
  , fabEvicted :: !BytesSize
  -- ^ Bytes the node no longer tracked, though it presumably once did: their
  -- point was pruned, or the LeiosTxCache had evicted them.
  , fabGood :: !BytesSize
  -- ^ Bytes the node needed.
  , fabExtra :: !BytesSize
  -- ^ Bytes the node already had (redundant).
  }
  deriving (Eq, Show)

instance Semigroup FetchArrivalBytes where
  MkFetchArrivalBytes a1 b1 c1 d1 <> MkFetchArrivalBytes a2 b2 c2 d2 =
    MkFetchArrivalBytes (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2)

instance Monoid FetchArrivalBytes where
  mempty = MkFetchArrivalBytes 0 0 0 0

-- | The message's whole size attributed to a single bucket, the rest zero.
fetchArrivalInvalid
  , fetchArrivalEvicted
  , fetchArrivalGood
  , fetchArrivalExtra ::
    BytesSize -> FetchArrivalBytes
fetchArrivalInvalid n = mempty{fabInvalid = n}
fetchArrivalEvicted n = mempty{fabEvicted = n}
fetchArrivalGood n = mempty{fabGood = n}
fetchArrivalExtra n = mempty{fabExtra = n}

data TraceLeiosFetchServerEvent
  = -- | The client requested an endorser block.
    TraceLeiosFetchServerBlockRequested !LeiosPoint
  | -- | The server sent it, of this many bytes.
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
