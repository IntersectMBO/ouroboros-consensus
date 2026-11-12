{-# LANGUAGE DeriveFunctor #-}

-- | What the node's LeiosFetch client and server trace.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace
  ( TraceLeiosFetchClientEvent (..)
  , LeiosBlockTxSources (..)
  , LeiosTxCacheInsertBodySummary (..)
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

-- | What the node's LeiosFetch clients do, naming the peer that sent what
-- arrived.
data TraceLeiosFetchClientEvent peer
  = -- | An endorser block arrived from this peer. Its bytes are invalid if it
    -- failed its checks, evicted if its point was pruned before its write
    -- settled, extra if another reply's write for the point settled first,
    -- and good otherwise. One the node writes is traced once its write settles.
    TraceLeiosFetchClientBlockArrived peer LeiosPoint FetchArrivalBytes
  | -- | Where the node takes the transactions of the endorser block at this
    -- point from, decided as it writes the body.
    TraceLeiosFetchClientBlockTxSources !LeiosPoint !LeiosBlockTxSources
  | -- | Some of an endorser block's transactions arrived from this peer. The
    -- whole reply is invalid if it failed its checks. Otherwise each tx's bytes
    -- are evicted if its point was pruned, or the LeiosTxCache no longer
    -- retains the point; extra if the tx's job was already done, or the
    -- LeiosTxCache already held the tx; and good otherwise. Another point's
    -- write can make the LeiosTxCache hold a tx this point still needed, which
    -- then counts as extra. A reply is traced once its write, if any, lands.
    TraceLeiosFetchClientTxsArrived peer LeiosPoint FetchArrivalBytes
  | -- | The node acquired the endorser block this long after its point's slot
    -- began, if that is known.
    TraceLeiosFetchClientBlockAcquired LeiosPoint (Maybe NominalDiffTime)
  | -- | The node acquired all of the endorser block's transactions this long
    -- after its point's slot began, if that is known.
    TraceLeiosFetchClientBlockTxsAcquired LeiosPoint (Maybe NominalDiffTime)
  deriving (Show, Functor)

-- | How many of an endorser block's transactions the node takes from each
-- source, decided as it writes the body. The fields sum to the transactions the
-- body references.
data LeiosBlockTxSources = MkLeiosBlockTxSources
  { btsLocalFills :: !Int
  -- ^ copied from other points at which the LeiosDB holds them, unless a point
  -- no longer holds its tx by the time the write runs
  , btsMempool :: !Int
  -- ^ written from the node's mempool
  , btsRemaining :: !Int
  -- ^ neither, so fetched from peers unless the point already holds them
  }
  deriving Show

-- | Summary of an EB body inserted into the LeiosTxCache, for observability.
-- The counts nest: @ibsTxsInEb >= ibsAcquired >= ibsValidated@.
data LeiosTxCacheInsertBodySummary = MkLeiosTxCacheInsertBodySummary
  { ibsTxsInEb :: !Int
  -- ^ txs the EB body references
  , ibsAcquired :: !Int
  -- ^ of those, how many the cache already held (inserted or validated)
  , ibsValidated :: !Int
  -- ^ of those acquired, how many were already validated
  , ibsCacheTxCount :: !Int
  -- ^ total txs the cache holds after this insert
  , ibsCacheCapacity :: !Int
  -- ^ txs the cache is sized for; its load is 'ibsCacheTxCount' over this
  }
  deriving (Eq, Show)

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
