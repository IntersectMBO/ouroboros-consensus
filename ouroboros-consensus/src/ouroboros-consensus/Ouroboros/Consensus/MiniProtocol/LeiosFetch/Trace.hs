{-# LANGUAGE DeriveFunctor #-}

-- | What the node's LeiosFetch client and server trace.
-- Policy summaries have strict bounded fields; their public trace wrappers
-- and formatting remain lazy. Producers materialize observation batches
-- before leaving the transaction that owns the facts.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace
  ( TraceLeiosFetchClientEvent (..)
  , LeiosBlockTxSources (..)
  , LeiosTxCacheInsertBodySummary (..)
  , ClosureSummary (..)
  , TraceLeiosFetchDetailEvent (..)
  , FetchRequest (..)
  , FetchUsage (..)
  , FetchBudget (..)
  , FetchSelection (..)
  , FetchHedgeCause (..)
  , FetchRoutine (..)
  , FetchLimit (..)
  , FetchPhase (..)
  , FetchDetail (..)
  , FetchArrivalBytes (..)
  , fetchArrivalInvalid
  , fetchArrivalEvicted
  , fetchArrivalGood
  , fetchArrivalExtra
  , TraceLeiosFetchServerEvent (..)
  ) where

import Control.Monad.Class.MonadTime.SI (DiffTime, Time)
import Data.Maybe.Strict (StrictMaybe)
import Data.Time.Clock (NominalDiffTime)
import Data.Word (Word32, Word64)
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Leios.Types (BytesSize, LeiosPoint)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Coverage (ClosureSummary (..))
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.PeerPenalty as P
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Server (ExnLeiosInvalidRequest)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Timing (RttSource)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type (TxBitmaps)
import Ouroboros.Consensus.Storage.LeiosDB.Exception (LeiosDbException)

-- | What the node's LeiosFetch clients do, naming the peer that sent what
-- arrived.
data TraceLeiosFetchClientEvent peer
  = -- | An endorser block arrived from this peer, its bytes classified by
    -- whether the node needed them. One the node writes is traced once the
    -- write settles, and was needed only if no other reply's write
    -- settled first.
    TraceLeiosFetchClientBlockArrived !peer !LeiosPoint !FetchArrivalBytes
  | -- | Where the node takes the transactions of the endorser block at this
    -- point from, decided as it writes the body, and, if the LeiosTxCache took
    -- the body in, its summary of doing so.
    TraceLeiosFetchClientBlockTxSources
      !LeiosPoint
      !LeiosBlockTxSources
      !(Maybe LeiosTxCacheInsertBodySummary)
  | -- | Some of an endorser block's transactions arrived from this peer, their
    -- bytes classified by a read-only arrival snapshot before writer admission.
    -- Repeated occurrences use the same snapshot without inserting cache state.
    -- This arrival trace is
    -- separate from the positive storage confirmation and acquisition events.
    TraceLeiosFetchClientTxsArrived !peer !LeiosPoint !FetchArrivalBytes
  | -- | The node durably holds the endorser block, which arrived this long
    -- after its point's slot began, if that is known.
    TraceLeiosFetchClientBlockAcquired !LeiosPoint !(Maybe NominalDiffTime)
  | -- | The node durably holds all of the endorser block's transactions. The
    -- arrival that completed them came this long after its point's slot
    -- began, if that is known.
    TraceLeiosFetchClientBlockTxsAcquired !LeiosPoint !(Maybe NominalDiffTime)
  | TraceLeiosFetchClientClosureSummary LeiosPoint ClosureSummary
  | TraceLeiosFetchClientPolicy peer Natural FetchRoutine
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

-- | The single independently suppressible high-frequency policy tracer. The
-- invocation is node-global within the fetch runtime and remains captured after
-- its original registration disappears; the request ordinal lives in its detail.
data TraceLeiosFetchDetailEvent peer = TraceLeiosFetchDetailEvent peer Natural FetchDetail
  deriving (Show, Functor)

-- | Detached fetch identity. The ordinal is local auditing metadata and is
-- never added to the fetch wire format. A job is the immutable whole-TX batch.
data FetchRequest = FetchRequest
  { fetchOrdinal :: !Word64
  -- ^ Nonwrapping local request number within the captured invocation.
  , fetchPoint :: !LeiosPoint
  -- ^ Original EB identity, retained after reassignment or disconnection.
  , fetchJob :: !(StrictMaybe Word32)
  -- ^ Stable job index; SNothing identifies a body request.
  , fetchPayloadBytes :: !Word32
  -- ^ Offered body size or actual immutable whole-TX job size.
  }
  deriving (Eq, Show)

-- | Independent transaction credit and body count, across all EBs at one peer.
-- These are logical outstanding requests, not preallocated buffers or DB space.
data FetchUsage = FetchUsage
  { fetchTxBytes :: !Word64
  -- ^ Live queued/sent transaction payload credit.
  , fetchBodies :: !Word32
  -- ^ Live body attempts across EBs, independent of transaction credit.
  , fetchTxLimit :: !Word32
  -- ^ Applicable ordinary/BLP transaction limit for this connection.
  , fetchBodyLimit :: !Word32
  -- ^ Applicable body-count limit for this connection.
  }
  deriving (Eq, Show)

-- | Frozen SI timing inputs; later samples and payload dribbles do not renew them.
data FetchBudget = FetchBudget !Word32 !DiffTime !DiffTime !DiffTime !RttSource
  deriving (Eq, Show)

-- | Exact ranking evidence captured before request admission. Usage at
-- admission is a separate snapshot, and delayed tracing does not refresh this.
data FetchSelection = FetchSelection !Time !P.Score !(StrictMaybe Time)
  deriving (Eq, Show)

data FetchHedgeCause = FetchOwnHeadExpiry | FetchBlockedFollower
  deriving (Eq, Show)

data FetchRoutine
  = FetchUpstreamChanged !Bool
  | FetchHedgeOpened !FetchRequest !FetchHedgeCause
  | FetchBodyWon !FetchRequest !Word32
  | FetchPenaltyCharged !FetchRequest !Time !P.Snapshot !P.Score !P.Snapshot
  | FetchInvocationClosed !FetchUsage !Bool
  | FetchRegistrationRemoved !FetchUsage
  deriving (Eq, Show)

data FetchLimit = FetchBodyCount | FetchTransactionCredit | FetchCommandCount
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | One compact transition per phase, never a packet/input-span trace.
data FetchPhase
  = FetchDispatchCommitted
  | FetchSoftExpired
  | FetchSendReturned
  | FetchReplyReceived
  | FetchHandlerStarted
  | FetchReceiptSettled
  | FetchWriterAccepted
  | FetchHandlerFinished
  | FetchResourceSettled
  | FetchObsoleteSkipped
  deriving (Bounded, Enum, Eq, Show)

data FetchDetail
  = FetchAdmitted !FetchRequest !FetchBudget !Time !FetchUsage !(StrictMaybe FetchSelection)
  | FetchTransition !FetchRequest !FetchPhase !Time !FetchUsage
  | FetchHeadActivated !FetchRequest !Time !Time !Bool
  | FetchPressureChanged ![FetchLimit] !FetchUsage
  deriving (Eq, Show)
