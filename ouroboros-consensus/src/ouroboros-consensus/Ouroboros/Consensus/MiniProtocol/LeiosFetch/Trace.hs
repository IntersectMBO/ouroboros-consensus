{-# LANGUAGE DeriveFunctor #-}

-- | What the node's LeiosFetch client and server trace.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace
  ( TraceLeiosFetchClientEvent (..)
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
  , detachFetchDetail
  , detachFetchRoutine
  , FetchArrivalBytes (..)
  , fetchArrivalInvalid
  , fetchArrivalEvicted
  , fetchArrivalGood
  , fetchArrivalExtra
  , TraceLeiosFetchServerEvent (..)
  ) where

import Control.Monad.Class.MonadTime.SI (DiffTime, Time)
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
    -- write is durable, and was needed only if no other delivery's write
    -- settled first.
    TraceLeiosFetchClientBlockArrived !peer !LeiosPoint !FetchArrivalBytes
  | -- | The node took the endorser block at this point from the body the
    -- LeiosTxCache held for another point, rather than fetching it. Traced once
    -- the write is durable, if no delivery's write settled first.
    TraceLeiosFetchClientBlockFromCache !LeiosPoint
  | -- | Some of an endorser block's transactions arrived from this peer, their
    -- bytes classified by whether the node needed them.
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
  , fetchJob :: !(Maybe Word32)
  -- ^ Stable job index; Nothing identifies a body request.
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
data FetchSelection = FetchSelection !Time !P.Score !(Maybe Time)
  deriving (Eq, Show)

data FetchHedgeCause = FetchOwnHeadExpiry | FetchBlockedFollower
  deriving (Eq, Show)

data FetchRoutine
  = FetchUpstreamChanged Bool
  | FetchHedgeOpened FetchRequest FetchHedgeCause
  | FetchBodyWon FetchRequest Word32
  | FetchPenaltyCharged FetchRequest Time P.Snapshot P.Score P.Snapshot
  | FetchInvocationClosed FetchUsage Bool
  | FetchRegistrationRemoved FetchUsage
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
  = FetchAdmitted FetchRequest FetchBudget Time FetchUsage (Maybe FetchSelection)
  | FetchTransition FetchRequest FetchPhase Time FetchUsage
  | FetchHeadActivated FetchRequest Time Time Bool
  | FetchPressureChanged [FetchLimit] FetchUsage
  deriving (Eq, Show)

-- | Force only bounded scalar summaries at the producer boundary. Public
-- event arguments remain lazy for disabled tracers; no payload is demanded.
detachFetchDetail :: FetchDetail -> ()
detachFetchDetail event = case event of
  FetchAdmitted req budget at used selected ->
    detachRequest req `seq`
      budget `seq`
        at `seq`
          used `seq`
            maybe
              ()
              ( \(FetchSelection chosen score offered) -> chosen `seq` score `seq` maybe () (\value -> value `seq` ()) offered
              )
              selected
  FetchTransition req phase at used -> detachRequest req `seq` phase `seq` at `seq` used `seq` ()
  FetchHeadActivated req start expiry continuous -> detachRequest req `seq` start `seq` expiry `seq` continuous `seq` ()
  FetchPressureChanged limits used -> foldr seq () limits `seq` used `seq` ()

detachFetchRoutine :: FetchRoutine -> ()
detachFetchRoutine event = case event of
  FetchUpstreamChanged blocked -> blocked `seq` ()
  FetchHedgeOpened req cause -> detachRequest req `seq` cause `seq` ()
  FetchBodyWon req actual -> detachRequest req `seq` actual `seq` ()
  FetchPenaltyCharged req at before effective after -> detachRequest req `seq` at `seq` before `seq` effective `seq` after `seq` ()
  FetchInvocationClosed used failed -> used `seq` failed `seq` ()
  FetchRegistrationRemoved used -> used `seq` ()

detachRequest :: FetchRequest -> ()
detachRequest req = req `seq` maybe () (\n -> n `seq` ()) (fetchJob req)
