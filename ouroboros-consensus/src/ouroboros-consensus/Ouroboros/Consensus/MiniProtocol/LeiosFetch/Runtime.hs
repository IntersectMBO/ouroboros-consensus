{-# LANGUAGE NamedFieldPuns #-}

-- | Bounded request facts shared by the ordinary client and the one kernel
-- coordinator. STM callbacks never acquire the outstanding-state MVar.
-- Rationale: @docs/fetch-policy/leiosfetch-improved.md#ownership-and-shutdown@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Runtime
  ( Runtime (..)
  , RuntimeState (..)
  , Session (..)
  , Attempt (..)
  , Client (..)
  , RequestError (..)
  , AdmissionRejection (..)
  , PendingOwner (..)
  , claimLocalBody
  , acceptLocalBody
  , clearLocalBody
  , settleLocalBody
  , newRuntime
  , openClient
  , closePeer
  , closeClient
  , enqueueRequest
  , workOwners
  , workEligible
  , takeRequest
  , commitRequest
  , observeSend
  , installReceiver
  , receiverAttempt
  , publishInput
  , completeWire
  , finishHandler
  , claimUseful
  , acceptReply
  , clearUnsubmitted
  , settleReceipt
  , releaseAttempt
  , discardAttempt
  , nextDeadline
  , closureScore
  , expireHeadsObserved
  , PenaltyCharge (..)
  , blocked
  , workOf
  , bytesOf
  ) where

import Control.Monad (void, when)
import qualified Data.IntMap.NonEmpty as NEIntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq
import Data.Word (Word32, Word64)
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Leios.Types (LeiosPoint)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Config
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.PeerPenalty as P
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Request
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.RequestState as L
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Timing (RttObservation)
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Timing as T
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace (FetchLimit)
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Wait as W
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type (maxLeiosTxsRequestBytesSize)
import Ouroboros.Consensus.Util.IOLike

-- | Configuration/internal errors are explicit; a legal indivisible job can
-- never remain silently unrequestable because it exceeds a chosen byte cap.
data RequestError
  = InvalidConfig String
  | SessionExists
  | OrdinalExhausted
  | MissingAttempt
  | InvalidRequestGeometry
  | ClientClosed
  deriving Show

instance Exception RequestError

-- | Why one complete candidate was not admitted. These are ordinary decisions,
-- rather than peer failures. Only the ledger's temporary limit refusals create
-- pressure observations; invalid or individually ineligible work creates none.
data AdmissionRejection
  = SourceClosed
  -- ^ Runtime closing or the captured client invocation no longer exists.
  | SourceBlocked
  -- ^ This peer owes an expired unresolved FIFO head.
  | WorkIneligible
  -- ^ Local ownership, forge ownership or a non-stalled existing copy.
  | ProtocolInvalid
  -- ^ The declared TX job exceeds the protocol maximum.
  | ReservationRejected !L.AdmissionError
  -- ^ Same-source work, a temporary limit, or an individually unfit job.
  deriving (Eq, Show)

-- | One node-owned fact store; no additional planner or storage task.
data Runtime m peer = Runtime
  { runtimeConfig :: !LeiosFetchStaticEnv
  -- ^ Validated request limits and receiver-policy settings shared by this runtime.
  , runtimeFacts :: !(StrictTVar m (RuntimeState m peer))
  -- ^ Shared admission, FIFO, credit and local-write ownership facts, updated
  -- together in STM so observers see a consistent request lifecycle.
  , runtimeClosing :: !(STM m Bool)
  -- ^ Admission observes the receipt task group closing in its publication transaction.
  , runtimeReady :: !(W.Readiness m)
  -- ^ Coalescing wakeup signal for the existing decision loop. Runtime updates
  -- that need replanning notify in their publishing transaction; the loop
  -- rereads facts after waking rather than retaining one event per change.
  }

-- | Live client sessions, physical leases and pending-write ownership.
data RuntimeState m peer = RuntimeState
  { sessions :: !(Map peer (Session m peer))
  -- ^ Live client invocations indexed by their original peer registrations.
  -- Accepted local writes can outlive these sessions through 'pending'.
  , attempts :: !(Map (L.AttemptKey peer) (Attempt peer))
  -- ^ Immutable requests and their timing/settlement facts, keyed by physical
  -- attempt. A settled descriptor can remain until a late send-return callback.
  , ledger :: !(L.Ledger peer LeiosPoint)
  -- ^ Authority for live request leases and per-peer reserved TX bytes/body
  -- counts, including queued work. A retained 'Attempt' need not still own
  -- a lease after its physical credit has been released.
  , pending :: !(Map (L.Work LeiosPoint) (PendingOwner peer, Bool))
  -- ^ Work with a local submission/receipt owner, independently of network
  -- credit. False means submission is not yet accepted; True means writer
  -- acceptance has transferred settlement to a receipt task, so peer removal
  -- must not clear the claim.
  , nextLocalOwner :: !Natural
  -- ^ Next runtime-wide identity for local body-import submission claims.
  -- Distinct tokens keep an old local completion from clearing a newer claim.
  , nextInvocation :: !Natural
  -- ^ Next runtime-wide client-run identity, allocated by 'openClient'. A new
  -- invocation can restart request ordinals without reusing old attempt keys.
  }

-- | Connection-local FIFO and publication facts, containing no payload.
data Session m peer = Session
  { invocation :: !Natural
  -- ^ Identity of this client run, allocated from 'nextInvocation'. Callbacks
  -- retain it so an older run cannot mutate a replacement session.
  , bigLedger :: !Bool
  , connectionRtt :: !(STM m (Maybe RttObservation))
  -- ^ Reader of the original connection's latest successful KeepAlive sample.
  -- Admission freezes the resulting allowance in each attempt's clock.
  , nextOrdinal :: !Word64
  -- ^ Next local request-attempt number in this invocation, across bodies, TX
  -- jobs and EBs. Successful admission increments it, with overflow checked.
  -- Together with peer/invocation it forms 'L.AttemptKey'; it is not a wire field.
  , queued :: !(StrictTVar m (Seq L.CommandKey))
  -- ^ Admitted request keys awaiting protocol-client dequeue. Live entries
  -- already own physical credit; stale keys are discarded during dequeue.
  -- This is the sole command queue shared with the peer's kernel registration.
  , wireQueue :: !(Seq (L.AttemptKey peer))
  -- ^ Requests committed for dispatch, in expected reply order. A completed
  -- reply remains at the head through local handling; 'finishHandler' removes
  -- it before activating the successor.
  , sendQueue :: !(Seq (L.AttemptKey peer))
  -- ^ Committed requests awaiting their channel-send completion callbacks.
  -- Separate from 'wireQueue' because a fast reply can arrive and settle
  -- before its send-return observation.
  , receiver :: !(Maybe (L.AttemptKey peer))
  -- ^ Original attempt installed for the current response decoder/handler.
  -- Receive callbacks use this captured identity rather than a later FIFO head.
  , previousReceipt :: !(Maybe Time)
  -- ^ Most recent complete reply's original terminal-byte observation time.
  -- The successor uses this network boundary separately from local handling.
  , receiverReady :: !(Maybe Time)
  -- ^ Completion time of the preceding local reply handler. Successor head
  -- activation accounts for this local delay separately from 'previousReceipt'.
  , localHandler :: !Bool
  -- ^ A complete reply is awaiting or undergoing local handling. Suppresses
  -- peer expiry and successor activation while our processing prevents reception.
  , publishingInput :: !Bool
  -- ^ Input publication/local decoding may still establish an earlier terminal
  -- receipt. Reconcile expiry only after completion or a new channel-input wait.
  , reportedBlocked :: !Bool
  -- ^ Last emitted blocked/resumed state, one bit per live invocation.
  , pressure :: ![FetchLimit]
  -- ^ At most the three current pressure kinds; no event history.
  , closurePenalty :: !P.Penalty
  -- ^ Last charged closure evidence for this live registration. Pure reads
  -- age it; successful service does not reset it. Removal clears the state.
  }

-- | Immutable command plus bounded lifecycle facts. A settled fast response
-- retains only this descriptor until its outstanding send-return is observed.
data Attempt peer = Attempt
  { attemptRequest :: !LeiosFetchRequest
  , attemptWork :: !(L.Work LeiosPoint)
  , attemptClock :: !T.Clock
  , committed :: !Bool
  , settled :: !Bool
  }

-- | Original registration and invocation retained by every callback.
data Client m peer = Client
  { clientRuntime :: !(Runtime m peer)
  , clientPeer :: !peer
  , clientInvocation :: !Natural
  }

newRuntime :: IOLike m => LeiosFetchStaticEnv -> W.Readiness m -> STM m Bool -> m (Runtime m peer)
newRuntime cfg ready closing = do
  checked <- either (throwIO . InvalidConfig) pure (validateLeiosFetchStaticEnv maxIngressBytes cfg)
  facts <- uncheckedNewTVarM (RuntimeState Map.empty Map.empty L.emptyLedger Map.empty 0 0)
  pure (Runtime checked facts closing ready)

openClient ::
  (IOLike m, Ord peer) =>
  Runtime m peer ->
  peer ->
  Bool ->
  StrictTVar m (Seq L.CommandKey) ->
  STM m (Maybe RttObservation) ->
  STM m (Client m peer)
openClient rt peer big commands rtt = do
  closing <- runtimeClosing rt
  when closing (throwSTM ClientClosed)
  st <- readTVar (runtimeFacts rt)
  when (Map.member peer (sessions st)) (throwSTM SessionExists)
  let n = nextInvocation st
      session =
        Session
          n
          big
          rtt
          0
          commands
          Seq.empty
          Seq.empty
          Nothing
          Nothing
          Nothing
          False
          False
          False
          []
          P.emptyPenalty
  writeTVar
    (runtimeFacts rt)
    st{sessions = Map.insert peer session (sessions st), nextInvocation = n + 1}
  W.notifyReadiness (runtimeReady rt)
  pure (Client rt peer n)

-- | Connection cleanup owns physical leases only. The existing kernel helper
-- alone releases requested-job indices/multiplicities under its shared lock.
-- Keep the wakeup even after an earlier client finalizer removed the session:
-- kernel unregister may still be releasing logical picks under that lock.
closePeer :: (IOLike m, Ord peer) => Runtime m peer -> peer -> STM m ()
closePeer rt peer = do
  st <- readTVar (runtimeFacts rt)
  -- Admission requires the original session; teardown removes its descriptors
  -- and leases together. Late callbacks cannot recreate either resource.
  when (Map.member peer (sessions st)) $ do
    let owned (L.AttemptKey p _ _) = p == peer
    writeTVar
      (runtimeFacts rt)
      st
        { sessions = Map.delete peer (sessions st)
        , attempts = Map.filterWithKey (\key _ -> not (owned key)) (attempts st)
        , ledger = L.removePeer peer (ledger st)
        }
  W.notifyReadiness (runtimeReady rt)

-- Pending claims belong to their registered submission/receipt tasks, not to
-- the connection. Their finalizers clear them even if removal precedes writer
-- acceptance; repeated removal must not erase that ownership.

workOf :: LeiosFetchRequest -> L.Work LeiosPoint
workOf (LeiosBlockRequest req) = L.Body (lbrPoint req)
workOf (LeiosBlockTxsRequest (MkLeiosBlockTxsRequest point jobs)) =
  L.Job point (fst (NEIntMap.findMin jobs))

bytesOf :: LeiosFetchRequest -> Word32
bytesOf (LeiosBlockRequest req) = lbrOfferedSize req
bytesOf (LeiosBlockTxsRequest (MkLeiosBlockTxsRequest _ jobs)) =
  sum [n | Jobs.MkLeiosJob _ n _ <- foldr (:) [] jobs]

-- | Physical owners of this work. Settled descriptors retained for send-return
-- reconciliation are not live copies and must not keep the hedge gate closed.
-- The ledger index avoids a global lease scan. Its peer order matches the old
-- attempt-key order because at most one live copy exists per peer/work pair.
workOwners :: L.Work LeiosPoint -> RuntimeState m peer -> [L.AttemptKey peer]
workOwners work st = maybe [] Map.elems (Map.lookup work (L.owners (ledger st)))

-- | A primary has no owners; another copy requires every current owner to be
-- behind an expired unresolved FIFO head. Fresh queued/undispatched copies
-- suppress further hedges. A terminal receipt or local work is not a stall.
-- This predicate is rechecked in the same STM transaction that publishes the
-- new lease, preventing one old expiry from authorizing repeated fanout.
-- Rationale: @docs/fetch-policy/leiosfetch-improved.md#ownership-and-shutdown@.
workEligible :: Ord peer => L.Work LeiosPoint -> RuntimeState m peer -> Bool
workEligible work st = Map.notMember work (pending st) && all stalled (workOwners work st)
 where
  stalled key@(L.AttemptKey peer generation _) = case (Map.lookup peer (sessions st), Map.lookup key (attempts st)) of
    (Just session, Just _) | invocation session == generation -> blocked peer st
    _ -> False

-- | Called under the coordinator/removal lock. Reserve before publication;
-- reject by complete job size and retain the same key through local dequeue.
-- Eligibility and the ledger's byte/body/shared-count reservation are checked
-- in this transaction. Refusal consumes no ordinal, descriptor or queue entry.
-- Malformed internal request geometry and invalid configuration still throw.
enqueueRequest ::
  (IOLike m, Ord peer) =>
  Client m peer ->
  Time ->
  LeiosFetchRequest ->
  STM m (Either AdmissionRejection (L.AttemptKey peer))
enqueueRequest client@(Client rt peer _) now req = do
  st <- readTVar (runtimeFacts rt)
  closing <- runtimeClosing rt
  if closing
    then pure (Left SourceClosed)
    else case liveSession client st of
      Nothing -> pure (Left SourceClosed)
      Just session -> do
        when (nextOrdinal session == maxBound) (throwSTM OrdinalExhausted)
        let cfg = runtimeConfig rt
            work = workOf req
            bytes = bytesOf req
            key = L.AttemptKey peer (invocation session) (nextOrdinal session)
        case req of
          LeiosBlockTxsRequest (MkLeiosBlockTxsRequest _ jobs) ->
            when (NEIntMap.size jobs /= 1) (throwSTM InvalidRequestGeometry)
          _ -> pure ()
        -- Declared job sizes are untrusted. Reject protocol-invalid work without
        -- creating pressure or hiding it behind a temporary source limit.
        let beyondProtocol = case req of
              LeiosBlockTxsRequest{} -> bytes > maxLeiosTxsRequestBytesSize
              _ -> False
        if beyondProtocol
          then pure (Left ProtocolInvalid)
          else if blocked peer st
            then pure (Left SourceBlocked)
            else if not (workEligible work st)
              then pure (Left WorkIneligible)
              else case L.reserve cfg (bigLedger session) key (L.Lease work bytes) (ledger st) of
                Left reason -> pure (Left (ReservationRejected reason))
                Right leased -> do
                  sample <- connectionRtt session
                  inputs <- either (throwSTM . InvalidConfig) pure (T.snapshot cfg now sample bytes)
                  let attempt = Attempt req work (T.newClock inputs) False False
                  writeTVar
                    (runtimeFacts rt)
                    st
                      { sessions =
                          Map.insert
                            peer
                            session{nextOrdinal = nextOrdinal session + 1}
                            (sessions st)
                      , attempts = Map.insert key attempt (attempts st)
                      , ledger = leased
                      }
                  modifyTVar (queued session) (|> commandKey key)
                  pure (Right key)

-- | Dequeue the next admitted request for this invocation, discarding keys whose
-- attempts have already disappeared. Returns Nothing when the queue is empty,
-- and throws 'ClientClosed' for an obsolete client. Dequeue keeps the existing
-- lease; the caller must still perform the final dispatch check.
takeRequest ::
  (IOLike m, Ord peer) => Client m peer -> STM m (Maybe (L.AttemptKey peer, LeiosFetchRequest))
takeRequest client@(Client rt _ _) = do
  st <- readTVar (runtimeFacts rt)
  case liveSession client st of
    Nothing -> throwSTM ClientClosed
    Just session ->
      readTVar (queued session) >>= \commands -> case commands of
        Empty -> pure Nothing
        L.CommandKey generation ordinal :<| rest -> do
          let key = L.AttemptKey (clientPeer client) generation ordinal
          writeTVar (queued session) rest
          case Map.lookup key (attempts st) of
            Nothing -> takeRequest client
            Just a -> pure (Just (key, attemptRequest a))

-- | Final commitment precedes channel I/O; send failure thereafter tears down.
-- Admission already reserved this work. Commitment activates no deadline, so
-- send-return/terminal observations supply the next timing wakeup. The planner
-- separately signals when another source takes a deferred priority barrier.
commitRequest :: (IOLike m, Ord peer) => Client m peer -> L.AttemptKey peer -> STM m Bool
commitRequest client@(Client rt peer _) key = do
  st <- readTVar (runtimeFacts rt)
  closing <- runtimeClosing rt
  case (liveSession client st, Map.lookup key (attempts st)) of
    (Just session, Just attempt)
      | not closing
      , Map.member key (L.leases (ledger st))
      , not (committed attempt) -> do
          writeTVar
            (runtimeFacts rt)
            st
              { sessions =
                  Map.insert
                    peer
                    session{wireQueue = wireQueue session |> key, sendQueue = sendQueue session |> key}
                    (sessions st)
              , attempts = Map.insert key attempt{committed = True} (attempts st)
              }
          pure True
    _ -> pure False

-- | Record channel-send return for the oldest request in 'sendQueue', with its
-- continuous-pipeline classification. The reply may already have arrived or
-- settled: reconcile that original attempt without restoring its lease, and
-- drop settled descriptors once their send observation is known. Activates the
-- eligible reply head and wakes planning; obsolete invocations are ignored.
observeSend :: (IOLike m, Ord peer) => Client m peer -> Time -> STM m ()
observeSend client@(Client rt peer _) at = do
  st <- readTVar (runtimeFacts rt)
  case liveSession client st of
    Just session | key :<| rest <- sendQueue session -> do
      let continuous =
            any
              ( \k ->
                  k /= key
                    && sameInvocation key k
                    && case Map.lookup k (attempts st) of
                      Just a ->
                        committed a
                          && not (settled a)
                          && T.clockReceipt (attemptClock a) == Nothing
                      Nothing -> False
              )
              (wireQueue session)
          observed a = retainAttempt a{attemptClock = T.sent at continuous (attemptClock a)}
          next =
            st
              { sessions = Map.insert peer session{sendQueue = rest} (sessions st)
              , attempts = Map.update observed key (attempts st)
              }
      writeTVar (runtimeFacts rt) (activateHead peer next)
      W.notifyReadiness (runtimeReady rt)
    _ -> pure ()

-- | Bind the next response decoder/handler to the original attempt selected by
-- the protocol client. Throws 'ClientClosed' if this invocation or the attempt
-- has disappeared; later receive callbacks keep this captured identity.
installReceiver :: (IOLike m, Ord peer) => Client m peer -> L.AttemptKey peer -> STM m ()
installReceiver client@(Client rt peer _) key = do
  st <- readTVar (runtimeFacts rt)
  case liveSession client st of
    Just session
      | Map.member key (attempts st) ->
          writeTVar
            (runtimeFacts rt)
            st{sessions = Map.insert peer session{receiver = Just key} (sessions st)}
    _ -> throwSTM ClientClosed

-- | Retrieve the installed response identity and its request/lifecycle facts.
-- The driver uses these to fix its receive scope before decoding. Throws
-- 'ClientClosed' if the invocation, installed receiver or attempt is gone.
receiverAttempt :: (IOLike m, Ord peer) => Client m peer -> STM m (L.AttemptKey peer, Attempt peer)
receiverAttempt client@(Client rt _ _) = do
  st <- readTVar (runtimeFacts rt)
  case liveSession client st >>= receiver of
    Just key | Just attempt <- Map.lookup key (attempts st) -> pure (key, attempt)
    _ -> throwSTM ClientClosed

-- | Publish the interval in which known input is being handed to or decoded
-- by the local codec, which may establish an earlier terminal-byte receipt.
-- Clear this flag before waiting for more channel input. It defers expiry
-- reconciliation without changing the original service deadline.
publishInput :: (IOLike m, Ord peer) => Client m peer -> Bool -> STM m ()
publishInput client@(Client rt peer _) active = do
  st <- readTVar (runtimeFacts rt)
  case liveSession client st of
    Just session | publishingInput session /= active -> do
      writeTVar
        (runtimeFacts rt)
        st{sessions = Map.insert peer session{publishingInput = active} (sessions st)}
      when (not active) (W.notifyReadiness (runtimeReady rt))
    _ -> pure ()

-- | Record a fully decoded reply using its original terminal-byte read time,
-- rather than the later callback time. Enters local handling and records the
-- network boundary for the FIFO successor. Storage acceptance, durable
-- acquisition and physical-credit settlement remain separate transitions.
completeWire :: (IOLike m, Ord peer) => Client m peer -> L.AttemptKey peer -> Time -> STM m ()
completeWire client@(Client rt peer _) key at = do
  st <- readTVar (runtimeFacts rt)
  case liveSession client st of
    Just session | Just attempt <- Map.lookup key (attempts st) -> do
      writeTVar
        (runtimeFacts rt)
        st
          { sessions =
              Map.insert
                peer
                session{localHandler = True, publishingInput = False, previousReceipt = Just at}
                (sessions st)
          , attempts = Map.insert key attempt{attemptClock = T.received at (attemptClock attempt)} (attempts st)
          }
      W.notifyReadiness (runtimeReady rt)
    _ -> pure ()

-- | Finish local reply processing: release any remaining physical credit, clear
-- only unaccepted submission claims, and remove the attempt from the reply
-- FIFO. Record local readiness and activate the successor. Accepted receipt
-- claims survive, as does a settled descriptor awaiting send-return; this does
-- not itself establish durable acquisition.
finishHandler ::
  (IOLike m, Ord peer) =>
  Client m peer -> L.AttemptKey peer -> L.Work LeiosPoint -> Time -> STM m ()
finishHandler client@(Client rt peer _) key work at = do
  releaseAttempt rt key
  clearUnsubmitted rt key work
  st <- readTVar (runtimeFacts rt)
  case liveSession client st of
    Just session -> do
      let finished a = retainAttempt a{settled = True}
          next =
            st
              { sessions =
                  Map.insert
                    peer
                    session
                      { receiver = Nothing
                      , localHandler = False
                      , publishingInput = False
                      , receiverReady = Just at
                      , wireQueue = Seq.filter (/= key) (wireQueue session)
                      }
                    (sessions st)
              , attempts = Map.update finished key (attempts st)
              }
      writeTVar (runtimeFacts rt) (activateHead peer next)
      W.notifyReadiness (runtimeReady rt)
    Nothing -> pure ()

-- A fast reply can finish before channel-send return. Each of those callbacks
-- reconciles its own descriptor; neither prunes unrelated attempts.
retainAttempt :: Attempt peer -> Maybe (Attempt peer)
retainAttempt a
  | settled a, Just _ <- T.clockSent (attemptClock a) = Nothing
  | otherwise = Just a

claimUseful :: (IOLike m, Ord peer) => Runtime m peer -> L.AttemptKey peer -> STM m Bool
claimUseful rt key = do
  st <- readTVar (runtimeFacts rt)
  case Map.lookup key (attempts st) of
    Just attempt
      | Map.member key (L.leases (ledger st))
      , Map.notMember (attemptWork attempt) (pending st) -> do
          writeTVar
            (runtimeFacts rt)
            st{pending = Map.insert (attemptWork attempt) (NetworkOwner key, False) (pending st)}
          pure True
    _ -> pure False

-- | After the writer accepts a validated reply, release its physical request
-- credit and mark any pending claim owned by this attempt as accepted. Its
-- receipt task now owns settlement, even after peer removal; writer acceptance
-- alone does not establish durable acquisition.
acceptReply ::
  (IOLike m, Ord peer) =>
  Runtime m peer -> L.AttemptKey peer -> L.Work LeiosPoint -> STM m ()
acceptReply rt key work = do
  releaseAttempt rt key
  void $ modifyPending rt work (== NetworkOwner key) (\_ -> Just True)

-- | Abandon only this attempt's unaccepted claim of the captured original work.
-- Accepted claims remain the receipt task's responsibility, including after
-- session/descriptor removal. An obsolete owner cannot erase a replacement.
clearUnsubmitted ::
  (IOLike m, Ord peer) =>
  Runtime m peer -> L.AttemptKey peer -> L.Work LeiosPoint -> STM m ()
clearUnsubmitted rt key work = do
  changed <- modifyPending rt work (== NetworkOwner key) $
    \accepted -> if accepted then Just True else Nothing
  when changed (W.notifyReadiness (runtimeReady rt))

-- | Retire pending ownership for this original work/attempt when its receipt
-- task finishes. The captured work is still available after descriptor removal;
-- matching the expected owner preserves a newer claim. Logical job completion
-- remains the receipt handler's separate responsibility.
settleReceipt ::
  (IOLike m, Ord peer) =>
  Runtime m peer -> L.AttemptKey peer -> L.Work LeiosPoint -> STM m ()
settleReceipt rt key work = do
  changed <- modifyPending rt work (== NetworkOwner key) (const Nothing)
  when changed (W.notifyReadiness (runtimeReady rt))

-- All callers carry the immutable work and expected owner; none reconstruct
-- them from a descriptor that teardown or send-return may already have removed.
-- Return a cheap change fact without comparing runtime or pending-map snapshots.
modifyPending ::
  IOLike m =>
  Runtime m peer ->
  L.Work LeiosPoint ->
  (PendingOwner peer -> Bool) ->
  (Bool -> Maybe Bool) ->
  STM m Bool
modifyPending rt work owns update = do
  st <- readTVar (runtimeFacts rt)
  case Map.lookup work (pending st) of
    Just (owner, accepted) | owns owner -> case update accepted of
      Nothing -> do
        writeTVar (runtimeFacts rt) st{pending = Map.delete work (pending st)}
        pure True
      Just next | next /= accepted -> do
        writeTVar (runtimeFacts rt) st{pending = Map.insert work (owner, next) (pending st)}
        pure True
      _ -> pure False
    _ -> pure False

-- | Return only this attempt's reserved TX-byte or body-count credit, once.
-- The lease ledger makes repeated or late releases harmless. Request
-- descriptors, pending-write ownership and logical job picks are unchanged;
-- their owners settle them separately. An absent lease neither writes facts
-- nor notifies; callers that also change logical assignments must signal that
-- independent change, even when this physical release is already complete.
releaseAttempt :: (IOLike m, Ord peer) => Runtime m peer -> L.AttemptKey peer -> STM m ()
releaseAttempt rt key = do
  st <- readTVar (runtimeFacts rt)
  let (released, leased) = L.release key (ledger st)
  when released $ do
    writeTVar (runtimeFacts rt) st{ledger = leased}
    W.notifyReadiness (runtimeReady rt)

nextDeadline :: Ord peer => RuntimeState m peer -> Maybe Time
nextDeadline st = case [ at
                       | (peer, session) <- Map.toList (sessions st)
                       , not (localHandler session || publishingInput session)
                       , Just attempt <- [headAttempt peer st]
                       , not (T.clockFired (attemptClock attempt))
                       , T.clockReceipt (attemptClock attempt) == Nothing
                       , Just at <- [T.deadline (attemptClock attempt)]
                       ] of
  [] -> Nothing
  xs -> Just (minimum xs)

-- | Detached facts captured only at an actual qualifying score increment.
-- No request payload, policy table or later lookup is retained by the report.
data PenaltyCharge peer = PenaltyCharge
  { penaltyAttempt :: !(L.AttemptKey peer)
  -- ^ Original invocation-qualified request, independently of later assignment.
  , penaltyWork :: !(L.Work LeiosPoint)
  , penaltyBytes :: !Word32
  -- ^ Bounded EB/job/payload summary, containing no transaction bytes.
  , penaltyAt :: !Time
  -- ^ Supplied monotonic expiry time; a stale value cannot rewind the baseline.
  , penaltyBefore :: !P.Snapshot
  -- ^ Saved evidence and baseline before this charge.
  , penaltyEffective :: !P.Score
  -- ^ Evidence after aging/cutoff, before adding the new stall.
  , penaltyAfter :: !P.Snapshot
  -- ^ Newly saved evidence and nondecreasing baseline.
  }
  deriving (Eq, Show)

-- | Expire physical FIFO heads and capture once-only selection penalties
-- for still-needed TX work. The caller holds the shared outstanding lock;
-- callbacks never acquire it to update these scores. Clocks and detached
-- charges are updated in the same owning fold.
-- The coordinator emits the adapted reports after STM and its outer lock.
-- Rationale: @docs/fetch-policy/leiosfetch-improved.md#ownership-and-shutdown@.
expireHeadsObserved ::
  Ord peer =>
  Time ->
  (L.Work LeiosPoint -> Bool) ->
  RuntimeState m peer ->
  (RuntimeState m peer, [PenaltyCharge peer])
expireHeadsObserved now needed st =
  let (updated, backwards) = Map.foldlWithKey' expireOne (st, []) (sessions st)
   in (updated, reverse backwards)
 where
  expireOne result@(acc, reports) peer session
    | localHandler session || publishingInput session = result
    | key :<| _ <- wireQueue session
    , Just attempt <- Map.lookup key (attempts acc) =
        let (fired, clock) = T.expire now (attemptClock attempt)
            charge =
              fired && case attemptWork attempt of
                work@L.Job{} -> needed work && Map.notMember work (pending acc)
                _ -> False
            previous = closurePenalty session
            effective = P.effectiveScore now previous
            following = P.chargePenalty now previous
            penalize current = current{closurePenalty = following}
            captured =
              PenaltyCharge
                key
                (attemptWork attempt)
                (bytesOf (attemptRequest attempt))
                now
                (P.penaltySnapshot previous)
                effective
                (P.penaltySnapshot following)
            updated =
              acc
                { attempts = Map.insert key attempt{attemptClock = clock} (attempts acc)
                , sessions = if charge then Map.adjust penalize peer (sessions acc) else sessions acc
                }
         in (updated, if charge then captured : reports else reports)
    | otherwise = result

-- | Effective score at the ranking pass's captured time. Reading never
-- renews evidence or changes FIFO eligibility.
-- Rationale: @docs/fetch-policy/leiosfetch-improved.md#source-selection-and-progressive-recovery@.
closureScore :: Time -> Session m peer -> P.Score
closureScore now = P.effectiveScore now . closurePenalty

-- | Whether the peer's current reply head has expired without a complete-message
-- receipt. This gates new admission and enables recovery from other sources.
-- The condition clears once the complete reply is observed.
blocked :: Ord peer => peer -> RuntimeState m peer -> Bool
blocked peer st =
  maybe
    False
    (\a -> T.clockFired (attemptClock a) && T.clockReceipt (attemptClock a) == Nothing)
    (headAttempt peer st)

-- | Look up the first committed exchange in this peer's reply FIFO, if present.
-- It can already have a complete reply awaiting local handling; being the head
-- does not by itself imply an active service deadline.
headAttempt :: Ord peer => peer -> RuntimeState m peer -> Maybe (Attempt peer)
headAttempt peer st = do
  session <- Map.lookup peer (sessions st)
  case wireQueue session of { Empty -> Nothing; key :<| _ -> Map.lookup key (attempts st) }

-- | Supply predecessor-receipt and local-readiness observations to the current
-- FIFO head when no local handler blocks reception. Followers stay inactive.
-- Clock activation preserves any earlier activation inputs, so repeated calls
-- cannot renew an existing deadline.
activateHead :: Ord peer => peer -> RuntimeState m peer -> RuntimeState m peer
activateHead peer st = case Map.lookup peer (sessions st) of
  Just session
    | not (localHandler session)
    , key :<| _ <- wireQueue session ->
        st
          { attempts =
              Map.adjust
                ( \a -> a{attemptClock = T.activate (previousReceipt session) (receiverReady session) (attemptClock a)}
                )
                key
                (attempts st)
          }
  _ -> st

liveSession :: Ord peer => Client m peer -> RuntimeState m peer -> Maybe (Session m peer)
liveSession (Client _ peer expected) st = do
  session <- Map.lookup peer (sessions st)
  if invocation session == expected then Just session else Nothing

sameInvocation :: Eq peer => L.AttemptKey peer -> L.AttemptKey peer -> Bool
sameInvocation (L.AttemptKey p n _) (L.AttemptKey q m _) = p == q && n == m

-- | Locally discard only an unsent intent. The caller releases its existing
-- shared job pick under the outstanding lock; sent descriptors never change.
discardAttempt :: (IOLike m, Ord peer) => Runtime m peer -> L.AttemptKey peer -> STM m ()
discardAttempt rt key = do
  st <- readTVar (runtimeFacts rt)
  case Map.lookup key (attempts st) of
    Just attempt | not (committed attempt) -> do
      let (_, leased) = L.release key (ledger st)
      writeTVar
        (runtimeFacts rt)
        st
          { attempts = Map.delete key (attempts st)
          , ledger = leased
          }
      let L.AttemptKey peer generation _ = key
      case Map.lookup peer (sessions st) of
        Just session | invocation session == generation ->
          modifyTVar (queued session) (Seq.filter (/= commandKey key))
        _ -> pure ()
      W.notifyReadiness (runtimeReady rt)
    _ -> pure ()

-- | An invocation's own finalizer must not close a newer invocation retained
-- under the same peer registration. Kernel removal instead retires that whole
-- registration through 'closePeer'.
closeClient :: (IOLike m, Ord peer) => Client m peer -> STM m ()
closeClient client@(Client rt peer _) = do
  st <- readTVar (runtimeFacts rt)
  case liveSession client st of
    Just _ -> closePeer rt peer
    Nothing -> pure ()

-- | Pending local acquisition is explicitly distinct from a network attempt.
-- Both are node-owned once a writer accepts; no fake peer identity is invented.
data PendingOwner peer = NetworkOwner !(L.AttemptKey peer) | LocalOwner !Natural
  deriving (Eq, Ord, Show)

-- | Try to claim a local body import with a fresh 'LocalOwner' token. The caller
-- checks that the body is still needed; existing acquisition claims take
-- precedence. The claim suppresses new body copies while submission is owned
-- locally, without creating or refunding a network request lease.
claimLocalBody :: IOLike m => Runtime m peer -> LeiosPoint -> STM m (Maybe Natural)
claimLocalBody rt point = do
  st <- readTVar (runtimeFacts rt)
  let work = L.Body point
      token = nextLocalOwner st
  if Map.member work (pending st)
    then pure Nothing
    else do
      writeTVar
        (runtimeFacts rt)
        st{pending = Map.insert work (LocalOwner token, False) (pending st), nextLocalOwner = token + 1}
      W.notifyReadiness (runtimeReady rt)
      pure (Just token)

-- | After writer acceptance, transfer the matching local body claim to receipt
-- ownership. It must survive ordinary caller cleanup until its receipt task
-- settles; acceptance alone does not establish durable possession.
acceptLocalBody ::
  IOLike m => Runtime m peer -> LeiosPoint -> Natural -> STM m ()
acceptLocalBody rt point token =
  void $ modifyPending rt (L.Body point) (isLocalOwner token) (\_ -> Just True)

-- | Clean up a local body import that may not have reached writer acceptance.
-- Remove only unaccepted claims for this token; leave accepted claims to their
-- receipt task. Token-specific cleanup cannot erase a replacement claim.
clearLocalBody :: IOLike m => Runtime m peer -> Natural -> STM m ()
clearLocalBody rt token = do
  modifyTVar (runtimeFacts rt) $ \st ->
    st
      { pending = Map.filter (\(owner, accepted) -> not (isLocalOwner token owner) || accepted) (pending st)
      }
  W.notifyReadiness (runtimeReady rt)

-- | Retire the matching local body claim when its receipt task finishes, then
-- wake planning. The captured point and expected token preserve newer owners;
-- this releases ownership without declaring the body acquired.
settleLocalBody ::
  IOLike m => Runtime m peer -> LeiosPoint -> Natural -> STM m ()
settleLocalBody rt point token = do
  changed <- modifyPending rt (L.Body point) (isLocalOwner token) (const Nothing)
  when changed (W.notifyReadiness (runtimeReady rt))

isLocalOwner :: Natural -> PendingOwner peer -> Bool
isLocalOwner token (LocalOwner other) = token == other
isLocalOwner _ NetworkOwner{} = False

commandKey :: L.AttemptKey peer -> L.CommandKey
commandKey (L.AttemptKey _ generation ordinal) = L.CommandKey generation ordinal
