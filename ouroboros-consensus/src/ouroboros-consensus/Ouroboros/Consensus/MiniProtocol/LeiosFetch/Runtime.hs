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
  , expireHeads
  , closureScore
  , expireHeadsObserved
  , PenaltyCharge (..)
  , blocked
  , workOf
  , bytesOf
  ) where

import Control.Monad (when)
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
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Wait as W
import Ouroboros.Consensus.Util.IOLike

-- | Configuration/internal errors are explicit; a legal indivisible job can
-- never remain silently unrequestable because it exceeds a chosen byte cap.
data RequestError
  = InvalidConfig String
  | SessionExists
  | OrdinalExhausted
  | MissingAttempt
  | JobBeyondCredit
  | ClientClosed
  deriving Show

instance Exception RequestError

-- | One node-owned fact store; no additional planner or storage task.
data Runtime m peer = Runtime
  { runtimeConfig :: !LeiosFetchStaticEnv
  , runtimeFacts :: !(StrictTVar m (RuntimeState m peer))
  , runtimeClosing :: !(STM m Bool)
  -- ^ Admission observes the receipt task group closing in its publication transaction.
  , runtimeReady :: !(W.Readiness m)
  }

-- | Live client sessions, physical leases and pending-write ownership.
data RuntimeState m peer = RuntimeState
  { sessions :: !(Map peer (Session m peer))
  , attempts :: !(Map (L.AttemptKey peer) (Attempt peer))
  , ledger :: !(L.Ledger peer LeiosPoint)
  , pending :: !(Map (L.Work LeiosPoint) (PendingOwner peer, Bool))
  -- ^ False is handler-owned; True is accepted and survives peer death.
  , nextLocalOwner :: !Natural
  , nextInvocation :: !Natural
  }

-- | Connection-local FIFO and publication facts, containing no payload.
data Session m peer = Session
  { invocation :: !Natural
  , bigLedger :: !Bool
  , connectionRtt :: !(STM m (Maybe RttObservation))
  , nextOrdinal :: !Word64
  , queued :: !(StrictTVar m (Seq L.CommandKey))
  -- ^ The peer's sole command queue, also exposed by its kernel registration.
  , wireQueue :: !(Seq (L.AttemptKey peer))
  , sendQueue :: !(Seq (L.AttemptKey peer))
  , receiver :: !(Maybe (L.AttemptKey peer))
  , previousReceipt :: !(Maybe Time)
  , receiverReady :: !(Maybe Time)
  , localHandler :: !Bool
  , publishingInput :: !Bool
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
  checked <- either (throwIO . InvalidConfig) pure (validateLeiosFetchStaticEnv 60000000 cfg)
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
        Session n big rtt 0 commands Seq.empty Seq.empty Nothing Nothing Nothing False False P.emptyPenalty
  writeTVar
    (runtimeFacts rt)
    st{sessions = Map.insert peer session (sessions st), nextInvocation = n + 1}
  W.notifyReadiness (runtimeReady rt)
  pure (Client rt peer n)

-- | Connection cleanup owns physical leases only. The existing kernel helper
-- alone releases requested-job indices/multiplicities under its shared lock.
closePeer :: (IOLike m, Ord peer) => Runtime m peer -> peer -> STM m ()
closePeer rt peer = do
  st <- readTVar (runtimeFacts rt)
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
workOwners :: L.Work LeiosPoint -> RuntimeState m peer -> [L.AttemptKey peer]
workOwners work st = [key | (key, lease) <- Map.toAscList (L.leases (ledger st)), L.leaseWork lease == work]

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
enqueueRequest ::
  (IOLike m, Ord peer) =>
  Client m peer -> Time -> LeiosFetchRequest -> STM m (Maybe (L.AttemptKey peer))
enqueueRequest client@(Client rt peer _) now req = do
  st <- readTVar (runtimeFacts rt)
  closing <- runtimeClosing rt
  if closing
    then pure Nothing
    else case liveSession client st of
      Nothing -> pure Nothing
      Just session -> do
        when (nextOrdinal session == maxBound) (throwSTM OrdinalExhausted)
        let cfg = runtimeConfig rt
            cap =
              if bigLedger session
                then maxRequestedBytesSizePerBigLedgerPeer cfg
                else maxRequestedBytesSizePerPeer cfg
            work = workOf req
            bytes = bytesOf req
            key = L.AttemptKey peer (invocation session) (nextOrdinal session)
        case req of
          LeiosBlockTxsRequest (MkLeiosBlockTxsRequest _ jobs) ->
            when (NEIntMap.size jobs /= 1 || bytes > cap || bytes > 500000) (throwSTM JobBeyondCredit)
          _ -> pure ()
        if blocked peer st
          || not (workEligible work st)
          || length [() | (L.AttemptKey p _ _, _) <- Map.toList (L.leases (ledger st)), p == peer] >= 150
          then pure Nothing
          else case L.reserve cfg (bigLedger session) key (L.Lease work bytes) (ledger st) of
            Left _ -> pure Nothing
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
              pure (Just key)

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
          W.notifyReadiness (runtimeReady rt)
          pure True
    _ -> pure False

observeSend :: (IOLike m, Ord peer) => Client m peer -> Time -> STM m ()
observeSend client@(Client rt peer _) at = do
  st <- readTVar (runtimeFacts rt)
  case liveSession client st of
    Just session | key :<| rest <- sendQueue session -> do
      let continuous =
            any
              ( \(k, a) ->
                  k /= key
                    && committed a
                    && not (settled a)
                    && T.clockReceipt (attemptClock a) == Nothing
                    && sameInvocation key k
              )
              (Map.toList (attempts st))
          updated = Map.adjust (\a -> a{attemptClock = T.sent at continuous (attemptClock a)}) key (attempts st)
          retained = Map.filter (\a -> not (settled a) || T.clockSent (attemptClock a) == Nothing) updated
          next = st{sessions = Map.insert peer session{sendQueue = rest} (sessions st), attempts = retained}
      writeTVar (runtimeFacts rt) (activateHead peer next)
      W.notifyReadiness (runtimeReady rt)
    _ -> pure ()

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

receiverAttempt :: (IOLike m, Ord peer) => Client m peer -> STM m (L.AttemptKey peer, Attempt peer)
receiverAttempt client@(Client rt _ _) = do
  st <- readTVar (runtimeFacts rt)
  case liveSession client st >>= receiver of
    Just key | Just attempt <- Map.lookup key (attempts st) -> pure (key, attempt)
    _ -> throwSTM ClientClosed

-- | Protect the tiny timestamp/publication interval. It never covers a blocked
-- channel read, decoding, hashing or storage and cannot renew service patience.
publishInput :: (IOLike m, Ord peer) => Client m peer -> Bool -> STM m ()
publishInput client@(Client rt peer _) active = do
  st <- readTVar (runtimeFacts rt)
  case liveSession client st of
    Just session -> do
      writeTVar
        (runtimeFacts rt)
        st{sessions = Map.insert peer session{publishingInput = active} (sessions st)}
      when (publishingInput session && not active) (W.notifyReadiness (runtimeReady rt))
    Nothing -> pure ()

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

finishHandler :: (IOLike m, Ord peer) => Client m peer -> L.AttemptKey peer -> Time -> STM m ()
finishHandler client@(Client rt peer _) key at = do
  releaseAttempt rt key
  clearUnsubmitted rt key
  st <- readTVar (runtimeFacts rt)
  case liveSession client st of
    Just session -> do
      let mark a = a{settled = True}
          retained =
            Map.filter
              (\a -> not (settled a) || T.clockSent (attemptClock a) == Nothing)
              (Map.adjust mark key (attempts st))
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
              , attempts = retained
              }
      writeTVar (runtimeFacts rt) (activateHead peer next)
      W.notifyReadiness (runtimeReady rt)
    Nothing -> pure ()

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

acceptReply :: (IOLike m, Ord peer) => Runtime m peer -> L.AttemptKey peer -> STM m ()
acceptReply rt key = do
  releaseAttempt rt key
  st <- readTVar (runtimeFacts rt)
  writeTVar
    (runtimeFacts rt)
    st
      { pending =
          Map.map (\(owner, accepted) -> (owner, accepted || owner == NetworkOwner key)) (pending st)
      }

clearUnsubmitted :: (IOLike m, Ord peer) => Runtime m peer -> L.AttemptKey peer -> STM m ()
clearUnsubmitted rt key = do
  st <- readTVar (runtimeFacts rt)
  writeTVar
    (runtimeFacts rt)
    st{pending = Map.filter (\(owner, accepted) -> owner /= NetworkOwner key || accepted) (pending st)}
  W.notifyReadiness (runtimeReady rt)

settleReceipt :: (IOLike m, Ord peer) => Runtime m peer -> L.AttemptKey peer -> STM m ()
settleReceipt rt key = do
  st <- readTVar (runtimeFacts rt)
  writeTVar (runtimeFacts rt) st{pending = Map.filter ((/= NetworkOwner key) . fst) (pending st)}
  W.notifyReadiness (runtimeReady rt)

releaseAttempt :: (IOLike m, Ord peer) => Runtime m peer -> L.AttemptKey peer -> STM m ()
releaseAttempt rt key = do
  st <- readTVar (runtimeFacts rt)
  let (_, leased) = L.release key (ledger st)
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

-- | Expire physical FIFO heads as before, charging a selection penalty only
-- on the first expiry of still-needed TX work. The caller holds the shared
-- outstanding lock; callbacks never acquire it to update these scores.
-- Rationale: @docs/fetch-policy/leiosfetch-improved.md#ownership-and-shutdown@.
expireHeads ::
  Ord peer => Time -> (L.Work LeiosPoint -> Bool) -> RuntimeState m peer -> RuntimeState m peer
expireHeads now wanted = fst . expireHeadsObserved now wanted

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

-- | Update clocks and capture once-only charges in the same owning fold.
-- The coordinator emits the adapted reports after STM and its outer lock.
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
-- Rationale: @docs/fetch-policy/leiosfetch-improved.md#ownership-and-shutdown@.
closureScore :: Time -> Session m peer -> P.Score
closureScore now = P.effectiveScore now . closurePenalty

blocked :: Ord peer => peer -> RuntimeState m peer -> Bool
blocked peer st =
  maybe
    False
    (\a -> T.clockFired (attemptClock a) && T.clockReceipt (attemptClock a) == Nothing)
    (headAttempt peer st)

headAttempt :: Ord peer => peer -> RuntimeState m peer -> Maybe (Attempt peer)
headAttempt peer st = do
  session <- Map.lookup peer (sessions st)
  case wireQueue session of { Empty -> Nothing; key :<| _ -> Map.lookup key (attempts st) }

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
      mapM_
        (\session -> modifyTVar (queued session) (Seq.filter (/= commandKey key)))
        (Map.elems (sessions st))
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

acceptLocalBody :: IOLike m => Runtime m peer -> Natural -> STM m ()
acceptLocalBody rt token = modifyTVar (runtimeFacts rt) $ \st ->
  st
    { pending =
        Map.map (\(owner, accepted) -> (owner, accepted || isLocalOwner token owner)) (pending st)
    }

clearLocalBody :: IOLike m => Runtime m peer -> Natural -> STM m ()
clearLocalBody rt token = do
  modifyTVar (runtimeFacts rt) $ \st ->
    st
      { pending = Map.filter (\(owner, accepted) -> not (isLocalOwner token owner) || accepted) (pending st)
      }
  W.notifyReadiness (runtimeReady rt)

settleLocalBody :: IOLike m => Runtime m peer -> Natural -> STM m ()
settleLocalBody rt token = do
  modifyTVar (runtimeFacts rt) $ \st -> st{pending = Map.filter (not . isLocalOwner token . fst) (pending st)}
  W.notifyReadiness (runtimeReady rt)

isLocalOwner :: Natural -> PendingOwner peer -> Bool
isLocalOwner token (LocalOwner other) = token == other
isLocalOwner _ NetworkOwner{} = False

commandKey :: L.AttemptKey peer -> L.CommandKey
commandKey (L.AttemptKey _ generation ordinal) = L.CommandKey generation ordinal
