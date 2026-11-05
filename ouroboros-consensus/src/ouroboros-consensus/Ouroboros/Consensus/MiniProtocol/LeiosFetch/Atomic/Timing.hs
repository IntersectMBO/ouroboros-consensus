{-# LANGUAGE NamedFieldPuns #-}

-- | Pure FIFO timing/event reconciliation, separate from the admission ledger
-- and storage ownership. This neither activates a runtime policy nor refunds wire
-- reservations. Register admitted exchanges before sending; reconcile observed
-- events before making timeout decisions on every coordinator wake.
--
-- Rationale (repository-root paths), with symbol-to-contract maps:
--
-- * @docs/fetch-policy/cancellation-contract.md#receiver-observable-deadline-activation@
-- * @docs/fetch-policy/cancellation-contract.md#validation-descriptors-and-progress-events@
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Timing
  ( TimingState
  , TimingKind (..)
  , TimerToken (..)
  , ValidationToken (..)
  , ValidationOutcome (..)
  , TimingEventError (..)
  , Expiry (..)
  , newTimingState
  , registerTiming
  , recordSend
  , recordTerminal
  , beginValidation
  , finishValidation
  , withdrawTiming
  , noteFailureHandled
  , reconcileTiming
  , wakeTimer
  , nextDeadline
  , activeDeadline
  , upstreamBlocked
  , rescuableRequests
  , retireTimingSession
  ) where

import Control.Monad (unless, when)
import Data.Foldable (toList)
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq
import Numeric.Natural (Natural)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Deadline
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity
import Ouroboros.Consensus.Util.IOLike (Time (Time))

data TimingKind = TrackData !TimingSnapshot | TrackAck !RttAllowance
  deriving (Eq, Show)

-- | Identity of one FIFO-head timer: its complete request key and non-wrapping
-- activation generation. 'wakeTimer' matches both so a delayed wake cannot
-- expire another head or an attempt in a replacement peer/session.
--
-- Rationale: @docs/fetch-policy/cancellation-contract.md#receiver-observable-deadline-activation@.
data TimerToken peer = TimerToken !(RequestKey peer) !Natural deriving (Eq, Show)

-- | Identity of one bounded decode/validation step for a particular request.
-- The non-wrapping generation distinguishes successive steps even on the same
-- request; 'finishValidation' rejects callbacks for old steps or other sessions.
--
-- Rationale: @docs/fetch-policy/cancellation-contract.md#validation-descriptors-and-progress-events@.
data ValidationToken peer = ValidationToken !(RequestKey peer) !Natural deriving (Eq, Show)

-- | A completed decode/validation step either needs more input or proves the
-- terminal at its input-span timestamp. An incomplete step never renews patience.
data ValidationOutcome = NeedMoreInput | ValidTerminal !Time deriving (Eq, Show)

data TimerStatus = Armed | AwaitingValidation | Fired deriving (Eq, Show)

-- | Timer state attached to the current FIFO head: its activation token,
-- absolute monotonic expiry and handling status. Firing or awaiting local
-- validation changes only the status; the token and expiry remain fixed until
-- the entry retires. Only armed timers are returned by 'nextDeadline'.
--
-- Rationale: @docs/fetch-policy/cancellation-contract.md#receiver-observable-deadline-activation@.
data Active peer = Active !(TimerToken peer) !Time !TimerStatus deriving (Eq, Show)

-- | Observations and policy disposition for one admitted data or control
-- exchange. Logical withdrawal or a handled failure leaves it in the FIFO until
-- timing retirement. Only the head can carry an active timer; send and validated
-- terminal notifications may arrive in either order.
--
-- Rationale: @docs/fetch-policy/cancellation-contract.md#receiver-observable-deadline-activation@.
data Entry peer = Entry
  { entryKey :: !(RequestKey peer)
  -- ^ Immutable peer-registration, fetch-session and wire-ordinal identity used
  -- to correlate every observation and callback with this exchange.
  , entryKind :: !TimingKind
  -- ^ Fixed data timing snapshot or ACK RTT allowance. Later RTT observations
  -- and backoff changes affect other attempts, not this entry.
  , entrySent :: !(Maybe Time)
  -- ^ Monotonic observation after ordinary channel send returns. May still be
  -- absent when a fast reply's terminal observation has already been recorded.
  , entryTerminal :: !(Maybe Time)
  -- ^ Last required input span's monotonic receipt time for a validated wire
  -- terminal. Partial payload progress and durable storage are separate events.
  , entryValidation :: !(Maybe (ValidationToken peer, Time))
  -- ^ Token and earliest unprocessed input observation for the current bounded
  -- decode/validation step. Pre-deadline input defers an expiry decision until
  -- the step settles, without proving completion or moving the deadline.
  , entryWanted :: !Bool
  -- ^ Whether this data attempt still contains needed work. Withdrawal clears
  -- it without retiring the wire exchange or dropping its head timer; ignored
  -- for ACKs.
  , entryFailureHandled :: !Bool
  -- ^ Monotone flag recording that a data failure was already accounted for,
  -- preventing a retained wire head from charging that failure again.
  , entryActive :: !(Maybe (Active peer))
  -- ^ Fixed timer token, expiry and handling status, present only at the FIFO
  -- head after its send observation is known. Retained after firing or while
  -- awaiting local validation, until the exchange retires.
  }
  deriving (Eq, Show)

-- | FIFO timing ledger for one fetch session within a peer registration.
-- 'reconcileTiming' processes observed events before due timers, retaining
-- predecessor receipt boundaries and generation counters for subsequent heads.
-- Admission, wire reservations and storage ownership have separate ledgers.
--
-- Rationale: @docs/fetch-policy/cancellation-contract.md#receiver-observable-deadline-activation@.
data TimingState peer = TimingState
  { timingLifetime :: !(LeiosPeerLifetimeId peer)
  -- ^ Peer-registration scope shared by every key in this state. A later
  -- registration of the same peer must use a different lifetime identity.
  , timingSession :: !FetchSessionToken
  -- ^ Fetch mini-protocol invocation within that registration. Restarting the
  -- mini-protocol starts a new identity domain even if the connection survives.
  , timingNextOrdinal :: !Natural
  -- ^ Next consecutive wire ordinal expected by 'registerTiming'. Mirrors
  -- admission order without allocating IDs itself; Natural prevents wraparound.
  , timingNextTimer :: !Natural
  -- ^ Next non-wrapping timer generation, allocated when a FIFO head activates.
  -- Combined with its request key to reject stale timer wakes.
  , timingNextValidation :: !Natural
  -- ^ Next non-wrapping validation generation, allocated for each bounded
  -- processing step so an old callback cannot settle a later step.
  , timingQueue :: !(Seq (Entry peer))
  -- ^ Exchanges in admission/FIFO order awaiting timing retirement. An entry
  -- with a known terminal remains here until its send observation is also known.
  , timingPreviousTerminal :: !(Maybe Time)
  -- ^ Terminal receipt time of the last exchange retired by reconciliation,
  -- including ACKs. Supplies the next exchange's FIFO head-time boundary.
  , timingPreviousDataTerminal :: !(Maybe Time)
  -- ^ Terminal receipt time of the last retired data reply. ACK retirement
  -- leaves it unchanged; used to classify fresh/drained versus continuous data.
  , timingLastReconcile :: !(Maybe Time)
  -- ^ Last coordinator time passed to 'reconcileTiming', used to reject a
  -- backwards local clock. It is not a request's activation time or deadline.
  , timingClosed :: !Bool
  -- ^ Set on session retirement, which clears timing entries and rejects new
  -- registrations and observations. Wire/storage cleanup belongs to its owners.
  }
  deriving (Eq, Show)

data TimingEventError
  = ClosedTimingSession
  | WrongTimingSession
  | NonConsecutiveTimingKey
  | UnknownTimingKey
  | DuplicateSend
  | DuplicateTerminal
  | ValidationAlreadyPending
  | UnknownValidation
  | InvalidObservationTime
  | TimeWentBackwards
  | FutureObservation
  | MissedObservationBoundary
  deriving (Eq, Show)

data Expiry peer = Expiry
  { expiryToken :: !(TimerToken peer)
  , expiryAt :: !Time
  , chargeDataFailure :: !Bool
  -- ^ False for ACKs, withdrawn attempts and previously handled failures.
  }
  deriving (Eq, Show)

newTimingState :: LeiosPeerLifetimeId peer -> FetchSessionToken -> TimingState peer
newTimingState lifetime session = TimingState lifetime session 0 0 0 Seq.empty Nothing Nothing Nothing False

-- | Registration mirrors the @Atomic.Admission@ ledger's order; it starts no
-- clock and consumes no additional capacity. That ledger owns admission
-- decisions and wire reservations.
registerTiming ::
  Eq peer =>
  RequestKey peer -> TimingKind -> TimingState peer -> Either TimingEventError (TimingState peer)
registerTiming key@(RequestKey _ _ (WireOrdinal ordinal)) kind s = do
  checkOpenKey key s
  unless (fromIntegral ordinal == timingNextOrdinal s) (Left NonConsecutiveTimingKey)
  pure
    s
      { timingNextOrdinal = timingNextOrdinal s + 1
      , timingQueue = timingQueue s |> Entry key kind Nothing Nothing Nothing True False Nothing
      }

-- | Notification of the ordinary channel's send-return timestamp, which may
-- arrive after a valid terminal notification. Neither event can create a record.
recordSend ::
  Eq peer => RequestKey peer -> Time -> TimingState peer -> Either TimingEventError (TimingState peer)
recordSend key at = modifyEntry key $ \e -> do
  validTime at
  when (entrySent e /= Nothing) (Left DuplicateSend)
  pure e{entrySent = Just at}

recordTerminal ::
  Eq peer => RequestKey peer -> Time -> TimingState peer -> Either TimingEventError (TimingState peer)
recordTerminal key at = modifyEntry key $ \e -> do
  validTime at
  when (entryTerminal e /= Nothing) (Left DuplicateTerminal)
  when (entryValidation e /= Nothing) (Left ValidationAlreadyPending)
  case entryActive e of
    Just (Active _ deadline Fired) | at <= deadline -> Left MissedObservationBoundary
    _ -> pure ()
  pure e{entryTerminal = Just at}

-- | Pin a bounded decode/validation step to its immutable request. If that step
-- includes pre-deadline input, expiry awaits its outcome without moving the
-- deadline. The receive adapter must publish/settle this boundary reliably and
-- bound local work.
-- No partial byte sequence is classified as a validated terminal.
beginValidation ::
  Eq peer =>
  RequestKey peer ->
  Time ->
  TimingState peer ->
  Either TimingEventError (ValidationToken peer, TimingState peer)
beginValidation key oldestInputAt s = do
  validTime oldestInputAt
  let token = ValidationToken key (timingNextValidation s)
  s' <-
    modifyEntry
      key
      ( \e -> do
          when (entryValidation e /= Nothing) (Left ValidationAlreadyPending)
          when (entryTerminal e /= Nothing) (Left DuplicateTerminal)
          pure e{entryValidation = Just (token, oldestInputAt)}
      )
      s
  pure (token, s'{timingNextValidation = timingNextValidation s + 1})

finishValidation ::
  Eq peer =>
  ValidationToken peer ->
  ValidationOutcome ->
  TimingState peer ->
  Either TimingEventError (TimingState peer)
finishValidation token@(ValidationToken key _) outcome = modifyEntry key $ \e -> do
  oldest <- case entryValidation e of
    Just (current, at) | current == token -> Right at
    _ -> Left UnknownValidation
  terminal <- case outcome of
    NeedMoreInput -> Right Nothing
    ValidTerminal at -> do
      validTime at
      when (at < oldest) (Left InvalidObservationTime)
      pure (Just at)
  pure e{entryValidation = Nothing, entryTerminal = terminal}

-- Neither logical withdrawal nor a previous failure retires a wire head or
-- drops its fixed deadline. Failure handling is monotone for this attempt.
withdrawTiming ::
  Eq peer => RequestKey peer -> TimingState peer -> Either TimingEventError (TimingState peer)
withdrawTiming key = modifyEntry key (\e -> Right e{entryWanted = False})

noteFailureHandled ::
  Eq peer => RequestKey peer -> TimingState peer -> Either TimingEventError (TimingState peer)
noteFailureHandled key = modifyEntry key (\e -> Right e{entryFailureHandled = True})

-- | Consume known terminals and all deadlines due at this wake. Valid completion
-- at or before the deadline wins; late completion emits the due timeout once
-- before retiring. Only one FIFO head can have an active timer. Observed times,
-- never this function's later execution time, activate successor deadlines.
reconcileTiming ::
  Time ->
  TimingState peer ->
  Either TimingEventError ([Expiry peer], TimingState peer)
reconcileTiming now s = do
  validTime now
  when (timingClosed s) (Left ClosedTimingSession)
  when (maybe False (> now) (timingLastReconcile s)) (Left TimeWentBackwards)
  when (any (> now) observations) (Left FutureObservation)
  (events, s') <- go s
  pure (events, s'{timingLastReconcile = Just now})
 where
  observations =
    concat
      [ maybe [] pure (entrySent e)
          ++ maybe [] pure (entryTerminal e)
          ++ maybe [] (pure . snd) (entryValidation e)
      | e <- toList (timingQueue s)
      ]
  go state = case timingQueue state of
    Empty -> Right ([], state)
    e :<| rest -> case entrySent e of
      Nothing -> Right ([], state)
      Just sent -> do
        let (active, activated) = activate sent e state
            Active token deadline status = active
            activeEntry = e{entryActive = Just active}
        case entryTerminal activeEntry of
          Just terminal -> do
            when (maybe False (> terminal) (timingPreviousTerminal state)) (Left InvalidObservationTime)
            let (events, _) = expire token deadline status (terminal > deadline) activeEntry
                retired =
                  activated
                    { timingQueue = rest
                    , timingPreviousTerminal = Just terminal
                    , timingPreviousDataTerminal = case entryKind activeEntry of
                        TrackData{} -> Just terminal
                        TrackAck{} -> timingPreviousDataTerminal state
                    }
            (laterEvents, final) <- go retired
            pure (events ++ laterEvents, final)
          Nothing -> do
            let (events, settled) =
                  if now < deadline || status == Fired
                    then ([], activeEntry)
                    else case entryValidation activeEntry of
                      Just (_, at)
                        | at <= deadline ->
                            ([], activeEntry{entryActive = Just (Active token deadline AwaitingValidation)})
                      _ -> expire token deadline status True activeEntry
            pure (events, activated{timingQueue = settled Seq.<| rest})
  activate sent e state = case entryActive e of
    Just active -> (active, state)
    Nothing ->
      let token = TimerToken (entryKey e) (timingNextTimer state)
          deadline = case entryKind e of
            TrackData snapshot ->
              deadlineExpiresAt $
                dataDeadline snapshot sent (timingPreviousTerminal state) (timingPreviousDataTerminal state)
            TrackAck rtt -> ackDeadline rtt sent (timingPreviousTerminal state)
       in (Active token deadline Armed, state{timingNextTimer = timingNextTimer state + 1})
  expire token deadline status due e
    | not due || status == Fired = ([], e)
    | otherwise =
        let charge = case entryKind e of
              TrackData{} -> entryWanted e && not (entryFailureHandled e)
              TrackAck{} -> False
         in ( [Expiry token deadline charge]
            , e
                { entryActive = Just (Active token deadline Fired)
                , entryFailureHandled = entryFailureHandled e || charge
                }
            )

-- | A stale registration/session/generation wake cannot expire a replacement.
-- Runtime integration still reconciles newly observed events on every wake.
wakeTimer ::
  Eq peer =>
  TimerToken peer ->
  Time ->
  TimingState peer ->
  Either TimingEventError ([Expiry peer], TimingState peer)
wakeTimer token now s = case activeDeadline s of
  Just (current, _) | token == current -> reconcileTiming now s
  _ -> Right ([], s)

activeDeadline :: TimingState peer -> Maybe (TimerToken peer, Time)
activeDeadline s = case timingQueue s of
  Entry{entryActive = Just (Active token deadline _)} :<| _ -> Just (token, deadline)
  _ -> Nothing

-- | Fired or locally deferred deadlines are not repeatedly registered in the
-- past. A validation outcome must signal readiness to resume reconciliation.
nextDeadline :: TimingState peer -> Maybe (TimerToken peer, Time)
nextDeadline s = case timingQueue s of
  Entry{entryActive = Just (Active token deadline Armed)} :<| _ -> Just (token, deadline)
  _ -> Nothing

upstreamBlocked :: TimingState peer -> Bool
upstreamBlocked s = case timingQueue s of
  Entry{entryActive = Just (Active _ _ Fired)} :<| _ -> True
  _ -> False

-- | Eligibility only: the calling fetch policy must enforce per-EB rescue
-- limits and admit replacements through @Atomic.Admission@. This starts no
-- service clock for queued work and releases no old wire debt.
rescuableRequests :: TimingState peer -> [RequestKey peer]
rescuableRequests s
  | not (upstreamBlocked s) = []
  | otherwise =
      [ entryKey e
      | e <- toList (timingQueue s)
      , TrackData{} <- [entryKind e]
      , entryWanted e
      , entryTerminal e == Nothing
      ]

retireTimingSession :: TimingState peer -> TimingState peer
retireTimingSession s = s{timingClosed = True, timingQueue = Seq.empty}

checkOpenKey :: Eq peer => RequestKey peer -> TimingState peer -> Either TimingEventError ()
checkOpenKey (RequestKey lifetime session _) s = do
  when (timingClosed s) (Left ClosedTimingSession)
  unless (lifetime == timingLifetime s && session == timingSession s) (Left WrongTimingSession)

modifyEntry ::
  Eq peer =>
  RequestKey peer ->
  (Entry peer -> Either TimingEventError (Entry peer)) ->
  TimingState peer ->
  Either TimingEventError (TimingState peer)
modifyEntry key f s = do
  checkOpenKey key s
  case Seq.findIndexL ((== key) . entryKey) (timingQueue s) of
    Nothing -> Left UnknownTimingKey
    Just index -> do
      updated <- f (Seq.index (timingQueue s) index)
      pure s{timingQueue = Seq.update index updated (timingQueue s)}

validTime :: Time -> Either TimingEventError ()
validTime at = when (at < Time 0) (Left InvalidObservationTime)
