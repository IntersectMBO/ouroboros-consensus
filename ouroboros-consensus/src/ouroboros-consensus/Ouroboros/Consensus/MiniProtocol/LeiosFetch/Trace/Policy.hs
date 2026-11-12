{-# LANGUAGE BangPatterns #-}

-- | Detached operation reports, emitted after their STM and enclosing lock.
-- No event queue or retained diagnostic history is introduced.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace.Policy
  ( Observation (..)
  , emitObservations
  , expiryChanges
  , forceObservations
  , requestSummary
  , requestSummaryOf
  , usageSummary
  , admitted
  , penaltyChanges
  , transition
  , headChanges
  , blockedChanges
  , pressureChange
  , headSummary
  ) where

import Control.Monad (forM, when)
import Control.Tracer (Tracer, traceWith)
import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Config
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Request (LeiosFetchRequest)
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.RequestState as L
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Runtime as R
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Timing as T
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace
import Ouroboros.Consensus.Util.IOLike

-- | The invocation key is captured now; it is never looked up by a tracer.
data Observation peer
  = Routine !(L.AttemptKey peer) !FetchRoutine
  | Detail !(L.AttemptKey peer) !FetchDetail

requestSummary :: L.AttemptKey peer -> R.Attempt peer -> FetchRequest
requestSummary key attempt = requestSummaryOf key (R.attemptRequest attempt)

requestSummaryOf :: L.AttemptKey peer -> LeiosFetchRequest -> FetchRequest
requestSummaryOf (L.AttemptKey _ _ ordinal) req =
  let !bytes = R.bytesOf req
   in case R.workOf req of
        L.Body point -> FetchRequest ordinal point Nothing bytes
        L.Job point jid -> let !n = fromIntegral jid in FetchRequest ordinal point (Just n) bytes

usageSummary :: Ord peer => R.Runtime m peer -> peer -> R.RuntimeState m peer -> FetchUsage
usageSummary rt peer state =
  let L.Usage !bytes !bodies = L.usage peer (R.ledger state)
      cfg = R.runtimeConfig rt
      !limit = case Map.lookup peer (R.sessions state) of
        Just session | R.bigLedger session -> maxRequestedBytesSizePerBigLedgerPeer cfg
        _ -> maxRequestedBytesSizePerPeer cfg
   in FetchUsage bytes bodies limit (maxBodiesPerPeer cfg)

admitted ::
  (IOLike m, Ord peer) =>
  R.Runtime m peer -> Time -> Maybe FetchSelection -> L.AttemptKey peer -> STM m [Observation peer]
admitted rt now selected key@(L.AttemptKey peer _ _) = do
  state <- readTVar (R.runtimeFacts rt)
  pure $! forceObservations $ case Map.lookup key (R.attempts state) of
    Nothing -> []
    Just attempt ->
      let !summary = requestSummary key attempt
          !used = usageSummary rt peer state
          T.Snapshot !rtt !transfer !cold !source = T.clockSnapshot (R.attemptClock attempt)
          !budget = FetchBudget (minimumServiceRate (R.runtimeConfig rt)) rtt transfer cold source
       in [Detail key (FetchAdmitted summary budget now used selected)]

-- | Adapt only the facts captured at the charge transaction; blocked status
-- and hedge admissions cannot establish a qualifying penalty.
penaltyChanges :: [R.PenaltyCharge peer] -> [Observation peer]
penaltyChanges = forceObservations . map report
 where
  report (R.PenaltyCharge key@(L.AttemptKey _ _ ordinal) work bytes at before effective following) =
    let !req = case work of
          L.Body point -> FetchRequest ordinal point Nothing bytes
          L.Job point jid -> FetchRequest ordinal point (Just (fromIntegral jid)) bytes
     in Routine key (FetchPenaltyCharged req at before effective following)

-- | Remember only one emitted bit per live invocation. A known wire terminal
-- resumes it even while its local storage handler is still running.
blockedChanges :: (IOLike m, Ord peer) => R.Runtime m peer -> STM m [Observation peer]
blockedChanges rt = do
  state <- readTVar (R.runtimeFacts rt)
  results <- forM (Map.toList (R.sessions state)) $ \(peer, session) -> do
    let !blocked = R.blocked peer state
        changed = blocked /= R.reportedBlocked session
        key = L.AttemptKey peer (R.invocation session) 0
    pure
      ( peer
      , session{R.reportedBlocked = blocked}
      , [Routine key (FetchUpstreamChanged blocked) | changed]
      )
  when (any (not . null . (\(_, _, events) -> events)) results) $
    writeTVar
      (R.runtimeFacts rt)
      state{R.sessions = Map.fromList [(peer, session) | (peer, session, _) <- results]}
  pure $! forceObservations (concat [events | (_, _, events) <- results])

-- | Pressure aggregates at most three bits per live peer, and is reported only
-- when otherwise-eligible work changes that state. No per-scan wait event.
pressureChange ::
  (IOLike m, Ord peer) => R.Runtime m peer -> peer -> [FetchLimit] -> STM m [Observation peer]
pressureChange rt peer desired = do
  state <- readTVar (R.runtimeFacts rt)
  case Map.lookup peer (R.sessions state) of
    Just session | R.pressure session /= fresh -> do
      let !used = usageSummary rt peer state
          key = L.AttemptKey peer (R.invocation session) 0
      writeTVar
        (R.runtimeFacts rt)
        state{R.sessions = Map.insert peer session{R.pressure = fresh} (R.sessions state)}
      pure $! forceObservations [Detail key (FetchPressureChanged fresh used)]
    _ -> pure []
 where
  fresh = forceLimits (sort (nub desired))
  forceLimits [] = []
  forceLimits (x : xs) = let !rest = forceLimits xs in x `seq` (x : rest)

-- | Only the current FIFO head, detached from the request's commitments/maps.
headSummary :: Ord peer => peer -> R.RuntimeState m peer -> Maybe (L.AttemptKey peer, FetchDetail)
headSummary peer state = do
  session <- Map.lookup peer (R.sessions state)
  key Seq.:<| _ <- pure (R.wireQueue session)
  attempt <- Map.lookup key (R.attempts state)
  limit <- T.deadline (R.attemptClock attempt)
  (sent, continuous) <- T.clockSent (R.attemptClock attempt)
  (prior, ready) <- T.clockHead (R.attemptClock attempt)
  if R.localHandler session || T.clockReceipt (R.attemptClock attempt) /= Nothing
    then Nothing
    else pure ()
  let !summary = requestSummary key attempt
      !start =
        maximum
          ( sent
              : [at | Just at <- [prior, ready]]
              ++ [addTime (T.snapshotRtt (T.clockSnapshot (R.attemptClock attempt))) sent]
          )
  pure (key, FetchHeadActivated summary start limit continuous)

transition ::
  (IOLike m, Ord peer) =>
  R.Runtime m peer ->
  L.AttemptKey peer ->
  LeiosFetchRequest ->
  FetchPhase ->
  Time ->
  STM m (Observation peer)
transition rt key@(L.AttemptKey peer _ _) req phase at = do
  state <- readTVar (R.runtimeFacts rt)
  let !summary = requestSummaryOf key req
      !used = usageSummary rt peer state
  let event = Detail key (FetchTransition summary phase at used)
  forceObservations [event] `seq` pure event

headChanges ::
  Eq peer =>
  Maybe (L.AttemptKey peer, FetchDetail) ->
  Maybe (L.AttemptKey peer, FetchDetail) ->
  [Observation peer]
headChanges old new
  | old == new = []
  | otherwise = maybe [] (\(key, event) -> [Detail key event]) new

-- | Materialize a returned batch before leaving its owning transaction. This
-- prevents an unevaluated selector/hedge cause from retaining an old fact map
-- through a delayed production tracer. It is not an event-history buffer.
forceObservations :: [Observation peer] -> [Observation peer]
forceObservations events = foldr (\event rest -> detach event `seq` rest) () events `seq` events
 where
  detach (Routine key event) = key `seq` detachFetchRoutine event
  detach (Detail key event) = key `seq` detachFetchDetail event

-- | Return original physical head expiries, independently of whether that
-- particular expiry qualifies for a still-needed TX service penalty.
expiryChanges ::
  Ord peer =>
  R.Runtime m peer -> Time -> R.RuntimeState m peer -> R.RuntimeState m peer -> [Observation peer]
expiryChanges rt now before following =
  forceObservations
    [ Detail
        key
        (FetchTransition (requestSummary key attempt) FetchSoftExpired now (usageSummary rt peer following))
    | (key@(L.AttemptKey peer _ _), attempt) <- Map.toList (R.attempts following)
    , T.clockFired (R.attemptClock attempt)
    , Just old <- [Map.lookup key (R.attempts before)]
    , not (T.clockFired (R.attemptClock old))
    ]

-- | Reports have already detached their bounded values inside the owner. This
-- function is called only after STM and the outer outstanding-state lock.
emitObservations ::
  Monad m =>
  Tracer m (TraceLeiosFetchClientEvent peer) ->
  Tracer m (TraceLeiosFetchDetailEvent peer) ->
  [Observation peer] ->
  m ()
emitObservations routine detail = mapM_ emit
 where
  emit (Routine (L.AttemptKey peer invocation _) value) = traceWith routine (TraceLeiosFetchClientPolicy peer invocation value)
  emit (Detail (L.AttemptKey peer invocation _) value) = traceWith detail (TraceLeiosFetchDetailEvent peer invocation value)
