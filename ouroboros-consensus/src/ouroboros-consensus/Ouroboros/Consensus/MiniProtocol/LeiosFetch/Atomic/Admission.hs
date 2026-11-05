{-# LANGUAGE NamedFieldPuns #-}

-- | Pure connection-wide admission and lifetime accounting, shared across EBs.
-- This module neither sends messages nor activates a fetching policy. Every
-- successful transition is intended to run in one admission-state STM update.
--
-- Rationale (repository-root paths), with links to the cancellation contract:
--
-- * @docs/fetch-policy/cancellation-contract.md#6-preserve-fifo-response-order@
-- * @docs/fetch-policy/cancellation-contract.md#deduplication-and-durable-acquisition@
-- * @docs/fetch-policy/cancellation-contract.md#7-define-too-old-by-a-bounded-request-window@
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Admission
  ( AdmissionLimits
  , mkAdmissionLimits
  , AdmissionState
  , newAdmissionState
  , AdmissionError (..)
  , Exchange (..)
  , OriginalTerminal (..)
  , HandoffKey (..)
  , Usage (..)
  , admitData
  , admitCancel
  , dispatchNext
  , observeElement
  , finishHead
  , releaseHandoff
  , retireSession
  , admissionUsage
  , liveRequests
  , outstandingExchanges
  , pendingHandoffs
  , nextOrdinal
  , admissionInvariant
  ) where

import Control.Monad (unless, when)
import Data.Foldable (toList)
import Data.List (nub, sort)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq
import Data.Word (Word16, Word64)
import Numeric.Natural (Natural)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Descriptor
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity

data AdmissionLimits = AdmissionLimits
  { limitData :: !Int
  , limitResponse :: !Natural
  , limitHandoff :: !Natural
  , limitEntries :: !Natural
  , limitOrdinal :: !Word64
  }
  deriving (Eq, Show)

-- | The ordinal ceiling is maxBound in production; a smaller ceiling permits
-- exhaustion tests of the identical arithmetic. Promote D before doubling it.
mkAdmissionLimits ::
  Word16 ->
  EncodedBytes ->
  PayloadBytes ->
  EntryCount ->
  Word64 ->
  Either AdmissionError AdmissionLimits
mkAdmissionLimits d (EncodedBytes wire) (PayloadBytes payload) (EntryCount entries) lastId
  | d == 0 || wire == 0 || payload == 0 || entries == 0 = Left InvalidLimits
  | otherwise = Right (AdmissionLimits (fromIntegral d) wire payload entries lastId)

data Exchange peer
  = DataExchange !(RequestKey peer)
  | -- | The cancel's own key followed by its data target.
    CancelExchange !(RequestKey peer) !(RequestKey peer)
  deriving (Eq, Show)

data Queued peer = Queued !(Exchange peer) !Bool deriving (Eq, Show)

data DataRecord peer = DataRecord
  { recordPrepared :: !PreparedRequest
  , recordOriginalDone :: !Bool
  , recordCancel :: !(Maybe (RequestKey peer))
  , recordObserved :: !Natural
  }
  deriving (Eq, Show)

-- | A whole-object handoff, independent of the lifetime of its wire record.
data HandoffKey peer = HandoffKey !(RequestKey peer) !Natural
  deriving (Eq, Ord, Show)

data AdmissionState peer = AdmissionState
  { stateLimits :: !AdmissionLimits
  , stateLifetime :: !(LeiosPeerLifetimeId peer)
  , stateSession :: !FetchSessionToken
  , stateNext :: !Natural
  , stateRecords :: !(Map.Map (RequestKey peer) (DataRecord peer))
  , stateQueue :: !(Seq (Queued peer))
  , statePending :: !(Map.Map (HandoffKey peer) PayloadBytes)
  , stateClosed :: !Bool
  }
  deriving (Eq, Show)

data AdmissionError
  = InvalidLimits
  | SessionClosed
  | StaleSession
  | DataCapacity
  | ExchangeCapacity
  | ResponseCapacity
  | HandoffCapacity
  | EntryCapacity
  | OrdinalExhausted
  | UnknownRequest
  | AlreadyCancelled
  | InvalidCancelTarget
  | DispatchOrder
  | ReplyOrder
  | NotDispatched
  | InvalidElement
  | InvalidTerminal
  | UnknownHandoff
  deriving (Eq, Show)

data OriginalTerminal = NormalTerminal | CancelledTerminal | CancelAcknowledgement
  deriving (Eq, Show)

data Usage = Usage
  { usedData :: !Int
  , usedExchanges :: !Int
  , usedResponseBytes :: !Natural
  , usedHandoffBytes :: !Natural
  , usedHandoffEntries :: !Natural
  }
  deriving (Eq, Show)

newAdmissionState ::
  AdmissionLimits -> LeiosPeerLifetimeId peer -> FetchSessionToken -> AdmissionState peer
newAdmissionState limits lifetime session =
  AdmissionState limits lifetime session 0 Map.empty Seq.empty Map.empty False

nextOrdinal :: AdmissionState peer -> Natural
nextOrdinal = stateNext

liveRequests :: AdmissionState peer -> [RequestKey peer]
liveRequests = Map.keys . stateRecords

outstandingExchanges :: AdmissionState peer -> [Exchange peer]
outstandingExchanges s = [e | Queued e _ <- toList (stateQueue s)]

pendingHandoffs :: AdmissionState peer -> [(HandoffKey peer, PayloadBytes)]
pendingHandoffs = Map.toList . statePending

-- | No data capacity is lent out from the D slots reserved for controls. Each
-- live data record owns at most its original and one cancel exchange.
admitData ::
  Ord peer =>
  PreparedRequest ->
  AdmissionState peer ->
  Either AdmissionError (RequestKey peer, AdmissionState peer)
admitData prepared s = do
  ensureOpen s
  let limits = stateLimits s
      usage = admissionUsage s
      (payload, entries) = elementCharge (NE.toList (preparedElements prepared))
      EncodedBytes wire = preparedResponseBytes prepared
  when (usedData usage >= limitData limits) (Left DataCapacity)
  when (usedExchanges usage >= 2 * limitData limits) (Left ExchangeCapacity)
  when (usedResponseBytes usage + wire > limitResponse limits) (Left ResponseCapacity)
  when (usedHandoffBytes usage + payload > limitHandoff limits) (Left HandoffCapacity)
  when (usedHandoffEntries usage + entries > limitEntries limits) (Left EntryCapacity)
  -- One ordinal for this data exchange and one for its possible cancel, plus
  -- all outstanding cancel promises. The local next counter never wraps.
  unless (ordinalCapacity s >= cancelPromises s + 2) (Left OrdinalExhausted)
  let key = freshKey s
  pure
    ( key
    , s
        { stateNext = stateNext s + 1
        , stateRecords = Map.insert key (DataRecord prepared False Nothing 0) (stateRecords s)
        , stateQueue = stateQueue s |> Queued (DataExchange key) False
        }
    )

admitCancel ::
  Ord peer =>
  RequestKey peer ->
  AdmissionState peer ->
  Either AdmissionError (RequestKey peer, AdmissionState peer)
admitCancel target s = do
  ensureOpen s
  checkSession target s
  record <- maybe (Left UnknownRequest) Right (Map.lookup target (stateRecords s))
  when (recordCancel record /= Nothing) (Left AlreadyCancelled)
  when (recordOriginalDone record) (Left InvalidCancelTarget)
  let RequestKey _ _ (WireOrdinal targetId) = target
      n = stateNext s
  -- Check ordering before subtraction, so an unsigned target cannot underflow.
  unless
    ( fromIntegral targetId < n
        && n - fromIntegral targetId < fromIntegral (2 * limitData (stateLimits s))
    )
    (Left InvalidCancelTarget)
  when (Seq.length (stateQueue s) >= 2 * limitData (stateLimits s)) (Left ExchangeCapacity)
  unless (ordinalCapacity s >= cancelPromises s) (Left OrdinalExhausted)
  let key = freshKey s
  pure
    ( key
    , s
        { stateNext = n + 1
        , stateRecords = Map.insert target (record{recordCancel = Just key}) (stateRecords s)
        , stateQueue = stateQueue s |> Queued (CancelExchange key target) False
        }
    )

-- | Hand the next admitted command to the driver, in order. This denotes the
-- start of send, not its return timestamp: a fast reply may beat send-return.
-- Once admitted, a command cannot be removed even if it is still unsent.
dispatchNext ::
  Eq peer => RequestKey peer -> AdmissionState peer -> Either AdmissionError (AdmissionState peer)
dispatchNext key s = do
  ensureOpen s
  checkSession key s
  let (sent, rest) = Seq.spanl (\(Queued _ dispatched) -> dispatched) (stateQueue s)
  case rest of
    Queued exchange False :<| tailQueue
      | exchangeKey exchange == key ->
          pure s{stateQueue = sent Seq.>< (Queued exchange True Seq.<| tailQueue)}
    _ -> Left DispatchOrder

-- | Record one validated whole object at the FIFO head. The caller verifies its
-- hash/content against the pinned descriptor first. Useful objects produce a
-- charged ticket; redundant objects release their reservation without a ticket.
-- There is no operation accepting a partial body or a fractional transaction.
observeElement ::
  Ord peer =>
  RequestKey peer ->
  PayloadBytes ->
  Bool ->
  AdmissionState peer ->
  Either AdmissionError (Maybe (HandoffKey peer), AdmissionState peer)
observeElement key bytes@(PayloadBytes actual) useful s = do
  exchange <- requireHead key s
  case exchange of
    CancelExchange{} -> Left InvalidElement
    DataExchange{} -> do
      record <- maybe (Left UnknownRequest) Right (Map.lookup key (stateRecords s))
      case drop (fromIntegral (recordObserved record)) (NE.toList (preparedElements (recordPrepared record))) of
        [] -> Left InvalidElement
        PayloadBytes expected : _ -> do
          let validSize = case preparedDescriptor (recordPrepared record) of
                BodyRequest{} -> actual > 0 && actual <= expected
                BatchRequest{} -> actual == expected
          unless validSize (Left InvalidElement)
          let ticket = HandoffKey key (recordObserved record)
          pure
            ( if useful then Just ticket else Nothing
            , s
                { stateRecords = Map.insert key (record{recordObserved = recordObserved record + 1}) (stateRecords s)
                , statePending = if useful then Map.insert ticket bytes (statePending s) else statePending s
                }
            )

-- | FIFO terminals release unreceived reservations exactly once. An original
-- record with an admitted cancel survives until that cancel's own ACK slot.
finishHead ::
  Ord peer =>
  RequestKey peer ->
  OriginalTerminal ->
  AdmissionState peer ->
  Either AdmissionError (AdmissionState peer)
finishHead key terminal s = do
  exchange <- requireHead key s
  let remainingQueue = Seq.drop 1 (stateQueue s)
  case exchange of
    DataExchange{} -> do
      record <- maybe (Left UnknownRequest) Right (Map.lookup key (stateRecords s))
      case terminal of
        CancelAcknowledgement -> Left InvalidTerminal
        NormalTerminal ->
          unless
            (recordObserved record == fromIntegral (NE.length (preparedElements (recordPrepared record))))
            (Left InvalidTerminal)
        CancelledTerminal -> do
          cancelKey <- maybe (Left InvalidTerminal) Right (recordCancel record)
          -- A cancelled reply cannot precede handing its cancel to the driver.
          unless
            (any (\(Queued e sent) -> sent && exchangeKey e == cancelKey) (stateQueue s))
            (Left InvalidTerminal)
          -- Once a normal reply starts it must drain; a cancelled terminal
          -- cannot truncate an already-observed normal payload. Malformed or
          -- disconnected prefixes go through retireSession instead.
          unless (recordObserved record == 0) (Left InvalidTerminal)
      pure
        s
          { stateQueue = remainingQueue
          , stateRecords = case recordCancel record of
              Nothing -> Map.delete key (stateRecords s)
              Just _ -> Map.insert key (record{recordOriginalDone = True}) (stateRecords s)
          }
    CancelExchange _ target -> do
      unless (terminal == CancelAcknowledgement) (Left InvalidTerminal)
      record <- maybe (Left UnknownRequest) Right (Map.lookup target (stateRecords s))
      unless (recordOriginalDone record && recordCancel record == Just key) (Left InvalidTerminal)
      pure s{stateQueue = remainingQueue, stateRecords = Map.delete target (stateRecords s)}

-- | A matching storage receipt can arrive after wire retirement/disconnect.
-- Duplicate and replacement-session receipts cannot refund a different ticket.
releaseHandoff ::
  Ord peer => HandoffKey peer -> AdmissionState peer -> Either AdmissionError (AdmissionState peer)
releaseHandoff ticket@(HandoffKey key _) s = do
  checkSession key s
  unless (Map.member ticket (statePending s)) (Left UnknownHandoff)
  pure s{statePending = Map.delete ticket (statePending s)}

-- | Release wire and unreceived storage capacity; keep accepted handoffs charged.
-- The planned node-owned handoff arena will own these tickets across reconnects.
-- This closed ledger must not be thrown away while its pending receipts remain
-- outstanding.
retireSession :: AdmissionState peer -> AdmissionState peer
retireSession s = s{stateClosed = True, stateRecords = Map.empty, stateQueue = Seq.empty}

admissionUsage :: AdmissionState peer -> Usage
admissionUsage s =
  Usage
    (Map.size records)
    (Seq.length (stateQueue s))
    response
    (reserved + pending)
    (entries + fromIntegral (Map.size (statePending s)))
 where
  records = stateRecords s
  response =
    sum
      [ n
      | r <- Map.elems records
      , not (recordOriginalDone r)
      , let EncodedBytes n = preparedResponseBytes (recordPrepared r)
      ]
  (reserved, entries) =
    elementCharge
      [ b
      | r <- Map.elems records
      , not (recordOriginalDone r)
      , b <- drop (fromIntegral (recordObserved r)) (NE.toList (preparedElements (recordPrepared r)))
      ]
  pending = sum [n | PayloadBytes n <- Map.elems (statePending s)]

ensureOpen :: AdmissionState peer -> Either AdmissionError ()
ensureOpen s = when (stateClosed s) (Left SessionClosed)

checkSession :: Eq peer => RequestKey peer -> AdmissionState peer -> Either AdmissionError ()
checkSession (RequestKey lifetime session _) s =
  unless (lifetime == stateLifetime s && session == stateSession s) (Left StaleSession)

requireHead ::
  Eq peer => RequestKey peer -> AdmissionState peer -> Either AdmissionError (Exchange peer)
requireHead key s = do
  ensureOpen s
  checkSession key s
  case stateQueue s of
    Queued e sent :<| _
      | exchangeKey e == key ->
          if sent then Right e else Left NotDispatched
    _ -> Left ReplyOrder

exchangeKey :: Exchange peer -> RequestKey peer
exchangeKey (DataExchange key) = key
exchangeKey (CancelExchange key _) = key

freshKey :: AdmissionState peer -> RequestKey peer
freshKey s = RequestKey (stateLifetime s) (stateSession s) (WireOrdinal (fromIntegral (stateNext s)))

ordinalCapacity :: AdmissionState peer -> Natural
ordinalCapacity s = fromIntegral (limitOrdinal (stateLimits s)) + 1 - stateNext s

cancelPromises :: AdmissionState peer -> Natural
cancelPromises s = fromIntegral (length [() | r <- Map.elems (stateRecords s), Nothing <- [recordCancel r]])

elementCharge :: [PayloadBytes] -> (Natural, Natural)
elementCharge xs = (sum [n | PayloadBytes n <- xs], fromIntegral (length xs))

-- | An executable invariant for generated transition traces, deliberately
-- independent of cached counters (usage is derived from owned reservations).
admissionInvariant :: Ord peer => AdmissionState peer -> Bool
admissionInvariant s =
  usedData usage <= limitData limits
    && usedExchanges usage <= 2 * limitData limits
    && usedResponseBytes usage <= limitResponse limits
    && usedHandoffBytes usage <= limitHandoff limits
    && usedHandoffEntries usage <= limitEntries limits
    && stateNext s <= fromIntegral (limitOrdinal limits) + 1
    && ordinalCapacity s >= cancelPromises s
    && ids == sort (nub ids)
    && all (< stateNext s) ids
    && and
      [ dispatched || not laterSent
      | (Queued _ dispatched, laterSent) <-
          zip queue (tail (scanr (\(Queued _ sent) acc -> sent || acc) False queue))
      ]
    && all validRecord (Map.toList (stateRecords s))
    && all validExchange queue
    && all (\(HandoffKey key _, _) -> checkSession key s == Right ()) (pendingHandoffs s)
    && (not (stateClosed s) || (null queue && Map.null (stateRecords s)))
 where
  limits = stateLimits s
  usage = admissionUsage s
  queue = toList (stateQueue s)
  ids = [fromIntegral n | Queued e _ <- queue, let RequestKey _ _ (WireOrdinal n) = exchangeKey e]
  validRecord (key, record) =
    checkSession key s == Right ()
      && recordObserved record <= fromIntegral (NE.length (preparedElements (recordPrepared record)))
      && length [() | Queued (DataExchange k) _ <- queue, k == key]
        == (if recordOriginalDone record then 0 else 1)
      && case recordCancel record of
        Nothing -> not (recordOriginalDone record)
        Just cancel -> length [() | Queued (CancelExchange c target) _ <- queue, c == cancel, target == key] == 1
  validExchange (Queued e _) = case e of
    DataExchange key -> maybe False (not . recordOriginalDone) (Map.lookup key (stateRecords s))
    CancelExchange key target -> maybe False ((== Just key) . recordCancel) (Map.lookup target (stateRecords s))
