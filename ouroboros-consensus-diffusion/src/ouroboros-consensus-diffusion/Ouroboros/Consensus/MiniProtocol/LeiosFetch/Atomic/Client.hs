{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Explicitly selected cancellation-capable client. Admission, timing and
-- immutable receive scopes are published before sending can permit a reply.
-- Accepted storage remains node-owned when the client stops or disconnects.
--
-- Rationale and code-to-contract map (repository-root path):
-- @docs/fetch-policy/cancellation-contract.md#6-preserve-fifo-response-order@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Client
  ( AtomicClient
  , ClientHooks (..)
  , nullClientHooks
  , ClientError (..)
  , ClientSnapshot (..)
  , withAtomicClient
  , admitClientRequest
  , publishClientRequest
  , cancelClientRequest
  , withdrawClientRequest
  , stopAtomicClient
  , closeAtomicClient
  , pollAtomicClient
  , readClientSnapshot
  , readClientUsage
  , clientStorageSession
  , atomicClientPeer
  , runAtomicClient
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
  ( MonadSTM (..)
  , STM
  , StrictTVar
  , newTVarIO
  , readTVar
  , retry
  , throwSTM
  , writeTVar
  )
import Control.Monad (unless, when)
import Control.Monad.Class.MonadThrow (Exception, SomeException, bracket, catch, throwIO)
import Control.Monad.Class.MonadTime.SI (Time, getMonotonicTime)
import Control.Monad.Class.MonadTimer.SI (MonadTimer)
import Control.Tracer (Tracer)
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq
import Data.Word (Word16, Word32)
import Network.Mux.Timeout (withTimeoutSerial)
import Network.TypedProtocol.Codec (AnyMessage (..))
import Network.TypedProtocol.Core (StateTokenI)
import Network.TypedProtocol.Driver (runPipelinedPeerWithDriver)
import Network.TypedProtocol.Peer.Client
import Ouroboros.Consensus.Leios.Types
  ( EndorserBlock
  , LeiosPoint
  , LeiosTx
  , decodeEndorserBlock
  , decodeLeiosPoint
  , decodeLeiosTx
  , encodeEndorserBlock
  , encodeLeiosPoint
  , encodeLeiosTx
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Acquisition
  ( OrdinaryAssignments
  , releaseCheckpoint
  , releaseSessionAssignments
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Admission as A
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Codec (codecLeiosFetchAtomicProgress)
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Deadline as D
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Descriptor
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Handoff as H
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Receive
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Timing as T
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Type
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision (offsetsToBitmap)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity
import Ouroboros.Consensus.Util.IOLike (IOLike)
import Ouroboros.Network.BlockFetch.ClientRegistry
  ( KeepAliveRtt (..)
  , KeepAliveRttReader
  , readKeepAliveRtt
  )
import Ouroboros.Network.Channel (Channel)
import Ouroboros.Network.Driver.Limits (TraceSendRecv)
import Ouroboros.Network.Driver.Progress
import Ouroboros.Network.Protocol.Limits (longWait)

type Atomic = LeiosFetchAtomic LeiosPoint EndorserBlock LeiosTx

-- | Bounded synchronous coordinator notifications, in the same STM transaction
-- as the corresponding client transition. They must not wait for storage,
-- retain an unbounded event history or re-enter client mutation functions.
data ClientHooks m peer = ClientHooks
  { clientWakeup :: STM m ()
  -- ^ Coalesced readiness after publishing changed state.
  , clientTransaction :: RequestKey peer -> ProgressEvent LeiosTx -> H.Acceptance -> STM m ()
  -- ^ Authenticated whole TX under its original descriptor; only Useful earns credit.
  , clientReply :: RequestKey peer -> ReplyCompletion -> STM m ()
  -- ^ Original typed terminal and input receipt, independent of storage durability.
  , clientExpired :: T.Expiry peer -> STM m ()
  -- ^ One reconciled soft expiry. The coordinator chooses persistence/rescue.
  , clientAborted :: SomeException -> STM m ()
  -- ^ At most once on interpreter/receive failure, after exact-session cleanup.
  }

nullClientHooks :: MonadSTM m => ClientHooks m peer
nullClientHooks = ClientHooks (pure ()) (\_ _ _ -> pure ()) (\_ _ -> pure ()) (const (pure ())) (const (pure ()))

-- | Local integration/configuration errors; protocol/codec/storage exceptions
-- retain their existing types when raised by the receive or driver layers.
data ClientError
  = ClientStorageError !H.StorageError
  | ClientTimingError !D.TimingError
  | ClientTimingEventError !T.TimingEventError
  | ClientClosed
  | ClientStopping
  | ClientUpstreamBlocked
  | ClientAlreadyStarted
  | ClientWrongSession
  | ClientNoReceiver
  | ClientQueueInvariant
  | ClientInvalidLimits
  | ClientBodySizeMisstated
  deriving (Eq, Show)

instance Exception ClientError

-- | An already admitted original or cancel command. Data descriptors and
-- control identities cannot be removed or rewritten after admission.
data Command peer
  = DataCommand !(H.StorageRequest peer)
  | CancelCommand !(RequestKey peer) !(RequestKey peer)

-- | Protocol execution state for one immutable fetch-session identity. The
-- queue holds admitted commands; finished receiver results are counted until
-- collected, so an STM wait can wake for either replies or new commands.
data ClientState m peer = ClientState
  { queuedCommands :: !(Seq (Command peer))
  -- ^ Dispatch order, already covered by the handoff admission ledger.
  , timingState :: !(T.TimingState peer)
  -- ^ FIFO head clocks, validated terminal observations and unresolved send returns.
  , uncollectedReplies :: !Word32
  -- ^ Finished receivers not yet collected by the typed sender; bounded by 2D.
  , currentReceiver :: !(Maybe (ReplyReceiver m peer))
  -- ^ One installed immutable scope, acquired by the progress driver once.
  , latestClock :: !Time
  -- ^ Last reconciled/observed time; concurrent publication can overtake a caller's clock read.
  , stopping :: !Bool
  -- ^ Reject new admission while draining already admitted commands and replies.
  , started :: !Bool
  -- ^ Sticky single-use interpreter guard, including after graceful completion.
  , closed :: !Bool
  -- ^ Permanent exact-session retirement; late callbacks cannot revive it.
  , firstFailure :: !(Maybe SomeException)
  -- ^ First receive/interpreter failure, so either direction reports the same
  -- cause when retirement wakes the sibling. At most one exception is retained.
  }

-- | Bracket-owned client session over the node-owned handoff/assignment tables.
-- Its owner must join any interpreter before leaving 'withAtomicClient'.
data AtomicClient m owner peer = AtomicClient
  { clientHandoff :: !(H.StorageHandoff m owner peer)
  -- ^ Shared node-owned acquisition and accepted storage.
  , clientSession :: !(H.StorageSession peer)
  -- ^ This invocation's admission/cleanup identity, never a peer-address lookup.
  , clientAssignments :: !(OrdinaryAssignments m peer)
  -- ^ Node-owned ordinary scheduling claims, released by exact request/session.
  , clientDataWindow :: !Word16
  -- ^ Frozen positive data-record ceiling; widen before deriving 2D bounds.
  , clientConfig :: !D.TimingConfig
  -- ^ Validated policy configuration, rechecked against the actual fatal guard.
  , clientRtt :: !(Maybe (KeepAliveRttReader m))
  -- ^ One captured connection-scoped reader; retirement yields fallback, never a replacement sample.
  , clientHooks :: !(ClientHooks m peer)
  -- ^ Bounded synchronous coordinator integration.
  , clientState :: !(StrictTVar m (ClientState m peer))
  -- ^ Commands, timing, receive binding and interpreter lifecycle.
  }

-- | Read-only execution/timing state. Wire/storage usage has its separate exact
-- session accessor; logical withdrawal never implies those reservations retired.
data ClientSnapshot peer = ClientSnapshot
  { snapshotClientTiming :: !(T.TimingState peer)
  -- ^ Current immutable observations and FIFO deadlines.
  , snapshotClientWindow :: !Word16
  -- ^ Frozen data-record ceiling used by this client session.
  , snapshotClientQueued :: !Word32
  -- ^ Admitted commands awaiting dispatch.
  , snapshotClientUncollected :: !Word32
  -- ^ Finished receiver results awaiting typed collection.
  , snapshotClientReceiver :: !(Maybe (RequestKey peer))
  -- ^ Installed original receive identity, if any.
  , snapshotClientStarted :: !Bool
  -- ^ Whether the single allowed interpreter has started.
  , snapshotClientStopping :: !Bool
  -- ^ Whether admission is closed for graceful drain.
  , snapshotClientClosed :: !Bool
  -- ^ Whether the session has been permanently retired.
  }
  deriving (Eq, Show)

-- | Allocate one client and retire its exact session on every exit. Pending
-- storage survives with its original charges. The scoped RTT reader is supplied
-- by connection assembly; this function never looks up a replacement peer key.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#10-admission-shutdown-and-compatibility@.
withAtomicClient ::
  (IOLike m, Ord peer) =>
  H.StorageHandoff m owner peer ->
  OrdinaryAssignments m peer ->
  owner ->
  LeiosPeerLifetimeId peer ->
  Word16 ->
  Word32 ->
  D.TimingConfig ->
  Maybe (KeepAliveRttReader m) ->
  ClientHooks m peer ->
  (AtomicClient m owner peer -> m a) ->
  m a
withAtomicClient handoff assignments owner lifetime count bytes config rtt hooks = bracket create (atomically . closeAtomicClient)
 where
  create = do
    either
      (throwIO . ClientTimingError)
      pure
      (maybe (Right ()) (`D.validateTimingConfig` config) longWait)
    session <-
      atomically (H.openStorageSession handoff owner lifetime count bytes)
        >>= either (throwIO . ClientStorageError) pure
    let (_, token) = H.storageSessionIdentity session
    now <- getMonotonicTime
    state <-
      newTVarIO
        (ClientState Seq.empty (T.newTimingState lifetime token) 0 Nothing now False False False Nothing)
    pure (AtomicClient handoff session assignments count config rtt hooks state)

-- | Admit prepared work and publish timing/dispatch context in one STM update.
-- Dropping a prepared descriptor before this call consumes no ordinal. Once
-- admitted, even an unsent command owes its original exchange and any cancel.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#receiver-observable-deadline-activation@.
admitClientRequest ::
  (IOLike m, Ord owner, Ord peer) =>
  AtomicClient m owner peer ->
  Time ->
  Rational ->
  PreparedRequest ->
  STM m (Either ClientError (H.StorageRequest peer))
admitClientRequest client now factor prepared = do
  ready <- admissionReady client now
  case ready of
    Left err -> pure (Left err)
    Right () -> do
      sample <- readRtt client
      case D.snapshotTiming (clientConfig client) now sample factor prepared of
        Left err -> pure (Left (ClientTimingError err))
        Right snapshot -> do
          admitted <- H.admitStorageRequest (clientHandoff client) (clientSession client) prepared
          case admitted of
            Left err -> pure (Left (ClientStorageError err))
            Right request -> publishData client request snapshot >> pure (Right request)

-- | Compose with checkpoint assignment/admission in the SAME STM transaction.
-- Failure throws in STM so the caller's preceding admission/ownership update is
-- rolled back too. Never retrofit already dispatched legacy requests here.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#receiver-observable-deadline-activation@.
publishClientRequest ::
  (IOLike m, Ord peer) =>
  AtomicClient m owner peer -> Time -> Rational -> H.StorageRequest peer -> STM m ()
publishClientRequest client now factor request = do
  admissionReady client now >>= either throwSTM pure
  let (lifetime, token) = H.storageSessionIdentity (clientSession client)
      RequestKey original originalToken _ = H.storageRequestKey request
  unless (original == lifetime && originalToken == token) (throwSTM ClientWrongSession)
  sample <- readRtt client
  snapshot <-
    either (throwSTM . ClientTimingError) pure $
      D.snapshotTiming (clientConfig client) now sample factor (H.storageRequestPrepared request)
  publishData client request snapshot

publishData ::
  (IOLike m, Ord peer) =>
  AtomicClient m owner peer -> H.StorageRequest peer -> D.TimingSnapshot -> STM m ()
publishData client request snapshot = do
  state <- readTVar (clientState client)
  timing <-
    timingResult
      (T.registerTiming (H.storageRequestKey request) (T.TrackData snapshot) (timingState state))
  writeTVar
    (clientState client)
    state{queuedCommands = queuedCommands state |> DataCommand request, timingState = timing}
  clientWakeup (clientHooks client)

-- | Admit an independent cancel exchange without freeing the original's wire
-- reservations. Logical ownership is released immediately; FIFO intake/output
-- and the original/ACK pair still govern its eventual settlement.
cancelClientRequest ::
  (IOLike m, Ord peer) =>
  AtomicClient m owner peer ->
  Time ->
  H.StorageRequest peer ->
  STM m (Either ClientError (RequestKey peer))
cancelClientRequest client now request = do
  pollAtomicClient client now
  state <- readTVar (clientState client)
  if closed state
    then pure (Left ClientClosed)
    else
      if stopping state
        then pure (Left ClientStopping)
        else
          if not (sameSession client (H.storageRequestKey request))
            then pure (Left ClientWrongSession)
            else do
              sample <- readRtt client
              canceled <- H.cancelStorageRequest (clientHandoff client) request
              case canceled of
                Left err -> pure (Left (ClientStorageError err))
                Right key -> do
                  state' <- readTVar (clientState client)
                  withdrawn <- timingResult (T.withdrawTiming (H.storageRequestKey request) (timingState state'))
                  timing <- timingResult (T.registerTiming key (T.TrackAck (D.conservativeRtt now sample)) withdrawn)
                  writeTVar
                    (clientState client)
                    state'
                      { queuedCommands = queuedCommands state' |> CancelCommand key (H.storageRequestKey request)
                      , timingState = timing
                      }
                  releaseCheckpoint (clientAssignments client) (H.storageRequestKey request)
                  pollAtomicClient client now
                  clientWakeup (clientHooks client)
                  pure (Right key)

-- | Clear logical interest without retiring the original wire head/deadline.
-- A coordinator abandoning unresolved admitted work also queues its cancel.
withdrawClientRequest ::
  (IOLike m, Ord peer) => AtomicClient m owner peer -> Time -> RequestKey peer -> STM m ()
withdrawClientRequest client now key = do
  pollAtomicClient client now
  state <- readTVar (clientState client)
  unless (closed state) $ do
    timing <- timingResult (T.withdrawTiming key (timingState state))
    writeTVar (clientState client) state{timingState = timing}
    releaseCheckpoint (clientAssignments client) key
    clientWakeup (clientHooks client)

-- | Close admission, send all admitted commands, drain every original/ACK
-- response, then send Done. This does not interrupt a response or storage work.
stopAtomicClient :: MonadSTM m => AtomicClient m owner peer -> STM m ()
stopAtomicClient client = do
  state <- readTVar (clientState client)
  unless (closed state) $ do
    writeTVar (clientState client) state{stopping = True}
    clientWakeup (clientHooks client)

-- | Exact-session cleanup, idempotent across bracket/abort/removal races.
-- A replacement mini-protocol in the same registration is a different session.
closeAtomicClient :: (IOLike m, Ord peer) => AtomicClient m owner peer -> STM m ()
closeAtomicClient client = do
  state <- readTVar (clientState client)
  unless (closed state) $ do
    let (lifetime, token) = H.storageSessionIdentity (clientSession client)
    H.closeStorageSession (clientHandoff client) (clientSession client)
    releaseSessionAssignments (clientAssignments client) lifetime token
    writeTVar
      (clientState client)
      state
        { queuedCommands = Seq.empty
        , timingState = T.retireTimingSession (timingState state)
        , currentReceiver = Nothing
        , closed = True
        }
    clientWakeup (clientHooks client)

-- | Reconcile published observations before all due timers. The effective local
-- clock is monotone even if another thread publishes after the caller read time.
-- Receive/send timestamps themselves are never changed. No event history grows
-- here: the coordinator consumes bounded synchronous expiry notifications.
pollAtomicClient :: IOLike m => AtomicClient m owner peer -> Time -> STM m ()
pollAtomicClient client now = do
  state <- readTVar (clientState client)
  unless (closed state) $ do
    let clock = max now (latestClock state)
    (expired, timing) <- timingResult (T.reconcileTiming clock (timingState state))
    writeTVar (clientState client) state{timingState = timing, latestClock = clock}
    mapM_ (clientExpired (clientHooks client)) expired
    unless (null expired) (clientWakeup (clientHooks client))

readClientSnapshot :: MonadSTM m => AtomicClient m owner peer -> STM m (ClientSnapshot peer)
readClientSnapshot client = do
  state <- readTVar (clientState client)
  pure $
    ClientSnapshot
      (timingState state)
      (clientDataWindow client)
      (fromIntegral (Seq.length (queuedCommands state)))
      (uncollectedReplies state)
      (replyRequestKey <$> currentReceiver state)
      (started state)
      (stopping state)
      (closed state)

-- | Exact admission handle for composing checkpoint assignment with
-- 'publishClientRequest' in the same STM transaction. It is not a new session.
clientStorageSession :: AtomicClient m owner peer -> H.StorageSession peer
clientStorageSession = clientSession

readClientUsage ::
  (MonadSTM m, Ord peer) => AtomicClient m owner peer -> STM m (Either H.StorageError A.Usage)
readClientUsage client = H.readStorageSessionUsage (clientHandoff client) (clientSession client)

admissionReady ::
  IOLike m => AtomicClient m owner peer -> Time -> STM m (Either ClientError ())
admissionReady client now = do
  pollAtomicClient client now
  state <- readTVar (clientState client)
  pure $
    if closed state
      then Left ClientClosed
      else
        if stopping state
          then Left ClientStopping
          else if T.upstreamBlocked (timingState state) then Left ClientUpstreamBlocked else Right ()

readRtt :: MonadSTM m => AtomicClient m owner peer -> STM m (Maybe D.RttObservation)
readRtt client = case clientRtt client of
  Nothing -> pure Nothing
  Just reader ->
    fmap (\sample -> D.RttObservation (rttDuration sample) (rttCompletedAt sample))
      <$> readKeepAliveRtt reader

commandKey :: Command peer -> RequestKey peer
commandKey (DataCommand req) = H.storageRequestKey req
commandKey (CancelCommand key _) = key

-- | Single-use pipelined peer. Its STM wait remains responsive to outbound
-- cancellation while the independent receiver is waiting for remote data.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#agreed-single-message-receive-sequence@.
atomicClientPeer ::
  forall m owner peer.
  (IOLike m, Ord peer) =>
  AtomicClient m owner peer -> ClientPipelined Atomic StIdle m ()
atomicClientPeer client = ClientPipelined $ Effect $ do
  atomically $ do
    state <- readTVar (clientState client)
    when (closed state) (throwClosed state)
    when (started state) (throwSTM ClientAlreadyStarted)
    writeTVar (clientState client) state{started = True}
  pure (go Zero)
 where
  go :: forall n. Nat n -> Client Atomic (Pipelined n ()) StIdle m ()
  go count = Effect $ atomically $ do
    state <- readTVar (clientState client)
    when (closed state) (throwClosed state)
    case count of
      Succ previous | uncollectedReplies state > 0 ->
        pure $ Collect Nothing $ \() -> Effect $ do
          atomically $ do
            current <- readTVar (clientState client)
            when (uncollectedReplies current == 0) (throwSTM ClientQueueInvariant)
            writeTVar (clientState client) current{uncollectedReplies = uncollectedReplies current - 1}
          pure (go previous)
      _ -> case queuedCommands state of
        command :<| rest -> do
          H.dispatchStorageRequest (clientHandoff client) (commandKey command)
            >>= either (throwSTM . ClientStorageError) pure
          writeTVar (clientState client) state{queuedCommands = rest}
          pure (sendCommand count command)
        Empty -> case count of
          Zero | stopping state -> pure $ Yield MsgDone (Done ())
          _ -> retry

  sendCommand :: forall n. Nat n -> Command peer -> Client Atomic (Pipelined n ()) StIdle m ()
  sendCommand count command = case command of
    DataCommand req ->
      let WireOrdinal ordinal = requestOrdinal (H.storageRequestKey req)
       in case preparedDescriptor (H.storageRequestPrepared req) of
            BodyRequest desc ->
              YieldPipelined
                (MsgLeiosBlockRequest (WireOrdinal ordinal) (bodyPoint desc))
                (receiver (DataReply req))
                (go (Succ count))
            BatchRequest desc ->
              case NE.nonEmpty
                (offsetsToBitmap (IntSet.fromList (map expectedOffset (NE.toList (batchExpected desc))))) of
                Nothing -> Effect (throwIO ClientQueueInvariant)
                Just bitmaps ->
                  YieldPipelined
                    (MsgLeiosBlockTxsRequest (WireOrdinal ordinal) (batchPoint desc) bitmaps)
                    (receiver (DataReply req))
                    (go (Succ count))
    CancelCommand key target ->
      YieldPipelined
        (MsgTryCancel (requestOrdinal key) (requestOrdinal target))
        (receiver (CancelReply key))
        (go (Succ count))

  receiver ::
    forall busy.
    StateTokenI (StBusy busy :: Atomic) =>
    ReplyBinding peer -> Receiver Atomic (StBusy busy) StIdle m ()
  receiver binding = ReceiverEffect $ do
    (pinned, validation) <- newPinnedReceiver client binding
    atomically $ do
      state <- readTVar (clientState client)
      when (closed state) (throwClosed state)
      when (maybe False (const True) (currentReceiver state)) (throwSTM ClientQueueInvariant)
      writeTVar (clientState client) state{currentReceiver = Just pinned}
    pure $ ReceiverAwait $ \message -> case message of
      MsgLeiosBlock{} -> finishReceiver binding pinned validation message
      MsgLeiosBlockTxs{} -> finishReceiver binding pinned validation message
      MsgBlockCancelled -> finishReceiver binding pinned validation message
      MsgBlockTxsCancelled -> finishReceiver binding pinned validation message
      MsgAckTryCancel -> finishReceiver binding pinned validation message

  finishReceiver ::
    forall busy.
    ReplyBinding peer ->
    ReplyReceiver m peer ->
    StrictTVar m (Maybe (T.ValidationToken peer)) ->
    Message Atomic (StBusy busy) StIdle ->
    Receiver Atomic StIdle StIdle m ()
  finishReceiver binding pinned validation message = ReceiverEffect $ do
    completion <- completeReply pinned message
    now <- getMonotonicTime
    atomically $ do
      state <- readTVar (clientState client)
      when (closed state) (throwClosed state)
      when
        (uncollectedReplies state >= 2 * fromIntegral (clientDataWindow client))
        (throwSTM ClientQueueInvariant)
      token <- readTVar validation
      let key = replyRequestKey pinned
          at = receiptObservedAt (completedReceipt completion)
      timing <- timingResult $ case token of
        Nothing -> T.recordTerminal key at (timingState state)
        Just pending -> T.finishValidation pending (T.ValidTerminal at) (timingState state)
      writeTVar validation Nothing
      writeTVar
        (clientState client)
        state
          { timingState = timing
          , latestClock = max at (latestClock state)
          , currentReceiver = Nothing
          , uncollectedReplies = uncollectedReplies state + 1
          }
      case binding of
        DataReply{} -> releaseCheckpoint (clientAssignments client) key
        CancelReply{} -> pure ()
      pollAtomicClient client now
      clientReply (clientHooks client) key completion
      clientWakeup (clientHooks client)
    case completedOutcome completion of
      BodyReceived accepted | H.bodySizeMisstated accepted -> throwIO ClientBodySizeMisstated
      _ -> pure ()
    pure (ReceiverDone ())

newPinnedReceiver ::
  (IOLike m, Ord peer) =>
  AtomicClient m owner peer ->
  ReplyBinding peer ->
  m (ReplyReceiver m peer, StrictTVar m (Maybe (T.ValidationToken peer)))
newPinnedReceiver client binding = do
  validation <- newTVarIO Nothing
  let key = case binding of DataReply req -> H.storageRequestKey req; CancelReply original -> original
      hooks =
        ReplyHooks
          { replyStarted = atomically (clientWakeup (clientHooks client))
          , replyInput = \observation -> do
              now <- getMonotonicTime
              atomically $ do
                state <- readTVar (clientState client)
                unless (closed state) $ do
                  pending <- readTVar validation
                  timing <- case pending of
                    Just _ -> pure (timingState state)
                    Nothing -> do
                      (token, next) <-
                        timingResult (T.beginValidation key (inputObservedAt observation) (timingState state))
                      writeTVar validation (Just token)
                      pure next
                  writeTVar
                    (clientState client)
                    state{timingState = timing, latestClock = max (inputObservedAt observation) (latestClock state)}
                  pollAtomicClient client now
                  clientWakeup (clientHooks client)
          , replyAwaitInput = do
              now <- getMonotonicTime
              atomically $ do
                state <- readTVar (clientState client)
                unless (closed state) $ do
                  pending <- readTVar validation
                  case pending of
                    Nothing -> pure ()
                    Just token -> do
                      timing <- timingResult (T.finishValidation token T.NeedMoreInput (timingState state))
                      writeTVar validation Nothing
                      writeTVar (clientState client) state{timingState = timing}
                      pollAtomicClient client now
                      clientWakeup (clientHooks client)
          , replyTransaction = \event accepted -> atomically $ do
              state <- readTVar (clientState client)
              unless (closed state) (clientTransaction (clientHooks client) key event accepted)
          , replyAborted = abortClient client
          }
  pinned <- newReplyReceiver (clientHandoff client) binding hooks
  pure (pinned, validation)

sameSession :: Eq peer => AtomicClient m owner peer -> RequestKey peer -> Bool
sameSession client (RequestKey lifetime token _) = (lifetime, token) == H.storageSessionIdentity (clientSession client)

requestOrdinal :: RequestKey peer -> WireOrdinal
requestOrdinal (RequestKey _ _ ordinal) = ordinal

observeSend ::
  (IOLike m, Ord peer) => AtomicClient m owner peer -> AnyMessage Atomic -> Time -> m ()
observeSend client (AnyMessage message) at = case message of
  MsgDone -> pure ()
  MsgLeiosBlockRequest ordinal _ -> record ordinal
  MsgLeiosBlockTxsRequest ordinal _ _ -> record ordinal
  MsgTryCancel ordinal _ -> record ordinal
  _ -> throwIO ClientQueueInvariant
 where
  record ordinal = atomically $ do
    state <- readTVar (clientState client)
    unless (closed state) $ do
      let (lifetime, token) = H.storageSessionIdentity (clientSession client)
      timing <- timingResult (T.recordSend (RequestKey lifetime token ordinal) at (timingState state))
      writeTVar (clientState client) state{timingState = timing, latestClock = max at (latestClock state)}
      pollAtomicClient client at
      clientWakeup (clientHooks client)

abortClient :: (IOLike m, Ord peer) => AtomicClient m owner peer -> SomeException -> m ()
abortClient client err = atomically $ do
  state <- readTVar (clientState client)
  unless (closed state) $ do
    writeTVar (clientState client) state{firstFailure = Just err}
    closeAtomicClient client
    clientAborted (clientHooks client) err

throwClosed :: IOLike m => ClientState m peer -> STM m a
throwClosed state = maybe (throwSTM ClientClosed) throwSTM (firstFailure state)

timingResult :: IOLike m => Either T.TimingEventError a -> STM m a
timingResult = either (throwSTM . ClientTimingEventError) pure

-- | Real staged codec and progress driver, using the normal protocol guards.
-- Input chunk capacity is supplied from the negotiated connection/mux assembly.
-- This runner does not advertise or select the atomic capability by itself.
runAtomicClient ::
  forall m owner peer.
  (IOLike m, MonadTimer m, Ord peer) =>
  Tracer m (TraceSendRecv Atomic) ->
  Int ->
  Word32 ->
  Channel m BL.ByteString ->
  AtomicClient m owner peer ->
  m (Maybe BL.ByteString)
runAtomicClient tracer maxTxs chunkBytes channel client =
  ( do
      when (maxTxs <= 0 || chunkBytes == 0) (throwIO ClientInvalidLimits)
      withTimeoutSerial $ \timeoutFn -> do
        let scope :: forall (st :: Atomic). SingLeiosFetchAtomic st -> m (ReceiveScope m LeiosTx)
            scope tok = do
              pinned <- atomically $ do
                state <- readTVar (clientState client)
                when (closed state) (throwClosed state)
                maybe (throwSTM ClientNoReceiver) pure (currentReceiver state)
              receiveReplyScope pinned tok
            codec =
              codecLeiosFetchAtomicProgress
                maxTxs
                encodeLeiosPoint
                decodeLeiosPoint
                encodeEndorserBlock
                decodeEndorserBlock
                encodeLeiosTx
                decodeLeiosTx
            driver =
              progressDriverWithLimits
                tracer
                timeoutFn
                codec
                (byteLimitsLeiosFetchAtomic (fromIntegral . BL.length))
                timeLimitsLeiosFetchAtomic
                chunkBytes
                scope
                (observeSend client)
                channel
        ((), trailing) <- runPipelinedPeerWithDriver driver (atomicClientPeer client)
        pure (progressTrailingBytes trailing)
  )
    `catch` \(err :: SomeException) -> abortClient client err >> throwIO err
