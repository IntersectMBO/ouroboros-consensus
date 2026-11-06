{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Node-scoped atomic acquisition. Connections share durable possession and budgets;
-- their wire identities and immutable reply contexts remain independent.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#10-admission-shutdown-and-compatibility@ (node assembly) and
-- @docs/fetch-policy/cancellation-contract.md#2-separate-policy-obligations-from-wire-attempts@ (body policy) and
-- @docs/fetch-policy/cancellation-contract.md#8-checkpoint-deadlines-and-policy-reactions@ (closure rescue and shared loop),
-- @docs/fetch-policy/cancellation-contract.md#8-checkpoint-deadlines-and-policy-reactions@ (backoff, recovery and cooldown),
-- @docs/fetch-policy/cancellation-contract.md#agreed-coordinator-wakeups@ (unified planning and cleanup).
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Node
  ( AtomicFetchNode
  , AtomicNodeConfig (..)
  , defaultAtomicNodeConfig
  , atomicResponseBudget
  , NodeObservation (..)
  , NodeIntegrationError (..)
  , withAtomicFetchNode
  , withNodeAtomicClient
  , withNodeLegacyClient
  , readNodeObservation
  , readNodeClosureSnapshots
  , readNodeBodySnapshots
  , readNodeStorageSnapshot
  , readNodeClientUsage
  ) where

import qualified Control.Concurrent.Class.MonadSTM.Strict as Strict
import Control.Monad (foldM, forM_, unless, void, when)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Set as Set
import Data.Word (Word16, Word32, Word64)
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Block (SlotNo)
import Ouroboros.Consensus.Leios.Acquisitions (LeiosEbNotification (..))
import Ouroboros.Consensus.Leios.Types
  ( LeiosPoint (..)
  , encodeEndorserBlockSize
  , maxEndorserBlockBytesSize
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Acquisition
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Admission as A
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Body as B
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Client as C
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Closure as L
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Deadline as D
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Descriptor
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Handoff as H
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Receive as R
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Retry as Retry
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Timing as T
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Wait as W
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies (LeiosFetchClientContext (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Kernel
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbWriter (..)
  , Promise (..)
  , TxWriteReceipt (..)
  )
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.BlockFetch.ClientRegistry (KeepAliveRttReader)
import Ouroboros.Network.Driver.Progress (ProgressEvent (..))

-- | Local atomic policy and resource settings. Constructing this value does
-- not advertise a capability; normal assembly owns negotiation and lifetime.
data AtomicNodeConfig = AtomicNodeConfig
  { nodeStorageLimits :: !H.StorageLimits
  -- ^ Node-owned storage/session/acquisition bounds, including draining peers.
  , nodeResponseBytes :: !Word32
  -- ^ Per-connection shared response budget, across every EB and request kind.
  , nodeCheckpointLimits :: !CheckpointLimits
  -- ^ Whole-transaction checkpoint target and independent protocol hard caps.
  , nodeClosureRetryConfig :: !Retry.RetryConfig
  -- ^ Local closure backoff/recovery and grouped cooldown; bodies are independent.
  , nodeTimingConfig :: !D.TimingConfig
  -- ^ Receiver policy, checked against the actual protocol guard by each client.
  }

-- | Initial source-tree defaults for the roughly 260 kB MVP profile. These
-- are local resource/policy settings, not publication sizes or wire constants.
-- Standard node assembly permits overriding this configuration at its low-level
-- entry point. The 0.3 Mb/s benchmark remains provisional.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#agreed-initial-sizing-and-validation@.
defaultAtomicNodeConfig :: AtomicNodeConfig
defaultAtomicNodeConfig =
  AtomicNodeConfig
    { nodeStorageLimits = H.StorageLimits 1048576 4096 128 64 20000
    , nodeResponseBytes = 1048576
    , nodeCheckpointLimits = defaultCheckpointLimits
    , nodeClosureRetryConfig = Retry.defaultRetryConfig
    , nodeTimingConfig =
        must
          (D.mkTimingConfig 300000 14600 0.5 2 60 (PayloadBytes (fromIntegral maxEndorserBlockBytesSize)))
    }
 where
  must :: Show e => Either e a -> a
  must = either (error . show) id

-- | Frozen response budget of the owning runtime. Transport assembly reads
-- this value, rather than accepting a second potentially inconsistent budget.
atomicResponseBudget :: AtomicFetchNode m owner peer blk -> Word32
atomicResponseBudget = nodeResponseBytes . fetchConfig

-- | Fixed-size functional counters, retained without an attempt/event history.
-- Saturation makes a long-lived node safe from counter wraparound.
data NodeObservation = NodeObservation
  { observedAtomicConnections :: !Word64
  -- ^ Connections that actually opened the new client.
  , observedLegacyConnections :: !Word64
  -- ^ Connections deliberately using the version-16 bridge.
  , observedScopedRttReaders :: !Word64
  -- ^ Captured KeepAlive registrations; a reader can still yield no raw sample.
  , observedRttFallbacks :: !Word64
  -- ^ Connections with no matching registration at capture time.
  , observedBodyRequests :: !Word64
  -- ^ Admitted new-version body requests.
  , observedBodyHedges :: !Word64
  -- ^ Successfully admitted logical body hedges.
  , observedBodyTriggers :: !Word64
  -- ^ Primary trigger expiries; each retained primary is charged once.
  , observedBodyFailures :: !Word64
  -- ^ Body hedge expiries, without primary/closure/cancellation penalties.
  , observedBodyCancels :: !Word64
  -- ^ Separately admitted body cancellation exchanges.
  , observedPeakWantedBodies :: !Int
  -- ^ Largest wanted-copy count for any one EB; never exceeds two.
  , observedCheckpoints :: !Word64
  -- ^ Admitted new-version transaction batches.
  , observedClosureStalls :: !Word64
  -- ^ Once-only ordinary closure service expiries; retained originals stay live.
  , observedClosureRescues :: !Word64
  -- ^ Admitted extra rescue batches, independently of ordinary checkpoints.
  , observedClosureFailures :: !Word64
  -- ^ Rescue service expiries; FIFO blockage adds no failure.
  , observedClosureCancels :: !Word64
  -- ^ Separately admitted cancellation exchanges for closure batches.
  , observedClosureHandoffs :: !Word64
  -- ^ Successful whole-batch transfers to productive rescue suppliers.
  , observedPeakClosureCopies :: !Int
  -- ^ Maximum wanted copies of any one TX, never more than two.
  , observedPeakData :: !Int
  -- ^ Largest one-session live data-record count across concurrent EBs.
  }
  deriving (Eq, Show)

-- | Local integration violations, distinct from malformed peer replies.
data NodeIntegrationError
  = ExtensionAlreadyInstalled
  | PeersAlreadyRunning
  | InvalidAcquisition !AcquisitionError
  | InvalidBodyPolicy !B.BodyError
  | InvalidClosurePolicy !L.ClosureError
  | FetchClientAlreadyRegistered
  | UnknownFetchRegistration
  deriving (Eq, Show)

instance Exception NodeIntegrationError

-- | Node lifetime state. Completed points retain only a generation tombstone
-- until the kernel prunes them; their heavy reference/body maps are retired.
data AtomicFetchNode m owner peer blk = AtomicFetchNode
  { fetchKernel :: !(LeiosFetchKernel m peer blk)
  -- ^ Existing focus, pruning and legacy wire accounting authority.
  , fetchConfig :: !AtomicNodeConfig
  -- ^ Frozen explicit configuration.
  , fetchHandoff :: !(H.StorageHandoff m owner peer)
  -- ^ Node-owned missing, pending and durable transaction state.
  , fetchAssignments :: !(OrdinaryAssignments m peer)
  -- ^ Ordinary scheduling ownership for new-version requests.
  , fetchClients :: !(StrictTVar m (Map.Map (LeiosPeerLifetimeId peer) (C.AtomicClient m owner peer)))
  -- ^ Live new-version clients only, removed by exact registration.
  , fetchLegacy :: !(StrictTVar m (Map.Map (LeiosPeerLifetimeId peer) Natural))
  -- ^ Legacy invocations and their non-reusable local cleanup tokens.
  , fetchNextLegacy :: !(StrictTVar m Natural)
  -- ^ Next invocation token; delayed cleanup cannot remove a replacement.
  , fetchGenerations :: !(StrictTVar m (Map.Map LeiosPoint (Maybe AcquisitionGeneration)))
  -- ^ Active generation or completed tombstone. Focus loss removes an active
  -- generation so explicit refocus can start anew; receipts never recreate it.
  , fetchBodies :: !(StrictTVar m (B.BodyState peer (H.StorageRequest peer)))
  -- ^ Per-EB persistent primary/hedge slots and exact unresolved body originals.
  , fetchClosures :: !(StrictTVar m (L.ClosureState peer (H.StorageRequest peer)))
  -- ^ Persistent ordinary episodes, one rescue slot per EB and whole-batch debt.
  , fetchClosing :: !(StrictTVar m Bool)
  -- ^ Stop new planning before draining storage; final projection still runs.
  , fetchLegacyReceipts :: !(StrictTVar m Word32)
  -- ^ Node-owned legacy receipt waiters, including replies whose peer already left.
  , fetchOwnerThread :: !(ThreadId m)
  -- ^ Storage/receipt failures are linked to this node scope rather than a departing peer.
  , fetchObservation :: !(StrictTVar m NodeObservation)
  -- ^ Bounded functional evidence for negotiation and policy validation.
  }

readNodeObservation :: MonadSTM m => AtomicFetchNode m owner peer blk -> STM m NodeObservation
readNodeObservation = readTVar . fetchObservation

-- | Install before any connections start. The action owns and joins its
-- connections before returning; accepted storage then drains at node scope.
-- The legacy writer retains its own admission/validation and supplies only
-- durable receipts to the shared acquisition, never a new-version reservation.
withAtomicFetchNode ::
  (IOLike m, Ord peer, Ord owner) =>
  AtomicNodeConfig ->
  LeiosFetchKernel m peer blk ->
  (AtomicFetchNode m owner peer blk -> m a) ->
  m a
withAtomicFetchNode config kernel action = do
  handoff <-
    H.newStorageHandoff (nodeStorageLimits config) >>= either (throwIO . H.StorageException) pure
  assignments <- newOrdinaryAssignments
  clients <- uncheckedNewTVarM Map.empty
  legacy <- uncheckedNewTVarM Map.empty
  nextLegacy <- uncheckedNewTVarM 0
  generations <- uncheckedNewTVarM Map.empty
  bodies <- uncheckedNewTVarM B.emptyBodyState
  initialClosure <-
    either
      (throwIO . InvalidClosurePolicy)
      pure
      ( L.newClosureState
          (nodeClosureRetryConfig config)
          (checkpointTargetBytes (nodeCheckpointLimits config))
      )
  closures <- uncheckedNewTVarM initialClosure
  closing <- uncheckedNewTVarM False
  observation <- uncheckedNewTVarM (NodeObservation 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
  legacyReceipts <- uncheckedNewTVarM 0
  ownerThread <- myThreadId
  let node =
        AtomicFetchNode
          kernel
          config
          handoff
          assignments
          clients
          legacy
          nextLegacy
          generations
          bodies
          closures
          closing
          legacyReceipts
          ownerThread
          observation
      context = lfkContext kernel
      extension =
        LeiosFetchExtension
          { extensionDecide = decide node
          , extensionNextDeadline = nextPolicyDeadline node
          , extensionPrune = prune node
          , extensionRemovePeer = removePeer node
          , extensionLegacyContext = bridgeContext node
          }
      install = modifyMVar_ (lfcOutstanding context) $ \outstanding -> do
        atomically $ do
          current <- readTVar (lfkExtension kernel)
          forM_ current (const (throwSTM ExtensionAlreadyInstalled))
          peers <- readTVar (lfkPeers kernel)
          unless (Map.null peers) (throwSTM PeersAlreadyRunning)
          writeTVar (lfkExtension kernel) (Just extension)
          wake node
        pure outstanding
      uninstall = modifyMVar_ (lfcOutstanding context) $ \outstanding -> atomically $ do
        peers <- readTVar (lfkPeers kernel)
        unless (Map.null peers) (throwSTM PeersAlreadyRunning)
        -- The worker has drained. Project its final durable receipts even if
        -- the coordinator did not get another turn before scope release.
        shared <- projectKnown node outstanding
        writeTVar (lfkExtension kernel) Nothing
        wake node
        pure shared
      finish = do
        atomically $ writeTVar closing True >> wake node
        atomically (readTVar legacyReceipts >>= check . (== 0))
  bracket_ install uninstall $
    H.withStorageHandoff handoff (lfcWriter context) (storageEvent node) $ \_ ->
      action node `finally` finish

-- | Register one actual new-version client with its frozen D and one captured
-- connection-scoped RTT reader. The caller joins the interpreter in the action.
withNodeAtomicClient ::
  (IOLike m, Ord peer) =>
  AtomicFetchNode m owner peer blk ->
  owner ->
  LeiosPeerLifetimeId peer ->
  Word16 ->
  Maybe (KeepAliveRttReader m) ->
  (C.AtomicClient m owner peer -> m a) ->
  m a
withNodeAtomicClient node owner lifetime d rtt action =
  C.withAtomicClient
    (fetchHandoff node)
    (fetchAssignments node)
    owner
    lifetime
    d
    (nodeResponseBytes config)
    (nodeTimingConfig config)
    rtt
    hooks
    $ \client ->
      bracket_
        ( atomically $ do
            ensureRegistration node lifetime
            clients <- readTVar (fetchClients node)
            legacy <- readTVar (fetchLegacy node)
            when
              (Map.member lifetime clients || Map.member lifetime legacy)
              (throwSTM FetchClientAlreadyRegistered)
            modifyTVar (fetchClients node) (Map.insert lifetime client)
            observe node $ \s ->
              s
                { observedAtomicConnections = inc (observedAtomicConnections s)
                , observedScopedRttReaders =
                    if maybe False (const True) rtt
                      then inc (observedScopedRttReaders s)
                      else observedScopedRttReaders s
                , observedRttFallbacks =
                    if maybe True (const False) rtt then inc (observedRttFallbacks s) else observedRttFallbacks s
                }
            wake node
        )
        (atomically (removeAtomicClient node lifetime client))
        (action client)
 where
  config = fetchConfig node
  hooks =
    C.nullClientHooks
      { C.clientWakeup = wake node
      , C.clientTransaction = \key event accepted -> do
          when (accepted == H.Useful) $
            modifyTVar
              (fetchClosures node)
              (L.noteClosureService key (progressIndex event) (progressObservedAt event))
          refreshClosureRequest node key
          wake node
      , C.clientExpired = \handledAt expiry -> do
          state <- readTVar (fetchBodies node)
          let T.Expiry{T.expiryToken = T.TimerToken key _, T.chargeDataFailure} = expiry
              (change, next) = B.expireBodyAttempt key chargeDataFailure state
          writeTVar (fetchBodies node) next
          case change of
            B.BodyPrimaryTriggered -> observe node $ \s -> s{observedBodyTriggers = inc (observedBodyTriggers s)}
            B.BodyHedgeFailed -> observe node $ \s -> s{observedBodyFailures = inc (observedBodyFailures s)}
            B.NoBodyChange -> pure ()
          refreshClosureRequest node key
          closure <- readTVar (fetchClosures node)
          let (closureChange, nextClosure) = L.expireClosureAttempt handledAt key chargeDataFailure closure
          writeTVar (fetchClosures node) nextClosure
          case closureChange of
            L.ClosurePrimaryStalled -> observe node $ \s -> s{observedClosureStalls = inc (observedClosureStalls s)}
            L.ClosureRescueFailed -> observe node $ \s -> s{observedClosureFailures = inc (observedClosureFailures s)}
            L.NoClosureChange -> pure ()
      , C.clientReply = \key completion -> do
          state <- readTVar (fetchBodies node)
          let accepted = case R.completedOutcome completion of
                R.BodyReceived body -> H.acceptedBody body /= H.Irrelevant
                _ -> False
          won <- case B.bodyRequestPoint key state of
            Just (point, generation) | accepted -> do
              source <- H.readVerifiedBodySource (fetchHandoff node) point generation
              let winner = case source of
                    Just (H.BodyFromRequest original) -> Just original
                    Just H.BodyFromStorage -> Nothing
                    Nothing -> Just key
              pure (B.fulfillBody point generation winner state)
            _ -> pure state
          let next = case R.completedOutcome completion of
                R.CancelAcknowledged -> B.finishBodyCancel key won
                _ -> B.finishBodyOriginal key won
          writeTVar (fetchBodies node) next
          refreshClosureRequest node key
          modifyTVar (fetchClosures node) $ case R.completedOutcome completion of
            R.CancelAcknowledged -> L.finishClosureCancel key
            _ -> L.finishClosureOriginal key
      }

-- | Permit only an explicitly selected fallback connection to use legacy
-- scheduling. A Notify-only registration is not mistaken for a legacy fetcher.
withNodeLegacyClient ::
  (IOLike m, Ord peer) => AtomicFetchNode m owner peer blk -> LeiosPeerLifetimeId peer -> m a -> m a
withNodeLegacyClient node lifetime action = bracket register unregister (const action)
 where
  register = atomically $ do
    ensureRegistration node lifetime
    clients <- readTVar (fetchClients node)
    legacy <- readTVar (fetchLegacy node)
    when
      (Map.member lifetime clients || Map.member lifetime legacy)
      (throwSTM FetchClientAlreadyRegistered)
    token <- readTVar (fetchNextLegacy node)
    writeTVar (fetchNextLegacy node) (token + 1)
    writeTVar (fetchLegacy node) (Map.insert lifetime token legacy)
    observe node $ \s -> s{observedLegacyConnections = inc (observedLegacyConnections s)}
    wake node
    pure token
  unregister token = atomically $ do
    current <- readTVar (fetchLegacy node)
    when (Map.lookup lifetime current == Just token) (removePeer node lifetime)

-- | All participants signal the kernel's one coalescing STM readiness cell.
wake :: MonadSTM m => AtomicFetchNode m owner peer blk -> STM m ()
wake node = W.notifyReadiness (lfcReady (lfkContext (fetchKernel node)))

-- | Reject a late client start after Notify/the connection retired its exact
-- registration. Opening a storage session does not itself register a peer.
ensureRegistration ::
  (IOLike m, Ord peer) => AtomicFetchNode m owner peer blk -> LeiosPeerLifetimeId peer -> STM m ()
ensureRegistration node lifetime = do
  peers <- readTVar (lfkPeers (fetchKernel node))
  unless (Map.member lifetime peers) (throwSTM UnknownFetchRegistration)
  readTVar (fetchClosing node) >>= \closing -> when closing (throwSTM UnknownFetchRegistration)

-- | An old invocation's finalizer owns only its captured storage session.
removeAtomicClient ::
  (IOLike m, Ord peer) =>
  AtomicFetchNode m owner peer blk ->
  LeiosPeerLifetimeId peer ->
  C.AtomicClient m owner peer ->
  STM m ()
removeAtomicClient node lifetime original = do
  clients <- readTVar (fetchClients node)
  forM_ (Map.lookup lifetime clients) $ \current ->
    when
      ( H.storageSessionIdentity (C.clientStorageSession current)
          == H.storageSessionIdentity (C.clientStorageSession original)
      )
      $ removePeer node lifetime

-- | Publish the last shared possession view under the outstanding lock. No
-- request allocation, DB access or wire publication belongs to shutdown.
projectKnown ::
  IOLike m =>
  AtomicFetchNode m owner peer blk ->
  LeiosOutstanding (LeiosPeerLifetimeId peer) ->
  STM m (LeiosOutstanding (LeiosPeerLifetimeId peer))
projectKnown node outstanding = do
  generations <- readTVar (fetchGenerations node)
  foldM project outstanding (Map.toList generations)
 where
  project state (point, Just gen) = do
    current <- H.readAcquisition (fetchHandoff node) point
    pure $ case current of
      Just snapshot
        | H.snapshotGeneration snapshot == gen ->
            projectAcquisition (lfcStaticEnv (lfkContext (fetchKernel node))) point gen snapshot state
      _ -> state
  project state (_, Nothing) = pure state

observe ::
  MonadSTM m => AtomicFetchNode m owner peer blk -> (NodeObservation -> NodeObservation) -> STM m ()
observe node = modifyTVar (fetchObservation node)

inc :: Word64 -> Word64
inc n = if n == maxBound then n else n + 1

storageEvent :: MonadSTM m => AtomicFetchNode m owner peer blk -> H.StorageEvent -> STM m ()
storageEvent node event = do
  case event of
    H.BodyDurable point _ body _ -> notify (AcquiredEb point (encodeEndorserBlockSize body))
    H.ClosureDurable point _ -> notify (AcquiredEbTxs point)
    _ -> pure ()
  wake node
 where
  notify = lfcNotify (lfkContext (fetchKernel node))

removePeer ::
  (IOLike m, Ord peer) => AtomicFetchNode m owner peer blk -> LeiosPeerLifetimeId peer -> STM m ()
removePeer node lifetime = do
  clients <- readTVar (fetchClients node)
  forM_ (Map.lookup lifetime clients) C.closeAtomicClient
  modifyTVar (fetchClients node) (Map.delete lifetime)
  modifyTVar (fetchLegacy node) (Map.delete lifetime)
  modifyTVar (fetchBodies node) (B.removeBodyPeer lifetime)
  modifyTVar (fetchClosures node) (L.removeClosurePeer lifetime)
  releasePeerAssignments (fetchAssignments node) lifetime
  wake node

prune :: (IOLike m, Ord peer) => AtomicFetchNode m owner peer blk -> SlotNo -> STM m ()
prune node slot = do
  generations <- readTVar (fetchGenerations node)
  forM_ (Map.toList (Map.takeWhileAntitone ((< slot) . pointSlotNo) generations)) $ \(point, gen) ->
    forM_ gen (retire node point)
  modifyTVar (fetchGenerations node) (Map.dropWhileAntitone ((< slot) . pointSlotNo))
  wake node

retire ::
  (IOLike m, Ord peer) =>
  AtomicFetchNode m owner peer blk -> LeiosPoint -> AcquisitionGeneration -> STM m ()
retire node point gen = do
  releaseAcquisitionAssignments (fetchAssignments node) point gen
  source <- H.readVerifiedBodySource (fetchHandoff node) point gen
  forM_ source $ \winner ->
    modifyTVar (fetchBodies node) $
      B.fulfillBody
        point
        gen
        (case winner of H.BodyFromRequest key -> Just key; H.BodyFromStorage -> Nothing)
  H.retireAcquisition (fetchHandoff node) point gen
  modifyTVar (fetchBodies node) (B.retireBodyAcquisition point gen)
  modifyTVar (fetchClosures node) (L.retireClosureAcquisition point gen)

-- | Called under the kernel's outstanding-state lock. No DB operation belongs
-- here: storage callbacks supply all durable metadata. Legacy scheduling runs
-- afterwards, with its own unchanged quotas and request packing.
decide ::
  (IOLike m, Ord peer, Ord owner) =>
  AtomicFetchNode m owner peer blk ->
  Time ->
  (SlotNo -> Maybe LeiosEbBounds) ->
  Maybe (SlotNo, Word64) ->
  Map.Map (LeiosPeerLifetimeId peer) (LeiosFetchPeer m blk) ->
  Map.Map (LeiosPeerLifetimeId peer) (Map.Map LeiosPoint PeerOffer) ->
  LeiosOutstanding (LeiosPeerLifetimeId peer) ->
  m
    ( LeiosOutstanding (LeiosPeerLifetimeId peer)
    , Map.Map (LeiosPeerLifetimeId peer) (Map.Map LeiosPoint PeerOffer)
    )
decide node now bounds horizon peers offers outstanding = atomically $ do
  closing <- readTVar (fetchClosing node)
  active <- readTVar (fetchGenerations node)
  shared <-
    foldM
      (synchronize closing)
      outstanding
      (Set.toAscList (Set.union (Map.keysSet active) (Set.fromList points)))
  unless closing $ do
    driveBodies node now peers offers shared
    driveClosure node now peers offers shared
    clients <- readTVar (fetchClients node)
    times <- traverse (readTVar . lfpOfferTimes) peers
    forM_ ordered $ \point -> do
      generations <- readTVar (fetchGenerations node)
      forM_ (Map.lookup point generations >>= id) $ \gen -> do
        current <- H.readAcquisition (fetchHandoff node) point
        forM_ current $ \snapshot -> when (H.snapshotBodyDurable snapshot) $ do
          closure <- readTVar (fetchClosures node)
          forM_ (closureCandidates closure times clients point) $
            \(client, offer) -> fill shared point gen client offer
  legacy <- readTVar (fetchLegacy node)
  bodyClaims <- readTVar (fetchBodies node)
  pure
    ( shared
    , Map.map
        (Map.mapMaybeWithKey (withoutClaimedBody bodyClaims))
        (Map.restrictKeys offers (if closing then Set.empty else Map.keysSet legacy))
    )
 where
  config = fetchConfig node
  handoff = fetchHandoff node
  points =
    [ p
    | (p, MkEbState _ state) <- Map.toList (ebState outstanding)
    , isFocusedPoint p outstanding
    , needs state
    ]
  needs NoBody = True
  needs (BodyAcquired pool) = not (Jobs.nullLeiosJobPool pool)
  (high, low) = fetchPriorityTiers horizon [(pointSlotNo p, p) | p <- points]
  ordered = high ++ low
  synchronize closing state point = do
    generations <- readTVar (fetchGenerations node)
    case Map.lookup point generations of
      Just (Just gen) | not (Map.member point (ebState state) && isFocusedPoint point state) -> do
        retire node point gen
        modifyTVar (fetchGenerations node) (Map.delete point)
        pure state
      Just Nothing -> pure state
      Nothing | closing -> pure state
      previous -> do
        gen <- case previous of
          Just (Just g) -> pure (Just g)
          Nothing -> begin state point
        case gen of
          Nothing -> pure state
          Just g -> do
            unless closing $ do
              modifyTVar (fetchClosures node) (L.ensureClosureAcquisition point g)
              previousBody <- readTVar (fetchBodies node)
              let updatedBody = B.ensureBodyAcquisition point g (bounds (pointSlotNo point)) previousBody
              when (updatedBody /= previousBody) $ writeTVar (fetchBodies node) updatedBody >> wake node
            current <- H.readAcquisition handoff point
            case current of
              Nothing -> throwSTM (H.StorageException H.UnknownAcquisition)
              Just snapshot -> do
                let projected = projectAcquisition (lfcStaticEnv (lfkContext (fetchKernel node))) point g snapshot state
                when
                  ( H.snapshotBodyDurable snapshot
                      && IntSet.null (H.snapshotMissing snapshot)
                      && IntSet.null (H.snapshotPending snapshot)
                  )
                  $ do
                    retire node point g
                    modifyTVar (fetchGenerations node) (Map.insert point Nothing)
                pure projected
  -- An unoffered announcement can occupy a cheap empty arena indefinitely.
  -- Reclaim one only when offered work needs capacity; never evict verified
  -- metadata/storage, a wanted body attempt or outstanding legacy body work.
  -- Old wire descriptors retain their own generation/accounting after retire.
  begin state point = do
    initial <- H.beginAcquisition handoff point
    result <- case initial of
      Left H.AcquisitionCapacity -> do
        eligible <- hasBodyOffer point
        reclaimed <- if eligible then reclaim state point else pure False
        if reclaimed then H.beginAcquisition handoff point else pure initial
      _ -> pure initial
    case result of
      Right g -> modifyTVar (fetchGenerations node) (Map.insert point (Just g)) >> pure (Just g)
      Left H.AcquisitionCapacity -> pure Nothing
      Left e -> throwSTM (H.StorageException e)
  hasBodyOffer point = do
    clients <- readTVar (fetchClients node)
    legacy <- readTVar (fetchLegacy node)
    let registered = Map.keysSet clients `Set.union` Map.keysSet legacy
    pure $
      any
        ( \source -> case Map.lookup source offers >>= Map.lookup point of
            Just offer -> case poOffered offer of OfferedClosure -> False; _ -> True
            Nothing -> False
        )
        (Set.toList registered)
  reclaim state incoming = do
    generations <- readTVar (fetchGenerations node)
    search [(point, gen) | (point, Just gen) <- Map.toAscList generations, point /= incoming]
   where
    search [] = pure False
    search ((point, gen) : rest) = do
      offered <- hasBodyOffer point
      bodyState <- readTVar (fetchBodies node)
      snapshot <- H.readAcquisition handoff point
      let wanted =
            maybe
              False
              (\body -> B.bodyPrimary body /= Nothing || B.bodyHedge body /= Nothing)
              (B.bodySnapshot point bodyState)
          empty =
            maybe
              False
              (\current -> H.snapshotGeneration current == gen && H.snapshotReferences current == Nothing)
              snapshot
      if not offered && not wanted && empty && Map.notMember point (requestedEbPeers state)
        then do
          retire node point gen
          modifyTVar (fetchGenerations node) (Map.delete point)
          pure True
        else search rest
  closureCandidates closure times clients point =
    [ (client, offer)
    | (_, _, _, client, offer) <-
        sortOn
          (\(fails, at, lifetime, _, _) -> (fails, at, lifetime))
          [ (L.closureFailures lifetime closure, at, lifetime, client, offer)
          | (lifetime, client) <- Map.toAscList clients
          , Just offer <- [Map.lookup lifetime offers >>= Map.lookup point]
          , case poOffered offer of OfferedBody{} -> False; _ -> True
          , Just at <- [Map.lookup lifetime times >>= Map.lookup point >>= closureOfferTime]
          ]
    ]
  fill shared point gen client offer = do
    state <- C.readClientSnapshot client
    unless (C.snapshotClientClosed state || C.snapshotClientStopping state) $ do
      keepGoing <-
        ( do
            result <-
              assignCheckpoint
                (fetchAssignments node)
                handoff
                (nodeCheckpointLimits config)
                (C.clientStorageSession client)
                point
                gen
                offer
                shared
            case result of
              Right Nothing -> pure False
              Right (Just req) -> do
                C.publishClientRequest client now 1 req
                registerClosure node now L.ClosurePrimary req
                observe node $ \s -> s{observedCheckpoints = inc (observedCheckpoints s)}
                peak node client
                pure True
              Left (StorageAdmissionError e) | storageCapacity e -> pure False
              Left e -> throwSTM (InvalidAcquisition e)
        )
          `catch` \(e :: C.ClientError) ->
            if clientCapacity e then pure False else throwSTM e
      when keepGoing (fill shared point gen client offer)
  withoutClaimedBody claimed point offer
    | maybe
        True
        (\body -> B.bodyPrimary body == Nothing && not (B.bodyFulfilled body))
        (B.bodySnapshot point claimed) =
        Just offer
    | otherwise = case poOffered offer of
        OfferedBody{} -> Nothing
        _ -> Just offer{poOffered = OfferedClosure}

firstSuccessful :: Monad m => [a] -> (a -> m Bool) -> m Bool
firstSuccessful [] _ = pure False
firstSuccessful (x : xs) f = f x >>= \ok -> if ok then pure True else firstSuccessful xs f

peak ::
  (IOLike m, Ord peer) => AtomicFetchNode m owner peer blk -> C.AtomicClient m owner peer -> STM m ()
peak node client = do
  usage <- C.readClientUsage client
  forM_ usage $ \u -> observe node $ \s -> s{observedPeakData = max (observedPeakData s) (A.usedData u)}

clientCapacity :: C.ClientError -> Bool
clientCapacity = \case
  C.ClientStorageError e -> storageCapacity e
  C.ClientUpstreamBlocked -> True
  C.ClientStopping -> True
  C.ClientClosed -> True
  _ -> False

storageCapacity :: H.StorageError -> Bool
storageCapacity = \case
  H.OwnerByteCapacity -> True
  H.OwnerEntryCapacity -> True
  H.AdmissionFailure e ->
    e
      `elem` [A.DataCapacity, A.ExchangeCapacity, A.ResponseCapacity, A.HandoffCapacity, A.EntryCapacity]
  _ -> False

-- | Adapt only the durability boundary. Original legacy commands, byte quotas,
-- whole-vector validation, refunds and job identities stay on their old path.
-- Notification has one owner: idempotent shared storage events.
bridgeContext ::
  (IOLike m, Ord peer) =>
  AtomicFetchNode m owner peer blk -> LeiosFetchClientContext m (LeiosPeerLifetimeId peer)
bridgeContext node = context{lfcWriter = writer, lfcNotify = const (pure ())}
 where
  context = lfkContext (fetchKernel node)
  raw = lfcWriter context
  capture point = atomically $ Map.lookup point <$> readTVar (fetchGenerations node)
  writer =
    raw
      { writeEbBody = \point body -> do
          generation <- capture point
          trackLegacyReceipt node (writeEbBody raw point body) $ \held -> do
            forM_ (generation >>= id) $ \gen ->
              H.registerDurableBody (fetchHandoff node) (storageEvent node) point gen body (IntSet.fromList held)
                >>= bridgeResult
            pure held
      , writeTxs = \point txs -> do
          generation <- capture point
          trackLegacyReceipt node (writeTxs raw point txs) $ \receipt -> do
            forM_ (generation >>= id) $ \gen -> atomically $ do
              if null (txWriteWrongSize receipt)
                then
                  H.registerDurableTransactions
                    (fetchHandoff node)
                    (storageEvent node)
                    point
                    gen
                    (txWriteConfirmed receipt)
                    >>= bridgeResultSTM
                else do
                  current <- readTVar (fetchGenerations node)
                  when (Map.lookup point current == Just (Just gen)) $ do
                    retire node point gen
                    modifyTVar (fetchGenerations node) (Map.insert point Nothing)
              wake node
            pure receipt
      }
  bridgeResult = either (\e -> unless (obsolete e) (throwIO (H.StorageException e))) pure
  bridgeResultSTM = either (\e -> unless (obsolete e) (throwSTM (H.StorageException e))) pure
  obsolete e = e == H.UnknownAcquisition || e == H.StaleAcquisition

-- | Once the legacy writer has accepted a submission, receipt reconciliation
-- belongs to the node even if cancellation prevents the old reply handler from
-- awaiting its promise. Mask the submission-to-owner handoff, retain one result,
-- and drain these waiters before shutting down the shared acquisition arena.
trackLegacyReceipt ::
  forall m owner peer blk a.
  IOLike m => AtomicFetchNode m owner peer blk -> m (Promise m a) -> (a -> m a) -> m (Promise m a)
trackLegacyReceipt node submit reconcile = mask_ $ do
  promise <- submit
  result <- Strict.newEmptyTMVarIO
  atomically $ do
    count <- readTVar (fetchLegacyReceipts node)
    when (count == maxBound) (throwSTM (H.StorageException H.OwnerEntryCapacity))
    writeTVar (fetchLegacyReceipts node) (count + 1)
  let finished = modifyTVar (fetchLegacyReceipts node) (subtract 1)
  task <-
    ( async $ do
        outcome <- (try (await promise >>= reconcile) :: m (Either SomeException a))
        atomically $ do
          Strict.putTMVar result outcome
          finished
        either throwIO (const (pure ())) outcome
    )
      `onException` atomically finished
  linkTo (fetchOwnerThread node) task
  pure $ Promise $ atomically (Strict.readTMVar result) >>= either throwIO pure

-- | Read logical slots and unresolved original debt without retaining history.
readNodeBodySnapshots ::
  MonadSTM m => AtomicFetchNode m owner peer blk -> STM m (Map.Map LeiosPoint (B.BodySnapshot peer))
readNodeBodySnapshots node = B.bodySnapshots <$> readTVar (fetchBodies node)

-- | Physical per-session usage, including canceled originals and their ACKs.
readNodeClientUsage ::
  (MonadSTM m, Ord peer) =>
  AtomicFetchNode m owner peer blk ->
  STM m (Map.Map (LeiosPeerLifetimeId peer) (Either H.StorageError A.Usage))
readNodeClientUsage node = readTVar (fetchClients node) >>= traverse C.readClientUsage

-- | Read-only deadline contribution to the kernel coordinator. An old scope
-- can be observed while its wake is in flight; closing scopes expose no timers.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#agreed-coordinator-wakeups@.
nextPolicyDeadline ::
  IOLike m => AtomicFetchNode m owner peer blk -> Time -> STM m (Maybe Time)
nextPolicyDeadline node now = do
  closing <- readTVar (fetchClosing node)
  if closing
    then pure Nothing
    else do
      clients <- readTVar (fetchClients node)
      snapshots <- traverse C.readClientSnapshot clients
      closure <- readTVar (fetchClosures node)
      let cooldown = L.nextClosureRetryDeadline now closure
      pure $ case mapMaybe
        id
        (cooldown : map (fmap snd . T.nextDeadline . C.snapshotClientTiming) (Map.elems snapshots)) of
        [] -> Nothing
        due -> Just (minimum due)

-- | Run only by the kernel coordinator. Never change an
-- admitted timer snapshot, and process verified winners before issuing cancels.
driveBodies ::
  (IOLike m, Ord owner, Ord peer) =>
  AtomicFetchNode m owner peer blk ->
  Time ->
  Map.Map (LeiosPeerLifetimeId peer) (LeiosFetchPeer m blk) ->
  Map.Map (LeiosPeerLifetimeId peer) (Map.Map LeiosPoint PeerOffer) ->
  LeiosOutstanding (LeiosPeerLifetimeId peer) ->
  STM m ()
driveBodies node now peers offers outstanding = do
  clients <- readTVar (fetchClients node)
  mapM_ (`C.pollAtomicClient` now) clients
  forM_ clients $ \client -> do
    snapshot <- C.readClientSnapshot client
    forM_ (T.rescuableRequests (C.snapshotClientTiming snapshot)) $ \key ->
      modifyTVar bodies (B.rescueBlockedBody key)
  times <- traverse (readTVar . lfpOfferTimes) peers
  policies <- readNodeBodySnapshots node
  forM_ (Map.toList policies) $ \(point, snapshot) -> do
    let generation = B.bodyGeneration snapshot
    if not (Map.member point (ebState outstanding) && isFocusedPoint point outstanding)
      then modifyTVar bodies (B.retireBodyAcquisition point generation)
      else do
        source <- H.readVerifiedBodySource (fetchHandoff node) point generation
        forM_ source $ \winner ->
          modifyTVar
            bodies
            ( B.fulfillBody
                point
                generation
                (case winner of H.BodyFromRequest key -> Just key; H.BodyFromStorage -> Nothing)
            )
        let arrived =
              sortOn
                fst
                [ (at, lifetime)
                | lifetime <- Map.keys clients
                , Just offer <- [Map.lookup lifetime offers >>= Map.lookup point]
                , offersBody offer
                , Just at <- [Map.lookup lifetime times >>= Map.lookup point >>= bodyOfferTime]
                ]
        modifyTVar bodies (B.registerBodyOffers point (map snd arrived))
  -- Withdraw once even when no control reservation is currently available.
  unwithdrawn <- B.unwithdrawnBodyAttempts <$> readTVar bodies
  forM_ unwithdrawn $ \key -> forM_ (clientFor clients key) $ \client -> do
    (C.withdrawClientRequest client now key) `catch` \(e :: C.ClientError) -> case e of
      C.ClientTimingEventError T.UnknownTimingKey -> pure ()
      _ -> throwSTM e
    modifyTVar bodies (B.recordBodyWithdrawal key)
  pending <- B.pendingBodyCancels <$> readTVar bodies
  forM_ pending $ \(original, request) -> forM_ (clientFor clients original) $ \client -> do
    result <- C.cancelClientRequest client now request
    case result of
      Right cancelKey -> do
        current <- readTVar bodies
        updated <-
          either (throwSTM . InvalidBodyPolicy) pure (B.recordBodyCancel original cancelKey current)
        writeTVar bodies updated
        observe node $ \s -> s{observedBodyCancels = inc (observedBodyCancels s)}
      Left e | clientCapacity e || retiredTarget e -> pure ()
      Left e -> throwSTM e
  forM_ (Map.keys policies) $ \point -> do
    current <- readTVar bodies
    case B.bodySnapshot point current of
      Just snapshot
        | not (B.bodyFulfilled snapshot)
        , Map.notMember point (requestedEbPeers outstanding) -> do
            issued <- issue clients point snapshot
            when issued $ do
              next <- readTVar bodies
              -- A replacement primary can retain an already-fired trigger; fill its
              -- separate hedge slot without waiting for another unrelated wake.
              forM_ (B.bodySnapshot point next) $ \fresh -> void (issue clients point fresh)
      _ -> pure ()
 where
  bodies = fetchBodies node
  config = fetchConfig node
  offersBody offer = case poOffered offer of OfferedClosure -> False; _ -> True
  clientFor clients key@(RequestKey lifetime _ _) = do
    client <- Map.lookup lifetime clients
    let (registered, session) = H.storageSessionIdentity (C.clientStorageSession client)
    case key of
      RequestKey original token _ | original == registered && token == session -> Just client
      _ -> Nothing
  retiredTarget (C.ClientStorageError (H.AdmissionFailure e)) = e `elem` [A.UnknownRequest, A.InvalidCancelTarget, A.AlreadyCancelled]
  retiredTarget _ = False
  issue clients point snapshot = do
    state <- readTVar bodies
    case B.nextBodyRole point state of
      Nothing -> pure False
      Just role -> firstSuccessful (B.bodyCandidates point state) $ \lifetime ->
        case (Map.lookup lifetime clients, Map.lookup lifetime offers >>= Map.lookup point) of
          (Just client, Just offer) -> case poOffered offer of
            OfferedClosure -> pure False
            half -> do
              let announced = case half of OfferedBody size -> size; OfferedBodyAndClosure size -> size
                  trusted = case poEbBounds offer of SJust b -> Just b; SNothing -> B.bodyBounds snapshot
                  factor = case role of
                    B.BodyPrimary -> 1
                    B.BodyHedge -> D.bodyBackoffFactor (nodeTimingConfig config) (B.bodyFailures snapshot)
              case trusted of
                Nothing -> pure False
                Just bounds -> do
                  prepared <-
                    either (throwSTM . H.StorageException . const H.InvalidDescriptor) pure $
                      prepareBody
                        (BodyDescriptor point (B.bodyGeneration snapshot) announced bounds)
                        (EncodedBytes (fromIntegral (lebMaxBodySize bounds) + 3))
                  result <- C.admitClientRequest client now factor prepared
                  case result of
                    Left e | clientCapacity e -> pure False
                    Left e -> throwSTM e
                    Right request -> do
                      current <- readTVar bodies
                      updated <-
                        either (throwSTM . InvalidBodyPolicy) pure $
                          B.admitBodyAttempt
                            point
                            (B.bodyGeneration snapshot)
                            role
                            (H.storageRequestKey request)
                            request
                            current
                      writeTVar bodies updated
                      let wanted = maybe 0 B.bodyWanted (B.bodySnapshot point updated)
                      observe node $ \s ->
                        s
                          { observedBodyRequests = inc (observedBodyRequests s)
                          , observedBodyHedges =
                              if role == B.BodyHedge then inc (observedBodyHedges s) else observedBodyHedges s
                          , observedPeakWantedBodies = max (observedPeakWantedBodies s) wanted
                          }
                      peak node client
                      pure True
          _ -> pure False

-- | Logical closure evidence; wire reservations remain visible separately.
readNodeClosureSnapshots ::
  MonadSTM m =>
  AtomicFetchNode m owner peer blk -> STM m (Map.Map LeiosPoint (L.ClosureSnapshot peer))
readNodeClosureSnapshots node = L.closureSnapshots <$> readTVar (fetchClosures node)

-- | Refresh possession before charging expiry: a verified winner on another
-- connection can already have made this original redundant.
refreshClosureRequest ::
  (MonadSTM m, Ord peer) => AtomicFetchNode m owner peer blk -> RequestKey peer -> STM m ()
refreshClosureRequest node key = do
  state <- readTVar (fetchClosures node)
  forM_ (L.closureRequestPoint key state) $ \(point, gen) -> do
    current <- H.readAcquisition (fetchHandoff node) point
    forM_ current $ \snapshot ->
      when (H.snapshotGeneration snapshot == gen) $
        modifyTVar (fetchClosures node) (L.refreshClosure point gen (H.snapshotMissing snapshot))

registerClosure ::
  (IOLike m, Ord peer) =>
  AtomicFetchNode m owner peer blk -> Time -> L.ClosureRole peer -> H.StorageRequest peer -> STM m ()
registerClosure node now role request = case preparedDescriptor (H.storageRequestPrepared request) of
  BodyRequest{} -> throwSTM (InvalidClosurePolicy L.InvalidClosureAttempt)
  BatchRequest batch -> do
    current <- H.readAcquisition (fetchHandoff node) (batchPoint batch)
    forM_ current $ \snapshot ->
      modifyTVar
        (fetchClosures node)
        (L.refreshClosure (batchPoint batch) (batchGeneration batch) (H.snapshotMissing snapshot))
    state <- readTVar (fetchClosures node)
    updated <-
      either
        (throwSTM . InvalidClosurePolicy)
        pure
        (L.admitClosureAttempt now role (H.storageRequestKey request) request batch state)
    writeTVar (fetchClosures node) updated
    let copies =
          [ n | snapshot <- Map.elems (L.closureSnapshots updated), n <- IntMap.elems (L.closureCopies snapshot)
          ]
    observe node $ \s -> s{observedPeakClosureCopies = maximum (observedPeakClosureCopies s : copies)}

-- | Capacity failure during a whole-batch transfer rolls back the old logical
-- release as well as any new admission. It is an internal STM retry result.
data HandoffUnavailable = HandoffUnavailable deriving Show

instance Exception HandoffUnavailable

-- | Exact invocation lookup. An old terminal/cancel can never mutate a new
-- invocation registered under the same connection lifetime.
closureClientFor ::
  Ord peer =>
  Map.Map (LeiosPeerLifetimeId peer) (C.AtomicClient m owner peer) ->
  RequestKey peer ->
  Maybe (C.AtomicClient m owner peer)
closureClientFor clients (RequestKey lifetime token _) = do
  client <- Map.lookup lifetime clients
  if H.storageSessionIdentity (C.clientStorageSession client) == (lifetime, token)
    then Just client
    else Nothing

-- | The kernel coordinator drives bodies and closure policy under its lock.
-- Rescue uses only idle eligible pipelines; productive handoff may append to a
-- healthy pipeline, preserving the existing order. E18 adds adaptive retry.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#agreed-liveness-rule-for-an-unfinished-data-reply@.
driveClosure ::
  (IOLike m, Ord owner, Ord peer) =>
  AtomicFetchNode m owner peer blk ->
  Time ->
  Map.Map (LeiosPeerLifetimeId peer) (LeiosFetchPeer m blk) ->
  Map.Map (LeiosPeerLifetimeId peer) (Map.Map LeiosPoint PeerOffer) ->
  LeiosOutstanding (LeiosPeerLifetimeId peer) ->
  STM m ()
driveClosure node now peers offers outstanding = do
  modifyTVar (fetchClosures node) (L.advanceClosureRetries now)
  clients <- readTVar (fetchClients node)
  policies <- L.closureSnapshots <$> readTVar state
  forM_ (Map.toList policies) $ \(point, policy) -> do
    let gen = L.closureGeneration policy
    if not (Map.member point (ebState outstanding) && isFocusedPoint point outstanding)
      then modifyTVar state (L.retireClosureAcquisition point gen)
      else do
        current <- H.readAcquisition handoff point
        forM_ current $ \snapshot ->
          when (H.snapshotGeneration snapshot == gen) $
            modifyTVar state (L.refreshClosure point gen (H.snapshotMissing snapshot))
  forM_ clients $ \client -> do
    snapshot <- C.readClientSnapshot client
    forM_ (T.rescuableRequests (C.snapshotClientTiming snapshot)) $ \key -> modifyTVar state (L.rescueBlockedClosure key)
  settle clients
  times <- traverse (readTVar . lfpOfferTimes) peers
  transfer clients
  rescueNext clients times
  settle clients
 where
  state = fetchClosures node
  handoff = fetchHandoff node
  limits = nodeCheckpointLimits (fetchConfig node)
  fullOffer lifetime point = do
    offer <- Map.lookup lifetime offers >>= Map.lookup point
    case poOffered offer of { OfferedBody{} -> Nothing; _ -> Just offer }
  offered times point =
    map snd $
      sortOn
        (\(at, p) -> (at, p))
        [ (at, lifetime)
        | lifetime <- Map.keys offers
        , Just _ <- [fullOffer lifetime point]
        , Just at <- [Map.lookup lifetime times >>= Map.lookup point >>= closureOfferTime]
        ]
  settle clients = do
    keys <- L.unwithdrawnClosureAttempts <$> readTVar state
    forM_ keys $ \key -> forM_ (closureClientFor clients key) $ \client -> do
      (C.withdrawClientRequest client now key) `catch` \(e :: C.ClientError) -> case e of
        C.ClientTimingEventError T.UnknownTimingKey -> pure ()
        _ -> throwSTM e
      modifyTVar state (L.recordClosureWithdrawal key)
    pending <- L.pendingClosureCancels <$> readTVar state
    forM_ pending $ \(key, request) -> forM_ (closureClientFor clients key) $ \client -> do
      -- Acceptance and its element counter commit together, before the client
      -- callback. Do not cancel an already complete payload in that callback gap.
      complete <- H.readStorageRequestComplete handoff request
      unless (complete == Right True) $ do
        result <- C.cancelClientRequest client now request
        case result of
          Right ack -> do
            current <- readTVar state
            updated <- either (throwSTM . InvalidClosurePolicy) pure (L.recordClosureCancel key ack current)
            writeTVar state updated
            observe node $ \s -> s{observedClosureCancels = inc (observedClosureCancels s)}
          Left e | clientCapacity e || retiredTarget e -> pure ()
          Left e -> throwSTM e

  retiredTarget (C.ClientStorageError (H.AdmissionFailure e)) = e `elem` [A.UnknownRequest, A.InvalidCancelTarget, A.AlreadyCancelled]
  retiredTarget _ = False
  rescueNext clients times = do
    current <- readTVar state
    issued <- firstSuccessful (L.closureRescuePlans now current) $ \(parent, point, gen, eligible) -> do
      fresh <- readTVar state
      firstSuccessful (L.closureRescueCandidates parent (offered times point) fresh) $ \lifetime ->
        case Map.lookup lifetime clients of
          Nothing -> pure False
          Just client -> do
            snapshot <- C.readClientSnapshot client
            if C.snapshotClientClosed snapshot
              || C.snapshotClientStopping snapshot
              || not (T.timingIdle (C.snapshotClientTiming snapshot))
              then pure False
              else do
                currentAcquisition <- H.readAcquisition handoff point
                case currentAcquisition of
                  Just acquisition | H.snapshotGeneration acquisition == gen ->
                    case buildCheckpoint limits point acquisition eligible of
                      Left e -> throwSTM (InvalidAcquisition e)
                      Right Nothing -> pure False
                      Right (Just prepared) -> do
                        result <- C.admitClientRequest client now (L.closureRescueFactor point fresh) prepared
                        case result of
                          Left e | clientCapacity e -> pure False
                          Left e -> throwSTM e
                          Right request -> do
                            registerClosure node now (L.ClosureRescue parent) request
                            observe node $ \s -> s{observedClosureRescues = inc (observedClosureRescues s)}
                            peak node client
                            pure True
                  _ -> pure False
    when issued (rescueNext clients times)
  transfer clients = do
    current <- readTVar state
    issued <- firstSuccessful (L.closureTailPlans current) $ \(old, lifetime, eligible) ->
      case (L.closureRequestPoint old current, Map.lookup lifetime clients) of
        (Just (point, gen), Just client) -> case fullOffer lifetime point of
          Nothing -> pure False
          Just offer ->
            ( do
                -- A failure below must roll back this whole original's release.
                releaseCheckpoint (fetchAssignments node) old
                modifyTVar state (L.abandonClosureAttempt old)
                result <-
                  assignCheckpointWithin
                    (fetchAssignments node)
                    handoff
                    limits
                    (C.clientStorageSession client)
                    point
                    gen
                    offer
                    outstanding
                    (Just eligible)
                case result of
                  Left (StorageAdmissionError e) | storageCapacity e -> throwSTM HandoffUnavailable
                  Left e -> throwSTM (InvalidAcquisition e)
                  Right Nothing -> throwSTM HandoffUnavailable
                  Right (Just request) -> do
                    C.publishClientRequest client now 1 request
                    registerClosure node now L.ClosurePrimary request
                    observe node $ \s ->
                      s
                        { observedCheckpoints = inc (observedCheckpoints s)
                        , observedClosureHandoffs = inc (observedClosureHandoffs s)
                        }
                    peak node client
                    pure True
            )
              `catch` (\HandoffUnavailable -> pure False)
              `catch` (\(e :: C.ClientError) -> if clientCapacity e then pure False else throwSTM e)
        _ -> pure False
    when issued (transfer clients)

-- | Count live and draining storage ownership, including closed peer sessions.
-- This read-only view retains no payload/event history and starts no work.
readNodeStorageSnapshot :: MonadSTM m => AtomicFetchNode m owner peer blk -> STM m H.StorageSnapshot
readNodeStorageSnapshot = H.readStorageSnapshot . fetchHandoff
