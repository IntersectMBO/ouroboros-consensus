{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Explicit node-scoped atomic acquisition. Normal node assembly does not
-- install this extension. Connections share durable possession and budgets;
-- their wire identities and immutable reply contexts remain independent.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#10-admission-shutdown-and-compatibility@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Node
  ( AtomicFetchNode
  , AtomicNodeConfig (..)
  , NodeObservation (..)
  , NodeIntegrationError (..)
  , withAtomicFetchNode
  , withNodeAtomicClient
  , withNodeLegacyClient
  , readNodeObservation
  ) where

import qualified Control.Concurrent.Class.MonadSTM.Strict as Strict
import Control.Monad (foldM, forM_, forever, unless, void, when)
import qualified Data.IntSet as IntSet
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Set as Set
import Data.Word (Word16, Word32, Word64)
import Ouroboros.Consensus.Block (SlotNo)
import Ouroboros.Consensus.Leios.Acquisitions (LeiosEbNotification (..))
import Ouroboros.Consensus.Leios.Types (LeiosPoint (..), encodeEndorserBlockSize)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Acquisition
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Admission as A
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Client as C
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Deadline as D
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Descriptor
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Handoff as H
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

-- | Explicit fixture/development sizing. No normal node defaults or version
-- advertisement are changed by constructing this value.
data AtomicNodeConfig = AtomicNodeConfig
  { nodeStorageLimits :: !H.StorageLimits
  -- ^ Node-owned storage/session/acquisition bounds, including draining peers.
  , nodeResponseBytes :: !Word32
  -- ^ Per-connection shared response budget, across every EB and request kind.
  , nodeCheckpointLimits :: !CheckpointLimits
  -- ^ Whole-transaction checkpoint target and independent protocol hard caps.
  , nodeTimingConfig :: !D.TimingConfig
  -- ^ Receiver policy, checked against the actual protocol guard by each client.
  }

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
  , observedCheckpoints :: !Word64
  -- ^ Admitted new-version transaction batches.
  , observedPeakData :: !Int
  -- ^ Largest one-session live data-record count across concurrent EBs.
  }
  deriving (Eq, Show)

-- | Local integration violations, distinct from malformed peer replies.
data NodeIntegrationError
  = ExtensionAlreadyInstalled
  | PeersAlreadyRunning
  | InvalidAcquisition !AcquisitionError
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
  , fetchLegacy :: !(StrictTVar m (Set.Set (LeiosPeerLifetimeId peer)))
  -- ^ Connections eligible for the existing legacy scheduler.
  , fetchGenerations :: !(StrictTVar m (Map.Map LeiosPoint (Maybe AcquisitionGeneration)))
  -- ^ Active generation, or completed/abandoned tombstone. Never recreated by a receipt.
  , fetchBodies :: !(StrictTVar m (Map.Map LeiosPoint (RequestKey peer)))
  -- ^ One ordinary new-version body request per point; not a wire-debt ledger.
  , fetchReady :: !(StrictTVar m Bool)
  -- ^ Coalesced STM-to-MVar wakeup, with no retained event queue.
  , fetchLegacyReceipts :: !(StrictTVar m Word32)
  -- ^ Node-owned legacy receipt waiters, including replies whose peer already left.
  , fetchOwnerThread :: !(ThreadId m)
  -- ^ Storage/receipt failures are linked to this node scope rather than a departing peer.
  , fetchObservation :: !(StrictTVar m NodeObservation)
  -- ^ Bounded functional evidence for explicit test/development activation.
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
  legacy <- uncheckedNewTVarM Set.empty
  generations <- uncheckedNewTVarM Map.empty
  bodies <- uncheckedNewTVarM Map.empty
  ready <- uncheckedNewTVarM False
  observation <- uncheckedNewTVarM (NodeObservation 0 0 0 0 0 0 0)
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
          generations
          bodies
          ready
          legacyReceipts
          ownerThread
          observation
      context = lfkContext kernel
      extension = LeiosFetchExtension (decide node) (prune node) (removePeer node) (bridgeContext node)
      install = modifyMVar_ (lfcOutstanding context) $ \outstanding -> do
        atomically $ do
          current <- readTVar (lfkExtension kernel)
          forM_ current (const (throwSTM ExtensionAlreadyInstalled))
          peers <- readTVar (lfkPeers kernel)
          unless (Map.null peers) (throwSTM PeersAlreadyRunning)
          writeTVar (lfkExtension kernel) (Just extension)
        pure outstanding
      uninstall = modifyMVar_ (lfcOutstanding context) $ \outstanding -> do
        atomically $ do
          peers <- readTVar (lfkPeers kernel)
          unless (Map.null peers) (throwSTM PeersAlreadyRunning)
          writeTVar (lfkExtension kernel) Nothing
        pure outstanding
      wakeLoop = forever $ do
        atomically $ do
          readTVar ready >>= check
          writeTVar ready False
        void $ tryPutMVar (lfcReady context) ()
  bracket_ install uninstall $
    H.withStorageHandoff handoff (lfcWriter context) (storageEvent node) $ \_ ->
      withAsync wakeLoop $ \_ -> action node `finally` atomically (readTVar legacyReceipts >>= check . (== 0))

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
        (atomically (removePeer node lifetime))
        (action client)
 where
  config = fetchConfig node
  hooks =
    C.nullClientHooks
      { C.clientWakeup = wake node
      , C.clientReply = \key _ -> modifyTVar (fetchBodies node) (Map.filter (/= key))
      }

-- | Permit only an explicitly selected fallback connection to use legacy
-- scheduling. A Notify-only registration is not mistaken for a legacy fetcher.
withNodeLegacyClient ::
  (IOLike m, Ord peer) => AtomicFetchNode m owner peer blk -> LeiosPeerLifetimeId peer -> m a -> m a
withNodeLegacyClient node lifetime =
  bracket_
    ( atomically $ do
        modifyTVar (fetchLegacy node) (Set.insert lifetime)
        observe node $ \s -> s{observedLegacyConnections = inc (observedLegacyConnections s)}
        wake node
    )
    (atomically (removePeer node lifetime))

wake :: MonadSTM m => AtomicFetchNode m owner peer blk -> STM m ()
wake node = writeTVar (fetchReady node) True

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
  modifyTVar (fetchLegacy node) (Set.delete lifetime)
  modifyTVar
    (fetchBodies node)
    (Map.filter (\key -> case key of RequestKey original _ _ -> original /= lifetime))
  releasePeerAssignments (fetchAssignments node) lifetime
  wake node

prune :: (IOLike m, Ord peer) => AtomicFetchNode m owner peer blk -> SlotNo -> STM m ()
prune node slot = do
  generations <- readTVar (fetchGenerations node)
  forM_ (Map.toList (Map.takeWhileAntitone ((< slot) . pointSlotNo) generations)) $ \(point, gen) ->
    forM_ gen (retire node point)
  modifyTVar (fetchGenerations node) (Map.dropWhileAntitone ((< slot) . pointSlotNo))
  modifyTVar (fetchBodies node) (Map.dropWhileAntitone ((< slot) . pointSlotNo))
  wake node

retire ::
  (IOLike m, Ord peer) =>
  AtomicFetchNode m owner peer blk -> LeiosPoint -> AcquisitionGeneration -> STM m ()
retire node point gen = do
  releaseAcquisitionAssignments (fetchAssignments node) point gen
  H.retireAcquisition (fetchHandoff node) point gen

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
  active <- readTVar (fetchGenerations node)
  shared <-
    foldM synchronize outstanding (Set.toAscList (Set.union (Map.keysSet active) (Set.fromList points)))
  clients <- readTVar (fetchClients node)
  times <- traverse (readTVar . lfpOfferTimes) peers
  forM_ ordered $ \point -> do
    generations <- readTVar (fetchGenerations node)
    forM_ (Map.lookup point generations >>= id) $ \gen -> do
      current <- H.readAcquisition (fetchHandoff node) point
      forM_ current $ \snapshot -> do
        unless (H.snapshotBodyDurable snapshot || isJust (H.snapshotReferences snapshot)) $ do
          bodyClaims <- readTVar (fetchBodies node)
          unless (Map.member point bodyClaims || Map.member point (requestedEbPeers shared)) $
            firstSuccessful (bodyCandidates times clients point) $ \(client, offer) -> do
              case poOffered offer of
                OfferedClosure -> pure False
                half -> do
                  let announced = case half of
                        OfferedBody size -> size
                        OfferedBodyAndClosure size -> size
                      trusted = case poEbBounds offer of
                        SJust b -> Just b
                        SNothing -> bounds (pointSlotNo point)
                  case trusted of
                    Nothing -> pure False
                    Just b -> do
                      prepared <-
                        either (throwSTM . H.StorageException . const H.InvalidDescriptor) pure $
                          prepareBody
                            (BodyDescriptor point gen announced b)
                            (EncodedBytes (fromIntegral (lebMaxBodySize b) + 3))
                      admitted <- C.admitClientRequest client now 1 prepared
                      case admitted of
                        Left e | clientCapacity e -> pure False
                        Left e -> throwSTM e
                        Right req -> do
                          modifyTVar (fetchBodies node) (Map.insert point (H.storageRequestKey req))
                          observe node $ \s -> s{observedBodyRequests = inc (observedBodyRequests s)}
                          peak node client
                          pure True
        when (H.snapshotBodyDurable snapshot) $
          forM_ (closureCandidates times clients point) $
            \(client, offer) -> fill shared point gen client offer
  legacy <- readTVar (fetchLegacy node)
  bodyClaims <- readTVar (fetchBodies node)
  pure
    ( shared
    , Map.map (Map.mapMaybeWithKey (withoutClaimedBody bodyClaims)) (Map.restrictKeys offers legacy)
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
  synchronize state point = do
    generations <- readTVar (fetchGenerations node)
    case Map.lookup point generations of
      Just Nothing -> pure state
      previous -> do
        gen <- case previous of
          Just (Just g) -> pure (Just g)
          Nothing ->
            H.beginAcquisition handoff point >>= \case
              Right g -> modifyTVar (fetchGenerations node) (Map.insert point (Just g)) >> pure (Just g)
              Left H.AcquisitionCapacity -> pure Nothing
              Left e -> throwSTM (H.StorageException e)
        case gen of
          Nothing -> pure state
          Just g -> do
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
  candidates which times clients point =
    [ (client, offer)
    | (_, client, offer) <-
        sortOn
          (\(at, _, _) -> at)
          [ (at, client, offer)
          | (lifetime, client) <- Map.toAscList clients
          , Just offer <- [Map.lookup lifetime offers >>= Map.lookup point]
          , Just at <- [Map.lookup lifetime times >>= Map.lookup point >>= which]
          ]
    ]
  bodyCandidates = candidates bodyOfferTime
  closureCandidates = candidates closureOfferTime
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
    | Map.notMember point claimed = Just offer
    | otherwise = case poOffered offer of
        OfferedBody{} -> Nothing
        _ -> Just offer{poOffered = OfferedClosure}

firstSuccessful :: Monad m => [a] -> (a -> m Bool) -> m ()
firstSuccessful [] _ = pure ()
firstSuccessful (x : xs) f = f x >>= \ok -> unless ok (firstSuccessful xs f)

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
