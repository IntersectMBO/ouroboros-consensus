{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The node's LeiosFetch state, which all its LeiosFetch clients share, and
-- the threads that maintain it: the decision loop, which assigns the peers'
-- offers to requests, and the pruner.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Kernel
  ( LeiosFetchKernel (..)
  , LeiosFetchPeer (..)
  , LeiosFetchExtension (..)
  , OfferTimes (..)
  , legacyClientContext
  , LeiosPeerLifetimeId (..)
  , newLeiosFetchKernel
  , newLeiosFetchKernelWithAnnouncements
  , withLeiosFetchPeer
  , recordLeiosOffer
  , onNewLeiosAnnouncement
  , leiosFetchDecisionLoop
  , leiosFetchPruner
  ) where

import Control.Monad (forM_, unless, when)
import Control.Monad.Class.MonadTime.SI (addTime)
import Control.Monad.Class.MonadTimer.SI (MonadTimer)
import Control.Monad.Except (runExcept)
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Sequence.NonEmpty as NESeq
import Data.Void (Void)
import Data.Word (Word64)
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Block
  ( BlockProtocol
  , Proxy (..)
  , SlotNo (..)
  , WithOrigin (..)
  , withOrigin
  )
import Ouroboros.Consensus.BlockchainTime (BlockchainTime (..), CurrentSlot (..))
import Ouroboros.Consensus.Config (TopLevelConfig, configLedger)
import Ouroboros.Consensus.Forecast (forecastFor)
import Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory (..))
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.Ledger.Abstract (getTipSlot)
import Ouroboros.Consensus.Ledger.Extended (ledgerState)
import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol (..))
import Ouroboros.Consensus.Leios.Protocol
  ( LeiosAnnouncement (..)
  , LeiosLedgerView (..)
  , LeiosProtocol (..)
  , leiosEbBounds
  )
import Ouroboros.Consensus.Leios.Types
  ( ElectionId (..)
  , LeiosPoint (..)
  , minCertificationSlot
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Wait as W
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies (LeiosFetchClientContext (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
  ( LeiosEbBounds (..)
  , LeiosFetchRequest
  , LeiosOutstanding (..)
  , OfferedHalves (..)
  , PeerOffer (..)
  , focusElectionIfUnfocused
  , initializeLeiosOutstanding
  , leiosFetchLogicIteration
  , pruneOffers
  , pruneOutstandingToImmTip
  , recordReferencedPoint
  , removePeerFromOutstanding
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity (LeiosPeerLifetimeId (..))
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( AnnouncingHeader (..)
  , ValidAnnouncement (..)
  )
import Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbHandle
  , LeiosDbReader (..)
  , withReader
  )
import Ouroboros.Consensus.Util.IOLike
  ( DiffTime
  , IOLike
  , STM
  , StrictMVar
  , StrictTVar
  , Time
  , atomically
  , bracket
  , getMonotonicTime
  , modifyMVar
  , modifyMVar_
  , modifyTVar
  , readTVar
  , retry
  , uncheckedNewMVar
  , uncheckedNewTVarM
  , writeTVar
  )
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (IsBigLedgerPeer)
import System.Random (StdGen)

-- | The node's LeiosFetch state, which all its LeiosFetch clients share.
data LeiosFetchKernel m peer blk = MkLeiosFetchKernel
  { lfkContext :: !(LeiosFetchClientContext m (LeiosPeerLifetimeId peer))
  , lfkOnAcceptedAnnouncement :: !(ValidAnnouncement blk -> m ())
  -- ^ Memory-only observation after focus acceptance, outside the outstanding lock.
  , lfkPeers :: !(StrictTVar m (Map.Map (LeiosPeerLifetimeId peer) (LeiosFetchPeer m blk)))
  -- ^ The upstream peers the decision loop assigns requests to, each
  -- registered at most once at a time; see 'withLeiosFetchPeer'.
  , lfkNextLifetimeId :: !(StrictTVar m Natural)
  -- ^ What tells the next registration's 'LeiosPeerLifetimeId' apart from
  -- every earlier one.
  , lfkExtension :: !(StrictTVar m (Maybe (LeiosFetchExtension m peer blk)))
  -- ^ Explicit opt-in integration. Ordinary kernels leave this absent.
  , lfkPrunedSlot :: !(StrictTVar m SlotNo)
  -- ^ The slot 'leiosFetchPruner' has most recently pruned the peers' offers
  -- to. Nothing older is recorded afterwards, so that an offer that arrives
  -- after the pruning cannot undo it.
  }

-- | Node-scoped integration around the shared decision loop. Planning, pruning
-- and peer removal run under its outstanding-state lock. Deadline reads run
-- outside that lock; STM callbacks must never acquire it.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#agreed-coordinator-wakeups@.
data LeiosFetchExtension m peer blk = LeiosFetchExtension
  { extensionDecide ::
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
  -- ^ Project shared progress, schedule new-version work, and return only the
  -- offers eligible for the unchanged legacy scheduler.
  , extensionNextDeadline :: Time -> STM m (Maybe Time)
  -- ^ Read-only earliest active policy deadline or cooldown. The kernel owns
  -- the wait and planning thread; this callback must not acquire its lock.
  , extensionPrune :: SlotNo -> STM m ()
  -- ^ Retire acquisition generations before later receipts can observe pruning.
  , extensionRemovePeer :: LeiosPeerLifetimeId peer -> STM m ()
  -- ^ Withdraw exact-registration ownership when either protocol unregisters.
  , extensionLegacyContext :: LeiosFetchClientContext m (LeiosPeerLifetimeId peer)
  -- ^ Legacy validation/accounting with a durable-receipt storage bridge.
  }

-- | First arrival of each independently offered half. Repeating a body offer
-- never gives its later closure offer an earlier arrival time.
data OfferTimes = OfferTimes
  { bodyOfferTime :: !(Maybe Time)
  -- ^ First body offer in this peer registration.
  , closureOfferTime :: !(Maybe Time)
  -- ^ First full-closure offer in this peer registration.
  }

-- | Capture the optional legacy receipt bridge before taking a command.
legacyClientContext ::
  IOLike m => LeiosFetchKernel m peer blk -> m (LeiosFetchClientContext m (LeiosPeerLifetimeId peer))
legacyClientContext kernel = atomically $ maybe (lfkContext kernel) extensionLegacyContext <$> readTVar (lfkExtension kernel)

-- | What the node's LeiosFetch state holds about one upstream peer.
data LeiosFetchPeer m blk = MkLeiosFetchPeer
  { lfpIsBigLedgerPeer :: !IsBigLedgerPeer
  -- ^ Fixed for the connection's lifetime. The decision logic fetches more
  -- aggressively from a big-ledger peer.
  , lfpOfferings :: !(StrictTVar m (Map.Map LeiosPoint PeerOffer))
  -- ^ The peer's current offers, keyed by point. Written only by the
  -- LeiosNotify client, as the peer offers, and by the decision loop and the
  -- pruner, as offers go dead.
  , lfpOfferTimes :: !(StrictTVar m (Map.Map LeiosPoint OfferTimes))
  -- ^ Arrival order for each half, pruned alongside the corresponding offers.
  , lfpRequestsToSend :: !(StrictTVar m (Seq LeiosFetchRequest))
  -- ^ The requests the decision loop assigned to this peer that its LeiosFetch
  -- client has not sent yet.
  }

-- | The node's LeiosFetch state, seeded with the complete closures its LeiosDB
-- holds whose announcements the immutable tip has not passed, so that it
-- neither fetches nor announces those again.
newLeiosFetchKernel ::
  (IOLike m, LedgerSupportsProtocol blk) =>
  ChainDB m blk ->
  LeiosDbHandle m ->
  -- | Seeds the decision loop's PRNG.
  StdGen ->
  -- | Everything of the clients' context but the state this creates.
  ( StrictMVar m (LeiosOutstanding (LeiosPeerLifetimeId peer)) ->
    W.Readiness m ->
    LeiosFetchClientContext m (LeiosPeerLifetimeId peer)
  ) ->
  m (LeiosFetchKernel m peer blk)
newLeiosFetchKernel = newLeiosFetchKernelWithAnnouncements (const (pure ()))

newLeiosFetchKernelWithAnnouncements ::
  (IOLike m, LedgerSupportsProtocol blk) =>
  (ValidAnnouncement blk -> m ()) ->
  ChainDB m blk ->
  LeiosDbHandle m ->
  -- | Seeds the decision loop's PRNG.
  StdGen ->
  -- | Everything of the clients' context but the state this creates.
  ( StrictMVar m (LeiosOutstanding (LeiosPeerLifetimeId peer)) ->
    W.Readiness m ->
    LeiosFetchClientContext m (LeiosPeerLifetimeId peer)
  ) ->
  m (LeiosFetchKernel m peer blk)
newLeiosFetchKernelWithAnnouncements onAnnouncement chainDB leiosDb prng mkContext = do
  immTipSlot <-
    atomically $
      withOrigin (SlotNo 0) id . getTipSlot . ledgerState <$> ChainDB.getImmutableLedger chainDB
  acquired <- withReader leiosDb $ \reader ->
    scanCompleteEbClosuresNotOlderThanSlot reader immTipSlot
  outstanding <- uncheckedNewMVar $ initializeLeiosOutstanding prng acquired immTipSlot
  ready <- atomically W.newReadiness
  lfkPeers <- uncheckedNewTVarM Map.empty
  lfkNextLifetimeId <- uncheckedNewTVarM 0
  lfkPrunedSlot <- uncheckedNewTVarM immTipSlot
  lfkExtension <- uncheckedNewTVarM Nothing
  pure
    MkLeiosFetchKernel
      { lfkOnAcceptedAnnouncement = onAnnouncement
      , lfkContext = mkContext outstanding ready
      , lfkPeers
      , lfkNextLifetimeId
      , lfkPrunedSlot
      , lfkExtension
      }

-- | Run the action with this peer's LeiosFetch state and the
-- 'LeiosPeerLifetimeId' of its registration, registering it with the decision
-- loop if no other mini-protocol of the peer has.
--
-- There is no reference count: the first of the peer's mini-protocols to end
-- unregisters it, and cancels what it had in flight, so that other peers are
-- asked instead. Any of its mini-protocols still running keep their state, but
-- the decision loop no longer assigns them anything.
--
-- A connection's mini-protocols find its registration by @peer@, so
-- connections whose mini-protocols run at the same time must have different
-- @peer@s. Their @ConnectionId@s differ, by the exclusivity of @ConnectionId@s
-- (see "Ouroboros.Network.ConnectionId#exclusivity").
withLeiosFetchPeer ::
  (IOLike m, Ord peer) =>
  LeiosFetchKernel m peer blk ->
  peer ->
  IsBigLedgerPeer ->
  (LeiosPeerLifetimeId peer -> LeiosFetchPeer m blk -> m a) ->
  m a
withLeiosFetchPeer kernel peer isBig k = bracket register unregister (uncurry k)
 where
  MkLeiosFetchKernel{lfkContext, lfkPeers, lfkNextLifetimeId} = kernel

  register = do
    fresh <-
      MkLeiosFetchPeer isBig
        <$> uncheckedNewTVarM Map.empty
        <*> uncheckedNewTVarM Map.empty
        <*> uncheckedNewTVarM Seq.empty
    atomically $ do
      peers <- readTVar lfkPeers
      -- 'LeiosPeerLifetimeId's order by their peer first, so this finds the
      -- peer's registration if it has one.
      case Map.lookupGE (MkLeiosPeerLifetimeId peer 0) peers of
        Just (lifetimeId@(MkLeiosPeerLifetimeId peer' _), existing)
          | peer' == peer -> pure (lifetimeId, existing)
        _ -> do
          n <- readTVar lfkNextLifetimeId
          writeTVar lfkNextLifetimeId (n + 1)
          let lifetimeId = MkLeiosPeerLifetimeId peer n
          writeTVar lfkPeers (Map.insert lifetimeId fresh peers)
          pure (lifetimeId, fresh)

  -- The decision loop reads the registered peers within the same critical
  -- section in which it assigns requests, so it cannot assign this peer
  -- anything after its requests are cancelled here. Removal and atomic
  -- admission/publication therefore share this lock, using the captured peer
  -- lifetime rather than an address lookup which could select a replacement.
  -- Keep IO and storage waits outside this critical section.
  unregister (lifetimeId, _) = modifyMVar_ (lfcOutstanding lfkContext) $ \outstanding -> do
    atomically $ do
      modifyTVar lfkPeers (Map.delete lifetimeId)
      extension <- readTVar (lfkExtension kernel)
      forM_ extension $ \e -> extensionRemovePeer e lifetimeId
      W.notifyReadiness (lfcReady lfkContext)
    pure $! removePeerFromOutstanding lifetimeId outstanding

-- | Record a peer's offer, which an earlier offer of the same point merges
-- with, unless the point is older than what has been pruned, and wake the
-- decision loop.
recordLeiosOffer ::
  IOLike m =>
  LeiosFetchKernel m peer blk ->
  LeiosFetchPeer m blk ->
  LeiosPoint ->
  PeerOffer ->
  m ()
recordLeiosOffer kernel MkLeiosFetchPeer{lfpOfferings, lfpOfferTimes} point offer = do
  now <- getMonotonicTime
  atomically $ do
    prunedSlot <- readTVar (lfkPrunedSlot kernel)
    unless (pointSlotNo point < prunedSlot) $ do
      modifyTVar lfpOfferings (Map.insertWith (<>) point offer)
      let fresh = case poOffered offer of
            OfferedBody{} -> OfferTimes (Just now) Nothing
            OfferedClosure -> OfferTimes Nothing (Just now)
            OfferedBodyAndClosure{} -> OfferTimes (Just now) (Just now)
          preserve new old =
            OfferTimes
              (first (bodyOfferTime old) (bodyOfferTime new))
              (first (closureOfferTime old) (closureOfferTime new))
          first (Just old) _ = Just old
          first Nothing new = new
      modifyTVar lfpOfferTimes (Map.insertWith preserve point fresh)
    W.notifyReadiness (lfcReady (lfkContext kernel))

-- | List the endorser block a new announcement names as one to fetch, if it
-- takes its election's focus.
--
-- One entry per election: the announcement that takes the election's focus. A
-- second announcement for an already-focused election is an equivocation, and
-- tracking its endorser block too would let a pool double what its won slots
-- cost us.
onNewLeiosAnnouncement ::
  IOLike m =>
  LeiosFetchKernel m peer blk ->
  ValidAnnouncement blk ->
  m ()
onNewLeiosAnnouncement kernel valid = do
  changed <- modifyMVar lfcOutstanding $ \outstanding ->
    let tooOld = slot < outstandingPrunedSlot outstanding
        introducesFocus = not $ Map.member elId (elFocus outstanding)
        outstanding' =
          focusElectionIfUnfocused elId ebHash $
            recordReferencedPoint point (SJust announcementOnset) outstanding
     in pure $
          if tooOld || not introducesFocus
            then (outstanding, False)
            else (outstanding', True)
  when changed $ do
    lfkOnAcceptedAnnouncement kernel valid
    atomically $ W.notifyReadiness lfcReady
 where
  MkLeiosFetchClientContext{lfcOutstanding, lfcReady} = lfkContext kernel
  MkValidAnnouncement{announcingHeader = anc, announcementOnset} = valid
  announcement = ancAnnouncement anc
  elId = ancElectionId anc
  slot = electionSlot elId
  ebHash = announcementEbHash announcement
  point = MkLeiosPoint slot ebHash

-- | One fetch coordinator. New-policy readiness/deadlines wake promptly;
-- legacy scheduling retains its own half-second gate and pending notification.
-- Every pass uses current state under the outstanding lock and publishes its
-- queues before a registration can be removed.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#agreed-coordinator-wakeups@.
leiosFetchDecisionLoop ::
  forall m peer blk.
  (IOLike m, MonadTimer m, Ord peer, LedgerSupportsProtocol blk, HasHardForkHistory blk) =>
  TopLevelConfig blk ->
  ChainDB m blk ->
  BlockchainTime m ->
  LeiosFetchKernel m peer blk ->
  m Void
leiosFetchDecisionLoop cfg chainDB btime kernel = go Nothing False
 where
  MkLeiosFetchKernel{lfkContext, lfkPeers, lfkExtension} = kernel
  MkLeiosFetchClientContext{lfcOutstanding, lfcReady} = lfkContext
  loopInterval :: DiffTime
  loopInterval = 0.5
  due now next = maybe True (now >=) next
  earliest [] = Nothing
  earliest deadlines = Just (minimum deadlines)
  go nextLegacy pending = do
    waitingAt <- getMonotonicTime
    extension <- atomically $ readTVar lfkExtension
    protocolDeadline <- case extension of
      Nothing -> pure Nothing
      Just e -> atomically (extensionNextDeadline e waitingAt)
    let deadline = earliest ([at | Just at <- [protocolDeadline]] ++ [at | pending, Just at <- [nextLegacy]])
    wakeup <- case extension of
      Nothing | not pending -> do
        atomically (W.awaitReadiness lfcReady)
        pure (W.Wakeup W.ReadinessWake True)
      _ -> W.waitForWakeup lfcReady deadline
    let pending' = pending || W.readinessConsumed wakeup
    now <- getMonotonicTime
    current <- atomically $ readTVar lfkExtension
    -- A legacy-only early notification stays remembered until its rate gate.
    -- Recheck the mode after the wait so installation can wake this old waiter.
    case current of
      Nothing | not (pending' && due now nextLegacy) -> go nextLegacy pending'
      _ -> do
        (next, stillPending) <- iteration nextLegacy pending'
        go next stillPending
  iteration nextLegacy pending = modifyMVar lfcOutstanding $ \outstanding -> do
    iterationStart <- getMonotonicTime
    -- Read the live peers while holding the outstanding state, so that this
    -- assigns nothing to a peer whose unregistering already cancelled its
    -- requests: their replies would never arrive, and the requests would stay
    -- outstanding forever.
    (peersVars, offerings, currentSlot, immLedger) <- atomically $ do
      peersVars <- readTVar lfkPeers
      offerings <- traverse (readTVar . lfpOfferings) peersVars
      (,,,) peersVars offerings <$> getCurrentSlot btime <*> ChainDB.getImmutableLedger chainDB
    let
      -- One forecast anchor for the whole pass: the immutable tip, which is
      -- also what the announcement validation a LeiosNotify offer rides on
      -- forecast from, so it reaches every slot such an offer names.
      forecastLeiosView slot =
        getLeiosLedgerView (Proxy @(BlockProtocol blk))
          <$> runExcept
            (forecastFor (ledgerViewForecastAt (configLedger cfg) (ledgerState immLedger)) slot)
      forecastEbBounds slot = maybe (MkLeiosEbBounds 0 0) leiosEbBounds <$> forecastLeiosView slot
      -- The current slot, and how many slots before it a point is still worth
      -- voting on, by the Leios periods of the ledger there, if the ledger
      -- reaches that far.
      mbHorizon = case currentSlot of
        CurrentSlotUnknown -> Nothing
        CurrentSlot slot -> do
          llv <- either (const Nothing) id (forecastLeiosView slot)
          (_onset, slotLength) <-
            either (const Nothing) Just $
              HardFork.runQuery
                (HardFork.slotToWallclock slot)
                (hardForkSummary (configLedger cfg) (ledgerState immLedger))
          let MkLeiosLedgerView{llvAnnouncementPeriod, llvVotePeriod, llvDiffusionPeriod} = llv
              SlotNo window =
                minCertificationSlot slotLength llvAnnouncementPeriod llvVotePeriod llvDiffusionPeriod 0
          pure (slot, window)
    extension <- atomically $ readTVar lfkExtension
    (shared, legacyOffers) <- case extension of
      Nothing -> pure (outstanding, offerings)
      Just e ->
        extensionDecide
          e
          iterationStart
          (either (const Nothing) Just . forecastEbBounds)
          mbHorizon
          peersVars
          offerings
          outstanding
    let considered = pending && due iterationStart nextLegacy
        hasLegacyOffers = any (not . Map.null) (Map.elems legacyOffers)
        runLegacy = considered && maybe True (const hasLegacyOffers) extension
        (!outstanding', requests, offerDrops) =
          if runLegacy
            then
              leiosFetchLogicIteration
                (lfcStaticEnv lfkContext)
                forecastEbBounds
                mbHorizon
                legacyOffers
                (Map.map lfpIsBigLedgerPeer peersVars)
                shared
            else (shared, Map.empty, Map.empty)
        next = if runLegacy then Just (addTime loopInterval iterationStart) else nextLegacy
    atomically $ do
      live <- readTVar lfkPeers
      forM_ (Map.toList offerDrops) $ \(peer, pruned) ->
        forM_ (Map.lookup peer live) $ \vars -> do
          modifyTVar (lfpOfferings vars) (pruneOffers (NEMap.toMap pruned))
          offers <- readTVar (lfpOfferings vars)
          modifyTVar (lfpOfferTimes vars) (`Map.intersection` offers)
      -- Publication shares the removal lock. A late finalizer cannot make
      -- an already removed registration receive a newly assigned command.
      forM_ (Map.toList requests) $ \(peer, reqs) ->
        forM_ (Map.lookup peer live) $ \vars ->
          modifyTVar (lfpRequestsToSend vars) (<> NESeq.toSeq reqs)
    pure (outstanding', (next, pending && not considered))

-- | Prune the node's LeiosFetch state and the peers' offers each time the
-- immutable tip's slot advances.
leiosFetchPruner ::
  forall m peer blk.
  (IOLike m, LedgerSupportsProtocol blk) =>
  ChainDB m blk ->
  LeiosFetchKernel m peer blk ->
  m Void
leiosFetchPruner chainDB kernel = go (SlotNo 0)
 where
  MkLeiosFetchKernel{lfkContext, lfkPeers, lfkPrunedSlot} = kernel
  MkLeiosFetchClientContext{lfcOutstanding} = lfkContext

  go prunedSlot = do
    immTipSlot <- atomically $ do
      immLedger <- ChainDB.getImmutableLedger chainDB
      case getTipSlot (ledgerState immLedger) of
        NotOrigin slot | prunedSlot < slot -> pure slot
        _ -> retry
    modifyMVar_ lfcOutstanding $ \outstanding -> do
      atomically $ do
        extension <- readTVar (lfkExtension kernel)
        forM_ extension $ \e -> extensionPrune e immTipSlot
      pure $ snd $ pruneOutstandingToImmTip immTipSlot outstanding
    -- Offers are keyed by point, which orders by slot first.
    atomically $ do
      writeTVar lfkPrunedSlot immTipSlot
      peersVars <- readTVar lfkPeers
      forM_ peersVars $ \vars -> do
        modifyTVar (lfpOfferings vars) (Map.dropWhileAntitone ((< immTipSlot) . pointSlotNo))
        modifyTVar (lfpOfferTimes vars) (Map.dropWhileAntitone ((< immTipSlot) . pointSlotNo))
      W.notifyReadiness (lfcReady lfkContext)
    go immTipSlot
