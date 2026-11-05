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
  , LeiosPeerLifetimeId (..)
  , newLeiosFetchKernel
  , withLeiosFetchPeer
  , recordLeiosOffer
  , onNewLeiosAnnouncement
  , leiosFetchDecisionLoop
  , leiosFetchPruner
  ) where

import Control.Monad (forM_, forever, unless, void, when)
import Control.Monad.Except (runExcept)
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Sequence.NonEmpty as NESeq
import Data.Void (Void)
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
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies (LeiosFetchClientContext (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
  ( LeiosEbBounds (..)
  , LeiosFetchRequest
  , LeiosOutstanding (..)
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
  , StrictMVar
  , StrictTVar
  , atomically
  , bracket
  , diffTime
  , getMonotonicTime
  , modifyMVar
  , modifyMVar_
  , modifyTVar
  , readTVar
  , retry
  , takeMVar
  , threadDelay
  , tryPutMVar
  , uncheckedNewEmptyMVar
  , uncheckedNewMVar
  , uncheckedNewTVarM
  , writeTVar
  )
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (IsBigLedgerPeer)
import System.Random (StdGen)

-- | The node's LeiosFetch state, which all its LeiosFetch clients share.
data LeiosFetchKernel m peer blk = MkLeiosFetchKernel
  { lfkContext :: !(LeiosFetchClientContext m (LeiosPeerLifetimeId peer))
  , lfkPeers :: !(StrictTVar m (Map.Map (LeiosPeerLifetimeId peer) (LeiosFetchPeer m blk)))
  -- ^ The upstream peers the decision loop assigns requests to, each
  -- registered at most once at a time; see 'withLeiosFetchPeer'.
  , lfkNextLifetimeId :: !(StrictTVar m Natural)
  -- ^ What tells the next registration's 'LeiosPeerLifetimeId' apart from
  -- every earlier one.
  , lfkPrunedSlot :: !(StrictTVar m SlotNo)
  -- ^ The slot 'leiosFetchPruner' has most recently pruned the peers' offers
  -- to. Nothing older is recorded afterwards, so that an offer that arrives
  -- after the pruning cannot undo it.
  }

-- | What the node's LeiosFetch state holds about one upstream peer.
data LeiosFetchPeer m blk = MkLeiosFetchPeer
  { lfpIsBigLedgerPeer :: !IsBigLedgerPeer
  -- ^ Fixed for the connection's lifetime. The decision logic fetches more
  -- aggressively from a big-ledger peer.
  , lfpOfferings :: !(StrictTVar m (Map.Map LeiosPoint PeerOffer))
  -- ^ The peer's current offers, keyed by point. Written only by the
  -- LeiosNotify client, as the peer offers, and by the decision loop and the
  -- pruner, as offers go dead.
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
    StrictMVar m () ->
    LeiosFetchClientContext m (LeiosPeerLifetimeId peer)
  ) ->
  m (LeiosFetchKernel m peer blk)
newLeiosFetchKernel chainDB leiosDb prng mkContext = do
  immTipSlot <-
    atomically $
      withOrigin (SlotNo 0) id . getTipSlot . ledgerState <$> ChainDB.getImmutableLedger chainDB
  acquired <- withReader leiosDb $ \reader ->
    scanCompleteEbClosuresNotOlderThanSlot reader immTipSlot
  outstanding <- uncheckedNewMVar $ initializeLeiosOutstanding prng acquired immTipSlot
  ready <- uncheckedNewEmptyMVar
  lfkPeers <- uncheckedNewTVarM Map.empty
  lfkNextLifetimeId <- uncheckedNewTVarM 0
  lfkPrunedSlot <- uncheckedNewTVarM immTipSlot
  pure
    MkLeiosFetchKernel
      { lfkContext = mkContext outstanding ready
      , lfkPeers
      , lfkNextLifetimeId
      , lfkPrunedSlot
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
  -- anything after its requests are cancelled here.
  unregister (lifetimeId, _) = modifyMVar_ (lfcOutstanding lfkContext) $ \outstanding -> do
    atomically $ modifyTVar lfkPeers (Map.delete lifetimeId)
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
recordLeiosOffer kernel MkLeiosFetchPeer{lfpOfferings} point offer = do
  atomically $ do
    prunedSlot <- readTVar (lfkPrunedSlot kernel)
    unless (pointSlotNo point < prunedSlot) $
      modifyTVar lfpOfferings (Map.insertWith (<>) point offer)
  void $ tryPutMVar (lfcReady (lfkContext kernel)) ()

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
  when changed $
    void $
      tryPutMVar lfcReady ()
 where
  MkLeiosFetchClientContext{lfcOutstanding, lfcReady} = lfkContext kernel
  MkValidAnnouncement{announcingHeader = anc, announcementOnset} = valid
  announcement = ancAnnouncement anc
  elId = ancElectionId anc
  slot = electionSlot elId
  ebHash = announcementEbHash announcement
  point = MkLeiosPoint slot ebHash

-- | Each time something might give a peer something new to do, decide what to
-- request from every peer, at most twice a second.
leiosFetchDecisionLoop ::
  forall m peer blk.
  (IOLike m, Ord peer, LedgerSupportsProtocol blk, HasHardForkHistory blk) =>
  TopLevelConfig blk ->
  ChainDB m blk ->
  BlockchainTime m ->
  LeiosFetchKernel m peer blk ->
  m Void
leiosFetchDecisionLoop cfg chainDB btime kernel = forever $ do
  () <- takeMVar lfcReady
  iterationStart <- getMonotonicTime
  (requests, offerDrops, peersVars) <- modifyMVar lfcOutstanding $ \outstanding -> do
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
      (!outstanding', requests, offerDrops) =
        leiosFetchLogicIteration
          (lfcStaticEnv lfkContext)
          forecastEbBounds
          mbHorizon
          offerings
          (Map.map lfpIsBigLedgerPeer peersVars)
          outstanding
    pure (outstanding', (requests, offerDrops, peersVars))
  atomically $ do
    -- Prune what of the offers the decision found can no longer help.
    forM_ (Map.toList offerDrops) $ \(peer, pruned) ->
      forM_ (Map.lookup peer peersVars) $ \vars ->
        modifyTVar (lfpOfferings vars) (pruneOffers (NEMap.toMap pruned))
    -- Published after the lock is released, so a peer that unregistered
    -- meanwhile still gets these, although unregistering cancelled their
    -- accounting. That is harmless: a peer unregisters only when one of its
    -- mini-protocols ends, which the diffusion layer answers by demoting or
    -- closing its connection, and its LeiosFetch client takes nothing more from
    -- its queue once told to terminate.
    forM_ (Map.toList requests) $ \(peer, reqs) ->
      forM_ (Map.lookup peer peersVars) $ \vars ->
        modifyTVar (lfpRequestsToSend vars) (<> NESeq.toSeq reqs)
  iterationEnd <- getMonotonicTime
  threadDelay $ loopInterval - (iterationEnd `diffTime` iterationStart)
 where
  MkLeiosFetchKernel{lfkContext, lfkPeers} = kernel
  MkLeiosFetchClientContext{lfcOutstanding, lfcReady} = lfkContext

  loopInterval :: DiffTime
  loopInterval = 0.5

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
    modifyMVar_ lfcOutstanding $ pure . snd . pruneOutstandingToImmTip immTipSlot
    -- Offers are keyed by point, which orders by slot first.
    atomically $ do
      writeTVar lfkPrunedSlot immTipSlot
      peersVars <- readTVar lfkPeers
      forM_ peersVars $ \vars ->
        modifyTVar (lfpOfferings vars) (Map.dropWhileAntitone ((< immTipSlot) . pointSlotNo))
    go immTipSlot
