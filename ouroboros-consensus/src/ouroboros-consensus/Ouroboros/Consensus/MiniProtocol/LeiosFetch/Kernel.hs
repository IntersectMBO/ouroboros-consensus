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
  , newLeiosFetchKernel
  , withLeiosFetchPeer
  , recordLeiosOffer
  , onNewLeiosAnnouncement
  , leiosFetchDecisionLoop
  , leiosFetchPruner
  ) where

import Control.Monad (forM_, forever, void, when)
import Control.Monad.Except (runExcept)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Sequence.NonEmpty as NESeq
import qualified Data.Set.NonEmpty as NESet
import Data.Void (Void)
import Ouroboros.Consensus.Block
  ( BlockProtocol
  , ConvertRawHash
  , HasHeader
  , Header
  , Proxy (..)
  , SlotNo (..)
  , WithOrigin (..)
  , headerHash
  , withOrigin
  )
import Ouroboros.Consensus.BlockchainTime (BlockchainTime (..), CurrentSlot (..))
import Ouroboros.Consensus.Config (TopLevelConfig, configLedger)
import Ouroboros.Consensus.Forecast (forecastFor)
import Ouroboros.Consensus.Ledger.Abstract (getTipSlot)
import Ouroboros.Consensus.Ledger.Extended (ledgerState)
import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol (..))
import Ouroboros.Consensus.Leios.Protocol
  ( LeiosAnnouncement (..)
  , LeiosLedgerView (..)
  , LeiosProtocol (..)
  )
import Ouroboros.Consensus.Leios.TxCache.API (LeiosTxCache (..))
import Ouroboros.Consensus.Leios.Types
  ( ElectionId (..)
  , LeiosPoint (..)
  , toRankingBlockHash
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies (LeiosFetchClientContext (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
  ( LeiosFetchRequest
  , LeiosOutstanding (..)
  , PeerOffer
  , ebStateHasBody
  , focusElectionIfUnfocused
  , initializeLeiosOutstanding
  , leiosFetchLogicIteration
  , pruneOutstandingToImmTip
  , recordMaxAnnouncementSlot
  , removePeerFromOutstanding
  )
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( AnnouncingHeader (..)
  , ValidAnnouncement (..)
  )
import Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbHandle
  , LeiosDbReader (..)
  , LeiosDbWriter (..)
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
  , readTVarIO
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
data LeiosFetchKernel m peer = MkLeiosFetchKernel
  { lfkContext :: !(LeiosFetchClientContext m peer)
  , lfkPeers :: !(StrictTVar m (Map.Map peer (LeiosFetchPeer m)))
  -- ^ The upstream peers the decision loop assigns requests to; see
  -- 'withLeiosFetchPeer'.
  }

-- | What the node's LeiosFetch state holds about one upstream peer.
data LeiosFetchPeer m = MkLeiosFetchPeer
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
  (StrictMVar m (LeiosOutstanding peer) -> StrictMVar m () -> LeiosFetchClientContext m peer) ->
  m (LeiosFetchKernel m peer)
newLeiosFetchKernel chainDB leiosDb prng mkContext = do
  immTipSlot <-
    atomically $
      withOrigin (SlotNo 0) id . getTipSlot . ledgerState <$> ChainDB.getImmutableLedger chainDB
  acquired <- withReader leiosDb $ \reader -> scanCompleteEbClosuresNotOlderThanSlot reader immTipSlot
  outstanding <- uncheckedNewMVar $ initializeLeiosOutstanding prng acquired immTipSlot
  ready <- uncheckedNewEmptyMVar
  lfkPeers <- uncheckedNewTVarM Map.empty
  pure MkLeiosFetchKernel{lfkContext = mkContext outstanding ready, lfkPeers}

-- | Run the action with this peer's LeiosFetch state, registering it with the
-- decision loop if no other mini-protocol of the peer has.
--
-- There is no reference count: the first of the peer's mini-protocols to end
-- unregisters it, and cancels what it had in flight, so that other peers are
-- asked instead. Any of its mini-protocols still running keep their state, but
-- the decision loop no longer assigns them anything.
withLeiosFetchPeer ::
  (IOLike m, Ord peer) =>
  LeiosFetchKernel m peer ->
  peer ->
  IsBigLedgerPeer ->
  (LeiosFetchPeer m -> m a) ->
  m a
withLeiosFetchPeer kernel peer isBig = bracket register (const unregister)
 where
  MkLeiosFetchKernel{lfkContext, lfkPeers} = kernel

  register = do
    fresh <-
      MkLeiosFetchPeer isBig
        <$> uncheckedNewTVarM Map.empty
        <*> uncheckedNewTVarM Seq.empty
    atomically $ do
      peers <- readTVar lfkPeers
      case Map.lookup peer peers of
        Just existing -> pure existing
        Nothing -> fresh <$ writeTVar lfkPeers (Map.insert peer fresh peers)

  -- The decision loop reads the registered peers within the same critical
  -- section in which it assigns requests, so it cannot assign this peer
  -- anything after its requests are cancelled here.
  unregister = modifyMVar_ (lfcOutstanding lfkContext) $ \outstanding -> do
    atomically $ modifyTVar lfkPeers (Map.delete peer)
    pure $! removePeerFromOutstanding peer outstanding

-- | Record a peer's offer, which an earlier offer of the same point merges
-- with, and wake the decision loop.
recordLeiosOffer ::
  IOLike m =>
  LeiosFetchKernel m peer ->
  LeiosFetchPeer m ->
  LeiosPoint ->
  PeerOffer ->
  m ()
recordLeiosOffer kernel MkLeiosFetchPeer{lfpOfferings} point offer = do
  atomically $ modifyTVar lfpOfferings (Map.insertWith (<>) point offer)
  void $ tryPutMVar (lfcReady (lfkContext kernel)) ()

-- | List the endorser block a new announcement names as one to fetch, if it
-- takes its election's focus, and register the announcement with the
-- LeiosTxCache. Also write its point, so that the LeiosDB keeps the endorser
-- block for at least as long as this announcement matters.
--
-- One entry per election: the announcement that takes the election's focus. A
-- second announcement for an already-focused election is an equivocation, and
-- tracking its endorser block too would let a pool double what its won slots
-- cost us. If that one turns out to be the certified one, a verified
-- certificate gives it an entry then.
--
-- TODO If this announcement is younger than every earlier one of an endorser
-- block the node already holds, announce the acquisition again at its point,
-- so that the ChainDB keeps the closure acquired for as long as this
-- announcement matters.
onNewLeiosAnnouncement ::
  forall m peer blk.
  (IOLike m, ConvertRawHash blk, HasHeader (Header blk)) =>
  LeiosFetchKernel m peer ->
  ValidAnnouncement blk ->
  m ()
onNewLeiosAnnouncement kernel valid = do
  void $
    insertAnnouncement
      lfcTxCache
      slot
      (toRankingBlockHash (Proxy @blk) (headerHash (ancHeader anc)))
      ebHash
  void $ writeEbPoint lfcWriter (MkLeiosPoint slot ebHash) (announcementEbSize announcement)
  changed <- modifyMVar lfcOutstanding $ \outstanding ->
    let tooOld = slot < outstandingPrunedSlot outstanding
        introducesFocus = not $ Map.member elId (elFocus outstanding)
        held = maybe False ebStateHasBody (Map.lookup ebHash (ebState outstanding))
     in pure $
          if tooOld || not introducesFocus
            then (outstanding, False)
            else
              ( focusElectionIfUnfocused elId ebHash $
                  recordMaxAnnouncementSlot ebHash slot (SJust announcementOnset) outstanding
              , not held
              )
  when changed $ void $ tryPutMVar lfcReady ()
 where
  MkLeiosFetchClientContext{lfcOutstanding, lfcReady, lfcTxCache, lfcWriter} = lfkContext kernel
  MkValidAnnouncement{announcingHeader = anc, announcementOnset} = valid
  announcement = ancAnnouncement anc
  elId = ancElectionId anc
  slot = electionSlot elId
  ebHash = announcementEbHash announcement

-- | Each time something might give a peer something new to do, decide what to
-- request from every peer, at most twice a second.
leiosFetchDecisionLoop ::
  forall m peer blk.
  (IOLike m, Ord peer, LedgerSupportsProtocol blk) =>
  TopLevelConfig blk ->
  ChainDB m blk ->
  BlockchainTime m ->
  LeiosFetchKernel m peer ->
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
    let mbCurrentSlot = case currentSlot of
          CurrentSlot s -> Just s
          CurrentSlotUnknown -> Nothing
        -- One forecast anchor for the whole pass: the immutable tip, which is
        -- also what the announcement validation a LeiosNotify offer rides on
        -- forecast from, so it reaches every slot such an offer names.
        forecastMaxEbTxsSize slot =
          maybe 0 llvMaxEbTxsSize . getLeiosLedgerView (Proxy @(BlockProtocol blk))
            <$> runExcept
              (forecastFor (ledgerViewForecastAt (configLedger cfg) (ledgerState immLedger)) slot)
        (!outstanding', requests, offerDrops) =
          leiosFetchLogicIteration
            (lfcStaticEnv lfkContext)
            forecastMaxEbTxsSize
            mbCurrentSlot
            offerings
            (Map.map lfpIsBigLedgerPeer peersVars)
            outstanding
    pure (outstanding', (requests, offerDrops, peersVars))
  atomically $ do
    -- Drop dead offers: those the decision found can no longer help.
    forM_ (Map.toList offerDrops) $ \(peer, dropped) ->
      forM_ (Map.lookup peer peersVars) $ \vars ->
        modifyTVar (lfpOfferings vars) (`Map.withoutKeys` NESet.toSet dropped)
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

-- | Prune the node's LeiosFetch state, the peers' offers, and the LeiosTxCache
-- each time the immutable tip's slot advances.
--
-- The LeiosTxCache must be pruned to a slot strictly before the LeiosDB is;
-- see 'evictOlderThan'. The ChainDB prunes the LeiosDB only to the slot of the
-- blocks it has copied to the ImmutableDB, after a delay, so pruning the cache
-- as soon as the immutable tip advances comes first.
--
-- TODO Make that ordering structural rather than a matter of timing.
leiosFetchPruner ::
  forall m peer blk.
  (IOLike m, LedgerSupportsProtocol blk) =>
  ChainDB m blk ->
  LeiosFetchKernel m peer ->
  m Void
leiosFetchPruner chainDB kernel = go (SlotNo 0)
 where
  MkLeiosFetchKernel{lfkContext, lfkPeers} = kernel
  MkLeiosFetchClientContext{lfcOutstanding, lfcTxCache} = lfkContext

  go prunedSlot = do
    immTipSlot <- atomically $ do
      immLedger <- ChainDB.getImmutableLedger chainDB
      case getTipSlot (ledgerState immLedger) of
        NotOrigin slot | prunedSlot < slot -> pure slot
        _ -> retry
    _ <- evictOlderThan lfcTxCache immTipSlot
    modifyMVar_ lfcOutstanding $ pure . snd . pruneOutstandingToImmTip immTipSlot
    -- Offers are keyed by point, which orders by slot first.
    peersVars <- readTVarIO lfkPeers
    atomically $ forM_ peersVars $ \vars ->
      modifyTVar (lfpOfferings vars) (Map.dropWhileAntitone ((< immTipSlot) . pointSlotNo))
    go immTipSlot
