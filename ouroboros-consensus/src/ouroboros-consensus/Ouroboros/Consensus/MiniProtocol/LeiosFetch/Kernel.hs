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
  , recordLeiosCertOffer
  , recordLeiosJumpOffers
  , ExnLeiosTwoCertificationClaims (..)
  , onNewLeiosAnnouncement
  , leiosFetchDecisionLoop
  , leiosFetchFocuser
  , leiosFetchPruner
  ) where

import Control.Monad (forM, forM_, forever, unless, void, when)
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
  , BlockSupportsProtocol (..)
  , ConvertRawHash
  , HasHeader
  , Header
  , Point (GenesisPoint)
  , Proxy (..)
  , SlotNo (..)
  , WithOrigin (..)
  , blockPoint
  , castPoint
  , headerHash
  , pointSlot
  , withOrigin
  )
import Ouroboros.Consensus.BlockchainTime (BlockchainTime (..), CurrentSlot (..))
import Ouroboros.Consensus.Config (TopLevelConfig, configBlock, configLedger)
import Ouroboros.Consensus.Forecast (forecastFor)
import Ouroboros.Consensus.HeaderStateHistory
  ( HeaderStateHistory (..)
  , HeaderStateWithTime (..)
  )
import Ouroboros.Consensus.HeaderValidation
  ( AnnTip (..)
  , HeaderState (..)
  , HeaderWithTime (..)
  , headerStatePoint
  )
import Ouroboros.Consensus.Ledger.Abstract (getTipSlot)
import Ouroboros.Consensus.Ledger.Extended (ledgerState)
import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol (..))
import Ouroboros.Consensus.Leios.Acquisitions (LeiosEbNotification (..))
import Ouroboros.Consensus.Leios.Protocol
  ( LeiosAnnouncement (..)
  , LeiosLedgerView (..)
  , LeiosProtocol (..)
  )
import Ouroboros.Consensus.Leios.TxCache.API (LeiosTxCache (..))
import Ouroboros.Consensus.Leios.Types
  ( ElectionId (..)
  , EndorserBlockHash
  , LeiosPoint (..)
  , toRankingBlockHash
  )
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State (JumpInfo (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies (LeiosFetchClientContext (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
  ( LeiosEbBounds (..)
  , LeiosFetchRequest
  , LeiosOutstanding (..)
  , PeerOffer (..)
  , WhetherTxsClosureOffered (..)
  , ebStateHasBody
  , focusCertifiedEb
  , focusElectionIfUnfocused
  , heldAcquisitionsAt
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
  , Promise (..)
  , withReader
  )
import Ouroboros.Consensus.Util.IOLike
  ( DiffTime
  , Exception
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
  , throwIO
  , tryPutMVar
  , uncheckedNewEmptyMVar
  , uncheckedNewMVar
  , uncheckedNewTVarM
  , writeTVar
  )
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.AnchoredSeq as AS
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (IsBigLedgerPeer)
import System.Random (StdGen)

-- | The node's LeiosFetch state, which all its LeiosFetch clients share.
data LeiosFetchKernel m peer blk = MkLeiosFetchKernel
  { lfkContext :: !(LeiosFetchClientContext m peer)
  , lfkPeers :: !(StrictTVar m (Map.Map peer (LeiosFetchPeer m blk)))
  -- ^ The upstream peers the decision loop assigns requests to; see
  -- 'withLeiosFetchPeer'.
  , lfkPrunedSlot :: !(StrictTVar m SlotNo)
  -- ^ The slot 'leiosFetchPruner' has most recently pruned the peers' offers
  -- and claims to. Nothing older is recorded afterwards, so that an offer or
  -- claim that arrives after the pruning cannot undo it.
  }

-- | What the node's LeiosFetch state holds about one upstream peer.
data LeiosFetchPeer m blk = MkLeiosFetchPeer
  { lfpIsBigLedgerPeer :: !IsBigLedgerPeer
  -- ^ Fixed for the connection's lifetime. The decision logic fetches more
  -- aggressively from a big-ledger peer.
  , lfpOfferings :: !(StrictTVar m (Map.Map LeiosPoint PeerOffer))
  -- ^ The peer's current offers, keyed by point. Written only by the
  -- LeiosNotify and ChainSync clients, as the peer offers, and by the decision
  -- loop and the pruner, as offers go dead.
  , lfpRequestsToSend :: !(StrictTVar m (Seq LeiosFetchRequest))
  -- ^ The requests the decision loop assigned to this peer that its LeiosFetch
  -- client has not sent yet.
  , lfpCertificationClaims :: !(StrictTVar m (Map.Map ElectionId EndorserBlockHash))
  -- ^ The endorser block the peer's ChainSync roll-forwards claim is certified,
  -- per election; see 'recordLeiosCertOffer'.
  , lfpJumpedThrough :: !(StrictTVar m (Point blk))
  -- ^ The newest header of the latest ChainSync jump the peer accepted; see
  -- 'recordLeiosJumpOffers'.
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
  m (LeiosFetchKernel m peer blk)
newLeiosFetchKernel chainDB leiosDb prng mkContext = do
  immTipSlot <-
    atomically $
      withOrigin (SlotNo 0) id . getTipSlot . ledgerState <$> ChainDB.getImmutableLedger chainDB
  acquired <- withReader leiosDb $ \reader -> scanCompleteEbClosuresNotOlderThanSlot reader immTipSlot
  outstanding <- uncheckedNewMVar $ initializeLeiosOutstanding prng acquired immTipSlot
  ready <- uncheckedNewEmptyMVar
  lfkPeers <- uncheckedNewTVarM Map.empty
  lfkPrunedSlot <- uncheckedNewTVarM immTipSlot
  pure MkLeiosFetchKernel{lfkContext = mkContext outstanding ready, lfkPeers, lfkPrunedSlot}

-- | Run the action with this peer's LeiosFetch state, registering it with the
-- decision loop if no other mini-protocol of the peer has.
--
-- There is no reference count: the first of the peer's mini-protocols to end
-- unregisters it, and cancels what it had in flight, so that other peers are
-- asked instead. Any of its mini-protocols still running keep their state, but
-- the decision loop no longer assigns them anything.
--
-- TODO Nothing tells a later connection with the same key from this one. If
-- one registers before all of this one's mini-protocols have ended, this one's
-- unregistering removes it, and this one's late replies refund its budget.
-- The diffusion layer allows that: its connection manager accepts an inbound
-- connection from a peer whose previous connection is in @TerminatingState@,
-- which it enters before that connection's mini-protocols are stopped, and
-- BlockFetch's @bracketFetchClient@ waits for an older client of the same peer
-- to unregister for that reason. So register each connection under its own
-- key.
withLeiosFetchPeer ::
  (IOLike m, Ord peer) =>
  LeiosFetchKernel m peer blk ->
  peer ->
  IsBigLedgerPeer ->
  (LeiosFetchPeer m blk -> m a) ->
  m a
withLeiosFetchPeer kernel peer isBig = bracket register (const unregister)
 where
  MkLeiosFetchKernel{lfkContext, lfkPeers} = kernel

  register = do
    fresh <-
      MkLeiosFetchPeer isBig
        <$> uncheckedNewTVarM Map.empty
        <*> uncheckedNewTVarM Seq.empty
        <*> uncheckedNewTVarM Map.empty
        <*> uncheckedNewTVarM GenesisPoint
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

-- | Record the offer a peer makes by rolling forward a header that contains a
-- Leios certificate: the endorser block the certificate certifies, both its
-- body, at the size its announcement names, and its closure.
--
-- That endorser block is read from the state as of the header's predecessor,
-- which announced it, so the peer chose which header to roll forward but not
-- what its predecessor announced. The certificate has not been verified yet, so
-- this only offers: what the node fetches is still up to each election's focus.
recordLeiosCertOffer ::
  forall m peer blk.
  (IOLike m, BlockSupportsProtocol blk) =>
  TopLevelConfig blk ->
  LeiosFetchKernel m peer blk ->
  LeiosFetchPeer m blk ->
  Header blk ->
  -- | The state as of the header's predecessor
  HeaderStateWithTime blk ->
  m ()
recordLeiosCertOffer cfg kernel peerVars hdr predState =
  when (getContainsLeiosCert p (validateView (configBlock cfg) hdr)) $
    -- The predecessor of a valid header that contains a certificate announced
    -- an endorser block, in an era with Leios, so the other cases cannot arise.
    -- Ignoring the header then would be harmless.
    case ( headerStateTip predHeaderState
         , getCertifiableLeiosAnnouncement p (headerStateChainDep predHeaderState)
         , hswtLeiosLedgerView predState
         ) of
      (NotOrigin predTip, Just announcement, SJust llv) -> do
        let slot = annTipSlotNo predTip
            ebHash = announcementEbHash announcement
        noteCertificationClaim
          kernel
          peerVars
          (ElectionId slot (announcementElectee announcement))
          ebHash
        recordLeiosOffer kernel peerVars (MkLeiosPoint slot ebHash) $
          MkPeerOffer
            { poEbBounds = SJust (leiosEbBounds llv)
            , poOfferedBody = SJust (announcementEbSize announcement)
            , poClosure = TxsClosureOffered
            }
      _ -> pure ()
 where
  p = Proxy @(BlockProtocol blk)
  predHeaderState = hswtHeaderState predState

leiosEbBounds :: LeiosLedgerView -> LeiosEbBounds
leiosEbBounds llv = MkLeiosEbBounds (llvMaxEbBodySize llv) (llvMaxEbTxsSize llv)

-- | Count the peer's claim that an endorser block is certified for an
-- election, disconnecting it if it contradicts an earlier claim of the peer's.
--
-- An honest peer rolls forward only what it selects, and one chain certifies at
-- most one endorser block per election. Two chains certifying different ones
-- take two valid certificates for one election, which only a committee that
-- equivocated can produce.
--
-- Without this, a pool could equivocate its won slots into arbitrarily many
-- announcing blocks, follow each with a header claiming a certificate --- the
-- certificate is in the block body, which the node need never fetch --- and roll
-- them all forward, growing the peer's offers without bound.
--
-- A claim for an election older than what has been pruned is not counted.
noteCertificationClaim ::
  IOLike m =>
  LeiosFetchKernel m peer blk -> LeiosFetchPeer m blk -> ElectionId -> EndorserBlockHash -> m ()
noteCertificationClaim kernel MkLeiosFetchPeer{lfpCertificationClaims} elId ebHash = do
  conflict <- atomically $ do
    prunedSlot <- readTVar (lfkPrunedSlot kernel)
    claims <- readTVar lfpCertificationClaims
    if electionSlot elId < prunedSlot
      then pure Nothing
      else case Map.lookup elId claims of
        Nothing -> Nothing <$ writeTVar lfpCertificationClaims (Map.insert elId ebHash claims)
        Just claimed -> pure $ if claimed == ebHash then Nothing else Just claimed
  forM_ conflict $ \claimed -> throwIO $ ExnLeiosTwoCertificationClaims elId claimed ebHash

-- | Thrown when a peer's ChainSync roll-forwards claim that two different
-- endorser blocks are certified for one election; see 'noteCertificationClaim'.
data ExnLeiosTwoCertificationClaims
  = ExnLeiosTwoCertificationClaims !ElectionId !EndorserBlockHash !EndorserBlockHash
  deriving Show

instance Exception ExnLeiosTwoCertificationClaims

-- | 'recordLeiosCertOffer' for each header of a ChainSync jump the peer
-- accepted that its earlier jumps did not already cover.
--
-- Successive jumps along one chain overlap in all but their newest headers. If
-- this jump's fragment holds the newest header of the previous jump, it holds
-- the same headers before that one too, so those are skipped.
recordLeiosJumpOffers ::
  (IOLike m, LedgerSupportsProtocol blk) =>
  TopLevelConfig blk ->
  LeiosFetchKernel m peer blk ->
  LeiosFetchPeer m blk ->
  JumpInfo blk ->
  m ()
recordLeiosJumpOffers cfg kernel peerVars jumpInfo = do
  jumpedThrough <- readTVarIO (lfpJumpedThrough peerVars)
  forM_ (headersWithPredecessorStates jumpedThrough jumpInfo) $
    uncurry (recordLeiosCertOffer cfg kernel peerVars)
  atomically $
    writeTVar (lfpJumpedThrough peerVars) (castPoint (AF.headPoint (jTheirFragment jumpInfo)))

-- | The headers of the jump's fragment after the given point, each with the
-- state as of its predecessor.
--
-- The history holds the state after each header of the fragment and after its
-- anchor, so splitting both after the same point leaves the history's anchor
-- holding the state before the first remaining header. If neither holds that
-- point, all of the fragment is walked.
headersWithPredecessorStates ::
  forall blk.
  LedgerSupportsProtocol blk =>
  Point blk ->
  JumpInfo blk ->
  [(Header blk, HeaderStateWithTime blk)]
headersWithPredecessorStates jumpedThrough jumpInfo =
  zip
    (hwtHeader <$> AF.toOldestFirst fragment)
    (AS.anchor history : AS.toOldestFirst history)
 where
  past ::
    AS.Anchorable (WithOrigin SlotNo) a b =>
    (Either a b -> Point blk) ->
    AS.AnchoredSeq (WithOrigin SlotNo) a b ->
    AS.AnchoredSeq (WithOrigin SlotNo) a b
  past pointOf s =
    maybe s snd $
      AS.splitAfterMeasure (pointSlot jumpedThrough) ((== jumpedThrough) . pointOf) s

  fragment =
    past (either (castPoint . AF.anchorToPoint) (castPoint . blockPoint)) $
      jTheirFragment jumpInfo
  history =
    past (headerStatePoint . hswtHeaderState . either id id) $
      unHeaderStateHistory $
        jTheirHeaderStateHistory jumpInfo

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
-- If the announcement takes the focus and the node already holds its endorser
-- block, this also announces the acquisition again, at its point (see
-- 'LeiosEbNotification').
onNewLeiosAnnouncement ::
  forall m peer blk.
  (IOLike m, ConvertRawHash blk, HasHeader (Header blk)) =>
  LeiosFetchKernel m peer blk ->
  ValidAnnouncement blk ->
  m ()
onNewLeiosAnnouncement kernel valid = do
  void $
    insertAnnouncement
      lfcTxCache
      slot
      (toRankingBlockHash (Proxy @blk) (headerHash (ancHeader anc)))
      ebHash
  pointWritten <- writeEbPoint lfcWriter point (announcementEbSize announcement)
  (changed, reacquired) <- modifyMVar lfcOutstanding $ \outstanding ->
    let tooOld = slot < outstandingPrunedSlot outstanding
        introducesFocus = not $ Map.member elId (elFocus outstanding)
        held = maybe False ebStateHasBody (Map.lookup ebHash (ebState outstanding))
        outstanding' =
          focusElectionIfUnfocused elId ebHash $
            recordMaxAnnouncementSlot ebHash slot (SJust announcementOnset) outstanding
     in pure $
          if tooOld || not introducesFocus
            then (outstanding, (False, []))
            else (outstanding', (not held, heldAcquisitionsAt point outstanding'))
  -- The LeiosDB keeps the endorser block only while some point it holds for
  -- the block is recent, so it must hold this one before anyone hears of it.
  unless (null reacquired) $ await pointWritten
  atomically $ mapM_ lfcNotify reacquired
  when changed $ void $ tryPutMVar lfcReady ()
 where
  MkLeiosFetchClientContext{lfcOutstanding, lfcReady, lfcTxCache, lfcWriter, lfcNotify} = lfkContext kernel
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
  (IOLike m, Ord peer, LedgerSupportsProtocol blk) =>
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
    let mbCurrentSlot = case currentSlot of
          CurrentSlot s -> Just s
          CurrentSlotUnknown -> Nothing
        -- One forecast anchor for the whole pass: the immutable tip, which is
        -- also what the announcement validation a LeiosNotify offer rides on
        -- forecast from, so it reaches every slot such an offer names.
        forecastEbBounds slot =
          maybe (MkLeiosEbBounds 0 0) leiosEbBounds . getLeiosLedgerView (Proxy @(BlockProtocol blk))
            <$> runExcept
              (forecastFor (ledgerViewForecastAt (configLedger cfg) (ledgerState immLedger)) slot)
        (!outstanding', requests, offerDrops) =
          leiosFetchLogicIteration
            (lfcStaticEnv lfkContext)
            forecastEbBounds
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

-- | Move each election's focus onto the endorser block a verified certificate
-- certifies, as ChainSel verifies such certificates.
--
-- This is how a node that only saw some other announcement for the election,
-- or none, because the announcing block has been on its selection since before
-- it started, comes to fetch the certified one. Each iteration compares the
-- certified endorser blocks against the ones it last saw, so a certificate
-- verified after the offer arrived still moves the focus.
--
-- If that moves an election's focus onto an endorser block the node already
-- holds, this also announces the acquisition again, at the certified point (see
-- 'LeiosEbNotification'), once the LeiosDB holds that point, so that it keeps
-- the endorser block for as long as the point matters.
--
-- TODO The LeiosDB records a size with each point, which LeiosFetch does not
-- know for an endorser block the LeiosDB held when the node started, so for
-- such a block this writes no point, and the LeiosDB can still evict it while
-- the certified point matters.
leiosFetchFocuser ::
  IOLike m =>
  ChainDB m blk ->
  LeiosFetchKernel m peer blk ->
  m Void
leiosFetchFocuser chainDB kernel = go Map.empty
 where
  MkLeiosFetchClientContext{lfcOutstanding, lfcReady, lfcNotify, lfcWriter} = lfkContext kernel

  go seen = do
    certified <- atomically $ do
      certified <- ChainDB.getLeiosCertifiedEbs chainDB
      if certified == seen then retry else pure certified
    let arrived =
          Map.differenceWith (\new old -> if new == old then Nothing else Just new) certified seen
    reacquired <- modifyMVar lfcOutstanding $ \outstanding ->
      pure $ Map.foldlWithKey' focus (outstanding, []) arrived
    pointsWritten <-
      forM [(p, size) | AcquiredEb p size <- reacquired] $ uncurry (writeEbPoint lfcWriter)
    mapM_ await pointsWritten
    atomically $ mapM_ lfcNotify reacquired
    void $ tryPutMVar lfcReady ()
    go certified

  focus (outstanding, reacquired) elId ebHash =
    let outstanding' = focusCertifiedEb elId ebHash outstanding
        focuses o = Map.lookup elId (elFocus o) == Just ebHash
        newlyFocused = focuses outstanding' && not (focuses outstanding)
     in ( outstanding'
        , if newlyFocused
            then reacquired <> heldAcquisitionsAt (MkLeiosPoint (electionSlot elId) ebHash) outstanding'
            else reacquired
        )

-- | Prune the node's LeiosFetch state, the peers' offers and claims, and the
-- LeiosTxCache each time the immutable tip's slot advances.
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
  LeiosFetchKernel m peer blk ->
  m Void
leiosFetchPruner chainDB kernel = go (SlotNo 0)
 where
  MkLeiosFetchKernel{lfkContext, lfkPeers, lfkPrunedSlot} = kernel
  MkLeiosFetchClientContext{lfcOutstanding, lfcTxCache} = lfkContext

  go prunedSlot = do
    immTipSlot <- atomically $ do
      immLedger <- ChainDB.getImmutableLedger chainDB
      case getTipSlot (ledgerState immLedger) of
        NotOrigin slot | prunedSlot < slot -> pure slot
        _ -> retry
    _ <- evictOlderThan lfcTxCache immTipSlot
    modifyMVar_ lfcOutstanding $ pure . snd . pruneOutstandingToImmTip immTipSlot
    -- Offers are keyed by point and claims by election, which both order by
    -- slot first.
    atomically $ do
      writeTVar lfkPrunedSlot immTipSlot
      peersVars <- readTVar lfkPeers
      forM_ peersVars $ \vars -> do
        modifyTVar (lfpOfferings vars) (Map.dropWhileAntitone ((< immTipSlot) . pointSlotNo))
        modifyTVar
          (lfpCertificationClaims vars)
          (Map.dropWhileAntitone ((< immTipSlot) . electionSlot))
    go immTipSlot
