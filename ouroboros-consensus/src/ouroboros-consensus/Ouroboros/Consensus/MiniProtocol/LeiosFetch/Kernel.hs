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
  , BlockSupportsProtocol (..)
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
  )
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
  -- LeiosNotify and ChainSync clients, as the peer offers, and by the decision
  -- loop and the pruner, as offers go dead.
  , lfpRequestsToSend :: !(StrictTVar m (Seq LeiosFetchRequest))
  -- ^ The requests the decision loop assigned to this peer that its LeiosFetch
  -- client has not sent yet.
  , lfpCertificationClaims :: !(StrictTVar m (Map.Map ElectionId EndorserBlockHash))
  -- ^ The endorser block the peer's ChainSync roll-forwards claim is certified,
  -- per election; see 'recordLeiosCertOffer'.
  , lfpJumpedThrough :: !(StrictTVar m (WithOrigin SlotNo))
  -- ^ The slot of the newest header of the ChainSync jumps the peer has
  -- accepted; see 'recordLeiosJumpOffers'.
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
        <*> uncheckedNewTVarM Map.empty
        <*> uncheckedNewTVarM Origin
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
  LeiosFetchKernel m peer ->
  LeiosFetchPeer m ->
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
noteCertificationClaim ::
  IOLike m => LeiosFetchPeer m -> ElectionId -> EndorserBlockHash -> m ()
noteCertificationClaim MkLeiosFetchPeer{lfpCertificationClaims} elId ebHash = do
  conflict <- atomically $ do
    claims <- readTVar lfpCertificationClaims
    case Map.lookup elId claims of
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
-- Successive jumps overlap in all but their newest headers. Skipping the
-- headers up to the newest of the previous jump is sound because a dynamo may
-- not roll back past the last jump it requested: ChainSync jumping disengages
-- one that tries.
recordLeiosJumpOffers ::
  (IOLike m, LedgerSupportsProtocol blk) =>
  TopLevelConfig blk ->
  LeiosFetchKernel m peer ->
  LeiosFetchPeer m ->
  JumpInfo blk ->
  m ()
recordLeiosJumpOffers cfg kernel peerVars jumpInfo = do
  jumpedThrough <- readTVarIO (lfpJumpedThrough peerVars)
  forM_ (headersWithPredecessorStates jumpedThrough jumpInfo) $
    uncurry (recordLeiosCertOffer cfg kernel peerVars)
  atomically $
    modifyTVar (lfpJumpedThrough peerVars) (max (AF.headSlot (jTheirFragment jumpInfo)))

-- | The headers of the jump's fragment newer than the given slot, each with the
-- state as of its predecessor.
--
-- The history holds the state after each header of the fragment and after its
-- anchor, so splitting both after the same slot leaves the history's anchor
-- holding the state before the first remaining header. If neither holds that
-- slot, which needs the peer to have accepted jumps onto two different chains,
-- all of the fragment is walked.
headersWithPredecessorStates ::
  LedgerSupportsProtocol blk =>
  WithOrigin SlotNo ->
  JumpInfo blk ->
  [(Header blk, HeaderStateWithTime blk)]
headersWithPredecessorStates jumpedThrough jumpInfo =
  zip
    (hwtHeader <$> AF.toOldestFirst fragment)
    (AS.anchor history : AS.toOldestFirst history)
 where
  past ::
    AS.Anchorable (WithOrigin SlotNo) a b =>
    AS.AnchoredSeq (WithOrigin SlotNo) a b ->
    AS.AnchoredSeq (WithOrigin SlotNo) a b
  past s = maybe s snd $ AS.splitAfterMeasure jumpedThrough (const True) s

  fragment = past $ jTheirFragment jumpInfo
  history = past $ unHeaderStateHistory $ jTheirHeaderStateHistory jumpInfo

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
  void $ writeEbPoint lfcWriter point (announcementEbSize announcement)
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
-- 'LeiosEbNotification').
leiosFetchFocuser ::
  IOLike m =>
  ChainDB m blk ->
  LeiosFetchKernel m peer ->
  m Void
leiosFetchFocuser chainDB kernel = go Map.empty
 where
  MkLeiosFetchClientContext{lfcOutstanding, lfcReady, lfcNotify} = lfkContext kernel

  go seen = do
    certified <- atomically $ do
      certified <- ChainDB.getLeiosCertifiedEbs chainDB
      if certified == seen then retry else pure certified
    let arrived =
          Map.differenceWith (\new old -> if new == old then Nothing else Just new) certified seen
    reacquired <- modifyMVar lfcOutstanding $ \outstanding ->
      pure $ Map.foldlWithKey' focus (outstanding, []) arrived
    atomically $ mapM_ lfcNotify reacquired
    void $ tryPutMVar lfcReady ()
    go certified

  focus (outstanding, reacquired) elId ebHash =
    let outstanding' = focusCertifiedEb elId ebHash outstanding
        newlyFocused = case Map.lookup elId (elFocus outstanding) of
          Just focused -> focused /= ebHash
          Nothing -> True
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
    -- Offers are keyed by point and claims by election, which both order by
    -- slot first.
    peersVars <- readTVarIO lfkPeers
    atomically $ forM_ peersVars $ \vars -> do
      modifyTVar (lfpOfferings vars) (Map.dropWhileAntitone ((< immTipSlot) . pointSlotNo))
      modifyTVar
        (lfpCertificationClaims vars)
        (Map.dropWhileAntitone ((< immTipSlot) . electionSlot))
    go immTipSlot
