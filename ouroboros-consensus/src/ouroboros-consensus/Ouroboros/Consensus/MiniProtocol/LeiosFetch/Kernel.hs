{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Shared LeiosFetch state, planning and node-owned work. Bounded workers
-- establish announcement focus and acquire payloads; the certificate focuser
-- follows ChainDB. The node's shared immutable-tip worker invokes this module's
-- fetch-pruning entrypoint. See
-- @docs/fetch-policy/leiosfetch-improved.md#ownership-and-shutdown@ and
-- @docs/fetch-policy/leiosfetch-improved.md#immutable-tip-pruning@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Kernel
  ( LeiosFetchKernel (..)
  , LeiosFetchPeer (..)
  , OfferTimes (..)
  , noteOfferTime
  , LeiosPeerLifetimeId (..)
  , newLeiosFetchKernel
  , newLeiosFetchKernelFromState
  , withLeiosFetchPeer
  , recordLeiosOffer
  , recordLeiosCertOffer
  , recordLeiosJumpOffers
  , ExnLeiosTwoCertificationClaims (..)
  , enqueueLeiosAnnouncement
  , onForgedLeiosEb
  , withLeiosFetchWorkers
  , leiosFetchDecisionLoop
  , leiosFetchCertificateFocuser
  , pruneLeiosFetchToImmTip
  ) where

import Control.Monad (forM_, forever, unless, void, when)
import qualified Control.Monad.Class.MonadTimer.SI as SI
import Control.Monad.Except (runExcept)
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Void (Void)
import Data.Word (Word64)
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
import Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory (..))
import qualified Ouroboros.Consensus.HardFork.History as HardFork
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
import Ouroboros.Consensus.Leios.AnnounceState (AnnounceState, onValidAnnouncement)
import Ouroboros.Consensus.Leios.Cert (LeiosClaim (..))
import Ouroboros.Consensus.Leios.Protocol
  ( LeiosAnnouncement (..)
  , LeiosLedgerView (..)
  , LeiosProtocol (..)
  , leiosEbByteSizeBounds
  )
import Ouroboros.Consensus.Leios.TxCache.API (evictOlderThan)
import Ouroboros.Consensus.Leios.Types
  ( ElectionId (..)
  , EndorserBlockHash
  , ForgedEndorserBlock (..)
  , LeiosEbByteSizes (..)
  , LeiosPoint (..)
  , RankingBlockHash
  , endorserBlockHash
  , minCertificationSlot
  , toRankingBlockHash
  )
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State (JumpInfo (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies
  ( BodyWork
  , LeiosFetchClientContext (..)
  , submitForgedEb
  , withBodyWorkers
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Config (LeiosFetchStaticEnv)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
  ( LeiosOutstanding (..)
  , OfferedHalves (..)
  , PeerOffer (..)
  , assignRequestsObserved
  , focusCertifiedEb
  , focusElectionIfUnfocused
  , initializeLeiosOutstanding
  , markReceivingFromForge
  , pruneOutstandingToImmTip
  , recordReferencedPoint
  , removePeerFromOutstanding
  , workStillNeeded
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.OwnedWork as Owned
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.ReceiptTasks as Receipts
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.RequestState as L
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Runtime as R
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace (FetchRoutine (..))
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace.Policy as A
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Wait as W
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
  ( Exception
  , IOLike
  , STM
  , StrictMVar
  , StrictTVar
  , Time
  , atomically
  , bracket
  , catch
  , finally
  , getMonotonicTime
  , mask_
  , modifyMVar
  , modifyMVar_
  , modifyTVar
  , onException
  , readTVar
  , readTVarIO
  , throwIO
  , throwSTM
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
  { lfkContext :: !(LeiosFetchClientContext m (LeiosPeerLifetimeId peer))
  , lfkPeers :: !(StrictTVar m (Map.Map (LeiosPeerLifetimeId peer) (LeiosFetchPeer m blk)))
  -- ^ The upstream peers the decision loop assigns requests to, each
  -- registered at most once at a time; see 'withLeiosFetchPeer'.
  , lfkNextLifetimeId :: !(StrictTVar m Word64)
  -- ^ What tells the next registration's 'LeiosPeerLifetimeId' apart from
  -- every earlier one.
  , lfkWorkClosing :: !(StrictTVar m Bool)
  -- ^ Closes every queue atomically before draining pre-admitted work.
  , lfkAnnouncements :: !(Owned.OwnedWork m RankingBlockHash (ValidAnnouncement blk))
  -- ^ Bounded metadata obligations, independent of slow storage producers.
  , lfkForged :: !(Owned.OwnedWork m LeiosPoint (ForgedEndorserBlock, STM m ()))
  -- ^ Matching full-point callbacks share submission and confirmed completion.
  , lfkPrunedSlot :: !(StrictTVar m SlotNo)
  -- ^ The slot 'pruneLeiosFetchToImmTip' has most recently pruned the peers' offers
  -- and claims to. Nothing older is recorded afterwards, so that an offer or
  -- claim that arrives after the pruning cannot undo it.
  }

-- | Identifies one registration of an upstream peer with the decision loop,
-- from when the first of its mini-protocols registers it until it is
-- unregistered; see 'withLeiosFetchPeer'. The decision loop's state is keyed
-- by it rather than by the peer, which the peer's later registrations share.
--
-- The peer alone would not do, even though connections whose mini-protocols
-- run at the same time have different @ConnectionId@s: that covers only the
-- mini-protocols' own threads. Accepted writes are reconciled by fixed body
-- workers and node-owned TX receipt tasks, which can outlive the client.
-- By then the peer may have registered again, after the mux
-- restarted its mini-protocols or after it reconnected with the same
-- @ConnectionId@, and the update would act on that registration instead.
data LeiosPeerLifetimeId peer = MkLeiosPeerLifetimeId !peer !Word64
  deriving (Eq, Ord, Show)

-- | What the node's LeiosFetch state holds about one upstream peer.
data LeiosFetchPeer m blk = MkLeiosFetchPeer
  { lfpIsBigLedgerPeer :: !IsBigLedgerPeer
  -- ^ Fixed for the connection's lifetime. Selects the independently
  -- configured BLP credit, equal to ordinary credit by default.
  , lfpOfferings :: !(StrictTVar m (Map.Map LeiosPoint PeerOffer))
  -- ^ The peer's current offers, keyed by point. Written only by the
  -- LeiosNotify and ChainSync clients, as the peer offers, and by the decision
  -- loop and the pruner, as offers go dead.
  , lfpOfferTimes :: !(StrictTVar m (Map.Map LeiosPoint OfferTimes))
  -- ^ Independent first body/closure observations, pruned with availability.
  , lfpRequestsToSend :: !(StrictTVar m (Seq L.CommandKey))
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
-- neither fetches them nor emits their acquisitions again.
newLeiosFetchKernel ::
  (IOLike m, LedgerSupportsProtocol blk) =>
  ChainDB m blk ->
  LeiosDbHandle m ->
  -- | Ordinary admission and service policy.
  LeiosFetchStaticEnv ->
  -- | The node's producer/receipt group has begun closing.
  STM m Bool ->
  -- | Seeds the decision loop's PRNG.
  StdGen ->
  -- | Everything of the clients' context but the state this creates.
  ( StrictMVar m (LeiosOutstanding (LeiosPeerLifetimeId peer)) ->
    W.Readiness m ->
    R.Runtime m (LeiosPeerLifetimeId peer) ->
    Owned.OwnedWork m LeiosPoint (BodyWork m) ->
    (LeiosPoint -> STM m Bool) ->
    LeiosFetchClientContext m (LeiosPeerLifetimeId peer)
  ) ->
  m (LeiosFetchKernel m peer blk)
newLeiosFetchKernel chainDB leiosDb env closing prng mkContext = do
  immTipSlot <-
    atomically $
      withOrigin (SlotNo 0) id . getTipSlot . ledgerState <$> ChainDB.getImmutableLedger chainDB
  acquired <- withReader leiosDb $ \reader ->
    scanCompleteEbClosuresNotOlderThanSlot reader immTipSlot
  newLeiosFetchKernelFromState
    (initializeLeiosOutstanding prng acquired immTipSlot)
    env
    closing
    mkContext

-- | Allocate the live runtime from a recovered outstanding snapshot. Database
-- recovery belongs to 'newLeiosFetchKernel'; queue ownership and activation are
-- independent of storage bootstrap.
newLeiosFetchKernelFromState ::
  IOLike m =>
  LeiosOutstanding (LeiosPeerLifetimeId peer) ->
  LeiosFetchStaticEnv ->
  STM m Bool ->
  ( StrictMVar m (LeiosOutstanding (LeiosPeerLifetimeId peer)) ->
    W.Readiness m ->
    R.Runtime m (LeiosPeerLifetimeId peer) ->
    Owned.OwnedWork m LeiosPoint (BodyWork m) ->
    (LeiosPoint -> STM m Bool) ->
    LeiosFetchClientContext m (LeiosPeerLifetimeId peer)
  ) ->
  m (LeiosFetchKernel m peer blk)
newLeiosFetchKernelFromState initial env closing mkContext = do
  outstanding <- uncheckedNewMVar initial
  ready <- atomically W.newReadiness
  lfkWorkClosing <- uncheckedNewTVarM False
  let workClosing = (||) <$> readTVar lfkWorkClosing <*> closing
  runtime <- R.newRuntime env ready workClosing
  lfkAnnouncements <- Owned.newOwnedWork "LeiosFetch.announcements" 128 workClosing
  bodies <- Owned.newOwnedWork "LeiosFetch.bodies" 2 workClosing
  lfkForged <- Owned.newOwnedWork "LeiosFetch.forged" 4 workClosing
  lfkPeers <- uncheckedNewTVarM Map.empty
  lfkNextLifetimeId <- uncheckedNewTVarM 0
  lfkPrunedSlot <- uncheckedNewTVarM (outstandingPrunedSlot initial)
  pure
    MkLeiosFetchKernel
      { lfkContext =
          mkContext
            outstanding
            ready
            runtime
            bodies
            (\point -> (pointSlotNo point <) <$> readTVar lfkPrunedSlot)
      , lfkPeers
      , lfkNextLifetimeId
      , lfkPrunedSlot
      , lfkWorkClosing
      , lfkAnnouncements
      , lfkForged
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
        <*> uncheckedNewTVarM Map.empty
        <*> uncheckedNewTVarM GenesisPoint
    atomically $ do
      peers <- readTVar lfkPeers
      -- 'LeiosPeerLifetimeId's order by their peer first, so this finds the
      -- peer's registration if it has one.
      case Map.lookupGE (MkLeiosPeerLifetimeId peer 0) peers of
        Just (lifetimeId@(MkLeiosPeerLifetimeId peer' _), existing)
          | peer' == peer -> pure (lifetimeId, existing)
        _ -> do
          n <- readTVar lfkNextLifetimeId
          when (n == maxBound) (throwSTM R.OrdinalExhausted)
          writeTVar lfkNextLifetimeId (n + 1)
          let lifetimeId = MkLeiosPeerLifetimeId peer n
          writeTVar lfkPeers (Map.insert lifetimeId fresh peers)
          pure (lifetimeId, fresh)

  -- The decision loop reads the registered peers within the same critical
  -- section in which it assigns requests, so it cannot assign this peer
  -- anything after its requests are cancelled here.
  unregister (lifetimeId, _) = do
    events <- mask_ $ modifyMVar (lfcOutstanding lfkContext) $ \outstanding -> do
      events <- atomically $ do
        facts <- readTVar (R.runtimeFacts (lfcRuntime lfkContext))
        let captured = case Map.lookup lifetimeId (R.sessions facts) of
              Nothing -> []
              Just session ->
                [ A.Routine
                    (L.AttemptKey lifetimeId (R.invocation session) 0)
                    (FetchRegistrationRemoved (A.usageSummary (lfcRuntime lfkContext) lifetimeId facts))
                ]
        modifyTVar lfkPeers (Map.delete lifetimeId)
        R.closePeer (lfcRuntime lfkContext) lifetimeId
        pure captured
      let !captured = A.forceObservations events
      pure (removePeerFromOutstanding lifetimeId outstanding, captured)
    A.emitObservations (lfcTracer lfkContext) (lfcDetailTracer lfkContext) events

-- | Record a peer's offer, which an earlier offer of the same point merges
-- with, unless the point is older than what has been pruned. Wake only for a
-- changed offer or first-arrival observation, in the same publishing transaction.
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
      offerings <- readTVar lfpOfferings
      times <- readTVar lfpOfferTimes
      let oldOffer = Map.lookup point offerings
          nextOffer = maybe offer (offer <>) oldOffer
          oldTime = Map.lookup point times
          nextTime = noteOfferTime now (poOffered offer) oldTime
          changedOffer = oldOffer /= Just nextOffer
          changedTime = oldTime /= Just nextTime
      when changedOffer $ writeTVar lfpOfferings (Map.insert point nextOffer offerings)
      when changedTime $ writeTVar lfpOfferTimes (Map.insert point nextTime times)
      -- Compare only this bounded metadata entry. Publish availability,
      -- earliest arrival and readiness together, including timestamp repair.
      when (changedOffer || changedTime) $
        W.notifyReadiness (lfcReady (lfkContext kernel))

-- | Record the offer a peer makes by rolling forward a header that claims its
-- block contains a Leios certificate: the endorser block the certificate
-- certifies, both its body, at the size its announcement names, and its
-- closure.
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
    -- The predecessor of a valid header that claims its block contains a
    -- certificate announced an endorser block, in an era with Leios, so the
    -- other cases cannot arise. Ignoring the header then would be harmless.
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
            { poEbBounds = SJust (leiosEbByteSizeBounds llv)
            , poOffered = OfferedBodyAndClosure (announcementEbSize announcement)
            }
      _ -> pure ()
 where
  p = Proxy @(BlockProtocol blk)
  predHeaderState = hswtHeaderState predState

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

-- | Own the bookkeeping of a newly accepted announcement in the same STM
-- transaction as global acceptance. The bounded metadata worker establishes
-- election focus and lists the point to fetch; it does not retain a cache point.
-- Body insertion alone establishes body-bearing cache retention.
--
-- Peer callers compose this with acceptance of a new non-proof announcement.
-- Network fetch state initially tracks one focused EB per election. An
-- equivocation proof creates no second initial-focus obligation; a verified
-- certificate can later select that EB.
--
-- Queued metadata alone never establishes body or transaction possession.
enqueueLeiosAnnouncement ::
  forall m peer blk.
  (IOLike m, ConvertRawHash blk, HasHeader (Header blk)) =>
  LeiosFetchKernel m peer blk ->
  ValidAnnouncement blk ->
  STM m ()
enqueueLeiosAnnouncement kernel valid = do
  void $
    Owned.submitWork
      (lfkAnnouncements kernel)
      (toRankingBlockHash (Proxy @blk) (headerHash (ancHeader (announcingHeader valid))))
      valid
  W.notifyReadiness (lfcReady (lfkContext kernel))

-- The bounded metadata worker keeps the focus-installation obligation after
-- the announcing peer disconnects, without cache insertion or database IO.
registerAnnouncement ::
  IOLike m =>
  LeiosFetchKernel m peer blk -> ValidAnnouncement blk -> m ()
registerAnnouncement kernel valid = do
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
  when changed $ atomically $ W.notifyReadiness lfcReady
 where
  MkLeiosFetchClientContext{lfcOutstanding, lfcReady} = lfkContext kernel
  MkValidAnnouncement{announcingHeader = anc, announcementOnset} = valid
  announcement = ancAnnouncement anc
  elId = ancElectionId anc
  slot = electionSlot elId
  ebHash = announcementEbHash announcement
  point = MkLeiosPoint slot ebHash

-- | The forge's counterpart to receiving an announcement and its endorser block
-- from a peer: take in the endorser block the node's own forge produced, and
-- the header announcing it, with no peer.
--
-- Admission installs an exact-point local-payload claim before the metadata
-- or storage worker runs. The worker marks 'ReceivingFromForge' and submits
-- its data. Matching callbacks share that operation through durable settlement.
-- The callback returns when the bounded admission transaction commits; it does
-- not wait for writer admission or durability. Full queues still backpressure
-- the forge. Caller cancellation after admission never cancels the shared
-- worker. Announcement novelty does not gate the payload.
--
-- Four payload slots cover queued, running and receipt-waiting operations;
-- metadata has a separate 128-entry bound. Retain this bounded backpressure:
-- saturation can delay ranking-block submission, even though admitted callers
-- return before storage completes. The consensus team owns any stronger forging
-- guarantee; see the TODO at the node's forged-EB handoff.
--
-- The header must announce this endorser block.
onForgedLeiosEb ::
  forall m peer blk.
  (IOLike m, ConvertRawHash blk, HasHeader (Header blk)) =>
  LeiosFetchKernel m peer blk ->
  AnnounceState m blk ->
  ValidAnnouncement blk ->
  ForgedEndorserBlock ->
  m ()
onForgedLeiosEb kernel announcements valid forged = atomically $ do
  closing <- R.runtimeClosing (lfcRuntime (lfkContext kernel))
  unless closing $ do
    void $ onValidAnnouncement announcements valid
    metadata <-
      Owned.submitWork
        (lfkAnnouncements kernel)
        (toRankingBlockHash (Proxy @blk) (headerHash (ancHeader anc)))
        valid
    W.notifyReadiness (lfcReady (lfkContext kernel))
    pruned <- readTVar (lfkPrunedSlot kernel)
    unless (pointSlotNo point < pruned) $ do
      admission <- Owned.submitWork (lfkForged kernel) point (forged, Owned.awaitAdmission metadata)
      when (Owned.newlyOwned admission) $ R.claimLocalForge (lfcRuntime (lfkContext kernel)) point
 where
  anc = announcingHeader valid
  point = MkLeiosPoint (electionSlot (ancElectionId anc)) (endorserBlockHash (forgedEbBody forged))

-- | The receipt supervisor encloses worker activation, processing, drain and
-- cleanup. Work admitted before activation remains queued until this scope.
-- Metadata, shared body preparation and forged payloads retain their separate
-- 128/two/four bounds. See
-- @docs/fetch-policy/leiosfetch-improved.md#ownership-and-shutdown@.
withLeiosFetchWorkers ::
  forall m peer blk a.
  IOLike m =>
  LeiosFetchKernel m peer blk -> m a -> m a
withLeiosFetchWorkers kernel action =
  ( Receipts.withReceiptTasks (lfcReceiptTasks ctx)
      . Owned.withOwnedWork
        (lfkAnnouncements kernel)
        (\_ valid -> registerAnnouncement kernel valid >> pure (pure False))
      . withBodyWorkers ctx
      . Owned.withOwnedWork (lfkForged kernel) submitForge
      $ action `finally` atomically (writeTVar (lfkWorkClosing kernel) True)
  )
    `finally` atomically (modifyTVar (R.runtimeFacts (lfcRuntime ctx)) (\st -> st{R.localForges = mempty}))
 where
  ctx = lfkContext kernel
  release point = atomically $ R.releaseLocalForge (lfcRuntime ctx) point
  submitForge :: LeiosPoint -> (ForgedEndorserBlock, STM m ()) -> m (m Bool)
  submitForge point (forged, metadataReady) =
    ( do
        atomically metadataReady
        current <- modifyMVar (lfcOutstanding ctx) $ \outstanding ->
          pure $
            if pointSlotNo point < outstandingPrunedSlot outstanding
              then (outstanding, False)
              else (markReceivingFromForge point outstanding, True)
        if not current
          then release point >> pure (pure False)
          else do
            result <- submitForgedEb ctx point forged
            case result of
              Nothing -> throwIO Owned.WorkClosed
              Just settled -> pure (atomically settled `finally` release point)
    )
      `onException` release point
      -- The metadata/receipt owner supervises its original failure. A forge
      -- waiting on that dependency abandons its claim through the cleanup above,
      -- without turning closure into another linked worker failure.
      `catch` \Owned.WorkClosed -> pure (pure False)

-- | Each time something might give a peer something new to do, decide what to
-- request from every peer, waking on readiness or the nearest active deadline.
leiosFetchDecisionLoop ::
  forall m peer blk.
  (IOLike m, SI.MonadTimer m, Ord peer, LedgerSupportsProtocol blk, HasHardForkHistory blk) =>
  TopLevelConfig blk ->
  ChainDB m blk ->
  BlockchainTime m ->
  LeiosFetchKernel m peer blk ->
  m Void
leiosFetchDecisionLoop cfg chainDB btime kernel = forever $ do
  due <- atomically (R.nextDeadline <$> readTVar (R.runtimeFacts lfcRuntime))
  _ <- W.waitForWakeup lfcReady due
  iterationStart <- getMonotonicTime
  reports <- modifyMVar lfcOutstanding $ \outstanding -> do
    -- Read the live peers while holding the outstanding state, so that this
    -- assigns nothing to a peer whose unregistering already cancelled its
    -- requests: their replies would never arrive, and the requests would stay
    -- outstanding forever.
    (offerings, times, currentSlot, immLedger) <- atomically $ do
      peersVars <- readTVar lfkPeers
      offerings <- traverse (readTVar . lfpOfferings) peersVars
      times <- traverse (fmap (Map.mapMaybe closureOfferTime) . readTVar . lfpOfferTimes) peersVars
      (,,,) offerings times <$> getCurrentSlot btime <*> ChainDB.getImmutableLedger chainDB
    let
      -- One forecast anchor for the whole pass: the immutable tip, which is
      -- also what the announcement validation a LeiosNotify offer rides on
      -- forecast from, so it reaches every slot such an offer names.
      forecastLeiosView slot =
        getLeiosLedgerView (Proxy @(BlockProtocol blk))
          <$> runExcept
            ( forecastFor
                (ledgerViewForecastAt (configLedger cfg) (ledgerState immLedger))
                (max anchorSlot slot)
            )
      anchorSlot = withOrigin (SlotNo 0) id (getTipSlot (ledgerState immLedger))
      forecastEbBounds slot = maybe (MkLeiosEbByteSizes 0 0) leiosEbByteSizeBounds <$> forecastLeiosView slot
      -- The current slot, and how many slots before it a point is still worth
      -- voting on, by the Leios periods of the ledger there, if the ledger
      -- reaches that far.
      mbHorizon = case currentSlot of
        CurrentSlotUnknown -> Nothing
        CurrentSlot slot _onset -> do
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
    (before, expired, charges, blocked) <- atomically $ do
      before <- readTVar (R.runtimeFacts lfcRuntime)
      let (expired, charges) = R.expireHeadsObserved iterationStart (`workStillNeeded` outstanding) before
      writeTVar (R.runtimeFacts lfcRuntime) expired
      blocked <- A.blockedChanges lfcRuntime
      pure (before, expired, charges, blocked)
    -- Force trace-only comparisons of these immutable snapshots after commit,
    -- before assignment or tracing can delay evaluation and retain the maps.
    let !penalties = A.penaltyChanges charges
        !expiries = A.expiryChanges lfcRuntime iterationStart before expired
        transitions = penalties ++ expiries ++ blocked
    (updated, admitted) <-
      assignRequestsObserved
        lfcRuntime
        iterationStart
        forecastEbBounds
        mbHorizon
        times
        offerings
        outstanding
    -- Each producer has already forced its captured observations; this only
    -- joins the detached batches, without another per-event traversal.
    pure (updated, transitions ++ admitted)
  A.emitObservations (lfcTracer lfkContext) (lfcDetailTracer lfkContext) reports
 where
  MkLeiosFetchKernel{lfkContext, lfkPeers} = kernel
  MkLeiosFetchClientContext{lfcOutstanding, lfcReady, lfcRuntime} = lfkContext

-- | Move each election's focus onto the endorser block a verified certificate
-- certifies, as ChainSel verifies such certificates.
--
-- This is how a node that only saw some other announcement for the election,
-- or none, because the announcing block has been on its selection since before
-- it started, comes to fetch the certified one. Each iteration focuses the
-- elections certified since the one before, so a certificate verified after
-- the offer arrived still moves the focus.
--
leiosFetchCertificateFocuser ::
  IOLike m =>
  -- | The claims of the new Leios certifications validated since this last
  -- returned, each with the announcement it certifies, blocking until there is
  -- one
  m (NonEmpty (LeiosClaim, LeiosAnnouncement)) ->
  LeiosFetchKernel m peer blk ->
  m Void
leiosFetchCertificateFocuser consumeValidClaims kernel = forever $ do
  validClaims <- consumeValidClaims
  modifyMVar_ lfcOutstanding $ \outstanding ->
    pure $ F.foldl' focus outstanding validClaims
  atomically $ W.notifyReadiness lfcReady
 where
  MkLeiosFetchClientContext{lfcOutstanding, lfcReady} = lfkContext kernel

  focus outstanding (claim, ann) =
    focusCertifiedEb
      (ElectionId (leiosClaimSlot claim) (announcementElectee ann))
      (announcementEbHash ann)
      outstanding

-- | Prune fetch state and the cache to the advancing immutable tip. The
-- node's shared immutable-tip worker calls this after pruning LeiosNotify.
-- Offer times, completed forge work and the planner wakeup follow the same
-- boundary. The storage-promotion wrapper independently fences cache locations
-- before database promotion; this node worker is not a database GC barrier.
-- See @docs/fetch-policy/leiosfetch-improved.md#immutable-tip-pruning@.
pruneLeiosFetchToImmTip ::
  IOLike m =>
  LeiosFetchKernel m peer blk ->
  SlotNo ->
  m ()
pruneLeiosFetchToImmTip kernel immTipSlot = do
  evictOlderThan lfcTxCache immTipSlot
  modifyMVar_ lfcOutstanding $ pure . pruneOutstandingToImmTip immTipSlot
  -- Offers are keyed by point and claims by election, which both order by
  -- slot first.
  atomically $ do
    writeTVar lfkPrunedSlot immTipSlot
    Owned.pruneCompleted (lfkForged kernel) ((>= immTipSlot) . pointSlotNo)
    peersVars <- readTVar lfkPeers
    forM_ peersVars $ \vars -> do
      modifyTVar (lfpOfferings vars) (Map.dropWhileAntitone ((< immTipSlot) . pointSlotNo))
      modifyTVar (lfpOfferTimes vars) (Map.dropWhileAntitone ((< immTipSlot) . pointSlotNo))
      modifyTVar
        (lfpCertificationClaims vars)
        (Map.dropWhileAntitone ((< immTipSlot) . electionSlot))
    -- Logical pruning and frontier publication can invalidate a deferred
    -- priority barrier even if its physical lease was already removed.
    W.notifyReadiness lfcReady
 where
  MkLeiosFetchKernel{lfkContext, lfkPeers, lfkPrunedSlot} = kernel
  MkLeiosFetchClientContext{lfcOutstanding, lfcReady, lfcTxCache} = lfkContext

-- | First arrivals are metadata about offers, never EB announcement times.
data OfferTimes = OfferTimes
  { bodyOfferTime :: !(Maybe Time)
  , closureOfferTime :: !(Maybe Time)
  }
  deriving (Eq, Show)

-- | Merge first observations independently; repeated/cross-protocol offers do
-- not renew priority. Taking the minimum also handles observation/STM reordering.
noteOfferTime :: Time -> OfferedHalves -> Maybe OfferTimes -> OfferTimes
noteOfferTime now offered previous = OfferTimes (choose body oldBody) (choose closure oldClosure)
 where
  OfferTimes oldBody oldClosure = maybe (OfferTimes Nothing Nothing) id previous
  body = case offered of OfferedClosure -> False; _ -> True
  closure = case offered of OfferedBody{} -> False; _ -> True
  choose False old = old
  choose True Nothing = Just $! now
  choose True (Just old) = Just $! min old now
