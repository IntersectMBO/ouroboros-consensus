{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
--
-- TO BE IMPORTED QUALIFIED
--
-- The Leios node must relay EbAnnouncements promptly.
-- CIP-0164 is designed around the idea that an EbAnnouncement at an honest stake pool will have propagated to all other honest stake pools within L_hdr, and suggests a value of 1 second.
-- On a network that spans the globe, that's a very tight window.
-- But each EbAnnouncement is slightly less than 1 kB, so it does seem plausible.
--
-- In the current design of CIP-0164, the existing Praos header (aka RbHeader) was extended with new fields.
-- One of those fields is an optional tuple: EbBody hash and EbBody size.
-- That's the announcement.
-- As part of the RbHeader, it is adjacent to the election proof that justifies the EbAnnouncement and it is signed by the electee.
-- Because that election proof necessarily includes the slot of the election, every EbAnnouncement has an age.
-- In particular, that age is used to evict too-old announcements so that the existing stochastic bound on the number of elections younger than some age also bounds the number of in-memory EbAnnouncements: zero (no announcement), one (unequivocal announcement), or two (equivocal announcements) per sufficiently-young election.
--
-- The LeiosNotify mini protocol in CIP-0164 already includes a notification MsgLeiosAnnouncement, whose payload is the RbHeader.
-- It is sent as one of the possible responses to the generic MsgLeiosNotificationRequestNext message.
-- The healthy honest node tries to maintain ~hundreds of those requests outstanding at all times, so that the upstream peer can always enqueue a notification immediately.
-- The intended timeline one a single connection from an honest node X to an honest node Y is as follows.
--
-- - X either receives or itself issues a new valid EbAnnouncement.
-- - If that's the first or second announcement X has seen for that election, then X tries to relay that announcement to its downstream peers, including Y.
-- - X will be able to send to Y, because X should have received at least one more MsgLeiosNotificationRequestNext message from Y than X has already replied to.
-- - When Y receives the announcement from X, it confirms that X hasn't already sent this announcement or any two distinct (equivocating) announcements for that same election, disconnecting if it has.
-- - Y also needs to validate that the announcement is well-signed and that its election proof is valid, disconnecting if either is invalid.
--
-- TO BE IMPORTED QUALIFIED
module LeiosDemoLogic.Announcements (module LeiosDemoLogic.Announcements) where

import Cardano.Slotting.Slot (SlotNo)
import Control.Concurrent.Class.MonadSTM (MonadSTM, atomically)
import Control.Concurrent.Class.MonadSTM.Strict.TVar (StrictTVar, readTVar, writeTVar)
import Control.Monad (foldM, void)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Trans (lift)
import Control.Tracer (Tracer, traceWith)
import Data.Functor.Compose (Compose (..))
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict as Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock (NominalDiffTime)
import LeiosDemoLogic.Announcements.ElBimap

-----
-- The state of each LeiosNotify client
-----

-- | State maintained for a single LeiosNotify upstream peer
data PeerState anc
  = MkPeerState
  { live :: !(Strict.Map ElId (ElState anc))
  }

emptyPeerState :: PeerState anc
emptyPeerState = MkPeerState Map.empty

-- | State maintained within 'PeerState' for each election
data ElState anc
  = -- | The peer has sent one announcement
    --
    -- INVARIANT: has valid signature and election proof
    OneAnnouncement !anc
  | -- | The peer has sent two announcements
    --
    -- INVARIANT: the left announcement was received first
    --
    -- INVARIANT: both have valid signature and election proof
    --
    -- INVARIANT: they are different announcements
    TwoAnnouncements !anc !anc

firstAnnouncement :: ElState anc -> anc
firstAnnouncement = \case
  OneAnnouncement x -> x
  TwoAnnouncements x1 _x2 -> x1

secondAnnouncement :: ElState anc -> Maybe anc
secondAnnouncement = \case
  OneAnnouncement _x -> Nothing
  TwoAnnouncements _x1 x2 -> Just x2

-- | Called whenever the ChainDB's immutable tip advances to a new slot
--
-- NOTE this pruning should happen ~60 s after the immutable tip
-- advances.  That accommodates clock skew, transmission time, etc
-- with only an insignificant increase in the stochastic bound on
-- election count
prunePeerState :: SlotNo -> PeerState anc -> PeerState anc
prunePeerState immTipSlot st =
  MkPeerState{live = live'}
 where
  (_pruned, live') = Map.spanAntitone tooOld (live st)

  -- NB strict comparison, so that the immtip's own announcement is
  -- not pruned
  tooOld (MkElId elSlot _poolId) = elSlot < immTipSlot

-- | Behaviors of a LeiosNotify upstream peer's announcement stream
-- that an honest node rejects
data ErrAnnouncement invalidity
  = -- | The peer had already sent this same announcement before
    ErrRepeat
  | -- | The peer had already sent two announcements for this election
    ErrThird
  | -- | This announcement is invalid
    ErrInvalid !invalidity
  | -- | This announcement's election is too old (its wall-clock age exceeds
    -- what a client tolerates from an upstream peer). Unlike 'ErrInvalid', this
    -- verdict depends on the wall clock rather than on the announcement itself,
    -- so 'onAnnouncement' cannot decide it directly; it is signalled by the
    -- validation callback returning 'VerdictTooOld' (see 'onAnnouncement').
    ErrTooOld
  deriving Show

-- | Returns 'Nothing' if this election was already 'TwoAnnouncements' or if this
-- announcement was already received
extendLive ::
  forall anc invalidity.
  Eq anc =>
  ElId ->
  anc ->
  PeerState anc ->
  Either (ErrAnnouncement invalidity) (ElState anc, PeerState anc)
extendLive elId anc st =
  getCompose $
    fmap
      (\live' -> MkPeerState{live = live'})
      (Map.alterF (inj . upd) elId (live st))
 where
  -- 0 -> 1 ok
  -- 1 -> 2 ok when unequal
  -- 2 -> 3 not ok
  upd :: Maybe (ElState anc) -> Either (ErrAnnouncement x) (ElState anc)
  upd = \case
    Nothing -> Right $ OneAnnouncement anc
    Just (OneAnnouncement x) ->
      if x == anc
        then Left ErrRepeat
        else
          Right $ TwoAnnouncements x anc
    Just TwoAnnouncements{} -> Left ErrThird

  inj :: Functor f => f a -> Compose f ((,) a) (Maybe a)
  inj = Compose . fmap (\x -> (x, Just x)) -- success is never a delete

-----
-- The per-peer logic for receiving a MsgLeiosAnnouncement
-----

data TraceLeiosNotifyPeerEvent anc
  = TracePeerAnnouncement !(ElState anc)

-- | The verdict of 'onAnnouncement'\'s validation callback
data AnnouncementVerdict invalidity validated
  = -- | Reject: the election's wall-clock age exceeds what a client tolerates
    --
    -- Raised as 'ErrTooOld'. The callback owns this check because only it holds
    -- the wall clock.
    VerdictTooOld
  | -- | Reject: the announcement is invalid; raised as 'ErrInvalid'
    VerdictInvalid !invalidity
  | -- | Accept from the peer, but neither process nor relay the announcement
    --
    -- Currently only a stale-OCIN announcement (see
    -- 'LeiosDemoLogic.Announcements.Validate.validateAnnouncementHeader').
    VerdictIgnore
  | -- | Accept, process, and relay
    VerdictProcess !validated

onAnnouncement ::
  (Eq anc, Monad m) =>
  Tracer m (TraceLeiosNotifyPeerEvent anc) ->
  (anc -> ElId) ->
  -- | How to validate the announcement
  --
  -- The peer is disconnected only for a rejecting verdict; both accepting
  -- verdicts also update its per-peer dedup state.
  (anc -> m (AnnouncementVerdict invalidity validated)) ->
  -- | How this central logic should react to a new announcement from
  -- this peer
  --
  -- ASSUMPTION: this is often a no-op (except maybe tracing), because
  -- the same announcement has already been received from other peers.
  (anc -> validated -> m ()) ->
  PeerState anc ->
  anc ->
  ExceptT (ErrAnnouncement invalidity) m (PeerState anc)
onAnnouncement tracer getEl validate process st anc = do
  (elSt, st') <- case extendLive (getEl anc) anc st of
    Left err -> throwError err
    Right x -> pure x
  lift $ traceWith tracer $ TracePeerAnnouncement elSt
  -- do the more expensive validation only after the trivial
  -- counting checks
  lift (validate anc) >>= \case
    VerdictTooOld -> throwError ErrTooOld
    VerdictInvalid err -> throwError $ ErrInvalid err
    -- The peer's dedup state was already updated by 'extendLive' above, so just
    -- keep it.
    VerdictIgnore -> pure st'
    VerdictProcess x -> do
      lift $ process anc x
      pure st'

-----
-- The central logic for receiving a MsgLeiosAnnouncement
-----

-- | State maintained for a node's own LeiosNotify behaviors
data CentralState m peer anc
  = MkCentralState
  { selfPeer :: !(PeerState anc)
  -- ^ An isomorph of `PeerState` that models the node itself as
  -- an upstream peer of its downstream peers. This allows it to
  -- ignore an announcement exactly when sending it would cause
  -- the recipient's 'extendLive' to raise an error.
  , queues :: !(Strict.Map peer (QueueAnnouncementView m anc))
  , gate :: !(ElBimap peer)
  -- ^ Which peers we have already sent announcements for this
  -- election
  --
  -- We only send equivocation proofs to them.
  --
  -- We don't need to track whether or not we've sent them an
  -- equivocation.
  }

emptyCentralState :: CentralState m peer anc
emptyCentralState = MkCentralState emptyPeerState Map.empty emptyElBimap

-- | A downstream peer's send queue and its count of available credits
data QueueAnnouncementView m anc
  = forall q.
    MkQueueAnnouncementView
      !(StrictTVar m Int)
      !(q -> anc -> q)
      !(StrictTVar m q)

data TraceLeiosNotifyEvent peer anc
  = -- | The final field is how late the announcement was — seconds from the
    -- election slot's wall-clock onset to when this node counted it — when
    -- known (a relayed announcement has it; a locally-forged one does not).
    TraceNewAnnouncement !(Maybe peer) !ElId !(ElState anc) !(Maybe NominalDiffTime)

-- | Called whenever the ChainDB's immutable tip advances to a new slot
--
-- Unlike 'prunePeerState', a delay is undesirable here.
pruneCentralState ::
  Ord peer => SlotNo -> CentralState m peer anc -> CentralState m peer anc
pruneCentralState immTipSlot st =
  MkCentralState
    { selfPeer = prunePeerState immTipSlot (selfPeer st) -- NB no delay
    , queues = queues st
    , gate = pruneElBimap immTipSlot (gate st)
    }

insertPeerCentral ::
  Ord peer =>
  peer ->
  QueueAnnouncementView m anc ->
  CentralState m peer anc ->
  CentralState m peer anc
insertPeerCentral peer qav st =
  MkCentralState
    { selfPeer = selfPeer st
    , queues = Map.insert peer qav (queues st)
    , gate = gate st
    }

deletePeerCentral ::
  Ord peer =>
  peer ->
  CentralState m peer anc ->
  CentralState m peer anc
deletePeerCentral peer st =
  MkCentralState
    { selfPeer = selfPeer st
    , queues = Map.delete peer (queues st)
    , gate = deleteElBimapR peer (gate st)
    }

-- | Whether 'onAnnouncementCentral' should relay an announcement downstream.
--
-- An honest node uses 'DoNotRelay' once an announcement's slot is old enough
-- that a peer whose immutable tip has advanced slightly further would
-- disconnect the relayer for relaying a below-its-immutable-tip announcement;
-- the caller decides this from the announcement's wall-clock age. Local
-- processing is unaffected either way.
data ShouldRelay = DoRelay | DoNotRelay
  deriving (Eq, Show)

-- | The nub of the callback argument to 'onAnnouncement'
--
-- NOTE: This is also called by the block forging thread when this node issues
-- its own announcement (with 'Nothing' as the source peer).
--
-- TODO: headers arriving via the ChainSync 'MsgRollForward' carry announcements
-- too, and so should also feed the 'CentralState' (for relay and dedup); they
-- do not yet.
onAnnouncementCentral ::
  forall m peer anc.
  (MonadSTM m, Ord peer, Eq anc) =>
  Tracer m (TraceLeiosNotifyEvent peer anc) ->
  (anc -> ElId) ->
  -- | Notify other components about a /new/ announcement
  --
  -- For example, notify the voting thread; it cares about both new
  -- announcements and also equivocations.
  --
  -- Maybe also update a cache used to dedup header validation
  -- computations. Etc.
  (ElState anc -> m ()) ->
  CentralState m peer anc ->
  -- | The upstream peer the announcement came from, or 'Nothing' if this node
  -- is itself the source (e.g. its own block forging).
  Maybe peer ->
  ShouldRelay ->
  -- | How late the announcement was (see 'TraceNewAnnouncement').
  Maybe NominalDiffTime ->
  anc ->
  m (CentralState m peer anc)
onAnnouncementCentral tracer getEl publishLocally st peer shouldRelay age anc =
  case extendLive el anc (selfPeer st) of
    Left{} -> pure st -- complete noop for duplicates
    Right (elSt, selfPeer') -> do
      traceWith tracer $ TraceNewAnnouncement peer el elSt age
      -- urgently relay
      newPeers <- case shouldRelay of
        DoNotRelay -> pure Set.empty
        DoRelay -> send elSt (queues st)
      -- signal other components
      publishLocally elSt
      pure
        MkCentralState
          { selfPeer = selfPeer'
          , queues = queues st
          , gate = insertElBimapLs el newPeers (gate st)
          }
 where
  el = getEl anc

  -- returns new peers to add to 'gate' for this election
  send :: ElState anc -> Strict.Map peer (QueueAnnouncementView m anc) -> m (Set peer)
  send elSt qs = case elSt of
    OneAnnouncement{} -> foldM enqueue Set.empty (Map.assocs qs)
    TwoAnnouncements{} -> do
      -- Only send equivocation proofs to peers we've already
      -- sent the first announcement to. Otherwise they'd be
      -- interpreting it as /our/ /first/ announcement for that
      -- election. TODO introduce a single message for the pair
      -- OR send both announcements if they currently have at
      -- least two credits.
      --
      -- And 'gate' doesn't change.
      mapM_
        (void . tryEnqueue)
        (Map.restrictKeys qs $ lookupElBimapL el $ gate st)
      pure Set.empty

  enqueue :: Set peer -> (peer, QueueAnnouncementView m anc) -> m (Set peer)
  enqueue !acc (peer', ancQueue) =
    fmap
      (\enqueued -> if not enqueued then acc else Set.insert peer' acc)
      (tryEnqueue ancQueue)

  tryEnqueue :: QueueAnnouncementView m anc -> m Bool
  tryEnqueue (MkQueueAnnouncementView free snoc tvar) = atomically $ do
    n <- readTVar free
    -- just drop it if there are currently no credits
    if n <= 0
      then pure False
      else do
        writeTVar free $! n - 1
        q <- readTVar tvar
        writeTVar tvar $! snoc q anc
        pure True
