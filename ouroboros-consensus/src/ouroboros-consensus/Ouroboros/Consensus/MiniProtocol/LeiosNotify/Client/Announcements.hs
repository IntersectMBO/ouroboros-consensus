{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
--
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
--
-- TO BE IMPORTED QUALIFIED
module Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements
  ( module Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements
  ) where

import Cardano.Slotting.Slot (SlotNo)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Trans (lift)
import Control.Tracer (Tracer, traceWith)
import Data.Functor.Compose (Compose (..))
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict as Strict (Map)
import Ouroboros.Consensus.Leios.Types (ElId (..))

-----
-- The state of each LeiosNotify client
-----

-- | What a single LeiosNotify upstream peer has announced, per election: the
-- announcements part of the client's per-peer state
data AnnouncementsState anc
  = MkAnnouncementsState
  { live :: !(Strict.Map ElId (ElState anc))
  }

emptyAnnouncementsState :: AnnouncementsState anc
emptyAnnouncementsState = MkAnnouncementsState Map.empty

-- | State maintained within 'AnnouncementsState' for each election
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

-- | Both of this election's announcements, first one first.
elStateAnnouncements :: ElState anc -> [anc]
elStateAnnouncements = \case
  OneAnnouncement x -> [x]
  TwoAnnouncements x1 x2 -> [x1, x2]

-- | Everything this peer has announced for elections in the given slot.
--
-- 'ElId' orders slot-first, so those elections are a contiguous range. An
-- endorser block's own slot is its election's slot, so this is what an offer
-- of that endorser block has to be backed by.
announcementsInSlot :: SlotNo -> AnnouncementsState anc -> [anc]
announcementsInSlot slot st =
  concatMap elStateAnnouncements (Map.elems atSlot)
 where
  (_below, atOrAbove) =
    Map.spanAntitone (\(MkElId elSlot _poolId) -> elSlot < slot) (live st)
  (atSlot, _above) =
    Map.spanAntitone (\(MkElId elSlot _poolId) -> elSlot <= slot) atOrAbove

-- | Called whenever the ChainDB's immutable tip advances to a new slot
--
-- NOTE this pruning should happen ~60 s after the immutable tip
-- advances.  That accommodates clock skew, transmission time, etc
-- with only an insignificant increase in the stochastic bound on
-- election count
pruneAnnouncementsState :: SlotNo -> AnnouncementsState anc -> AnnouncementsState anc
pruneAnnouncementsState immTipSlot st =
  MkAnnouncementsState{live = live'}
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
  AnnouncementsState anc ->
  Either (ErrAnnouncement invalidity) (ElState anc, AnnouncementsState anc)
extendLive elId anc st =
  getCompose $
    fmap
      (\live' -> MkAnnouncementsState{live = live'})
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
    -- 'Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Validate.validateAnnouncementHeader').
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
  AnnouncementsState anc ->
  anc ->
  ExceptT (ErrAnnouncement invalidity) m (AnnouncementsState anc)
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
