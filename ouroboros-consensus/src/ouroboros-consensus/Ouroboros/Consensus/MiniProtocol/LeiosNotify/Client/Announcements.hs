{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict as Strict (Map)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Leios.Types (ElectionId (..))

-----
-- The state of each LeiosNotify client
-----

-- | What a single LeiosNotify upstream peer has announced, per election: the
-- announcements part of the client's per-peer state
newtype State anc = MkState {live :: Strict.Map ElectionId (SeenAnnouncements anc)}
  deriving stock Generic
  deriving anyclass NoThunks

empty :: State anc
empty = MkState Map.empty

-- | The announcements the peer has sent for one election
data SeenAnnouncements anc
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
  deriving stock (Generic, Foldable)
  deriving anyclass NoThunks

-- | Everything this peer has announced for elections in the given slot.
--
-- 'ElectionId' orders slot-first, so those elections are a contiguous range. An
-- endorser block's own slot is its election's slot, so this is what an offer
-- of that endorser block has to be backed by.
inSlot :: SlotNo -> State anc -> [anc]
inSlot slot st =
  concatMap toList (Map.elems atSlot)
 where
  (_below, atOrAbove) =
    Map.spanAntitone (\(ElectionId elSlot _poolId) -> elSlot < slot) (live st)
  (atSlot, _above) =
    Map.spanAntitone (\(ElectionId elSlot _poolId) -> elSlot <= slot) atOrAbove

-- | Called whenever the ChainDB's immutable tip advances to a new slot. Also
-- returns the announcements it forgets.
--
-- NOTE this pruning should happen ~60 s after the immutable tip
-- advances.  That accommodates clock skew, transmission time, etc
-- with only an insignificant increase in the stochastic bound on
-- election count
prune :: SlotNo -> State anc -> ([anc], State anc)
prune immTipSlot st =
  (concatMap toList (Map.elems pruned), MkState live')
 where
  (pruned, live') = Map.spanAntitone tooOld (live st)

  -- NB strict comparison, so that the immtip's own announcement is
  -- not pruned
  tooOld (ElectionId elSlot _poolId) = elSlot < immTipSlot

-- | Behaviors of a LeiosNotify upstream peer's announcement stream
-- that an honest node rejects
data Error invalidity
  = -- | The peer had already sent this same announcement before
    ErrRepeat
  | -- | The peer had already sent two announcements for this election
    ErrThird
  | -- | This announcement is invalid
    ErrInvalid !invalidity
  | -- | This announcement's election is too old (its wall-clock age exceeds
    -- what a client tolerates from an upstream peer). Unlike 'ErrInvalid', this
    -- verdict depends on the wall clock rather than on the announcement itself.
    ErrTooOld
  deriving Show

-- | Fails if this election was already 'TwoAnnouncements' or if this
-- announcement was already received
insert ::
  forall anc invalidity.
  Eq anc =>
  ElectionId ->
  anc ->
  State anc ->
  Either (Error invalidity) (State anc)
insert elId anc st =
  MkState <$> Map.alterF (fmap Just . upd) elId (live st)
 where
  -- 0 -> 1 ok
  -- 1 -> 2 ok when unequal
  -- 2 -> 3 not ok
  upd :: Maybe (SeenAnnouncements anc) -> Either (Error invalidity) (SeenAnnouncements anc)
  upd = \case
    Nothing -> Right $ OneAnnouncement anc
    Just (OneAnnouncement x) ->
      if x == anc
        then Left ErrRepeat
        else
          Right $ TwoAnnouncements x anc
    Just TwoAnnouncements{} -> Left ErrThird
