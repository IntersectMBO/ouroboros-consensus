{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | What one LeiosNotify client remembers about its upstream peer, and how it
-- judges that peer's replies against it.
module Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.State
  ( ExnLeiosNotifyInvalidAnnouncement (..)
  , ExnLeiosNotifyInvalidVotes (..)
  , AcceptedVote (..)
  , checkLeiosVotes
  , voteLateGrace
  , ExnLeiosNotifyInvalidOffer (..)
  , checkLeiosBlockOffer
  , checkLeiosClosureOffer
  , LeiosNotifyPeerState (..)
  , emptyLeiosNotifyPeerState
  , onLeiosNotifyAnnouncement
  , pruneLeiosNotifyPeerState
  ) where

import Cardano.Crypto.Leios (LeiosCommittee, LeiosSeatId)
import Control.Monad (foldM, when)
import Control.Monad.Except (Except, ExceptT, liftEither, throwError, withExcept, withExceptT)
import Control.Monad.Trans (lift)
import Data.Bifunctor (first)
import qualified Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock (NominalDiffTime)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block
  ( BlockProtocol
  , ConvertRawHash (..)
  , GetHeader (..)
  , Header
  , Proxy (..)
  , SlotNo (..)
  , WithOrigin (NotOrigin)
  , blockSlot
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime
  , SystemTime
  , addRelTime
  , diffRelTime
  )
import Ouroboros.Consensus.Config (TopLevelConfig, configLedger)
import Ouroboros.Consensus.Forecast (OutsideForecastRange, forecastFor)
import Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory (..))
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.HeaderValidation (OtherHeaderEnvelopeError)
import Ouroboros.Consensus.Ledger.Abstract (EmptyMK, getTipSlot)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState, ledgerState)
import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol, ledgerViewForecastAt)
import Ouroboros.Consensus.Leios.AnnounceState (AnnounceState, onValidAnnouncement)
import Ouroboros.Consensus.Leios.Protocol
  ( LeiosAnnouncement (..)
  , LeiosLedgerView (..)
  , LeiosProtocol (..)
  )
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , LeiosPoint (..)
  , LeiosVote (..)
  , RbHash (..)
  , milliseconds32ToNominalDiffTime
  )
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck as InFutureCheck
import qualified Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements as Announcements
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( AnnouncementInvalidity
  , AnnouncingHeader (ancAnnouncement, ancElectionId)
  , announcementValidity
  , announcingRbHash
  , mkAnnouncingHeader
  )
import Ouroboros.Consensus.Protocol.Abstract (ValidationErr)
import Ouroboros.Consensus.Util.IOLike (Exception, IOLike, atomically)

-- | What one LeiosNotify client remembers about its upstream peer.
--
-- Only an announcement adds to it, and every entry is for an election at or
-- after 'lnpsPruneSlot'; the node prunes it as its immutable tip advances.
data LeiosNotifyPeerState blk = MkLeiosNotifyPeerState
  { lnpsPruneSlot :: !SlotNo
  -- ^ The slot the state has been pruned up to.
  , lnpsAnnouncements :: !(Announcements.State (AnnouncingHeader blk))
  -- ^ What this peer has announced, for dedup and equivocation counting.
  , lnpsBodyOffers :: !(Set LeiosPoint)
  -- ^ Which endorser blocks this peer has offered the body of, so that
  -- repeating an offer costs it the connection.
  , lnpsClosureOffers :: !(Set LeiosPoint)
  -- ^ Likewise, which it has offered the transaction closure of.
  , lnpsAnnouncedRbHashes :: !(Map RbHash SlotNo)
  -- ^ The announcing blocks this peer has sent, with their election slots, so
  -- that its votes can be held to them.
  , lnpsVotes :: !(Set (SlotNo, LeiosSeatId))
  -- ^ The slots and seats this peer has sent votes for.
  }
  deriving stock Generic

deriving anyclass instance NoThunks (Header blk) => NoThunks (LeiosNotifyPeerState blk)

emptyLeiosNotifyPeerState :: LeiosNotifyPeerState blk
emptyLeiosNotifyPeerState =
  MkLeiosNotifyPeerState
    { lnpsPruneSlot = SlotNo 0
    , lnpsAnnouncements = Announcements.empty
    , lnpsBodyOffers = Set.empty
    , lnpsClosureOffers = Set.empty
    , lnpsAnnouncedRbHashes = Map.empty
    , lnpsVotes = Set.empty
    }

-- | Forget everything about elections before the given slot, which is the
-- immutable tip's.
pruneLeiosNotifyPeerState ::
  forall blk.
  (ConvertRawHash blk, GetHeader blk) =>
  SlotNo ->
  LeiosNotifyPeerState blk ->
  LeiosNotifyPeerState blk
pruneLeiosNotifyPeerState immTipSlot st
  | lnpsPruneSlot st < immTipSlot =
      MkLeiosNotifyPeerState
        { lnpsPruneSlot = immTipSlot
        , lnpsAnnouncements = announcements
        , lnpsBodyOffers =
            -- 'LeiosPoint' orders slot-first, so the pruned points are a prefix.
            Set.dropWhileAntitone (\p -> pointSlotNo p < immTipSlot) (lnpsBodyOffers st)
        , lnpsClosureOffers =
            Set.dropWhileAntitone (\p -> pointSlotNo p < immTipSlot) (lnpsClosureOffers st)
        , lnpsAnnouncedRbHashes =
            Foldable.foldl'
              (\m anc -> Map.delete (announcingRbHash anc) m)
              (lnpsAnnouncedRbHashes st)
              prunedAnnouncements
        , lnpsVotes = Set.dropWhileAntitone (\(slot, _seat) -> slot < immTipSlot) (lnpsVotes st)
        }
  | otherwise = st
 where
  (prunedAnnouncements, announcements) =
    Announcements.prune immTipSlot (lnpsAnnouncements st)

-- | Thrown when a peer's offer over LeiosNotify is one an honest peer would not
-- send; the ensuing thread death disconnects it.
--
-- Without these checks, peers could send bogus offers, and there are infinitely
-- many of those.
data ExnLeiosNotifyInvalidOffer
  = -- | A body offer of an endorser block this peer never announced.
    ExnLeiosBlockOfferWithoutAnnouncement
      !LeiosPoint
      -- | the size it claimed
      !BytesSize
  | -- | A body offer claiming no bytes, or more bytes than the ledger lets an
    -- endorser block in that slot have.
    ExnLeiosBlockOfferBadSize
      !LeiosPoint
      -- | the size it claimed
      !BytesSize
      -- | the most the ledger allows
      !BytesSize
  | -- | A second body offer of an endorser block.
    ExnLeiosRepeatedBlockOffer !LeiosPoint
  | -- | A closure offer of an endorser block this peer never announced.
    ExnLeiosClosureOfferWithoutAnnouncement !LeiosPoint
  | -- | A second closure offer of an endorser block.
    ExnLeiosRepeatedClosureOffer !LeiosPoint
  | -- | An offer below the slot this peer's state has been pruned to, which is
    -- therefore unanswerable on its own terms.
    --
    -- TODO When the honest server offers endorser blocks, explain why it never
    -- trips this, eg because it offers one only while it is at least a minimum
    -- lead younger than its own immutable tip.
    ExnLeiosOfferTooOld
      !LeiosPoint
      -- | the slot this peer's state has been pruned to
      !SlotNo
  | -- | A body offer whose slot the immutable tip cannot forecast to. Its
    -- announcement was validated by such a forecast, from an older immutable
    -- tip, so this should never happen.
    ExnLeiosBlockOfferOutsideHorizon !LeiosPoint !OutsideForecastRange
  deriving Show

instance Exception ExnLeiosNotifyInvalidOffer

-- | Accept a peer's 'MsgLeiosBlockOffer', or say why the peer must go.
--
-- A peer may offer only an endorser block it has itself announced. That is
-- what stops an offer from being an independent way to make us track an
-- endorser block, outside the two-per-election cap on announcements.
--
-- Only the point is held to the announcements; the size is free, within the
-- ledger's bound. An offer claims to hold a body, and whoever holds a body
-- knows its size, so a peer may well state a size that no announcement we
-- received claimed.
checkLeiosBlockOffer ::
  forall blk.
  (LedgerSupportsProtocol blk, LeiosProtocol (BlockProtocol blk)) =>
  TopLevelConfig blk ->
  -- | the immutable tip's ledger state
  ExtLedgerState blk EmptyMK ->
  LeiosPoint ->
  BytesSize ->
  LeiosNotifyPeerState blk ->
  Except ExnLeiosNotifyInvalidOffer (LeiosNotifyPeerState blk)
checkLeiosBlockOffer cfg immLedger point claimed st
  | pointSlotNo point < lnpsPruneSlot st =
      throwError $ ExnLeiosOfferTooOld point (lnpsPruneSlot st)
  | Set.member point (lnpsBodyOffers st) =
      throwError $ ExnLeiosRepeatedBlockOffer point
  | not (announcedIt point st) =
      throwError $ ExnLeiosBlockOfferWithoutAnnouncement point claimed
  | otherwise = do
      view <-
        withExcept (ExnLeiosBlockOfferOutsideHorizon point) $
          forecastFor
            (ledgerViewForecastAt (configLedger cfg) (ledgerState immLedger))
            (pointSlotNo point)
      let bound = maybe 0 llvMaxEbBodySize $ getLeiosLedgerView (Proxy @(BlockProtocol blk)) view
      if claimed == 0 || claimed > bound
        then throwError $ ExnLeiosBlockOfferBadSize point claimed bound
        else pure st{lnpsBodyOffers = Set.insert point (lnpsBodyOffers st)}

-- | Accept a peer's 'MsgLeiosBlockTxsOffer', or say why the peer must go.
--
-- Held to the same announcement as the body offer and otherwise independent of
-- it: either may arrive first, or alone. So a LeiosNotify server never has to
-- track what it has already offered a peer in order to synthesise a body offer
-- ahead of a closure offer.
checkLeiosClosureOffer ::
  LeiosPoint ->
  LeiosNotifyPeerState blk ->
  Except ExnLeiosNotifyInvalidOffer (LeiosNotifyPeerState blk)
checkLeiosClosureOffer point st
  | pointSlotNo point < lnpsPruneSlot st =
      throwError $ ExnLeiosOfferTooOld point (lnpsPruneSlot st)
  | Set.member point (lnpsClosureOffers st) =
      throwError $ ExnLeiosRepeatedClosureOffer point
  | not (announcedIt point st) =
      throwError $ ExnLeiosClosureOfferWithoutAnnouncement point
  | otherwise =
      pure st{lnpsClosureOffers = Set.insert point (lnpsClosureOffers st)}

-- | Whether this peer has announced this endorser block.
announcedIt :: LeiosPoint -> LeiosNotifyPeerState blk -> Bool
announcedIt point st =
  any
    (\ancHdr -> announcementEbHash (ancAnnouncement ancHdr) == pointEbHash point)
    (Announcements.inSlot (pointSlotNo point) (lnpsAnnouncements st))

-- | Thrown when a peer's 'MsgLeiosBlockAnnouncement' is one an honest peer
-- would not send; the ensuing thread death disconnects it.
data ExnLeiosNotifyInvalidAnnouncement blk
  = -- | The header carries no announcement, so it should not have been relayed
    -- as one.
    ExnLeiosNotifyAnnouncementMissing
  | ExnLeiosNotifyInvalidAnnouncement !(Announcements.Error (AnnouncementInvalidity blk))

deriving stock instance
  (Show (ValidationErr (BlockProtocol blk)), Show (OtherHeaderEnvelopeError blk)) =>
  Show (ExnLeiosNotifyInvalidAnnouncement blk)

instance
  ( Typeable blk
  , Show (ValidationErr (BlockProtocol blk))
  , Show (OtherHeaderEnvelopeError blk)
  ) =>
  Exception (ExnLeiosNotifyInvalidAnnouncement blk)

-- | Accept a peer's 'MsgLeiosBlockAnnouncement', or say why the peer must go.
--
-- An accepted announcement goes on to the node's 'AnnounceState'.
onLeiosNotifyAnnouncement ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , ConvertRawHash blk
  , LeiosProtocol (BlockProtocol blk)
  ) =>
  SystemTime m ->
  InFutureCheck.SomeHeaderInFutureCheck m blk ->
  TopLevelConfig blk ->
  AnnounceState m blk ->
  -- | the immutable tip's ledger state
  ExtLedgerState blk EmptyMK ->
  LeiosNotifyPeerState blk ->
  Header blk ->
  ExceptT (ExnLeiosNotifyInvalidAnnouncement blk) m (LeiosNotifyPeerState blk)
onLeiosNotifyAnnouncement systemTime futureCheck cfg announceState immLedger st hdr = do
  anc <- maybe (throwError ExnLeiosNotifyAnnouncementMissing) pure $ mkAnnouncingHeader cfg hdr
  withExceptT ExnLeiosNotifyInvalidAnnouncement $ do
    announcements <-
      liftEither $ Announcements.insert (ancElectionId anc) anc (lnpsAnnouncements st)
    -- The expensive validation comes after the trivial counting checks. Even a
    -- 'StaleOCIN' announcement counts against the peer.
    valid <- announcementValidity systemTime futureCheck cfg immLedger anc
    lift $ atomically $ onValidAnnouncement announceState valid anc
    pure
      st
        { lnpsAnnouncements = announcements
        , lnpsAnnouncedRbHashes =
            Map.insert (announcingRbHash anc) (blockSlot hdr) (lnpsAnnouncedRbHashes st)
        }

-- | Thrown when a peer's 'MsgLeiosVotes' holds a vote an honest peer would not
-- send; the ensuing thread death disconnects it.
data ExnLeiosNotifyInvalidVotes
  = -- | A vote for an announcing block this peer never sent us. This also rules
    -- out a vote in an empty slot.
    ExnLeiosVoteWithoutAnnouncement !RbHash
  | -- | A second vote from one seat in one slot.
    ExnLeiosRepeatedVote
      -- | the election slot
      !SlotNo
      -- | the seat
      !LeiosSeatId
  | -- | A vote too young to have been cast. Votes are cast 3 L_hdr after the
    -- slot began, the time equivocations take to come to light, widened by the
    -- node's maximum clock skew so that an honest peer is never disconnected.
    ExnLeiosVoteTooEarly
      -- | the announcing block
      !RbHash
      -- | the election slot
      !SlotNo
      -- | how long after the election slot began the vote arrived
      !NominalDiffTime
  | -- | A vote too old to still be relayed. Votes stop being relayed 3 L_hdr +
    -- L_vote after the slot began, widened by 'voteLateGrace' so that an
    -- honest peer is never disconnected. A vote for a slot behind the immutable
    -- tip is among these, long past any window.
    ExnLeiosVoteTooLate
      -- | the announcing block
      !RbHash
      -- | the election slot
      !SlotNo
      -- | how long after the election slot began the vote arrived
      !NominalDiffTime
  | -- | A vote whose election slot the immutable tip cannot forecast to. Its
    -- announcement was validated by such a forecast, from an older immutable
    -- tip, so this should never happen.
    ExnLeiosVoteOutsideHorizon
      -- | the announcing block
      !RbHash
      -- | the election slot
      !SlotNo
  deriving Show

instance Exception ExnLeiosNotifyInvalidVotes

-- | A vote a client accepted from its peer, with what the node's 'VoteState'
-- needs to judge and relay it.
data AcceptedVote = MkAcceptedVote
  { acceptedVote :: !LeiosVote
  , voteElectionSlot :: !SlotNo
  , voteWindowEnd :: !RelativeTime
  -- ^ 3 L_hdr + L_vote after the election slot began, when the vote is no
  -- longer relayed.
  , voteCommittee :: !LeiosCommittee
  -- ^ The committee of the election slot's ledger view.
  }

-- | Accept a peer's 'MsgLeiosVotes', or say why the peer must go; see
-- 'ExnLeiosNotifyInvalidVotes'.
--
-- This does not check a vote's seat or signature. The node's 'VoteState' will,
-- once per vote, so a badly signed copy of a vote the node already has costs it
-- only the bytes.
checkLeiosVotes ::
  forall blk.
  ( LedgerSupportsProtocol blk
  , HasHardForkHistory blk
  , LeiosProtocol (BlockProtocol blk)
  ) =>
  TopLevelConfig blk ->
  -- | the most that an honest peer's clock may differ from ours
  InFutureCheck.ClockSkew ->
  -- | the immutable tip's ledger state
  ExtLedgerState blk EmptyMK ->
  -- | now
  RelativeTime ->
  NonEmpty LeiosVote ->
  LeiosNotifyPeerState blk ->
  Except ExnLeiosNotifyInvalidVotes (LeiosNotifyPeerState blk, [AcceptedVote])
checkLeiosVotes cfg skew immLedger now votes st0 =
  fmap reverse <$> foldM checkOne (st0, []) votes
 where
  checkOne (st, accepted) vote = do
    slot <-
      maybe (throwError (ExnLeiosVoteWithoutAnnouncement rbHash)) pure $
        Map.lookup rbHash (lnpsAnnouncedRbHashes st)
    when (Set.member (slot, seat) (lnpsVotes st)) $
      throwError (ExnLeiosRepeatedVote slot seat)
    (onset, _slotLength) <-
      liftEither . first (const (ExnLeiosVoteOutsideHorizon rbHash slot)) $
        HardFork.runQuery
          (HardFork.slotToWallclock slot)
          (hardForkSummary (configLedger cfg) (ledgerState immLedger))
    let age = diffRelTime now onset
    -- 'forecastFor' cannot judge a slot behind its anchor, the immutable tip.
    when (NotOrigin slot < getTipSlot (ledgerState immLedger)) $
      throwError (ExnLeiosVoteTooLate rbHash slot age)
    view <-
      withExcept (const (ExnLeiosVoteOutsideHorizon rbHash slot)) $
        forecastFor
          (ledgerViewForecastAt (configLedger cfg) (ledgerState immLedger))
          slot
    -- Without Leios at this slot, the protocol announces nothing there, so no
    -- vote in it can have an announcement.
    MkLeiosLedgerView{llvAnnouncementPeriod, llvVotePeriod, llvCommittee} <-
      maybe (throwError (ExnLeiosVoteWithoutAnnouncement rbHash)) pure $
        getLeiosLedgerView (Proxy @(BlockProtocol blk)) view
    let lHdr = milliseconds32ToNominalDiffTime llvAnnouncementPeriod
        lVote = milliseconds32ToNominalDiffTime llvVotePeriod
    when (age < 3 * lHdr - InFutureCheck.unClockSkew skew) $
      throwError (ExnLeiosVoteTooEarly rbHash slot age)
    when (age > 3 * lHdr + lVote + voteLateGrace) $
      throwError (ExnLeiosVoteTooLate rbHash slot age)
    pure
      ( st{lnpsVotes = Set.insert (slot, seat) (lnpsVotes st)}
      , MkAcceptedVote vote slot (addRelTime (3 * lHdr + lVote) onset) llvCommittee : accepted
      )
   where
    rbHash = voteAnnouncingRbHash vote
    seat = voteSeatId vote

-- | How much later than 3 L_hdr + L_vote after its election slot began a vote
-- may arrive: enough for transmission, clock skew, and votes queued behind
-- other traffic.
--
-- TODO magic number; measure buffer bloat on a loaded network.
voteLateGrace :: NominalDiffTime
voteLateGrace = 60
