{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | What one LeiosNotify client remembers about its upstream peer, and how it
-- judges that peer's replies against it.
module Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.State
  ( ExnLeiosNotifyInvalidAnnouncement (..)
  , ExnLeiosNotifyInvalidVotes (..)
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

import Cardano.Crypto.Leios (LeiosSeatId (..))
import Control.Monad (foldM, when)
import Control.Monad.Except (runExcept, runExceptT)
import Control.Tracer (nullTracer)
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock (NominalDiffTime)
import Data.Typeable (Typeable)
import Data.Word (Word16)
import NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime, SystemTime, diffRelTime)
import Ouroboros.Consensus.Config (TopLevelConfig, configLedger)
import Ouroboros.Consensus.Forecast (OutsideForecastRange, forecastFor)
import Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory (..))
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.Ledger.Abstract (EmptyMK, getTipSlot)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState, ledgerState)
import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol, ledgerViewForecastAt)
import Ouroboros.Consensus.Leios.Protocol
  ( LeiosAnnouncement (..)
  , LeiosLedgerView (..)
  , LeiosProtocol (..)
  )
import Ouroboros.Consensus.Leios.Types (BytesSize, LeiosPoint (..), LeiosVote (..), RbHash (..))
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck as InFutureCheck
import qualified Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements as Announcements
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Validate
import Ouroboros.Consensus.Util.IOLike (Exception, IOLike, throwIO)

-- | What one LeiosNotify client remembers about its upstream peer.
--
-- Only an announcement adds to it, and every entry is for an election at or
-- after 'lnpsPruneSlot'; the node prunes it as its immutable tip advances.
data LeiosNotifyPeerState blk = MkLeiosNotifyPeerState
  { lnpsPruneSlot :: !SlotNo
  -- ^ The slot the state has been pruned up to.
  , lnpsAnnouncements :: !(Announcements.AnnouncementsState (AnnouncingHeader blk))
  -- ^ What this peer has announced, for dedup and equivocation counting.
  , lnpsBodyOffers :: !(Set LeiosPoint)
  -- ^ Which endorser blocks this peer has offered the body of, so that
  -- repeating an offer costs it the connection.
  , lnpsClosureOffers :: !(Set LeiosPoint)
  -- ^ Likewise, which it has offered the transaction closure of.
  , lnpsAnnouncedRbHashes :: !(Map RbHash SlotNo)
  -- ^ The announcing blocks this peer has sent, with their election slots, so
  -- that its votes can be held to them.
  , lnpsVotes :: !(Set (SlotNo, Word16))
  -- ^ The slots and seats this peer has sent votes for.
  }
  deriving
    NoThunks
    via OnlyCheckWhnfNamed "LeiosNotifyPeerState" (LeiosNotifyPeerState blk)

emptyLeiosNotifyPeerState :: LeiosNotifyPeerState blk
emptyLeiosNotifyPeerState =
  MkLeiosNotifyPeerState
    { lnpsPruneSlot = SlotNo 0
    , lnpsAnnouncements = Announcements.emptyAnnouncementsState
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
            foldl'
              (\m anc -> Map.delete (announcingRbHash anc) m)
              (lnpsAnnouncedRbHashes st)
              prunedAnnouncements
        , lnpsVotes = Set.dropWhileAntitone (\(slot, _seat) -> slot < immTipSlot) (lnpsVotes st)
        }
  | otherwise = st
 where
  (prunedAnnouncements, announcements) =
    Announcements.pruneAnnouncementsState immTipSlot (lnpsAnnouncements st)

-- | The hash of the announcing block, which votes name.
announcingRbHash ::
  forall blk. (ConvertRawHash blk, GetHeader blk) => AnnouncingHeader blk -> RbHash
announcingRbHash anc = MkRbHash (toRawHash (Proxy @blk) (headerHash (ancHeader anc)))

-- | Thrown when a peer's offer over LeiosNotify is one an honest peer would not
-- send; the ensuing thread death disconnects it.
--
-- Without these checks, peers could send bogus offers, and there are infinitely
-- many of those.
data ExnLeiosNotifyInvalidOffer
  = -- | A body offer of an endorser block this peer never announced, with the
    -- size it claimed.
    ExnLeiosBlockOfferWithoutAnnouncement !LeiosPoint !BytesSize
  | -- | A body offer claiming no bytes, or more bytes than the ledger lets an
    -- endorser block in that slot have: the offered point, the size it claimed,
    -- and the bound.
    ExnLeiosBlockOfferBadSize !LeiosPoint !BytesSize !BytesSize
  | -- | A second body offer of an endorser block.
    ExnLeiosRepeatedBlockOffer !LeiosPoint
  | -- | A closure offer of an endorser block this peer never announced.
    ExnLeiosClosureOfferWithoutAnnouncement !LeiosPoint
  | -- | A second closure offer of an endorser block.
    ExnLeiosRepeatedClosureOffer !LeiosPoint
  | -- | An offer below the slot this peer's state has been pruned to, which is
    -- therefore unanswerable on its own terms: the offered point, and that slot.
    --
    -- TODO When the honest server offers endorser blocks, explain why it never
    -- trips this, eg because it offers one only while it is at least a minimum
    -- lead younger than its own immutable tip.
    ExnLeiosOfferTooOld !LeiosPoint !SlotNo
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
  Either ExnLeiosNotifyInvalidOffer (LeiosNotifyPeerState blk)
checkLeiosBlockOffer cfg immLedger point claimed st
  | pointSlotNo point < lnpsPruneSlot st =
      Left $ ExnLeiosOfferTooOld point (lnpsPruneSlot st)
  | Set.member point (lnpsBodyOffers st) =
      Left $ ExnLeiosRepeatedBlockOffer point
  | not (announcedIt point st) =
      Left $ ExnLeiosBlockOfferWithoutAnnouncement point claimed
  | otherwise = do
      view <-
        first (ExnLeiosBlockOfferOutsideHorizon point) $
          runExcept $
            forecastFor
              (ledgerViewForecastAt (configLedger cfg) (ledgerState immLedger))
              (pointSlotNo point)
      let bound = maybe 0 llvMaxEbBodySize $ getLeiosLedgerView (Proxy @(BlockProtocol blk)) view
      if claimed == 0 || claimed > bound
        then Left $ ExnLeiosBlockOfferBadSize point claimed bound
        else Right st{lnpsBodyOffers = Set.insert point (lnpsBodyOffers st)}

-- | Accept a peer's 'MsgLeiosBlockTxsOffer', or say why the peer must go.
--
-- Held to the same announcement as the body offer and otherwise independent of
-- it: either may arrive first, or alone. So a LeiosNotify server never has to
-- track what it has already offered a peer in order to synthesise a body offer
-- ahead of a closure offer.
checkLeiosClosureOffer ::
  LeiosPoint ->
  LeiosNotifyPeerState blk ->
  Either ExnLeiosNotifyInvalidOffer (LeiosNotifyPeerState blk)
checkLeiosClosureOffer point st
  | pointSlotNo point < lnpsPruneSlot st =
      Left $ ExnLeiosOfferTooOld point (lnpsPruneSlot st)
  | Set.member point (lnpsClosureOffers st) =
      Left $ ExnLeiosRepeatedClosureOffer point
  | not (announcedIt point st) =
      Left $ ExnLeiosClosureOfferWithoutAnnouncement point
  | otherwise =
      Right st{lnpsClosureOffers = Set.insert point (lnpsClosureOffers st)}

-- | Whether this peer has announced this endorser block.
announcedIt :: LeiosPoint -> LeiosNotifyPeerState blk -> Bool
announcedIt point st =
  any
    (\ancHdr -> announcementEbHash (ancAnnouncement ancHdr) == pointEbHash point)
    (Announcements.announcementsInSlot (pointSlotNo point) (lnpsAnnouncements st))

-- | Thrown when a peer's 'MsgLeiosBlockAnnouncement' is one an honest peer
-- would not send; the ensuing thread death disconnects it.
data ExnLeiosNotifyInvalidAnnouncement blk
  = -- | The header carries no announcement, so it should not have been relayed
    -- as one.
    ExnLeiosNotifyAnnouncementMissing
  | ExnLeiosNotifyInvalidAnnouncement !(Announcements.ErrAnnouncement (AnnouncementInvalidity blk))
  deriving Show

instance Typeable blk => Exception (ExnLeiosNotifyInvalidAnnouncement blk)

-- | Judge a peer's 'MsgLeiosBlockAnnouncement' against what it has announced
-- before, throwing if it is one an honest peer would not send.
--
-- Nothing else in the node processes the announcement yet.
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
  -- | the immutable tip's ledger state
  ExtLedgerState blk EmptyMK ->
  LeiosNotifyPeerState blk ->
  Header blk ->
  m (LeiosNotifyPeerState blk)
onLeiosNotifyAnnouncement systemTime futureCheck cfg immLedger st hdr = do
  anc <- case mkAnnouncingHeader cfg hdr of
    Nothing -> throwIO (ExnLeiosNotifyAnnouncementMissing @blk)
    Just x -> pure x
  res <-
    runExceptT $
      Announcements.onAnnouncement
        nullTracer
        ancElId
        (announcementValidity systemTime futureCheck cfg immLedger)
        (\_anc _validated -> pure ())
        (lnpsAnnouncements st)
        anc
  case res of
    Left err -> throwIO (ExnLeiosNotifyInvalidAnnouncement err :: ExnLeiosNotifyInvalidAnnouncement blk)
    Right announcements ->
      pure
        st
          { lnpsAnnouncements = announcements
          , lnpsAnnouncedRbHashes =
              Map.insert (announcingRbHash anc) (blockSlot hdr) (lnpsAnnouncedRbHashes st)
          }

-- | Thrown when a peer's 'MsgLeiosVotes' holds a vote an honest peer would not
-- send; the ensuing thread death disconnects it.
data ExnLeiosNotifyInvalidVotes
  = -- | An empty 'MsgLeiosVotes', which only spends a reply; 'MsgCanceled' is how a
    -- peer answers a request it has nothing for.
    ExnLeiosEmptyVotes
  | -- | A vote for an announcing block this peer never sent us. This also rules
    -- out a vote in an empty slot.
    ExnLeiosVoteWithoutAnnouncement !RbHash
  | -- | A second vote from one seat in one slot: the election slot, and the
    -- seat.
    ExnLeiosRepeatedVote !SlotNo !Word16
  | -- | A vote too young to have been cast: its announcing block, that block's
    -- election slot, and how long after that slot began the vote arrived.
    -- Votes are cast 3 L_hdr after the slot began, the time equivocations take
    -- to come to light, widened by the node's maximum clock skew so that an
    -- honest peer is never disconnected.
    ExnLeiosVoteTooEarly !RbHash !SlotNo !NominalDiffTime
  | -- | A vote too old to still be relayed, likewise. Votes stop being relayed
    -- 3 L_hdr + L_vote after the slot began, widened by 'voteLateGrace' so
    -- that an honest peer is never disconnected. A vote for a slot behind the
    -- immutable tip is among these, long past any window.
    ExnLeiosVoteTooLate !RbHash !SlotNo !NominalDiffTime
  | -- | A vote whose election slot, given with its announcing block, the
    -- immutable tip cannot forecast to. Its announcement was validated by such
    -- a forecast, from an older immutable tip, so this should never happen.
    ExnLeiosVoteOutsideHorizon !RbHash !SlotNo
  deriving Show

instance Exception ExnLeiosNotifyInvalidVotes

-- | Accept a peer's 'MsgLeiosVotes', or say why the peer must go; see
-- 'ExnLeiosNotifyInvalidVotes'.
--
-- This does not check a vote's seat or signature. The node's central vote
-- processing will, once per vote, caching the result, so a badly signed copy of
-- a vote the node already has costs it only the bytes.
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
  [LeiosVote] ->
  LeiosNotifyPeerState blk ->
  Either ExnLeiosNotifyInvalidVotes (LeiosNotifyPeerState blk)
checkLeiosVotes cfg skew immLedger now votes st0 = case votes of
  [] -> Left ExnLeiosEmptyVotes
  _ : _ -> foldM checkOne st0 votes
 where
  checkOne st vote = do
    slot <-
      maybe (Left (ExnLeiosVoteWithoutAnnouncement rbHash)) Right $
        Map.lookup rbHash (lnpsAnnouncedRbHashes st)
    when (Set.member (slot, seat) (lnpsVotes st)) $
      Left (ExnLeiosRepeatedVote slot seat)
    (onset, _slotLength) <-
      first (const (ExnLeiosVoteOutsideHorizon rbHash slot)) $
        HardFork.runQuery
          (HardFork.slotToWallclock slot)
          (hardForkSummary (configLedger cfg) (ledgerState immLedger))
    let age = diffRelTime now onset
    -- 'forecastFor' cannot judge a slot behind its anchor, the immutable tip.
    when (NotOrigin slot < getTipSlot (ledgerState immLedger)) $
      Left (ExnLeiosVoteTooLate rbHash slot age)
    view <-
      first (const (ExnLeiosVoteOutsideHorizon rbHash slot)) $
        runExcept $
          forecastFor
            (ledgerViewForecastAt (configLedger cfg) (ledgerState immLedger))
            slot
    MkLeiosLedgerView{llvAnnouncementPeriod = lHdr, llvVotePeriod = lVote} <-
      maybe (Left (ExnLeiosVoteWithoutAnnouncement rbHash)) Right $
        getLeiosLedgerView (Proxy @(BlockProtocol blk)) view
    when (age < 3 * lHdr - InFutureCheck.unClockSkew skew) $
      Left (ExnLeiosVoteTooEarly rbHash slot age)
    when (age > 3 * lHdr + lVote + voteLateGrace) $
      Left (ExnLeiosVoteTooLate rbHash slot age)
    pure st{lnpsVotes = Set.insert (slot, seat) (lnpsVotes st)}
   where
    rbHash = voteAnnouncingRbHash vote
    seat = leiosSeatIndex (voteSeatId vote)

-- | How much later than 3 L_hdr + L_vote after its election slot began a vote
-- may arrive: enough for transmission, clock skew, and votes queued behind
-- other traffic.
--
-- TODO magic number; measure buffer bloat on a loaded network.
voteLateGrace :: NominalDiffTime
voteLateGrace = 60
