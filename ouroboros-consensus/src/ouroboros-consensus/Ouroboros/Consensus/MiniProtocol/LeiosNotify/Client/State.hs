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
  , ExnLeiosNotifyInvalidOffer (..)
  , checkLeiosBlockOffer
  , checkLeiosClosureOffer
  , LeiosNotifyPeerState (..)
  , emptyLeiosNotifyPeerState
  , onLeiosNotifyAnnouncement
  , pruneLeiosNotifyPeerState
  ) where

import Control.Monad.Except (runExcept, runExceptT)
import Control.Tracer (nullTracer)
import Data.Bifunctor (first)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemTime)
import Ouroboros.Consensus.Config (TopLevelConfig, configLedger)
import Ouroboros.Consensus.Forecast (OutsideForecastRange, forecastFor)
import Ouroboros.Consensus.Ledger.Abstract (EmptyMK)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState, ledgerState)
import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol, ledgerViewForecastAt)
import Ouroboros.Consensus.Leios.Protocol
  ( LeiosAnnouncement (..)
  , LeiosLedgerView (..)
  , LeiosProtocol (..)
  )
import Ouroboros.Consensus.Leios.Types (BytesSize, LeiosPoint (..))
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
    }

-- | Forget everything about elections before the given slot, which is the
-- immutable tip's.
pruneLeiosNotifyPeerState :: SlotNo -> LeiosNotifyPeerState blk -> LeiosNotifyPeerState blk
pruneLeiosNotifyPeerState immTipSlot st
  | lnpsPruneSlot st < immTipSlot =
      MkLeiosNotifyPeerState
        { lnpsPruneSlot = immTipSlot
        , lnpsAnnouncements = Announcements.pruneAnnouncementsState immTipSlot (lnpsAnnouncements st)
        , lnpsBodyOffers =
            -- 'LeiosPoint' orders slot-first, so the pruned points are a prefix.
            Set.dropWhileAntitone (\p -> pointSlotNo p < immTipSlot) (lnpsBodyOffers st)
        , lnpsClosureOffers =
            Set.dropWhileAntitone (\p -> pointSlotNo p < immTipSlot) (lnpsClosureOffers st)
        }
  | otherwise = st

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
    Right announcements -> pure st{lnpsAnnouncements = announcements}
