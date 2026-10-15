{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Validation of a relayed Leios announcement, which is carried as an RB
-- 'Header'.
--
-- The announcement can arrive out-of-order with respect to the local chain, so
-- we run only the /protocol-level/ header validation against the immutable
-- tip's ledger state, forecast to the announced slot; the envelope
-- (chain-extension) check is deliberately skipped, except for the bounds a
-- ledger view alone determines.
--
-- For a caught-up node the immutable tip is always within the forecast horizon
-- of a fresh announcement (by appeal to Praos Chain Growth); a LeiosNotify
-- client sends no request until then (see 'awaitImmTipCanForecastNow').
module Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( AnnouncementInvalidity (..)
  , AnnouncingHeader (ancAnnouncement, ancElectionId, ancHeader)
  , ValidAnnouncement (..)
  , announcementValidity
  , announcingRbHash
  , maxAnnouncementAgeRecv
  , maxAnnouncementAgeSend
  , mkAnnouncingHeader
  , validateAnnouncementHeader
  ) where

import Control.Monad (when)
import Control.Monad.Except (Except, ExceptT, liftEither, runExcept, throwError, withExcept)
import Control.Monad.Trans (lift)
import Data.Bifunctor (first)
import Data.Time.Clock (NominalDiffTime)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime
  , SystemTime (..)
  , diffRelTime
  )
import Ouroboros.Consensus.Config
  ( TopLevelConfig
  , configBlock
  , configConsensus
  , configLedger
  )
import Ouroboros.Consensus.Forecast (OutsideForecastRange, forecastFor)
import Ouroboros.Consensus.HeaderValidation
  ( OtherHeaderEnvelopeError
  , ValidateEnvelope (..)
  , tickHeaderState
  , tickedHeaderStateChainDep
  )
import Ouroboros.Consensus.Ledger.Abstract (EmptyMK, getTipSlot)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol
  , ledgerViewForecastAt
  )
import Ouroboros.Consensus.Leios.Protocol
import Ouroboros.Consensus.Leios.Types (ElectionId (..), RbHash (..))
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck as InFutureCheck
import qualified Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements as Announcements
import Ouroboros.Consensus.Protocol.Abstract (ValidationErr)
import Ouroboros.Consensus.Util.IOLike (IOLike, throwIO)

-- | A header that carries an announcement.
data AnnouncingHeader blk
  = -- | INVARIANT: 'ancHeader' carries the announcement 'ancAnnouncement', and
    -- 'ancElectionId' is its election
    UnsafeMkAnnouncingHeader
    { ancHeader :: !(Header blk)
    , ancElectionId :: !ElectionId
    , ancAnnouncement :: !LeiosAnnouncement
    }
  deriving stock Generic

deriving anyclass instance NoThunks (Header blk) => NoThunks (AnnouncingHeader blk)

instance HasHeader (Header blk) => Eq (AnnouncingHeader blk) where
  a == b = headerHash (ancHeader a) == headerHash (ancHeader b)

-- | The hash of the announcing block, which votes name.
announcingRbHash ::
  forall blk. (ConvertRawHash blk, GetHeader blk) => AnnouncingHeader blk -> RbHash
announcingRbHash anc = MkRbHash (toRawHash (Proxy @blk) (headerHash (ancHeader anc)))

-- | Interpret a header as a relayed announcement, or 'Nothing' if it carries
-- none (so it should not have been relayed as one).
mkAnnouncingHeader ::
  forall blk.
  (BlockSupportsProtocol blk, LeiosProtocol (BlockProtocol blk)) =>
  TopLevelConfig blk ->
  Header blk ->
  Maybe (AnnouncingHeader blk)
mkAnnouncingHeader cfg hdr = do
  ann <-
    getLeiosAnnouncement
      (Proxy @(BlockProtocol blk))
      (validateView (configBlock cfg) hdr)
  pure $
    UnsafeMkAnnouncingHeader
      hdr
      (ElectionId (blockSlot hdr) (announcementElectee ann))
      ann

-- | Reasons a LeiosNotify client would reject its upstream peer's announcement,
-- regardless of other messages they had sent
data AnnouncementInvalidity blk
  = -- | The announced slot is beyond the forecast horizon from the immutable
    -- tip. This should not occur for a caught-up node (by appeal to Praos Chain
    -- Growth), so it is either a bogus far-future slot or we are falling behind;
    -- either way disconnecting is acceptable (if we are behind, then either we
    -- are unhealthy or the peer is not serving us well). We cannot simply ignore
    -- it, because there is an unbounded supply of far-future slots.
    OutsideHorizon !OutsideForecastRange
  | -- | The announced slot is before the immutable tip (the forecast anchor),
    -- so it cannot be forecast\/validated at all. This is a separate case from
    -- 'OutsideHorizon' because 'forecastFor' does not report a below-anchor
    -- slot as 'OutsideForecastRange' (that only bounds the future
    -- end)---instead, a below-anchor slot violates 'forecastFor''s
    -- precondition. For a caught-up node a slot so stale is as bogus as one in
    -- the far-future.
    --
    -- The 'Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.ErrTooOld' bound rejects any
    -- announcement older than 'maxAnnouncementAgeRecv' (minutes), which is
    -- always far younger than the immutable tip (hours), so this case should
    -- never actually fire. It is kept as a distinct error to throw in case that
    -- branch is somehow reached, eg on an oddly-configured testnet.
    SlotBeforeImmutableTip
  | -- | The header failed the relaxed, out-of-context protocol-level validation
    -- (election proof and signature). See 'validateLeiosAnnouncement'
    -- for which check is relaxed (the OCIN upper bound) and
    -- 'validateAnnouncementHeader' for which are skipped (the chain-extension
    -- envelope checks).
    HeaderInvalid !(ValidationErr (BlockProtocol blk))
  | -- | The RbHeader failed an envelope check — chiefly, it exceeds the
    -- protocol's max header size (see 'validateAnnouncementHeader').
    RbHeaderEnvelopeInvalid !(OtherHeaderEnvelopeError blk)

deriving stock instance
  (Show (ValidationErr (BlockProtocol blk)), Show (OtherHeaderEnvelopeError blk)) =>
  Show (AnnouncementInvalidity blk)

-- | Protocol-level validation of an announced RB 'Header' against the immutable
-- tip's ledger state (forecast to the header's slot). Envelope check skipped;
-- see the module header.
--
-- The operational certificate (opcert) issue number (OCIN) governs only relay,
-- not acceptance. We cannot soundly constrain it as strictly as genuine header
-- validation does because we're using merely the immutable ledger state, which
-- may be out of date. Only a non-OCIN failure — a bad election proof or
-- signature, a slot before the immutable tip, an oversized header — rejects and
-- disconnects. But even if an announcement is not considered invalid, it might
-- otherwise be ignored. An announcement whose OCIN is at least our immutable
-- tip's recorded counter is 'FreshOCIN': processed and relayed as usual. A
-- lower one, or one from a key we have no recorded counter for, is 'StaleOCIN'
-- (see 'validateLeiosAnnouncement'): accepted from the peer — updating
-- that peer's dedup state, never disconnecting — but not processed, published,
-- or relayed.
--
-- Skipping OCIN validation effectively treats every one of a pool's OCINs as
-- the pool owner's, which is contrary to the intended purpose of OCINs, so
-- omitting the two checks that strict header validation would make needs
-- justification.
--
-- We do not enforce the counter's /upper/ bound: we accept that the true OCIN
-- may have run ahead of our imm tip's view. In fact, this logic treats distinct
-- OCINs as the same identity. The crux is a Catch 22 if we were to distinguish
-- them: we would have to either treat those identities /independently/ (as
-- distinct elections) or /prioritize/ the greater OCIN (which seems
-- intuitive). But an adversary can mint arbitrarily many OCINs for its own
-- pools and abuse either choice. The former lets them multiply the Leios load
-- per election; the latter lets them "partition" the network, one clique
-- certifying a lower OCIN's announcement while the other ignores it. So
-- elections are keyed merely on @(slot, pool)@ alone (see
-- 'Ouroboros.Consensus.Leios.Types.ElectionId').
--
-- We do not /promptly/ enforce the counter's /lower/ bound. An OCIN increment
-- revokes the old opcert immediately for Praos (at header validation/chain
-- adoption), but for Leios only once the increment reaches our immutable tip.
-- Until then our recorded counter is still the old one, so an announcement
-- bearing it (or greater) is 'FreshOCIN' and we process and relay it as
-- usual. The downside of not being prompt is that a leaked hot key lets the
-- attacker equivocate the victim's announcements until the victim notices,
-- lands a new opcert, and waits for it to become immutable (~12 hr, <= ~36 hr)
-- — and during that window announcements using the revoked OCIN are fresh, so
-- they can even be voted for\/part of equivocation proofs\/etc. Not ideal, but
-- tolerable.
--
-- Once the increment /is/ immutable for us, an announcement bearing the old
-- counter is 'StaleOCIN'. But even then we must not /reject/ it, only decline
-- to process or relay it. Different honest nodes might have different immutable
-- tips, so honest nodes can briefly disagree on whether the increment is yet
-- immutable. An adversary who bumps a pool's OCIN and then sends a ~on-time
-- announcement using the /revoked/ OCIN, timed to straddle that boundary, would
-- otherwise make \"fast\" nodes (increment immutable, so 'StaleOCIN')
-- disconnect the \"slow\" honest peers (increment not yet immutable, so still
-- 'FreshOCIN') that legitimately relay it — an OCIN-timed partition.
--
-- That results in a diffusion asymmetry — the slow peers relay the old-counter
-- announcement, the fast ones do not — but it is not the certify/ignore
-- partition the upper-bound Catch 22 warned about, because once the increment
-- is immutable even just /for/ /us/ the announcement can never be voted for,
-- hence never certified:
--
--   * Once the increment is immutable for /any/ honest node then, by the Praos
--     Common Prefix property, it is in every healthy honest node's selection now
--     and forever, so every honest node's up-to-date view records the incremented
--     counter.
--
--   * A vote follows selection, and selection is driven by ChainSync
--     @MsgRollForward@ headers. Those are /not/ dangling: they extend the node's
--     current chain and undergo full header validation with the exact
--     chain-dependent OCIN bounds. Only announcements are dangling, which is
--     precisely why only they fall back to the immutable-tip ledger view and this
--     relaxed, report-don't-reject OCIN check.
--
-- So the old-counter RB fails @MsgRollForward@ validation on every healthy honest
-- node — even the slow ones still relaying its announcement — never becomes a
-- selected tip, and is never voted for; the asymmetry never yields a certified
-- EB, so it is no Linear Leios availability violation.
--
-- Returns the OCIN staleness.
validateAnnouncementHeader ::
  forall blk.
  ( LedgerSupportsProtocol blk
  , LeiosProtocol (BlockProtocol blk)
  ) =>
  TopLevelConfig blk ->
  ExtLedgerState blk EmptyMK ->
  AnnouncingHeader blk ->
  Except (AnnouncementInvalidity blk) OCINStaleness
validateAnnouncementHeader cfg extLedger ancHdr = do
  -- 'forecastFor' does not reject a slot below its anchor (the immutable
  -- tip's slot) — that is a precondition violation, not 'OutsideForecastRange'
  -- — so guard it explicitly before forecasting.
  when (NotOrigin slot < getTipSlot (ledgerState extLedger)) $
    throwError SlotBeforeImmutableTip
  ledgerView <-
    withExcept OutsideHorizon $
      forecastFor
        (ledgerViewForecastAt (configLedger cfg) (ledgerState extLedger))
        slot
  -- Reject an RbHeader bigger than the protocol allows: it is the
  -- announcement's transmission unit. These checks depend only on the ledger
  -- view, so they are meaningful out of context, unlike the chain-extension
  -- checks, which we skip.
  withExcept RbHeaderEnvelopeInvalid $
    additionalEnvelopeChecks cfg ledgerView hdr
  -- Ticks the immutable tip's header state to the announcing slot without
  -- applying the intervening headers: sound within the forecast horizon, since
  -- the nonce and stake the VRF/KES checks use are already fixed and stale
  -- opcert counters only yield 'StaleOCIN', never a false rejection.
  let tickedHeaderState =
        tickHeaderState (configConsensus cfg) ledgerView slot (headerState extLedger)
  withExcept HeaderInvalid $
    validateLeiosAnnouncement
      (configConsensus cfg)
      (validateView (configBlock cfg) hdr)
      slot
      (tickedHeaderStateChainDep tickedHeaderState)
 where
  hdr = ancHeader ancHdr
  slot = blockSlot hdr

-- | Validate a relayed announcement, after the peer's per-election counting
-- checks have passed.
--
-- First apply ChainSync's in-future check to the announced slot's wall-clock
-- onset (reusing the node's own 'InFutureCheck.SomeHeaderInFutureCheck'):
-- a far-future slot raises 'InFutureCheck.HeaderArrivalException' (disconnecting
-- the peer), a near-future slot blocks until the slot's onset (Ouroboros
-- Chronos) — blocking the per-peer handler is acceptable, as a (near-)future
-- announcement is the peer's fault.
--
-- A valid announcement may still be 'StaleOCIN', which the peer's dedup state
-- counts but the node does not process.
announcementValidity ::
  ( IOLike m
  , LedgerSupportsProtocol blk
  , LeiosProtocol (BlockProtocol blk)
  ) =>
  SystemTime m ->
  InFutureCheck.SomeHeaderInFutureCheck m blk ->
  TopLevelConfig blk ->
  -- | The immutable tip's ledger state, for the OCIN revocation check: a
  -- revocation only counts once it cannot be rolled back.
  ExtLedgerState blk EmptyMK ->
  AnnouncingHeader blk ->
  ExceptT (Announcements.Error (AnnouncementInvalidity blk)) m (ValidAnnouncement blk)
announcementValidity systemTime futureCheck cfg immLedger ancHdr = do
  onset <- lift $ case futureCheck of
    InFutureCheck.SomeHeaderInFutureCheck hifc -> do
      arrival <- InFutureCheck.recordHeaderArrival hifc (ancHeader ancHdr)
      judgment <-
        either throwIO pure $
          runExcept $
            InFutureCheck.judgeHeaderArrival
              hifc
              (configLedger cfg)
              (ledgerState immLedger)
              arrival
      arrivalResult <- InFutureCheck.handleHeaderArrival hifc judgment
      either throwIO pure (runExcept arrivalResult)
  -- The in-future check has delayed this thread until 'onset' if the
  -- slot was near-future, so 'now' is at or after 'onset' and the age
  -- is non-negative.
  now <- lift $ systemTimeCurrent systemTime
  let age = diffRelTime now onset
  when (age > maxAnnouncementAgeRecv) $
    throwError Announcements.ErrTooOld
  staleness <-
    liftEither . first Announcements.ErrInvalid . runExcept $
      validateAnnouncementHeader cfg immLedger ancHdr
  pure
    MkValidAnnouncement
      { announcingHeader = ancHdr
      , announcementOnset = onset
      , announcementAge = age
      , announcementStaleness = staleness
      }

-- | A valid announcement, and what 'announcementValidity' learns about it.
data ValidAnnouncement blk = MkValidAnnouncement
  { announcingHeader :: !(AnnouncingHeader blk)
  , announcementOnset :: !RelativeTime
  -- ^ The wall-clock onset of the election's slot.
  , announcementAge :: !NominalDiffTime
  -- ^ How long after that onset the announcement arrived.
  , announcementStaleness :: !OCINStaleness
  }

-- | Do not relay an announcement whose slot's wall-clock onset is older than
-- this.
--
-- Comfortably less than 'maxAnnouncementAgeRecv', so that an announcement an
-- honest node relays just before this bound still arrives within its
-- downstream peer's larger bound, even after transmission time and clock skew.
-- It is also far younger than any peer's immutable tip, so no downstream peer
-- rejects a relayed announcement as 'SlotBeforeImmutableTip'.
--
-- TODO magic number; should be a config/RunNode option
maxAnnouncementAgeSend :: NominalDiffTime
maxAnnouncementAgeSend = 300 -- 5 minutes

-- | Disconnect an upstream peer that relays an announcement whose slot's
-- wall-clock onset is older than this. See
-- 'Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.ErrTooOld'.
--
-- Comfortably greater than 'maxAnnouncementAgeSend', so that an honest peer is
-- never disconnected on account of transmission time or clock skew.
--
-- TODO magic number; should be a config/RunNode option... or even a protocol
-- parameter?
maxAnnouncementAgeRecv :: NominalDiffTime
maxAnnouncementAgeRecv = 600 -- 10 minutes
