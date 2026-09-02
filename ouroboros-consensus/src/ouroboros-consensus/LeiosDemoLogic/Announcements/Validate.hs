{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Validation of a relayed Leios EB announcement, which is carried as an RB
-- 'Header'.
--
-- The announcement can arrive out-of-order with respect to the local chain, so we
-- run only the /protocol-level/ header validation (see 'validateHeaderProtocol')
-- against the immutable tip's ledger state, forecast to the announced slot; the
-- envelope (chain-extension) check is deliberately skipped. As of Dijkstra the
-- protocol-level header rules already fold in the announcement's checks (the
-- EbBody size bound etc.), so this is the whole announcement validation.
--
-- For a caught-up node the immutable tip is always within the forecast horizon
-- of a fresh announcement (by appeal to Praos Chain Growth); still-syncing
-- nodes do not request LeiosNotify notifications, so they never reach here.
module LeiosDemoLogic.Announcements.Validate
  ( AnnouncementInvalidity (..)
  , validateAnnouncementHeader
  ) where

import Control.Monad (when)
import Control.Monad.Except (runExcept, throwError, withExcept)
import LeiosDemoTypes (BytesSize, LeiosPoint)
import Ouroboros.Consensus.Block
  ( BlockProtocol
  , Header
  , WithOrigin (NotOrigin)
  , blockSlot
  , validateView
  )
import Ouroboros.Consensus.Config
  ( TopLevelConfig
  , configBlock
  , configConsensus
  , configLedger
  )
import Ouroboros.Consensus.Forecast (OutsideForecastRange, forecastFor)
import Ouroboros.Consensus.HeaderValidation
  ( ValidateEnvelope (..)
  , tickHeaderState
  , tickedHeaderStateChainDep
  )
import Ouroboros.Consensus.Ledger.Abstract (getTipSlot)
import Ouroboros.Consensus.Ledger.Basics (EmptyMK)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol
  , ledgerViewForecastAt
  )
import Ouroboros.Consensus.Protocol.Abstract (ValidationErr)
import Ouroboros.Consensus.Storage.LedgerDB.Forker
  ( OCINStaleness (..)
  , ResolveLeiosBlock
  , headerLeiosAnnouncement
  , validateAnnouncementChainDepState
  )

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
    -- See 'LeiosDemoLogic.Announcements.ShouldRelay' for how honest
    -- servers avoid triggering this case despite clock
    -- skew\/transmission delays\/buffering, etc.
    --
    -- The 'LeiosDemoLogic.Announcements.ErrTooOld' bound rejects any
    -- announcement older than 'maxAnnouncementAgeRecv' (minutes), which is
    -- always far younger than the immutable tip (hours), so this case should
    -- never actually fire. It is kept as a distinct error to throw in case that
    -- branch is somehow reached, eg on an oddly-configured testnet.
    SlotBeforeImmutableTip
  | -- | The header failed the relaxed, out-of-context protocol-level validation
    -- (election proof and signature). See 'validateAnnouncementChainDepState'
    -- for which check is relaxed (the OCIN upper bound) and
    -- 'validateAnnouncementHeader' for which are skipped (the chain-extension
    -- envelope checks).
    HeaderInvalid !(ValidationErr (BlockProtocol blk))
  | -- | The RbHeader failed an envelope check — chiefly, it exceeds the
    -- protocol's max header size (see 'validateAnnouncementHeader').
    RbHeaderEnvelopeInvalid !(OtherHeaderEnvelopeError blk)
  | -- | The header carries no EB announcement, so it should not have been
    -- relayed as a 'MsgLeiosBlockAnnouncement' at all.
    NoAnnouncement

-- | NB 'HeaderInvalid' and 'RbHeaderEnvelopeInvalid' do not render their
-- wrapped errors, so that this instance is unconstrained in @blk@ (avoiding
-- @Show@ constraints on those errors that would have to be threaded through the
-- node). The wrapped values still carry them.
instance Show (AnnouncementInvalidity blk) where
  show ai = case ai of
    OutsideHorizon r -> "OutsideHorizon (" <> show r <> ")"
    SlotBeforeImmutableTip -> "SlotBeforeImmutableTip"
    HeaderInvalid{} -> "HeaderInvalid <header-validation-error>"
    RbHeaderEnvelopeInvalid{} -> "RbHeaderEnvelopeInvalid <envelope-error>"
    NoAnnouncement -> "NoAnnouncement"

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
-- (see 'validateAnnouncementChainDepState'): accepted from the peer — updating
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
-- 'LeiosDemoLogic.Announcements.ElBimap.ElId').
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
-- Returns the OCIN staleness and the output of 'headerLeiosAnnouncement'.
validateAnnouncementHeader ::
  forall blk.
  ( LedgerSupportsProtocol blk
  , ResolveLeiosBlock blk
  ) =>
  TopLevelConfig blk ->
  ExtLedgerState blk EmptyMK ->
  Header blk ->
  Either (AnnouncementInvalidity blk) (OCINStaleness, (LeiosPoint, BytesSize))
validateAnnouncementHeader cfg extLedger hdr =
  runExcept $ do
    x <- case headerLeiosAnnouncement hdr of
      Nothing -> throwError NoAnnouncement
      Just x -> pure x
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
    -- announcement's transmission unit. This is the full Shelley envelope check,
    -- so it also bounds the declared RB body size and rejects an obsolete node.
    -- Those two are harmless extras — a legitimate announcement's RbHeader always
    -- passes them — and, being LedgerView-derived, they are meaningful
    -- out-of-context (unlike the chain-extension checks, which we skip).
    withExcept RbHeaderEnvelopeInvalid $
      additionalEnvelopeChecks cfg ledgerView hdr
    -- The out-of-context, relaxed protocol-level validation: the election proof
    -- and the signature, but not the RB-header-specific checks nor the checks
    -- that our lagging tip would spuriously trip (see
    -- 'validateAnnouncementChainDepState'). Any error it returns is a genuine
    -- rejection.
    --
    -- Ticks the immutable tip's header state to the announcing slot without
    -- applying the intervening headers: sound within the forecast horizon, since
    -- the nonce and stake the VRF/KES checks use are already fixed and stale
    -- opcert counters only yield 'StaleOCIN', never a false rejection. No TICKF
    -- analog is warranted: unlike the ledger tick, nothing costly is discarded.
    let tickedHeaderState =
          tickHeaderState (configConsensus cfg) ledgerView slot (headerState extLedger)
    staleness <-
      withExcept HeaderInvalid $
        validateAnnouncementChainDepState @blk
          (configConsensus cfg)
          (validateView (configBlock cfg) hdr)
          slot
          (tickedHeaderStateChainDep tickedHeaderState)
    pure (staleness, x)
 where
  slot = blockSlot hdr
