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
  ( ResolveLeiosBlock
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
-- The operational certficiate (opcert) issue number (OCIN) is checked only as a
-- lower bound: any number at least the immutable tip's counter is accepted (the
-- over-increment upper bound, which the strict protocol check would enforce, is
-- skipped — see 'validateAnnouncementChainDepState' and
-- 'WhetherToUpperBoundOCERT'), and a lower one is rejected as a revoked key.
--
-- OCINs are otherwise ignored. In effect, this logic is assuming that all OCINs
-- are controlled by the pool owner. That's patentedly contrary the intended
-- purpose of OCINs, so it needs justification; hence this comment.
--
-- The crux is a Catch 22 if we consider different OCINs as different
-- identities. We must either treat all of those identities
-- _independently_ (ie as distinct elections) or _prioritize_ the
-- greater OCINs (which seems intuitive). The problem is that the
-- adversary can create arbitrarily many OCINs for its own pools. And
-- then it can abuse either choice we make: either it gets to multiply
-- the Leios load on the network per election, or it can cause
-- arbitrary "partitions" of the network, with one clique certifying a
-- lower OCIN's announcement but the other clique completely ignoring
-- that announcement.
--
-- The current behavior is to accept (and relay!) any OCIN at least as
-- great as the counter in our immutable tip's ledger state. The only
-- downside to this is that an increment OCIN doesn't revoke the old
-- opcert _for Leios_ until the increment is on the immutable tip
-- (Praos is still immediate). So a leaked hot key means the attacker
-- can equivocate all of the victim's announcements until the victim
-- notices, lands a new opcert on chain, and then waits for that
-- opcert to become immutable (~12 hr, <= ~36 hr). Not ideal, but
-- tolerable.
--
-- Returns the output of 'headerLeiosAnnouncement'.
validateAnnouncementHeader ::
  forall blk.
  ( LedgerSupportsProtocol blk
  , ResolveLeiosBlock blk
  ) =>
  TopLevelConfig blk ->
  ExtLedgerState blk EmptyMK ->
  Header blk ->
  Either (AnnouncementInvalidity blk) (LeiosPoint, BytesSize)
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
    let tickedHeaderState =
          tickHeaderState (configConsensus cfg) ledgerView slot (headerState extLedger)
    withExcept HeaderInvalid $
      validateAnnouncementChainDepState @blk
        (configConsensus cfg)
        (validateView (configBlock cfg) hdr)
        slot
        (tickedHeaderStateChainDep tickedHeaderState)
    pure x
 where
  slot = blockSlot hdr
