{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Validation of a relayed Leios EB announcement, which is carried as an RB
-- 'Header'.
--
-- The announcement arrives out-of-order with respect to the local chain, so we
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
import LeiosDemoTypes (AnnouncementDisposition (..), LeiosPoint, BytesSize)
import Ouroboros.Consensus.Block (BlockProtocol, Header, WithOrigin (NotOrigin), blockSlot)
import Ouroboros.Consensus.Config (TopLevelConfig, configConsensus, configLedger)
import Ouroboros.Consensus.Forecast (OutsideForecastRange, forecastFor)
import Ouroboros.Consensus.HeaderValidation
  ( tickHeaderState
  , validateHeaderProtocol
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
  , classifyAnnouncementValidationErr
  , headerLeiosAnnouncement
  )

data AnnouncementInvalidity blk
  = -- | The announced slot is beyond the forecast horizon from the immutable
    -- tip. This should not occur for a caught-up node (by appeal to Praos Chain
    -- Growth), so it is either a bogus far-future slot or we are falling behind;
    -- either way disconnecting is acceptable (if we are behind, then either we
    -- are unhealthy or the peer is not serving us well). We cannot simply ignore
    -- it, because there is an unbounded supply of far-future slots.
    OutsideHorizon !OutsideForecastRange
  | -- | The announced slot is before the immutable tip (the forecast anchor),
    -- so it cannot be forecast/validated at all. This is a /separate/ case
    -- because 'forecastFor' does not report a below-anchor slot as
    -- 'OutsideForecastRange' (that only bounds the future end); a below-anchor
    -- slot violates 'forecastFor''s precondition. For a caught-up node such a
    -- stale slot is as bogus as a far-future one.
    --
    -- See 'LeiosDemoLogic.Announcements.ShouldRelay' for how honest
    -- servers avoid triggering this case despite clock
    -- skew\/transmission delays\/buffering, etc.
    SlotBeforeImmutableTip
  | -- | The header failed protocol-level validation.
    HeaderInvalid !(ValidationErr (BlockProtocol blk))
  | -- | The header carries no EB announcement, so it should not have been
    -- relayed as a 'MsgLeiosBlockAnnouncement' at all.
    NoAnnouncement

-- | NB 'HeaderInvalid' does not render its 'ValidationErr', so that this
-- instance is unconstrained in @blk@ (avoiding a
-- @Show (ValidationErr (BlockProtocol blk))@ constraint that would have to be
-- threaded through the node). The wrapped value still carries it.
instance Show (AnnouncementInvalidity blk) where
  show ai = case ai of
    OutsideHorizon r -> "OutsideHorizon (" <> show r <> ")"
    SlotBeforeImmutableTip -> "SlotBeforeImmutableTip"
    HeaderInvalid{} -> "HeaderInvalid <header-validation-error>"
    NoAnnouncement -> "NoAnnouncement"

-- | Protocol-level validation of an announced RB 'Header' against the immutable
-- tip's ledger state (forecast to the header's slot). Envelope check skipped;
-- see the module header.
--
-- The opcert issue number is checked only as a lower bound: any number at
-- least the immutable tip's counter is accepted (an over-increment, which the
-- strict protocol check would reject, is 'Tolerate'd — see
-- 'AnnouncementDisposition'), and a lower one is rejected as a revoked key. See
-- the note on 'LeiosDemoLogic.Announcements.ErrAnnouncement' for why OCINs are
-- otherwise ignored.
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
    -- Reject a failed protocol-level check, /except/ for the out-of-context
    -- artifacts we 'Tolerate' (an opcert issue number ahead of, or a pool
    -- absent from, the immutable tip's counter state).
    case runExcept
      ( validateHeaderProtocol
          cfg
          hdr
          (tickHeaderState (configConsensus cfg) ledgerView slot (headerState extLedger))
      ) of
      Right _ -> pure ()
      Left err -> case classifyAnnouncementValidationErr @blk err of
        Tolerate -> pure ()
        Reject -> throwError (HeaderInvalid err)
    pure x
 where
  slot = blockSlot hdr
