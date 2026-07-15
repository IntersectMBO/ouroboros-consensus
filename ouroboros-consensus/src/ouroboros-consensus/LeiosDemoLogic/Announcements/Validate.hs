{-# LANGUAGE FlexibleContexts #-}

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

import Control.Monad (void)
import Control.Monad.Except (runExcept, throwError, withExcept)
import LeiosDemoTypes (LeiosPoint, BytesSize)
import Ouroboros.Consensus.Block (BlockProtocol, Header, blockSlot)
import Ouroboros.Consensus.Config (TopLevelConfig, configConsensus, configLedger)
import Ouroboros.Consensus.Forecast (OutsideForecastRange, forecastFor)
import Ouroboros.Consensus.HeaderValidation
  ( tickHeaderState
  , validateHeaderProtocol
  )
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
  )

data AnnouncementInvalidity blk
  = -- | The announced slot is beyond the forecast horizon from the immutable
    -- tip (should not occur for a caught-up node, by appeal to Praos Chain
    -- Growth).
    OutsideHorizon !OutsideForecastRange
  | -- | The header failed protocol-level validation.
    HeaderInvalid !(ValidationErr (BlockProtocol blk))
  | -- | The header carries no EB announcement, so it should not have been
    -- relayed as a 'MsgLeiosBlockAnnouncement' at all.
    NoAnnouncement

-- | Protocol-level validation of an announced RB 'Header' against the immutable
-- tip's ledger state (forecast to the header's slot). Envelope check skipped;
-- see the module header.
--
-- Note that this will accept an opcert issue number that is equal to
-- or one greater than the opcert issue number from the immutable
-- tip's ledger state. TODO restrict opcert issue number increments to
-- be at least one stabiliy window apart, so that this limitation will
-- never falsely reject an honest opcert issue number increment.
--
-- Returns the output of 'headerLeiosAnnouncement'.
validateAnnouncementHeader ::
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
    ledgerView <-
      withExcept OutsideHorizon $
        forecastFor
          (ledgerViewForecastAt (configLedger cfg) (ledgerState extLedger))
          slot
    void $
      withExcept HeaderInvalid $
        validateHeaderProtocol
          cfg
          hdr
          (tickHeaderState (configConsensus cfg) ledgerView slot (headerState extLedger))
    pure x
 where
  slot = blockSlot hdr
