{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Ledger.SupportsProtocol
  ( GenesisWindow (..)
  , LedgerSupportsProtocol (..)
  , ledgerViewOfTip
  ) where

import Control.Monad.Except
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Forecast
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Tables.Utils (forgetLedgerTables)
import Ouroboros.Consensus.Protocol.Abstract

-- | Link protocol to ledger

-- | The ledger view at the given ledger state's own tip slot
--
-- This is 'protocolLedgerView' without the ticking. It asks the state's own
-- forecast for the slot that forecast is anchored at, which satisfies
-- 'forecastFor''s precondition and which every implementation answers by
-- projecting the state it already holds -- no TICKF, no ticked state, no
-- ledger tables. So it is cheap enough to call once per entry of a @k@-deep
-- history.
--
-- Total: the only way 'forecastFor' can fail is by being asked for a slot
-- outside the forecast's range, and the forecast's own anchor is always in range.
ledgerViewOfTip ::
  forall blk mk.
  LedgerSupportsProtocol blk =>
  LedgerConfig blk ->
  LedgerState blk mk ->
  LedgerView (BlockProtocol blk)
ledgerViewOfTip cfg st =
  case runExcept $ forecastFor forecast slot of
    Right lv -> lv
    Left err ->
      error $
        "ledgerViewOfTip: forecast refused its own anchor: " <> show err
 where
  forecast = ledgerViewForecastAt cfg st
  -- At genesis there is no tip slot; slot 0 is the first slot any header could
  -- occupy, and the forecast's anchor is 'Origin', so it is in range.
  slot = fromWithOrigin (SlotNo 0) $ getTipSlot st

class
  ( BlockSupportsProtocol blk
  , UpdateLedger blk
  , ValidateEnvelope blk
  ) =>
  LedgerSupportsProtocol blk
  where
  -- | Extract the ledger view from the given ticked ledger state
  --
  -- See 'ledgerViewForecastAt' for a discussion and precise definition of the
  -- relation between this and forecasting.
  protocolLedgerView ::
    LedgerConfig blk ->
    Ticked (LedgerState blk) mk ->
    LedgerView (BlockProtocol blk)

  -- | Get a forecast at the given ledger state.
  --
  -- This forecast can be used to validate headers of blocks within the range of
  -- the forecast. These blocks need to live on a chain that fits on the last
  -- applied block of the given ledger.
  --
  -- The range of the forecast should allow to validate a sufficient number of
  -- headers to validate an alternative chain longer than ours, so that chain
  -- selection can decide whether or not we prefer the alternative chain to our
  -- current chain. In addition, it would be helpful, though not essential, if
  -- we can look further ahead than that, as this would improve sync
  -- performance.
  --
  -- NOTE (difference between 'ledgerViewForecastAt' and 'applyChainTick'):
  -- Both 'ledgerViewForecastAt' and 'applyChainTick' can be used to obtain
  -- a protocol ledger view for a future slot. The difference between the two
  -- is that 'applyChainTick' assumes no blocks are present between the current
  -- ledger tip and the specified 'SlotNo', whereas 'ledgerViewForecastAt'
  -- cannot make such an assumption. Thus, 'applyChainTick' cannot fail, whereas
  -- the forecast returned by 'ledgerViewForecastAt' might report an
  -- 'OutsideForecastRange' for the same 'SlotNo'. We expect the two functions
  -- to produce the same view whenever the 'SlotNo' /is/ in range, however.
  -- More precisely:
  --
  -- If
  --
  -- >    forecastFor (ledgerViewForecastAt cfg st) for
  -- > == Just view
  --
  -- then
  --
  -- >    protocolLedgerView cfg (applyChainTick cfg for st)
  -- > == view
  --
  -- See 'lemma_ledgerViewForecastAt_applyChainTick'.
  ledgerViewForecastAt ::
    HasCallStack =>
    LedgerConfig blk ->
    LedgerState blk mk ->
    Forecast (LedgerView (BlockProtocol blk))

-- | Relation between 'ledgerViewForecastAt' and 'applyChainTick'
_lemma_ledgerViewForecastAt_applyChainTick ::
  ( LedgerSupportsProtocol blk
  , Eq (LedgerView (BlockProtocol blk))
  ) =>
  LedgerConfig blk ->
  LedgerState blk mk ->
  Forecast (LedgerView (BlockProtocol blk)) ->
  SlotNo ->
  Either String ()
_lemma_ledgerViewForecastAt_applyChainTick cfg st forecast for
  | NotOrigin for >= ledgerTipSlot st
  , let lhs = forecastFor forecast for
        rhs =
          protocolLedgerView cfg
            . applyChainTick OmitLedgerEvents cfg for
            . forgetLedgerTables
            $ st
  , Right lhs' <- runExcept lhs
  , lhs' /= rhs =
      Left $
        unlines
          [ "ledgerViewForecastAt /= protocolLedgerView . applyChainTick:"
          , show lhs'
          , " /= "
          , show rhs
          ]
  | otherwise =
      Right ()
