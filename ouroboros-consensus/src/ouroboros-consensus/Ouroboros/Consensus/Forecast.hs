{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Consensus.Forecast
  ( Forecast (..)
  , OutsideForecastRange (..)
  , constantForecastInRange
  , constantForecastOf
  , mapForecast
  , trivialForecast

    -- * Utilities for constructing forecasts
  , crossEraForecastBound
  ) where

import Control.Exception (Exception)
import Control.Monad (guard)
import Control.Monad.Except (Except, throwError)
import Data.Word (Word64)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.HardFork.History.Util (addSlots)
import Ouroboros.Consensus.Ledger.Basics (GetTip, getTipSlot)

data Forecast a = Forecast
  { forecastAt :: WithOrigin SlotNo
  , -- Precondition: @At s >= forecastAt@
    forecastFor :: SlotNo -> Except OutsideForecastRange a
  }

mapForecast :: (a -> b) -> Forecast a -> Forecast b
mapForecast f (Forecast at for) =
  Forecast
    { forecastAt = at
    , forecastFor = fmap f . for
    }

-- | Trivial forecast of values of type @()@ performed by an instance of
-- 'GetTip'.
--
-- Specialization of 'constantForecast'.
trivialForecast :: GetTip b => b mk -> Forecast ()
trivialForecast x = constantForecastOf () (getTipSlot x)

-- | Forecast where the values are never changing
--
-- This is primarily useful for tests; the forecast range is infinite, but we
-- do still check the precondition, to catch any bugs.
constantForecastOf :: a -> WithOrigin SlotNo -> Forecast a
constantForecastOf = constantForecastInRange Nothing

-- | Forecast where the values are never changing, in a certain window.
--
-- This is primarily useful for tests; the forecast range is finite, and we
-- do still check the precondition, to catch any bugs.
constantForecastInRange :: Maybe SlotNo -> a -> WithOrigin SlotNo -> Forecast a
constantForecastInRange range' a at =
  Forecast
    { forecastAt = at
    , forecastFor = forecastForWithRange range'
    }
 where
  forecastForWithRange Nothing = \for ->
    if NotOrigin for >= at
      then return a
      else error "constantForecastOf: precondition violated"
  forecastForWithRange (Just range) = \for ->
    let outsideForecastMaxFor = succWithOrigin at + range
     in if for >= outsideForecastMaxFor
          then
            throwError $
              OutsideForecastRange
                { outsideForecastAt = at
                , outsideForecastMaxFor
                , outsideForecastFor = for
                }
          else forecastForWithRange Nothing for

data OutsideForecastRange
  = OutsideForecastRange
  { outsideForecastAt :: !(WithOrigin SlotNo)
  -- ^ The slot for which the forecast was obtained
  , outsideForecastMaxFor :: !SlotNo
  -- ^ Exclusive upper bound on the range of the forecast
  , outsideForecastFor :: !SlotNo
  -- ^ The slot for which we requested a value
  }
  deriving (Show, Eq)

instance Exception OutsideForecastRange

{-------------------------------------------------------------------------------
  Utilities for constructing forecasts
-------------------------------------------------------------------------------}

-- | Compute the upper bound for a range for a forecast across eras.
--
-- We have to be very careful here in how we compute the maximum lookahead.
-- As long as we are in a single era, things look like this:
--
-- >                                          /-------------------\
-- >                                          |                   |
-- > chain     ... - block - block - block [block]                |
-- >                                   |                          v
-- > ledger                           TIP                  VIEW
--
-- where @TIP@ is the current ledger tip and @VIEW@ is the last ledger view we
-- can forecast, because the next block @[block]@ to arrive will take effect in
-- the next leger state after @VIEW@. Note that if the maximum lookahead is
-- zero, this looks like
--
-- > chain     ... - block - block - block [block]
-- >                                   |      |
-- > ledger                           TIP
--
-- where @[block]@ can have immediate changes on the ledger, and so we can't
-- look ahead at all (of course, we always know the /current/ @TIP@).
--
-- Note that blocks arriving /after/ @[block]@ can only take effect /later/ than
-- @[block]@, and so they are not relevant for computing the maximum slot number
-- we can compute a ledger view for.
--
-- Now, if we are near an era transition, this picture gets a bit more
-- complicated. /If/ the next block is still in this era (that is, unless we are
-- /right/ at the edge), then that imposes /one/ constraint, as before. However,
-- the first block in the /next/ era imposes an /additional/ constraint:
--
-- >                      ~
-- >                      ~    /------------------\
-- >                      ~    |                  |
-- >          /---------- ~ ---|----------\       |
-- >          |           ~    |          |       |
-- > block [block]        ~ [block']      |       |
-- >   |                  ~               v       v
-- >  TIP                 ~         VIEW
-- >                      ~
--
-- There are no restrictions on the relative values of these two maximum
-- lookahead values. This means that it's quite possible for the next era to
-- have a /smaller/ lookahead (to re-iterate, since that era has not yet begun,
-- the first block in that era is at the transition, and so the maximum
-- lookahead applies from the transition point):
--
-- >                      ~
-- >                      ~    /----------\
-- >                      ~    |          |
-- >          /---------- ~ ---|----------|-------\
-- >          |           ~    |          |       |
-- > block [block]        ~ [block']      |       |
-- >   |                  ~               v       v
-- >  TIP                 ~         VIEW
-- >                      ~
--
-- Indeed, if the next era has zero lookahead, when the first block of the next
-- era comes it, it can make changes immediately, and so we can't even know what
-- the view at the transition point is.
--
-- Note that if there can be no more blocks in this era, the maximum lookahead
-- of the current era is irrelevant:
--
-- >       ~
-- >       ~    /----------\
-- >       ~    |          |
-- >       ~    |          |
-- >       ~    |          |
-- > block ~ [block']      |
-- >   |   ~               v
-- >  TIP  ~         VIEW
-- >       ~
--
-- We can therefore compute the earliest 'SlotNo' the next block in this era
-- (if any) can make changes to the ledger state, as well as the earliest
-- 'SlotNo' the first block in the next era can; their @minimum@ will serve as
-- an exclusive upper bound for the forecast range.
crossEraForecastBound ::
  -- | Current tip (the slot the forecast is at)
  WithOrigin SlotNo ->
  -- | Slot at which the transition to the next era happens
  SlotNo ->
  -- | Max lookeahead in the current era
  Word64 ->
  -- | Max lookeahead in the next era
  Word64 ->
  SlotNo
crossEraForecastBound currentTip transitionSlot currentLookahead nextLookahead =
  maybe boundFromNextEra (min boundFromNextEra) boundFromCurrentEra
 where
  tipSucc :: SlotNo
  tipSucc = succWithOrigin currentTip

  -- Upper bound arising from blocks in the current era
  --
  -- 'Nothing' if there are no more blocks in this era
  boundFromCurrentEra :: Maybe SlotNo
  boundFromCurrentEra = do
    guard (tipSucc < transitionSlot)
    return $ addSlots currentLookahead tipSucc

  -- Upper bound arising from blocks in the next era
  boundFromNextEra :: SlotNo
  boundFromNextEra = addSlots nextLookahead transitionSlot
