{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains 'SupportsProtocol' instances tying the ledger and
-- protocol together. Since these instances reference both ledger concerns and
-- protocol concerns, it is the one class where we cannot provide a generic
-- instance for 'ShelleyBlock'.
module Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol () where

import qualified Cardano.Ledger.Dijkstra.Forecast as Dijkstra
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Protocol.TPraos.API as SL
import Control.Monad.Except (MonadError (throwError))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Forecast
import Ouroboros.Consensus.HardFork.History.Util
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol (..)
  )
import Ouroboros.Consensus.Protocol.Praos (Praos)
import qualified Ouroboros.Consensus.Protocol.Praos as Praos (PraosCrypto)
import qualified Ouroboros.Consensus.Protocol.Praos.Views as Praos
import Ouroboros.Consensus.Protocol.Praos2 (Praos2)
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Shelley.Ledger.Block
import Ouroboros.Consensus.Shelley.Ledger.Ledger
import Ouroboros.Consensus.Shelley.Ledger.Protocol ()
import Ouroboros.Consensus.Shelley.Protocol.Abstract ()
import Ouroboros.Consensus.Shelley.Protocol.Praos ()
import Ouroboros.Consensus.Shelley.Protocol.TPraos ()

instance
  ( ShelleyCompatible (TPraos crypto) era
  , SL.ShelleyEraForecast era
  , SL.PraosCrypto crypto
  ) =>
  LedgerSupportsProtocol (ShelleyBlock (TPraos crypto) era)
  where
  protocolLedgerView _cfg =
    SL.forecastToTPraosLedgerView . SL.currentForecast . tickedShelleyLedgerState

  -- Extra context available in
  -- https://github.com/IntersectMBO/ouroboros-consensus/blob/main/docs/website/contents/for-developers/HardWonWisdom.md#why-doesnt-ledger-code-ever-return-pasthorizonexception
  ledgerViewForecastAt cfg ledgerState = Forecast at $ \for ->
    if
      | NotOrigin for == at ->
          return $ SL.forecastToTPraosLedgerView (SL.currentForecast shelleyLedgerState)
      | for < maxFor ->
          return $ futureLedgerView for
      | otherwise ->
          throwError $
            OutsideForecastRange
              { outsideForecastAt = at
              , outsideForecastMaxFor = maxFor
              , outsideForecastFor = for
              }
   where
    ShelleyLedgerState{shelleyLedgerState} = ledgerState
    globals = shelleyLedgerGlobals cfg
    swindow = SL.stabilityWindow globals
    at = ledgerTipSlot ledgerState

    futureLedgerView :: SlotNo -> SL.TPraosLedgerView
    futureLedgerView for =
      SL.forecastToTPraosLedgerView $
        SL.futureForecast globals for shelleyLedgerState

    -- Exclusive upper bound
    maxFor :: SlotNo
    maxFor = addSlots swindow $ succWithOrigin at

instance
  ( ShelleyCompatible (Praos crypto) era
  , SL.EraForecast era
  , Praos.PraosCrypto crypto
  ) =>
  LedgerSupportsProtocol (ShelleyBlock (Praos crypto) era)
  where
  protocolLedgerView = protocolLedgerViewPolyPraos
  ledgerViewForecastAt = ledgerViewForecastAtPolyPraos

-- | 'protocolLedgerView' for every Praos.
--
-- Uses the same projection as 'ledgerViewForecastAtPolyPraos', so the two agree.
protocolLedgerViewPolyPraos ::
  forall proto era mk.
  ( Praos.ForecastsLeios proto era
  , SL.EraForecast era
  ) =>
  LedgerConfig (ShelleyBlock proto era) ->
  Ticked LedgerState (ShelleyBlock proto era) mk ->
  Praos.PolyPraosLedgerView proto
protocolLedgerViewPolyPraos _cfg =
  Praos.forecastToPolyPraosLedgerView . SL.currentForecast . tickedShelleyLedgerState

-- | 'ledgerViewForecastAt' for every Praos.
ledgerViewForecastAtPolyPraos ::
  forall proto era mk.
  ( ShelleyCompatible proto era
  , Praos.ForecastsLeios proto era
  ) =>
  LedgerConfig (ShelleyBlock proto era) ->
  LedgerState (ShelleyBlock proto era) mk ->
  Forecast (Praos.PolyPraosLedgerView proto)
ledgerViewForecastAtPolyPraos cfg ledgerState = Forecast at $ \for ->
  if
    | NotOrigin for == at ->
        return $
          Praos.forecastToPolyPraosLedgerView (SL.currentForecast shelleyLedgerState)
    | for < maxFor ->
        return $ futureLedgerView for
    | otherwise ->
        throwError $
          OutsideForecastRange
            { outsideForecastAt = at
            , outsideForecastMaxFor = maxFor
            , outsideForecastFor = for
            }
 where
  ShelleyLedgerState{shelleyLedgerState} = ledgerState
  globals = shelleyLedgerGlobals cfg
  swindow = SL.stabilityWindow globals
  at = ledgerTipSlot ledgerState

  futureLedgerView :: SlotNo -> Praos.PolyPraosLedgerView proto
  futureLedgerView for =
    Praos.forecastToPolyPraosLedgerView $
      SL.futureForecast globals for shelleyLedgerState

  -- Exclusive upper bound
  maxFor :: SlotNo
  maxFor = addSlots swindow $ succWithOrigin at

instance
  ( ShelleyCompatible (Praos2 c) era
  , Dijkstra.DijkstraEraForecast era
  , SL.EraForecast era
  ) =>
  LedgerSupportsProtocol (ShelleyBlock (Praos2 c) era)
  where
  protocolLedgerView = protocolLedgerViewPolyPraos
  ledgerViewForecastAt = ledgerViewForecastAtPolyPraos
