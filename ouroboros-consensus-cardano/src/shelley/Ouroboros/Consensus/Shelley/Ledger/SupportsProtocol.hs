{-# LANGUAGE DataKinds #-}
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
import Ouroboros.Consensus.Protocol.Praos (Praos, PraosWithLeios)
import qualified Ouroboros.Consensus.Protocol.Praos as Praos (PraosCrypto)
import qualified Ouroboros.Consensus.Protocol.Praos.Views as Praos
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Shelley.Ledger.Block
import Ouroboros.Consensus.Shelley.Ledger.Ledger
import Ouroboros.Consensus.Shelley.Ledger.Protocol ()
import Ouroboros.Consensus.Shelley.Protocol.Abstract ()
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

-- | What both Praos-family instances below do, given how their protocol reads
-- a forecast.
--
-- 'SL.currentForecast' is 'SL.mkForecast': a projection of the state, with no
-- TICKF. 'SL.futureForecast' is TICKF and then that same projection. Taking the
-- projection as an argument is what makes 'protocolLedgerView' and
-- 'ledgerViewForecastAt' agree by construction, instead of by two copies of it
-- staying in step.
praosLedgerViewForecastAt ::
  forall proto era mk lv.
  ShelleyCompatible proto era =>
  (forall t. SL.Forecast t era -> lv) ->
  LedgerConfig (ShelleyBlock proto era) ->
  LedgerState (ShelleyBlock proto era) mk ->
  Forecast lv
praosLedgerViewForecastAt toLedgerView cfg ledgerState = Forecast at $ \for ->
  if
    | NotOrigin for == at ->
        return $ toLedgerView (SL.currentForecast shelleyLedgerState)
    | for < maxFor ->
        return $ toLedgerView (SL.futureForecast globals for shelleyLedgerState)
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
  at = ledgerTipSlot ledgerState

  -- Exclusive upper bound
  maxFor :: SlotNo
  maxFor = addSlots (SL.stabilityWindow globals) $ succWithOrigin at

instance
  ( ShelleyCompatible (Praos crypto) era
  , SL.EraForecast era
  , Praos.PraosCrypto crypto
  ) =>
  LedgerSupportsProtocol (ShelleyBlock (Praos crypto) era)
  where
  protocolLedgerView _cfg =
    Praos.forecastToPraosLedgerView . SL.currentForecast . tickedShelleyLedgerState

  ledgerViewForecastAt = praosLedgerViewForecastAt Praos.forecastToPraosLedgerView

instance
  ( ShelleyCompatible (PraosWithLeios crypto) era
  , SL.EraForecast era
  , Dijkstra.DijkstraEraForecast era
  , Praos.PraosCrypto crypto
  ) =>
  LedgerSupportsProtocol (ShelleyBlock (PraosWithLeios crypto) era)
  where
  protocolLedgerView _cfg =
    Praos.forecastToPraosWithLeiosLedgerView
      . SL.currentForecast
      . tickedShelleyLedgerState

  ledgerViewForecastAt =
    praosLedgerViewForecastAt Praos.forecastToPraosWithLeiosLedgerView
