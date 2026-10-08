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

import qualified Cardano.Ledger.Core as LedgerCore
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Ledger.Shelley.LedgerState (nesStakePoolDistrG)
import qualified Cardano.Protocol.TPraos.API as SL
import Control.Monad.Except (MonadError (throwError))
import qualified Lens.Micro
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Forecast
import Ouroboros.Consensus.HardFork.History.Util
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol (..)
  )
import Ouroboros.Consensus.Protocol.Leios (Leios, LeiosCrypto)
import Ouroboros.Consensus.Protocol.Praos (Praos)
import qualified Ouroboros.Consensus.Protocol.Praos as Praos (PraosCrypto)
import qualified Ouroboros.Consensus.Protocol.Praos.Views as Praos
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

-- | 'protocolLedgerView' for any protocol whose ledger view is Praos's.
praosProtocolLedgerView ::
  forall proto era mk.
  ShelleyCompatible proto era =>
  Ticked LedgerState (ShelleyBlock proto era) mk ->
  Praos.PraosLedgerView
praosProtocolLedgerView st =
  let nes = tickedShelleyLedgerState st

      pparam :: forall a. Lens.Micro.Lens' (LedgerCore.PParams era) a -> a
      pparam lens = getPParams nes Lens.Micro.^. lens
   in Praos.PraosLedgerView
        { Praos.plvPoolDistr = nes Lens.Micro.^. nesStakePoolDistrG
        , Praos.plvMaxBodySize = pparam LedgerCore.ppMaxBBSizeL
        , Praos.plvMaxHeaderSize = pparam LedgerCore.ppMaxBHSizeL
        , Praos.plvProtocolVersion = pparam LedgerCore.ppProtocolVersionL
        }

-- | 'ledgerViewForecastAt' for any protocol whose ledger view is Praos's.
--
-- 'SL.currentForecast' is a projection of the state with no TICKF, and
-- 'SL.futureForecast' is TICKF and then that same projection, which is what
-- makes this agree with 'praosProtocolLedgerView' by construction.
praosLedgerViewForecastAt ::
  forall proto era mk.
  ShelleyCompatible proto era =>
  LedgerConfig (ShelleyBlock proto era) ->
  LedgerState (ShelleyBlock proto era) mk ->
  Forecast Praos.PraosLedgerView
praosLedgerViewForecastAt cfg ledgerState = Forecast at $ \for ->
  if
    | NotOrigin for == at ->
        return $
          Praos.forecastToPraosLedgerView (SL.currentForecast shelleyLedgerState)
    | for < maxFor ->
        return $
          Praos.forecastToPraosLedgerView $
            SL.futureForecast globals for shelleyLedgerState
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
  protocolLedgerView _cfg = praosProtocolLedgerView
  ledgerViewForecastAt = praosLedgerViewForecastAt

instance
  ( ShelleyCompatible (Leios crypto) era
  , SL.EraForecast era
  , LeiosCrypto crypto
  ) =>
  LedgerSupportsProtocol (ShelleyBlock (Leios crypto) era)
  where
  protocolLedgerView _cfg = praosProtocolLedgerView
  ledgerViewForecastAt = praosLedgerViewForecastAt
