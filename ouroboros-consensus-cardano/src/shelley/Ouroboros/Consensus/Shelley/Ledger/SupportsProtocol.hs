{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains 'SupportsProtocol' instances tying the ledger and
-- protocol together. Since these instances reference both ledger concerns and
-- protocol concerns, it is the one class where we cannot provide a generic
-- instance for 'ShelleyBlock'.
module Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol () where

import qualified Cardano.Ledger.Core as LedgerCore
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Protocol.TPraos.API as SL
import Control.Monad.Except (MonadError (throwError))
import Data.Coerce (coerce)
import qualified Lens.Micro
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Forecast
import Ouroboros.Consensus.HardFork.History.Util
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol (..)
  )
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Protocol.Abstract (translateLedgerView)
import Ouroboros.Consensus.Protocol.Praos (Praos)
import qualified Ouroboros.Consensus.Protocol.Praos.Views as Praos
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Shelley.Ledger.Block
import Ouroboros.Consensus.Shelley.Ledger.Ledger
import Ouroboros.Consensus.Shelley.Ledger.Protocol ()
import Ouroboros.Consensus.Shelley.Protocol.Abstract ()
import Ouroboros.Consensus.Shelley.Protocol.Praos ()
import Ouroboros.Consensus.Shelley.Protocol.TPraos ()
import Data.SOP.Constraint (All)
import NoThunks.Class

instance
  ( ShelleyCompatible (TPraos crypto) era
  , CanStowLedgerTables (LedgerState (ShelleyBlock (TPraos crypto) era))
  , CanStowLedgerTables (Ticked (LedgerState (ShelleyBlock (TPraos crypto) era)))
  , All (KVConstraintsMK (ShelleyBlock (TPraos crypto) era) DiffMK) (TablesForBlock (ShelleyBlock (TPraos crypto) era))
  , LedgerTableConstraints (ShelleyBlock (TPraos crypto) era)
  , forall mk. (Eq (LedgerState (ShelleyBlock (TPraos crypto) era) mk))
  , forall mk. (Show (LedgerState (ShelleyBlock (TPraos crypto) era) mk))
  , forall mk. (NoThunks (LedgerState (ShelleyBlock (TPraos crypto) era) mk))
  ) =>
  LedgerSupportsProtocol (ShelleyBlock (TPraos crypto) era)
  where
  protocolLedgerView _cfg = SL.currentLedgerView . tickedShelleyLedgerState

  -- Extra context available in
  -- https://github.com/IntersectMBO/ouroboros-consensus/blob/main/docs/website/contents/for-developers/HardWonWisdom.md#why-doesnt-ledger-code-ever-return-pasthorizonexception
  ledgerViewForecastAt cfg ledgerState = Forecast at $ \for ->
    if
      | NotOrigin for == at ->
          return $ SL.currentLedgerView shelleyLedgerState
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

    futureLedgerView :: SlotNo -> SL.LedgerView
    futureLedgerView =
      either
        (\e -> error ("futureLedgerView failed: " <> show e))
        id
        . SL.futureLedgerView globals shelleyLedgerState

    -- Exclusive upper bound
    maxFor :: SlotNo
    maxFor = addSlots swindow $ succWithOrigin at

instance
  ( ShelleyCompatible (Praos crypto) era
  , ShelleyCompatible (TPraos crypto) era
  , CanStowLedgerTables (LedgerState (ShelleyBlock (Praos crypto) era))
  , CanStowLedgerTables (Ticked (LedgerState (ShelleyBlock (Praos crypto) era)))
  , All (KVConstraintsMK (ShelleyBlock (Praos crypto) era) DiffMK) (TablesForBlock (ShelleyBlock (Praos crypto) era))
  , All (KVConstraintsMK (ShelleyBlock (TPraos crypto) era) DiffMK) (TablesForBlock (ShelleyBlock (TPraos crypto) era))
  , LedgerTableConstraints (ShelleyBlock (Praos crypto) era)
  , forall mk. (Eq (LedgerState (ShelleyBlock (Praos crypto) era) mk))
  , forall mk. (Show (LedgerState (ShelleyBlock (Praos crypto) era) mk))
  , forall mk. (NoThunks (LedgerState (ShelleyBlock (Praos crypto) era) mk))
  , CanStowLedgerTables (LedgerState (ShelleyBlock (TPraos crypto) era))
  , CanStowLedgerTables (Ticked (LedgerState (ShelleyBlock (TPraos crypto) era)))
  , All (KVConstraintsMK (ShelleyBlock (TPraos crypto) era) DiffMK) (TablesForBlock (ShelleyBlock (TPraos crypto) era))
  , LedgerTableConstraints (ShelleyBlock (TPraos crypto) era)
  , forall mk. (Eq (LedgerState (ShelleyBlock (TPraos crypto) era) mk))
  , forall mk. (Show (LedgerState (ShelleyBlock (TPraos crypto) era) mk))
  , forall mk. (NoThunks (LedgerState (ShelleyBlock (TPraos crypto) era) mk))
  ) =>
  LedgerSupportsProtocol (ShelleyBlock (Praos crypto) era)
  where
  protocolLedgerView _cfg st =
    let nes = tickedShelleyLedgerState st

        SL.NewEpochState{nesPd} = nes

        pparam :: forall a. Lens.Micro.Lens' (LedgerCore.PParams era) a -> a
        pparam lens = getPParams nes Lens.Micro.^. lens
     in Praos.LedgerView
          { Praos.lvPoolDistr = nesPd
          , Praos.lvMaxBodySize = pparam LedgerCore.ppMaxBBSizeL
          , Praos.lvMaxHeaderSize = pparam LedgerCore.ppMaxBHSizeL
          , Praos.lvProtocolVersion = pparam LedgerCore.ppProtocolVersionL
          }

  -- \| Currently the Shelley+ ledger is hard-coded to produce a TPraos ledger
  -- view. Since we can convert them, we piggy-back on this to get a Praos
  -- ledger view. Ultimately, we will want to liberalise the ledger code
  -- slightly.
  ledgerViewForecastAt cfg st =
    mapForecast (translateLedgerView (Proxy @(TPraos crypto, Praos crypto))) $
      ledgerViewForecastAt @(ShelleyBlock (TPraos crypto) era) cfg st'
   where
    st' :: LedgerState (ShelleyBlock (TPraos crypto) era) EmptyMK
    st' =
      ShelleyLedgerState
        { shelleyLedgerTip = coerceTip <$> shelleyLedgerTip st
        , shelleyLedgerState = shelleyLedgerState st
        , shelleyLedgerTransition = shelleyLedgerTransition st
        , shelleyLedgerTables = emptyLedgerTables
        }
    coerceTip (ShelleyTip slot block hash) = ShelleyTip slot block (coerce hash)
