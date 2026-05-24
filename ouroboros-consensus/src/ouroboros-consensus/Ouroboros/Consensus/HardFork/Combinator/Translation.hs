{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}

module Ouroboros.Consensus.HardFork.Combinator.Translation
  ( -- * Translate from one era to the next
    EraTranslation (..)
  , trivialStateHandleTranslation
  , StateHandleTranslation (..)
  , trivialEraTranslation
  ) where

import Data.SOP.InPairs (InPairs (..), RequiringBoth (..))
import Ouroboros.Consensus.HardFork.Combinator.State.Types
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.TypeFamilyWrappers

{-------------------------------------------------------------------------------
  Translate from one era to the next
-------------------------------------------------------------------------------}

data StateHandleTranslation m xs = StateHandleTranslation
  { translateLedgerState ::
      !(InPairs (RequiringBoth WrapLedgerConfig (TranslateLedgerState m)) xs)
  }

data EraTranslation xs = EraTranslation
  { translateChainDepState ::
      !(InPairs (RequiringBoth WrapConsensusConfig (Translate WrapChainDepState)) xs)
  , crossEraForecast ::
      !(InPairs (RequiringBoth WrapLedgerConfig (CrossEraForecaster LedgerState WrapLedgerView)) xs)
  }

trivialStateHandleTranslation :: StateHandleTranslation m '[blk]
trivialStateHandleTranslation = StateHandleTranslation PNil

trivialEraTranslation :: EraTranslation '[blk]
trivialEraTranslation =
  EraTranslation
    { crossEraForecast = PNil
    , translateChainDepState = PNil
    }
