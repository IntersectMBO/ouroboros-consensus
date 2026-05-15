{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}

module Ouroboros.Consensus.HardFork.Combinator.Translation
  ( -- * Translate from one era to the next
    EraTranslation (..)
  , trivialStateRefTranslation
  , StateRefTranslation (..)
  , trivialEraTranslation
  ) where

import Data.SOP.InPairs (InPairs (..), RequiringBoth (..))
import Ouroboros.Consensus.HardFork.Combinator.State.Types
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.TypeFamilyWrappers

{-------------------------------------------------------------------------------
  Translate from one era to the next
-------------------------------------------------------------------------------}

data StateRefTranslation m xs = StateRefTranslation
  { translateLedgerState ::
      !(InPairs (RequiringBoth WrapLedgerConfig (TranslateLedgerState m)) xs)
  }

data EraTranslation xs = EraTranslation
  { translateChainDepState ::
      !(InPairs (RequiringBoth WrapConsensusConfig (Translate WrapChainDepState)) xs)
  , crossEraForecast ::
      !(InPairs (RequiringBoth WrapLedgerConfig (CrossEraForecaster LedgerState WrapLedgerView)) xs)
  }

trivialStateRefTranslation :: StateRefTranslation m '[blk]
trivialStateRefTranslation = StateRefTranslation PNil

trivialEraTranslation :: EraTranslation '[blk]
trivialEraTranslation =
  EraTranslation
    { crossEraForecast = PNil
    , translateChainDepState = PNil
    }
