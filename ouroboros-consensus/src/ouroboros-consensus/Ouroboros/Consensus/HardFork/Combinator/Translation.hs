{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}

module Ouroboros.Consensus.HardFork.Combinator.Translation
  ( -- * Translate from one era to the next
    EraTranslation (..)
  , trivialEraTranslation
  ) where

import Data.SOP.InPairs (InPairs (..), RequiringBoth (..))
import NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))
import Ouroboros.Consensus.HardFork.Combinator.State.Types
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.TypeFamilyWrappers

{-------------------------------------------------------------------------------
  Translate from one era to the next
-------------------------------------------------------------------------------}

data EraTranslation m xs = EraTranslation
  { translateLedgerState ::
      !(InPairs (RequiringBoth WrapLedgerConfig (TranslateLedgerState m)) xs)
  , translateChainDepState ::
      !(InPairs (RequiringBoth WrapConsensusConfig (Translate WrapChainDepState)) xs)
  , crossEraForecast ::
      !(InPairs (RequiringBoth WrapLedgerConfig (CrossEraForecaster (LedgerState m) WrapLedgerView)) xs)
  }
  deriving
    NoThunks
    via OnlyCheckWhnfNamed "EraTranslation" (EraTranslation m xs)

trivialEraTranslation :: EraTranslation m '[blk]
trivialEraTranslation =
  EraTranslation
    { translateLedgerState = PNil
    , crossEraForecast = PNil
    , translateChainDepState = PNil
    }
