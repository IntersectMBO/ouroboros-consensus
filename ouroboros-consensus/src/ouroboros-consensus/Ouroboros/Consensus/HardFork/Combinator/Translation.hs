{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}

module Ouroboros.Consensus.HardFork.Combinator.Translation
  ( -- * Translate from one era to the next
    EraTranslation (..)
  , EraTranslationM (..)
  , trivialEraTranslation
  , trivialEraTranslationM
  ) where

import Data.SOP.InPairs (InPairs (..), RequiringBoth (..))
import NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))
import Ouroboros.Consensus.HardFork.Combinator.State.Types
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.TypeFamilyWrappers

{-------------------------------------------------------------------------------
  Translate from one era to the next
-------------------------------------------------------------------------------}

data EraTranslationM m xs = EraTranslationM
  { translateLedgerState ::
      !(InPairs (RequiringBoth WrapLedgerConfig (TranslateLedgerState m)) xs)
  , crossEraForecast ::
      !(InPairs (RequiringBoth WrapLedgerConfig (CrossEraForecaster (LedgerState m) WrapLedgerView)) xs)
  }
  deriving
    NoThunks
    via OnlyCheckWhnfNamed "EraTranslationM" (EraTranslationM m xs)

data EraTranslation xs = EraTranslation
  { translateChainDepState ::
      !(InPairs (RequiringBoth WrapConsensusConfig (Translate WrapChainDepState)) xs)
  }
  deriving
    NoThunks
    via OnlyCheckWhnfNamed "EraTranslation" (EraTranslation xs)

trivialEraTranslation :: EraTranslation '[blk]
trivialEraTranslation =
  EraTranslation
    { translateChainDepState = PNil
    }

trivialEraTranslationM :: EraTranslationM m '[blk]
trivialEraTranslationM =
  EraTranslationM
    { translateLedgerState = PNil
    , crossEraForecast = PNil
    }
