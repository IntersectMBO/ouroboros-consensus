{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

module Ouroboros.Consensus.HardFork.Combinator.Translation (
    -- * Translate from one era to the next
    EraTranslation (..)
  , trivialEraTranslation
  ) where

import           Data.SOP.InPairs (InPairs (..), RequiringBoth)
import           NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers
{-------------------------------------------------------------------------------
  Translate from one era to the next
-------------------------------------------------------------------------------}

data EraTranslation xs = EraTranslation {
      crossEraTickLedgerState   ::
        InPairs (RequiringBoth WrapLedgerConfig    CrossEraTickLedgerState)                         xs
    , crossEraTickChainDepState ::
        InPairs (RequiringBoth WrapConsensusConfig CrossEraTickChainDepState)                       xs
    , crossEraForecast          ::
        InPairs (RequiringBoth WrapLedgerConfig    (CrossEraForecaster LedgerState WrapLedgerView)) xs
    }
  deriving NoThunks
       via OnlyCheckWhnfNamed "EraTranslation" (EraTranslation xs)

trivialEraTranslation :: EraTranslation '[blk]
trivialEraTranslation = EraTranslation {
      crossEraTickLedgerState   = PNil
    , crossEraTickChainDepState = PNil
    , crossEraForecast          = PNil
    }
