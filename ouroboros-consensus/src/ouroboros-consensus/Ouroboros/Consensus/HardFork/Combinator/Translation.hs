{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}

module Ouroboros.Consensus.HardFork.Combinator.Translation (
    -- * Translate from one era to the next
    EraTranslation (..)
  , trivialEraTranslation
  ) where

import           Data.SOP.InPairs (InPairs (..), RequiringBoth (..))
import           NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers
{-------------------------------------------------------------------------------
  Translate from one era to the next
-------------------------------------------------------------------------------}

data EraTranslation xs = EraTranslation {
      -- | For each pair @(x, y)@ of subsequent eras, describe how to construct
      -- the initial ledger state for @y@ from the last ledger state in @x@.
      --
      -- When ticking across an era boundary, the HFC will first invoke this and
      -- then tick the resulting ledger state (in @y@) to the requested slot.
      --
      -- The resulting ledger state must summarize every relevant aspect of what
      -- came before the new era. This is intentionally vague; for example,
      -- ticking in @y@ might work rather differntly than in @x@, and so certain
      -- aspects of the ticking logic of @x@ might need to happen as part of
      -- 'translateLedgerState'. For a concrete example in Cardano, see
      -- 'translateLedgerStateBabbageToConwayWrapper'.
      translateLedgerState   :: InPairs (RequiringBoth WrapLedgerConfig    (Translate LedgerState))       xs
      -- | For each pair @(x, y)@ of subsequent eras, describe how to construct
      -- the initial chain-dependent state for @y@ from the last chain-dep state
      -- in @x@.
      --
      -- When ticking across an era boundary, the HFC will first invoke this and
      -- then tick the resulting chain-dep state (in @y@) to the requested slot.
    , translateChainDepState :: InPairs (RequiringBoth WrapConsensusConfig (Translate WrapChainDepState)) xs
    , crossEraForecast       :: InPairs (RequiringBoth WrapLedgerConfig    (CrossEraForecaster LedgerState WrapLedgerView)) xs
    }
  deriving NoThunks
       via OnlyCheckWhnfNamed "EraTranslation" (EraTranslation xs)

trivialEraTranslation :: EraTranslation '[blk]
trivialEraTranslation = EraTranslation {
      translateLedgerState   = PNil
    , crossEraForecast       = PNil
    , translateChainDepState = PNil
    }
