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
      -- | Translate a @'Ticked' 'LedgerState'@ from one era to a 'LedgerState'
      -- in the next era.
      --
      -- The HFC will always supply a 'LedgerState' that has just been ticked
      -- across the era boundary. The translations provided here should *not* do
      -- any ticking. The HFC will later tick the result to whatever slot was
      -- requested. Note that it is important that this second ticking operation
      -- can recognize, if applicable for the underlying ledger, that the epoch
      -- boundary was already passed, such that epoch-related updates are not
      -- performed twice.
      --
      -- For more context/motivation, see
      -- <https://github.com/input-output-hk/ouroboros-consensus/issues/339>.
      translateLedgerState   :: InPairs (RequiringBoth WrapLedgerConfig    (TickedTranslate LedgerState)) xs
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
