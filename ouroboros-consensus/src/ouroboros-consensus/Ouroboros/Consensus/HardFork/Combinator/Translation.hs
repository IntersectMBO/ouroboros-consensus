{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}

module Ouroboros.Consensus.HardFork.Combinator.Translation
  ( -- * Translate from one era to the next
    EraTranslation (..)
  , ipTranslateTxOut
  , trivialEraTranslation
  ) where

import Data.SOP.Constraint
import Data.SOP.InPairs (InPairs (..), RequiringBoth (..))
import qualified Data.SOP.InPairs as InPairs
import NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))
import Ouroboros.Consensus.HardFork.Combinator.State.Types
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.TypeFamilyWrappers

{-------------------------------------------------------------------------------
  Translate from one era to the next
-------------------------------------------------------------------------------}

data EraTranslation xs = EraTranslation
  { translateLedgerState ::
      !(InPairs (RequiringBoth WrapLedgerConfig TranslateLedgerState) xs)
  , translateLedgerTables ::
      !(InPairs TranslateLedgerTables xs)
  , translateChainDepState ::
      !(InPairs (RequiringBoth WrapConsensusConfig (Translate WrapChainDepState)) xs)
  , crossEraForecast ::
      !(InPairs (RequiringBoth WrapLedgerConfig (CrossEraForecaster LedgerState WrapLedgerView)) xs)
  }
  deriving
    NoThunks
    via OnlyCheckWhnfNamed "EraTranslation" (EraTranslation xs)

ipTranslateTxOut ::
  All Top xs =>
  EraTranslation xs ->
  InPairs TranslateTxOut xs
ipTranslateTxOut = InPairs.hmap (TranslateTxOut . translateTxOutWith) . translateLedgerTables

trivialEraTranslation :: EraTranslation '[blk]
trivialEraTranslation =
  EraTranslation
    { translateLedgerState = PNil
    , translateLedgerTables = PNil
    , crossEraForecast = PNil
    , translateChainDepState = PNil
    }
