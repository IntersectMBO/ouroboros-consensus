{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DerivingVia      #-}
{-# LANGUAGE FlexibleContexts #-}

module Ouroboros.Consensus.HardFork.Combinator.Translation (
    -- * Translate from one era to the next
    EraTranslation (..)
  , Ouroboros.Consensus.HardFork.Combinator.Translation.translateTxIn
  , Ouroboros.Consensus.HardFork.Combinator.Translation.translateTxOut
  , trivialEraTranslation
  ) where

import           Data.SOP.InPairs (InPairs (..), RequiringBoth (..))
import qualified Data.SOP.InPairs as InPairs
import           Data.SOP.Strict
import           NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers

{-------------------------------------------------------------------------------
  Translate from one era to the next
-------------------------------------------------------------------------------}

data EraTranslation xs = EraTranslation {
      translateLedgerState   :: InPairs (RequiringBoth WrapLedgerConfig     TranslateLedgerState        ) xs
    , translateLedgerTables  :: InPairs                                     TranslateLedgerTables         xs
    , translateChainDepState :: InPairs (RequiringBoth WrapConsensusConfig (Translate WrapChainDepState)) xs
    , crossEraForecast       :: InPairs (RequiringBoth WrapLedgerConfig    (CrossEraForecaster LedgerState WrapLedgerView)) xs
    }
  deriving NoThunks
       via OnlyCheckWhnfNamed "EraTranslation" (EraTranslation xs)

translateTxIn ::
     All Top xs
  => EraTranslation xs
  -> InPairs TranslateTxIn xs
translateTxIn = InPairs.hmap (TranslateTxIn . translateTxInWith) . translateLedgerTables

translateTxOut ::
     All Top xs
  => EraTranslation xs
  -> InPairs TranslateTxOut xs
translateTxOut = InPairs.hmap (TranslateTxOut . translateTxOutWith) . translateLedgerTables

trivialEraTranslation :: EraTranslation '[blk]
trivialEraTranslation = EraTranslation {
      translateLedgerState   = PNil
    , translateLedgerTables  = PNil
    , crossEraForecast       = PNil
    , translateChainDepState = PNil
    }
