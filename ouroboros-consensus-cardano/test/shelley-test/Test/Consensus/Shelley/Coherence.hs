module Test.Consensus.Shelley.Coherence (tests) where

import Cardano.Ledger.Alonzo.Scripts (ExUnits, pointWiseExUnits)
import qualified Data.Measure as Measure
import Data.Word (Word32)
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( ByteSize32 (..)
  , IgnoringOverflow (..)
  , TxCount (TxCount)
  )
import Ouroboros.Consensus.Shelley.Ledger.Mempool
  ( AlonzoMeasure (..)
  , ConwayMeasure (..)
  , fromExUnits
  )
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Shelley coherences"
    [ testProperty "Measure.<= uses pointWiseExUnits (<=)" leqCoherence
    ]

-- | 'Measure.<=' and @'pointWiseExUnits' (<=)@ must agree
leqCoherence :: Word32 -> Word32 -> Word32 -> ExUnits -> ExUnits -> Property
leqCoherence w1 w2 tc eu1 eu2 =
  actual === expected
 where
  -- ConwayMeasure is the fullest TxMeasure and mainnet's
  inj eu =
    ConwayMeasure
      ( AlonzoMeasure
          (IgnoringOverflow $ ByteSize32 w1)
          (fromExUnits eu)
      )
      (IgnoringOverflow $ ByteSize32 w2)
      (IgnoringOverflow $ TxCount tc)

  actual = inj eu1 Measure.<= inj eu2
  expected = pointWiseExUnits (<=) eu1 eu2
