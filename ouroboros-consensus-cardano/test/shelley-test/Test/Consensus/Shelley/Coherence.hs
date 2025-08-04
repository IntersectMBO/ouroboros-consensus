module Test.Consensus.Shelley.Coherence (tests) where

import Cardano.Ledger.Alonzo.Scripts (ExUnits, pointWiseExUnits)
import qualified Data.Measure as Measure
import Data.Word (Word32)
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( ByteSize32 (..)
  , IgnoringOverflow (..)
  )
import Ouroboros.Consensus.Shelley.Ledger.Mempool
  ( AlonzoMeasure (..)
  , ConwayMeasure (..)
  , fromExUnits
  )
import Test.Cardano.Ledger.Alonzo.Binary.Twiddle ()
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Shelley coherences"
    [ testProperty "Measure.<= uses pointWiseExUnits (<=)" leqCoherence
    ]

-- | 'Measure.<=' and @'pointWiseExUnits' (<=)@ must agree
leqCoherence :: Word32 -> Word32 -> ExUnits -> ExUnits -> Property
leqCoherence w1 w2 eu1 eu2 =
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

  actual = inj eu1 Measure.<= inj eu2
  expected = pointWiseExUnits (<=) eu1 eu2
