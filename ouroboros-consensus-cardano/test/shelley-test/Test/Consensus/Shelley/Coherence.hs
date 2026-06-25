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
leqCoherence :: Word32 -> ExUnits -> ExUnits -> Property
leqCoherence w1 eu1 eu2 =
  actual === expected
 where
  -- 'AlonzoMeasure' is the phase 1 measure that carries the ExUnits, and the
  -- one whose ordering must agree with 'pointWiseExUnits'. The reference script
  -- size lives in a separate phase 2 measure dimension that is irrelevant here.
  inj eu =
    AlonzoMeasure
      (IgnoringOverflow $ ByteSize32 w1)
      (fromExUnits eu)

  actual = inj eu1 Measure.<= inj eu2
  expected = pointWiseExUnits (<=) eu1 eu2
