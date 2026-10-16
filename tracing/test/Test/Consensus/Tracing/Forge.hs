{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Tracing.Forge (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.Measure as Measure
import Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import Ouroboros.Consensus.Cardano.CanHardFork ()
import Ouroboros.Consensus.HardFork.Combinator (HardForkBlock)
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( ByteSize32 (..)
  , IgnoringOverflow (..)
  , TxLimits (TxEbMeasure)
  )
import Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import Ouroboros.Consensus.Shelley.HFEras (StandardConwayBlock, StandardDijkstraBlock)
import Ouroboros.Consensus.Shelley.Ledger.Mempool (DijkstraEbMeasure (..))
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Tracing (jsonTxReferencesSize)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Forge"
    [ testCase "Cardano forwards the reference-size measure" $
        Aeson.Object
          (jsonTxReferencesSize (referenceMeasure :: TxEbMeasure (CardanoBlock StandardCrypto)))
          @?= expected
    , testCase "a single-era hard fork forwards the reference-size measure" $
        Aeson.Object
          ( jsonTxReferencesSize
              (referenceMeasure :: TxEbMeasure (HardForkBlock '[StandardDijkstraBlock]))
          )
          @?= expected
    , testCase "an era without reference sizes omits the field" $
        Aeson.Object (jsonTxReferencesSize (Measure.zero :: TxEbMeasure StandardConwayBlock))
          @?= Aeson.object []
    ]
 where
  expected = Aeson.object ["txReferencesSize" Aeson..= (123 :: Int)]

-- The closure measure is zero while the reference size is nonzero, so reading
-- only the closure measure would miss the value this test expects.
referenceMeasure :: DijkstraEbMeasure
referenceMeasure =
  Measure.zero
    { txReferencesSize = IgnoringOverflow (ByteSize32 123)
    }
