{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.Cardano.Golden (tests) where

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.Node
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation
import           Ouroboros.Consensus.Ledger.Query (QueryVersion)
import           Ouroboros.Consensus.Shelley.HFEras ()
import           Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import           System.FilePath ((</>))
import           Test.Consensus.Cardano.Examples
import           Test.Tasty
import           Test.Util.Paths
import           Test.Util.Serialisation.Golden

tests :: TestTree
tests = goldenTest_all codecConfig ($(getGoldenDir) </> "cardano") examples

instance CardanoHardForkConstraints c
      => ToGoldenDirectory (HardForkNodeToNodeVersion (CardanoEras c)) where
  toGoldenDirectory v = case v of
    CardanoNodeToNodeVersion1 -> "CardanoNodeToNodeVersion1"
    CardanoNodeToNodeVersion2 -> "CardanoNodeToNodeVersion2"
    CardanoNodeToNodeVersion3 -> "CardanoNodeToNodeVersion3"
    _                         -> error $ "Unknown version: " <> show v

instance CardanoHardForkConstraints c
      => ToGoldenDirectory (QueryVersion, HardForkNodeToClientVersion (CardanoEras c)) where
  toGoldenDirectory (queryVersion, blockVersion) = show queryVersion </> case blockVersion of
    CardanoNodeToClientVersion12 -> "CardanoNodeToClientVersion12"
    CardanoNodeToClientVersion13 -> "CardanoNodeToClientVersion13"
    CardanoNodeToClientVersion14 -> "CardanoNodeToClientVersion14"
    CardanoNodeToClientVersion15 -> "CardanoNodeToClientVersion15"
    CardanoNodeToClientVersion16 -> "CardanoNodeToClientVersion16"
    CardanoNodeToClientVersion17 -> "CardanoNodeToClientVersion17"
    _                            -> error $ "Unknown version: " <> show blockVersion
