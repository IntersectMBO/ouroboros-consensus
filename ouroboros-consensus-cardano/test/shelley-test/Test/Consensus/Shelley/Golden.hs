{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Shelley.Golden (tests) where

import           Ouroboros.Consensus.Ledger.Query (QueryVersion)
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion
import           Ouroboros.Consensus.Shelley.Node ()
<<<<<<< HEAD:ouroboros-consensus-cardano/test/shelley-test/Test/Consensus/Shelley/Golden.hs
import           System.FilePath ((</>))
import           Test.Consensus.Shelley.Examples
||||||| parent of 2726854bf... Satisfy new serialisation constraints on LedgerConfig:ouroboros-consensus-shelley-test/test/Test/Consensus/Shelley/Golden.hs

=======
import           Ouroboros.Consensus.Shelley.ShelleyHFC ()

>>>>>>> 2726854bf... Satisfy new serialisation constraints on LedgerConfig:ouroboros-consensus-shelley-test/test/Test/Consensus/Shelley/Golden.hs
import           Test.Tasty
import           Test.Util.Paths
import           Test.Util.Serialisation.Golden

tests :: TestTree
tests = goldenTest_all codecConfig ($(getGoldenDir) </> "shelley") examplesShelley

instance ToGoldenDirectory ShelleyNodeToNodeVersion
  -- Use defaults

instance ToGoldenDirectory (QueryVersion, ShelleyNodeToClientVersion) where
  toGoldenDirectory (queryVersion, blockVersion)
    = show queryVersion </> show blockVersion
