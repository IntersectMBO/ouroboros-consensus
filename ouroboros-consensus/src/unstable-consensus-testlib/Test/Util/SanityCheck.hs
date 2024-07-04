module Test.Util.SanityCheck (
    prop_sanityChecks
  , prop_securityParamConsistent
  ) where

import           Ouroboros.Consensus.Block.SupportsSanityCheck
import           Ouroboros.Consensus.Config
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.Arbitrary ()

prop_sanityChecks
  :: BlockSupportsSanityCheck blk
  => TopLevelConfig blk -> Property
prop_sanityChecks cfg =
  sanityCheckConfig cfg === []

prop_securityParamConsistent
  :: BlockSupportsSanityCheck blk
  => TopLevelConfig blk -> Property
prop_securityParamConsistent cfg =
  checkSecurityParamConsistency cfg === Nothing
