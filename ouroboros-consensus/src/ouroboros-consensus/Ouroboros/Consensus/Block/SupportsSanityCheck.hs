module Ouroboros.Consensus.Block.SupportsSanityCheck
  ( BlockSupportsSanityCheck(..)
  , ProtocolConfigHasSecurityParam(..)
  ) where

import Ouroboros.Consensus.Config (TopLevelConfig)
import Data.List.NonEmpty (NonEmpty)
import Ouroboros.Consensus.Config.SecurityParam
import Ouroboros.Consensus.Protocol.Abstract

class BlockSupportsSanityCheck blk where
  checkSecurityParamConsistency
    :: TopLevelConfig blk
    -> NonEmpty SecurityParam

class ProtocolConfigHasSecurityParam p where
  protocolConfigSecurityParam :: ConsensusConfig p -> SecurityParam
